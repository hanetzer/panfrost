/*
 * Copyright (C) 2018 Alyssa Rosenzweig <alyssa@rosenzweig.io>
 *
 * Copyright (C) 2014 Rob Clark <robclark@freedesktop.org>
 * Copyright (c) 2014 Scott Mansell
 * Copyright © 2014 Broadcom
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <err.h>

#include "compiler/glsl/glsl_to_nir.h"
#include "compiler/nir_types.h"
#include "main/imports.h"
#include "compiler/nir/nir_builder.h"
#include "util/half_float.h"
#include "util/u_dynarray.h"
#include "main/mtypes.h"

#include "midgard.h"
#include "midgard_nir.h"
#include "midgard_compile.h"
#include "helpers.h"

#define NIR_DEBUG

/* Instruction arguments represented as block-local SSA indices, rather than
 * registers. Negative values mean unused. */

typedef struct {
	int src0;
	int src1;
	int dest;

	/* The output is -not- SSA -- it's a direct register from I/O -- and
	 * must not be culled/renamed */
	bool literal_out;

	/* src1 is -not- SSA but instead a 16-bit inline constant to be smudged
	 * in. Only valid for ALU ops. */
	bool inline_constant;
} ssa_args;

/* Forward declare so midgard_branch can reference */
struct midgard_block;

typedef struct midgard_branch {
	/* If conditional, the condition is specified in r31.w */
	bool conditional;
	
	/* For conditionals, if this is true, we branch on FALSE. If false, we  branch on TRUE. */
	bool invert_conditional;

	/* We can either branch to the start or the end of the block */
	struct midgard_block *target_start;
	struct midgard_block *target_after;
} midgard_branch;

/* Generic in-memory data type repesenting a single logical instruction, rather
 * than a single instruction group. This is the preferred form for code gen.
 * Multiple midgard_insturctions will later be combined during scheduling,
 * though this is not represented in this structure.  Its format bridges
 * the low-level binary representation with the higher level semantic meaning.
 *
 * Notably, it allows registers to be specified as block local SSA, for code
 * emitted before the register allocation pass.
 */

typedef struct midgard_instruction {
	unsigned type; /* ALU, load/store, texture */

	/* If the register allocator has not run yet... */
	bool uses_ssa;
	ssa_args ssa_args;

	/* Special fields for an ALU instruction */
	bool vector; 
	midgard_reg_info registers;

	/* I.e. (1 << alu_bit) */
	int unit;

	bool has_constants;
	float constants[4];

	bool compact_branch;
	bool writeout;
	bool prepacked_branch;

	/* dynarray's are O(n) to delete from, which makes peephole
	 * optimisations a little awkward. Instead, just have an unused flag
	 * which the code gen will skip over */

	bool unused;

	union {
		midgard_load_store_word load_store;
		midgard_vector_alu alu;
		midgard_texture_word texture;
		uint16_t br_compact;

		/* General branch, rather than packed br_compact. Higher level
		 * than the other components */
		midgard_branch branch;
	};
} midgard_instruction;

/* Helpers to generate midgard_instruction's using macro magic, since every
 * driver seems to do it that way */

#define EMIT(op, ...) util_dynarray_append((ctx->current_block), midgard_instruction, v_##op(__VA_ARGS__));

#define M_LOAD_STORE(name, rname, uname) \
	static midgard_instruction m_##name(unsigned ssa, unsigned address) { \
		midgard_instruction i = { \
			.type = TAG_LOAD_STORE_4, \
			.uses_ssa = true, \
			.ssa_args = { \
				.rname = ssa, \
				.uname = -1, \
				.src1 = -1 \
			}, \
			.unused = false, \
			.load_store = { \
				.op = midgard_op_##name, \
				.mask = 0xF, \
				.swizzle = SWIZZLE(COMPONENT_X, COMPONENT_Y, COMPONENT_Z, COMPONENT_W), \
				.address = address \
			} \
		}; \
		\
		return i; \
	}

#define M_LOAD(name) M_LOAD_STORE(name, dest, src0)
#define M_STORE(name) M_LOAD_STORE(name, src0, dest)

const midgard_vector_alu_src blank_alu_src = {
	.swizzle = SWIZZLE(COMPONENT_X, COMPONENT_Y, COMPONENT_Z, COMPONENT_W),
};

const midgard_scalar_alu_src blank_scalar_alu_src = {
	.full = true
};

/* Used for encoding the unused source of 1-op instructions */
const midgard_vector_alu_src zero_alu_src = { 0 };

/* Coerce structs to integer */

static unsigned
vector_alu_srco_unsigned(midgard_vector_alu_src src)
{
	unsigned u;
	memcpy(&u, &src, sizeof(src));
	return u;
}

/* Inputs a NIR ALU source, with modifiers attached if necessary, and outputs
 * the corresponding Midgard source */

static midgard_vector_alu_src
vector_alu_modifiers(nir_alu_src *src)
{
	if (!src) return blank_alu_src;

	midgard_vector_alu_src alu_src = {
		.abs = src->abs,
		.negate = src->negate,
		.rep_low = 0,
		.rep_high = 0,
		.half = 0, /* TODO */
		.swizzle = SWIZZLE_FROM_ARRAY(src->swizzle)
	};

	return alu_src;
}

static midgard_instruction
m_alu_vector(midgard_alu_op op, int unit, unsigned src0, midgard_vector_alu_src mod1, unsigned src1, midgard_vector_alu_src mod2, unsigned dest, bool literal_out, midgard_outmod outmod)
{
	/* TODO: Use literal_out hint during register allocation */
	midgard_instruction ins = {
		.type = TAG_ALU_4,
		.unit = unit,
		.unused = false,
		.uses_ssa = true,
		.ssa_args = {
			.src0 = src0,
			.src1 = src1,
			.dest = dest,
			.literal_out = literal_out
		},
		.vector = true,
		.alu = {
			.op = op,
			.reg_mode = midgard_reg_mode_full,
			.dest_override = midgard_dest_override_none,
			.outmod = outmod,
			.mask = 0xFF,
			.src1 = vector_alu_srco_unsigned(mod1),
			.src2 = vector_alu_srco_unsigned(mod2)
		},
	};

	return ins;
}

#define M_ALU_VECTOR_1(unit, name) \
	static midgard_instruction v_##name(unsigned src, midgard_vector_alu_src mod1, unsigned dest, bool literal, midgard_outmod outmod) { \
		return m_alu_vector(midgard_alu_op_##name, ALU_ENAB_VEC_##unit, SSA_UNUSED_1, zero_alu_src, src, mod1, dest, literal, outmod); \
	}

/* load/store instructions have both 32-bit and 16-bit variants, depending on
 * whether we are using vectors composed of highp or mediump. At the moment, we
 * don't support half-floats -- this requires changes in other parts of the
 * compiler -- therefore the 16-bit versions are commented out. */

//M_LOAD(load_attr_16);
M_LOAD(load_attr_32);
//M_LOAD(load_vary_16);
M_LOAD(load_vary_32);
//M_LOAD(load_uniform_16);
//M_LOAD(load_uniform_32);
//M_STORE(store_vary_16);
M_STORE(store_vary_32);

/* Used as a sort of intrinsic outside of the ALU code */
M_ALU_VECTOR_1(MUL, fmov);

static midgard_instruction
v_alu_br_compact_cond(midgard_jmp_writeout_op op, unsigned tag, signed offset, unsigned cond)
{
	midgard_branch_cond branch = {
		.op = op,
		.dest_tag = tag,
		.offset = offset,
		.cond = cond
	};

	uint16_t compact;
	memcpy(&compact, &branch, sizeof(branch));

	midgard_instruction ins = {
		.type = TAG_ALU_4,
		.unit = ALU_ENAB_BR_COMPACT ,
		.unused = false,
		.uses_ssa = false,

		.prepacked_branch = true,
		.compact_branch = true, 
		.br_compact = compact
	};

	if (op == midgard_jmp_writeout_op_writeout)
		ins.writeout = true;

	return ins;
}

static midgard_instruction
v_branch(bool conditional, bool invert)
{
	midgard_instruction ins = {
		.type = TAG_ALU_4,
		.unit = ALU_ENAB_BR_COMPACT,
		.compact_branch = true,
		.branch = {
			.conditional = conditional,
			.invert_conditional = invert
		}
	};

	return ins;
}

static void
attach_constants(midgard_instruction *ins, void *constants)
{
	ins->has_constants = true;
	memcpy(&ins->constants, constants, 16); /* TODO: How big? */
}

typedef struct midgard_bundle {
	/* Tag for the overall bundle */
	int tag;

	/* Instructions contained by the bundle */
	int instruction_count;
	midgard_instruction instructions[5];

	/* Bundle-wide ALU configuration */
	int padding;
	int control;
	bool has_embedded_constants;
	float constants[4];

	uint16_t register_words[8];
	int register_words_count;

	uint64_t body_words[8];
	size_t body_size[8];
	int body_words_count;
} midgard_bundle;

typedef struct midgard_block {
	/* List of midgard_instructions emitted for the current block */
	struct util_dynarray instructions;

	bool is_scheduled;

	/* List of midgard_bundles emitted (after the scheduler has run) */
	struct util_dynarray bundles;

	/* Number of quadwords _actually_ emitted, as determined after scheduling */
	unsigned quadword_count;

	struct midgard_block *next_fallthrough;
} midgard_block;

typedef struct compiler_context {
	gl_shader_stage stage;

	/* Current NIR function */
	nir_function *func;

	/* Unordered array of midgard_block */
	int block_count;
	struct util_dynarray blocks;

	midgard_block *initial_block;
	midgard_block *previous_source_block;
	midgard_block *final_block;

	/* List of midgard_instructions emitted for the current block */
	struct util_dynarray *current_block;

	/* Constants which have been loaded, for later inlining */
	struct hash_table_u64 *ssa_constants;

	/* SSA values / registers which have been aliased. Naively, these
	 * demand a fmov output; instead, we alias them in a later pass to
	 * avoid the wasted op.
	 *
	 * A note on encoding: to avoid dynamic memory management here, rather
	 * than ampping to a pointer, we map to the source index; the key
	 * itself is just the destination index. */

	struct hash_table_u64 *ssa_to_alias;
	struct set *leftover_ssa_to_alias;
	
	/* Encoded the same as ssa_to_alias, except now it's mapping SSA source indicdes as the keys to fixed destination registers as the values */
	struct hash_table_u64 *register_to_ssa;

	/* Actual SSA-to-register for RA */
	struct hash_table_u64 *ssa_to_register;

	/* Active register bitfield for RA */
	unsigned used_registers : 16;

	/* Used for cont/last hinting. Increase when a tex op is added.
	 * Decrease when a tex op is removed. */
	int texture_op_count;

	/* Count of special uniforms (viewport, etc) in vec4 units */
	int special_uniforms;
} compiler_context;

static int
glsl_type_size(const struct glsl_type *type)
{
	return glsl_count_attribute_slots(type, false);
}

static void
optimise_nir(nir_shader *nir)
{
	bool progress;

	NIR_PASS(progress, nir, nir_lower_regs_to_ssa);

	do {
		progress = false;

		NIR_PASS(progress, nir, midgard_nir_lower_algebraic);
		NIR_PASS(progress, nir, nir_lower_io, nir_var_all, glsl_type_size, 0);
		NIR_PASS(progress, nir, nir_lower_var_copies);
		NIR_PASS(progress, nir, nir_lower_vars_to_ssa);

		NIR_PASS(progress, nir, nir_copy_prop);
		NIR_PASS(progress, nir, nir_opt_dce);
		NIR_PASS(progress, nir, nir_opt_dead_cf);
		NIR_PASS(progress, nir, nir_opt_cse);
		//NIR_PASS(progress, nir, nir_opt_peephole_select, 8);
		NIR_PASS(progress, nir, nir_opt_algebraic);
		NIR_PASS(progress, nir, nir_opt_constant_folding);
		NIR_PASS(progress, nir, nir_opt_undef);
		NIR_PASS(progress, nir, nir_opt_loop_unroll, 
				nir_var_shader_in |
				nir_var_shader_out |
				nir_var_local);
	} while(progress);

	/* Must be run at the end to prevent creation of fsin/fcos ops */
	NIR_PASS(progress, nir, midgard_nir_scale_trig);

	do {
		progress = false;

		NIR_PASS(progress, nir, nir_opt_dce);
		NIR_PASS(progress, nir, nir_opt_algebraic);
		NIR_PASS(progress, nir, nir_opt_constant_folding);
		NIR_PASS(progress, nir, nir_copy_prop);
	} while(progress);

	/* Lower mods */
	NIR_PASS(progress, nir, nir_lower_to_source_mods);
	NIR_PASS(progress, nir, nir_copy_prop);
	NIR_PASS(progress, nir, nir_opt_dce);
}

/* Front-half of aliasing the SSA slots, merely by inserting the flag in the
 * appropriate hash table. Intentional off-by-one to avoid confusing NULL with
 * r0. See the comments in compiler_context */

static void
alias_ssa(compiler_context *ctx, int dest, int src, bool literal_dest)
{
	if (literal_dest) {
		/* If this is just a constant, emit a move for it */
		int is_constant = _mesa_hash_table_u64_search(ctx->ssa_constants, src + 1);

		if (is_constant)
			EMIT(fmov, src, blank_alu_src, dest, true, midgard_outmod_none);

		_mesa_hash_table_u64_insert(ctx->register_to_ssa, src + 1, (void *) ((uintptr_t) dest + 1));
	} else {
		_mesa_hash_table_u64_insert(ctx->ssa_to_alias, dest + 1, (void *) ((uintptr_t) src + 1));
		_mesa_set_add(ctx->leftover_ssa_to_alias, (void *) (uintptr_t) (dest + 1));
	}
}

/* Do not actually emit a load; instead, cache the constant for inlining */

static void
emit_load_const(compiler_context *ctx, nir_load_const_instr *instr)
{
	nir_ssa_def def = instr->def;

	float *v = ralloc_array(NULL, float, 4);
	memcpy(v, &instr->value.f32, 4 * sizeof(float));
	_mesa_hash_table_u64_insert(ctx->ssa_constants, def.index + 1, v);
}

/* Duplicate bits to convert sane 4-bit writemask to obscure 8-bit format */

static unsigned
expand_writemask(unsigned mask)
{
	unsigned o = 0;

	for (int i = 0; i < 4; ++i)
		if (mask & (1 << i))
		       o |= (3 << (2*i));
	
	return o;
}

/* Generate write mask when there are a specific number of components, e.g.
 * xyz -> 3 -> 0x7 */

static unsigned
writemask_for_nr_components(int nr_components)
{
	unsigned mask = 0;
	
	while(nr_components--)
		mask = (mask << 1) | 1;

	return mask;
}

static unsigned
nir_src_index(nir_src *src)
{
	if (src->is_ssa)
		return src->ssa->index;
	else
		return 4096 + src->reg.reg->index;
}

static unsigned
nir_dest_index(nir_dest *dst)
{
	if (dst->is_ssa)
		return dst->ssa.index;
	else
		return 4096 + dst->reg.reg->index;
}

static unsigned
nir_alu_src_index(nir_alu_src *src)
{
	return nir_src_index(&src->src);
}

/* Midgard puts conditionals in r31.w; move an arbitrary source (the output of
 * a conditional test) into that register */

static void
emit_condition(compiler_context *ctx, nir_src *src)
{
	/* XXX: Force component correct */
	int condition = nir_src_index(src);

	const midgard_vector_alu_src alu_src = {
		.swizzle = SWIZZLE(COMPONENT_X, COMPONENT_X, COMPONENT_X, COMPONENT_X),
	};

	midgard_instruction ins = v_fmov(condition, alu_src, 31, true, midgard_outmod_none);
	ins.alu.mask = 0x3 << 6; /* mask out w */

	util_dynarray_append(ctx->current_block, midgard_instruction, ins);
}

/* Components: Number/style of arguments:
 * 	3: One-argument op with r24 (i2f, f2i)
 * 	2: Standard two argument op (fadd, fmul)
 * 	1: Flipped one-argument op (fmov, imov)
 * 	0: Standard one-argument op (frcp)
 * NIR: NIR instruction op.
 * Op: Midgard instruction op.
 */

#define ALU_CASE(_components, nir, _op) \
	case nir_op_##nir: \
		components = _components; \
		op = midgard_alu_op_##_op; \
		break;

static void
emit_alu(compiler_context *ctx, nir_alu_instr *instr)
{
	bool is_ssa = instr->dest.dest.is_ssa;

	unsigned dest = nir_dest_index(&instr->dest.dest);
	unsigned nr_components = is_ssa ? instr->dest.dest.ssa.num_components : instr->dest.dest.reg.reg->num_components;

	/* ALU ops are unified in NIR between scalar/vector, but partially
	 * split in Midgard. Reconcile that here, to avoid diverging code paths
	 */
	bool is_vector = nr_components != 1;

	/* Most Midgard ALU ops have a 1:1 correspondance to NIR ops; these are
	 * supported. A few do not and are therefore commented and TODO to
	 * figure out what code paths would generate these. Also, there are a
	 * number of NIR ops which Midgard does not support and need to be
	 * lowered, also TODO. This switch block emits the opcode and calling
	 * convention of the Midgard instruction; actual packing is done in emit_alu below
	 * */

	unsigned op, unit, components;

	switch(instr->op) {
		ALU_CASE(2, fadd, fadd);
		ALU_CASE(2, fmul, fmul);
		ALU_CASE(2, fmin, fmin);
		ALU_CASE(2, fmax, fmax);
		ALU_CASE(2, imin, imin);
		ALU_CASE(2, imax, imax);
		ALU_CASE(1, fmov, fmov);
		ALU_CASE(0, ffloor, ffloor);
		ALU_CASE(0, fceil, fceil);
		ALU_CASE(2, fdot3, fdot3);
		//ALU_CASE(2, fdot3r);
		ALU_CASE(2, fdot4, fdot4);
		//ALU_CASE(2, freduce);
		ALU_CASE(2, iadd, iadd);
		ALU_CASE(2, isub, isub);
		ALU_CASE(2, imul, imul);
		ALU_CASE(1, imov, imov);

		ALU_CASE(2, feq, feq);
		ALU_CASE(2, fne, fne);
		ALU_CASE(2, flt, flt);
		//ALU_CASE(2, fle);
		ALU_CASE(2, ieq, ieq);
		ALU_CASE(2, ine, ine);
		ALU_CASE(2, ilt, ilt);
		//ALU_CASE(2, ile);
		//ALU_CASE(2, icsel, icsel);
		//ALU_CASE(0, fatan_pt2);
		ALU_CASE(0, frcp, frcp);
		ALU_CASE(0, frsq, frsqrt);
		ALU_CASE(0, fsqrt, fsqrt);
		ALU_CASE(0, fexp2, fexp2);
		ALU_CASE(0, flog2, flog2);

		ALU_CASE(3, f2i32, f2i);
		ALU_CASE(3, f2u32, f2u);
		ALU_CASE(3, i2f32, i2f);
		ALU_CASE(3, u2f32, u2f);

		ALU_CASE(0, fsin, fsin);
		ALU_CASE(0, fcos, fcos);

		//ALU_CASE(0, fatan_pt1);

		ALU_CASE(2, iand, iand);
		ALU_CASE(2, ior, ior);
		ALU_CASE(2, ixor, ixor);
		ALU_CASE(0, inot, inot);
		ALU_CASE(2, ishl, ishl);
		ALU_CASE(2, ishr, iasr);
		ALU_CASE(2, ushr, ilsr);
		//ALU_CASE(2, ilsr, ilsr);

		ALU_CASE(2, ball_fequal4, fball_eq);
		ALU_CASE(2, bany_fnequal4, fbany_neq);
		ALU_CASE(2, ball_iequal4, iball_eq);
		ALU_CASE(2, bany_inequal4, ibany_neq);

		case nir_op_bcsel: {
			components = 2;
			op = midgard_alu_op_fcsel;

			emit_condition(ctx, &instr->src[0].src);
			//alias_ssa(ctx, instr->src[0].src.ssa->index, SSA_FIXED_REGISTER(31), true);

			/* The condition is the first argument; move the other
			 * arguments up one to be a binary instruction for
			 * Midgard */

			memmove(instr->src, instr->src + 1, 2 * sizeof(nir_alu_src));
			break;
		}

		/* Vector combination from SSA requires some special case TODO */
		case nir_op_vec2:
		case nir_op_vec3:
		case nir_op_vec4: {
			for (int i = 0; i < nr_components; ++i) {
				nir_alu_src *asrc = &instr->src[i];

				midgard_vector_alu_src src = vector_alu_modifiers(asrc);
				int c = asrc->swizzle[0];
				src.swizzle = SWIZZLE(c, c, c, c);
				midgard_instruction ins = v_fmov(nir_src_index(&asrc->src), src, dest, false, midgard_outmod_none);
				ins.alu.mask = 0x3 << (2 * i);

				util_dynarray_append((ctx->current_block), midgard_instruction, ins);
			}

			return;
		};

		default:
			printf("Unhandled ALU op %X\n", instr->op);
			assert(0);
			return;
	}

	int _unit = alu_opcode_unit[op];

	/* slut doesn't exist; lower to vlut which acts as scalar
	 * despite the name */

	if (_unit == UNIT_VLUT)
		is_vector = true;

	/* Certain ops cannot run as scalars */
	if (!(_unit & UNITS_SCALAR))
		is_vector = true;

	/* Initialise fields common between scalar/vector instructions */
	midgard_outmod outmod = instr->dest.saturate ? midgard_outmod_sat : midgard_outmod_none;

	/* src0 will always exist afaik, but src1 will not for 1-argument
	 * instructions. The latter can only be fetched if the instruction
	 * needs it, or else we may segfault. */

	unsigned src0 = nir_alu_src_index(&instr->src[0]);
	unsigned src1 = components == 2 ? nir_alu_src_index(&instr->src[1]) : 0;

	/* Rather than use the instruction generation helpers, we do it
	 * ourselves here to avoid the mess */

	midgard_instruction ins = {
		.type = TAG_ALU_4,
		.unused = false,
		.uses_ssa = true,
		.ssa_args = {
			.src0 = components == 3 || components == 2 || components == 0 ? src0 : SSA_UNUSED_1,
			.src1 = components == 2 ? src1 : components == 1 ? src0 : components == 0 ? 0 : SSA_UNUSED_1,
			.dest = dest,
			.inline_constant = components == 0
		},
		.vector = is_vector
	};

	nir_alu_src *nirmod0 = NULL;
	nir_alu_src *nirmod1 = NULL;

	if (components == 2) {
		nirmod0 = &instr->src[0];
		nirmod1 = &instr->src[1];
	} else if (components == 1) {
		nirmod1 = &instr->src[0];
	} else if (components == 0) {
		nirmod0 = &instr->src[0];
	}

	midgard_vector_alu alu = {
		.op = op,
		.reg_mode = midgard_reg_mode_full,
		.dest_override = midgard_dest_override_none,
		.outmod = outmod,

		/* Writemask only valid for non-SSA NIR */
		.mask = is_ssa ? expand_writemask(writemask_for_nr_components(nr_components)) : expand_writemask(instr->dest.write_mask),

		.src1 = vector_alu_srco_unsigned(vector_alu_modifiers(nirmod0)),
		.src2 = vector_alu_srco_unsigned(vector_alu_modifiers(nirmod1)),
	};

	ins.alu = alu;

	if (_unit == UNIT_VLUT) {
		/* To avoid duplicating the LUTs (we think?), LUT instructions can only
		 * operate as if they were scalars. Lower them here by changing the
		 * component. */

		assert(components == 0);

		uint8_t original_swizzle[4];
		memcpy(original_swizzle, nirmod0->swizzle, sizeof(nirmod0->swizzle));

		for (int i = 0; i < nr_components; ++i) {
			ins.alu.mask = (0x3) << (2 * i); /* Mask the associated component */

			for (int j = 0; j < 4; ++j)
				nirmod0->swizzle[j] = original_swizzle[i]; /* Pull from the correct component */

			ins.alu.src1 = vector_alu_srco_unsigned(vector_alu_modifiers(nirmod0));
			util_dynarray_append(ctx->current_block, midgard_instruction, ins);
		}
	} else {
		util_dynarray_append(ctx->current_block, midgard_instruction, ins);
	}
}

static void
emit_intrinsic(compiler_context *ctx, nir_intrinsic_instr *instr)
{
        nir_const_value *const_offset;
        unsigned offset, reg, nr_components;

	switch(instr->intrinsic) {
		case nir_intrinsic_load_uniform:
		case nir_intrinsic_load_input:
			const_offset = nir_src_as_const_value(instr->src[0]);
			assert (const_offset && "no indirect inputs");

			offset = nir_intrinsic_base(instr) + const_offset->u32[0];

			reg = instr->dest.ssa.index;
			nr_components = instr->dest.ssa.num_components;

			if (instr->intrinsic == nir_intrinsic_load_uniform) {
				/* TODO: half-floats */
				/* TODO: Spill to ld_uniform */

				if (offset >= SPECIAL_UNIFORM_BASE) {
					/* XXX: Resolve which uniform */
					offset = 0;
				} else {
					/* Offset away from the special
					 * uniform block */

					offset += ctx->special_uniforms;
				}

				int reg_slot = 23 - offset;
				
				/* Uniform accesses are 0-cycle, since they're
				 * just a register fetch in the usual case. So,
				 * we alias the registers while we're still in
				 * SSA-space */

				alias_ssa(ctx, reg, SSA_FIXED_REGISTER(reg_slot), false);
			} else if (ctx->stage == MESA_SHADER_FRAGMENT) {
				/* XXX: Half-floats? */
				/* TODO: swizzle, mask */

				midgard_instruction ins = m_load_vary_32(reg, offset);
				ins.load_store.is_varying = 1;
				ins.load_store.interpolation = midgard_interp_default;
				ins.load_store.unknown = 0x1E9E; /* XXX: What is this? */
				util_dynarray_append(ctx->current_block, midgard_instruction, ins);
			} else if (ctx->stage == MESA_SHADER_VERTEX) {
				midgard_instruction ins = m_load_attr_32(reg, offset);
				ins.load_store.unknown = 0x1E1E; /* XXX: What is this? */

				/* TODO: Generalise, and is this necessary? */
				if (nr_components == 2) {
					ins.load_store.mask = 0x3;
					ins.load_store.swizzle = SWIZZLE(COMPONENT_X, COMPONENT_Y, COMPONENT_X, COMPONENT_X);
				} else if (nr_components == 3) {
					ins.load_store.mask = 0x7;
					ins.load_store.swizzle = SWIZZLE(COMPONENT_X, COMPONENT_Y, COMPONENT_Z, COMPONENT_X);
				}

				util_dynarray_append(ctx->current_block, midgard_instruction, ins);
			} else {
				printf("Unknown load\n");
			}

			break;

		case nir_intrinsic_store_output:
			const_offset = nir_src_as_const_value(instr->src[1]);
			assert(const_offset && "no indirect outputs");

			offset = nir_intrinsic_base(instr) + const_offset->u32[0];

			reg = nir_src_index(&instr->src[0]);

			if (ctx->stage == MESA_SHADER_FRAGMENT) {
				/* gl_FragColor is not emitted with load/store
				 * instructions. Instead, it gets plonked into
				 * r0 at the end of the shader and we do the
				 * framebuffer writeout dance. TODO: Defer
				 * writes */

				alias_ssa(ctx, 0, reg, true);
			} else if (ctx->stage == MESA_SHADER_VERTEX) {
				/* Varyings are written into one of two special
				 * varying register, r26 or r27. The register itself is selected as the register 
				 * in the st_vary instruction, minus the base of 26. E.g. write into r27 and then call st_vary(1)
				 *
				 * Normally emitting fmov's is frowned upon,
				 * but due to unique constraints of
				 * REGISTER_VARYING, fmov emission + a
				 * dedicated cleanup pass is the only way to
				 * guarantee correctness when considering some
				 * (common) edge cases XXX: FIXME */

				/* TODO: Integrate with special purpose RA (and scheduler?) */
				bool high_varying_register = false;

				EMIT(fmov, reg, blank_alu_src, REGISTER_VARYING_BASE + high_varying_register, true, midgard_outmod_none);

				/* Compute offset: gl_Position is zero. The
				 * first varying is two. The nth varying is 2+n
				 * for zero-indexed n. Varying #1 is unused.
				 * See the corresponding structures in the
				 * command stream. */

				if (offset > 0)
					offset += 1;

				midgard_instruction ins = m_store_vary_32(high_varying_register, offset);
				ins.load_store.unknown = 0x1E9E; /* XXX: What is this? */
				ins.uses_ssa = false;
				util_dynarray_append(ctx->current_block, midgard_instruction, ins);
			} else {
				printf("Unknown store\n");
			}

			break;

		default:
			printf ("Unhandled intrinsic\n");
			break;
	}
}

static unsigned
midgard_tex_format(enum glsl_sampler_dim dim)
{
	switch (dim) {
		case GLSL_SAMPLER_DIM_2D:
			return TEXTURE_2D;

		case GLSL_SAMPLER_DIM_3D:
			return TEXTURE_3D;

		case GLSL_SAMPLER_DIM_CUBE:
			return TEXTURE_CUBE;

		default:
			printf("Unknown sampler dim type\n");
			return 0;
	}
}

static void
emit_tex(compiler_context *ctx, nir_tex_instr *instr)
{
	/* TODO */
	//assert (!instr->sampler);
	assert (!instr->texture_array_size);
	assert (instr->op == nir_texop_tex);

	int texture_index = instr->texture_index;

	/* TODO: Vulkan, where texture =/= sampler */
	int sampler_index = texture_index;

	for (unsigned i = 0; i < instr->num_srcs; ++i) {
		switch (instr->src[i].src_type) {
			case nir_tex_src_coord: {
				int index = nir_src_index(&instr->src[i].src);
				/* TODO: Also use other texture reg? */

				midgard_vector_alu_src alu_src = blank_alu_src;
				alu_src.swizzle = (COMPONENT_Y << 2);

				midgard_instruction ins = v_fmov(index, alu_src, REGISTER_TEXTURE_BASE, true, midgard_outmod_none);
				util_dynarray_append((ctx->current_block), midgard_instruction, ins);

				break;
			}
			default: {
				printf("Unknown source type\n");
				assert(0);
				break;
			 }
		}
	}
	
	/* No helper to build texture words -- we do it all here */
	midgard_instruction ins = {
		.type = TAG_TEXTURE_4,
		.texture = {
			.op = TEXTURE_OP_NORMAL,
			.format = midgard_tex_format(instr->sampler_dim),
			.texture_handle = texture_index,
			.sampler_handle = sampler_index,

			/* TODO: Don't force xyzw */
			.swizzle = SWIZZLE(COMPONENT_X, COMPONENT_Y, COMPONENT_Z, COMPONENT_W), 
			.mask = 0xF, 

			/* TODO: half */
			.in_reg_full = 1,
			.out_full = 1,

			/* TODO: RA on tex file */
			.out_reg_select = 1,

			.filter = 1,
			
			/* Always 1 */
			.unknown7 = 1,

			/* Assume we can continue; hint it out later */
			.cont = 1,
		}
	};
	
	/* TODO: Dynamic swizzle input selection, half-swizzles? */
	if (instr->sampler_dim == GLSL_SAMPLER_DIM_3D) {
		ins.texture.in_reg_swizzle_right = COMPONENT_X;
		ins.texture.in_reg_swizzle_left = COMPONENT_Y;
		ins.texture.in_reg_swizzle_third = COMPONENT_Z;
	} else {
		ins.texture.in_reg_swizzle_left = COMPONENT_X;
		ins.texture.in_reg_swizzle_right = COMPONENT_Y;
		ins.texture.in_reg_swizzle_third = COMPONENT_X;
	}

	util_dynarray_append(ctx->current_block, midgard_instruction, ins);

	/* Emit a move for the destination as well TODO eliminate */
	
	EMIT(fmov, SSA_FIXED_REGISTER(REGISTER_TEXTURE_BASE + 1), blank_alu_src, nir_dest_index(&instr->dest), false, midgard_outmod_none);
	//alias_ssa(ctx, nir_dest_index(&instr->dest), SSA_FIXED_REGISTER(REGISTER_TEXTURE_BASE + 1), false);
	//^^ does not work due to overwriting with multiple tex ops, FIXME

	/* Used for .cont and .last hinting */
	ctx->texture_op_count++;
}

static void
emit_instr(compiler_context *ctx, struct nir_instr *instr)
{
#ifdef NIR_DEBUG
	nir_print_instr(instr, stdout);
	putchar('\n');
#endif

	switch(instr->type) {
		case nir_instr_type_load_const:
			emit_load_const(ctx, nir_instr_as_load_const(instr));
			break;

		case nir_instr_type_intrinsic:
			emit_intrinsic(ctx, nir_instr_as_intrinsic(instr));
			break;

		case nir_instr_type_alu:
			emit_alu(ctx, nir_instr_as_alu(instr));
			break;

		case nir_instr_type_tex:
			emit_tex(ctx, nir_instr_as_tex(instr));
			break;

		default:
			printf("Unhandled instruction type\n");
			break;
	}
}

/* XXX: This has really awful asymptomatic complexity. Fix it or switch to
 * anholt's RA or something */

#define IN_ARRAY(n, arr) (n < (((char *) ((arr)->data)) + ((arr)->size)))

static bool
is_ssa_used_later(midgard_block *block, midgard_instruction *ins, int ssa)
{
	for (midgard_instruction *candidate = ins + 1;
	     IN_ARRAY(candidate, &block->instructions);
	     candidate += 1) {
		if (!candidate->uses_ssa) continue;

		if ((candidate->ssa_args.src0 == ssa) ||
		    (candidate->ssa_args.src1 == ssa))
			return true;
	}

	return false;
}

static int
allocate_first_free_register(compiler_context *ctx)
{
	for (int i = 1; i < MAX_WORK_REGISTERS; ++i) {
		bool used = ctx->used_registers & (1 << i);

		if (used) continue;

		ctx->used_registers |= (1 << i);
		return i;
	}

	printf("OUT OF REGISTERS, NO SPILLING IMPLEMENTED"); /* XXX */
	return 0;
}

static int
normal_ssa_to_register(compiler_context *ctx, midgard_block *block, midgard_instruction *ins, int ssa)
{
	int reg = (int) _mesa_hash_table_u64_search(ctx->ssa_to_register, ssa + 1);

	if (reg) {
		reg -= 1; /* Intentional off-by-one */
	} else {
		/* XXX: Proper register allocation */
		reg = allocate_first_free_register(ctx);
		_mesa_hash_table_u64_insert(ctx->ssa_to_register, ssa + 1, (void *) (uintptr_t) (reg + 1));
	}

	/* Free the register if possible */

	if (!is_ssa_used_later(block, ins, ssa))
		ctx->used_registers &= ~(1 << reg);

	return reg;
}

/* Transform to account for SSA register aliases */

static int
dealias_register(compiler_context *ctx, midgard_block *block, midgard_instruction *ins, int reg, bool is_ssa)
{
	if (reg >= SSA_FIXED_MINIMUM)
		return SSA_REG_FROM_FIXED(reg);

	if (reg >= 0)
		return is_ssa ? normal_ssa_to_register(ctx, block, ins, reg) : reg;

	switch(reg) {
		/* fmov style unused */
		case SSA_UNUSED_0: return 0;
		
		/* lut style unused */
		case SSA_UNUSED_1: return REGISTER_UNUSED;

		default:
		   printf("Unknown SSA register alias %d\n", reg);
		   return 31;
	}
}

static void
allocate_registers(compiler_context *ctx, midgard_block *block)
{
	util_dynarray_foreach(&block->instructions, midgard_instruction, ins) {
		if (ins->unused) continue;

		ssa_args args = ins->ssa_args;

		switch (ins->type) {
			case TAG_ALU_4:
				ins->registers.src1_reg = dealias_register(ctx, block, ins, args.src0, ins->uses_ssa);

				ins->registers.src2_imm = args.inline_constant;

				if (args.inline_constant) {
					/* Encode inline 16-bit constant */

					ins->registers.src2_reg = args.src1 >> 11;

					int lower_11 = args.src1 & ((1 << 12) - 1);

					//if (ins->vector) {
						uint16_t imm = ((lower_11 >> 8) & 0x7) | ((lower_11 & 0xFF) << 3);
						ins->alu.src2 = imm << 2;
#if 0
					} else {
						uint16_t imm = 0;
						imm |= (lower_11 >> 9) & 3;
						imm |= (lower_11 >> 6) & 4;
						imm |= (lower_11 >> 2) & 0x38;
						imm |= (lower_11 & 63) << 6;
						ins->scalar_alu.src2 = imm;
					}
#endif
					//assert(0); /* XXX: Reenable inline constant */
				} else {
					ins->registers.src2_reg = dealias_register(ctx, block, ins, args.src1, ins->uses_ssa);
				}

				/* Output register at the end due to the natural flow of registers, allowing for in place operations */
				ins->registers.out_reg = dealias_register(ctx, block, ins, args.dest, ins->uses_ssa && !args.literal_out);

				break;
			
			case TAG_LOAD_STORE_4: {
				bool has_dest = args.dest >= 0;
				int ssa_arg = has_dest ? args.dest : args.src0;
				bool not_literal_out = has_dest ? !args.literal_out : true;

				ins->load_store.reg = dealias_register(ctx, block, ins, ssa_arg, ins->uses_ssa && not_literal_out);

				break;
		        }

			default: 
				printf("Unknown tag in register assignment pass\n");
				break;
		}
	}
}

/* Midgard IR only knows vector ALU types, but we sometimes need to actually
 * use scalar ALU instructions, for functional or performance reasons. To do
 * this, we just demote vector ALU payloads to scalar. */

static int
component_from_mask(unsigned mask) {
	for (int c = 0; c < 4; ++c) {
		if (mask & (3 << (2*c)))
			return c;
	}

	assert(0);
}

static bool
is_single_component_mask(unsigned mask)
{
	int components = 0;

	for (int c = 0; c < 4; ++c)
		if (mask & (3 << (2*c)))
			components++;
	
	return components == 1;
}

static unsigned
vector_to_scalar_source(unsigned u)
{
	midgard_vector_alu_src v;
	memcpy(&v, &u, sizeof(v));

	midgard_scalar_alu_src s = {
		.abs = v.abs,
		.negate = v.negate,
		.full = !v.half,
		.component = (v.swizzle & 3) << 1
	};

	unsigned o;
	memcpy(&o, &s, sizeof(s));

	return o;
}

static midgard_scalar_alu
vector_to_scalar_alu(midgard_vector_alu v)
{
	/* The output component is from the mask */
	midgard_scalar_alu s = {
		.op = v.op,
		.src1 = vector_to_scalar_source(v.src1),
		.src2 = vector_to_scalar_source(v.src2),
		.unknown = 0,
		.outmod = v.outmod,
		.output_full = 1, /* TODO: Half */
		.output_component = component_from_mask(v.mask) << 1,
	};

	return s;
}

/* Midgard prefetches instruction types, so during emission we need to
 * lookahead too. Unless this is the last instruction, in which we return 1. Or
 * if this is the second to last and the last is an ALU, then it's also 1... */

#define IS_ALU(tag) (tag == TAG_ALU_4 || tag == TAG_ALU_8 ||  \
		     tag == TAG_ALU_12 || tag == TAG_ALU_16)

#define EMIT_AND_COUNT(type, val) util_dynarray_append(emission, type, val); \
				  bytes_emitted += sizeof(type)

static void
emit_binary_vector_instruction(midgard_instruction *ains,
		uint16_t *register_words, int *register_words_count, 
		uint64_t *body_words, size_t *body_size, int *body_words_count, 
		size_t *bytes_emitted)
{
	memcpy(&register_words[(*register_words_count)++], &ains->registers, sizeof(ains->registers));
	*bytes_emitted += sizeof(midgard_reg_info);

	body_size[*body_words_count] = sizeof(midgard_vector_alu);
	memcpy(&body_words[(*body_words_count)++], &ains->alu, sizeof(ains->alu));
	*bytes_emitted += sizeof(midgard_vector_alu);
}

static int
effective_ld_st_reg(midgard_load_store_word *ldst)
{
	if (OP_IS_STORE(ldst->op))
		return REGISTER_VARYING_BASE + ldst->reg;
	else
		return ldst->reg;
}

/* Checks for a data hazard between two adjacent ld_st instructions to see if
 * they can run in parallel for VLIW */

static bool
can_ld_st_run_concurrent(midgard_load_store_word *first,
			 midgard_load_store_word *second)
{
	/* No registers are touched for the case of two stores, therefore no
	 * hazard. */

	if (OP_IS_STORE(first->op) && OP_IS_STORE(second->op))
		return true;

	/* If one is a load, the data hazard comes up iff the two instructions write to the same place, */

	return effective_ld_st_reg(first) != effective_ld_st_reg(second);
}

/* Checks for a data hazard between two adjacent ALU instructions */

static bool
can_alu_run_concurrent(midgard_instruction *first, midgard_instruction *second)
{
	/* Bail early for non-SSA case? */
	if (!first->uses_ssa || !second->uses_ssa) {
		printf("Non-SSA?\n");
		return false;
	}

	/* Each instruction reads some registers and writes to a register. See
	 * where the first writes */

	if (first->ssa_args.literal_out)
		return false; /* Bail */

	int source = first->ssa_args.dest;

	/* As long as the second doesn't read from the first, we're okay */
	if (second->ssa_args.src0 == source)
		return false;

	if (second->ssa_args.src1 == source && !second->ssa_args.inline_constant)
		return false;

	/* Otherwise, it's safe in that regard. Another data hazard is both
	 * writing to the same place, of course */

	if (!second->ssa_args.literal_out && second->ssa_args.dest == source)
		return false;

	/* ...That's it */
	return true;
}

/* Schedules, but does not emit, a single basic block. After scheduling, the
 * final tag and size of the block are known, which are necessary for branching
 * */

static midgard_bundle
schedule_bundle(compiler_context *ctx, midgard_instruction *ins)
{
	int instructions_emitted = 0;
	midgard_bundle bundle = { 0 };

	uint8_t tag = ins->type;

	/* Default to the instruction's tag */
	bundle.tag = tag;

	switch(ins->type) {
		case TAG_ALU_4: {
			uint32_t control = 0;
			size_t bytes_emitted = sizeof(control);
		
			/* TODO: Constant combining */
			int index = 0, last_unit = 0;
			bool has_embedded_constants = false;
			float constants[4];

			/* Previous instructions, for the purpose of parallelism */
			midgard_instruction *segment[4];
			int segment_size = 0;

			while (ins + index) {
				midgard_instruction *ains = ins + index; 

				/* Ensure that the chain can continue */
				if (ains->unused) goto skip_instruction;
				if (ains->type != TAG_ALU_4) break;

				/* According to the presentation "The ARM
				 * Mali-T880 Mobile GPU" from HotChips 27,
				 * there are two pipeline stages. Branching
				 * position determined experimentally. Lines
				 * are executed in parallel: 
				 *
				 * [ VMUL ] [ SADD ]
				 * [ VADD ] [ SMUL ] [ LUT ] [ BRANCH ]
				 *
				 * Verify that there are no ordering dependencies here.
				 *
				 * TODO: Allow for parallelism!!!
				 */

				/* Pick a unit for it if it doesn't force a particular unit */

				if (!ains->unit) {
					int op = ains->alu.op;
					int units = alu_opcode_unit[op];

					/* TODO: Promotion of scalars to vectors */
					int vector = (!is_single_component_mask(ains->alu.mask)) || ((units & UNITS_SCALAR) == 0);

					if (vector) {
						if (last_unit >= UNIT_VADD) {
							if (units & UNIT_VADD)
								ains->unit = UNIT_VADD;
							else if (units & UNIT_VLUT)
								ains->unit = UNIT_VLUT;
							else
								break;
						} else {
							if (units & UNIT_VMUL)
								ains->unit = UNIT_VMUL;
							else if (units & UNIT_VADD)
								ains->unit = UNIT_VADD;
							else if (units & UNIT_VLUT)
								ains->unit = UNIT_VLUT;
							else
								assert(0);
						}
					} else {
						if (last_unit >= UNIT_VADD) {
							if (units & UNIT_SMUL)
								ains->unit = UNIT_SMUL;
							else
								break;
						} else {
							if (units & UNIT_SADD)
								ains->unit = UNIT_SADD;
							else {
								assert(units & UNIT_SMUL);
								ains->unit = UNIT_SMUL;
							}
						}
					}
				}

				/* Late unit check, this time for encoding (not parallelism) */
				if (ains->unit <= last_unit) break;

				/* Clear the segment */
				if (last_unit < UNIT_VADD && ains->unit >= UNIT_VADD)
					segment_size = 0;

				/* Check for data hazards */
				int has_hazard = false;

				for (int s = 0; s < segment_size; ++s)
					if (!can_alu_run_concurrent(segment[s], ains))
						has_hazard = true;

				if (has_hazard)
					break;

				segment[segment_size++] = ains;

				/* Only one set of embedded constants per
				 * bundle possible; if we have more, we must
				 * break the chain early, unfortunately */

				if (ains->has_constants) {
					if (bundle.has_embedded_constants) {
						/* ...but if there are already
						 * constants but these are the
						 * *same* constants, we let it
						 * through */

						if (memcmp(bundle.constants, ains->constants, sizeof(bundle.constants)))
							break;
					} else {
						bundle.has_embedded_constants = true;
						memcpy(bundle.constants, ains->constants, sizeof(bundle.constants));
					}
				}

				if (ains->unit & UNITS_ANY_VECTOR) {
					emit_binary_vector_instruction(ains, bundle.register_words,
							&bundle.register_words_count, bundle.body_words,
							bundle.body_size, &bundle.body_words_count, &bytes_emitted);
				} else if (ains->compact_branch) {
					/* ERRATA: Workaround hardware errata
					 * where branches cannot stand alone in
					 * a word by including a dummy move */

					/* XXX: This is not the issue per se.
					 * Rather, all of r0 has to be written
					 * out along with the branch writeout.
					 * For now, emit a dummy move always
					 * (slow!) */

					if (ains->writeout) {
						if (index == 0) {
							midgard_instruction ins = v_fmov(0, blank_alu_src, 0, true, midgard_outmod_none);

							control |= ins.unit;

							emit_binary_vector_instruction(&ins, bundle.register_words,
									&bundle.register_words_count, bundle.body_words,
									bundle.body_size, &bundle.body_words_count, &bytes_emitted);
						} else {
							/* Analyse the group to see if r0 is written in full */
							bool components[4] = { 0 };

							for (int t = 0; t < index; ++t) {
								midgard_instruction *qins = ins + t;
								
								if (qins->registers.out_reg != 0) continue;

								int mask = qins->alu.mask;

								for (int c = 0; c < 4; ++c)
									if (mask & (0x3 << (2 * c)))
										components[c] = true;
							}

							/* If even a single component is not written, break it up (conservative check). */

							bool breakup = false;

							for (int c = 0; c < 4; ++c)
								if (!components[c])
									breakup = true;

							if (breakup)
								break;

							/* Otherwise, we're free to proceed */
						}
					}

					bundle.body_size[bundle.body_words_count] = sizeof(ains->br_compact);
					memcpy(&bundle.body_words[bundle.body_words_count++], &ains->br_compact, sizeof(ains->br_compact));
					bytes_emitted += sizeof(ains->br_compact);
				} else {
					/* TODO: Vector/scalar stuff operates in parallel. This is probably faulty logic */


					memcpy(&bundle.register_words[bundle.register_words_count++], &ains->registers, sizeof(ains->registers));
					bytes_emitted += sizeof(midgard_reg_info);

					bundle.body_size[bundle.body_words_count] = sizeof(midgard_scalar_alu);
					bundle.body_words_count++;
					bytes_emitted += sizeof(midgard_scalar_alu);
				}

				/* Defer marking until after writing to allow for break */
				control |= ains->unit;
				last_unit = ains->unit;

skip_instruction:
				++index;
			}

			/* Bubble up the number of instructions for skipping */
			instructions_emitted = index - 1;

			int padding = 0;

			/* Pad ALU op to nearest word */

			if (bytes_emitted & 15) {
				padding = 16 - (bytes_emitted & 15);
				bytes_emitted += padding;
			}

			/* Constants must always be quadwords */
			if (bundle.has_embedded_constants)
				bytes_emitted += 16;

			/* Size ALU instruction for tag */
			bundle.tag = (TAG_ALU_4) + (bytes_emitted / 16) - 1;
			bundle.padding = padding;
			bundle.control = bundle.tag | control;

			break;
		 }

#if 0
		case TAG_LOAD_STORE_4: {
			/* Load store instructions have two words at once. If we
			 * only have one queued up, we need to NOP pad.
			 * Otherwise, we store both in succession to save space
			 * and cycles -- letting them go in parallel -- skip
			 * the next. The usefulness of this optimisation is
			 * greatly dependent on the quality of the (presently
			 * nonexistent) instruction scheduler.
			 */

			midgard_load_store_word current = ins->load_store;

			if (IN_ARRAY(ins + 1, ctx->current_block) && ((ins + 1)->type == TAG_LOAD_STORE_4)) {
				midgard_load_store_word next = (ins + 1)->load_store;

				/* As the two operate concurrently, make sure
				 * they are not dependent */

				if (can_ld_st_run_concurrent(&current, &next)) {
					/* Skip ahead one, since it's redundant with the pair */
					instructions_emitted++;
				}
			}

			break;
		}

		case TAG_TEXTURE_4:
			/* TODO: Schedule texture ops */
			break;
#endif

		default:
			printf("Unknown midgard instruction type\n");
			break;
	}

	/* Copy the instructions into the bundle */
	bundle.instruction_count = instructions_emitted + 1;
	
	for (int i = 0; i < bundle.instruction_count; ++i)
		bundle.instructions[i] = *(ins + i);

	return bundle;
}

static int
quadword_size(int tag)
{
	switch (tag) {
		case TAG_ALU_4: 	return 1;
		case TAG_ALU_8: 	return 2;
		case TAG_ALU_12: 	return 3;
		case TAG_ALU_16: 	return 4;
		case TAG_LOAD_STORE_4: 	return 1;
		case TAG_TEXTURE_4: 	return 1;
		default: 		assert(0);
	}
}

/* Schedule a single block by iterating its instruction to create bundles.
 * While we go, tally about the bundle sizes to compute the block size. */

static void
schedule_block(compiler_context *ctx, midgard_block *block)
{
	printf("Scheduling block %p\n", block);

	/* Do RA right now since some of the registers are de facto needed by schediling XXX */
	allocate_registers(ctx, block);

	util_dynarray_init(&block->bundles, NULL);

	block->quadword_count = 0;

	util_dynarray_foreach(&block->instructions, midgard_instruction, ins) {
		if (!ins->unused) {
			midgard_bundle bundle = schedule_bundle(ctx, ins);
			util_dynarray_append(&block->bundles, midgard_bundle, bundle);
			
			ins += bundle.instruction_count - 1;
			block->quadword_count += quadword_size(bundle.tag);
		}
	}

	block->is_scheduled = true;
}

static void
schedule_program(compiler_context *ctx)
{
	util_dynarray_foreach(&ctx->blocks, midgard_block, block) {
		schedule_block(ctx, block);
	}
}

/* After everything is scheduled, emit whole bundles at a time */

static void
emit_binary_bundle(compiler_context *ctx, midgard_bundle *bundle, struct util_dynarray *emission, int next_tag)
{
	int lookahead = next_tag << 4;

	switch(bundle->tag) {
		case TAG_ALU_4:
		case TAG_ALU_8:
		case TAG_ALU_12:
		case TAG_ALU_16: {
                       /* Actually emit each component */
		       util_dynarray_append(emission, uint32_t, bundle->control | lookahead);

                       for (int i = 0; i < bundle->register_words_count; ++i)
                               util_dynarray_append(emission, uint16_t, bundle->register_words[i]);

		       /* Emit body words based on the instructions bundled */
		       for (int i = 0; i < bundle->instruction_count; ++i) {
			       midgard_instruction *ins = &bundle->instructions[i];

			       if (ins->unit & UNITS_ANY_VECTOR) {
					memcpy(util_dynarray_grow(emission, sizeof(midgard_vector_alu)), &ins->alu, sizeof(midgard_vector_alu));
			       } else if (ins->compact_branch) {
				       /* Dummy move, XXX DRY */
				       if ((i == 0) && ins->writeout) {
						midgard_instruction ins = v_fmov(0, blank_alu_src, 0, true, midgard_outmod_none);
						memcpy(util_dynarray_grow(emission, sizeof(midgard_vector_alu)), &ins.alu, sizeof(midgard_vector_alu));
				       }

					memcpy(util_dynarray_grow(emission, sizeof(ins->br_compact)), &ins->br_compact, sizeof(ins->br_compact));
			       } else {
				       /* Scalar */
 					midgard_scalar_alu scalarised = vector_to_scalar_alu(ins->alu);
					memcpy(util_dynarray_grow(emission, sizeof(scalarised)), &scalarised, sizeof(scalarised));
			       }
		       }

                       /* Emit padding */
                       util_dynarray_grow(emission, bundle->padding);

                       /* Tack on constants */

                       if (bundle->has_embedded_constants) {
			       util_dynarray_append(emission, float, bundle->constants[0]);
			       util_dynarray_append(emission, float, bundle->constants[1]);
			       util_dynarray_append(emission, float, bundle->constants[2]);
			       util_dynarray_append(emission, float, bundle->constants[3]);
                       }

			printf("Skip ALU\n");
			break;
		 }

		case TAG_LOAD_STORE_4: {
			/* One or two composing instructions */

			uint64_t current64, next64;

			memcpy(&current64, &bundle->instructions[0].load_store, sizeof(current64));
			
			if (bundle->instruction_count == 2) {
				memcpy(&next64, &bundle->instructions[1].load_store, sizeof(next64));
			} else {
				next64 = LDST_NOP;
			}

			midgard_load_store instruction = {
				.type = bundle->tag,
				.next_type = next_tag,
				.word1 = current64,
				.word2 = next64
			};

			util_dynarray_append(emission, midgard_load_store, instruction);

			break;
		}

		case TAG_TEXTURE_4: {
			/* Texture instructions are easy, since there is no
			 * pipelining nor VLIW to worry about. We may need to set the .last flag */
			
			midgard_instruction *ins = &bundle->instructions[0];

			ins->texture.type = TAG_TEXTURE_4;
			ins->texture.next_type = next_tag;

			ctx->texture_op_count--;

			if (!ctx->texture_op_count) {
				ins->texture.cont = 0;
				ins->texture.last = 1;
			}

	    		util_dynarray_append(emission, midgard_texture_word, ins->texture);
			break;
		}		

		default:
			printf("Unknown midgard instruction type\n");
			break;
	}
}


/* ALU instructions can inline or embed constants, which decreases register
 * pressure and saves space. */

#define CONDITIONAL_ATTACH(src) { \
	void *entry = _mesa_hash_table_u64_search(ctx->ssa_constants, alu->ssa_args.src + 1); \
\
	if (entry) { \
		attach_constants(alu, entry); \
		alu->ssa_args.src = SSA_FIXED_REGISTER(REGISTER_CONSTANT); \
	} \
}

static void
inline_alu_constants(compiler_context *ctx)
{
	util_dynarray_foreach(ctx->current_block, midgard_instruction, alu) {
		/* Other instructions cannot inline constants */
		if (alu->type != TAG_ALU_4) continue;

		/* If there is already a constant here, we can do nothing */
		if (alu->has_constants) continue;

		/* Constants should always be SSA... */
		if (!alu->uses_ssa) continue;

		CONDITIONAL_ATTACH(src0);

		if (!alu->ssa_args.inline_constant)
			CONDITIONAL_ATTACH(src1);
	}
}

/* Similarly, for varyings we have to emit a move (due to decay), but we can often inline it */

static void
eliminate_varying_mov(compiler_context *ctx)
{
	util_dynarray_foreach(ctx->current_block, midgard_instruction, move) {
		/* Only interest ourselves with fmov instructions */
		
		if (move->type != TAG_ALU_4) continue;
		if (move->alu.op != midgard_alu_op_fmov) continue;
		if (!move->ssa_args.literal_out) continue;
		if ((move->ssa_args.dest & ~1) != REGISTER_VARYING_BASE) continue;

		int source = move->ssa_args.src1;

		/* Scan the succeeding instructions for usage */

		bool used = false;

		for (midgard_instruction *candidate = ctx->current_block->data;
		     IN_ARRAY(candidate, ctx->current_block);
		     candidate += 1) {
			/* If not using SSA, the sources are meaningless here */
			if (!candidate->uses_ssa) continue;
			
			/* Tonto special case but yeah */
			if (candidate == move) continue;

			/* Check this candidate for usage */

			if (candidate->ssa_args.src0 == source ||
			    candidate->ssa_args.src1 == source) {
				used = true;
				break;
			}
		}

		/* At this point, we know if the move is used or not. If it's
		 * not, inline it! */

		if (!used) {
			for (midgard_instruction *candidate = (move - 1);
			     candidate >= ctx->current_block->data;
			     candidate -= 1) {
				if (!candidate->uses_ssa) continue;

				if (candidate->ssa_args.literal_out) continue;

				if (candidate->ssa_args.dest == source) {
					candidate->ssa_args.dest = move->ssa_args.dest;
					candidate->ssa_args.literal_out = true;
					move->unused = true;
				}
			}
		}
	}
}

/* Midgard supports two types of constants, embedded constants (128-bit) and
 * inline constants (16-bit). Sometimes, especially with scalar ops, embedded
 * constants can be demoted to inline constants, for space savings and
 * sometimes a performance boost */

static void
embedded_to_inline_constant(compiler_context *ctx)
{
	util_dynarray_foreach(ctx->current_block, midgard_instruction, ins) {
		if (!ins->has_constants) continue;
		if (ins->ssa_args.inline_constant) continue;
		if (ins->unused) continue;

		/* src1 cannot be an inline constant due to encoding
		 * restrictions. So, if possible we try to flip the arguments
		 * in that case */

		int op = ins->alu.op;

		if (ins->ssa_args.src0 == SSA_FIXED_REGISTER(REGISTER_CONSTANT)) {
			/* Flip based on op. Fallthrough intentional */

			switch (op) {
				/* These ops require an operational change to flip their arguments TODO */
				case midgard_alu_op_flt: 
				case midgard_alu_op_fle: 
				case midgard_alu_op_ilt: 
				case midgard_alu_op_ile: 
				case midgard_alu_op_fcsel: 
				case midgard_alu_op_icsel: 
				case midgard_alu_op_isub: 
					printf("Missed non-commutative flip\n");
					break;

				/* These ops are commutative and Just Flip */
				case midgard_alu_op_fne: 
				case midgard_alu_op_fadd: 
				case midgard_alu_op_fmul: 
				case midgard_alu_op_fmin: 
				case midgard_alu_op_fmax: 
				case midgard_alu_op_iadd: 
				case midgard_alu_op_imul: 
				case midgard_alu_op_feq: 
				case midgard_alu_op_ieq: 
				case midgard_alu_op_ine: 
				case midgard_alu_op_iand:
				case midgard_alu_op_ior:
				case midgard_alu_op_ixor:
					/* Flip the SSA numbers */
					ins->ssa_args.src0 = ins->ssa_args.src1;
					ins->ssa_args.src1 = SSA_FIXED_REGISTER(REGISTER_CONSTANT);

					/* And flip the modifiers */

					unsigned src_temp;

					src_temp = ins->alu.src2;
					ins->alu.src2 = ins->alu.src1;
					ins->alu.src1 = src_temp;
				default:
					break;
			}
		}

		if (ins->ssa_args.src1 == SSA_FIXED_REGISTER(REGISTER_CONSTANT)) {
			int component = 0;

			/* Scale constant appropriately, if we can legally */
			uint16_t scaled_constant = 0;

			/* XXX: Check legality */
			if (midgard_is_integer_op(op)) {
				unsigned int *iconstants = (unsigned int *) ins->constants;
				scaled_constant = (uint16_t) iconstants[component];

				/* Constant overflow after resize */
				if (scaled_constant != iconstants[component])
				       continue;
			} else {
				scaled_constant = _mesa_float_to_half((float) ins->constants[component]);
			}

			midgard_vector_alu_src *src;
			int q = ins->alu.src2;
			midgard_vector_alu_src *m = (midgard_vector_alu_src *) &q;
			src = m;

			assert(!src->abs);
			assert(!src->negate);
			assert(!src->half);
			assert(!src->rep_low);
			assert(!src->rep_high);

			/* Make sure that the constant is not itself a
			 * vector by checking if all accessed values
			 * (by the swizzle) are the same. */

			uint32_t *cons = (uint32_t *) ins->constants;
			uint32_t value = cons[src->swizzle & 3];

			bool is_vector = false;

			for (int c = 1; c < 4; ++c) {
				uint32_t test = cons[(src->swizzle >> (2 * c)) & 3];
				
				if (test != value) {
					is_vector = true;
					break;
				}
			}

			if (is_vector)
				continue;

			/* Get rid of the embedded constant */
			ins->has_constants = false; 
			ins->ssa_args.inline_constant = true;
			ins->ssa_args.src1 = scaled_constant;
		}
	}
}

/* Map normal SSA sources to other SSA sources / fixed registers (like
 * uniforms) */

static void
map_ssa_to_alias(compiler_context *ctx, int *ref)
{
	unsigned int alias = (unsigned int) _mesa_hash_table_u64_search(ctx->ssa_to_alias, *ref + 1);
	
	if (alias) {
		/* Remove entry in leftovers to avoid a redunant fmov */

		struct set_entry *leftover = _mesa_set_search(ctx->leftover_ssa_to_alias, ((void *) (uintptr_t) (*ref + 1)));

		if (leftover)
			_mesa_set_remove(ctx->leftover_ssa_to_alias, leftover);

		/* Assign the alias map */
		*ref = alias - 1;
		return;
	}

	alias = _mesa_hash_table_u64_search(ctx->register_to_ssa, (*ref) + 1);

	if (alias) {
		*ref = alias - 1;
		return;
	}
}

/* If there are leftovers after the below pass, emit actual fmov
 * instructions for the slow-but-correct path */

static void
emit_leftover_move(compiler_context *ctx)
{
	struct set_entry *leftover;

	set_foreach(ctx->leftover_ssa_to_alias, leftover) {
		int base = ((uintptr_t) leftover->key) - 1;
		int mapped = base;

		map_ssa_to_alias(ctx, &mapped);
		EMIT(fmov, mapped, blank_alu_src, base, false, midgard_outmod_none);
	}
}

static void
actualise_ssa_to_alias(compiler_context *ctx)
{
	util_dynarray_foreach(ctx->current_block, midgard_instruction, ins) {
		if (!ins->uses_ssa) continue;

		map_ssa_to_alias(ctx, &ins->ssa_args.src0);
		map_ssa_to_alias(ctx, &ins->ssa_args.src1);
	}

	emit_leftover_move(ctx);
}

/* Sort of opposite of the above */

static void
actualise_register_to_ssa(compiler_context *ctx)
{
	util_dynarray_foreach(ctx->current_block, midgard_instruction, ins) {
		uintptr_t reg = (uintptr_t) _mesa_hash_table_u64_search(ctx->register_to_ssa, ins->ssa_args.dest + 1);

		if (reg) {
			ins->ssa_args.dest = reg - 1;
			ins->ssa_args.literal_out = true;
		}
	}
}

/* Vertex shaders do not write gl_Position as is; instead, they write a
 * transformed screen space position as a varying. See section 12.5 "Coordinate
 * Transformation" of the ES 3.2 full specification for details.
 *
 * This transformation occurs early on, as NIR and prior to optimisation, in
 * order to take advantage of NIR optimisation passes of the transform itself.
 * */

static void
write_transformed_position(nir_builder *b, nir_src input_point_src, int uniform_no)
{
	nir_ssa_def *input_point = nir_ssa_for_src(b, input_point_src, 4);

	/* Get viewport from the uniforms */
	nir_intrinsic_instr *load;
	load = nir_intrinsic_instr_create(b->shader, nir_intrinsic_load_uniform);
	load->num_components = 4;
	load->src[0] = nir_src_for_ssa(nir_imm_int(b, uniform_no));
	nir_ssa_dest_init(&load->instr, &load->dest, 4, 32, NULL);
	nir_builder_instr_insert(b, &load->instr);
	
	/* Formatted as <width, height, centerx, centery> */
	nir_ssa_def *viewport_vec4 = &load->dest.ssa;
	nir_ssa_def *viewport_width = nir_channel(b, viewport_vec4, 0);
	nir_ssa_def *viewport_height = nir_channel(b, viewport_vec4, 1);
	nir_ssa_def *viewport_offset = nir_channels(b, viewport_vec4, 0x8 | 0x4);
#if 0

	nir_ssa_def *viewport_width = nir_imm_float(b, 2048);
	nir_ssa_def *viewport_height = nir_imm_float(b, 1280);
	nir_ssa_def *viewport_offset = nir_vec2(b, nir_imm_float(b, 1024), nir_imm_float(b, 640));
#endif

	/* XXX: From uniforms? */
	nir_ssa_def *depth_near = nir_imm_float(b, 0.0);
	nir_ssa_def *depth_far = nir_imm_float(b, 1.0);

	/* World space to normalised device coordinates */

	nir_ssa_def *w_recip = nir_frcp(b, nir_channel(b, input_point, 3));
	nir_ssa_def *ndc_point = nir_fmul(b, nir_channels(b, input_point, 0x7), w_recip);

	/* Normalised device coordinates to screen space */

	nir_ssa_def *viewport_multiplier = nir_vec2(b,
			nir_fmul(b, viewport_width, nir_imm_float(b, 0.5f)),
			nir_fmul(b, viewport_height, nir_imm_float(b, 0.5f)));

	nir_ssa_def *viewport_xy = nir_fadd(b, nir_fmul(b, nir_channels(b, ndc_point, 0x3), viewport_multiplier), viewport_offset);

	nir_ssa_def *depth_multiplier = nir_fmul(b, nir_fsub(b, depth_far, depth_near), nir_imm_float(b, 0.5f));
	nir_ssa_def *depth_offset     = nir_fmul(b, nir_fadd(b, depth_far, depth_near), nir_imm_float(b, 0.5f));
	nir_ssa_def *screen_depth     = nir_fadd(b, nir_fmul(b, nir_channel(b, ndc_point, 2), depth_multiplier), depth_offset);

	nir_ssa_def *screen_space = nir_vec4(b, 
			nir_channel(b, viewport_xy, 0),
			nir_channel(b, viewport_xy, 1),
			screen_depth,
			nir_imm_float(b, 0.0));

	/* Finally, write out the transformed values to the varying */

	nir_intrinsic_instr *store;
	store = nir_intrinsic_instr_create(b->shader, nir_intrinsic_store_output);
	store->num_components = 4;
	nir_intrinsic_set_base(store, 0);
	nir_intrinsic_set_write_mask(store, 0xf);
	store->src[0].ssa = screen_space;
	store->src[0].is_ssa = true;
	store->src[1] = nir_src_for_ssa(nir_imm_int(b, 0));
	nir_builder_instr_insert(b, &store->instr);
}

static void
transform_position_writes(nir_shader *shader)
{
	nir_foreach_function(func, shader) {
		nir_foreach_block(block, func->impl) {
			nir_foreach_instr_safe(instr, block) {
				if (instr->type != nir_instr_type_intrinsic) continue;

				nir_intrinsic_instr *intr = nir_instr_as_intrinsic(instr);
				nir_variable *out = NULL;

				switch (intr->intrinsic) {
					case nir_intrinsic_store_output:
						/* already had i/o lowered.. lookup the matching output var: */
						printf("SO\n");
						nir_foreach_variable(var, &shader->outputs) {
							int drvloc = var->data.driver_location;
							printf("Base %d, loc %d\n", nir_intrinsic_base(intr), drvloc);
							if (nir_intrinsic_base(intr) == drvloc) {
								printf("Var: %p\n", var);
								out = var;
								break;
							}
						}

						break;
					default:
						break;
				}

				if (!out) continue;
				printf("Out: %p\n", out);

				if (out->data.mode != nir_var_shader_out)
					continue;

				if (out->data.location != VARYING_SLOT_POS)
					continue;

				nir_builder b;
				nir_builder_init(&b, func->impl);
				b.cursor = nir_before_instr(instr);

				write_transformed_position(&b, intr->src[0], UNIFORM_VIEWPORT);
				nir_instr_remove(instr);
			}
		}
	}
}

static void
emit_fragment_epilogue(compiler_context *ctx)
{
	/* See the docs for why this works. TODO: gl_FragDepth */

	EMIT(alu_br_compact_cond, midgard_jmp_writeout_op_writeout, TAG_ALU_4, 0, COND_FBWRITE);
	EMIT(alu_br_compact_cond, midgard_jmp_writeout_op_writeout, TAG_ALU_4, -1, COND_FBWRITE);
}

static midgard_block *
emit_block(compiler_context *ctx, nir_block *block)
{
	midgard_block this_block;
	this_block.is_scheduled = false;
	++ctx->block_count;

	/* Set up current block */
	util_dynarray_init(&this_block.instructions, NULL);
	ctx->current_block = &this_block.instructions;

	ctx->ssa_constants = _mesa_hash_table_u64_create(NULL); 
	ctx->ssa_to_alias = _mesa_hash_table_u64_create(NULL); 
	ctx->register_to_ssa = _mesa_hash_table_u64_create(NULL); 
	ctx->ssa_to_register = _mesa_hash_table_u64_create(NULL); 
	ctx->leftover_ssa_to_alias = _mesa_set_create(NULL, _mesa_hash_pointer, _mesa_key_pointer_equal);

	nir_foreach_instr(instr, block) {
		emit_instr(ctx, instr);
	}

	inline_alu_constants(ctx);
	//embedded_to_inline_constant(ctx);

	//eliminate_varying_mov(ctx);

	/* Perform heavylifting for aliasing */
	actualise_ssa_to_alias(ctx);
	actualise_register_to_ssa(ctx);

	/* Append fragment shader epilogue (value writeout) */
	if (ctx->stage == MESA_SHADER_FRAGMENT)
		if (block == nir_impl_last_block(ctx->func->impl))
			emit_fragment_epilogue(ctx);

	/* Fallthrough save */
	this_block.next_fallthrough = ctx->previous_source_block;

	/* Save the block. Initial and final may be the same. */
	util_dynarray_append(&ctx->blocks, midgard_block, this_block);

	midgard_block *block_ptr = util_dynarray_top_ptr(&ctx->blocks, midgard_block);
	printf("Pushed up %p\n", block_ptr);

	if (block == nir_start_block(ctx->func->impl))
		ctx->initial_block = block_ptr;

	if (block == nir_impl_last_block(ctx->func->impl))
		ctx->final_block = block_ptr;

	/* Allow the next control flow to access us retroactively, for
	 * branching etc */
	ctx->current_block = &block_ptr->instructions;
	
	/* Document the fallthrough chain */
	ctx->previous_source_block = block_ptr;

	return block_ptr;
}

static midgard_block *emit_cf_list(struct compiler_context *ctx, struct exec_list *list);

static void
emit_if(struct compiler_context *ctx, nir_if *nif)
{
	/* Conditional branches expect the condition in r31.w; emit a move for
	 * that in the _previous_ block (which is the current block). */
	printf("Current %p\n", ctx->current_block);
	emit_condition(ctx, &nif->condition);

	/* Speculatively emit the branch, but we can't fill it in until later */
	EMIT(branch, true, true);
	midgard_instruction *then_branch = util_dynarray_top_ptr(ctx->current_block, midgard_instruction);
	printf("count %d\n", ctx->block_count);

	/* Emit the two subblocks */
	midgard_block *then_block = emit_cf_list(ctx, &nif->then_list);
	printf("then %d\n", ctx->block_count);

	/* Emit a jump from the end of the then block to the end of the else */
	EMIT(branch, false, false);
	midgard_instruction *then_exit = util_dynarray_top_ptr(ctx->current_block, midgard_instruction);

	int else_idx = ctx->block_count;
	midgard_block *else_block = emit_cf_list(ctx, &nif->else_list);

	/* Now that we have the subblocks emitted, fix up the branch */

	assert(then_block);
	assert(else_block);

	printf("ELSE IDX %d\n", else_idx);
	then_branch->branch.target_start = else_idx;
	then_exit->branch.target_start = else_idx + 1;
}

static void
emit_loop(struct compiler_context *ctx, nir_loop *nloop)
{
	emit_cf_list(ctx, &nloop->body);
}

static midgard_block *
emit_cf_list(struct compiler_context *ctx, struct exec_list *list)
{
	midgard_block *start_block;

	foreach_list_typed(nir_cf_node, node, node, list) {
		switch (node->type) {
		case nir_cf_node_block: {
			midgard_block *block = emit_block(ctx, nir_cf_node_as_block(node));

			if (!start_block)
				start_block = block;

			break;
		}
		case nir_cf_node_if:
			emit_if(ctx, nir_cf_node_as_if(node));
			break;
		case nir_cf_node_loop:
			emit_loop(ctx, nir_cf_node_as_loop(node));
			break;
		case nir_cf_node_function:
			assert(0);
			break;
		}
	}

	return start_block;
}

int
midgard_compile_shader_nir(nir_shader *nir, struct util_dynarray *compiled)
{
	bool progress;

	compiler_context ictx = {
		.stage = nir->info.stage
	};

	compiler_context *ctx = &ictx;

	switch (ctx->stage) {
		case MESA_SHADER_VERTEX:
			ctx->special_uniforms = 1;
			break;
		default:
			ctx->special_uniforms = 0;
			break;
	}

	/* Append epilogue uniforms if necessary. The cmdstream depends on
	 * these being at the -end-; see assign_var_locations. */

        if (ctx->stage == MESA_SHADER_VERTEX) {
                nir_variable_create(nir, nir_var_uniform, glsl_vec4_type(), "viewport");
        }


	/* Assign var locations early, so the epilogue can use them if necessary */

	nir_assign_var_locations(&nir->outputs, &nir->num_outputs, glsl_type_size);
	nir_assign_var_locations(&nir->inputs, &nir->num_inputs, glsl_type_size);
	nir_assign_var_locations(&nir->uniforms, &nir->num_uniforms, glsl_type_size);

	/* Lower vars -- not I/O -- before epilogue */

	NIR_PASS(progress, nir, nir_lower_var_copies);
	NIR_PASS(progress, nir, nir_lower_vars_to_ssa);
	NIR_PASS(progress, nir, nir_split_var_copies);
	NIR_PASS(progress, nir, nir_lower_var_copies);
	NIR_PASS(progress, nir, nir_lower_global_vars_to_local);
	NIR_PASS(progress, nir, nir_lower_var_copies);
	NIR_PASS(progress, nir, nir_lower_vars_to_ssa);

	/* Append vertex epilogue before optimisation, so the epilogue itself
	 * is optimised */

	if (ctx->stage == MESA_SHADER_VERTEX)
		transform_position_writes(nir);

	/* Optimisation passes */

#ifdef NIR_DEBUG
	nir_print_shader(nir, stdout);
#endif

	optimise_nir(nir);

#ifdef NIR_DEBUG
	nir_print_shader(nir, stdout);
#endif

	nir_foreach_function(func, nir) {
		if (!func->impl)
			continue;

		util_dynarray_init(&ctx->blocks, NULL);
		ctx->block_count = 0;
		ctx->func = func;

		emit_cf_list(ctx, &func->impl->body);
		emit_block(ctx, func->impl->end_block);

		break; /* TODO: Multi-function shaders */
	}

	util_dynarray_init(compiled, NULL);

	/* ERRATA: Workaround hardware errata where shaders must start with a
	 * load/store instruction by adding a noop load */

	int first_tag = 0;

	util_dynarray_foreach(&ctx->initial_block->instructions, midgard_instruction, ins) {
		if (!ins->unused) {
			first_tag = ins->type;
			break;
		}
	}

	if (unlikely(first_tag != TAG_LOAD_STORE_4)) {
		midgard_load_store instruction = {
			.type = TAG_LOAD_STORE_4,
			.word1 = 3,
			.word2 = 3
		};

		util_dynarray_append(compiled, midgard_load_store, instruction);
	}

	/* Schedule! */
	schedule_program(ctx);

	/* Now that all the bundles are schedule and we can calculate block
	 * sizes, emit actual branch instructions rather than placeholders */

	int br_block_idx = 0;

	util_dynarray_foreach(&ctx->blocks, midgard_block, block) {
		printf("Scheduled? %d\n", block->is_scheduled);
		util_dynarray_foreach(&block->bundles, midgard_bundle, bundle) {
			for (int c = 0; c < bundle->instruction_count; ++c) {
				midgard_instruction *ins = &bundle->instructions[c];

				if (ins->unused) continue;
				if (ins->unit != ALU_ENAB_BR_COMPACT) continue;
				if (ins->prepacked_branch) continue;

				uint16_t compact;

				/* Determine the block we're jumping to */
				int target_number = ins->branch.target_start;
				printf("Jumping from %d to %d\n", br_block_idx, target_number);

				midgard_block *target = util_dynarray_element(&ctx->blocks, midgard_block, target_number);
				assert(target);

				/* Determine the destination tag */
				midgard_bundle *first = util_dynarray_element(&target->bundles, midgard_bundle, 0);
				assert(first);

				int dest_tag = first->tag;
				printf("Dest tag: %X\n", dest_tag);

				/* Count up the number of quadwords we're jumping over. That is, the number of quadwords in each of the blocks between (br_block_idx, target_number) */
				assert(target_number > br_block_idx); /* TODO: Jumps backwards */

				int quadword_offset = 0;

				for (int idx = br_block_idx + 1; idx < target_number; ++idx) {
					midgard_block *blk = util_dynarray_element(&ctx->blocks, midgard_block, idx);
					assert(blk);
					
					quadword_offset += blk->quadword_count;
				}

				if (ins->branch.conditional) {
					midgard_branch_cond branch = {
						.op = midgard_jmp_writeout_op_branch_cond,
						.dest_tag = dest_tag,
						.offset = quadword_offset,
						.cond = ins->branch.invert_conditional ? 1 : 2
					};

					memcpy(&compact, &branch, sizeof(branch));
				} else {
					midgard_branch_uncond branch = {
						.op = midgard_jmp_writeout_op_branch_uncond,
						.dest_tag = dest_tag, 
						.offset = quadword_offset, 
						.unknown = 1
					};

					memcpy(&compact, &branch, sizeof(branch));
				}

				/* Swap in the generic branch for our actual branch */
				ins->unit = ALU_ENAB_BR_COMPACT;
				ins->br_compact = compact;
			}

		}

		++br_block_idx;
	}

	/* Emit flat binary from the instruction arrays. Iterate each block in
	 * sequence. Save instruction boundaries such that lookahead tags can
	 * be assigned easily */

	/* Cache _all_ bundles in source order for lookahead across failed branches */

	int bundle_count = 0;
	util_dynarray_foreach(&ctx->blocks, midgard_block, block) {
		bundle_count += block->bundles.size / sizeof(midgard_bundle);
	}
	printf("Bundle count %d\n", bundle_count);
	midgard_bundle **source_order_bundles = malloc(sizeof(midgard_bundle*) * bundle_count);
	int bundle_idx = 0;
	util_dynarray_foreach(&ctx->blocks, midgard_block, block) {
		util_dynarray_foreach(&block->bundles, midgard_bundle, bundle) {
			source_order_bundles[bundle_idx++] = bundle;
		}
	}
		

	int current_bundle = 0;

	util_dynarray_foreach(&ctx->blocks, midgard_block, block) {
		printf("Emit block\n");
		util_dynarray_foreach(&block->bundles, midgard_bundle, bundle) {
			int lookahead = 1;

			if (current_bundle + 1 < bundle_count) {
				uint8_t next = source_order_bundles[current_bundle + 1]->tag;

				if (!(current_bundle + 2 < bundle_count) && IS_ALU(next)) {
					lookahead = 1;
				} else {
					lookahead = next;
				}
			}

			emit_binary_bundle(ctx, bundle, compiled, lookahead);
			++current_bundle;
		}

		/* TODO: Free deeper */
		util_dynarray_fini(block);
	}

	/* TODO: Propagate compiled code up correctly */
	return 0;
}


