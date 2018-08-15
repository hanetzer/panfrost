/*
 * © Copyright 2017-2018 The Panfrost Community
 *
 * This program is free software and is provided to you under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation, and any use by you of this program is subject to the terms
 * of such GNU licence.
 *
 * A copy of the licence is included with the program, and can also be obtained
 * from Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301, USA.
 *
 */

#ifndef __MALI_JOB_H__
#define __MALI_JOB_H__

#include <mali-ioctl.h>

#define MALI_SHORT_PTR_BITS (sizeof(uintptr_t)*8)

#define MALI_FBD_HIERARCHY_WEIGHTS 8

#define MALI_PAYLOAD_SIZE 256

enum mali_job_type {
	JOB_NOT_STARTED	= 0,
	JOB_TYPE_NULL = 1,
	JOB_TYPE_SET_VALUE = 2,
	JOB_TYPE_CACHE_FLUSH = 3,
	JOB_TYPE_COMPUTE = 4,
	JOB_TYPE_VERTEX = 5,
	JOB_TYPE_TILER = 7,
	JOB_TYPE_FUSED = 8,
	JOB_TYPE_FRAGMENT = 9,
};

enum mali_gl_mode {
	MALI_GL_POINTS         = 0x1,
	MALI_GL_LINES          = 0x2,
	MALI_GL_LINE_STRIP     = 0x4,
	MALI_GL_LINE_LOOP      = 0x6,
	MALI_GL_TRIANGLES      = 0x8,
	MALI_GL_TRIANGLE_STRIP = 0xA,
	MALI_GL_TRIANGLE_FAN   = 0xC,
};

#define MALI_GL_CULL_FACE_BACK  0x80
#define MALI_GL_CULL_FACE_FRONT 0x40

#define MALI_GL_FRONT_FACE(v) (v << 5)
#define MALI_GL_CCW (0)
#define MALI_GL_CW  (1)

/* TODO: Might this actually be a finer bitfield? */
#define MALI_DEPTH_STENCIL_ENABLE 0x6400

#define DS_ENABLE(field) \
	(field == MALI_DEPTH_STENCIL_ENABLE) \
	? "MALI_DEPTH_STENCIL_ENABLE" \
	: (field == 0) ? "0" \
	: "0 /* XXX: Unknown, check hexdump */"

/* Used in stencil and depth tests */

enum mali_func {
	MALI_FUNC_NEVER    = 0,
	MALI_FUNC_LESS     = 1,
	MALI_FUNC_EQUAL    = 2,
	MALI_FUNC_LEQUAL   = 3,
	MALI_FUNC_GREATER  = 4,
	MALI_FUNC_NOTEQUAL = 5,
	MALI_FUNC_GEQUAL   = 6,
	MALI_FUNC_ALWAYS   = 7
};

/* Same OpenGL, but mixed up. Why? Because forget me, that's why! */

enum mali_alt_func {
	MALI_ALT_FUNC_NEVER    = 0,
	MALI_ALT_FUNC_GREATER  = 1,
	MALI_ALT_FUNC_EQUAL    = 2,
	MALI_ALT_FUNC_GEQUAL   = 3,
	MALI_ALT_FUNC_LESS     = 4,
	MALI_ALT_FUNC_NOTEQUAL = 5,
	MALI_ALT_FUNC_LEQUAL   = 6,
	MALI_ALT_FUNC_ALWAYS   = 7
};

/* Flags apply to unknown2_3? */

#define MALI_HAS_MSAA		(1 << 0)
#define MALI_CAN_DISCARD 	(1 << 5)
#define MALI_HAS_BLEND_SHADER 	(1 << 6)

/* func is mali_func */
#define MALI_DEPTH_FUNC(func)	   (func << 8)
#define MALI_GET_DEPTH_FUNC(flags) ((flags >> 8) & 0x7)
#define MALI_DEPTH_FUNC_MASK	   MALI_DEPTH_FUNC(0x7)
 
#define MALI_DEPTH_TEST		(1 << 11)

/* Next flags to unknown2_4 */
#define MALI_STENCIL_TEST      	(1 << 0)

/* What?! */
#define MALI_SAMPLE_ALPHA_TO_COVERAGE_NO_BLEND_SHADER (1 << 1)

#define MALI_NO_DITHER		(1 << 9)
#define MALI_DEPTH_RANGE_A	(1 << 12)
#define MALI_DEPTH_RANGE_B	(1 << 13)
#define MALI_NO_MSAA		(1 << 14)

/* Stencil test state is all encoded in a single u32, just with a lot of
 * enums... */

enum mali_stencil_op {
	MALI_STENCIL_KEEP 	= 0,
	MALI_STENCIL_REPLACE 	= 1,
	MALI_STENCIL_ZERO 	= 2,
	MALI_STENCIL_INVERT 	= 3,
	MALI_STENCIL_INCR_WRAP 	= 4,
	MALI_STENCIL_DECR_WRAP 	= 5,
	MALI_STENCIL_INCR 	= 6,
	MALI_STENCIL_DECR 	= 7
};

struct mali_stencil_test {
	unsigned ref  			: 8;
	unsigned mask 			: 8;
	enum mali_func func 		: 3;
	enum mali_stencil_op sfail 	: 3;
	enum mali_stencil_op dpfail 	: 3;
	enum mali_stencil_op dppass 	: 3;
	unsigned zero			: 4;
} __attribute__((packed));

/* Blending is a mess, since anything fancy triggers a blend shader, and
 * -those- are not understood whatsover yet */

#define MALI_MASK_R (1 << 0)
#define MALI_MASK_G (1 << 1)
#define MALI_MASK_B (1 << 2)
#define MALI_MASK_A (1 << 3)

enum mali_nondominant_mode {
	MALI_BLEND_NON_MIRROR = 0,
	MALI_BLEND_NON_ZERO = 1
};

enum mali_dominant_blend {
	MALI_BLEND_DOM_SOURCE = 0,
	MALI_BLEND_DOM_DESTINATION  = 1
};

enum mali_dominant_factor {
	MALI_DOMINANT_UNK0 = 0,
	MALI_DOMINANT_ZERO = 1,
	MALI_DOMINANT_SRC_COLOR = 2,
	MALI_DOMINANT_DST_COLOR = 3,
	MALI_DOMINANT_UNK4 = 4,
	MALI_DOMINANT_SRC_ALPHA = 5,
	MALI_DOMINANT_DST_ALPHA = 6,
	MALI_DOMINANT_UNK7 = 7,
};

enum mali_blend_modifier {
	MALI_BLEND_MOD_UNK0 = 0,
	MALI_BLEND_MOD_NORMAL = 1,
	MALI_BLEND_MOD_SOURCE_ONE = 2,
	MALI_BLEND_MOD_DEST_ONE = 3,
};

struct mali_blend_mode {
	enum mali_blend_modifier clip_modifier : 2;
	unsigned unused_0 : 1;
	unsigned negate_source : 1;

	enum mali_dominant_blend dominant : 1;

	enum mali_nondominant_mode nondominant_mode : 1;

	unsigned unused_1 : 1;

	unsigned negate_dest : 1;

	enum mali_dominant_factor dominant_factor : 3;
	unsigned complement_dominant : 1;
} __attribute__((packed));

struct mali_blend_equation {
	/* Of type mali_blend_mode */
	unsigned rgb_mode : 12;
	unsigned alpha_mode : 12;

	unsigned zero1 : 4;

	/* Corresponds to MALI_MASK_* above and glColorMask arguments */

	unsigned color_mask : 4;

	unsigned padding   : 32;
} __attribute__((packed));

/* Alpha coverage is encoded as 4-bits (from a clampf), with inversion
 * literally performing a bitwise invert. This function produces slightly wrong
 * results and I'm not sure why; some rounding issue I suppose... */

#define MALI_ALPHA_COVERAGE(clampf) ((uint16_t) (int) (clampf * 15.0f))
#define MALI_GET_ALPHA_COVERAGE(nibble) ((float) nibble / 15.0f)

/* Applies to unknown1 */
#define MALI_NO_ALPHA_TO_COVERAGE (1 << 10)

struct mali_tripipe {
	mali_ptr shader;

	u16 texture_count; 
	u16 sampler_count;

	/* Counted as number of address slots (i.e. half-precision vec4's) */
	u16 attribute_count;
	u16 varying_count;

	/* 0x200 except MALI_NO_ALPHA_TO_COVERAGE. Mysterious 1 other times. Who knows really? */
	u16 unknown1; 

	 /* Whole number of uniform registers used, times two; whole number of
	  * work registers used (no scale). 
	 */

	unsigned work_count : 5;
	unsigned uniform_count : 5;
	unsigned unknown2 : 6;
} __attribute__((packed));

struct mali_fragment_core {
	/* Depth factor is exactly as passed to glDepthOffset. Depth units is
	 * equal to the value passed to glDeptOhffset + 1.0f (use
	 * MALI_NEGATIVE) */

	float depth_units;
	float depth_factor;

	u32 unknown2_2;

	u16 alpha_coverage;
	u16 unknown2_3;

	u8 stencil_mask_front;
	u8 stencil_mask_back;
	u16 unknown2_4;

	struct mali_stencil_test stencil_front;
	struct mali_stencil_test stencil_back;

	u32 unknown2_7;
	u32 unknown2_8;

	/* Check for MALI_HAS_BLEND_SHADER to decide how to interpret */

	union {
		mali_ptr blend_shader;

		/* Exact format of this is not known yet */
		struct mali_blend_equation blend_equation;
	};
} __attribute__((packed));

/* See the presentations about Mali architecture for why these are together like this */

struct mali_shader_meta {
	struct mali_tripipe tripipe;
	struct mali_fragment_core fragment_core;
} __attribute__((packed));

/* This only concerns hardware jobs */

/* Possible values for job_descriptor_size */

#define MALI_JOB_32 0
#define MALI_JOB_64 1

struct mali_job_descriptor_header {
	u32 exception_status;
	u32 first_incomplete_task;
	u64 fault_pointer;
	u8 job_descriptor_size : 1;
	enum mali_job_type job_type : 7;
	u8 job_barrier : 1;
	u8 unknown_flags : 7;
	u16 job_index;
	u16 job_dependency_index_1;
	u16 job_dependency_index_2;
	
	union {
		u64 next_job_64;
		u32 next_job_32;
	};
} __attribute__((packed));

struct mali_payload_set_value {
	u64 out;
	u64 unknown;
} __attribute__((packed));

/* Special attributes have a fixed index */
#define MALI_SPECIAL_ATTRIBUTE_BASE 16
#define MALI_VERTEX_ID   (MALI_SPECIAL_ATTRIBUTE_BASE + 0)
#define MALI_INSTANCE_ID (MALI_SPECIAL_ATTRIBUTE_BASE + 1)

struct mali_attr {
	mali_ptr elements;
	u32 stride;
	u32 size;
} __attribute__((packed));

/* TODO: I'm pretty sure this isn't really right in the presence of more
 * complicated metadata, like matrices or varyings */

enum mali_attr_type {
	MALI_ATYPE_PACKED = 1,
	MALI_ATYPE_UNK1 = 1,
	MALI_ATYPE_BYTE = 3,
	MALI_ATYPE_SHORT = 4,
	MALI_ATYPE_INT = 5,
	MALI_ATYPE_GPVARYING = 6,
	MALI_ATYPE_FLOAT = 7,
};

struct mali_attr_meta {
	/* Vertex buffer index */
	u8 index;

	u64 unknown1 :14;

	/* Part of the type specifier, anyway:
	 * 1: packed (with other encoding weirdness)
	 * 3: byte
	 * 4: short
	 * 5: int
	 * 6: used for float gl_Position varying?
	 * 7: half, float, packed
	 */

	unsigned type : 3;

	/* After MALI_POSITIVE, 4 for vec4, 1 for scalar, etc */
	unsigned nr_components : 2;

	/* Somewhat correlated to the opposite of not_normalised, or the opposite of is_half_float? */
	unsigned unknown2 : 1;

	/* If the type is a signed integer, is_int_signed is set. If the type
	 * is a half-float, it's also set. Otherwise, it is clear. */

	unsigned is_int_signed : 1;

	/* if `normalized` passed to VertexAttribPointer is clear */
	unsigned not_normalised : 1;

	/* Always observed to be zero at the moment */
	unsigned unknown3 : 2;

	/* When packing multiple attributes in a buffer, offset addresses by this value */
	uint32_t src_offset;
} __attribute__((packed));
ASSERT_SIZEOF_TYPE(struct mali_attr_meta,
		   sizeof(u64), sizeof(u64));

enum mali_fbd_type {
	MALI_SFBD = 0,
	MALI_MFBD = 1,
};

#define FBD_TYPE (1)
#define FBD_MASK (~0x3f)

/* Applies to unknown_draw */
#define MALI_DRAW_INDEXED_UINT8  (0x10)
#define MALI_DRAW_INDEXED_UINT16 (0x20)
#define MALI_DRAW_INDEXED_UINT32 (0x30)

struct mali_payload_vertex_tiler {
	/* Exactly as passed to glLineWidth */
	float line_width;

	/* Off by one */
	u32 vertex_count; 

	u32 unk1; // 0x28000000

	unsigned draw_mode : 4;
	unsigned unknown_draw : 28; 

	u32 zero0;
	u32 zero1;

	/* Like many other strictly nonzero quantities, index_count is
	 * subtracted by one. For an indexed cube, this is equal to 35 = 6
	 * faces * 2 triangles/per face * 3 vertices/per triangle - 1. For
	 * non-indexed draws, equal to vertex_count. */

	u32 index_count;

	/* No hidden structure; literally just a pointer to an array of
	 * uint32_t indices. Thanks, guys, for not making my life insane for
	 * once! NULL for non-indexed draws. */

	uintptr_t indices;

	u32 zero3;
	u32 gl_enables; // 0x5

	/* Offset for first vertex in buffer */
	u32 draw_start;

	u32 zero5;

	/* Zero for vertex jobs. Pointer to the position (gl_Position) varying
	 * output from the vertex shader for tiler jobs. */

	uintptr_t position_varying;

	uintptr_t unknown1; /* pointer */

	/* For reasons I don't quite understand this is a pointer to a pointer.
	 * That second pointer points to the actual texture descriptor. */
	uintptr_t texture_trampoline;

	/* For OpenGL, from what I've seen, this is intimately connected to
	 * texture_meta. cwabbott says this is not the case under Vulkan, hence
	 * why this field is seperate (Midgard is Vulkan capable) */
	uintptr_t sampler_descriptor;

	uintptr_t uniforms;
	u8 flags : 4;
	uintptr_t _shader_upper : MALI_SHORT_PTR_BITS - 4; /* struct shader_meta */
	uintptr_t attributes; /* struct attribute_buffer[] */
	uintptr_t attribute_meta; /* attribute_meta[] */
	uintptr_t varyings; /* struct attr */
	uintptr_t unknown6; /* pointer */
	uintptr_t viewport;
	u32 zero6;
	mali_ptr framebuffer;
} __attribute__((packed));
//ASSERT_SIZEOF_TYPE(struct mali_payload_vertex_tiler, 256, 256);

/* Pointed to from texture_trampoline, mostly unknown still, haven't
 * managed to replay successfully */

/* Purposeful off-by-one in width, height fields. For example, a (64, 64)
 * texture is stored as (63, 63) in these fields. This adjusts for that.
 * There's an identical pattern in the framebuffer descriptor. Even vertex
 * count fields work this way, hence the generic name -- integral fields that
 * are strictly positive generally need this adjustment. */

#define MALI_POSITIVE(dim) (dim - 1)

/* Opposite of MALI_POSITIVE, found in the depth_units field */

#define MALI_NEGATIVE(dim) (dim + 1)

/* Used with channel swizzling */
enum mali_channel {
	MALI_CHANNEL_RED = 0,
	MALI_CHANNEL_GREEN = 1,
	MALI_CHANNEL_BLUE = 2,
	MALI_CHANNEL_ALPHA = 3,
	MALI_CHANNEL_ZERO = 4,
	MALI_CHANNEL_ONE = 5,
	MALI_CHANNEL_RESERVED_0 = 6,
	MALI_CHANNEL_RESERVED_1 = 7,
};

/* Used with wrapping. Incomplete (this is a 4-bit field...) */

enum mali_wrap_mode {
	MALI_WRAP_REPEAT = 0x8,
	MALI_WRAP_CLAMP_TO_EDGE = 0x9,
	MALI_WRAP_CLAMP_TO_BORDER = 0xB,
	MALI_WRAP_MIRRORED_REPEAT = 0xC
};

/* 8192x8192 */
#define MAX_MIP_LEVELS (13)

/* Cubemap bloats everything up */
#define MAX_FACES (6)

/* Corresponds to the type passed to glTexImage2D and so forth */

struct mali_texture_format {
	unsigned bottom : 8;
	unsigned unk1 : 4;

	/*
	 * 0: ushort_5_6_5
	 * 2: ushort_4_4_4_4
	 * 3: u8
	 * 4: u16
	 * 5: u32
	 * 7: float
	 */

	unsigned component_size : 3;

	unsigned nr_channels : 2;

	/*
	 * 2: ushort_5_5_5_1, ushort_5_6_5
	 * 3: snorm
	 * 4: unsigned int
	 * 5: (unsigned) int / full-float
	 * 6: signed int / half-float
	 * 7: maybe also snorm related
	 */

	unsigned typeA : 3;

	unsigned usage1 : 3;
	unsigned is_not_cubemap : 1;
	unsigned usage2 : 8;
} __attribute__((packed));

ASSERT_SIZEOF_TYPE(struct mali_texture_format,
		   sizeof(u32), sizeof(u32));

struct mali_texture_descriptor {
	uint16_t width;
	uint16_t height;
	uint16_t depth;

	uint16_t unknown1;

	struct mali_texture_format format; 

	uint16_t unknown3;

	/* One for non-mipmapped, zero for mipmapped */
	uint8_t unknown3A;

	/* Zero for non-mipmapped, (number of levels - 1) for mipmapped */
	uint8_t nr_mipmap_levels;

	/* Swizzling is a single 32-bit word, broken up here for convenience.
	 * Here, swizzling refers to the ES 3.0 texture parameters for channel
	 * level swizzling, not the internal pixel-level swizzling which is
	 * below OpenGL's reach */

	enum mali_channel swizzle_r : 3;
	enum mali_channel swizzle_g : 3;
	enum mali_channel swizzle_b : 3;
	enum mali_channel swizzle_a : 3;
	unsigned swizzle_zero       : 20;

	uint32_t unknown5;
	uint32_t unknown6;
	uint32_t unknown7;

	mali_ptr swizzled_bitmaps[MAX_MIP_LEVELS * MAX_FACES];
} __attribute__((packed));

/* Used as part of filter_mode */

#define MALI_GL_LINEAR 0
#define MALI_GL_NEAREST 1
#define MALI_GL_MIP_LINEAR (0x18)

/* Used to construct low bits of filter_mode */

#define MALI_GL_TEX_MAG(mode) (((mode) & 1) << 0)
#define MALI_GL_TEX_MIN(mode) (((mode) & 1) << 1)

#define MALI_GL_TEX_MAG_MASK (1)
#define MALI_GL_TEX_MIN_MASK (2)

#define MALI_FILTER_NAME(filter) (filter ? "MALI_GL_NEAREST" : "MALI_GL_LINEAR")

/* Used for lod encoding. Thanks @urjaman for pointing out these routines can
 * be cleaned up a lot. */

#define DECODE_FIXED_16(x) ((float) (x / 256.0))

static inline uint16_t
FIXED_16(float x) {
	/* Clamp inputs, accounting for float error */
	float max_lod = (32.0 - (1.0/512.0));

	x = ((x > max_lod) ? max_lod : ((x < 0.0) ? 0.0 : x));

	return (int) (x * 256.0);
}

struct mali_sampler_descriptor {
	uint32_t filter_mode;
	
	/* Fixed point. Upper 8-bits is before the decimal point, although it
	 * caps [0-31]. Lower 8-bits is after the decimal point: int(round(x *
	 * 256)) */

	uint16_t min_lod;
	uint16_t max_lod;

	/* All one word in reality, but packed a bit */

	enum mali_wrap_mode wrap_s : 4;
	enum mali_wrap_mode wrap_t : 4;
	enum mali_wrap_mode wrap_r : 4;
	enum mali_alt_func compare_func : 3;

	/* A single set bit of unknown, ha! */
	unsigned unknown2 : 1;

	unsigned zero : 16;

	uint32_t zero2;
	float border_color[4];
} __attribute__((packed));

/* TODO: What are the floats? Apparently always { -inf, -inf, inf, inf },
 * unless the scissor test is enabled.
 *
 * viewport0/viewport1 form the arguments to glViewport. viewport1 is modified
 * by MALI_POSITIVE; viewport0 is as-is.
 */

struct mali_viewport {
	float floats[4];

	float depth_range_n;
	float depth_range_f;

	u16 viewport0[2];
	u16 viewport1[2];
} __attribute__((packed));

/* TODO: Varying meta is symmetrical with attr_meta, but there is some
 * weirdness associated. Figure it out. */

struct mali_unknown6 {
	u64 unknown0;
	u64 unknown1;
};

/* From presentations, 16x16 tiles externally. Use shift for fast computation
 * of tile numbers. */

#define MALI_TILE_SHIFT 4
#define MALI_TILE_LENGTH (1 << MALI_TILE_SHIFT)

/* Tile coordinates are stored as a compact u32, as only 12 bits are needed to
 * each component. Notice that this provides a theoretical upper bound of (1 <<
 * 12) = 4096 tiles in each direction, addressing a maximum framebuffer of size
 * 65536x65536. Multiplying that together, times another four given that Mali
 * framebuffers are 32-bit ARGB8888, means that this upper bound would take 16
 * gigabytes of RAM just to store the uncompressed framebuffer itself, let
 * alone rendering in real-time to such a buffer.
 *
 * Nice job, guys.*/

/* From mali_kbase_10969_workaround.c */
#define MALI_X_COORD_MASK 0x00000FFF
#define MALI_Y_COORD_MASK 0x0FFF0000

/* Extract parts of a tile coordinate */

#define MALI_TILE_COORD_X(coord) ((coord) & MALI_X_COORD_MASK)
#define MALI_TILE_COORD_Y(coord) (((coord) & MALI_Y_COORD_MASK) >> 16)
#define MALI_TILE_COORD_FLAGS(coord) ((coord) & ~(MALI_X_COORD_MASK | MALI_Y_COORD_MASK))

/* No known flags yet, but just in case...? */

#define MALI_TILE_NO_FLAG (0)

/* Helpers to generate tile coordinates based on the boundary coordinates in
 * screen space. So, with the bounds (0, 0) to (128, 128) for the screen, these
 * functions would convert it to the bounding tiles (0, 0) to (7, 7).
 * Intentional "off-by-one"; finding the tile number is a form of fencepost
 * problem. */

#define MALI_MAKE_TILE_COORDS(X, Y) ((X) | ((Y) << 16))
#define MALI_BOUND_TO_TILE(B, bias) ((B - bias) >> MALI_TILE_SHIFT)
#define MALI_COORDINATE_TO_TILE(W, H, bias) MALI_MAKE_TILE_COORDS(MALI_BOUND_TO_TILE(W, bias), MALI_BOUND_TO_TILE(H, bias))
#define MALI_COORDINATE_TO_TILE_MIN(W, H) MALI_COORDINATE_TO_TILE(W, H, 0) 
#define MALI_COORDINATE_TO_TILE_MAX(W, H) MALI_COORDINATE_TO_TILE(W, H, 1)

struct mali_payload_fragment {
	u32 min_tile_coord;
	u32 max_tile_coord;
	mali_ptr framebuffer;
} __attribute__((packed));
//ASSERT_SIZEOF_TYPE(struct mali_payload_fragment, 12, 16);

/* (Single?) Framebuffer Descriptor */

/* Flags apply to format. With just MSAA_A and MSAA_B, the framebuffer is
 * configured for 4x. With MSAA_8, it is configured for 8x. */

#define MALI_FRAMEBUFFER_MSAA_8 (1 << 3)
#define MALI_FRAMEBUFFER_MSAA_A (1 << 4)
#define MALI_FRAMEBUFFER_MSAA_B (1 << 23)

/* Fast/slow based on whether all three buffers are cleared at once */

#define MALI_CLEAR_FAST         (1 << 18)
#define MALI_CLEAR_SLOW         (1 << 28)
#define MALI_CLEAR_SLOW_STENCIL (1 << 31)

struct mali_single_framebuffer {
	u32 unknown1;
	u32 unknown2;
	u64 unknown_address_0;
	u64 zero1;
	u64 zero0;

	/* Exact format is ironically not known, since EGL is finnicky with the
	 * blob. MSAA, colourspace, etc are configured here. */

	u32 format; 

	u32 clear_flags;
	u32 zero2;

	/* Purposeful off-by-one in these fields should be accounted for by the
	 * MALI_DIMENSION macro */

	u16 width;
	u16 height;

	u32 zero3[8];

	/* By default, the framebuffer is upside down from OpenGL's
	 * perspective. Set framebuffer to the end and negate the stride to
	 * flip in the Y direction */

	mali_ptr framebuffer;
	int32_t stride;

	u32 zero4;

	/* Depth and stencil buffers are interleaved, it appears, as they are
	 * set to the same address in captures. Both fields set to zero if the
	 * buffer is not being cleared. Depending on GL_ENABLE magic, you might
	 * get a zero enable despite the buffer being present; that still is
	 * disabled. */

	mali_ptr depth_buffer; // not SAME_VA
	u64 depth_buffer_enable; 

	mali_ptr stencil_buffer; // not SAME_VA
	u64 stencil_buffer_enable; 

	u32 clear_color_1; // RGBA8888 from glClear, actually used by hardware
	u32 clear_color_2; // always equal, but unclear function?
	u32 clear_color_3; // always equal, but unclear function?
	u32 clear_color_4; // always equal, but unclear function?

	/* Set to zero if not cleared */

	float clear_depth_1; // float32, ditto
	float clear_depth_2; // float32, ditto
	float clear_depth_3; // float32, ditto
	float clear_depth_4; // float32, ditto

	u32 clear_stencil; // Exactly as it appears in OpenGL

	u32 zero6[7];

	/* Very weird format, see generation code in trans_builder.c */
	u32 resolution_check;

	u32 tiler_flags;

	u64 unknown_address_1; /* Pointing towards... a zero buffer? */
	u64 unknown_address_2;

	/* See mali_kbase_replay.c */
	u64 tiler_heap_free;
	u64 tiler_heap_end;

	/* More below this, maybe */
} __attribute__((packed));

struct bifrost_render_target {
	u32 unk1; // = 0x4000000
	u32 format;

	u64 zero1;

	/* Stuff related to ARM Framebuffer Compression. When AFBC is enabled,
	 * there is an extra metadata buffer that contains 16 bytes per tile.
	 * The framebuffer needs to be the same size as before, since we don't
	 * know ahead of time how much space it will take up. The
	 * framebuffer_stride is set to 0, since the data isn't stored linearly
	 * anymore.
	 */

	mali_ptr afbc_metadata;
	u32 afbc_stride; // stride in units of tiles
	u32 afbc_unk; // = 0x20000

	mali_ptr framebuffer;

	u32 zero2 : 4;
	u32 framebuffer_stride : 28; // in units of bytes
	u32 zero3;

	u32 clear_color_1; // RGBA8888 from glClear, actually used by hardware
	u32 clear_color_2; // always equal, but unclear function?
	u32 clear_color_3; // always equal, but unclear function?
	u32 clear_color_4; // always equal, but unclear function?
} __attribute__((packed));

/* An optional part of bifrost_framebuffer. It comes between the main structure
 * and the array of render targets. It must be included if any of these are
 * enabled:
 *
 * - Transaction Elimination
 * - Depth/stencil
 * - TODO: Anything else?
 */

struct bifrost_fb_extra {
	mali_ptr checksum;
	/* Each tile has an 8 byte checksum, so the stride is "width in tiles * 8" */
	u32 checksum_stride;

	u32 unk;

	union {
		/* Note: AFBC is only allowed for 24/8 combined depth/stencil. */
		struct {
			mali_ptr depth_stencil_afbc_metadata;
			u32 depth_stencil_afbc_stride; // in units of tiles
			u32 zero1;

			mali_ptr depth_stencil;

			u64 padding;
		} ds_afbc;

		struct {
			/* Depth becomes depth/stencil in case of combined D/S */
			mali_ptr depth;
			u32 depth_stride_zero : 4;
			u32 depth_stride : 28;
			u32 zero1;

			mali_ptr stencil;
			u32 stencil_stride_zero : 4;
			u32 stencil_stride : 28;
			u32 zero2;
		} ds_linear;
	};


	u64 zero3, zero4;
} __attribute__((packed));

/* flags for unk3 */
#define MALI_MFBD_EXTRA (1 << 13)

struct bifrost_framebuffer {
	u32 unk0; // = 0x10
	u32 zero1;
	u64 zero2;
	/* 0x10 */
	mali_ptr sample_locations;
	mali_ptr unknown1;
	/* 0x20 */
	u16 width1, height1;
	u32 zero3;
	u16 width2, height2;
	u32 unk1 : 19; // = 0x01000
	u32 rt_count_1 : 2; // off-by-one (use MALI_POSITIVE)
	u32 unk2 : 3; // = 0
	u32 rt_count_2 : 3; // no off-by-one
	u32 zero4 : 5;
	/* 0x30 */
	u32 clear_stencil : 8;
	u32 unk3 : 24; // = 0x100
	float clear_depth;
	mali_ptr tiler_meta;
	/* 0x40 */
	u64 zero5, zero6, zero7, zero8, zero9, zero10, zero11, zero12;

	/* optional: struct bifrost_fb_extra extra */
	/* struct bifrost_render_target rts[] */
} __attribute__((packed));

/* Originally from chai, which found it from mali_kase_reply.c */

#endif /* __MALI_JOB_H__ */
