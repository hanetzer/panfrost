/*
 * © Copyright 2018 The Panfrost Community
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pan_nondrm.h"

/* TODO: Bifrost */

/* Takes shader source code in *src, calls out to the shader assembler, and
 * sticks the resulting raw binary in dst, for use in replays */

/* TODO: Interface with Python C API directly? */

void
pandev_shader_assemble(uint32_t *dst, const char *src, int type)
{
	FILE *fp0 = fopen("/dev/shm/shader.asm", "w");
	fwrite(src, 1, strlen(src), fp0);
	fclose(fp0);

	system("python3 ../tools/midgard-assemble.py /dev/shm/shader.asm /dev/shm/shader.bin");

	FILE *fp1 = fopen("/dev/shm/shader.bin", "rb");

	fseek(fp1, 0, SEEK_END);
	size_t sz = ftell(fp1);
	fseek(fp1, 0, SEEK_SET);

	fread(dst, 1, sz, fp1);
	fclose(fp1);
}

/* XXX: This is a hack to avoid breaking prototypes in a bunch of places */
int last_shader_size = 0;

void *
pandev_shader_compile(uint32_t *dst, const char *src, int type)
{
	FILE *fp1 = fopen(type == JOB_TYPE_TILER ? "/dev/shm/fragment.bin" : "/dev/shm/vertex.bin", "rb");

	fseek(fp1, 0, SEEK_END);
	size_t sz = ftell(fp1);
	fseek(fp1, 0, SEEK_SET);

	/* Allocate space if necessary */

	if (!dst) {
		dst = malloc(sz * 16);
		last_shader_size = sz;
	}

	fread(dst, 1, sz, fp1);
	fclose(fp1);

	return dst;
}

#include "pan_context.h"

#include "compiler/nir/nir.h"
#include "nir/tgsi_to_nir.h"
#include "midgard/midgard_compile.h"
#include "util/u_dynarray.h"

#include "tgsi/tgsi_dump.h"

void
panfrost_shader_compile(struct panfrost_context *ctx, struct mali_shader_meta *meta, const char *src, int type, struct panfrost_shader_state *state)
{
	uint8_t* dst;

	/* When running inside Mesa, invoke the compiler do the whole compile shebang */
	nir_shader *s;

	struct pipe_shader_state* cso = &state->base;
	
	if (cso->type == PIPE_SHADER_IR_NIR) {
		printf("NIR in\n");
		s = cso->ir.nir;
	} else {
		printf("TGSI in\n");
		assert (cso->type == PIPE_SHADER_IR_TGSI);
		s = tgsi_to_nir(cso->tokens, &midgard_nir_options);
	}

	s->info.stage = type == JOB_TYPE_VERTEX ? MESA_SHADER_VERTEX : MESA_SHADER_FRAGMENT;

	/* Dump NIR for debugging */

	nir_print_shader(s, stderr);

	/* Call out to Midgard compiler given the above NIR */

	struct util_dynarray compiled;
	midgard_compile_shader_nir(s, &compiled);

	/* Prepare the compiled binary for upload */
	last_shader_size = compiled.size;
	dst = compiled.data;
	printf("Size: %d\n", last_shader_size);
	printf("dst: %p\n", dst);
	char name[128];
	snprintf(name, 128, "/dev/shm/%d.bin", type);
	FILE *fp = fopen(name, "wb");
	fwrite(dst, 1, last_shader_size, fp);
	fclose(fp);

	meta->shader = panfrost_upload(&ctx->shaders, dst, last_shader_size, true) | 5;

	util_dynarray_fini(&compiled);

    /* TODO: From compiler */

	/* Measured in vec4 words */
	int varying_count = 2;

    if (type == JOB_TYPE_VERTEX) {
	    meta->attribute_count = 3;

	    /* Vertex shader counts gl_Position as a varying */
	    meta->varying_count = varying_count + 2;

	    meta->midgard1.uniform_count = 12;
	    meta->midgard1.unknown1 = 1; /* XXX: WTF is this?! */
    } else {
	    meta->attribute_count = 0;
	    meta->varying_count = varying_count;
	    meta->texture_count = 3;
	    meta->sampler_count = 3;
	    meta->midgard1.uniform_count = 12;
	    meta->midgard1.unknown1 = MALI_NO_ALPHA_TO_COVERAGE | 0x200; /* XXX: WTF is this?! */
    }

    meta->midgard1.work_count = 8;
    meta->midgard1.unknown2 = 8; /* XXX: WTF is this?! */

    /* Varyings are known only through the shader. We choose to upload this
     * information with the vertex shader, though the choice is perhaps
     * arbitrary */

    if (type == JOB_TYPE_VERTEX) {
	    /* Setup two buffers, one for position, the other for normal
	     * varyings, as seen in traces. TODO: Are there other
	     * configurations we might use? */

	    ctx->varying_buffer_count = 2;

	    /* mediump vec4s sequentially */
	    ctx->varyings_stride[0] = (2 * sizeof(float)) * varying_count;

	    /* highp gl_Position */
	    ctx->varyings_stride[1] = 4 * sizeof(float);

	    /* Setup gl_Position and its weirdo analogue */

	    struct mali_attr_meta position_meta = {
		    .index = 1,
		    .type = 6, /* gl_Position */
		    .nr_components = MALI_POSITIVE(4),
		    .is_int_signed = 1,
		    .unknown1 = 0x1a22
	    };

	    struct mali_attr_meta position_meta_prime = {
		    .index = 0,
		    .type = 7, /* float */
		    .nr_components = MALI_POSITIVE(4),
		    .is_int_signed = 1,
		    .unknown1 = 0x2490
	    };

	    ctx->vertex_only_varyings[0] = position_meta;
	    ctx->vertex_only_varyings[1] = position_meta_prime;

	    /* Setup actual varyings. XXX: Don't assume vec4 */

	    for (int i = 0; i < varying_count; ++i) {
		    struct mali_attr_meta vec4_varying_meta = {
			    .index = 0,
			    .type = 7, /* float */
			    .nr_components = MALI_POSITIVE(4),
			    .not_normalised = 1,
			    .unknown1 = 0x1a22,

			    /* mediump => half-floats */
			    .is_int_signed = 1,

			    /* Set offset to keep everything back-to-back in
			     * the same buffer */
			    .src_offset = 8 * i,
#ifdef T6XX
			    .unknown2 = 1,
#endif
		    };

		    ctx->varyings[i] = vec4_varying_meta;
	    }

	    ctx->varying_count = varying_count;
    }
}
