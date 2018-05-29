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

#ifndef __BUILDER_H__
#define __BUILDER_H__

#define _LARGEFILE64_SOURCE 1
#define CACHE_LINE_SIZE 1024 /* TODO */ 
#include <sys/mman.h>
#include <assert.h>
#include "pan_nondrm.h"

#ifdef HAVE_DRI3
#include "pipe/p_compiler.h"
#include "pipe/p_config.h"
#include "pipe/p_context.h"
#include "pipe/p_defines.h"
#include "pipe/p_format.h"
#include "pipe/p_screen.h"
#include "pipe/p_state.h"
#else
#include "extrapipe/pipe.h"
#endif

/* Forward declare to avoid extra header dep */
struct prim_convert_context;

/* TODO: Handle on newer hardware */
#define PANFROST_DEFAULT_FBD (MALI_SFBD)

#define MAX_DRAW_CALLS 4096
#define MAX_VARYINGS   4096

#define PAN_DIRTY_DUMMY	     (1 << 0) 
#define PAN_DIRTY_RASTERIZER (1 << 2)
#define PAN_DIRTY_FS	     (1 << 3)
#define PAN_DIRTY_FRAG_CORE  (PAN_DIRTY_FS) /* Dirty writes are tied */
#define PAN_DIRTY_VS	     (1 << 4)
#define PAN_DIRTY_VERTEX     (1 << 5)
#define PAN_DIRTY_VERT_BUF   (1 << 6)
#define PAN_DIRTY_VIEWPORT   (1 << 7)
#define PAN_DIRTY_SAMPLERS   (1 << 8)
#define PAN_DIRTY_TEXTURES   (1 << 9)

struct panfrost_constant_buffer {
	bool dirty;
	size_t size;
	void *buffer;
};

struct panfrost_context {
	/* Gallium context */
	struct pipe_context base;

	/* TODO: DRM driver? */
	int fd;
	struct pipe_framebuffer_state pipe_framebuffer;

	struct panfrost_memory cmdstream;
	struct panfrost_memory textures;
	struct panfrost_memory shaders;
	struct panfrost_memory scratchpad;
	struct panfrost_memory tiler_heap;
	struct panfrost_memory varying_mem;
	struct panfrost_memory framebuffer;

	/* Common framebuffer settings */
	int width;
	int height;
	int32_t stride; /* Signed as framebuffers can be flipped vertically */
	int bytes_per_pixel;
	bool has_alpha_channel;

	/* Set for OpenGL's default mode. */
	bool flip_vertical;

	/* Each render job has multiple framebuffer descriptors associated with
	 * it, used for various purposes with more or less the same format. The
	 * most obvious is the fragment framebuffer descriptor, which carries
	 * e.g. clearing information */
	
	struct mali_single_framebuffer fragment_fbd;

	/* Each draw has corresponding vertex and tiler payloads */
	struct mali_payload_vertex_tiler payload_vertex;
	struct mali_payload_vertex_tiler payload_tiler;

	/* The fragment shader binary itself is pointed here (for the tripipe) but
	 * also everything else in the shader core, including blending, the
	 * stencil/depth tests, etc. Refer to the presentations. */

	struct mali_fragment_core fragment_shader_core;

	/* A frame is composed of a starting set value job, a number of vertex
	 * and tiler jobs, linked to the fragment job at the end. See the
	 * presentations for more information how this works */

	int draw_count;

	mali_ptr set_value_job;
	mali_ptr vertex_jobs[MAX_DRAW_CALLS];
	mali_ptr tiler_jobs[MAX_DRAW_CALLS];

	mali_ptr depth_stencil_buffer;

	/* Dirty flags are setup like any other driver */
	int dirty;

	int vertex_count;

	struct mali_attr attributes[PIPE_MAX_ATTRIBS];

	int varyings_stride[MAX_VARYINGS];
	int varying_count;
	int varying_height;
	struct mali_unknown6 varyings_descriptor_0;
	struct mali_unknown6 varyings_descriptor_1;

	struct mali_viewport viewport;

	/* TODO: Multiple uniform buffers (index =/= 0), finer updates? */

	struct panfrost_constant_buffer constant_buffer[PIPE_SHADER_TYPES];

	/* CSOs */
	struct panfrost_rasterizer *rasterizer;

	struct panfrost_shader_state *vs;
	struct panfrost_shader_state *fs;

	struct panfrost_vertex_state *vertex;

	struct pipe_vertex_buffer *vertex_buffers;
	int vertex_buffer_count;

	struct panfrost_sampler_state *samplers[PIPE_SHADER_TYPES][PIPE_MAX_SAMPLERS];
	int sampler_count[PIPE_SHADER_TYPES];

	struct panfrost_sampler_view *sampler_views[PIPE_SHADER_TYPES][PIPE_MAX_SHADER_SAMPLER_VIEWS];
	int sampler_view_count[PIPE_SHADER_TYPES];

	struct primconvert_context *primconvert;
};

/* Corresponds to the CSO */

struct panfrost_rasterizer {
	struct pipe_rasterizer_state base;

	/* Bitmask of front face, etc */
	unsigned tiler_gl_enables;
};

struct panfrost_shader_state {
	struct pipe_shader_state base;

	/* Compiled descriptor, ready for the hardware */
	bool compiled;
	struct mali_tripipe tripipe;
};

struct panfrost_vertex_state {
	unsigned num_elements;

	struct pipe_vertex_element pipe[PIPE_MAX_ATTRIBS];
	struct mali_attr_meta hw[PIPE_MAX_ATTRIBS];
	int nr_components[PIPE_MAX_ATTRIBS];
};

struct panfrost_sampler_state {
	struct pipe_sampler_state base;
	struct mali_sampler_descriptor hw;
};

/* Misnomer: Sampler view corresponds to textures, not samplers */

struct panfrost_sampler_view {
	struct pipe_sampler_view base;
	struct mali_texture_descriptor hw;
};

/* Corresponds to pipe_resource for our hacky pre-DRM interface */

struct sw_displaytarget;

struct panfrost_resource {
	struct pipe_resource base;

	/* Address to the resource in question */

	uint8_t *cpu[MAX_MIP_LEVELS];

	/* Not necessarily a GPU mapping of cpu! In case of texture tiling, gpu
	 * points to the GPU-side, tiled texture, while cpu points to the
	 * CPU-side, untiled texture from mesa */

	mali_ptr gpu[MAX_MIP_LEVELS];

	/* Is something other than level 0 ever written? */
	bool is_mipmap;

	/* Valid for textures; 1 otherwise */
	int bytes_per_pixel;

	/* bytes_per_pixel*width + padding */
	int stride;

	struct sw_displaytarget *dt;
};

static inline struct panfrost_context *
panfrost_context(struct pipe_context *pcontext)
{
	return (struct panfrost_context *) pcontext;
}

void
trans_default_shader_backend(struct panfrost_context *ctx);

void
trans_emit_vertex_payload(struct panfrost_context *ctx);

void
trans_emit_tiler_payload(struct panfrost_context *ctx);

void
trans_emit_vt_framebuffer(struct panfrost_context *ctx);

struct mali_single_framebuffer trans_emit_fbd(struct panfrost_context *ctx);

void
trans_viewport(struct panfrost_context *ctx,
		float depth_range_n, float depth_range_f,
		int viewport_x0, int viewport_y0,
		int viewport_x1, int viewport_y1);

void
trans_rasterizer_state(struct mali_payload_vertex_tiler *vt,
		float line_width,
		int front_face);

struct mali_attr trans_attr(size_t type_sz, int columns, int vertices);

void
trans_queue_draw(struct panfrost_context *ctx);

struct pipe_context *
panfrost_create_context(struct pipe_screen *screen, void *priv, unsigned flags);

void
trans_setup_framebuffer(struct panfrost_context *ctx, uint32_t *addr, int w, int h);

#ifndef CALLOC_STRUCT
#define CALLOC_STRUCT(s) calloc(sizeof(struct s), 1)
#endif

struct pipe_resource *
panfrost_resource_create_front(struct pipe_screen *screen,
			       const struct pipe_resource *template,
			       const void *map_front_private);

#endif
