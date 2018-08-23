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

#include <sys/poll.h>

#include "pan_context.h"
#include "pan_swizzle.h"

#ifdef HAVE_DRI3
#include "util/macros.h"
#include "util/u_format.h"
#include "util/u_inlines.h"
#include "util/u_surface.h"
#include "util/u_upload_mgr.h"
#include "util/u_transfer.h"
#include "util/u_transfer_helper.h"
#include "indices/u_primconvert.h"

#include "pan_screen.h"
#endif

/* Don't use the mesa winsys; use our own X11 window with Xshm */
#define USE_SLOWFB

/* Do not actually send anything to the GPU; merely generate the cmdstream as fast as possible. Disables framebuffer writes */
//#define DRY_RUN

#define SET_BIT(lval, bit, cond) \
	if (cond) \
		lval |= (bit); \
	else \
		lval &= ~(bit);

/* MSAA is not supported in sw_winsys but it does make for nicer demos ;) so we
 * can force it regardless of gallium saying we don't have it */
static bool FORCE_MSAA = false;

/* Descriptor is generated along with the shader compiler */

static void
trans_upload_varyings_descriptor(struct panfrost_context *ctx)
{
        mali_ptr unknown6_1_p = panfrost_upload(&ctx->cmdstream_persistent, &ctx->varyings_descriptor_0, sizeof(struct mali_unknown6), true);
        mali_ptr unknown6_2_p = panfrost_upload_sequential(&ctx->cmdstream_persistent, &ctx->varyings_descriptor_1, sizeof(struct mali_unknown6));
        ctx->payload_vertex.postfix.varying_meta = (unknown6_1_p) | 0x0;
        ctx->payload_tiler.postfix.varying_meta = (unknown6_2_p) | 0x8;
}

/* TODO: Sample size, etc */

static void
trans_set_framebuffer_msaa(struct panfrost_context *ctx, bool enabled)
{
	SET_BIT(ctx->fragment_shader_core.unknown2_3, MALI_HAS_MSAA, enabled);
	SET_BIT(ctx->fragment_shader_core.unknown2_4, MALI_NO_MSAA, !enabled);

#ifdef SFBD
	SET_BIT(ctx->fragment_fbd.format, MALI_FRAMEBUFFER_MSAA_A | MALI_FRAMEBUFFER_MSAA_B, enabled);
#else
	SET_BIT(ctx->fragment_rts[0].format, MALI_FRAMEBUFFER_MSAA_A | MALI_FRAMEBUFFER_MSAA_B, enabled);
#endif
}

/* Framebuffer descriptor */

static void
trans_set_framebuffer_resolution(struct mali_single_framebuffer *fb, int w, int h)
{
	fb->width = MALI_POSITIVE(w);
	fb->height = MALI_POSITIVE(h);

	/* No idea why this is needed, but it's how resolution_check is
	 * calculated.  It's not clear to us yet why the hardware wants this.
	 * The formula itself was discovered mostly by manual bruteforce and
	 * aggressive algebraic simplification. */

	fb->resolution_check = ((w + h) / 3) << 4;
}

static PANFROST_FRAMEBUFFER
trans_emit_fbd(struct panfrost_context *ctx)
{
#ifdef SFBD
    struct mali_single_framebuffer framebuffer = {
	    .unknown2 = 0x1f,
	    .format = 0x30000000,
	    .clear_flags = 0x1000,
	    .unknown_address_0 = ctx->scratchpad.gpu,
	    .unknown_address_1 = ctx->scratchpad.gpu + 0x6000,
	    .unknown_address_2 = ctx->scratchpad.gpu + 0x6200,
	    .tiler_flags = 0xf0,
	    .tiler_heap_free = ctx->tiler_heap.gpu,
	    .tiler_heap_end = ctx->tiler_heap.gpu + ctx->tiler_heap.size,
    };

    trans_set_framebuffer_resolution(&framebuffer, ctx->width, ctx->height);
#else
	struct bifrost_framebuffer framebuffer = {
		.tiler_meta = 0xf000000c00,

		.width1 = MALI_POSITIVE(ctx->width),
		.height1 = MALI_POSITIVE(ctx->height),
		.width2 = MALI_POSITIVE(ctx->width),
		.height2 = MALI_POSITIVE(ctx->height),

		.unk1 = 0x1080,

		/* TODO: MRT */
		.rt_count_1 = MALI_POSITIVE(1),
		.rt_count_2 = 4,

		/* TODO: WTF is this structure? */
		.zero1 = 0x1f,
		.zero2 = ctx->scratchpad.gpu,
		.zero5 = ctx->misc_0.gpu,
		.zero6 = ctx->misc_0.gpu + 512,
		.zero7 = ctx->tiler_heap.gpu,
		.zero8 = ctx->tiler_heap.gpu + 4096*32768,
	};

#endif

    return framebuffer;
}

/* The above function is for generalised fbd emission, used in both fragment as
 * well as vertex/tiler payloads. This payload is specific to fragment
 * payloads. */

static void
trans_new_frag_framebuffer(struct panfrost_context *ctx)
{
#ifdef SFBD
        struct mali_single_framebuffer fb = trans_emit_fbd(ctx);

        fb.framebuffer = ctx->framebuffer.gpu;
	fb.stride = ctx->stride;

	/* The default is upside down from OpenGL's perspective */

	if (ctx->flip_vertical) {
		fb.framebuffer += ctx->framebuffer.size;
		fb.stride = -fb.stride;
	}

        fb.format = 0xb84e0281; /* RGB32, no MSAA */
#else
	struct bifrost_framebuffer fb = trans_emit_fbd(ctx);

	/* XXX: MRT case */
	fb.rt_count_2 = 1;
	fb.unk3 = 0x100;

	struct bifrost_render_target rt = {
		.unk1 = 0x4000000,
		.format = 0x880a8899, /* RGB32, no MSAA */
		.framebuffer = ctx->framebuffer.gpu,
		.framebuffer_stride = ctx->width / 4,
	};

	memcpy(&ctx->fragment_rts[0], &rt, sizeof(rt));
#endif

        memcpy(&ctx->fragment_fbd, &fb, sizeof(fb));
}

/* Maps float 0.0-1.0 to int 0x00-0xFF */
static uint8_t
normalised_float_to_u8(float f) {
	return (uint8_t) (int) (f * 255.0f);
}

static void
panfrost_flush(
		struct pipe_context *pipe,
		struct pipe_fence_handle ** fence,
		unsigned flags);


static void
panfrost_clear(
		struct pipe_context *pipe,
		unsigned buffers,
		const union pipe_color_union *color,
		double depth, unsigned stencil)
{
	struct panfrost_context *ctx = panfrost_context(pipe);

	bool clear_color = buffers & PIPE_CLEAR_COLOR;
	bool clear_depth = buffers & PIPE_CLEAR_DEPTH;
	bool clear_stencil = buffers & PIPE_CLEAR_STENCIL;

	/* Remember that we've done something */
	ctx->dirty |= PAN_DIRTY_DUMMY;

	/* Alpha clear only meaningful without alpha channel */
	float clear_alpha = ctx->has_alpha_channel ? color->f[3] : 1.0f;

	uint32_t packed_color =
		(normalised_float_to_u8(clear_alpha) << 24) |
		(normalised_float_to_u8(color->f[2]) << 16) |
		(normalised_float_to_u8(color->f[1]) <<  8) |
		(normalised_float_to_u8(color->f[0]) <<  0);

#ifdef MFBD
	struct bifrost_render_target* buffer_color = &ctx->fragment_rts[0];
#else
	struct mali_single_framebuffer* buffer_color = &ctx->fragment_fbd;
#endif

#ifdef MFBD
	struct bifrost_framebuffer *buffer_ds = &ctx->fragment_fbd;
#else
	struct mali_single_framebuffer *buffer_ds = buffer_color;
#endif

	if (clear_color) {
		/* Fields duplicated 4x for unknown reasons. Same in Utgard,
		 * too, which is doubly weird. */

		buffer_color->clear_color_1 = packed_color;
		buffer_color->clear_color_2 = packed_color;
		buffer_color->clear_color_3 = packed_color;
		buffer_color->clear_color_4 = packed_color;
	}

	if (clear_depth) {
#ifdef SFBD
		buffer_ds->clear_depth_1 = depth;
		buffer_ds->clear_depth_2 = depth;
		buffer_ds->clear_depth_3 = depth;
		buffer_ds->clear_depth_4 = depth;
#else
		buffer_ds->clear_depth = depth;
#endif
	}

	if (clear_stencil) {
		buffer_ds->clear_stencil = stencil;
	}

	/* Setup buffers depending on MFBD/SFBD */

#ifdef MFBD
	if (clear_depth || clear_stencil) {
		/* Setup combined 24/8 depth/stencil */
		ctx->fragment_fbd.unk3 |= MALI_MFBD_EXTRA;
		ctx->fragment_extra.unk = 0x405;
		ctx->fragment_extra.ds_linear.depth = ctx->depth_stencil_buffer.gpu;
		ctx->fragment_extra.ds_linear.depth_stride = ctx->width * 4;
	}
#else
	if (clear_depth) {
		buffer_ds->depth_buffer = ctx->depth_stencil_buffer.gpu;
		buffer_ds->depth_buffer_enable = MALI_DEPTH_STENCIL_ENABLE;
	}

	if (clear_stencil) {
		buffer_ds->stencil_buffer = ctx->depth_stencil_buffer.gpu;
		buffer_ds->stencil_buffer_enable = MALI_DEPTH_STENCIL_ENABLE;
	}
#endif

#ifdef SFBD
	/* Set flags based on what has been cleared, for the SFBD case */
	/* XXX: What do these flags mean? */
	int clear_flags = 0x101100;

	if (clear_color && clear_depth && clear_stencil) {
		/* On a tiler like this, it's fastest to clear all three buffers at once */

		clear_flags |= MALI_CLEAR_FAST;
	} else {
		clear_flags |= MALI_CLEAR_SLOW;

		if (clear_stencil)
			clear_flags |= MALI_CLEAR_SLOW_STENCIL;
	}

	fbd->clear_flags = clear_flags;
#endif

	panfrost_flush(pipe, NULL, 0);
}

static void
trans_attach_vt_framebuffer(struct panfrost_context *ctx)
{
#ifdef MFBD
	mali_ptr who_knows = panfrost_reserve(&ctx->cmdstream, 1024);
#endif

	mali_ptr framebuffer_1_p = panfrost_upload(&ctx->cmdstream, &ctx->vt_framebuffer, sizeof(ctx->vt_framebuffer), false) | PANFROST_DEFAULT_FBD;

#ifdef MFBD
	/* MFBD needs a sequential semi-render target upload */

	/* WTF any of this is, is beyond me for now */
	struct bifrost_render_target rts_list[] = {
		{
			.chunknown = {
				.unk = 0x30005,
				.pointer = who_knows,
			},
			.framebuffer = ctx->scratchpad.gpu + 0x6000,
			.zero2 = 0x3,
		},
	};

	panfrost_upload_sequential(&ctx->cmdstream, rts_list, sizeof(rts_list));
#endif
	ctx->payload_vertex.postfix.framebuffer = framebuffer_1_p;
	ctx->payload_tiler.postfix.framebuffer = framebuffer_1_p;
}

/* Reset per-frame context, called on context initialisation as well as after
 * flushing a frame */

static void
trans_invalidate_frame(struct panfrost_context *ctx)
{
	/* Reset varyings allocated */
	ctx->varying_height = 0;

	/* The cmdstream is dirty every frame; the only bits worth preserving
	 * (textures, shaders, etc) are in other buffers anyways */
	ctx->cmdstream.stack_bottom = 0;

	/* XXX: Shader binaries should be preserved with dirty tracking!!! */
	ctx->shaders.stack_bottom = 0;
	ctx->textures.stack_bottom = 0;

	/* Regenerate payloads */
	trans_attach_vt_framebuffer(ctx);

	if (ctx->rasterizer)
		ctx->dirty |= PAN_DIRTY_RASTERIZER;

	/* Viewport */
	/* XXX: Galliumify for realsies */

        trans_viewport(ctx, 0.0, 1.0, 0, 0, ctx->width, ctx->height);
	ctx->dirty |= PAN_DIRTY_VIEWPORT;

	/* Uniforms are all discarded with the above stack discard */

	for (int i = 0; i <= PIPE_SHADER_FRAGMENT; ++i)
		ctx->constant_buffer[i].dirty = true;

	/* XXX */
	ctx->dirty |= PAN_DIRTY_SAMPLERS | PAN_DIRTY_TEXTURES;
}

void
trans_viewport(struct panfrost_context *ctx,
		float depth_range_n,
		float depth_range_f,
		int viewport_x0, int viewport_y0,
		int viewport_x1, int viewport_y1)
{
	/* Viewport encoding is asymmetric. Purpose of the floats is unknown? */

	struct mali_viewport ret = {
		.floats = {
			-inff, -inff,
			inff, inff,
		},

		.depth_range_n = depth_range_n, 
		.depth_range_f = depth_range_f,

		.viewport0 = { viewport_x0, viewport_y0 },
		.viewport1 = { MALI_POSITIVE(viewport_x1), MALI_POSITIVE(viewport_y1) },
	};

	memcpy(&ctx->viewport, &ret, sizeof(ret));
}

/* Shared descriptor for attributes as well as varyings, which can be
 * considered like a matrix. This function is just a simplified constructor; no
 * actual bookkeeping is done (hence the lack of a context parameter). */

struct mali_attr
trans_attr(size_t type_sz, int columns, int vertices)
{
	int stride = type_sz * columns;

	struct mali_attr ret = {
		.stride = stride,
		.size = stride * vertices
	};

	return ret;
}

/* In practice, every field of these payloads should be configurable
 * arbitrarily, which means these functions are basically catch-all's for
 * as-of-yet unwavering unknowns */

static void
trans_emit_vertex_payload(struct panfrost_context *ctx)
{
	struct midgard_payload_vertex_tiler payload = {
		.prefix = {
			.workgroups_z_shift = 32,
			.workgroups_x_shift_2 = 0x2,
			.workgroups_x_shift_3 = 0x5,
		},
		.gl_enables = 0x6
	};

	memcpy(&ctx->payload_vertex, &payload, sizeof(payload));
}

static unsigned
trans_translate_texture_swizzle(enum pipe_swizzle s) {
	switch (s) {
		case PIPE_SWIZZLE_X: return MALI_CHANNEL_RED;
		case PIPE_SWIZZLE_Y: return MALI_CHANNEL_GREEN;
		case PIPE_SWIZZLE_Z: return MALI_CHANNEL_BLUE;
		case PIPE_SWIZZLE_W: return MALI_CHANNEL_ALPHA;
		case PIPE_SWIZZLE_0: return MALI_CHANNEL_ZERO;
		case PIPE_SWIZZLE_1: return MALI_CHANNEL_ONE;
		default: assert(0);
	}
}

static unsigned
translate_tex_wrap(enum pipe_tex_wrap w) {
	switch (w) {
		case PIPE_TEX_WRAP_REPEAT: return MALI_WRAP_REPEAT;
		case PIPE_TEX_WRAP_CLAMP_TO_EDGE: return MALI_WRAP_CLAMP_TO_EDGE;
		case PIPE_TEX_WRAP_CLAMP_TO_BORDER: return MALI_WRAP_CLAMP_TO_BORDER;
		case PIPE_TEX_WRAP_MIRROR_REPEAT: return MALI_WRAP_MIRRORED_REPEAT;
		default: assert(0);
	}
}

static unsigned
translate_tex_filter(enum pipe_tex_filter f) {
	switch (f) {
		case PIPE_TEX_FILTER_NEAREST: return MALI_GL_NEAREST;
		case PIPE_TEX_FILTER_LINEAR: return MALI_GL_LINEAR;
		default: assert(0);
	}
}

static unsigned
translate_mip_filter(enum pipe_tex_mipfilter f)
{
	return (f == PIPE_TEX_MIPFILTER_LINEAR) ? MALI_GL_MIP_LINEAR : 0;
}	

static unsigned
trans_translate_compare_func(enum pipe_compare_func in)
{
	switch (in) {
		case PIPE_FUNC_NEVER:	return MALI_FUNC_NEVER;
		case PIPE_FUNC_LESS: 	return MALI_FUNC_LESS;
		case PIPE_FUNC_EQUAL: 	return MALI_FUNC_EQUAL;
		case PIPE_FUNC_LEQUAL: 	return MALI_FUNC_LEQUAL;
		case PIPE_FUNC_GREATER: return MALI_FUNC_GREATER;
		case PIPE_FUNC_NOTEQUAL:return MALI_FUNC_NOTEQUAL;
		case PIPE_FUNC_GEQUAL: 	return MALI_FUNC_GEQUAL;
		case PIPE_FUNC_ALWAYS: 	return MALI_FUNC_ALWAYS;
	}

	return 0; /* Unreachable */
}

static unsigned
trans_translate_alt_compare_func(enum pipe_compare_func in)
{
	switch (in) {
		case PIPE_FUNC_NEVER:	return MALI_ALT_FUNC_NEVER;
		case PIPE_FUNC_LESS: 	return MALI_ALT_FUNC_LESS;
		case PIPE_FUNC_EQUAL: 	return MALI_ALT_FUNC_EQUAL;
		case PIPE_FUNC_LEQUAL: 	return MALI_ALT_FUNC_LEQUAL;
		case PIPE_FUNC_GREATER: return MALI_ALT_FUNC_GREATER;
		case PIPE_FUNC_NOTEQUAL:return MALI_ALT_FUNC_NOTEQUAL;
		case PIPE_FUNC_GEQUAL: 	return MALI_ALT_FUNC_GEQUAL;
		case PIPE_FUNC_ALWAYS: 	return MALI_ALT_FUNC_ALWAYS;
	}

	return 0; /* Unreachable */
}

static void
trans_emit_tiler_payload(struct panfrost_context *ctx)
{
	struct midgard_payload_vertex_tiler payload_1 = {
		.prefix = {
			.workgroups_z_shift = 32,
			.workgroups_x_shift_2 = 0x2,
			.workgroups_x_shift_3 = 0x6,
			/* XXX: TODO */

			.zero1 = 0xffff, /* Why is this only seen on test-quad-textured? */
		},
	};

	memcpy(&ctx->payload_tiler, &payload_1, sizeof(payload_1));
}

static bool
trans_make_dominant_factor(unsigned src_factor, enum mali_dominant_factor *factor, bool *invert)
{
	switch (src_factor) {
		case PIPE_BLENDFACTOR_SRC_COLOR: 
		case PIPE_BLENDFACTOR_INV_SRC_COLOR: 
			*factor = MALI_DOMINANT_SRC_COLOR;
			break;

		case PIPE_BLENDFACTOR_SRC_ALPHA: 
		case PIPE_BLENDFACTOR_INV_SRC_ALPHA: 
			*factor = MALI_DOMINANT_SRC_ALPHA;
			break;

		case PIPE_BLENDFACTOR_DST_COLOR: 
		case PIPE_BLENDFACTOR_INV_DST_COLOR: 
			*factor = MALI_DOMINANT_DST_COLOR;
			break;

		case PIPE_BLENDFACTOR_DST_ALPHA: 
		case PIPE_BLENDFACTOR_INV_DST_ALPHA: 
			*factor = MALI_DOMINANT_DST_ALPHA;
			break;

		case PIPE_BLENDFACTOR_ONE: 
		case PIPE_BLENDFACTOR_ZERO: 
			*factor = MALI_DOMINANT_ZERO;
			break;

		default:
			/* Fancy blend modes not supported */
			return false;
	}

	/* Set invert flags */

	switch (src_factor) {
		case PIPE_BLENDFACTOR_ONE:
		case PIPE_BLENDFACTOR_INV_SRC_COLOR: 
		case PIPE_BLENDFACTOR_INV_SRC_ALPHA: 
		case PIPE_BLENDFACTOR_INV_DST_ALPHA: 
		case PIPE_BLENDFACTOR_INV_DST_COLOR: 
		case PIPE_BLENDFACTOR_INV_SRC1_COLOR: 
		case PIPE_BLENDFACTOR_INV_SRC1_ALPHA: 
			*invert = true;
		default:
			break;
	}
	
	return true;
}

static bool
is_edge_blendfactor(unsigned factor) {
	return factor == PIPE_BLENDFACTOR_ONE || factor == PIPE_BLENDFACTOR_ZERO;
}

static int
complement_factor(int factor)
{
	switch (factor) {
		case PIPE_BLENDFACTOR_INV_SRC_COLOR: 
			return PIPE_BLENDFACTOR_SRC_COLOR;
		case PIPE_BLENDFACTOR_INV_SRC_ALPHA: 
			return PIPE_BLENDFACTOR_SRC_ALPHA;
		case PIPE_BLENDFACTOR_INV_DST_ALPHA: 
			return PIPE_BLENDFACTOR_DST_ALPHA;
		case PIPE_BLENDFACTOR_INV_DST_COLOR: 
			return PIPE_BLENDFACTOR_DST_COLOR;
		default:
			return -1;
	}
}

static bool
trans_make_fixed_blend_part(unsigned func, unsigned src_factor, unsigned dst_factor, unsigned *out)
{
	struct mali_blend_mode part = { 0 };

	/* Make sure that the blend function is representible with negate flags */

	if (func == PIPE_BLEND_ADD) {
		/* Default, no modifiers needed */
	} else if (func == PIPE_BLEND_SUBTRACT)
		part.negate_dest = true;
	else if (func == PIPE_BLEND_REVERSE_SUBTRACT)
		part.negate_source = true;
	else
		return false;

	part.clip_modifier = MALI_BLEND_MOD_NORMAL;

	/* Decide which is dominant, source or destination. If one is an edge
	 * case, use the other as a factor. If they're the same, it doesn't
	 * matter; we just mirror. If they're different non-edge-cases, you
	 * need a blend shader (don't do that). */
	
	if (is_edge_blendfactor(dst_factor)) {
		part.dominant = MALI_BLEND_DOM_SOURCE;
		part.nondominant_mode = MALI_BLEND_NON_ZERO;

		if (dst_factor == PIPE_BLENDFACTOR_ONE)
			part.clip_modifier = MALI_BLEND_MOD_DEST_ONE;
	} else if (is_edge_blendfactor(src_factor)) {
		part.dominant = MALI_BLEND_DOM_DESTINATION;
		part.nondominant_mode = MALI_BLEND_NON_ZERO;

		if (src_factor == PIPE_BLENDFACTOR_ONE)
			part.clip_modifier = MALI_BLEND_MOD_SOURCE_ONE;

	} else if (src_factor == dst_factor) {
		part.dominant = MALI_BLEND_DOM_SOURCE; /* Arbitrary choice, but align with the blob until we understand more */
		part.nondominant_mode = MALI_BLEND_NON_MIRROR;
	} else if (src_factor == complement_factor(dst_factor)) {
		/* TODO: How does this work exactly? */
		part.dominant = MALI_BLEND_DOM_SOURCE;
		part.nondominant_mode = MALI_BLEND_NON_MIRROR;
		part.clip_modifier = MALI_BLEND_MOD_DEST_ONE;
	} else if (dst_factor == complement_factor(src_factor)) {
		part.dominant = MALI_BLEND_DOM_SOURCE;
		part.nondominant_mode = MALI_BLEND_NON_MIRROR;
		part.clip_modifier = MALI_BLEND_MOD_SOURCE_ONE;
	} else {
		printf("Failed to find dominant factor?\n");
		return false;
	}

	unsigned in_dominant_factor =
		part.dominant == MALI_BLEND_DOM_SOURCE ? src_factor : dst_factor;

	if (part.clip_modifier == MALI_BLEND_MOD_NORMAL && in_dominant_factor == PIPE_BLENDFACTOR_ONE) {
		part.clip_modifier = part.dominant == MALI_BLEND_DOM_SOURCE ? MALI_BLEND_MOD_SOURCE_ONE : MALI_BLEND_MOD_DEST_ONE;
		in_dominant_factor = PIPE_BLENDFACTOR_ZERO;
	}

	bool invert_dominant = false;
	enum mali_dominant_factor dominant_factor;

	if (!trans_make_dominant_factor(in_dominant_factor, &dominant_factor, &invert_dominant))
		return false;

	part.dominant_factor = dominant_factor;
	part.complement_dominant = invert_dominant;

	/* Write out mode */
	memcpy(out, &part, sizeof(part));

	return true;
}

/* Create the descriptor for a fixed blend mode given the corresponding Gallium
 * state, if possible. Return true and write out the blend descriptor into
 * blend_equation. If it is not possible with the fixed function
 * representating, return false to handle degenerate cases with a blend shader
 */

static const struct pipe_rt_blend_state default_blend = {
	.blend_enable = 1,

	.rgb_func = PIPE_BLEND_ADD,
	.rgb_src_factor = PIPE_BLENDFACTOR_ONE,
	.rgb_dst_factor = PIPE_BLENDFACTOR_ZERO,

	.alpha_func = PIPE_BLEND_ADD,
	.alpha_src_factor = PIPE_BLENDFACTOR_ONE,
	.alpha_dst_factor = PIPE_BLENDFACTOR_ZERO,

	.colormask = PIPE_MASK_RGBA
};

static bool
trans_make_fixed_blend_mode(const struct pipe_rt_blend_state *blend, struct mali_blend_equation *out)
{
	/* If no blending is enabled, default back on `replace` mode */

	if (!blend->blend_enable)
		return trans_make_fixed_blend_mode(&default_blend, out);

	unsigned rgb_mode = 0;
	unsigned alpha_mode = 0;

	if (!trans_make_fixed_blend_part(
		blend->rgb_func, blend->rgb_src_factor, blend->rgb_dst_factor,
		&rgb_mode))
			return false;

	if (!trans_make_fixed_blend_part(
		blend->alpha_func, blend->alpha_src_factor, blend->alpha_dst_factor,
		&alpha_mode))
			return false;

	out->rgb_mode = rgb_mode;
	out->alpha_mode = alpha_mode;

	/* Gallium and Mali represent colour masks identically. XXX: Static assert for future proof */
	out->color_mask = blend->colormask;

	return true;
}

static unsigned
trans_translate_stencil_op(enum pipe_stencil_op in)
{
	switch (in) {
		case PIPE_STENCIL_OP_KEEP:	return MALI_STENCIL_KEEP;
		case PIPE_STENCIL_OP_ZERO:	return MALI_STENCIL_ZERO;
		case PIPE_STENCIL_OP_REPLACE: 	return MALI_STENCIL_REPLACE;
		case PIPE_STENCIL_OP_INCR: 	return MALI_STENCIL_INCR;
		case PIPE_STENCIL_OP_DECR: 	return MALI_STENCIL_DECR;
		case PIPE_STENCIL_OP_INCR_WRAP: return MALI_STENCIL_INCR_WRAP;
		case PIPE_STENCIL_OP_DECR_WRAP: return MALI_STENCIL_DECR_WRAP;
		case PIPE_STENCIL_OP_INVERT: 	return MALI_STENCIL_INVERT;
	}

	return 0; /* Unreachable */
}

static void
trans_make_stencil_state(const struct pipe_stencil_state *in, struct mali_stencil_test *out)
{
	/* TODO: enabled check */
	out->ref = 0; /* Gallium gets it from else where */

	out->mask = in->valuemask; 
	out->func = trans_translate_compare_func(in->func);
	out->sfail = trans_translate_stencil_op(in->fail_op);
	out->dpfail = trans_translate_stencil_op(in->zfail_op);
	out->dppass = trans_translate_stencil_op(in->zpass_op);
}

void
trans_default_shader_backend(struct panfrost_context *ctx)
{
	struct mali_shader_meta shader = {
		.alpha_coverage = ~MALI_ALPHA_COVERAGE(0.000000),
		.unknown2_3 = MALI_DEPTH_FUNC(MALI_FUNC_ALWAYS) | 0x3010 /*| MALI_CAN_DISCARD*/,
#ifdef T8XX
		.unknown2_4 = MALI_NO_MSAA | 0x4e0,
#else
		.unknown2_4 = MALI_NO_MSAA | 0x4f0,
#endif
	};

	struct pipe_stencil_state default_stencil = {
		.enabled = 0,
		.func = PIPE_FUNC_ALWAYS,
		.fail_op = MALI_STENCIL_KEEP,
		.zfail_op = MALI_STENCIL_KEEP,
		.zpass_op = MALI_STENCIL_KEEP,
		.writemask = 0xFF,
		.valuemask = 0xFF
	};

	trans_make_stencil_state(&default_stencil, &shader.stencil_front);
	shader.stencil_mask_front = default_stencil.writemask;

	trans_make_stencil_state(&default_stencil, &shader.stencil_back);
	shader.stencil_mask_back = default_stencil.writemask;
	
	if (default_stencil.enabled)
		shader.unknown2_4 |= MALI_STENCIL_TEST;

#if 0
	if (!trans_make_fixed_blend_mode(&default_blend, &shader.blend_equation))
		printf("ERROR: Default shader backend must not trigger blend shader\n");
#endif

    memcpy(&ctx->fragment_shader_core, &shader, sizeof(shader));
}

/* Generates a vertex/tiler job. This is, in some sense, the heart of the
 * graphics command stream. It should be called once per draw, accordding to
 * presentations. Set is_tiler for "tiler" jobs (fragment shader jobs, but in
 * Mali parlance, "fragment" refers to framebuffer writeout). Clear it for
 * vertex jobs. */

static mali_ptr
trans_vertex_tiler_job(struct panfrost_context *ctx, bool is_tiler)
{
	/* Each draw call corresponds to two jobs, and we want to offset to leave room for the set-value job */
	int draw_job_index = 1 + (2 * ctx->draw_count);

	struct mali_job_descriptor_header job = {
		.job_type = is_tiler ? JOB_TYPE_TILER : JOB_TYPE_VERTEX,
		.job_index = draw_job_index + (is_tiler ? 1 : 0),
#ifdef BIT64
		.job_descriptor_size = 1,
#endif 
	};

	/* XXX: What is this? */
#ifdef T6XX
	if (is_tiler)
		job.unknown_flags = ctx->draw_count ? 64 : 1;
#endif

	/* Only tiler jobs have dependencies which are known at this point */

	if (is_tiler) {
		/* Tiler jobs depend on vertex jobs */

		job.job_dependency_index_1 = draw_job_index;

		/* Tiler jobs also depend on the previous tiler job */

		if (ctx->draw_count)
			job.job_dependency_index_2 = draw_job_index - 1;
	}
	struct midgard_payload_vertex_tiler *payload = is_tiler ? &ctx->payload_tiler : &ctx->payload_vertex;

	/* There's some padding hacks on 32-bit */

#ifdef BIT64
	int offset = 0;
#else
	int offset = 4;
#endif

	mali_ptr job_p = panfrost_upload(&ctx->cmdstream, &job, sizeof(job) - offset, true);
	panfrost_upload_sequential(&ctx->cmdstream, payload, sizeof(*payload));
	return job_p;
}

/* Generates a set value job. It's unclear what exactly this does, why it's
 * necessary, and when to call it. */

static mali_ptr
trans_set_value_job(struct panfrost_context *ctx)
{
	struct mali_job_descriptor_header job_0 = {
		.job_type = JOB_TYPE_SET_VALUE,
		.job_descriptor_size = 1,
		.job_index = 1 + (2 * ctx->draw_count),
	};

	struct mali_payload_set_value payload_0 = {
		.out = ctx->scratchpad.gpu + 0x6000,
		.unknown = 0x3,
	};

	mali_ptr job_0_p = panfrost_upload(&ctx->cmdstream, &job_0, sizeof(job_0), true);
	panfrost_upload_sequential(&ctx->cmdstream, &payload_0, sizeof(payload_0));

	return job_0_p;
}

/* Generate a fragment job. This should be called once per frame. (According to
 * presentations, this is supposed to correspond to eglSwapBuffers) */

static mali_ptr
trans_fragment_job(struct panfrost_context *ctx)
{
	/* The frame is complete and therefore the framebuffer descriptor is
	 * ready for linkage and upload */

	mali_ptr fbd = panfrost_upload(&ctx->cmdstream, &ctx->fragment_fbd, sizeof(ctx->fragment_fbd), true);

	/* Upload extra framebuffer info if necessary */
	if (ctx->fragment_fbd.unk3 & MALI_MFBD_EXTRA) {
		panfrost_upload_sequential(&ctx->cmdstream, &ctx->fragment_extra, sizeof(struct bifrost_fb_extra));
	}

	/* Upload (single) render target */
	panfrost_upload_sequential(&ctx->cmdstream, &ctx->fragment_rts[0], sizeof(struct bifrost_render_target) * 1);

	/* Generate the fragment (frame) job */

	struct mali_job_descriptor_header header = {
		.job_type = JOB_TYPE_FRAGMENT,
		.job_index = 1,
#ifdef BIT64
		.job_descriptor_size = 1
#endif
	};

	struct mali_payload_fragment payload = {
		.min_tile_coord = MALI_COORDINATE_TO_TILE_MIN(0, 0),
		.max_tile_coord = MALI_COORDINATE_TO_TILE_MAX(ctx->width, ctx->height),
		.framebuffer = fbd | PANFROST_DEFAULT_FBD,
	};

	/* Normally, there should be no padding. However, fragment jobs are
	 * shared with 64-bit Bifrost systems, and accordingly there is 4-bytes
	 * of zero padding in between. */

	mali_ptr job_pointer = panfrost_upload(&ctx->cmdstream, &header, sizeof(header), true);
	panfrost_upload_sequential(&ctx->cmdstream, &payload, sizeof(payload));

	return job_pointer;
}

/* Emits attributes and varying descriptors, which should be called every draw,
 * excepting some obscure circumstances */

static void
trans_emit_vertex_data(struct panfrost_context *ctx)
{
	/* TODO: Only update the dirtied buffers */
	struct mali_attr attrs[PIPE_MAX_ATTRIBS];
	struct mali_attr varyings[PIPE_MAX_ATTRIBS];

	for (int i = 0; i < ctx->vertex_buffer_count; ++i) {
		struct pipe_vertex_buffer *buf = &ctx->vertex_buffers[i];
		struct panfrost_resource *rsrc = (struct panfrost_resource *) (buf->buffer.resource);

		/* Offset vertex count by draw_start to make sure we upload enough */
		attrs[i].stride = buf->stride;
		attrs[i].size = buf->stride * (ctx->payload_vertex.draw_start + ctx->vertex_count);
		attrs[i].elements = panfrost_upload(&ctx->cmdstream, rsrc->cpu[0] + buf->buffer_offset, attrs[i].size, false) | 1;
	}

	for (int i = 0; i < ctx->varying_count; ++i) {
		varyings[i].elements = (ctx->varying_mem.gpu + ctx->varying_height) | 1;
		varyings[i].size = ctx->varyings_stride[i] * ctx->vertex_count;
		varyings[i].stride = ctx->varyings_stride[i];

		/* gl_Position varying is always last by convention */
		if ((i + 1) == ctx->varying_count)
			ctx->payload_tiler.postfix.position_varying = ctx->varying_mem.gpu + ctx->varying_height;

		/* Varyings appear to need 64-byte alignment */
		ctx->varying_height += ALIGN(varyings[i].size, 64);
	}

	ctx->payload_vertex.postfix.attributes = panfrost_upload(&ctx->cmdstream, attrs, ctx->vertex_buffer_count * sizeof(struct mali_attr), false);

	mali_ptr varyings_p = panfrost_upload(&ctx->cmdstream, &varyings, ctx->varying_count * sizeof(struct mali_attr), false);
	ctx->payload_vertex.postfix.varyings = varyings_p;
	ctx->payload_tiler.postfix.varyings = varyings_p;
}

/* Go through dirty flags and actualise them in the cmdstream. */

static void
trans_emit_for_draw(struct panfrost_context *ctx)
{
	trans_emit_vertex_data(ctx);

	if (ctx->dirty & PAN_DIRTY_RASTERIZER) {
		ctx->payload_tiler.line_width = ctx->rasterizer->base.line_width;
		ctx->payload_tiler.gl_enables = ctx->rasterizer->tiler_gl_enables;

		trans_set_framebuffer_msaa(ctx, FORCE_MSAA || ctx->rasterizer->base.multisample);
	}

	if (ctx->dirty & PAN_DIRTY_VS) {
		assert(ctx->vs);
		ctx->payload_vertex.postfix._shader_upper = panfrost_upload(&ctx->cmdstream_persistent, &ctx->vs->tripipe, sizeof(struct mali_shader_meta), true) >> 4;

		/* Varying descriptor is tied to the vertex shader. Also the
		 * fragment shader, I suppose, but it's generated with the
		 * vertex shader so */

		trans_upload_varyings_descriptor(ctx);
	}

	if (ctx->dirty & PAN_DIRTY_FS) { 
		assert(ctx->fs);
#define COPY(name) ctx->fragment_shader_core.name = ctx->fs->tripipe.name

		COPY(shader);
		COPY(texture_count);
		COPY(sampler_count);
		COPY(attribute_count);
		COPY(varying_count);
		COPY(midgard1.uniform_count);
		COPY(midgard1.work_count);
		COPY(midgard1.unknown1);
		COPY(midgard1.unknown2);

#undef COPY

		ctx->payload_tiler.postfix._shader_upper = panfrost_upload(&ctx->cmdstream_persistent, &ctx->fragment_shader_core, sizeof(struct mali_shader_meta), true) >> 4;

#ifdef T8XX
		/* TODO: MERGE FOR WORKING BLENDING */
		struct mali_blend_meta blend_meta[] = {
			{
				.unk1 = 0x200,
				.blend_equation_1 = {
					.rgb_mode = 0x122,
					.alpha_mode = 0x122,
					.color_mask = MALI_MASK_R | MALI_MASK_G | MALI_MASK_B | MALI_MASK_A,
				},
				.blend_equation_2 = {
					.rgb_mode = 0x122,
					.alpha_mode = 0x122,
					.color_mask = MALI_MASK_R | MALI_MASK_G | MALI_MASK_B | MALI_MASK_A,
				},
			},
		};

		panfrost_upload_sequential(&ctx->cmdstream_persistent, blend_meta, sizeof(blend_meta));
#endif
	}

	if (ctx->dirty & PAN_DIRTY_VERTEX) {
		ctx->payload_vertex.postfix.attribute_meta = panfrost_upload(&
				ctx->cmdstream_persistent, &ctx->vertex->hw,
				sizeof(struct mali_attr_meta) * ctx->vertex->num_elements, false);
	}

	if (ctx->dirty & PAN_DIRTY_VIEWPORT) {
		ctx->payload_tiler.postfix.viewport = panfrost_upload(&ctx->cmdstream, &ctx->viewport, sizeof(struct mali_viewport), false);
	}

	if (ctx->dirty & PAN_DIRTY_SAMPLERS) {
		/* Upload samplers back to back, no padding */

		for (int t = 0; t <= PIPE_SHADER_FRAGMENT; ++t) {
			mali_ptr samplers_base = 0;

			for (int i = 0; i < ctx->sampler_count[t]; ++i) {
				if (i)
					panfrost_upload_sequential(&ctx->cmdstream, &ctx->samplers[t][i]->hw, sizeof(struct mali_sampler_descriptor));
				else
					samplers_base = panfrost_upload(&ctx->cmdstream, &ctx->samplers[t][i]->hw, sizeof(struct mali_sampler_descriptor), true);
			}

			if (t == PIPE_SHADER_FRAGMENT)
				ctx->payload_tiler.postfix.sampler_descriptor = samplers_base;
			else if (t == PIPE_SHADER_VERTEX)
				ctx->payload_vertex.postfix.sampler_descriptor = samplers_base;
			else
				assert(0);
		}
	}

	if (ctx->dirty & PAN_DIRTY_TEXTURES) {
		for (int t = 0; t <= PIPE_SHADER_FRAGMENT; ++t) {
			/* Shortcircuit */
			if (!ctx->sampler_view_count[t]) continue;

			uint64_t trampolines[PIPE_MAX_SHADER_SAMPLER_VIEWS];

			for (int i = 0; i < ctx->sampler_view_count[t]; ++i) {
				/* XXX: Why does this work? */
				if (!ctx->sampler_views[t][i])
					continue;

				struct pipe_resource *tex_rsrc = ctx->sampler_views[t][i]->base.texture;
				struct panfrost_resource *rsrc = (struct panfrost_resource *) tex_rsrc;

				/* Inject the address in. */
				for (int l = 0; l < (tex_rsrc->last_level + 1); ++l)
					ctx->sampler_views[t][i]->hw.swizzled_bitmaps[l] = rsrc->gpu[l];

				/* Workaround maybe-errata (?) with non-mipmaps */
				int s = ctx->sampler_views[t][i]->hw.nr_mipmap_levels;

				if (!rsrc->is_mipmap) {
#ifdef T6XX
					/* HW ERRATA, not needed after T6XX */
					ctx->sampler_views[t][i]->hw.swizzled_bitmaps[1] = rsrc->gpu[0];

					ctx->sampler_views[t][i]->hw.unknown3A = 1;
#endif
					ctx->sampler_views[t][i]->hw.nr_mipmap_levels = 0;
				}

				trampolines[i] = panfrost_upload(&ctx->cmdstream, &ctx->sampler_views[t][i]->hw, sizeof(struct mali_texture_descriptor), false);

				/* Restore */
				ctx->sampler_views[t][i]->hw.nr_mipmap_levels = s;
				ctx->sampler_views[t][i]->hw.unknown3A = 0;
			}

			mali_ptr trampoline = panfrost_upload(&ctx->cmdstream, trampolines, sizeof(uint64_t) * ctx->sampler_view_count[t], false);

			if (t == PIPE_SHADER_FRAGMENT)
				ctx->payload_tiler.postfix.texture_trampoline = trampoline;
			else if (t == PIPE_SHADER_VERTEX)
				ctx->payload_vertex.postfix.texture_trampoline = trampoline;
			else
				assert(0);
		}
	}

	for (int i = 0; i < PIPE_SHADER_TYPES; ++i) {
		struct panfrost_constant_buffer *buf = &ctx->constant_buffer[i];

		if (buf->dirty) {
			mali_ptr address;
			
			if (buf->size)
				address = panfrost_upload(&ctx->cmdstream, buf->buffer, buf->size, false);
			else
				address = panfrost_reserve(&ctx->cmdstream, 256);
			
			switch (i) {
				case PIPE_SHADER_VERTEX:
					ctx->payload_vertex.postfix.uniforms = address;
					break;

				case PIPE_SHADER_FRAGMENT:
					ctx->payload_tiler.postfix.uniforms = address;
					break;

				default:
					printf("Unknown shader stage %d in uniform upload\n", i);
					break;
			}

			buf->dirty = 0;
		}
	}

	mali_ptr ubuf = panfrost_reserve(&ctx->cmdstream, 4 * 4 * 2);
	struct mali_uniform_buffer_meta uniform_buffers[] = {
		{
			.size = MALI_POSITIVE(2),
			.ptr = ubuf >> 2,
		},
	};

	mali_ptr ubufs = panfrost_upload(&ctx->cmdstream, uniform_buffers, sizeof(uniform_buffers), false);
	ctx->payload_vertex.postfix.uniform_buffers = ubufs;

	ctx->dirty = 0;
}

/* Corresponds to exactly one draw, but does not submit anything */

void
trans_queue_draw(struct panfrost_context *ctx)
{
	/* TODO: Expand the array? */
	if (ctx->draw_count >= MAX_DRAW_CALLS) {
		printf("Job buffer overflow, ignoring draw\n");
		return;
	}

	/* Handle dirty flags now */
	trans_emit_for_draw(ctx);

	ctx->vertex_jobs[ctx->draw_count] = trans_vertex_tiler_job(ctx, false);
	ctx->tiler_jobs[ctx->draw_count] = trans_vertex_tiler_job(ctx, true);
	ctx->draw_count++;
}

/* At the end of the frame, the vertex and tiler jobs are linked together and
 * then the fragment job is plonked at the end. Set value job is first for
 * unknown reasons. */

#define JOB_DESC(ptr) ((struct mali_job_descriptor_header *) (uintptr_t) (ptr - mem.gpu + (uintptr_t) mem.cpu))
static void
trans_link_job_pair(struct panfrost_memory mem, mali_ptr first, mali_ptr next)
{
	if (JOB_DESC(first)->job_descriptor_size)
		JOB_DESC(first)->next_job_64 = (u64) (uintptr_t) next;
	else
		JOB_DESC(first)->next_job_32 = (u32) (uintptr_t) next;
}

static void
trans_link_jobs(struct panfrost_context *ctx)
{
	if (ctx->draw_count) {
		/* Generate the set_value_job */
		ctx->set_value_job = trans_set_value_job(ctx);

		struct panfrost_memory mem = ctx->cmdstream;

		/* Have the first vertex job depend on the set value job */
		JOB_DESC(ctx->vertex_jobs[0])->job_dependency_index_1 = JOB_DESC(ctx->set_value_job)->job_index;

		/* SV -> V */
		trans_link_job_pair(mem, ctx->set_value_job, ctx->vertex_jobs[0]);
	}

	/* V -> V/T ; T -> T/null */
	for (int i = 0; i < ctx->draw_count; ++i) {
		bool isLast = (i + 1) == ctx->draw_count;

		trans_link_job_pair(ctx->cmdstream, ctx->vertex_jobs[i], isLast ? ctx->tiler_jobs[0] : ctx->vertex_jobs[i + 1]);
		trans_link_job_pair(ctx->cmdstream, ctx->tiler_jobs[i], isLast ? 0 : ctx->tiler_jobs[i + 1]);
	}
}

/* Use to allocate atom numbers for jobs. We probably want to overhaul this in kernel space at some point. */
uint8_t atom_counter = 0;

static uint8_t
allocate_atom()
{
	atom_counter++;

	/* Workaround quirk where atoms must be strictly positive */

	if (atom_counter == 0)
		atom_counter++;

	return atom_counter;
}

int last_fragment_id = -1;

/* Forces a flush, to make sure everything is consistent.
 * Bad for parallelism. Necessary for glReadPixels etc. Use cautiously.
 */

static void
force_flush_fragment(struct panfrost_context *ctx)
{
	if (last_fragment_id != -1) {
		uint8_t ev[/* 1 */ 4 + 4 + 8 + 8];

		do {
			read(ctx->fd, ev, sizeof(ev));
		} while (ev[4] != last_fragment_id);
	}
}

/* The entire frame is in memory -- send it off to the kernel! */

static void
trans_submit_frame(struct panfrost_context *ctx)
{
	/* Edge case if screen is cleared and nothing else */
	bool has_draws = ctx->draw_count > 0;

	/* A number of jobs are batched -- this must be linked and cleared */
	trans_link_jobs(ctx);

	ctx->draw_count = 0;

#ifndef DRY_RUN
	/* XXX: FLUSH SHOULD HAPPEN HERE */
	//force_flush_fragment(ctx);

	mali_external_resource framebuffer[] = {
		ctx->framebuffer.gpu | MALI_EXT_RES_ACCESS_EXCLUSIVE,
	};

	struct mali_jd_atom_v2 atoms[] = {
		{
			.jc = ctx->set_value_job,
			.atom_number = allocate_atom(),
			.compat_core_req = MALI_JD_REQ_CS | MALI_JD_REQ_T | MALI_JD_REQ_CF | MALI_JD_REQ_COHERENT_GROUP | MALI_JD_REQ_EVENT_NEVER
		},
		{
			.jc = trans_fragment_job(ctx),
			.nr_ext_res = 1,
			.ext_res_list = framebuffer,
			.atom_number = allocate_atom(),
			.compat_core_req = MALI_JD_REQ_FS /*| MALI_JD_REQ_EXTERNAL_RESOURCES | MALI_JD_REQ_SKIP_CACHE_START*/
		},
	};

	if (has_draws) {
		struct mali_ioctl_job_submit submit = {
			.addr = atoms,
			.nr_atoms = 1,
			.stride = sizeof(struct mali_jd_atom_v2),
		};

		if (pandev_ioctl(ctx->fd, MALI_IOCTL_JOB_SUBMIT, &submit))
		    printf("Error submitting\n");
	}


	/* Submit jobs seperately to workaround the missing tile issue? XXX
	 * FIXME when we redesign the kernel interface */

	struct mali_ioctl_job_submit submit2 = {
		.addr = atoms + 1,
		.nr_atoms = 1,
		.stride = sizeof(struct mali_jd_atom_v2),
	};

	if (pandev_ioctl(ctx->fd, MALI_IOCTL_JOB_SUBMIT, &submit2))
	    printf("Error submitting\n");

	last_fragment_id = atoms[1].atom_number;

	/* XXX XXX XXX THIS KILLS PERF */
	force_flush_fragment(ctx);
#endif
}

static void
panfrost_flush(
		struct pipe_context *pipe,
		struct pipe_fence_handle ** fence,
		unsigned flags)
{
	struct panfrost_context *ctx = panfrost_context(pipe);

	/* If there is nothing drawn, skip the frame */
	if (!ctx->draw_count && !(ctx->dirty & PAN_DIRTY_DUMMY)) return;

	/* Submit the frame itself */
	trans_submit_frame(ctx);

	/* Prepare for the next frame */
	trans_invalidate_frame(ctx);

#ifdef USE_SLOWFB
#ifndef DRY_RUN
	/* Display the frame in our cute little window */
	slowfb_update((uint8_t*) ctx->framebuffer.cpu, ctx->width, ctx->height);
	printf("%x\n", ctx->depth_stencil_buffer.cpu[0]);
#endif
#endif
}	

#define DEFINE_CASE(c) case PIPE_PRIM_##c: return MALI_GL_##c;

static int
g2m_draw_mode(enum pipe_prim_type mode)
{
	switch(mode) {
		DEFINE_CASE(POINTS);
		DEFINE_CASE(LINES);
		DEFINE_CASE(LINE_LOOP);
		DEFINE_CASE(LINE_STRIP);
		DEFINE_CASE(TRIANGLES);
		DEFINE_CASE(TRIANGLE_STRIP);
		DEFINE_CASE(TRIANGLE_FAN);

		default:
			printf("Illegal draw mode %d\n", mode);
			return MALI_GL_LINE_LOOP;
	}
}

#undef DEFINE_CASE

static unsigned
trans_translate_index_size(unsigned size)
{
	switch (size) {
		case 1: return MALI_DRAW_INDEXED_UINT8;
		case 2: return MALI_DRAW_INDEXED_UINT16;
		case 4: return MALI_DRAW_INDEXED_UINT32;
		default:
			printf("Unknown index size %d\n", size);
			return 0;
	}
}

static void
panfrost_draw_vbo(
		struct pipe_context *pipe,
		const struct pipe_draw_info *info)
{
	struct panfrost_context *ctx = panfrost_context(pipe);

	ctx->payload_vertex.draw_start = info->start;
	ctx->payload_tiler.draw_start = info->start;

	int mode = info->mode;
#ifdef HAVE_DRI3
	/* Fallback for non-ES draw modes */

	if (info->mode >= PIPE_PRIM_QUADS) {
		mode = PIPE_PRIM_TRIANGLE_STRIP;
		/*
		util_primconvert_save_rasterizer_state(ctx->primconvert, &ctx->rasterizer->base);
		util_primconvert_draw_vbo(ctx->primconvert, info);
		printf("Fallback\n");
		return; */
	}
#endif

        ctx->payload_tiler.prefix.draw_mode = g2m_draw_mode(mode);

	ctx->vertex_count = info->count;

        ctx->payload_vertex.prefix.invocation_count = MALI_POSITIVE(ctx->vertex_count);
        ctx->payload_tiler.prefix.invocation_count = MALI_POSITIVE(ctx->vertex_count);
	
	ctx->payload_tiler.prefix.unknown_draw |= /*0x3000*/0x18000;

	if (info->index_size) {
		ctx->payload_tiler.prefix.index_count = MALI_POSITIVE(info->count);

		//assert(!info->restart_index); /* TODO: Research */
		assert(!info->index_bias);
		//assert(!info->min_index); /* TODO: Use value */

		ctx->payload_tiler.prefix.unknown_draw |= trans_translate_index_size(info->index_size);

		const uint32_t *ibuf = NULL;

		if (info->has_user_indices) {
			ibuf = info->index.user;
		} else {
			struct panfrost_resource *rsrc = (struct panfrost_resource *) (info->index.resource);
			ibuf = (const uint32_t *) rsrc->cpu[0];
		}

		ctx->payload_tiler.prefix.indices = panfrost_upload(&ctx->cmdstream, ibuf, info->count * info->index_size, true);
	} else {
		/* Index count == vertex count, if no indexing is applied, as
		 * if it is internally indexed in the expected order */

		ctx->payload_tiler.prefix.index_count = MALI_POSITIVE(ctx->vertex_count);

		/* Reverse index state */
		ctx->payload_tiler.prefix.unknown_draw &= ~MALI_DRAW_INDEXED_UINT32;
		ctx->payload_tiler.prefix.indices = (uintptr_t) NULL;
	}

	/* Fire off the draw itself */
	trans_queue_draw(ctx);
}

/* CSO state */

static void
panfrost_generic_cso_delete(struct pipe_context *pctx, void *hwcso)
{
	free(hwcso);
}

static void*
panfrost_create_rasterizer_state(
		struct pipe_context *pctx,
		const struct pipe_rasterizer_state *cso)
{
	struct panfrost_rasterizer *so = CALLOC_STRUCT(panfrost_rasterizer);

	so->base = *cso;

	/* Bitmask, unknown meaning of the start value */
#ifdef T8XX
	so->tiler_gl_enables = 0x7;
#else
	so->tiler_gl_enables = 0x105;
#endif


	so->tiler_gl_enables |= MALI_GL_FRONT_FACE(
			cso->front_ccw ? MALI_GL_CCW : MALI_GL_CW);

	if (cso->cull_face & PIPE_FACE_FRONT)
		so->tiler_gl_enables |= MALI_GL_CULL_FACE_FRONT;

	if (cso->cull_face & PIPE_FACE_BACK)
		so->tiler_gl_enables |= MALI_GL_CULL_FACE_BACK;

	return so;
}

static void
panfrost_bind_rasterizer_state(
		struct pipe_context *pctx,
		void *hwcso)
{
	struct panfrost_context *ctx = panfrost_context(pctx);

	ctx->rasterizer = hwcso;
	ctx->dirty |= PAN_DIRTY_RASTERIZER;
}

static void*
panfrost_create_vertex_elements_state(
		struct pipe_context *pctx,
		unsigned num_elements,
		const struct pipe_vertex_element *elements)
{
	struct panfrost_vertex_state *so = CALLOC_STRUCT(panfrost_vertex_state);

	so->num_elements = num_elements;
	memcpy(so->pipe, elements, sizeof(*elements) * num_elements);

	for (int i = 0; i < num_elements; ++i) {
		/* XXX: What if they're all packed into the same buffer? */
		so->hw[i].index = elements[i].vertex_buffer_index;
		
#ifdef HAVE_DRI3
		enum pipe_format fmt = elements[i].src_format;
		const struct util_format_description *desc = util_format_description(fmt);
		struct util_format_channel_description chan = desc->channel[0];

		int type = 0;

		switch (chan.type) {
			case UTIL_FORMAT_TYPE_UNSIGNED:
			case UTIL_FORMAT_TYPE_SIGNED:
				if (chan.size == 8)
					type = MALI_ATYPE_BYTE;
				else if (chan.size == 16)
					type = MALI_ATYPE_SHORT;
				else if (chan.size == 32)
					type = MALI_ATYPE_INT;
				else
					printf("BAD INT SIZE %d\n", chan.size);

				break;

			case UTIL_FORMAT_TYPE_FLOAT:
				type = MALI_ATYPE_FLOAT;
				break;

			default:
				printf("Unknown atype %d\n", chan.type);
		}

		so->hw[i].type = type;
		so->nr_components[i] = desc->nr_channels;
		so->hw[i].nr_components = MALI_POSITIVE(4); /* XXX */
                so->hw[i].not_normalised = !chan.normalized;

		/* Bit used for both signed/unsigned and full/half designation */
		so->hw[i].is_int_signed =
			(chan.type == UTIL_FORMAT_TYPE_SIGNED) ? 1 :
			(chan.type == UTIL_FORMAT_TYPE_FLOAT && chan.size != 32) ? 1 :
			0;
#else
		so->hw[i].type = 7;
		so->hw[i].nr_components = MALI_POSITIVE(4);
		so->nr_components[i] = 4;
                so->hw[i].not_normalised = 1;
		so->hw[i].is_int_signed = 0;
#endif

                so->hw[i].unknown1 = 0x2a22;
                so->hw[i].unknown2 = 0x1;

		/* The field itself should probably be shifted over */
                so->hw[i].src_offset = elements[i].src_offset;
	}

	return so;
}

static void
panfrost_bind_vertex_elements_state(
		struct pipe_context *pctx,
		void *hwcso)
{
	struct panfrost_context *ctx = panfrost_context(pctx);

	ctx->vertex = hwcso;
	ctx->dirty |= PAN_DIRTY_VERTEX;
}

static void *
panfrost_create_shader_state(
		struct pipe_context *pctx,
		const struct pipe_shader_state *cso)
{
	struct panfrost_shader_state *so = CALLOC_STRUCT(panfrost_shader_state);
	so->base = *cso;
	return so;
}

static void
panfrost_delete_shader_state(
		struct pipe_context *pctx,
		void *so)
{
	free(so);
}

static void *
panfrost_create_sampler_state(
		struct pipe_context *pctx,
		const struct pipe_sampler_state *cso)
{
	struct panfrost_sampler_state *so = CALLOC_STRUCT(panfrost_sampler_state);
	so->base = *cso;

	/* sampler_state corresponds to mali_sampler_descriptor, which we can generate entirely here */

	struct mali_sampler_descriptor sampler_descriptor = {
		.filter_mode = MALI_GL_TEX_MIN(translate_tex_filter(cso->min_img_filter))
			     | MALI_GL_TEX_MAG(translate_tex_filter(cso->mag_img_filter))
			     | translate_mip_filter(cso->min_mip_filter)
			     | 0x20,

		.wrap_s = translate_tex_wrap(cso->wrap_s),
		.wrap_t = translate_tex_wrap(cso->wrap_t),
		.wrap_r = translate_tex_wrap(cso->wrap_r),
		.compare_func = trans_translate_alt_compare_func(cso->compare_func),
		.border_color = {
			cso->border_color.f[0],
			cso->border_color.f[1],
			cso->border_color.f[2],
			cso->border_color.f[3]
		},
		.min_lod = FIXED_16(0.0),
		.max_lod = FIXED_16(31.0),
		.unknown2 = 1,
	};

	so->hw = sampler_descriptor;

	return so;
}

static void
panfrost_bind_sampler_states(
		struct pipe_context *pctx,
		enum pipe_shader_type shader,
		unsigned start_slot, unsigned num_sampler,
		void **sampler)
{
	assert(start_slot == 0);

	struct panfrost_context *ctx = panfrost_context(pctx);
	
	/* XXX: Should upload, not just copy? */
	ctx->sampler_count[shader] = num_sampler;
	memcpy(ctx->samplers[shader], sampler, num_sampler * sizeof (void *));

	ctx->dirty |= PAN_DIRTY_SAMPLERS;
}

static void
panfrost_bind_fs_state(
		struct pipe_context *pctx,
		void *hwcso)
{
	struct panfrost_context *ctx = panfrost_context(pctx);

	ctx->fs = hwcso;

	/* XXX TODO: Actual compilation pipeline */
	if (hwcso) {
		if (!ctx->fs->compiled) {
			panfrost_shader_compile(ctx, &ctx->fs->tripipe, NULL, JOB_TYPE_TILER, hwcso);
			ctx->fs->compiled = true;
		}
	}

	ctx->dirty |= PAN_DIRTY_FS;
}

static void
panfrost_bind_vs_state(
		struct pipe_context *pctx,
		void *hwcso)
{
	struct panfrost_context *ctx = panfrost_context(pctx);

	ctx->vs = hwcso;

	if (hwcso) {
		if (!ctx->vs->compiled) {
			panfrost_shader_compile(ctx, &ctx->vs->tripipe, NULL, JOB_TYPE_VERTEX, hwcso);
			ctx->vs->compiled = true;
		}
	} 

	ctx->dirty |= PAN_DIRTY_VS;
}

static void
panfrost_set_vertex_buffers(
		struct pipe_context *pctx,
		unsigned start_slot,
		unsigned num_buffers,
		const struct pipe_vertex_buffer *buffers)
{
	struct panfrost_context *ctx = panfrost_context(pctx);
	assert(num_buffers <= PIPE_MAX_ATTRIBS);

	/* XXX: Dirty tracking? etc */
	if (buffers) {
		size_t sz = sizeof(buffers[0]) * num_buffers;
		ctx->vertex_buffers = malloc(sz);
		ctx->vertex_buffer_count = num_buffers;
		memcpy(ctx->vertex_buffers, buffers, sz);
	} else {
		/* XXX leak */
		ctx->vertex_buffers = NULL;
		ctx->vertex_buffer_count = 0;
	}
}

static void
panfrost_set_constant_buffer(
		struct pipe_context *pctx,
		enum pipe_shader_type shader, uint index,
		const struct pipe_constant_buffer *buf)
{
	struct panfrost_context *ctx = panfrost_context(pctx);
	struct panfrost_constant_buffer *pbuf = &ctx->constant_buffer[shader];

	size_t sz = buf ? buf->buffer_size : 0;

	/* Free previous buffer */

	pbuf->dirty = true;
	pbuf->size = sz;

	if (pbuf->buffer) {
		free(pbuf->buffer);
		pbuf->buffer = NULL;
	}

	/* If unbinding, we're done */

	if (!buf)
		return;

	/* Multiple constant buffers not yet supported */
	assert(index == 0);

	const void *cpu;

	struct panfrost_resource *rsrc = (struct panfrost_resource *) (buf->buffer);

	if (rsrc) {
		cpu = rsrc->cpu[0];
	} else if (buf->user_buffer) {
		cpu = buf->user_buffer;
	} else {
		printf("No constant buffer?\n");
		return;
	}

	/* Copy the constant buffer into the driver context for later upload */

	pbuf->buffer = malloc(sz);
	memcpy(pbuf->buffer, cpu, sz);
}

static void
panfrost_set_stencil_ref(
		struct pipe_context *pctx,
		const struct pipe_stencil_ref *ref)
{
	struct panfrost_context *ctx = panfrost_context(pctx);

	ctx->stencil_ref = ref;

	ctx->fragment_shader_core.stencil_front.ref = ref->ref_value[0];
	ctx->fragment_shader_core.stencil_back.ref = ref->ref_value[1];

	/* Shader core dirty */
	ctx->dirty |= PAN_DIRTY_FS;
}

static struct pipe_sampler_view *
panfrost_create_sampler_view(
		struct pipe_context *pctx,
		struct pipe_resource *texture,
		const struct pipe_sampler_view *template)
{
	struct panfrost_sampler_view *so = CALLOC_STRUCT(panfrost_sampler_view);

#ifdef HAVE_DRI3
	pipe_reference(NULL, &texture->reference);
#endif

	struct panfrost_resource *prsrc = (struct panfrost_resource *) texture;

	so->base = *template;
	so->base.texture = texture;
	so->base.reference.count = 1;
	so->base.context = pctx;

	/* sampler_views correspond to texture descriptors, minus the texture
	 * (data) itself. So, we serialise the descriptor here and cache it for
	 * later. */

	/* TODO: Other types of textures */
	assert(template->target == PIPE_TEXTURE_2D);

	/* Make sure it's something with which we're familiar */
	assert(prsrc->bytes_per_pixel >= 1 && prsrc->bytes_per_pixel <= 4);

	struct mali_texture_descriptor texture_descriptor = {
		.width = MALI_POSITIVE(texture->width0),
		.height = MALI_POSITIVE(texture->height0),
		.depth = MALI_POSITIVE(texture->depth0),

		/* TODO: Decode */
		.format = {
			.bottom = 0x88,
			.unk1 = 0xb,
			.component_size = 0x3,
			.nr_channels = MALI_POSITIVE(prsrc->bytes_per_pixel),
			.typeA = 5,

			.usage1 = 0x0,
			.is_not_cubemap = 1,
			.usage2 = 0x11,
		},

		.swizzle_r = trans_translate_texture_swizzle(template->swizzle_r),
		.swizzle_g = trans_translate_texture_swizzle(template->swizzle_g),
		.swizzle_b = trans_translate_texture_swizzle(template->swizzle_b),
		.swizzle_a = trans_translate_texture_swizzle(template->swizzle_a),
	};

	/* TODO: Other base levels require adjusting dimensions / level numbers / etc */
	assert (template->u.tex.first_level == 0);
	
	texture_descriptor.nr_mipmap_levels = template->u.tex.last_level - template->u.tex.first_level;

	so->hw = texture_descriptor;

	return (struct pipe_sampler_view *) so;
}

static void
panfrost_set_sampler_views(
		struct pipe_context *pctx,
		enum pipe_shader_type shader,
		unsigned start_slot, unsigned num_views,
		struct pipe_sampler_view **views)
{
	struct panfrost_context *ctx = panfrost_context(pctx);

	assert(start_slot == 0);

	ctx->sampler_view_count[shader] = num_views;
	memcpy(ctx->sampler_views[shader], views, num_views * sizeof (void *));

	ctx->dirty |= PAN_DIRTY_TEXTURES;
}

static void
panfrost_sampler_view_destroy(
		struct pipe_context *pctx,
		struct pipe_sampler_view *views)
{
	//struct panfrost_context *ctx = panfrost_context(pctx);

	/* TODO */
	
	free(views);
}

/* TODO: Proper resource tracking depends on, well, proper resources. This
 * section will be woefully incomplete until we can sort out a proper DRM
 * driver. */

struct pipe_resource *
panfrost_resource_create_front(struct pipe_screen *screen,
			       const struct pipe_resource *template,
			       const void *map_front_private)
{
	struct panfrost_resource *so = CALLOC_STRUCT(panfrost_resource);

	so->base = *template;
	so->base.screen = screen;
#ifdef HAVE_DRI3
	pipe_reference_init(&so->base.reference, 1);

	/* Fill out fields based on format itself */
	so->bytes_per_pixel = util_format_get_blocksize(template->format);
#else
	so->bytes_per_pixel = 4;
#endif

	/* TODO: Alignment? */
	so->stride = so->bytes_per_pixel * template->width0;
	
	size_t sz = so->stride;

	if (template->height0) sz *= template->height0;
	if (template->depth0) sz *= template->depth0;

	if (!(template->bind & (PIPE_BIND_DISPLAY_TARGET | PIPE_BIND_DEPTH_STENCIL))) {
		/* TODO: For linear resources, allocate straight on the cmdstream for
		 * zero-copy operation */

		for (int l = 0; l < (template->last_level + 1); ++l) {
			so->cpu[l] = malloc(sz);
			//sz >>= 2;
		}
	}

	return (struct pipe_resource *)so;
}

static struct pipe_resource *
panfrost_resource_create(struct pipe_screen *screen,
		const struct pipe_resource *templat)
{
	return panfrost_resource_create_front(screen, templat, NULL);
}

static void
panfrost_resource_destroy(struct pipe_screen *screen,
		struct pipe_resource *pt)
{
	printf("--resource destroy--\n");
	/* TODO */
}

static void *
panfrost_transfer_map(struct pipe_context *pctx,
                      struct pipe_resource *resource,
                      unsigned level,
                      unsigned usage,  /* a combination of PIPE_TRANSFER_x */
                      const struct pipe_box *box,
                      struct pipe_transfer **out_transfer)
{
	struct panfrost_context *ctx = panfrost_context(pctx);
	struct panfrost_resource *rsrc = (struct panfrost_resource *) resource;

	struct pipe_transfer *transfer = CALLOC_STRUCT(pipe_transfer);

	transfer->level = level;
	transfer->usage = usage;
	transfer->box = *box;
	transfer->stride = rsrc->stride;
	assert(!transfer->box.z);

#ifdef HAVE_DRI3
        pipe_resource_reference(&transfer->resource, resource);
#else
	transfer->resource = resource;
#endif

	*out_transfer = transfer;

	/* If non-zero level, it's a mipmapped resource and needs to be treated as such */
	rsrc->is_mipmap |= transfer->level;

	/* Direct mapping are not possible with tiled textures. */

	if (transfer->usage & PIPE_TRANSFER_MAP_DIRECTLY) {
		switch (resource->target) {
			case PIPE_TEXTURE_2D:
				return NULL;
			default:
				break;
		}
	}

	if (resource->bind & PIPE_BIND_DISPLAY_TARGET) {
		/* Mipmapped readpixels?! */
		assert(level == 0);

		/* Set the CPU mapping to that of the framebuffer in memory, untiled */
		rsrc->cpu[level] = ctx->framebuffer.cpu;
		printf("Display target so %p\n", ctx->framebuffer.cpu);
	} else if (resource->bind & PIPE_BIND_DEPTH_STENCIL) {
		/* Mipmapped readpixels?! */
		assert(level == 0);

		/* Set the CPU mapping to that of the depth/stencil buffer in memory, untiled */
		rsrc->cpu[level] = ctx->depth_stencil_buffer.cpu;
		printf("DS target so %p\n", ctx->depth_stencil_buffer.cpu);

	}

	return rsrc->cpu[level] + transfer->box.x + transfer->box.y * transfer->stride;
}

static void
panfrost_set_framebuffer_state(struct pipe_context *pctx,
                               const struct pipe_framebuffer_state *fb)
{
#ifdef HAVE_DRI3
	struct panfrost_context *ctx = panfrost_context(pctx);

   for (int i = 0; i < PIPE_MAX_COLOR_BUFS; i++) {
      struct pipe_surface *cb = i < fb->nr_cbufs ? fb->cbufs[i] : NULL;

      /* check if changing cbuf */
      if (ctx->pipe_framebuffer.cbufs[i] != cb) {
	      if (i != 0) {
		      printf("XXX: Multiple render targets not supported before t7xx!\n");
		      break;
	      }

         /* assign new */
         pipe_surface_reference(&ctx->pipe_framebuffer.cbufs[i], cb);
	 
	 struct panfrost_screen* scr = (struct panfrost_screen *) pctx->screen;
	 struct pipe_surface *surf = ctx->pipe_framebuffer.cbufs[i];
      }
   }

   ctx->pipe_framebuffer.nr_cbufs = fb->nr_cbufs;

   ctx->pipe_framebuffer.width = fb->width;
   ctx->pipe_framebuffer.height = fb->height;
   ctx->pipe_framebuffer.samples = fb->samples;
   ctx->pipe_framebuffer.layers = fb->layers;
#endif
}

#define mem_dup(mem, sz) (memcpy(malloc(sz), mem, sz))

static void *
panfrost_create_blend_state(struct pipe_context *pipe,
                            const struct pipe_blend_state *blend)
{
	return mem_dup(blend, sizeof(*blend));
}

static void
panfrost_bind_blend_state(struct pipe_context *pipe,
                          void *cso)
{
	struct panfrost_context *ctx = panfrost_context(pipe);
	struct pipe_blend_state *blend = (struct pipe_blend_state *) cso;
	ctx->blend = blend;

	if (!blend)
		return;

	/* TODO: The following features are not yet implemented */
	assert(!blend->logicop_enable);
	assert(!blend->alpha_to_coverage);
	assert(!blend->alpha_to_one);

	SET_BIT(ctx->fragment_shader_core.unknown2_4, MALI_NO_DITHER, !blend->dither);

#if 0
	/* Assume one color buffer atm TODO */
	/* TODO: Move to CSO create for perf improvement */
	if (!trans_make_fixed_blend_mode(&blend->rt[0], &ctx->fragment_shader_core.blend_equation)) {
		printf("ERROR: Blend shaders not yet implemented\n");
		/* TODO: Handle */
		if (!trans_make_fixed_blend_mode(&default_blend, &ctx->fragment_shader_core.blend_equation))
			printf("ERROR: Default shader backend must not trigger blend shader\n");
		//assert(0);
	}
#endif

	/* Shader itself is not dirty, but the shader core is */
	ctx->dirty |= PAN_DIRTY_FS;
}

static void
panfrost_delete_blend_state(struct pipe_context *pipe,
                            void *blend)
{
	free(blend);
}

static void
panfrost_set_blend_color(struct pipe_context *pipe,
                         const struct pipe_blend_color *blend_color)
{
   //struct panfrost_context *panfrost = panfrost_context(pipe);
}

static void *
panfrost_create_depth_stencil_state(struct pipe_context *pipe,
                                    const struct pipe_depth_stencil_alpha_state *depth_stencil)
{
   return mem_dup(depth_stencil, sizeof(*depth_stencil));
}

static void
panfrost_bind_depth_stencil_state(struct pipe_context *pipe,
					void *cso)		
{
	struct panfrost_context *ctx = panfrost_context(pipe);
	const struct pipe_depth_stencil_alpha_state *depth_stencil = cso;
	ctx->depth_stencil = depth_stencil;

	if (!depth_stencil)
		return;

	/* Alpha does not exist on ES2... */
	assert(!depth_stencil->alpha.enabled);

	/* Stencil state */
	SET_BIT(ctx->fragment_shader_core.unknown2_4, MALI_STENCIL_TEST, depth_stencil->stencil[0].enabled); /* XXX: which one? */

	trans_make_stencil_state(&depth_stencil->stencil[0], &ctx->fragment_shader_core.stencil_front);
	ctx->fragment_shader_core.stencil_mask_front = depth_stencil->stencil[0].writemask;

	trans_make_stencil_state(&depth_stencil->stencil[1], &ctx->fragment_shader_core.stencil_back);
	ctx->fragment_shader_core.stencil_mask_back = depth_stencil->stencil[1].writemask;

	/* Depth state (TODO: Refactor) */
	SET_BIT(ctx->fragment_shader_core.unknown2_3, MALI_DEPTH_TEST, depth_stencil->depth.enabled);
	
	int func = depth_stencil->depth.enabled ? depth_stencil->depth.func : PIPE_FUNC_ALWAYS;

	ctx->fragment_shader_core.unknown2_3 &= ~MALI_DEPTH_FUNC_MASK;
	ctx->fragment_shader_core.unknown2_3 |= MALI_DEPTH_FUNC(trans_translate_compare_func(func));

	/* Bounds test not implemented */
	assert(!depth_stencil->depth.bounds_test);

	ctx->dirty |= PAN_DIRTY_FS;
}

static void
panfrost_delete_depth_stencil_state(struct pipe_context *pipe, void *depth)
{
   free( depth );
}

static void
panfrost_set_sample_mask(struct pipe_context *pipe,
                         unsigned sample_mask)
{
}

static struct pipe_surface *
panfrost_create_surface(struct pipe_context *pipe,
                        struct pipe_resource *pt,
                        const struct pipe_surface *surf_tmpl)
{
   struct pipe_surface *ps = NULL;

#ifdef HAVE_DRI3
   ps = CALLOC_STRUCT(pipe_surface);
   if (ps) {
      pipe_reference_init(&ps->reference, 1);
      pipe_resource_reference(&ps->texture, pt);
      ps->context = pipe;
      ps->format = surf_tmpl->format;
      if (pt->target != PIPE_BUFFER) {
         assert(surf_tmpl->u.tex.level <= pt->last_level);
         ps->width = u_minify(pt->width0, surf_tmpl->u.tex.level);
         ps->height = u_minify(pt->height0, surf_tmpl->u.tex.level);
         ps->u.tex.level = surf_tmpl->u.tex.level;
         ps->u.tex.first_layer = surf_tmpl->u.tex.first_layer;
         ps->u.tex.last_layer = surf_tmpl->u.tex.last_layer;
      }
      else {
         /* setting width as number of elements should get us correct renderbuffer width */
         ps->width = surf_tmpl->u.buf.last_element - surf_tmpl->u.buf.first_element + 1;
         ps->height = pt->height0;
         ps->u.buf.first_element = surf_tmpl->u.buf.first_element;
         ps->u.buf.last_element = surf_tmpl->u.buf.last_element;
         assert(ps->u.buf.first_element <= ps->u.buf.last_element);
         assert(ps->u.buf.last_element < ps->width);
      }
   }
#endif

   return ps;
}

static void 
panfrost_surface_destroy(struct pipe_context *pipe,
                         struct pipe_surface *surf)
{
#ifdef HAVE_DRI3
   assert(surf->texture);
   pipe_resource_reference(&surf->texture, NULL);
   free(surf);
#endif
}

static void
panfrost_set_clip_state(struct pipe_context *pipe,
                        const struct pipe_clip_state *clip)
{
	//struct panfrost_context *panfrost = panfrost_context(pipe);
}

static void
panfrost_set_viewport_states(struct pipe_context *pipe,
                             unsigned start_slot,
                             unsigned num_viewports,
                             const struct pipe_viewport_state *viewports)
{
	struct panfrost_context *ctx = panfrost_context(pipe);

	assert(start_slot == 0);
	assert(num_viewports == 1);

	const struct pipe_viewport_state *vp = &viewports[0];
	printf("Scale: %f, %f, %f\n", vp->scale[0], vp->scale[1], vp->scale[2]);
	printf("Translate: %f, %f, %f\n", vp->translate[0], vp->translate[1], vp->translate[2]);

	ctx->viewports = viewports;

	/* TODO */
}

static void
panfrost_set_scissor_states(struct pipe_context *pipe,
                            unsigned start_slot,
                            unsigned num_scissors,
                            const struct pipe_scissor_state *scissors)
{
	struct panfrost_context *ctx = panfrost_context(pipe);

	assert(start_slot == 0);
	assert(num_scissors == 1);

	const struct pipe_scissor_state *ss = &scissors[0];
	printf("(%d, %d) -> (%d, %d)\n", ss->minx, ss->miny, ss->maxx, ss->maxy);

	ctx->scissors = scissors;

	/* TODO */
        trans_viewport(ctx, 0.0, 1.0, ss->minx, ss->miny, ss->maxx, ss->maxy);
	ctx->dirty |= PAN_DIRTY_VIEWPORT;
	/* TODO */
}

static void
panfrost_set_polygon_stipple(struct pipe_context *pipe,
                             const struct pipe_poly_stipple *stipple)
{
   //struct panfrost_context *panfrost = panfrost_context(pipe);
}

static void
panfrost_set_active_query_state(struct pipe_context *pipe,
                             boolean enable)
{
   //struct panfrost_context *panfrost = panfrost_context(pipe);
}



static void
panfrost_destroy(struct pipe_context *pipe)
{
	struct panfrost_context *panfrost = panfrost_context(pipe);

#ifdef HAVE_DRI3
	if (panfrost->blitter)
		util_blitter_destroy(panfrost->blitter);
#endif
}

static void
panfrost_tile_texture(struct panfrost_context *ctx, struct panfrost_resource *rsrc, int level)
{
	int width = rsrc->base.width0 >> level;
	int height = rsrc->base.height0 >> level;

	/* Estimate swizzled bitmap size. Slight overestimates are fine.
	 * Underestimates will result in memory corruption or worse. */

	int swizzled_sz = trans_swizzled_size(width, height, rsrc->bytes_per_pixel);

	/* Allocate the transfer given that known size but do not copy */

	uint8_t *swizzled = panfrost_allocate_transfer(&ctx->textures, swizzled_sz, &rsrc->gpu[level]);

	/* Run actual texture swizzle, writing directly to the mapped GPU chunk
	 * we allocated */

	trans_texture_swizzle(width, height, rsrc->bytes_per_pixel, rsrc->stride, rsrc->cpu[level], swizzled);
}

static void
panfrost_transfer_unmap(struct pipe_context *pctx,
                       struct pipe_transfer *transfer)
{
	struct panfrost_context *ctx = panfrost_context(pctx);

	if (transfer->usage & PIPE_TRANSFER_WRITE) {
		if (transfer->resource->target == PIPE_TEXTURE_2D) {
			/* Gallium thinks writeback happens here; instead, this is our cue to tile */
			panfrost_tile_texture(ctx, (struct panfrost_resource *) transfer->resource, transfer->level);
		}
	}

#ifdef HAVE_DRI3
	/* Derefence the resource */
        pipe_resource_reference(&transfer->resource, NULL);
#endif

	/* Transfer itself is CALLOCed at the moment */
	free(transfer);
}

static void panfrost_blit(struct pipe_context *pipe,
                    const struct pipe_blit_info *info)
{
   struct panfrost_context *pan = panfrost_context(pipe);

   /* STUB */
   printf("Skipping blit XXX\n");
   return;
#if 0

#if 0
   if (info->render_condition_enable && !panfrost_check_render_cond(pan))
      return;
#endif

   if (info->src.resource->nr_samples > 1 &&
       info->dst.resource->nr_samples <= 1 &&
       !util_format_is_depth_or_stencil(info->src.resource->format) &&
       !util_format_is_pure_integer(info->src.resource->format)) {
      printf("panfrost: color resolve unimplemented\n");
      return;
   }

   if (util_try_blit_via_copy_region(pipe, info)) {
      return; /* done */
   }

   if (!util_blitter_is_blit_supported(pan->blitter, info)) {
      debug_printf("panfrost: blit unsupported %s -> %s\n",
                   util_format_short_name(info->src.resource->format),
                   util_format_short_name(info->dst.resource->format));
      return;
   }

   /* XXX turn off occlusion and streamout queries */

   util_blitter_save_vertex_buffer_slot(pan->blitter, pan->vertex_buffers);
   util_blitter_save_vertex_elements(pan->blitter, pan->vertex->pipe);
   util_blitter_save_vertex_shader(pan->blitter, pan->vs);
   //util_blitter_save_geometry_shader(pan->blitter, pan->gs);
   //util_blitter_save_so_targets(pan->blitter, pan->num_so_targets,
   //                  (struct pipe_stream_output_target**)pan->so_targets);
   util_blitter_save_rasterizer(pan->blitter, pan->rasterizer);
   util_blitter_save_viewport(pan->blitter, pan->viewports);
   util_blitter_save_scissor(pan->blitter, pan->scissors);
   util_blitter_save_fragment_shader(pan->blitter, pan->fs);
   util_blitter_save_blend(pan->blitter, pan->blend);
   util_blitter_save_depth_stencil_alpha(pan->blitter, pan->depth_stencil);
   util_blitter_save_stencil_ref(pan->blitter, &pan->stencil_ref);
   /*util_blitter_save_sample_mask(pan->blitter, pan->sample_mask);*/
   util_blitter_save_framebuffer(pan->blitter, &pan->pipe_framebuffer);
   util_blitter_save_fragment_sampler_states(pan->blitter,
                     pan->sampler_count[PIPE_SHADER_FRAGMENT],
                     (void**)pan->samplers[PIPE_SHADER_FRAGMENT]);
   util_blitter_save_fragment_sampler_views(pan->blitter,
                     pan->sampler_view_count[PIPE_SHADER_FRAGMENT],
                     pan->sampler_views[PIPE_SHADER_FRAGMENT]);
#if 0
   util_blitter_save_render_condition(pan->blitter, pan->render_cond_query,
                                      pan->render_cond_cond, pan->render_cond_mode);
#endif

   util_blitter_blit(pan->blitter, info);
#endif
}

static void
trans_allocate_slab(struct panfrost_context *ctx,
		    struct panfrost_memory *mem,
		    size_t pages,
		    bool mapped,
		    bool same_va,
		    int extra_flags,
		    int commit_count,
		    int extent)
{
	int flags = MALI_MEM_PROT_CPU_RD | MALI_MEM_PROT_CPU_WR | MALI_MEM_PROT_GPU_RD | MALI_MEM_PROT_GPU_WR;

	flags |= extra_flags;

	/* w+x are mutually exclusive */
	if (extra_flags & MALI_MEM_PROT_GPU_EX)
		flags &= ~MALI_MEM_PROT_GPU_WR;

	if (same_va)
		flags |= MALI_MEM_SAME_VA;

	if (commit_count || extent)
		pandev_general_allocate(ctx->fd, pages, commit_count, extent, flags, &mem->gpu);
	else
		pandev_standard_allocate(ctx->fd, pages, flags, &mem->gpu);

	mem->size = pages * 4096;

	/* mmap for 64-bit, mmap64 for 32-bit. ironic, I know */
	if (mapped)
		mem->cpu = mmap(NULL, mem->size, 3, 1, ctx->fd, mem->gpu);

	mem->stack_bottom = 0;
}

/* Setups a framebuffer, either by itself (with the independent slow-fb
 * interface) or from an existing pointer (for shared memory, from the winsys)
 * */

void
trans_setup_framebuffer(struct panfrost_context *ctx, uint32_t *addr, int width, int height)
{
	ctx->width = width;
	ctx->height = height;
	ctx->bytes_per_pixel = 4; /* RGB32 */
	ctx->has_alpha_channel = false;
	ctx->flip_vertical = true; /* OpenGL */

	/* drisw rounds the stride */
	int rw = 16.0 * (int) ceil((float) ctx->width / 16.0);

	ctx->stride = rw * ctx->bytes_per_pixel;

	size_t framebuffer_sz = ctx->stride * ctx->height;

	/* Unclear why framebuffers are offset like this */
	int offset = (ctx->stride - 256) * ctx->bytes_per_pixel;

#if 0
	if (addr) {
		ctx->framebuffer.cpu = (uint8_t *) addr;
	} else {
		posix_memalign((void **) &ctx->framebuffer.cpu, CACHE_LINE_SIZE, framebuffer_sz + offset);
		struct slowfb_info info = slowfb_init((uint8_t*) (ctx->framebuffer.cpu + offset), rw, ctx->height);

		/* May not be the same as our original alloc if we're using XShm, etc */
		ctx->framebuffer.cpu = info.framebuffer;
		ctx->stride = info.stride;
	}
#endif

#if 0
	struct mali_mem_import_user_buffer framebuffer_handle = { .ptr = (uint64_t) (uintptr_t) ctx->framebuffer.cpu, .length = framebuffer_sz + offset };

	struct mali_ioctl_mem_import framebuffer_import = {
		.phandle = (uint64_t) (uintptr_t) &framebuffer_handle,
		.type = MALI_MEM_IMPORT_TYPE_USER_BUFFER,
		.flags = MALI_MEM_PROT_CPU_RD | MALI_MEM_PROT_CPU_WR | MALI_MEM_PROT_GPU_RD | MALI_MEM_PROT_GPU_WR,
	};

	pandev_ioctl(ctx->fd, MALI_IOCTL_MEM_IMPORT, &framebuffer_import);
#endif
	/* TODO: Reenable imports when we understand the new kernel API */

	trans_allocate_slab(ctx, &ctx->framebuffer, 2*(ctx->stride * ctx->height) / 4096, true, true, 0, 0, 0);
	trans_allocate_slab(ctx, &ctx->depth_stencil_buffer, 2*(ctx->stride * ctx->height) / 4096, true, true, 0, 0, 0);
	struct slowfb_info info = slowfb_init((uint8_t*) (ctx->framebuffer.cpu), rw, ctx->height);
	ctx->stride = info.stride;

	//ctx->framebuffer.gpu = framebuffer_import.gpu_va;
	//ctx->framebuffer.size = ctx->stride * ctx->height;
}

static void
trans_setup_hardware(struct panfrost_context *ctx)
{
	ctx->fd = pandev_open();

	trans_allocate_slab(ctx, &ctx->cmdstream, 8*64*2, true, true, 0, 0, 0);
	trans_allocate_slab(ctx, &ctx->cmdstream_persistent, 8*64*4, true, true, 0, 0, 0);
	trans_allocate_slab(ctx, &ctx->textures, 4*64*64, true, true, 0, 0, 0);
	trans_allocate_slab(ctx, &ctx->scratchpad, 32, true, true, 0, 0, 0);
	trans_allocate_slab(ctx, &ctx->varying_mem, 32, false, true, 0, 0, 0);
	trans_allocate_slab(ctx, &ctx->shaders, 4096, true, false, MALI_MEM_PROT_GPU_EX, 0, 0);
	trans_allocate_slab(ctx, &ctx->tiler_heap, 32768, false, false, 0, 0, 0);
	trans_allocate_slab(ctx, &ctx->misc_0, 64, false, false, 0, 0, 0);

#ifdef USE_SLOWFB
	trans_setup_framebuffer(ctx, NULL, 1920, 1080);
#endif
}

#ifdef HAVE_DRI3
static const struct u_transfer_vtbl transfer_vtbl = {
        .resource_create          = panfrost_resource_create,
        .resource_destroy         = panfrost_resource_destroy,
        .transfer_map             = panfrost_transfer_map,
        .transfer_unmap           = panfrost_transfer_unmap,
        .transfer_flush_region    = u_default_transfer_flush_region,
        //.get_internal_format      = panfrost_resource_get_internal_format,
        //.set_stencil              = panfrost_resource_set_stencil,
        //.get_stencil              = panfrost_resource_get_stencil,
};
#endif

/* New context creation, which also does hardware initialisation since I don't
 * know the better way to structure this :smirk: */

struct pipe_context *
panfrost_create_context(struct pipe_screen *screen, void *priv, unsigned flags)
{
	screen->resource_create = panfrost_resource_create;
	screen->resource_destroy = panfrost_resource_destroy;
	screen->resource_create_front = panfrost_resource_create_front;
#ifdef HAVE_DRI3
	screen->transfer_helper = u_transfer_helper_create(&transfer_vtbl, true, true, true);
#endif

	struct panfrost_context *ctx = CALLOC_STRUCT(panfrost_context);
	memset(ctx, 0, sizeof(*ctx));
	struct pipe_context *gallium = (struct pipe_context *) ctx;

	gallium->screen = screen;

	gallium->destroy = panfrost_destroy;

	gallium->set_framebuffer_state = panfrost_set_framebuffer_state;

	gallium->transfer_map = panfrost_transfer_map;
	gallium->transfer_unmap = panfrost_transfer_unmap;

#ifdef HAVE_DRI3
	gallium->transfer_flush_region = u_transfer_helper_transfer_flush_region;
	gallium->buffer_subdata = u_default_buffer_subdata;
	gallium->texture_subdata = u_default_texture_subdata;
	gallium->clear_texture = util_clear_texture;
#endif

	gallium->create_surface = panfrost_create_surface;
	gallium->surface_destroy = panfrost_surface_destroy;

	gallium->flush = panfrost_flush;
	gallium->clear = panfrost_clear;
	gallium->draw_vbo = panfrost_draw_vbo;

	gallium->set_vertex_buffers = panfrost_set_vertex_buffers;
	gallium->set_constant_buffer = panfrost_set_constant_buffer;

	gallium->set_stencil_ref = panfrost_set_stencil_ref;

	gallium->create_sampler_view = panfrost_create_sampler_view;
	gallium->set_sampler_views = panfrost_set_sampler_views;
	gallium->sampler_view_destroy = panfrost_sampler_view_destroy;

	gallium->create_rasterizer_state = panfrost_create_rasterizer_state;
	gallium->bind_rasterizer_state = panfrost_bind_rasterizer_state;
	gallium->delete_rasterizer_state = panfrost_generic_cso_delete;

	gallium->create_vertex_elements_state = panfrost_create_vertex_elements_state;
	gallium->bind_vertex_elements_state = panfrost_bind_vertex_elements_state;
	gallium->delete_vertex_elements_state = panfrost_generic_cso_delete;

	gallium->create_fs_state = panfrost_create_shader_state;
	gallium->delete_fs_state = panfrost_delete_shader_state;
	gallium->bind_fs_state = panfrost_bind_fs_state;

	gallium->create_vs_state = panfrost_create_shader_state;
	gallium->delete_vs_state = panfrost_delete_shader_state;
	gallium->bind_vs_state = panfrost_bind_vs_state;

	gallium->create_sampler_state = panfrost_create_sampler_state;
	gallium->delete_sampler_state = panfrost_generic_cso_delete;
	gallium->bind_sampler_states = panfrost_bind_sampler_states;

	gallium->create_blend_state = panfrost_create_blend_state;
	gallium->bind_blend_state   = panfrost_bind_blend_state;
	gallium->delete_blend_state = panfrost_delete_blend_state;

	gallium->set_blend_color = panfrost_set_blend_color;

	gallium->create_depth_stencil_alpha_state = panfrost_create_depth_stencil_state;
	gallium->bind_depth_stencil_alpha_state   = panfrost_bind_depth_stencil_state;
	gallium->delete_depth_stencil_alpha_state = panfrost_delete_depth_stencil_state;

	gallium->set_sample_mask = panfrost_set_sample_mask;

	gallium->set_clip_state = panfrost_set_clip_state;
	gallium->set_viewport_states = panfrost_set_viewport_states;
	gallium->set_scissor_states = panfrost_set_scissor_states;
	gallium->set_polygon_stipple = panfrost_set_polygon_stipple;
	gallium->set_active_query_state = panfrost_set_active_query_state;

	gallium->blit = panfrost_blit;

#ifdef HAVE_DRI3
	/* XXX: leaks */
	gallium->stream_uploader = u_upload_create_default(gallium);
	gallium->const_uploader = gallium->stream_uploader;
	assert(gallium->stream_uploader);

	ctx->primconvert = util_primconvert_create(gallium,
			(1 << PIPE_PRIM_QUADS) - 1);
	assert(ctx->primconvert);

	ctx->blitter = util_blitter_create(gallium);
	assert(ctx->blitter);
#endif

	/* Prepare for render! */
	trans_setup_hardware(ctx);
	
	/* TODO: XXX */
	ctx->vt_framebuffer = trans_emit_fbd(ctx);
	
	trans_emit_vertex_payload(ctx);
	trans_emit_tiler_payload(ctx);
	trans_invalidate_frame(ctx);
	trans_new_frag_framebuffer(ctx);
	trans_default_shader_backend(ctx);
	trans_generate_space_filler_indices();

	return gallium;
}
