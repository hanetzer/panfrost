#define _LARGEFILE64_SOURCE 1
#define CACHE_LINE_SIZE 1024 /* TODO */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include "pan_nondrm.h"
#include "pan_context.h"

int main(int argc, const char **argv)
{
	struct pipe_screen _screen;
	struct pipe_screen *screen = &_screen;
	struct pipe_context *gallium = panfrost_create_context(screen, NULL, 0);

	struct pipe_shader_state vs_cso, fs_cso;

	gallium->bind_fs_state(gallium, gallium->create_fs_state(gallium, &fs_cso));
	gallium->bind_vs_state(gallium, gallium->create_vs_state(gallium, &vs_cso));

	float attributes_data_1_0[] = {
            -0.95000f, 0.950000f, 0.000000f, 0.0f,
            0.950000f, 0.950000f, 0.000000f, 1.0f,
            -0.95000f, -0.950000f, 1.000000f, 1.0f,
            0.950000f, -0.950000f, 1.000000f, 0.0f,

            -0.50000f, 0.500000f, 0.000000f, 0.0f,
            0.500000f, 0.500000f, 0.000000f, 1.0f,
            -0.50000f, -0.500000f, 1.000000f, 1.0f,
            0.500000f, -0.500000f, 1.000000f, 0.0f,

        };

	struct pipe_resource templ = {
		.width0 = sizeof(attributes_data_1_0)
	};

	struct pipe_box box = {};

	struct pipe_resource *rsrc = screen->resource_create(screen, &templ);

	struct pipe_transfer *transfer;
	uint8_t *attrib_trans = gallium->transfer_map(gallium, rsrc, 0, 0, &box, &transfer);

	const struct pipe_vertex_element attrs[] = {
		{
			/* TODO */
		},
		{}
	};

	gallium->bind_vertex_elements_state(gallium, gallium->create_vertex_elements_state(gallium, 2, attrs));

	const struct pipe_vertex_buffer buffs[] ={
	       	{
			.buffer = { .resource = rsrc },
			.stride = 4*sizeof(float)
		},
	       	{
			.buffer = { .resource = rsrc },
			.stride = 4*sizeof(float)
		},
	};

	gallium->set_vertex_buffers(gallium, 0, 2, buffs);

#if 0
        float uniforms[] = {
		0.8, 0.0, 0.4, 1.0,
		0.2, 0.9, 0.6, 1.0,
		0.1, 0.1, 0.2, 1.0,
		0.3, 0.7, 0.1, 1.0,
        };
	templ.width0 = sizeof(uniforms);

	struct pipe_transfer *transfer2;
	struct pipe_resource *cbuf = screen->resource_create(screen, &templ);
	memcpy(gallium->transfer_map(gallium, cbuf, 0, 0, &box, &transfer2), uniforms, sizeof(uniforms));

	struct pipe_constant_buffer consts = {
		.buffer = cbuf,
		.buffer_size = sizeof(uniforms)
	};
	
	//gallium->set_constant_buffer(gallium, PIPE_SHADER_VERTEX, 0, &consts);
#endif

	uint32_t indices[] = {
		0, 1, 2,
		1, 0, 3,
		2, 3, 0
	};
	templ.width0 = sizeof(indices);

	struct pipe_transfer *transfer3;
	struct pipe_resource *ibuf = screen->resource_create(screen, &templ);
	memcpy(gallium->transfer_map(gallium, ibuf, 0, 0, &box, &transfer3), indices, sizeof(indices));


	memcpy(attrib_trans,
		attributes_data_1_0,
		sizeof(attributes_data_1_0));

	const struct pipe_rasterizer_state state = {
		.line_width = 10.0f,
		.front_ccw = true,
		.multisample = true
	};

	gallium->bind_rasterizer_state(gallium,
			gallium->create_rasterizer_state(gallium, &state));

	struct pipe_sampler_state sampler_state = {
		.wrap_s = PIPE_TEX_WRAP_REPEAT,
		.wrap_t = PIPE_TEX_WRAP_REPEAT,
		.wrap_r = PIPE_TEX_WRAP_REPEAT,
		.compare_func = PIPE_FUNC_LEQUAL,
		.min_img_filter = PIPE_TEX_FILTER_LINEAR,
		.mag_img_filter = PIPE_TEX_FILTER_LINEAR,
		.border_color = { .f = { 0.0, 0.0, 0.0, 0.0 } },
	};

	struct pipe_sampler_view _sampler_view = {
		.target = PIPE_TEXTURE_2D,
		.swizzle_r = PIPE_SWIZZLE_X,
		.swizzle_g = PIPE_SWIZZLE_Y,
		.swizzle_b = PIPE_SWIZZLE_Z,
		.swizzle_a = PIPE_SWIZZLE_W,
	};

	uint8_t bitmap_data[568 * 770 * 4];

	FILE *fp = fopen("/home/alyssa/img.bin", "rb");
	fread(bitmap_data, 1, sizeof(bitmap_data), fp);

	templ.width0 = 568;
	templ.height0 = 770;
	templ.depth0 = 1;
	templ.target = PIPE_TEXTURE_2D;

	struct pipe_transfer *transfer4;
	struct pipe_resource *tbuf = screen->resource_create(screen, &templ);
	memcpy(gallium->transfer_map(gallium, tbuf, 0, PIPE_TRANSFER_WRITE, &box, &transfer4), bitmap_data, sizeof(bitmap_data));
	gallium->transfer_unmap(gallium, transfer4);

	void *tex = gallium->create_sampler_view(gallium, tbuf, &_sampler_view);
	void *samp = gallium->create_sampler_state(gallium, &sampler_state);

	void *texs[] = { tex, tex };
	void *samps[] = { samp, samp };

	gallium->bind_sampler_states(gallium,
			PIPE_SHADER_FRAGMENT,
			0, 2,
			samps);

	gallium->set_sampler_views(gallium,
			PIPE_SHADER_FRAGMENT,
			0, 2,
			(struct pipe_sampler_view **) texs);

    for (int i = 0; i < 1000; ++i) {
#if 0
	const struct pipe_rasterizer_state stat = {
		.line_width = 10.0f,
		.front_ccw = false,
		.multisample = true
	};

	gallium->bind_rasterizer_state(gallium,
			gallium->create_rasterizer_state(gallium, &stat));
#endif

	attributes_data_1_0[0] = sin(((float) i ) * 0.03f);

	memcpy(attrib_trans,
		attributes_data_1_0,
		sizeof(attributes_data_1_0));

        union pipe_color_union u = { .f = { 0.1, 0.0, 0.1, 1.0 } };
        gallium->clear(gallium, PIPE_CLEAR_COLOR | PIPE_CLEAR_DEPTH | PIPE_CLEAR_STENCIL, &u, 0.0, 0.0);

	struct pipe_draw_info info = {
		.start = 0,
		.count = 9,
		.index_size = 4,
		.index = { .resource = ibuf },
		.mode = PIPE_PRIM_TRIANGLE_STRIP
	};

	gallium->draw_vbo(gallium, &info);

	/*
	attributes_data_1_0[0] = 0;

	memcpy(attrib_trans,
		attributes_data_1_0,
		sizeof(attributes_data_1_0));
		*/

#if 0
	const struct pipe_rasterizer_state staate = {
		.line_width = 1.0f,
		.front_ccw = false,
		.multisample = true
	};

	info.mode = PIPE_PRIM_LINE_STRIP;

	gallium->bind_rasterizer_state(gallium,
			gallium->create_rasterizer_state(gallium, &staate));


	gallium->draw_vbo(gallium, &info);

#endif

	gallium->flush(gallium, NULL, PIPE_FLUSH_END_OF_FRAME);
    }

    //gallium->transfer_unmap(gallium, transfer);

    return 0;
}
