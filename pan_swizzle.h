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

#ifndef __TEXSWZ_H__
#define __TEXSWZ_H__

#include <stdint.h>
#include "pan_nondrm.h"

void
panfrost_generate_space_filler_indices(void);

void
panfrost_texture_swizzle(int width, int height, int bytes_per_pixel, int source_stride, 
		   const uint8_t *pixels,
		   uint8_t *ldest);

unsigned
panfrost_swizzled_size(int width, int height, int bytes_per_pixel);

#endif 
