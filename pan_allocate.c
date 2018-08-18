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

/* TODO: What does this actually have to be? */
#define ALIGNMENT 128

// TODO: An actual allocator, perhaps
// TODO: Multiple stacks for multiple bases?

int hack_stack_bottom = 4096; /* Don't interfere with constant offsets */
int last_offset = 0;

static inline int
pandev_allocate_offset(int *stack, size_t sz)
{
	/* First, align the stack bottom to something nice; it's not critical
	 * at this point if we waste a little space to do so. */

	int excess = *stack & (ALIGNMENT - 1);

	/* Add the secret of my */
	if (excess)
		*stack += ALIGNMENT - excess;

	/* Finally, use the new bottom for the allocation and move down the
	 * stack */

	int ret = *stack;
	*stack += sz;
	return ret;
}

inline mali_ptr
pandev_upload(int cheating_offset, int *stack_bottom, mali_ptr base, void *base_map, const void *data, size_t sz, bool no_pad)
{
	int offset;

	/* This is probably wrong */
	no_pad = true;

	/* We're not positive about the sizes of all objects, but we don't want
	 * them to crash against each other either. Let the caller disable
	 * padding if they so choose, though. */

	size_t padded_size = no_pad ? sz : sz * 2;

	/* If no specific bottom is specified, use a global one... don't do
	 * this in production, kids */

	if (!stack_bottom)
		stack_bottom = &hack_stack_bottom;

	/* Allocate space for the new GPU object, if required */

	if (cheating_offset == -1) {
		offset = pandev_allocate_offset(stack_bottom, padded_size);
	} else {
		offset = cheating_offset;
	}

	/* Save last offset for sequential uploads (job descriptors) */
	last_offset = offset + padded_size;

	/* Upload it */
	memcpy((uint8_t*) base_map + offset, data, sz);

	/* Return the GPU address */
	return base + offset;
}

/* Upload immediately after the last allocation */

mali_ptr
pandev_upload_sequential(mali_ptr base, void *base_map, const void *data, size_t sz) {
	return pandev_upload(last_offset, NULL, base, base_map, data, sz, /* false */ true);
}

/* Simplified APIs for the real driver, rather than replays */

mali_ptr
panfrost_upload(struct panfrost_memory *mem, const void *data, size_t sz, bool no_pad)
{
	return pandev_upload(-1, &mem->stack_bottom, mem->gpu, mem->cpu, data, sz, no_pad);
}

mali_ptr
panfrost_upload_sequential(struct panfrost_memory *mem, const void *data, size_t sz)
{
	return pandev_upload_sequential(mem->gpu, mem->cpu, data, sz);
}

/* Simplified interface to allocate a chunk without any upload, to allow
 * zero-copy uploads. This is particularly useful when the copy would happen
 * anyway, for instance with texture swizzling. */

void *
panfrost_allocate_transfer(struct panfrost_memory *mem, size_t sz, mali_ptr *gpu)
{
	int offset = pandev_allocate_offset(&mem->stack_bottom, sz);
	
	*gpu = mem->gpu + offset;
	return mem->cpu + offset;
}
