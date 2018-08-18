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

#ifndef __PANDEV_H__
#define __PANDEV_H__

#include <unistd.h>
#include <sys/mman.h>
#include <stdbool.h>
#include <mali-ioctl.h>
#include <mali-job.h>
#include <linux/ioctl.h>
#include "pan_slowfb.h"

int pandev_open(void);

/* Calls used while replaying */
int pandev_raw_open(void);
u8* pandev_map_mtp(int fd);
int pandev_ioctl(int fd, unsigned long request, void *args);

int pandev_standard_allocate(int fd, int va_pages, int flags, u64 *out);
int pandev_general_allocate(int fd, int va_pages, int commit_pages, int extent, int flags, u64 *out);

void pandev_shader_assemble(uint32_t *dst, const char *src, int type);
void* pandev_shader_compile(uint32_t *dst, const char *src, int type);

struct panfrost_context;
struct panfrost_shader_state;
void
panfrost_shader_compile(struct panfrost_context *ctx, struct mali_shader_meta *meta, const char *src, int type, struct panfrost_shader_state *state);

struct panfrost_memory {
	uint8_t* cpu;
	mali_ptr gpu;
	int stack_bottom;
	size_t size;
};

/* Functions for replay */
mali_ptr pandev_upload(int cheating_offset, int *stack_bottom, mali_ptr base, void *base_map, const void *data, size_t sz, bool no_pad);
mali_ptr pandev_upload_sequential(mali_ptr base, void *base_map, const void *data, size_t sz);

/* Functions for the actual Galliumish driver */
mali_ptr panfrost_upload(struct panfrost_memory *mem, const void *data, size_t sz, bool no_pad);
mali_ptr panfrost_upload_sequential(struct panfrost_memory *mem, const void *data, size_t sz);

void *
panfrost_allocate_transfer(struct panfrost_memory *mem, size_t sz, mali_ptr *gpu);

static inline mali_ptr
panfrost_reserve(struct panfrost_memory *mem, size_t sz)
{
	mem->stack_bottom += sz;
	return mem->gpu + (mem->stack_bottom - sz);
}

#include <math.h>
#define inff INFINITY

#define R(...) #__VA_ARGS__
#define ALIGN(x, y) (((x) + ((y) - 1)) & ~((y) - 1))

#endif /* __PANDEV_H__ */
