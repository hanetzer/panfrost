/*
 * © Copyright 2017 The Panfrost Community
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
#include <fcntl.h>
#include <sys/ioctl.h>
#include <string.h>
#include <sys/mman.h>
#include <assert.h>

#include <mali-ioctl.h>
#include "pan_nondrm.h"

/* From the kernel module */

#define MALI_MEM_MAP_TRACKING_HANDLE (3ull << 12)

int
pandev_ioctl(int fd, unsigned long request, void *args)
{
	union mali_ioctl_header *h = args;
	h->id = ((_IOC_TYPE(request) & 0xF) << 8) | _IOC_NR(request);
	assert(ioctl(fd, request, args) == 0);
}

int
pandev_general_allocate(int fd, int va_pages, int commit_pages, int extent, int flags, u64 *out)
{
	struct mali_ioctl_mem_alloc args = {
		.va_pages = va_pages,
		.commit_pages = commit_pages,
		.extent = extent,
		.flags = flags
	};

	assert(pandev_ioctl(fd, MALI_IOCTL_MEM_ALLOC, &args) == 0);

	*out = args.gpu_va;
}

int
pandev_standard_allocate(int fd, int va_pages, int flags, u64 *out)
{
	return pandev_general_allocate(fd, va_pages, va_pages, 0, flags, out);
}

int
pandev_open()
{
	struct mali_ioctl_get_version version = { .major = 10, .minor = 4 };
	struct mali_ioctl_set_flags args = {};

	int fd = open("/dev/mali0", O_RDWR | O_NONBLOCK | O_CLOEXEC);
	assert(fd);
	printf("%d\n", fd);

#if 0
	assert(pandev_ioctl(fd, MALI_IOCTL_GET_VERSION, &version) == 0);
#endif
	assert(mmap(NULL, 4096, PROT_NONE, MAP_SHARED, fd, MALI_MEM_MAP_TRACKING_HANDLE) != MAP_FAILED);
	assert(pandev_ioctl(fd, MALI_IOCTL_SET_FLAGS, &args) == 0);

	return fd;
}
