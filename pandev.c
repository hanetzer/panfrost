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
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <string.h>
#include <sys/mman.h>
#include <assert.h>

#include <panloader-util.h>
#include <mali-ioctl.h>
#include "pandev.h"
#include <mali-job.h>

#include <sys/user.h>

/* From the kernel module */

#define MALI_MEM_MAP_TRACKING_HANDLE (3ull << 12)
#define MALI_CONTEXT_CREATE_FLAG_NONE 0

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

/**
 * Low-level open call, used by the main pandev_open
 */

int
pandev_raw_open()
{
	return open("/dev/mali0", O_RDWR | O_NONBLOCK | O_CLOEXEC);
}

/* The Memmap Tracking Handle needs to mapped for vendor kernel; it serves no
 * real purpose */

u8*
pandev_map_mtp(int fd)
{
	return mmap(NULL, 4096, PROT_NONE, MAP_SHARED, fd, MALI_MEM_MAP_TRACKING_HANDLE);
}

/**
 * Open the device file for communicating with the vendor kernelspace driver,
 */

int
pandev_open()
{
	int fd = pandev_raw_open();

	struct mali_ioctl_get_version version = {
		.major = 10,
		.minor = 4
	};

	struct mali_ioctl_set_flags args = {
		.create_flags = MALI_CONTEXT_CREATE_FLAG_NONE
	};

	assert(pandev_ioctl(fd, MALI_IOCTL_GET_VERSION, &version) == 0);
	assert(pandev_map_mtp(fd) != MAP_FAILED);
	assert(pandev_ioctl(fd, MALI_IOCTL_SET_FLAGS, &args) == 0);

	return fd;
}
