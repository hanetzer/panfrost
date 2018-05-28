#ifndef SP_PUBLIC_H
#define SP_PUBLIC_H

#ifdef __cplusplus
extern "C" {
#endif

struct pipe_screen;
struct renderonly;

struct pipe_screen *
panfrost_create_screen(int fd, struct renderonly *ro);

#ifdef __cplusplus
}
#endif

#endif
