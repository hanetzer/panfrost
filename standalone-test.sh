midgard_compiler ~/shaders/test.vert ~/shaders/test.frag
#gcc -Iinclude -I/home/guest/panloader/trans pan_context.c pan_nondrm.c trans-test.c pan_allocate.c pan_assemble.c pan_swizzle.c pan_slowfb.c -O3 -lX11 -lXext -lm -g -o standalone-test
./standalone-test
