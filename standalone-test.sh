midgard_compiler ~/shaders/test.vert ~/shaders/test.frag
gcc -Iinclude -I/home/guest/panloader/trans trans-builder.c pandev.c trans-test.c allocate.c assemble.c limare-swizzle.c slow-framebuffer.c -lX11 -lm -o standalone-test
./standalone-test
