# Some paths
prefix=/usr/local
exec_prefix=${prefix}
bindir=${exec_prefix}/bin

CC = gcc
SDL_CFLAGS = -I/usr/local/include/SDL -D_GNU_SOURCE=1 -D_THREAD_SAFE
SDL_LIBS = -L/usr/local/lib -lSDLmain -lSDL -Wl,-framework,Cocoa

LBUI_CFLAGS = -I/usr/local/include/SDL -D_GNU_SOURCE=1 -D_THREAD_SAFE  -DUSE_EXTERNAL_SOUND -DEXTDAEMON_PATH="\"${bindir}"\" 
LBUI_OBJS =  ext-sound.o main-gcu.o gcu-extra.o sdl-extra.o main-sdl.o
LBUI_LIBS = -L/usr/local/lib -lSDLmain -lSDL -Wl,-framework,Cocoa -lncurses  -lSDL_image

LBE_OBJS = collected.o sound.o frame.o lisp_if.o tools.o
LBE_LIBS = 
LBE_CFLAGS =  -Wall -W -fPIC -fvisibility=default
LBE_LINKFLAGS = 

SHAREDFLAG = -dynamiclib

LBSND_LIBS = 
LBSND_SRC = server.c sound.c tools.c
LBSND_CFLAGS = 

MAKE_EXTSOUND=yes

DLLEXT=dylib
DLLCOPY=true

all: shared_objs lbsd dll_copy

clean:
	$(RM) *.o lbui.so dircall.so lbui.dll dircall.dll *.fasl *.a *~ lbsd

%.o : %.c
	${CC} ${LBE_CFLAGS} ${LBUI_CFLAGS} -c $<

shared_objs: ${LBE_OBJS} ${LBUI_OBJS} dummy_call.o
	${CC} ${LBE_LINKFLAGS} ${SHAREDFLAG} -o lbui.$(DLLEXT) ${LBE_OBJS} ${LBUI_OBJS} ${LBE_LIBS} ${LBUI_LIBS}

dll_copy:
	${DLLCOPY}

lbsd: ${LBSND_SRC}
	if test "${MAKE_EXTSOUND}" = "yes"; then \
	  ${CC} ${LBE_CFLAGS} ${LBSND_CFLAGS} -o lbsd ${LBSND_SRC} ${LBSND_LIBS}; \
	fi
