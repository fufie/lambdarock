#ifndef LBTOOLS_H
#define LBTOOLS_H

#ifndef WIN32
#include "autoconf.h"
#endif

#include <stdio.h>

extern void lbui_format(FILE *ofile, int priority, const char *fmt, ...);
#ifdef DEBUG
extern void DBGPUT(const char *fmt, ...);
#else
#define DBGPUT if(1){}else printf
#endif

extern void ERRORMSG(const char *fmt, ...);
#define INFOMSG ERRORMSG
#define DEBUGPUT DBGPUT

#endif /* lbtools.h */
