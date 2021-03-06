#ifndef LANGBAND_H
#define LANGBAND_H

/*
 * DESC: langband.h - langband-related defined/includes/etc.
 * Copyright (c) 2000-2002, 2009 - Stig Erik Sand�
 */

#ifndef WIN32
#include "autoconf.h"
#endif

#ifndef INTERFACE
#if defined(WIN_MAKEDLL)
#  define INTERFACE __declspec(dllexport)
#elif defined(WIN_USEDLL)
#  define INTERFACE __declspec(dllimport)
#else
#  define INTERFACE
#endif
#endif /* iface */

#include <stdio.h>
#include <ctype.h>
# include <stdlib.h>

#if defined(unix) || defined(DARWIN)
#include <unistd.h>
#endif

#include <time.h>
#include <string.h>
#include <stdarg.h>

/* Signed/Unsigned 16 bit value */
typedef signed short s16b;
typedef unsigned short u16b;

/* Signed/Unsigned 32 bit value */
#ifdef L64	/* 64 bit longs */
typedef signed int s32b;
typedef unsigned int u32b;
#else
typedef signed long s32b;
typedef unsigned long u32b;
#endif


/*** Pointers to Functions of special types (for various purposes) ***/

typedef enum {
    LISPSYS_CMUCL     = 0,
    LISPSYS_ACL       = 1,
    LISPSYS_CLISP     = 2,
    LISPSYS_LISPWORKS = 3,
    LISPSYS_SBCL      = 4,
    LISPSYS_CORMAN    = 5,
    LISPSYS_OPENMCL   = 6,
    LISPSYS_ECL       = 7,
    LISPSYS_BAD       = 20
} LISP_SYSTEMS;

typedef enum {
    UITYPE_X11        = 0,
    UITYPE_GCU        = 1,
    UITYPE_GTK        = 2,
    UITYPE_WIN        = 3,
    UITYPE_SDL        = 4,
    UITYPE_BAD        = 20
} UITYPES;


/*
 * Hack -- Define NULL
 */
#ifndef NULL
# ifdef __STDC__
#  define NULL ((void*)0)
# else
#  define NULL ((char*)0)
# endif /* __STDC__ */
#endif /* NULL */


/*** Color constants ***/


/*
 * Angband "attributes" (with symbols, and base (R,G,B) codes)
 *
 * The "(R,G,B)" codes are given in "fourths" of the "maximal" value,
 * and should "gamma corrected" on most (non-Macintosh) machines.
 */
#define TERM_DARK		0	/* 'd' */	/* 0,0,0 */
#define TERM_WHITE		1	/* 'w' */	/* 4,4,4 */
#define TERM_SLATE		2	/* 's' */	/* 2,2,2 */
#define TERM_ORANGE		3	/* 'o' */	/* 4,2,0 */
#define TERM_RED		4	/* 'r' */	/* 3,0,0 */
#define TERM_GREEN		5	/* 'g' */	/* 0,2,1 */
#define TERM_BLUE		6	/* 'b' */	/* 0,0,4 */
#define TERM_UMBER		7	/* 'u' */	/* 2,1,0 */
#define TERM_L_DARK		8	/* 'D' */	/* 1,1,1 */
#define TERM_L_WHITE	9	/* 'W' */	/* 3,3,3 */
#define TERM_VIOLET		10	/* 'v' */	/* 4,0,4 */
#define TERM_YELLOW		11	/* 'y' */	/* 4,4,0 */
#define TERM_L_RED		12	/* 'R' */	/* 4,0,0 */
#define TERM_L_GREEN	13	/* 'G' */	/* 0,4,0 */
#define TERM_L_BLUE		14	/* 'B' */	/* 0,4,4 */
#define TERM_L_UMBER	15	/* 'U' */	/* 3,2,1 */


#define NO_FLAGS 0x00
#define LANGBAND_GRAPHICS 0x01
#define LANGBAND_SOUND 0x02
//#define LANGBAND_1024 0x04
//#define LANGBAND_1280 0x08
#define LANGBAND_FULLSCREEN 0x10

#define LANGBAND_TEXT_END 0x80
#define LANGBAND_GFX_START 0x100

// Bitfield-values for what to blit (first two bits define)
#define LBBF_CLEAN 0
#define LBBF_CHAR  1
#define LBBF_GFX   2
#define LBBF_IDX   3

#define MINIMUM_WINDOW_WIDTH_SDL   800
#define MINIMUM_WINDOW_HEIGHT_SDL  600
#define MAXIMUM_WINDOW_WIDTH_SDL  4800
#define MAXIMUM_WINDOW_HEIGHT_SDL 4800

// Gervais values!
#define GFXTILE_WIDTH 32
#define GFXTILE_HEIGHT 32

extern const char *lbui_base_source_path;
extern const char *lbui_base_config_path;
extern const char *lbui_base_data_path;
//extern int lbui_use_sound;
/** will we access lisp through callbacks? */
extern int lbui_will_use_callback;
extern LISP_SYSTEMS lbui_current_lisp_system;

INTERFACE int lbui_paint_image(const char *fname, int x, int y);
INTERFACE int lbui_load_gfx_image(const char *fname, int idx, unsigned int transcolour);
INTERFACE int lbui_paint_gfx_image(const char *fname, const char *name, int x, int y);
INTERFACE int lbui_load_texture(int idx, const char*filename, int twid, int thgt, int alpha);
INTERFACE int lbui_init_tile_files(void);
INTERFACE int lbui_fill_area(int image_index, int tile_num, int x1, int y1, int x2, int y2);


/* remove later */
extern void lbui_print_image_list();

INTERFACE int lbui_load_scaled_image(const char *filename, int image_index, int width,
                                     int height, unsigned int transcolour);

INTERFACE int lbui_get_image_width(int idx);
INTERFACE int lbui_get_image_height(int idx);
INTERFACE int lbui_get_window_width();
INTERFACE int lbui_get_window_height();

INTERFACE int lbui_install_font_in_frame(int win_num, const char *font, int ptsize, int style);
// this will change
INTERFACE int lbui_set_idx_intvalue(int idx, int value);
INTERFACE int lbui_set_idx_stringvalue(int idx, const char*value);
INTERFACE const char *lbui_get_idx_value(int idx);

INTERFACE int lbui_current_ui();
INTERFACE int lbui_current_soundsystem();
//INTERFACE void print_coloured_token(int wantedTerm, int colour, int token, int row, int col);
//INTERFACE void print_coloured_stat(int wantedTerm, int colour, int stat, int row, int col);
//INTERFACE void print_coloured_number(int wantedTerm, int colour, long number, int padding, int row, int col);
INTERFACE int lbui_init_c_side(const char *sourcePath,
			       const char *confPath, const char *gfxPath,
			       int win_wid, int win_hgt, int extra_flags);
INTERFACE char *lbui_load_sound(int msg, char *fname);

int lbui_play_game_lisp();
void lbui_readjust_screen_lisp(int width, int height);
void lbui_mouse_clicked(int button, int x, int y);



INTERFACE void lbui_set_lisp_system(LISP_SYSTEMS type);

//#ifdef WIN32
//INTERFACE int setHINST(long val);
//#else
INTERFACE void lbui_set_lisp_callback(char *name, void *ptr);
//#endif



INTERFACE int lbui_transparent_blit(short win_num, short x, short y, unsigned int img, short flags);
INTERFACE int lbui_full_blit(short win_num, short x, short y, unsigned int img, short flags);
INTERFACE int lbui_clear_coords(short win_num, short x, short y, short w, short h);
INTERFACE int lbui_flush_coords(short win_num, short x, short y, short w, short h);
//INTERFACE int lbui_get_event(int option);
INTERFACE int lbui_cleanup_c_side(void);

INTERFACE int lbui_listen_for_event(int option);
INTERFACE unsigned lbui_get_internal_time();
INTERFACE int lbui_flip_framebuffer();
//INTERFACE int sdl_complex_blit(short win_num, short x, short y, unsigned int img, int flags);

#ifdef USE_SDL
INTERFACE int sdl_cleanup(void);
#endif


#ifdef USE_SDL

#ifdef WIN32
#include "SDL.h"
#else
#include <SDL/SDL.h>
#endif

#define FONT_TYPE_TTF 5
#define FONT_TYPE_HEX 6
#define MAX_FONT_CHAR_NUMBER 256

#define LBUI_FONT_STYLE_NORMAL 0
#define LBUI_FONT_STYLE_BOLD 0x01
#define LBUI_FONT_STYLE_ITALIC 0x02

struct FontData {
    char *fontname;
    int width;
    int height;
    
    void *theFont;
    int font_type;
    SDL_Surface *letters[MAX_FONT_CHAR_NUMBER];
};

struct graf_tiles {

    /*
     * to find a character:
     * x = character * w
     */
    SDL_Surface *face;
    /* 
     * font metrics.
     * Obviously, the font system is very minimalist.
     */
    Uint8 w;
    Uint8 h;

    Uint8 dw; /* width and height of font on destination surface */
    Uint8 dh;

    Uint8 precolorized;
};

struct tile_information {

    int array_size;
    SDL_Surface **tiles;
    char **tile_files;
    int *tile_columns;
    int num_tiles;

    int tile_width;
    int tile_height;
};


typedef struct FontData FontData; /* must be here to avoid fwd. ref. */
typedef struct graf_tiles graf_tiles; 
typedef struct tile_information TileInformation;

FontData *sdl_load_font(const char *fname, int ptsize, int style);

FontData *sdl_load_hex_font(const char *filename, int justmetrics);
//int load_HEX_font_sdl(FontData *fd, const char *filename, int justmetrics);
int sdl_strtoii(const char *str, Uint32 *w, Uint32 *h);

#ifdef ALLOW_TTF
int sdl_display_char(SDL_Surface *surface, SDL_Rect *dest, FontData *fdata, s16b attr, s16b ch);
FontData *sdl_load_ttf_font(const char *fname, int size, int style);
#endif /* use_ttf */


//int sdl_textureBackground(int term_num, const char *fname, int alpha);

int JAI_BlitSurfaceAlpha(SDL_Surface *src, SDL_Rect *srcrect,
			 SDL_Surface *dst, SDL_Rect *dstrect);

extern int sdl_load_gfx_image(const char *fname, int idx, unsigned int transcolour);
extern int sdl_paint_gfx_image(const char *fname, const char *name, int x, int y);
extern int sdl_load_scaled_image(const char *filename, int image_index, int width,
				 int height, unsigned int transcolour);
//extern int sdl_switch_terms(int bigterm);
//extern int sdl_term_gfx_use_p(term *t);
//extern int sdl_swap_map();
extern int sdl_recalculate_frame_placements(int arg);

extern int sdl_load_texture(int idx, const char *filename, int target_width, int target_height, int alpha);
extern int sdl_get_event(int option);
extern unsigned sdl_get_internal_time();
extern int sdl_flip_framebuffer();

extern int sdl_get_image_width(int idx);
extern int sdl_get_image_height(int idx);
extern int sdl_get_window_width();
extern int sdl_get_window_height();
extern int sdl_transparent_blit(short win_num, short x, short y, unsigned int img, short flags);
extern int sdl_full_blit(short win_num, short x, short y, unsigned int img, short flags);
extern int sdl_clear_coords(short win_num, short x, short y, short w, short h);
extern int sdl_flush_coords(short win_num, short x, short y, short w, short h);

extern SDL_Color sdl_colour_data[16];

#endif /* use sdl */

#endif /* langband_h */
