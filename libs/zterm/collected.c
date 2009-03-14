/*
 * DESC: collected.h - a kitchen sink of all non-system specific C-code
 * Copyright (c) 2000-2002, 2009 - Stig Erik Sandoe
 */

#ifdef WIN32
#ifndef USE_SDL
#define USE_SDL
#endif
#else
#include "autoconf.h"
#endif

#include "lbwindows.h"

#ifdef USE_SOUND
#include "lbsound.h"
#endif
#include "lbtools.h"

#if defined(USE_SDL)
int lbui_init_sdl(int win_wid, int win_hgt, int flags);
#endif

extern int lbui_cleanup_callbacks();
extern int lbui_cleanup_frame_system();

/** defaults to false, since we don't need callbacks anymore */
int lbui_will_use_callback = 0;

/** set default illegal value. */
LISP_SYSTEMS lbui_current_lisp_system = LISPSYS_BAD;

/** the base path for source files */
const char *lbui_base_source_path = "./";
/** the base path for config files */
const char *lbui_base_config_path = "./config/";
/** the base path for gfx files */
const char *lbui_base_data_path = "./data/";

const int lbui_index_len = 256;
const int lbui_max_value_len = 40;
char **lbui_indexes = NULL;



int lbui_which_ui_used = -1;

int lbui_current_ui() { return lbui_which_ui_used; }

int
lbui_init_c_side(const char *sourcePath, const char *configPath,
                 const char *dataPath, int win_wid, int win_hgt,
                 int extra_flags) {
    
    int init_retval = -666;
    
    //#ifdef USE_SDL
#ifdef USE_SOUND    
    lbui_set_sound_status(extra_flags & LANGBAND_SOUND);
    //#else
    //    lbui_set_sound_status(0);
    //#endif
    
    if (lbui_get_sound_status()) {
#ifdef USE_SDL_MIXER
        lbui_set_soundsystem(SOUNDSYSTEM_SDL_MIXER);
#endif
        
#ifdef USE_OPENAL
        lbui_set_soundsystem(SOUNDSYSTEM_OPENAL);
#endif
#ifdef USE_EXTERNAL_SOUND
        lbui_set_soundsystem(SOUNDSYSTEM_EXTERNAL);
#endif
    }
#endif
    
    if (sourcePath && (strlen(sourcePath)>0)) {
        char *str = malloc(strlen(sourcePath) +2);
        strcpy(str, sourcePath);
        lbui_base_source_path = str;
    }
    
    if (configPath && (strlen(configPath)>0)) {
        char *str = malloc(strlen(configPath) +2);
        strcpy(str, configPath);
        lbui_base_config_path = str;
    }
    if (dataPath && (strlen(dataPath)>0)) {
        char *str = malloc(strlen(dataPath) +2);
        strcpy(str, dataPath);
        lbui_base_data_path = str;
    }
    
    // allocate memory for indexes
    {
        int i = 0;
        lbui_indexes = malloc(lbui_index_len * sizeof(char*));
        for (i=0; i < lbui_index_len; i++) {
            lbui_indexes[i] = malloc(lbui_max_value_len * sizeof(char));
        }
    }
    
    lbui_which_ui_used = UITYPE_SDL;
    printf("init sdl\n");
    init_retval = lbui_init_sdl(win_wid, win_hgt, extra_flags);
    printf("inited sdl\n");
    
    if (init_retval != 0) {
        ERRORMSG("Init of UI screwed up miserably (retval = %d), exiting.\n", init_retval);
        return init_retval;
    }
    
    /* Verify main term */
    if (lbui_has_frame(0, ACTIVE) == 0) {
        ERRORMSG("main window does not exist\n");
        return -2;
    }
    
    if (lbui_will_use_callback) {
        // this is a callback
        DBGPUT("going back");
        return lbui_play_game_lisp();
    }
    
    return -42; // never really started the ball.
}


int
lbui_cleanup_c_side(void) {
    
    int retval = -1;
    
    lbui_cleanup_callbacks();
    lbui_cleanup_frame_system();
#ifdef USE_SOUND    
    lbui_close_sound_system();
#endif
    
    retval = sdl_cleanup();
    
    lbui_current_lisp_system = LISPSYS_BAD;
    lbui_which_ui_used = -1;
#ifdef USE_SOUND    
    lbui_set_sound_status(0);
#endif
    return retval;
}

int
lbui_load_gfx_image(const char *fname, int idx, unsigned int transcolour) {
    return sdl_load_gfx_image(fname, idx, transcolour);
}

int
lbui_load_texture(int idx, const char *filename, int target_width, int target_height, int alpha) {
    return sdl_load_texture(idx, filename, target_width, target_height, alpha);
}

int
lbui_listen_for_event(int option) {
    return sdl_get_event(option);
}

unsigned 
lbui_get_internal_time() {
    return sdl_get_internal_time();
}

int 
lbui_flip_framebuffer() {
    return sdl_flip_framebuffer();
}


int
lbui_get_image_width(int idx) {
    return sdl_get_image_width(idx);
}

int
lbui_get_image_height(int idx) {
    return sdl_get_image_height(idx);
}

int
lbui_get_window_width() {
    return sdl_get_window_width();
}

int
lbui_get_window_height() {
    return sdl_get_window_height();
}

int
lbui_full_blit(short win_num, short x, short y, unsigned int img, short flags) {
    return sdl_full_blit(win_num, x, y, img, flags);
}

int
lbui_transparent_blit(short win_num, short x, short y, unsigned int img, short flags) {
    return sdl_transparent_blit(win_num, x, y, img, flags);
}

int
lbui_clear_coords(short win_num, short x, short y, short w, short h) {
    return sdl_clear_coords(win_num, x, y, w, h);
}

int
lbui_flush_coords(short win_num, short x, short y, short w, short h) {
    return sdl_flush_coords(win_num, x, y, w, h);
}

int
lbui_recalculate_frame_placements(int arg) {
    return sdl_recalculate_frame_placements(arg);
}

int
lbui_install_font_in_frame(int win_num, const char *font, int ptsize, int style) {

    LangbandFrame *lf =  NULL;
    int install_retval = 0;
    
    //DBGPUT("Adding to frame\n");
    
    install_retval = lbui_add_frame_fontinfo(win_num, font, ptsize, style);
    
    //DBGPUT("Done add frame\n");
	
    if (install_retval) {
        DBGPUT("Returning from add_frame with %d from %d\n", install_retval, win_num);
        return install_retval;
    }
    
    lf = lbui_predefinedFrames[win_num];
    if (lf) {
        lf = sdl_install_font_in_frame(lf);
    }
    if (lf)
        return 0;
    else
        return -3;
}

int
lbui_set_idx_intvalue(int idx, int value) {
    if (idx >= 0 && idx < lbui_index_len) {
        sprintf(lbui_indexes[idx], "%d", value);
        return idx;
    }
    else {
        return -1;
    }
}

int
lbui_set_idx_stringvalue(int idx, const char *value) {
    if (value != NULL && idx >= 0 && idx < lbui_index_len) {
        strncpy(lbui_indexes[idx], value, lbui_max_value_len - 2);
        return idx;
    }
    else {
        return -1;
    }
}

const char *
lbui_get_idx_value(int idx) {
    if (idx >= 0 && idx < lbui_index_len) {
        return lbui_indexes[idx];
    }
    else {
        return NULL; // que??
    }
}
