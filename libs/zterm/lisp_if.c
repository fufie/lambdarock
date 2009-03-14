#include <stdio.h>
#include "langband.h"
#include "lbtools.h"


static void lbui_set_generic_callback(char *name, int (*fun)());
static int (*generic_callback_play)() = 0;
static int (*generic_callback_resize)(int,int) = 0;
static int (*generic_callback_mouseclick)(int,int,int) = 0;

// forward
#ifdef DARWIN
void cocoahelper_init();
#endif

void
lbui_set_lisp_system(LISP_SYSTEMS val) {

#ifdef DARWIN
    cocoahelper_init();
#endif
    
    if (val == LISPSYS_CMUCL || val == LISPSYS_ACL ||
        val == LISPSYS_SBCL || val == LISPSYS_LISPWORKS ||
        val == LISPSYS_ECL) {
	lbui_current_lisp_system = val;
    }
    else if (val == LISPSYS_CLISP || val == LISPSYS_CORMAN) {
	lbui_current_lisp_system = val;
	lbui_will_use_callback = 0;
    }
    else {
	ERRORMSG("Unknown lisp-system given: %d.\n", val);
    }
    
}

int
lbui_cleanup_callbacks() {

    //DBGPUT("Cleaning callbacks\n");
    generic_callback_play = 0;
    generic_callback_resize = 0;
    generic_callback_mouseclick = 0;
    
    return 0;
}

void
lbui_set_lisp_callback (char *name, void *ptr) {

    //DBGPUT("callback %s %p\n", name, ptr);
    if (lbui_current_lisp_system == LISPSYS_ACL || 
        lbui_current_lisp_system == LISPSYS_SBCL ||
        lbui_current_lisp_system == LISPSYS_LISPWORKS || 
        lbui_current_lisp_system == LISPSYS_ECL) {
	lbui_set_generic_callback(name, ptr);
    }
    else {
	ERRORMSG("Don't know how to set callback '%s' for lisp-system %d.\n", name, lbui_current_lisp_system);
    }
}


void
lbui_set_generic_callback(char *name, int (*fun)()) {
    if (name != NULL && strlen(name)> 0) {
	if (!strcmp(name, "play-game")) {
	    lbui_will_use_callback = 1;
	    generic_callback_play = fun;
	}
	else if (!strcmp(name, "adjust-size")) {
	    generic_callback_resize = fun;
	}
	else if (!strcmp(name, "mouse-clicked")) {
	    generic_callback_mouseclick = fun;
	}
	else {
	    ERRORMSG("Unknown callback '%s'\n", name);
	}
    }
}

int
lbui_play_game_lisp() {

    if (lbui_will_use_callback) {
	
	if ((lbui_current_lisp_system == LISPSYS_SBCL ||
             lbui_current_lisp_system == LISPSYS_ACL ||
             lbui_current_lisp_system == LISPSYS_LISPWORKS ||
             lbui_current_lisp_system == LISPSYS_ECL)
            && generic_callback_play) {
	    (*generic_callback_play)();
	}
	else {
	    ERRORMSG("Unable to handle callback for system %d..\n", lbui_current_lisp_system);
	    return -5;
	}
    }
    else {
	ERRORMSG("Tried to play by callback, but lisp-system %d doesn't want callbacking.\n",
		 lbui_current_lisp_system);
	return -6;
    }

    return 0;
}

#define make_fixnum(n) ((cmucl_lispobj)((n)<<2))

void
lbui_readjust_screen_lisp(int width, int height) {

    if (lbui_will_use_callback) {
	// DBGPUT("Note: calling resize on lisp-side\n"); 
	
	if ((lbui_current_lisp_system == LISPSYS_SBCL ||
             lbui_current_lisp_system == LISPSYS_ACL ||
             lbui_current_lisp_system == LISPSYS_LISPWORKS ||
             lbui_current_lisp_system == LISPSYS_ECL)
            && generic_callback_resize) {
	    (*generic_callback_resize)(width, height);
	}
	else {
	    ERRORMSG("Unable to handle resize-callback for system %d..\n", lbui_current_lisp_system);
	}
    }
    else {
        //	ERRORMSG("Tried to resize by callback, but lisp-system %d doesn't want callbacking.\n",
        //		lbui_current_lisp_system);
    }
 

}

void
lbui_mouse_clicked(int button, int x, int y) {

    if (lbui_will_use_callback) {
	if ((lbui_current_lisp_system == LISPSYS_SBCL ||
             lbui_current_lisp_system == LISPSYS_ACL ||
             lbui_current_lisp_system == LISPSYS_LISPWORKS ||
             lbui_current_lisp_system == LISPSYS_ECL)
            && generic_callback_resize) {
	    (*generic_callback_mouseclick)(button, x, y);
	}

	else {
	    ERRORMSG("Unable to handle mouseclick-callback for system %d..\n", lbui_current_lisp_system);
	}
    }
    else {
	// INFOMSG("Tried to resize by callback, but lisp-system %d doesn't want callbacking.\n",
	//         lbui_current_lisp_system);
    }

}
