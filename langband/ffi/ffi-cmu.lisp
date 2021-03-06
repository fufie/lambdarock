;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(alien:def-alien-type cptr c-call:c-string)

(export 'cptr)


(declaim (inline c_current_ui))
(alien:def-alien-routine ("lbui_current_ui" c_current_ui)
           c-call:int)


(declaim (inline c-listen-for-event))
(alien:def-alien-routine ("lbui_listen_for_event" c-listen-for-event)
           c-call:int
           (option c-call:int :in))


(declaim (inline c-init-c-side&))
(alien:def-alien-routine ("lbui_init_c_side" c-init-c-side&)
           c-call:int
           (ui cptr)
           (source-path cptr)
           (config-path cptr)
           (data-path cptr)
           (win-width c-call:int :in)
           (win-height c-call:int :in)
           (flags c-call:int :in))


(declaim (inline c-cleanup-c-side&))
(alien:def-alien-routine ("lbui_cleanup_c_side" c-cleanup-c-side&)
           c-call:int)


(declaim (inline c-set-lisp-system!))
(alien:def-alien-routine ("lbui_set_lisp_system" c-set-lisp-system!)
           c-call:void
           (type c-call:int :in))


#+use-callback-from-c

(declaim (inline c-set-lisp-callback!))

#+use-callback-from-c
(alien:def-alien-routine ("lbui_set_lisp_callback" c-set-lisp-callback!)
           c-call:void
           (name cptr)
           (ptr alien:unsigned :in))


(declaim (inline c-init-sound-system&))
(alien:def-alien-routine ("lbui_init_sound_system" c-init-sound-system&)
           c-call:int
           (size c-call:int :in))


(declaim (inline c-activate-sound-system&))
(alien:def-alien-routine ("lbui_activate_sound_system" c-activate-sound-system&)
           c-call:int)


(declaim (inline c-get-sound-status))
(alien:def-alien-routine ("lbui_get_sound_status" c-get-sound-status)
           c-call:int)


(declaim (inline c-load-sound-effect&))
(alien:def-alien-routine ("lbui_load_sound_effect" c-load-sound-effect&)
           c-call:int
           (fname cptr)
           (idx c-call:int :in))


(declaim (inline c-play-sound-effect))
(alien:def-alien-routine ("lbui_play_sound_effect" c-play-sound-effect)
           c-call:int
           (idx c-call:int :in)
           (channel c-call:short :in)
           (loops c-call:short :in))


(declaim (inline c-halt-sound-effects))
(alien:def-alien-routine ("lbui_halt_sound_effects" c-halt-sound-effects)
           c-call:int
           (channel c-call:short :in))


(declaim (inline c-load-music-file&))
(alien:def-alien-routine ("lbui_load_music_file" c-load-music-file&)
           c-call:int
           (fname cptr)
           (idx c-call:int :in))


(declaim (inline c-play-music-file))
(alien:def-alien-routine ("lbui_play_music_file" c-play-music-file)
           c-call:int
           (idx c-call:int :in)
           (loops c-call:short :in))


(declaim (inline c-halt-music))
(alien:def-alien-routine ("lbui_halt_music" c-halt-music)
           c-call:int)


#+image-support

(declaim (inline load-gfx-image&))

#+image-support
(alien:def-alien-routine ("lbui_load_gfx_image" load-gfx-image&)
           c-call:int
           (fname cptr)
           (idx c-call:int :in)
           (transcolour alien:unsigned :in))


#+image-support

(declaim (inline c-load-texture&))

#+image-support
(alien:def-alien-routine ("lbui_load_texture" c-load-texture&)
           c-call:int
           (idx c-call:int :in)
           (fname cptr)
           (twid c-call:int :in)
           (thgt c-call:int :in)
           (alpha alien:unsigned :in))


#+image-support

(declaim (inline c-get-image-width))

#+image-support
(alien:def-alien-routine ("lbui_get_image_width" c-get-image-width)
           c-call:int
           (idx c-call:int :in))


#+image-support

(declaim (inline c-get-image-height))

#+image-support
(alien:def-alien-routine ("lbui_get_image_height" c-get-image-height)
           c-call:int
           (idx c-call:int :in))


(declaim (inline c-init-frame-system&))
(alien:def-alien-routine ("lbui_init_frame_system" c-init-frame-system&)
           c-call:int
           (act-size c-call:int :in)
           (pre-size c-call:int :in))


(declaim (inline c-add-frame!))
(alien:def-alien-routine ("lbui_add_frame" c-add-frame!)
           c-call:int
           (key c-call:int :in)
           (name cptr))


(declaim (inline c-add-frame-coords!))
(alien:def-alien-routine ("lbui_add_frame_coords" c-add-frame-coords!)
           c-call:int
           (key c-call:int :in)
           (x c-call:int :in)
           (y c-call:int :in)
           (w c-call:int :in)
           (h c-call:int :in))


(declaim (inline c-add-frame-tileinfo!))
(alien:def-alien-routine ("lbui_add_frame_tileinfo" c-add-frame-tileinfo!)
           c-call:int
           (key c-call:int :in)
           (tw c-call:int :in)
           (th c-call:int :in))


(declaim (inline c-add-frame-fontinfo!))
(alien:def-alien-routine ("lbui_add_frame_fontinfo" c-add-frame-fontinfo!)
           c-call:int
           (key c-call:int :in)
           (font cptr)
           (ptsize c-call:int :in)
           (style c-call:int :in))


(declaim (inline c-add-frame-gfxinfo!))
(alien:def-alien-routine ("lbui_add_frame_gfxinfo" c-add-frame-gfxinfo!)
           c-call:int
           (key c-call:int :in)
           (use-tiles c-call:int :in))


(declaim (inline c-add-frame-bg!))
(alien:def-alien-routine ("lbui_add_frame_bg" c-add-frame-bg!)
           c-call:int
           (key c-call:int :in)
           (img-idx c-call:int :in))


(declaim (inline c-has_frame))
(alien:def-alien-routine ("lbui_has_frame" c-has_frame)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-frame-columns))
(alien:def-alien-routine ("lbui_get_frame_columns" c-get-frame-columns)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-frame-rows))
(alien:def-alien-routine ("lbui_get_frame_rows" c-get-frame-rows)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-frame-tile-width))
(alien:def-alien-routine ("lbui_get_frame_tile_width" c-get-frame-tile-width)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-frame-tile-height))
(alien:def-alien-routine ("lbui_get_frame_tile_height" c-get-frame-tile-height)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get_frame-gfx-tiles))
(alien:def-alien-routine ("lbui_get_frame_gfx_tiles" c-get_frame-gfx-tiles)
           c-call:int
           (key c-call:int :in)
           (type c-call:int :in))


(declaim (inline c-get-window-width))
(alien:def-alien-routine ("lbui_get_window_width" c-get-window-width)
           c-call:int)


(declaim (inline c-get-window-height))
(alien:def-alien-routine ("lbui_get_window_height" c-get-window-height)
           c-call:int)


(declaim (inline c-full-blit))
(alien:def-alien-routine ("lbui_full_blit" c-full-blit)
           c-call:int
           (num c-call:short :in)
           (x c-call:short :in)
           (y c-call:short :in)
           (img alien:unsigned :in)
           (flag c-call:short :in))


(declaim (inline c-transparent-blit))
(alien:def-alien-routine ("lbui_transparent_blit" c-transparent-blit)
           c-call:int
           (num c-call:short :in)
           (x c-call:short :in)
           (y c-call:short :in)
           (img alien:unsigned :in)
           (flag c-call:short :in))


(declaim (inline c-clear-coords!))
(alien:def-alien-routine ("lbui_clear_coords" c-clear-coords!)
           c-call:int
           (num c-call:short :in)
           (x c-call:short :in)
           (y c-call:short :in)
           (w c-call:short :in)
           (h c-call:short :in))


(declaim (inline c-flush-coords!))
(alien:def-alien-routine ("lbui_flush_coords" c-flush-coords!)
           c-call:int
           (num c-call:short :in)
           (x c-call:short :in)
           (y c-call:short :in)
           (w c-call:short :in)
           (h c-call:short :in))


(declaim (inline c-recalculate-frame-placements!))
(alien:def-alien-routine ("lbui_recalculate_frame_placements" c-recalculate-frame-placements!)
           c-call:int
           (arg c-call:int :in))


(declaim (inline c-install-font-in-frame!))
(alien:def-alien-routine ("lbui_install_font_in_frame" c-install-font-in-frame!)
           c-call:int
           (key c-call:int :in)
           (font cptr)
           (ptsize c-call:int :in)
           (style c-call:int :in))


(declaim (inline c-get-internal-time))
(alien:def-alien-routine ("lbui_get_internal_time" c-get-internal-time)
           alien:unsigned)


(declaim (inline c-flip-framebuffer))
(alien:def-alien-routine ("lbui_flip_framebuffer" c-flip-framebuffer)
           c-call:int)


(declaim (inline c-get-idx-value))
(alien:def-alien-routine ("lbui_get_idx_value" c-get-idx-value)
           cptr
           (idx c-call:int :in))


(declaim (inline c-set-idx-intvalue))
(alien:def-alien-routine ("lbui_set_idx_intvalue" c-set-idx-intvalue)
           c-call:int
           (idx c-call:int :in)
           (value c-call:int :in))


(declaim (inline c-set-idx-stringvalue))
(alien:def-alien-routine ("lbui_set_idx_stringvalue" c-set-idx-stringvalue)
           c-call:int
           (idx c-call:int :in)
           (value cptr))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c-listen-for-event c-init-c-side& c-cleanup-c-side&
     c-set-lisp-system! c-set-lisp-callback! c-init-sound-system&
     c-activate-sound-system& c-get-sound-status c-load-sound-effect&
     c-play-sound-effect c-halt-sound-effects c-load-music-file&
     c-play-music-file c-halt-music load-gfx-image& c-load-texture&
     c-get-image-width c-get-image-height c-init-frame-system& c-add-frame!
     c-add-frame-coords! c-add-frame-tileinfo! c-add-frame-fontinfo!
     c-add-frame-gfxinfo! c-add-frame-bg! c-has_frame c-get-frame-columns
     c-get-frame-rows c-get-frame-tile-width c-get-frame-tile-height
     c-get_frame-gfx-tiles c-get-window-width c-get-window-height c-full-blit
     c-transparent-blit c-clear-coords! c-flush-coords!
     c-recalculate-frame-placements! c-install-font-in-frame!
     c-get-internal-time c-flip-framebuffer c-get-idx-value c-set-idx-intvalue
     c-set-idx-stringvalue)))

;;; End of generated file.
