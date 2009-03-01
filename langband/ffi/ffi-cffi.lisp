;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(cffi:defctype cptr :string)


(cffi:defcfun ("lbui_current_ui" c_current_ui :library :lbui :calling-convention :stdcall) :int)

(cffi:defcfun ("lbui_listen_for_event" c-listen-for-event :library :lbui :calling-convention :stdcall) :int
 (option :int) )

(cffi:defcfun ("lbui_init_c_side" c-init-c-side& :library :lbui :calling-convention :stdcall) :int
 (ui cptr) (source-path cptr) (config-path cptr) (data-path cptr) (win-width
                                                                   :int) (win-height
                                                                          :int) (flags
                                                                                 :int) )

(cffi:defcfun ("lbui_cleanup_c_side" c-cleanup-c-side& :library :lbui :calling-convention :stdcall) :int)

(cffi:defcfun ("lbui_set_lisp_system" c-set-lisp-system! :library :lbui :calling-convention :stdcall) :void
 (type :int) )


#+use-callback-from-c
(cffi:defcfun ("lbui_set_lisp_callback" c-set-lisp-callback! :library :lbui :calling-convention :stdcall) :void
 (name cptr) (ptr :pointer) )

(cffi:defcfun ("lbui_init_sound_system" c-init-sound-system& :library :lbui :calling-convention :stdcall) :int
 (size :int) )

(cffi:defcfun ("lbui_activate_sound_system" c-activate-sound-system& :library :lbui :calling-convention :stdcall) :int)

(cffi:defcfun ("lbui_get_sound_status" c-get-sound-status :library :lbui :calling-convention :stdcall) :int)

(cffi:defcfun ("lbui_load_sound_effect" c-load-sound-effect& :library :lbui :calling-convention :stdcall) :int
 (fname cptr) (idx :int) )

(cffi:defcfun ("lbui_play_sound_effect" c-play-sound-effect :library :lbui :calling-convention :stdcall) :int
 (idx :int) (channel :short) (loops :short) )

(cffi:defcfun ("lbui_halt_sound_effects" c-halt-sound-effects :library :lbui :calling-convention :stdcall) :int
 (channel :short) )

(cffi:defcfun ("lbui_load_music_file" c-load-music-file& :library :lbui :calling-convention :stdcall) :int
 (fname cptr) (idx :int) )

(cffi:defcfun ("lbui_play_music_file" c-play-music-file :library :lbui :calling-convention :stdcall) :int
 (idx :int) (loops :short) )

(cffi:defcfun ("lbui_halt_music" c-halt-music :library :lbui :calling-convention :stdcall) :int)


#+image-support
(cffi:defcfun ("lbui_load_gfx_image" load-gfx-image& :library :lbui :calling-convention :stdcall) :int
 (fname cptr) (idx :int) (transcolour :unsigned-long) )


#+image-support
(cffi:defcfun ("lbui_load_texture" c-load-texture& :library :lbui :calling-convention :stdcall) :int
 (idx :int) (fname cptr) (twid :int) (thgt :int) (alpha :unsigned-long) )


#+image-support
(cffi:defcfun ("lbui_get_image_width" c-get-image-width :library :lbui :calling-convention :stdcall) :int
 (idx :int) )


#+image-support
(cffi:defcfun ("lbui_get_image_height" c-get-image-height :library :lbui :calling-convention :stdcall) :int
 (idx :int) )

(cffi:defcfun ("lbui_init_frame_system" c-init-frame-system& :library :lbui :calling-convention :stdcall) :int
 (act-size :int) (pre-size :int) )

(cffi:defcfun ("lbui_add_frame" c-add-frame! :library :lbui :calling-convention :stdcall) :int
 (key :int) (name cptr) )

(cffi:defcfun ("lbui_add_frame_coords" c-add-frame-coords! :library :lbui :calling-convention :stdcall) :int
 (key :int) (x :int) (y :int) (w :int) (h :int) )

(cffi:defcfun ("lbui_add_frame_tileinfo" c-add-frame-tileinfo! :library :lbui :calling-convention :stdcall) :int
 (key :int) (tw :int) (th :int) )

(cffi:defcfun ("lbui_add_frame_fontinfo" c-add-frame-fontinfo! :library :lbui :calling-convention :stdcall) :int
 (key :int) (font cptr) (ptsize :int) (style :int) )

(cffi:defcfun ("lbui_add_frame_gfxinfo" c-add-frame-gfxinfo! :library :lbui :calling-convention :stdcall) :int
 (key :int) (use-tiles :int) )

(cffi:defcfun ("lbui_add_frame_bg" c-add-frame-bg! :library :lbui :calling-convention :stdcall) :int
 (key :int) (img-idx :int) )

(cffi:defcfun ("lbui_has_frame" c-has_frame :library :lbui :calling-convention :stdcall) :int
 (key :int) (type :int) )

(cffi:defcfun ("lbui_get_frame_columns" c-get-frame-columns :library :lbui :calling-convention :stdcall) :int
 (key :int) (type :int) )

(cffi:defcfun ("lbui_get_frame_rows" c-get-frame-rows :library :lbui :calling-convention :stdcall) :int
 (key :int) (type :int) )

(cffi:defcfun ("lbui_get_frame_tile_width" c-get-frame-tile-width :library :lbui :calling-convention :stdcall) :int
 (key :int) (type :int) )

(cffi:defcfun ("lbui_get_frame_tile_height" c-get-frame-tile-height :library :lbui :calling-convention :stdcall) :int
 (key :int) (type :int) )

(cffi:defcfun ("lbui_get_frame_gfx_tiles" c-get_frame-gfx-tiles :library :lbui :calling-convention :stdcall) :int
 (key :int) (type :int) )

(cffi:defcfun ("lbui_get_window_width" c-get-window-width :library :lbui :calling-convention :stdcall) :int)

(cffi:defcfun ("lbui_get_window_height" c-get-window-height :library :lbui :calling-convention :stdcall) :int)

(cffi:defcfun ("lbui_full_blit" c-full-blit :library :lbui :calling-convention :stdcall) :int
 (num :short) (x :short) (y :short) (img :unsigned-long) (flag :short) )

(cffi:defcfun ("lbui_transparent_blit" c-transparent-blit :library :lbui :calling-convention :stdcall) :int
 (num :short) (x :short) (y :short) (img :unsigned-long) (flag :short) )

(cffi:defcfun ("lbui_clear_coords" c-clear-coords! :library :lbui :calling-convention :stdcall) :int
 (num :short) (x :short) (y :short) (w :short) (h :short) )

(cffi:defcfun ("lbui_flush_coords" c-flush-coords! :library :lbui :calling-convention :stdcall) :int
 (num :short) (x :short) (y :short) (w :short) (h :short) )

(cffi:defcfun ("lbui_recalculate_frame_placements" c-recalculate-frame-placements! :library :lbui :calling-convention :stdcall) :int
 (arg :int) )

(cffi:defcfun ("lbui_install_font_in_frame" c-install-font-in-frame! :library :lbui :calling-convention :stdcall) :int
 (key :int) (font cptr) (ptsize :int) (style :int) )

(cffi:defcfun ("lbui_get_internal_time" c-get-internal-time :library :lbui :calling-convention :stdcall) :unsigned-long)

(cffi:defcfun ("lbui_flip_framebuffer" c-flip-framebuffer :library :lbui :calling-convention :stdcall) :int)

(cffi:defcfun ("lbui_get_idx_value" c-get-idx-value :library :lbui :calling-convention :stdcall) cptr
 (idx :int) )

(cffi:defcfun ("lbui_set_idx_intvalue" c-set-idx-intvalue :library :lbui :calling-convention :stdcall) :int
 (idx :int) (value :int) )

(cffi:defcfun ("lbui_set_idx_stringvalue" c-set-idx-stringvalue :library :lbui :calling-convention :stdcall) :int
 (idx :int) (value cptr) )


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
