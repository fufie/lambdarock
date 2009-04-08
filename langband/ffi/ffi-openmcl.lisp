;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ccl::def-foreign-type cptr :address)


(defun c_current_ui nil
  (ccl::external-call "_lbui_current_ui" :signed-fullword))

(defun c-listen-for-event (option)
  (ccl::external-call "_lbui_listen_for_event" :signed-fullword option :signed-fullword))

(defun c-init-c-side& (source-path config-path data-path win-width win-height
                       flags)
 (ccl::with-cstr (source-path824 source-path)
 (ccl::with-cstr (config-path825 config-path)
 (ccl::with-cstr (data-path826 data-path)
  (ccl::external-call "_lbui_init_c_side" :address source-path824 :address config-path825 :address data-path826 :signed-fullword win-width :signed-fullword win-height :signed-fullword flags :signed-fullword)))))

(defun c-cleanup-c-side& nil
  (ccl::external-call "_lbui_cleanup_c_side" :signed-fullword))

(defun c-set-lisp-system! (type)
  (ccl::external-call "_lbui_set_lisp_system" :signed-fullword type :void))


#+use-callback-from-c
(defun c-set-lisp-callback! (name ptr)
 (ccl::with-cstr (name827 name)
  (ccl::external-call "_lbui_set_lisp_callback" :address name827 :address ptr :void)))


#+sound-support
(defun c-init-sound-system& (size)
  (ccl::external-call "_lbui_init_sound_system" :signed-fullword size :signed-fullword))


#+sound-support
(defun c-activate-sound-system& nil
  (ccl::external-call "_lbui_activate_sound_system" :signed-fullword))


#+sound-support
(defun c-get-sound-status nil
  (ccl::external-call "_lbui_get_sound_status" :signed-fullword))


#+sound-support
(defun c-load-sound-effect& (fname idx)
 (ccl::with-cstr (fname828 fname)
  (ccl::external-call "_lbui_load_sound_effect" :address fname828 :signed-fullword idx :signed-fullword)))


#+sound-support
(defun c-play-sound-effect (idx channel loops)
  (ccl::external-call "_lbui_play_sound_effect" :signed-fullword idx :signed-halfword channel :signed-halfword loops :signed-fullword))


#+sound-support
(defun c-halt-sound-effects (channel)
  (ccl::external-call "_lbui_halt_sound_effects" :signed-halfword channel :signed-fullword))


#+sound-support
(defun c-load-music-file& (fname idx)
 (ccl::with-cstr (fname829 fname)
  (ccl::external-call "_lbui_load_music_file" :address fname829 :signed-fullword idx :signed-fullword)))


#+sound-support
(defun c-play-music-file (idx loops)
  (ccl::external-call "_lbui_play_music_file" :signed-fullword idx :signed-halfword loops :signed-fullword))


#+sound-support
(defun c-halt-music nil
  (ccl::external-call "_lbui_halt_music" :signed-fullword))


#+image-support
(defun load-gfx-image& (fname idx transcolour)
 (ccl::with-cstr (fname830 fname)
  (ccl::external-call "_lbui_load_gfx_image" :address fname830 :signed-fullword idx :unsigned-fullword transcolour :signed-fullword)))


#+image-support
(defun c-load-texture& (idx fname twid thgt alpha)
 (ccl::with-cstr (fname831 fname)
  (ccl::external-call "_lbui_load_texture" :signed-fullword idx :address fname831 :signed-fullword twid :signed-fullword thgt :unsigned-fullword alpha :signed-fullword)))


#+image-support
(defun c-get-image-width (idx)
  (ccl::external-call "_lbui_get_image_width" :signed-fullword idx :signed-fullword))


#+image-support
(defun c-get-image-height (idx)
  (ccl::external-call "_lbui_get_image_height" :signed-fullword idx :signed-fullword))

(defun c-init-frame-system& (act-size pre-size)
  (ccl::external-call "_lbui_init_frame_system" :signed-fullword act-size :signed-fullword pre-size :signed-fullword))

(defun c-add-frame! (key name)
 (ccl::with-cstr (name832 name)
  (ccl::external-call "_lbui_add_frame" :signed-fullword key :address name832 :signed-fullword)))

(defun c-add-frame-coords! (key x y w h px py)
  (ccl::external-call "_lbui_add_frame_coords" :signed-fullword key :signed-fullword x :signed-fullword y :signed-fullword w :signed-fullword h :signed-fullword px :signed-fullword py :signed-fullword))

(defun c-add-frame-tileinfo! (key tw th)
  (ccl::external-call "_lbui_add_frame_tileinfo" :signed-fullword key :signed-fullword tw :signed-fullword th :signed-fullword))

(defun c-add-frame-fontinfo! (key font ptsize style)
 (ccl::with-cstr (font833 font)
  (ccl::external-call "_lbui_add_frame_fontinfo" :signed-fullword key :address font833 :signed-fullword ptsize :signed-fullword style :signed-fullword)))

(defun c-add-frame-gfxinfo! (key use-tiles)
  (ccl::external-call "_lbui_add_frame_gfxinfo" :signed-fullword key :signed-fullword use-tiles :signed-fullword))

(defun c-add-frame-bg! (key img-idx)
  (ccl::external-call "_lbui_add_frame_bg" :signed-fullword key :signed-fullword img-idx :signed-fullword))

(defun c-has_frame (key type)
  (ccl::external-call "_lbui_has_frame" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-columns (key type)
  (ccl::external-call "_lbui_get_frame_columns" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-rows (key type)
  (ccl::external-call "_lbui_get_frame_rows" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-tile-width (key type)
  (ccl::external-call "_lbui_get_frame_tile_width" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-tile-height (key type)
  (ccl::external-call "_lbui_get_frame_tile_height" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get_frame-gfx-tiles (key type)
  (ccl::external-call "_lbui_get_frame_gfx_tiles" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-window-width nil
  (ccl::external-call "_lbui_get_window_width" :signed-fullword))

(defun c-get-window-height nil
  (ccl::external-call "_lbui_get_window_height" :signed-fullword))

(defun c-full-blit (num x y img flag)
  (ccl::external-call "_lbui_full_blit" :signed-halfword num :signed-halfword x :signed-halfword y :unsigned-fullword img :signed-halfword flag :signed-fullword))

(defun c-transparent-blit (num x y img flag)
  (ccl::external-call "_lbui_transparent_blit" :signed-halfword num :signed-halfword x :signed-halfword y :unsigned-fullword img :signed-halfword flag :signed-fullword))

(defun c-clear-coords! (num x y w h)
  (ccl::external-call "_lbui_clear_coords" :signed-halfword num :signed-halfword x :signed-halfword y :signed-halfword w :signed-halfword h :signed-fullword))

(defun c-flush-coords! (num x y w h)
  (ccl::external-call "_lbui_flush_coords" :signed-halfword num :signed-halfword x :signed-halfword y :signed-halfword w :signed-halfword h :signed-fullword))

(defun c-recalculate-frame-placements! (arg)
  (ccl::external-call "_lbui_recalculate_frame_placements" :signed-fullword arg :signed-fullword))

(defun c-install-font-in-frame! (key font ptsize style)
 (ccl::with-cstr (font834 font)
  (ccl::external-call "_lbui_install_font_in_frame" :signed-fullword key :address font834 :signed-fullword ptsize :signed-fullword style :signed-fullword)))

(defun c-get-internal-time nil
  (ccl::external-call "_lbui_get_internal_time" :unsigned-fullword))

(defun c-flip-framebuffer nil
  (ccl::external-call "_lbui_flip_framebuffer" :signed-fullword))

(defun c-get-idx-value (idx)
  (ccl::external-call "_lbui_get_idx_value" :signed-fullword idx cptr))

(defun c-set-idx-intvalue (idx value)
  (ccl::external-call "_lbui_set_idx_intvalue" :signed-fullword idx :signed-fullword value :signed-fullword))

(defun c-set-idx-stringvalue (idx value)
 (ccl::with-cstr (value835 value)
  (ccl::external-call "_lbui_set_idx_stringvalue" :signed-fullword idx :address value835 :signed-fullword)))


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
