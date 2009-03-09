(sb-alien:load-shared-object "/Users/stig/Library/Frameworks/ZTerminal.framework/ZTerminal")
(sb-alien:define-alien-routine "cocoahelper_init" void)
(sb-alien:define-alien-routine "load_sdl" int)

(cocoahelper-init)
(load-sdl)
