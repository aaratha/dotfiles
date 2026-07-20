;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; undecorated-round goes here so the window is never drawn with decorations,
;; avoiding a visible flash on startup. NS-specific params (alpha-background,
;; ns-background-blur, ns-alpha-elements) require a live frame and are applied
;; via after-init-hook in the Tweaks section instead.
(push '(undecorated-round . t) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
