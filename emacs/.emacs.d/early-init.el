;; early-init.el --- Early initialization file -*- lexical-binding: t; -*-
;; Author: Aseem Ratha

;;; Commentary:
;; Early initialization file to set LSP to use plists for better performance.

;;; Code:
(setenv "LSP_USE_PLISTS" "true")



(provide 'early-init)
;;; early-init.el ends here
