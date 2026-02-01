;;; test-config.el --- Example configuration for testing org-personal-site -*- lexical-binding: t; -*-

;; This is a sample configuration for testing org-personal-site locally.
;; Copy this to your init.el or evaluate it to test the package.

;;; Usage:
;; 1. Load this file: M-x load-file RET test-config.el RET
;; 2. Or add to your init.el with modifications

;;; Code:

(use-package org-personal-site
  :load-path "~/org/site/org-personal-site/"
  
  :custom
  ;; Required settings
  (org-personal-site-source-dir "~/org/site")
  (org-personal-site-output-dir "~/site-output")
  (org-personal-site-name "nana adane")
  (org-personal-site-author "Nana Adane")
  
  ;; Optional: Site URL (important for RSS)
  (org-personal-site-url "https://yourdomain.com")
  (org-personal-site-description "Personal blog and projects")
  
  ;; Optional: Custom domain for GitHub Pages
  ;; (org-personal-site-cname "yourdomain.com")
  
  ;; Theme: use 'opencode-tui, 'light, 'gruvbox, or custom plist
  (org-personal-site-theme 'opencode-tui)
  
  ;; Fonts
  (org-personal-site-font-logo "Pixelify Sans")
  (org-personal-site-font-heading "JetBrains Mono")
  (org-personal-site-font-body "Iosevka Etoile")
  
  ;; Custom CDN for body font (Iosevka not on Google Fonts)
  (org-personal-site-font-body-cdn 
   "https://cdnjs.cloudflare.com/ajax/libs/Iosevka/6.0.0/iosevka-etoile/iosevka-etoile.min.css")
  
  ;; Optional customizations
  (org-personal-site-server-port 8080)
  (org-personal-site-auto-push t)
  (org-personal-site-recent-posts-count 5)
  
  ;; Keybindings (optional but recommended)
  :bind (("C-c s n" . org-personal-site-new-post)
         ("C-c s b" . org-personal-site-build)
         ("C-c s p" . org-personal-site-preview)
         ("C-c s d" . org-personal-site-deploy)
         ("C-c s i" . org-personal-site-insert-image)))

;;; Alternative: Custom Theme Example

;; (use-package org-personal-site
;;   :load-path "~/org/site/org-personal-site/"
;;   :custom
;;   (org-personal-site-source-dir "~/org/site")
;;   (org-personal-site-output-dir "~/site-output")
;;   (org-personal-site-name "my site")
;;   (org-personal-site-author "Your Name")
;;   
;;   ;; Custom theme with your own colors
;;   (org-personal-site-theme
;;    '(:background "#1e1e1e"
;;      :foreground "#d4d4d4"
;;      :accent "#569cd6"))
;;   
;;   ;; All fonts from Google Fonts
;;   (org-personal-site-font-logo "Roboto Mono")
;;   (org-personal-site-font-heading "Inter")
;;   (org-personal-site-font-body "Source Sans Pro"))

;;; test-config.el ends here
