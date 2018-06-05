; -*- mode: Emacs-Lisp;-*-
;; Abdullah's XPS 13 Emacs File

;; Notes:
;; When making changes to the .emacs file, you can evaluate distinct pieces of
;; code or buffers by selecting the region and then typing:
;; 
;;     M-x eval-region
;;
;; to evaluate the region or
;; 
;;     M-x eval-buffer
;; 
;; to re-evaluate the entire buffer.

;; Basic UI Changes

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(global-linum-mode 1)                   ; Displays Line numbers
(defalias 'yes-or-no-p 'y-or-n-p)       ; Shortens yes no to y n
(menu-bar-mode -1)                      ; No menu bar
(tool-bar-mode -1)                      ; No tool bar
(scroll-bar-mode -1)                 ; No scroll bar
(define-key global-map (kbd "RET") 'newline-and-indent) ; Auto-indent on enter
(setq line-number-mode t)
(setq inhibit-startup-message t         ; Get rid of welcome screen
      inhibit-startup-echo-area-message t)  
(setq tab-width 4)                      ; Sets tab-width to 4 characters
(setq display-time-day-and-date t       ; Displays the date and time
      display-time-12hr-format t)
(display-time)

;; Aliases
(defalias 'eb 'eval-buffer)             ; Use eb to run eval buffer
(defalias 'er 'eval-region)             ; Use er to run eval region

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; -------------------------------------------------------------------------- ;;
;;                                 Packages                                   ;;
;; -------------------------------------------------------------------------- ;;

(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (markdown-mode+ markdown-mode dockerfile-mode yaml-mode ess ido-vertical-mode fill-column-indicator powerline darkokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . fullscreen)))))

(load-theme 'darkokai t)

;; Powerline
(require 'powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)

;; Ido mode
(require 'ido)
(ido-mode t)

;; Vertical buffer list
(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; Fill-column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq fci-rule-color "#465457")
(setq fci-rule-column 80) ; Sets the fill column to appear at 80
(set-default-font "Consolas-10")
(add-to-list 'default-frame-alist '(fullscreen . fullscreen)) ;Maximize on startup

;; ;; Set tabs to be 4 spaces long
;; All spaces
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq python-indent 4)
        (setq tab-width 4))
      (untabify (point-min) (point-max)))

;; All tabs
 ;; (add-hook 'python-mode-hook
 ;;          (lambda ()
 ;;            (setq indent-tabs-mode t)
 ;;            (setq python-indent 4)
 ;;            (setq tab-width 4))
 ;;          (tabify (point-min) (point-max)))

;; Show parenthesis
(require 'paren)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)
