; -*- mode: Emacs-Lisp;-*-
;; Abdullah's Emacs File

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

(global-display-line-numbers-mode t)    ; Displays Line numbers
(defalias 'yes-or-no-p 'y-or-n-p)       ; Shortens yes no to y n
(menu-bar-mode -1)                      ; No menu bar
(tool-bar-mode -1)                      ; No tool bar
(scroll-bar-mode -1)                    ; No scroll bar
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
 '(custom-safe-themes
   (quote
	("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-archives
   (quote
	(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
	(smart-mode-line-powerline-theme markdown-mode+ markdown-mode dockerfile-mode yaml-mode ess ido-vertical-mode fill-column-indicator darkokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . fullscreen)))))

;; Insert 'import ipdb; ipdb.set_trace()' at the cursor position.
(defun insert-ipdb-set-trace ()
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(global-set-key (kbd "C-c p") 'insert-ipdb-set-trace)

;; Insert 'import IPython; IPython.start_ipython()' at the cursor position.
(defun insert-ipython-start ()
  (interactive)
  (insert "import IPython; IPython.start_ipython(argv=[], user_ns=globals().update(locals()))"))

(global-set-key (kbd "C-c i") 'insert-ipython-start)

(load-theme 'darkokai t)

;; Powerline
;; (require 'powerline)
;; (powerline-default-theme)
;; (set-face-attribute 'mode-line nil
;;                     :foreground "Black"
;;                     :background "DarkOrange"
;;                     :box nil)
(sml/setup)
(setq sml/theme 'dark)

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
;; (set-default-font "Consolas-10")
(set-default-font "YaHei Consolas Hybrid-12")
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

;; JS Tab
(setq js-indent-level 2)

;; Show parenthesis
(require 'paren)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'subword-mode)

;; Markdown mode changes
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


(defun markdown-html (buffer)
  "Render the buffer as html for impatient-mode."
  (princ (with-current-buffer buffer
	   (format "<!DOCTYPE html>
<html>
<title>Impatient Markdown</title>
<xmp theme=\"united\" style=\"display:none;\"> %s  </xmp>
<script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script>
</html>"
		   (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

(defun preview-md ()
  "Start an HTTP server and preview the current Markdown buffer in impatient-mode."
  (interactive)
  (require 'impatient-mode)
  (require 'simple-httpd)

  ;; Ensure httpd is started on an available port
  (unless (httpd-running-p)
    (let ((port 8080))
      (while (condition-case nil
                 (progn
                   (setq httpd-port port)
                   (httpd-start)
                   nil)  ;; Stop looping if successful
               (error (setq port (1+ port)) t)))  ;; Try next port if error
      (message "HTTP server started on port %d." httpd-port)))

  ;; Enable impatient-mode for the current buffer
  (impatient-mode 1)

  ;; Set the Markdown rendering function for impatient-mode
  (setq imp-user-filter #'markdown-html)  

  ;; Force impatient-mode to refresh its state and notify connected clients
  (cl-incf imp-last-state)
  (imp--notify-clients)
    
  ;; Open the browser automatically with a direct link to this buffer
  (browse-url (format "http://localhost:%d/imp/live/%s" httpd-port (url-hexify-string (buffer-name)))))

(defun preview-md-end ()
  "Stop the HTTP server and disable impatient-mode in the current buffer."
  (interactive)
  (require 'impatient-mode)
  (require 'simple-httpd)

  ;; Disable impatient-mode in the current buffer
  (when (bound-and-true-p impatient-mode)
    (impatient-mode -1)
    (message "impatient-mode disabled in the current buffer."))

  ;; Stop the HTTP server if it's running
  (when (httpd-running-p)
    (httpd-stop)
    (message "HTTP server stopped.")))

(defun preview-md-reset ()
  "Stop the current Markdown preview and restart it for the current buffer."
  (interactive)
  (preview-md-end)
  (preview-md))

(defun preview-md-list ()
  "Open the impatient-mode listing page in the browser."
  (interactive)
  (require 'simple-httpd)
  (if (httpd-running-p)
      (browse-url (format "http://localhost:%d/imp/" httpd-port))
    (message "HTTP server is not running.")))
