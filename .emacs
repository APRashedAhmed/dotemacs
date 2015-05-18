;;;; Emacs Config File

;; Features to add:
;; yasnippet - template system that allows to type an abbreviation and get a function template
;; jedi - python auto completion that doesnt seem to be initialized here but is installed
;; elpy - requires flake8 and importmagic, neither of which are installed. Check this
;; powershell - get it working
;; Setup C/C++


;; Add git bin to load path
(add-to-list 'load-path "C:\Program Files (x86)\Git\bin")

(setenv "GIT_ASKPASS" "git-gui--askpass")


(setq display-time-day-and-date t
                display-time-12hr-format t)
             (display-time)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default)))
 '(display-time-day-and-date t)
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd elixir emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint json-jsonlint less lua make perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scala-scalastyle scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(global-linum-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . fullscreen))))
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(package-initialize)


3(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-preprocessor-face ((t (:foreground "blue")))))

(load-theme 'monokai t)

(require 'powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil
:foreground "Black"
:background "DarkOrange2"
:box nil)


;loads the .emacs file on startup
(find-file "C:\\Users\\Abdullah\\AppData\\Roaming\\.emacs")

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;Shortens Yes and No to y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;;No backup files
(setq make-backup-files nil)

(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)


;; smex code

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;Delayed Initiation

(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

;;Hyphen on Space

      (defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command 
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))

;;Dont Update as much
    (defun smex-update-after-load (unused)
      (when (boundp 'smex-cache)
        (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)


;enables vertical ido mode on startup
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(require 'sublimity)
(sublimity-mode 1)

;;; Code:

(require 'sublimity)
(require 'cl-lib)

(defconst sublimity-scroll-version "1.2.0")

;; * customs

(defcustom sublimity-scroll-weight 3
  "scroll is maybe divided into N small scrolls"
  :group 'sublimity)

(defcustom sublimity-scroll-drift-length 2
  "scroll last N lines especially slowly"
  :group 'sublimity)

;; * utils

(defun sublimity-scroll--vscroll (lins)
  "FOR ANIMATION USE ONLY"
  (goto-char (window-start))
  (forward-line lins)
  (set-window-start nil (point)))

(defun sublimity-scroll--hscroll (cols)
  (if (< cols 0)
      (scroll-right (- cols))
    (scroll-left cols)))

;; * animation

;; should this be cached ?
(defun sublimity-scroll--gen-speeds (amount)
  "10 => '(2 2 2 1 1 1)"
  (cl-labels ((fix-list (lst &optional eax)
                        (if (null lst) nil
                          (let* ((rem (car lst))
                                 (val (floor rem))
                                 (rem (+ (- rem val) (or eax 0)))
                                 (val (if (>= rem 1) (1+ val) val))
                                 (rem (if (>= rem 1) (1- rem) rem)))
                            (cons val (fix-list (cdr lst) rem))))))
    (let (a lst)
      (cond ((integerp sublimity-scroll-weight)
             (setq sublimity-scroll-weight (float sublimity-scroll-weight))
             (sublimity-scroll--gen-speeds amount))
            ((< amount 0)
             (mapcar '- (sublimity-scroll--gen-speeds (- amount))))
            ((< amount sublimity-scroll-drift-length)
             (make-list amount 1))
            (t
             (setq amount (- amount sublimity-scroll-drift-length))
             ;; x = a t (t+1) / 2 <=> a = 2 x / (t^2 + t)
             (setq a (/ (* 2 amount)
                        (+ (expt (float sublimity-scroll-weight) 2)
                           sublimity-scroll-weight)))
             (dotimes (n sublimity-scroll-weight)
               (setq lst (cons (* a (1+ n)) lst)))
             (append (cl-remove-if 'zerop (sort (fix-list lst) '>))
                     (make-list sublimity-scroll-drift-length 1)))))))

(defun sublimity-scroll--vscroll-effect (lins)
  (save-excursion
    (let ((speeds (sublimity-scroll--gen-speeds lins)))
      (sublimity-scroll--vscroll (- lins))
      (dolist (speed speeds)
        (sublimity-scroll--vscroll speed)
        (force-window-update (selected-window))
        (redisplay)))))

(defun sublimity-scroll--hscroll-effect (cols)
  (save-excursion
    (let ((speeds (sublimity-scroll--gen-speeds cols)))
      (sublimity-scroll--hscroll (- cols))
      (dolist (speed speeds)
        (sublimity-scroll--hscroll speed)
        (force-window-update (selected-window))
        (redisplay)))))

;; * triggers

(defun sublimity-scroll--post-vscroll (lins)
  (sublimity-scroll--vscroll-effect lins))

(defun sublimity-scroll--post-hscroll (cols)
  (sublimity-scroll--hscroll-effect cols))

(add-hook 'sublimity--post-vscroll-functions 'sublimity-scroll--post-vscroll t)
(add-hook 'sublimity--post-hscroll-functions 'sublimity-scroll--post-hscroll t)

;; * provide

(provide 'sublimity-scroll)

;;; sublimity-scroll.el ends here(setq sublimity-scroll-weight 10
(cd "C:/Users/Abdullah/Skydrive/Documents/Code")
(put 'scroll-left 'disabled nil)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(setq
 python-shell-interpreter "C:\\Anaconda\\python.exe"
 python-shell-interpreter-args
 "-i C:\\Anaconda\\Scripts\\ipython-script.py console --pylab=qt")

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(set-default-font "Consolas-12")

;; Remove the scroll bar
(scroll-bar-mode -1)

(defun my-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))
