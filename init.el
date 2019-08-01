;; basic emacs tweaks
(setq async-shell-command-buffer 'confirm-kill-process)
(setq backward-delete-char-untabify-method 'hungry)
(setq column-number-mode t)
(setq comint-input-ignoredups t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq custom-file "~/.emacs-custom.el")
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-pass-command-to-system nil)
(set-frame-font "Monaco-14")


;; history
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)


;; gui tweaks
(when window-system
  (scroll-bar-mode t)
  ;; (set-frame-size (selected-frame) 80 60)
  (set-frame-parameter (selected-frame) 'alpha 90)
  (set-frame-parameter (selected-frame) 'ns-transparent-titlebar t)
  (set-frame-parameter (selected-frame) 'ns-appearance 'dark)
  (set-frame-parameter (selected-frame) 'undecorated t)
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  )


;; packages
;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; use-package
(eval-when-compile
  (require 'use-package))

;; install packages
(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :ensure t
  :config
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; global keybinds
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "C-c o") (lambda ()
				(interactive)
				(other-window -1)))


;; esoteric file types
(add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.job\\'" . conf-colon-mode))
(add-to-list 'auto-mode-alist '("\\.pmf\\'" . conf-colon-mode))


;; documentation modes
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'visual-line-mode)


;; indentation
(defun web-stuff-indent (n)
  ;; java/c/c++/proto
  (setq c-basic-offset n)
  (setq tab-width n)
  ;; web development
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )
(web-stuff-indent 2)


;; shell stuff
(defun my-shell-mode-hook ()
  (setq comint-input-ring-file-name "~/.bash_history")
  (comint-read-input-ring t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(setenv "EDITOR" "emacsclient")

(defun async-shell-command (command &optional output-buffer error-buffer)
  "Execute string COMMAND asynchronously in background.

Like `shell-command', but adds `&' at the end of COMMAND
to execute it asynchronously.

The output appears in the buffer `*Async Shell Command*'.
That buffer is in shell mode.

You can configure `async-shell-command-buffer' to specify what to do
when the `*Async Shell Command*' buffer is already taken by another
running shell command.  To run COMMAND without displaying the output
in a window you can configure `display-buffer-alist' to use the action
`display-buffer-no-window' for the buffer `*Async Shell Command*'.

In Elisp, you will often be better served by calling `start-process'
directly, since it offers more control and does not impose the use of
a shell (with its need to quote arguments)."
  (interactive
   (list
    (read-shell-command "Async shell command: " nil nil
			(let ((filename
			       (cond
				(buffer-file-name)
				((eq major-mode 'dired-mode)
				 (dired-get-filename nil t)))))
			  (and filename (file-relative-name filename))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (unless (string-match "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (shell-command command
		 (or output-buffer (concat "*" command " in " default-directory "*"))
		 error-buffer))


;; useful functions
(defun apply-function-to-region (fn)
  (save-excursion
    (let* ((beg (region-beginning))
	   (end (region-end))
	   (resulting-text
	    (funcall
	     fn
	     (buffer-substring-no-properties beg end))))
      (kill-region beg end)
      (insert resulting-text))))

(defun url-unhex-region ()
  (interactive)
  (apply-function-to-region 'url-unhex-string))

(defun url-hexify-region ()
  (interactive)
  (apply-function-to-region 'url-hexify-string))


;; go
(defun go-run-tests (args)
  (interactive)
  (save-selected-window
    (async-shell-command (concat "go test -v " args))))

(defun go-run-package-tests ()
  (interactive)
  (go-run-tests ""))

(defun go-run-package-tests-nested ()
  (interactive)
  (go-run-tests "./..."))

(defun go-run-main ()
  (interactive)
  (save-selected-window
    (async-shell-command
     (format "go run %s"
             (shell-quote-argument (buffer-file-name))))))


;; for java
(add-hook 'c-mode-common-hook (lambda ()
				(subword-mode)))
