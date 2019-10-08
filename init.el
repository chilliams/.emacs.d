;;; init.el --- My Emacs init file

;;; Commentary:

;; After years of using Spacemacs, I've decided to venture out on my own.

;;; Code:

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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
(show-paren-mode 1)

;; backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; history
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)


;; gui tweaks
(when window-system
  (scroll-bar-mode t)
  ;; (set-frame-size (selected-frame) 80 60)
  (set-frame-parameter (selected-frame) 'alpha 95)
  (set-frame-parameter (selected-frame) 'ns-transparent-titlebar t)
  (set-frame-parameter (selected-frame) 'ns-appearance 'dark)
  (set-frame-parameter (selected-frame) 'undecorated t)
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  )

(straight-use-package 'undo-tree)
(global-undo-tree-mode 1)

(straight-use-package 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)

(straight-use-package 'projectile)
(projectile-mode +1)
(setq projectile-enable-caching t)
(setq projectile-use-git-grep t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'helm-projectile)
(helm-projectile-on)

(straight-use-package 'helm-rg)
(define-key projectile-mode-map (kbd "M-m /") #'helm-rg)


(straight-use-package 'magit)

(straight-use-package 'which-key)
(which-key-mode)

(straight-use-package 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

;; global keybinds
(global-set-key (kbd "C-`") 'other-frame)
(require 'dired)
(global-set-key (kbd "C-x C-j") 'dired-jump)
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
(defun show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

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

;; syntax checking
(straight-use-package 'flycheck)
;; (global-flycheck-mode)

;; (straight-use-package 'flycheck-pkg-config)

;; nix
(straight-use-package 'nix-mode)

(straight-use-package 'nix-sandbox)
;; (setq flycheck-command-wrapper-function
;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
;;       flycheck-executable-find
;;       (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(straight-use-package 'direnv)
(direnv-mode)

(straight-use-package 'lsp-mode)
(setq lsp-enable-snippet nil)
(add-hook 'c-mode-hook #'lsp)

(straight-use-package 'ccls)
(require 'ccls)


;; go
(straight-use-package 'go-eldoc)
(straight-use-package 'go-guru)
(straight-use-package 'go-mode)
(straight-use-package 'go-rename)

(require 'go-eldoc)
(require 'go-guru)
(require 'go-mode)
(define-key go-mode-map (kbd "M-.") #'go-guru-definition)
(add-hook 'go-mode-hook #'go-eldoc-setup)


;; end of file
(provide 'init)
;;; init.el ends here
