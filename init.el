;;; init.el --- My Emacs init file

;;; Commentary:

;; After years of using Spacemacs, I've decided to venture out on my own.

;;; Code:

(load "~/.emacs.d/config")
(global-hl-line-mode -1)

;; no lockfiles
(setq create-lockfiles nil)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; bootstrap straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
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
(require 'comint)
(setq comint-input-ignoredups t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq custom-file "~/.emacs-custom.el")
(setq dired-listing-switches "-alh")
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-pass-command-to-system nil)
(savehist-mode 1)
(show-paren-mode 1)

;; history
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(defun prog-mode-setup ()
  "Set desired `prog-mode' locals."
  (flyspell-prog-mode)
  (setq require-final-newline 'ask)
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'prog-mode-setup)

;; spell checking
(add-hook 'text-mode-hook #'flyspell-mode)

(straight-use-package 'ws-butler)
(require 'ws-butler)
(ws-butler-global-mode)

(straight-use-package 'undo-tree)
(global-undo-tree-mode)

(straight-use-package 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(global-set-key (kbd "C-c y") #'helm-show-kill-ring)
(global-set-key (kbd "C-c j") #'helm-semantic-or-imenu)

(require 'shell)
(define-key shell-mode-map (kbd "M-r") #'helm-comint-input-ring)

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "M-r") #'helm-eshell-history)))

(straight-use-package 'projectile)
(projectile-mode +1)
(setq projectile-enable-caching t)
(setq projectile-use-git-grep t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'helm-projectile)
(helm-projectile-on)

(straight-use-package 'helm-ag)
;; ripgrep
(setq helm-ag-base-command "rg-wrapper --vimgrep --no-heading --smart-case --max-columns 1000 ")
(setq helm-ag-use-agignore t)
(setq helm-ag-use-grep-ignore-list t)
(define-key projectile-mode-map (kbd "M-m /") #'helm-projectile-ag)

(straight-use-package 'magit)
(require 'magit)

(straight-use-package 'which-key)
(which-key-mode)

(straight-use-package 'mwim)
(global-set-key (kbd "C-a") #'mwim-beginning)
(global-set-key (kbd "C-e") #'mwim-end)

;; global keybinds
(global-set-key (kbd "C-`") #'other-frame)
(require 'dired)
(global-set-key (kbd "C-x C-j") #'dired-jump)
(global-set-key (kbd "C-c o") (lambda ()
                                (interactive)
                                (other-window -1)))


;; esoteric file types
(add-to-list 'auto-mode-alist '("\\.i\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.job\\'" . conf-colon-mode))
(add-to-list 'auto-mode-alist '("\\.pmf\\'" . conf-colon-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bazel\\'" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . python-mode))

(setq python-guess-indent nil)

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

(defun java-indent ()
  (setq c-basic-offset 4)
  (setq tab-width 4))
(add-hook 'java-mode-hook #'java-indent)


;; shell stuff
(defun my-shell-mode-hook ()
  (setq comint-input-ring-file-name "~/.bash_history")
  (comint-read-input-ring t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "head -n 1000")

(defun wrap-async-shell-command (args)
  "Execute `async-shell-command' with a better buffer names"
  (let ((command (nth 0 args))
        (output-buffer (nth 1 args))
        (error-buffer (nth 2 args)))
    (list command
          (or output-buffer (concat "*" command " in " default-directory "*"))
          (or error-buffer (concat "* errors from " command " in " default-directory "*")))))

(advice-add 'async-shell-command :filter-args #'wrap-async-shell-command)

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


;; camel case
(add-hook 'c-mode-common-hook #'subword-mode)


;; syntax checking
(straight-use-package 'flycheck)
(global-flycheck-mode)

;; (straight-use-package 'flycheck-pkg-config)


;; nix
(straight-use-package 'nix-mode)

(straight-use-package 'nix-sandbox)
;; (setq flycheck-command-wrapper-function
;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
;;       flycheck-executable-find
;;       (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))


;; autocomplete
(straight-use-package 'company)
(require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; suggestions from company-go
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(straight-use-package 'lsp-mode)
(setq lsp-enable-snippet nil)
(setq lsp-enable-file-watchers nil)

(straight-use-package 'company-lsp)
(push 'company-lsp company-backends)


;; c++
(add-hook 'c-mode-hook #'lsp)
(add-hook 'cc-mode-hook #'lsp)

(straight-use-package 'ccls)
(require 'ccls)


;; java
(straight-use-package 'lsp-java)
(require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)

(straight-use-package 'ggtags)
(straight-use-package 'helm-gtags)

(require 'ggtags)
(require 'helm-gtags)
(define-key ggtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key ggtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
(define-key ggtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key ggtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack)

(defun ggtags-mode-enable ()
  "Enable ggtags and eldoc mode."
  (ggtags-mode 1)
  (eldoc-mode 1))

(load "~/.emacs.d/google-c-style")
(require 'google-c-style)
(defun setup-c-ish-modes ()
  "Set up a bunch of settings for C/Java."

  (semantic-mode 1)
  (google-set-c-style)
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook #'setup-c-ish-modes)


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

;; (straight-use-package 'go-eldoc)
;; (straight-use-package 'go-guru)
(straight-use-package 'go-mode)
;; (straight-use-package 'go-rename)

;; (require 'go-eldoc)
;; (require 'go-guru)
(require 'go-mode)
;; (define-key go-mode-map (kbd "M-.") #'go-guru-definition)
(add-hook 'before-save-hook 'gofmt-before-save)

;; (straight-use-package 'company-go)
;; (require 'company-go)
;; (setq company-go-show-annotation t)
(add-hook 'go-mode-hook
          (lambda ()
            (lsp)
            (set (make-local-variable 'company-backends) '(company-lsp))))


;; protobuf
(straight-use-package 'protobuf-mode)


;; js
(defun eslint-fix-file ()
  "Format js file with eslint."
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(straight-use-package 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))


;; other
(defun jump-to-test-dir ()
  "Jump to the test version of the current directory."
  (interactive)
  (dired (replace-regexp-in-string "/src/" "/test/" default-directory)))

(defun jump-to-src-dir ()
  "Jump to the src version of the current directory."
  (interactive)
  (dired (replace-regexp-in-string "/test/" "/src/" default-directory)))

(defun toggle-src-test-dir ()
  "Jump between src and test dirs."
  (interactive)
  (cond ((string-match "/src/" default-directory) (jump-to-test-dir))
        ((string-match "/test/" default-directory) (jump-to-src-dir))
        (t (print "Not in src or test directory"))))

(global-set-key (kbd "C-c t") #'toggle-src-test-dir)

(straight-use-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))

(straight-use-package 'helm-swoop)
(global-set-key (kbd "C-c s") #'helm-swoop)

(require 'lsp-go)

(straight-use-package 'lsp-ui)
(require 'lsp-ui)
(lsp-ui-mode 1)

(straight-use-package 'helm-lsp)
(require 'helm-lsp)

(straight-use-package 'dap-mode)
(require 'dap-mode)
(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)
(tooltip-mode 1)

(require 'dap-java)
(require 'dap-go)

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(require 'cider)
(setq cider-prompt-for-symbol t)
(setq nrepl-log-messages t)

(defun setup-company-cider ()
  "Make company use cider."
  (require 'cider)
  (set (make-local-variable 'company-backends) '(company-capf)))

(add-hook 'cider-mode-hook #'setup-company-cider)
(add-hook 'cider-repl-mode-hook #'setup-company-cider)

(straight-use-package 'glsl-mode)

(straight-use-package 'json-mode)
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.g3dj\\'" . json-mode))

(straight-use-package 'smartparens)
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

(straight-use-package 'clj-refactor)
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c r"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(straight-use-package 'racket-mode)
(add-hook 'racket-mode-hook #'smartparens-strict-mode)

(let ((machine-specific-file "~/.emacs.d/pc.el"))
  (when (file-exists-p machine-specific-file)
    (load machine-specific-file)))

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (set-frame-parameter (selected-frame) 'alpha 95)
  (server-start))

(straight-use-package 'clang-format)

(defun clang-format-for-filetype ()
  "Run clang-format if the current file has a file extensions
in the filetypes list."
  (let ((filetypes '("c" "cpp")))
    (when (member (file-name-extension (buffer-file-name)) filetypes)
      (clang-format-buffer))))

(add-hook 'before-save-hook 'clang-format-for-filetype)

(straight-use-package 'lua-mode)

(defun sudo-edit (&optional arg)
  (interactive "P")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun curl (url)
  "Create tmp buffer with curl output"
  (interactive "sURL: ")
  (let ((buffer url))
    (with-output-to-temp-buffer buffer
      (shell-command (format "curl -s %s" url) buffer)
      (pop-to-buffer buffer))))

(straight-use-package 'csharp-mode)

(defun indent-csharp ()
  (setq c-basic-offset 4)
  (setq tab-width 4))

(add-hook 'csharp-mode-hook #'indent-csharp)

;; end of file
(provide 'init)
;;; init.el ends here
