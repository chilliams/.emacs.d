(load "~/.emacs.d/clean")
(load "~/.emacs.d/config")
(global-hl-line-mode -1)

;; no lockfiles
(setq create-lockfiles nil)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;;; straight.el
;; bootstrap
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

;; configure
(require 'straight)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;;; basic emacs tweaks
(setq async-shell-command-buffer 'confirm-kill-process)
(setq backward-delete-char-untabify-method 'hungry)
(setq column-number-mode t)
(setq view-read-only t)
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

(defun prog-mode-setup ()
  "Set desired `prog-mode' locals."
  (flyspell-prog-mode)
  (setq require-final-newline 'ask)
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'prog-mode-setup)

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))


;;;; history
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)


;;;; spell checking
(add-hook 'text-mode-hook #'flyspell-mode)


;;;; whitespace
(straight-use-package 'ws-butler)
(require 'ws-butler)
(ws-butler-global-mode)


;;;; indentation
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


;;;; undo tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode)


;;;; ivy
(use-package ivy
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        projectile-completion-system 'ivy
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)
  (ivy-mode +1))

(use-package counsel
  :config
  (require 'shell)
  (define-key shell-mode-map (kbd "M-r") #'counsel-shell-history)
  (counsel-mode +1))

(use-package wgrep)


;;;; projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :config
  (define-key projectile-mode-map (kbd "M-m /") #'counsel-projectile-git-grep))


;;;; git
(straight-use-package 'magit)
(require 'magit)


;;; keys
(global-set-key (kbd "C-`") #'other-frame)
(require 'dired)
(global-set-key (kbd "C-x C-j") #'dired-jump)
(global-set-key (kbd "C-c o") (lambda ()
                                (interactive)
                                (other-window -1)))

(straight-use-package 'mwim)
(global-set-key (kbd "C-a") #'mwim-beginning)
(global-set-key (kbd "C-e") #'mwim-end)

(straight-use-package 'which-key)
(which-key-mode)

;;;; file types
(add-to-list 'auto-mode-alist '("\\.i\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.job\\'" . conf-colon-mode))
(add-to-list 'auto-mode-alist '("\\.pmf\\'" . conf-colon-mode))
(add-to-list 'auto-mode-alist '("\\.fs\\'". glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'". glsl-mode))
(add-to-list 'auto-mode-alist '("\\.script\\'" . lua-mode))

(setq python-guess-indent nil)


;;;; documentation modes
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'visual-line-mode)


;;;; shell stuff
(load "~/.emacs.d/emacs-pager")

(defun my-shell-mode-hook ()
  (setq comint-input-ring-file-name "~/.bash_history")
  (comint-read-input-ring t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(setenv "GIT_PAGER" "cat")

(defun wrap-async-shell-command (args)
  "Execute `async-shell-command' with a better buffer names"
  (let ((command (nth 0 args))
        (output-buffer (nth 1 args))
        (error-buffer (nth 2 args)))
    (list command
          (or output-buffer (concat "*" command " in " default-directory "*"))
          (or error-buffer (concat "* errors from " command " in " default-directory "*")))))

(advice-add 'async-shell-command :filter-args #'wrap-async-shell-command)


;;;; camel case
(add-hook 'c-mode-common-hook #'subword-mode)


;;;; syntax checking
(straight-use-package 'flycheck)
(global-flycheck-mode)


;;;; bazel
(straight-use-package
 '(emacs-bazel-mode :type git
                    :host github
                    :repo "bazelbuild/emacs-bazel-mode"))
(setq bazel-mode-buildifier-before-save t)
(add-hook 'bazel-mode-hook #'flymake-mode)


;;;; autocomplete
(use-package company
  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends '(company-capf)

        ;; These auto-complete the current selection when
        ;; `company-auto-complete-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-complete nil
        company-auto-complete-chars nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  :config
  (company-tng-mode +1)
  (global-company-mode))


;;;; lsp
(use-package lsp-mode
  :commands lsp-install-server
  :init
  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Disable indentation because I don't like it
  (setq lsp-enable-indentation nil))


;;;; formatting
(load "~/.emacs.d/google-c-style")
(require 'google-c-style)
(defun setup-c-ish-modes ()
  "Set up a bunch of settings for C/Java."
  (semantic-mode 1)
  (google-set-c-style))

(add-hook 'c-mode-common-hook #'setup-c-ish-modes)


;;;; java
(use-package lsp-java
  :after lsp-mode
  :preface
  (add-hook 'java-mode-hook #'lsp-deferred))

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


;;;; go
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

(straight-use-package 'go-mode)

(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)


;;;; protobuf
(straight-use-package 'protobuf-mode)


;;;; js
(defun eslint-fix-file ()
  "Format js file with eslint."
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(straight-use-package 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))


;;;; clojure
(straight-use-package 'clojure-mode)

(straight-use-package 'cider)
(require 'cider)
(setq cider-clojure-cli-global-options "-A:dev")
(setq cider-prompt-for-symbol t)
(setq nrepl-log-messages t)
(setq nrepl-sync-request-timeout 30)

(straight-use-package 'clj-refactor)
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c r"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)


;;;; lisp
(straight-use-package 'racket-mode)

(straight-use-package 'smartparens)
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'racket-mode-hook #'smartparens-strict-mode)


;;;; misc langs
(straight-use-package 'glsl-mode)

(straight-use-package 'json-mode)
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.g3dj\\'" . json-mode))

(straight-use-package 'lua-mode)


;;;; functions
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

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(global-set-key (kbd "C-c g") #'google)

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


;;;; work
(defun yext-java-format ()
  (interactive)
  (shell-command
   (concat "$ALPHA/tools/java/javafmt/format-java-changes.sh " buffer-file-name))
  (revert-buffer t t))

(defun yext-java-format-after-save ()
  (interactive)
  (when (and  (eq major-mode 'java-mode)
              (getenv "ALPHA"))
    (yext-java-format)))

(add-hook 'after-save-hook 'yext-java-format-after-save)

(defun jump-to-jsx ()
  "Jump to jsx source from transpiled js"
  (interactive)
  (let ((file (replace-regexp-in-string "bazel-out/darwin-fastbuild/bin/"
                                        ""
                                        (buffer-file-name))))
    (find-file (replace-regexp-in-string "\\.js" ".jsx" file))))

(global-set-key (kbd "C-c x") #'jump-to-jsx)

(defun copy-gerrit-link ()
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory))
        (alpha (getenv "ALPHA")))
    (when (not file-name)
      (error "Buffer not visiting a file"))
    (when (not (string-prefix-p alpha file-name))
      (error "File not in alpha"))
    (let ((link
           (replace-regexp-in-string
            (regexp-quote alpha)
            "https://gerrit.yext.com/plugins/gitiles/alpha/+/refs/heads/master/"
            file-name
            nil
            'literal))
          (line (number-to-string (line-number-at-pos))))
      (message (kill-new (concat link "#" line))))))


;;;; other
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (set-frame-parameter (selected-frame) 'alpha 95)
  (server-start))

(let ((machine-specific-file "~/.emacs.d/pc.el"))
  (when (file-exists-p machine-specific-file)
    (load machine-specific-file)))
