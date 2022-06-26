;;; -*- lexical-binding: t -*-

;;; references:
;;; https://emacs-jp.github.io/tips/emacs-in-2020
;;; https://uwabami.github.io/cc-env/Emacs.html
;;; https://www.grugrut.net/posts/my-emacs-init-el/
;;; https://emacs-jp.github.io/tips/startup-optimization


(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(defun my/locate-user-emacs-data-file (name)
  "Emacsが保存する個人用データファイルのパスを返す。
XDG_DATA_HOMEが設定されていれば$XDG_DATA_HOME/emacs、
そうでなければuser-emacs-directoryとなる。"
  (let* ((xdg-data-home (getenv "XDG_DATA_HOME"))
         (dir (if xdg-data-home
                  (expand-file-name "emacs" xdg-data-home)
                user-emacs-directory)))
    (expand-file-name name dir)))

(defmacro my/with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

;;; 追加のload-path
(defvar my/additional-library-dir (locate-user-emacs-file "opt")
  "パッケージ管理外のライブラリが置かれるディレクトリ。
このディレクトリ直下のサブディレクトリがload-pathに追加される。")

;; パッケージ管理外のライブラリをload-pathに追加
(when (file-directory-p my/additional-library-dir)
  (dolist (subdir (directory-files my/additional-library-dir))
    (unless (string-prefix-p "." subdir)
      (let ((dir (expand-file-name subdir my/additional-library-dir)))
        (when (file-directory-p dir)
          (push dir load-path))))))

;; ここに設定を書く
(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf startup
  :leaf-defer nil
  :custom
  `((inhibit-startup-screen . t)
    (initial-scratch-message . nil)
    (user-mail-address . "yanagi@shakenbu.org")
    (source-directory . "~/opt/emacs")
    )
  :hook
  (window-setup-hook . (lambda ()
                         (message (format "emacs-init-time = %s"(emacs-init-time)))))
  (my/emacs-launched-hook . (lambda ()
                              (setq gc-cons-threshold (* 32 1000 1000))
                              (setq gc-cons-percentage 0.1)))
  :config
  (run-with-idle-timer 60.0 t #'garbage-collect)
  (fset 'display-startup-echo-area-message 'ignore))

(leaf exec-path-from-shell-cache
  :if (memq window-system '(mac ns))
  :require t
  :custom
  `(exec-path-from-shell-cache-file . ,(my/locate-user-emacs-data-file "path-cache"))
  :config
  (exec-path-from-shell-cache-initialize-with-cache))

(leaf scratch-ext
  :require t
  :bind
  ("M-s M-s" . scratch-ext-switch-to-scratch)
  :custom
  `(scratch-ext-log-directory . ,(my/locate-user-emacs-data-file "scratch")))

(leaf cus-start
  :preface
  (defun my/kill-region-or-backward-delete-word (&optional arg)
    "Kill a region or a word backward."
    (interactive)
    (if (use-region-p)
        (kill-region (mark) (point))
      (delete-region (point) (progn (backward-word (or arg 1)) (point)))))

  :bind
  (("C-h" . backward-delete-char)
   ("C-c h" . help-command)
   ("M-g" . goto-line)
   ("C-q" . nil)
   ("C-c C-q" . quoted-insert)
   ("C-w" . my/kill-region-or-backward-delete-word)
   )

  :custom
  `((indent-tabs-mode . nil)
    (create-lockfiles . nil)
    (history-length . 1000)
    (history-delete-duplicates . t)
    (text-quoting-style . 'straight)
    (use-dialog-box . nil)
    )
  )

(leaf modus-themes
  :ensure t
  :custom
  (
   (modus-themes-syntax . '(green-strings))
   (modus-themes-fringes . 'intense)
   (modus-themes-paren-match '(bold intense))
   (modus-themes-mode-line '(3d))
   )
  :config
  (modus-themes-load-themes)
  (modus-themes-load-vivendi)
  )

(leaf window
  :preface
  (defun my/swap-buffers-in-windows ()
    (let* ((win1 (selected-window))
           (win2 (next-window))
           (buf1 (window-buffer win1))
           (buf2 (window-buffer win2)))
      (set-window-buffer win2 buf1)
      (set-window-buffer win1 buf2)))

  (defun my/other-window-or-split (arg)
    (interactive "P")
    (if (one-window-p)
        (if arg
            (split-window-horizontally)
          (split-window-vertically))
      (if arg
          (my/swap-buffers-in-windows)))
    (other-window 1))

  :bind
  (("C-t" . my/other-window-or-split)
   ("C-q C-q" . delete-other-windows)
   ("C-M-j" . scroll-up-line)
   ("C-M-k" . scroll-down-line))
  )

(leaf frame
  :preface
  (defvar my/frame-opacity-step 5)

  (defun my/increase-frame-opacity ()
    (interactive)
    (let* ((frame-opacity-max 100)
           (current-opacity (or (frame-parameter nil 'alpha) 100))
           (new-opacity (min frame-opacity-max
                             (+ current-opacity my/frame-opacity-step))))
      (set-frame-parameter nil 'alpha new-opacity)))

  (defun my/decrease-frame-opacity ()
    (interactive)
    (let* ((frame-opacity-min 40)
           (current-opacity (or (frame-parameter nil 'alpha) 100))
           (new-opacity (max frame-opacity-min
                             (- current-opacity my/frame-opacity-step))))
      (set-frame-parameter nil 'alpha new-opacity)))

  :bind
  (("<M-left>" . my/decrease-frame-opacity)
   ("<M-right>" . my/increase-frame-opacity))
  :custom
  ((window-resize-pixelwise . t)
   (frame-resize-pixelwise . t))
  )

(leaf font
  :custom
  ((use-default-font-for-symbols . nil))

  :config
  ;; http://extra-vision.blogspot.com/2016/07/emacs.html
  (let* ((fontset-name "myfont")
         (fsn (create-fontset-from-ascii-font "Monaco-14" nil fontset-name)))
    (set-fontset-font fsn '#x3000 (font-spec :family "HackGen Console" :size 15) nil 'append)
    (set-fontset-font fsn 'unicode (font-spec :family "Migu 1M" :size 16) nil 'append)
    (add-to-list 'default-frame-alist `(font . ,fsn)))
  )

(leaf display-settings
  :custom
  `((line-number-mode . t)
    (column-number-mode . t)
    (transient-mark-mode . t)
    (search-highlight . t)
    (query-replace-highlight . t)
    (show-paren-mode . 1)
    (indicate-empty-lines . t)
    (indicate-buffer-boundaries . 'left)
    (frame-title-format . ,(concat "%b [ %f ]"
                                   " - "
                                   (emacs-version)))
    (display-time-default-load-average . nil)
    (display-time-format . "%m/%d(%a)%H:%M")
    (display-time-mode . t)
    (display-line-numbers . t)
    (show-trailing-whitespace . t)
    )
  :config
  (add-to-list 'default-frame-alist '(width . 180))
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(left . 130))
  (add-to-list 'default-frame-alist '(top . 100))

  (leaf Buffer-menu-mode
    :hook
    (Buffer-menu-mode-hook . (lambda ()
                               (setq show-trailing-whitespace nil)))
    )
  )

(leaf *mode-line
  :preface
  (defun my:mode-line-position-reverse ()
    (propertize
     (format-message "(%d,%d)"
                     (if (eobp)
                         -1
                       (- (line-number-at-pos) (count-lines (point-min) (point-max)) 1))
                     (- (point) (line-end-position) 1))
     'face '(:foreground "gray60")))
  :custom
  ((mode-line-format
    .
    '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification
      mode-line-buffer-identification "   " mode-line-position
      (:eval
       (my:mode-line-position-reverse))
      (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
    )))


(leaf files
  :preface
  (defvar my/auto-save-exclude-filename-regexp "\\(^/[a-z]+:\\)\\|\\(\\.gpg$\\)\\|\\(/Dropbox/\\)")

  :custom
  ((auto-save-visited-interval . 0.5)
   (make-backup-files . nil)
   (require-final-newline . t)
   )

  :config
  (leaf switch-buffer-functions
    :ensure t
    :preface
    (defun my/toggle-auto-save-visited-mode-for-buffer (prev cur)
      (let ((filename (buffer-file-name cur)))
        (auto-save-visited-mode
         (if (and (stringp filename)
                  (string-match my/auto-save-exclude-filename-regexp filename))
             -1))))
    :hook
    (switch-buffer-functions . my/toggle-auto-save-visited-mode-for-buffer)
    )

  ;; 開いているファイルとバッファをリネームする
  ;; https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
  (defun my/rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let* ((name (buffer-name))
           (filename (buffer-file-name))
           (basename (file-name-nondirectory filename)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer `%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
          (if (get-buffer new-name)
              (error "A buffer named `%s' already exists!" new-name)
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File `%s' successfully renamed to `%s'"
                     name (file-name-nondirectory new-name)))))))
  )

(leaf save-place
  :custom
  `(save-place-file . ,(my/locate-user-emacs-data-file "places"))
  :global-minor-mode t)

(leaf simple
  :doc "basic editing commands for Emacs"
  :preface
  (defun my/copy-whole-buffer-as-kill ()
    "Copy whole buffer."
    (interactive)
    (save-excursion
      (kill-new (buffer-string))
      (message "Buffer copied onto the kill ring.")
      ))
  (defun my/copy-buffer-file-name ()
    "Copy name of the visiting buffer."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (kill-new file-name)
      (message "File name copied: %s" file-name)
      ))

  :bind*
  (("C-c C-k" . kill-current-buffer)
   ("C-x M-w" . my/copy-whole-buffer-as-kill))
  :custom
  ((kill-ring-max . 300)
   (kill-do-not-save-duplicates . t)
   (kill-read-only-ok . t)
   ))

(leaf electric-pair
  :hook
  (minibuffer-setup-hook . (lambda () (electric-pair-local-mode -1)))
  :global-minor-mode t)

(leaf indent
  :preface
  (defun my/yank-and-indent ()
    (interactive)
    (yank)
    (call-interactively 'indent-region))
  :bind
  (("C-M-'" . indent-region)
   ("C-M-y" . my/yank-and-indent)))

(leaf highlight-indent-guides
  :ensure t
  :blackout t
  :hook (yaml-mode-hook . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method . 'bitmap)
  (highlight-indent-guides-responsive . 'stack)
  )

(leaf comment
  :custom
  ((comment-style . 'multi-line))
  )

(leaf uniquify
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
   (uniquify-ignore-buffers-re . "*[^*]+*")))

(leaf view
  :bind
  ((view-mode-map
    ("N" . View-search-last-regexp-backward)
    ("?" . View-search-regexp-backward)
    ("b" . View-scroll-page-backward)
    ("f" . View-scroll-page-forward)
    ("j" . View-scroll-line-forward)
    ("k" . View-scroll-line-backward)))
  :custom
  ((view-read-only . t)
   ))

(leaf find-func
  :hook
  ((find-function-after-hook . view-mode-enter)))

(leaf sh-script
  :custom
  (sh-basic-offset . 2))

(leaf makefile-mode
  :hook
  (makefile-mode-hook . (lambda ()
                          (fset 'makefile-warn-suspicious-lines 'ignore)))
  )

(leaf ruby-mode
  :hook
  (ruby-mode-hook . ruby-electric-mode)
  :custom
  (ruby-insert-encoding-magic-comment . nil)
  :config
  (leaf ruby-electric :ensure t :blackout t)
  )

(leaf lua-mode
  :ensure t
  :custom
  (lua-indent-level . 2)
  )

(leaf iflipb
  :ensure t
  :bind
  (("C-." . iflipb-next-buffer)
   ("C-," . iflipb-previous-buffer))
  :custom
  ((iflipb-wrap-around . t)
   (iflipb-ignore-buffers . (lambda (bufname)
                              (and (not (string= "*scratch*" bufname))
                                   (string-match "^[*]" bufname))))
   )
  )

(leaf recentf
  :custom
  (recentf-auto-cleanup . 'never)
  (recentf-max-saved-items . 1000)
  `(recentf-save-file . ,(my/locate-user-emacs-data-file "recentf"))
  :hook
  (find-file-hook . (lambda ()
                      (my/with-suppressed-message (recentf-save-list))))
  :global-minor-mode t)

(leaf *completion
  :config
  (leaf minibuffer
    :custom
    (enable-recursive-minibuffers . t)
    (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt))
    :hook
    (minibuffer-setup-hook . cursor-intangible-mode)
    )

  (leaf vertico
    :ensure t
    :preface
    (autoload 'vertico-directory--completing-file-p "vertico-directory")
    (defun my/vertico-insert-project-root ()
      (interactive)
      (when (and (> (point) (minibuffer-prompt-end))
                 (vertico-directory--completing-file-p))
        (let ((pdir (consult--project-root)))
          (when pdir
            (delete-minibuffer-contents)
            (insert (abbreviate-file-name pdir))))))
    :custom
    (vertico-count . 20)
    (vertico-cycle . t)
    :global-minor-mode t
    :bind
    (:vertico-map
     ("^" . #'my/vertico-insert-project-root))
    )

  (leaf orderless
    :ensure t
    :require t
    :preface
    ;; orderless-migemo: https://nyoho.jp/diary/?date=20210615
    (defun orderless-migemo (component)
      (let ((pattern (migemo-get-pattern component)))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))

    :custom
    (completion-styles . '(partial-completion orderless))
    (completion-category-overrides . '((consult-location (styles orderless-migemo-style))))

    :config
    (orderless-define-completion-style
     orderless-migemo-style
     (orderless-matching-styles '(orderless-literal
                                  orderless-regexp
                                  orderless-migemo)))
    )

  (leaf marginalia
    :ensure t
    :global-minor-mode t)

  (leaf consult
    :ensure t
    :preface
    (defun my/consult-preview-exclude-by-file-name (orig-fun name)
      (let ((excluded (and name
                           (string-match-p "\.gpg$"
                                           (abbreviate-file-name (expand-file-name name))))))
        (if excluded
            (progn
              (message "not previewed")
              nil)
          (apply orig-fun (list name)))))
    :bind
    (("C-S-s" . consult-line)
     ("C-x b" . consult-buffer)
     ("M-y" . consult-yank-from-kill-ring)
     (:isearch-mode-map
      ("C-l" . consult-line))
     )
    :advice
    (:around consult--find-file-temporarily my/consult-preview-exclude-by-file-name)
    )

  (leaf consult-dir
    :ensure t
    :bind
    ("C-x C-d" . consult-dir)
    (:vertico-map
     ("C-x C-d" . consult-dir)
     ("C-x C-j" . consult-dir-jump-file))
    )

  (leaf embark
    :ensure t
    :bind
    ("C-;" . embark-act)
    (:embark-file-map
     ("r" . find-file-read-only)
     ("R" . rename-file))
    :config
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    )

  (leaf embark-consult
    :ensure t
    :after (embark consult)
    :hook
    (embark-collect-mode-hook . consult-preview-at-point-mode))

  (leaf affe
    :ensure t
    :preface
    (defun my/affe-grep (&optional at-point)
      "C-uをつけるとカーソル位置の文字列で検索するaffe-grep。"
      (interactive "P")
      (if at-point
          (affe-grep nil (thing-at-point 'symbol))
        (affe-grep)))
    :bind
    ("M-o" . my/affe-grep)
    ("M-p" . affe-find)
    :custom
    (affe-count . 100)
    (affe-regexp-function . #'orderless-pattern-compiler)
    (affe-highlight-function . #'orderless--highlight)
    :config
    (consult-customize my/affe-grep :preview-key (kbd "M-."))
    )

  (leaf savehist-mode
    :custom
    `((savehist-file . ,(my/locate-user-emacs-data-file "history"))
      (savehist-additional-variables . '(search-ring regexp-search-ring)))
    :global-minor-mode t)
  )

(leaf corfu
  :ensure t
  :require t
  :custom
  (corfu-cycle . t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto . t)                 ;; Enable auto completion
  (corfu-auto-delay . 0.01)                 ;; Enable auto completion
  (corfu-count . 15)                        ;; show more candidates
  (corfu-auto-prefix . 3)
  (corfu-max-width . 150)               ;; max width of corfu completion UI
  (corfu-commit-predicate . #'my:corfu-commit-predicate)   ;; Do not commit selected candidates on next input
  (corfu-quit-no-match . 'separator)
  (corfu-separator . ?\s)
  (corfu-preview-current . t)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-echo-documentation . nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind
  (:corfu-map
   ("SPC" . corfu-insert-separator))
  :preface
  ;; from https://github.com/minad/corfu/wiki
  (defun my:corfu-commit-predicate ()
    "Auto-commit candidates if

  1. A \".\" is typed, except after a SPACE.
  2. A key is typed after corfu-complete
  3. A selection was made, aside from entering SPACE.
  "
    (cond
     ((seq-contains-p (this-command-keys-vector) ?.)
      (or (string-empty-p (car corfu--input))
          (not (string= (substring (car corfu--input) -1) " "))))

     ((equal last-command #'corfu-complete)
      (or (string-empty-p (car corfu--input))
          (not (string= (substring (car corfu--input) -1) " "))))

     ((/= corfu--index corfu--preselect) ; a selection was made
      (not (seq-contains-p (this-command-keys-vector) ? )))))
  :config
  (global-corfu-mode))

(leaf migemo
  :ensure t
  :custom
  `((migemo-dictionary . ,(expand-file-name "~/.config/migemo/migemo-dict"))
    (migemo-options . '("-q" "--emacs" "-s" ,(expand-file-name "~/.config/migemo/migemo-dict.jis3_4")))
    )
  :hook
  (my/emacs-launched-hook . migemo-init)
  )

(leaf undo-fu
  :ensure t
  :bind
  (("C-/" . undo-fu-only-undo)
   ("C-]" . undo-fu-only-redo))
  :custom
  ((undo-limit . 1000000)
   (undo-strong-limit . 1500000))
  )

(leaf epa-file
  :custom
  (epg-gpg-program . "gpg1"))

(leaf yaml-mode
  :ensure t
  )

(leaf markdown-mode
  :ensure t
  )

(leaf terraform-mode
  :ensure t
  )

(leaf dockerfile-mode
  :ensure t
  )

(leaf sequential-command
  :ensure t
  :hook
  (my/emacs-launched-hook . (lambda ()
                              (require 'sequential-command-config)
                              (sequential-command-setup-keys))))

(leaf popwin
  :ensure t
  :global-minor-mode t)

(leaf beacon
  :ensure t
  :blackout t
  :global-minor-mode t)

(leaf volatile-highlights
  :ensure t
  :after modus-themes
  :blackout t
  :config
  (volatile-highlights-mode t)
  (set-face-attribute 'vhl/default-face nil :inherit 'modus-themes-special-mild :background nil :foreground nil)
  )

(leaf symbol-overlay
  :ensure t
  :bind
  ("M-i" . symbol-overlay-put)
  ("M-I" . symbol-overlay-remove-all)
  ("<f2>" . symbol-overlay-query-replace))

(leaf anzu
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :custom
  ((anzu-use-migemo . t)
   )
  :global-minor-mode t
  )

(leaf region-bindings-mode
  :ensure t
  :require t
  :bind
  ((region-bindings-mode-map
    ("i" . mc/edit-lines)
    ))
  :config
  (region-bindings-mode-enable))

(leaf multiple-cursors
  :ensure t
  :custom
  ((mc/always-run-for-all . t))
  )

(leaf *mac
  :preface
  (defun my/mac-change-cursor-color-based-on-input-source ()
    (let ((mac-input-source (mac-input-source)))
      (set-cursor-color
       (if (string-match "com.apple.inputmethod.Kotoeri.Romaji" mac-input-source)
           "red"
         "white"))))
  (defun my/ime-off ()
    (mac-select-input-source "com.apple.keylayout.ABC"))
  :custom
  ((mac-option-modifier . 'alt))
  :config
  (mac-auto-ascii-mode 1)
  :hook
  (mac-selected-keyboard-input-source-change-hook . my/mac-change-cursor-color-based-on-input-source)
  (minibuffer-setup-hook . my/ime-off)
  )

;;; 起動後に後回しできる初期化処理をフックで呼ぶ
(run-with-idle-timer 0.5 nil (lambda () (run-hooks 'my/emacs-launched-hook)))

;; TODO: judge-indent

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
