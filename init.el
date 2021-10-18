;;; -*- lexical-binding: t -*-

;;; references:
;;; https://emacs-jp.github.io/tips/emacs-in-2020
;;; https://uwabami.github.io/cc-env/Emacs.html
;;; https://www.grugrut.net/posts/my-emacs-init-el/



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

;; ここに設定を書く
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf startup
  :custom
  ((inhibit-startup-screen . t)
   (user-mail-address . "yanagi@shakenbu.org")
   (gc-cons-threshold . 5000000)
   ))

(leaf exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(leaf cus-start
  :preface
  (defun my/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

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
   ("M-s M-s" . my/switch-to-scratch)
   ("C-w" . my/kill-region-or-backward-delete-word)
   )

  :custom
  `((indent-tabs-mode . nil)
    (create-lockfiles . nil)
    (enable-recursive-minibuffers . t)
    (history-length . 1000)
    (history-delete-duplicates . t)
    )
  )

(leaf modus-themes
  :ensure t
  :custom
  (
   (modus-themes-syntax . 'green-strings)
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
          (swap-buffers-in-windows)))
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
  )

(leaf font
  :custom
  ((use-default-font-for-symbols . nil))

  :config
  (set-fontset-font nil '(#x00 . #x7f) (font-spec :family "Monaco" :size 14))
  (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Migu 1M" :size 16))
  (set-fontset-font nil '#x3000 (font-spec :family "HackGen Console" :size 15))
  )

(leaf display-settings
  :custom
  `((line-number-mode . t)
    (column-number-mode . t)
    (tool-bar-mode . nil)
    (menu-bar-mode . nil)
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
    ))

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
  )

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

(leaf indent
  :preface
  (defun my/yank-and-indent ()
    (interactive)
    (yank)
    (call-interactively 'indent-region))
  :bind
  (("C-M-'" . indent-region)
   ("C-M-y" . my/yank-and-indent)))

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

(leaf ruby
  :hook
  ((ruby-mode-hook . ruby-electric-mode)
   (ruby-mode-hook . (lambda ()
                       (remove-hook 'after-save-hook 'ruby-mode-set-encoding t))))
  :config
  (leaf ruby-electric :ensure t)
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
  :config
  (setq-default header-line-format
                '((:eval
                   (let* ((interesting-buffers (iflipb-interesting-buffers))
                          (buffer-list (if (iflipb-first-iflipb-buffer-switch-command)
                                           interesting-buffers
                                         (append
                                          iflipb-saved-buffers
                                          (list (current-buffer))
                                          (nthcdr (1+ (length iflipb-saved-buffers)) interesting-buffers)
                                          ))))
                     (format-message "%s" (iflipb-format-buffers (current-buffer) buffer-list))
                     ))))
  )

(leaf *completion
  :config
  (leaf vertico
    :ensure t
    :custom
    (vertico-count . 20)
    :config
    (vertico-mode)
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
    :config
    (marginalia-mode)
    )

  (leaf consult
    :ensure t
    :bind
    (;("M-s" . consult-line)
     ("M-g" . consult-goto-line)
     ("C-x b" . consult-buffer)
     )
    )

  (leaf savehist-mode
    :custom
    `((savehist-file . ,(expand-file-name "~/var/emacs/history")))
    :config
    (savehist-mode))
  )

(leaf migemo
  :ensure t
  :require t
  :custom
  `((migemo-dictionary . ,(expand-file-name "~/var/migemo/migemo-dict"))
    )
  :config
  ;; 追加辞書
  (dolist (dict '("~/var/migemo/migemo-dict.jis3_4"))
    (setq dict (expand-file-name dict))
    (if (file-exists-p dict)
        (setq migemo-options (append migemo-options (list "-s" dict)))))
  (migemo-init)
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

(leaf sequential-command
  :ensure t
  :require sequential-command-config
  :config
  (sequential-command-setup-keys)
  )

;; (leaf scratch-ext
;;   :ensure t
;;   :require t
;;   )
(load "/Users/ani/c/scratch-ext-el/scratch-ext.el")

(leaf popwin
  :ensure t
  :config
  (popwin-mode))

(leaf beacon
  :ensure t
  :config
  (beacon-mode))

(leaf volatile-highlights
  :ensure t
  :after modus-themes
  :config
  (volatile-highlights-mode t)
  (set-face-attribute 'vhl/default-face nil :inherit 'modus-themes-special-mild :background nil :foreground nil)
  )

(leaf *mac
  :config
  (mac-auto-ascii-mode 1)
  )

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
