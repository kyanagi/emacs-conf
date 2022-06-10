;; 起動中はMagicファイル名を無効にする
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'window-setup-hook
          (lambda ()
            (setq file-name-handler-alist my/saved-file-name-handler-alist)
            (makunbound 'my/saved-file-name-handler-alist)))

;; 起動中はGCしない
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

(menu-bar-mode -1)
(tool-bar-mode -1)
