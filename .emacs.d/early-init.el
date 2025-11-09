;; package.el を完全停止（重複ロード防止）
(setq package-enable-at-startup nil)

;; （任意）起動高速化の定番
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))
