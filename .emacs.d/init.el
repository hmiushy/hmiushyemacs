;;; package --- Summary
;;; Commentary:
;;
(setq windmove-wrap-around t)
;;(define-key global-map (kbd "C-z") 'eshell)
(define-key global-map (kbd "C-j") 'switch-to-next-buffer)
(define-key global-map (kbd "C-o") 'switch-to-prev-buffer)
(define-key global-map (kbd "C-q") 'other-window)

;; Maximize the frame
(toggle-frame-maximized)

;; setting to C-h is backspace
(keyboard-translate ?\C-h ?\C-?)

;; the above definition may not have been defined within Mini buffer
(global-set-key "\C-h" nil)

;; setting of windowsize
(setq default-frame-alist (append (list '(cursor-color . "purple"))
				  default-frame-alist))

;; do not display the menu bar
(menu-bar-mode 0)

;; do not display startup messages
(setq inhibit-startup-message t)

;; display the name of a function at the current point
(which-function-mode 1)

;; set language as japanese
(set-language-environment 'Japanese)
;; coding UTF8
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;; automatically complete parantheses
(electric-pair-mode 1)

;; display full-path at the title bar
(setq frame-title-format "%f")

;; do not create backup files
(setq make-backup-files nil)
(setq make-save-default nil)

;; display the line num
(require 'linum)
(global-linum-mode 1)
;; light up the line
(global-hl-line-mode t)

;; high light
(require 'hl-line)
(set-face-background 'hl-line "white")

;; tab
(setq-default tab-width 4 indent-tabs-mode nil)


;; tab
;;(setq c-tab-always-indent nil)
;; c++?
;; (defun my-c-c++-mode-init()
;;   (setq c-basic-offset 4))

;;(load (expand-file-name "~/.emacs.d/init.el"))
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(package-selected-packages
;;   '(markdown-mode mozc eglot lsp-clients lsp-clisents company-lsp lsp-company company lsp-ui lsp-mode flycheck yatex leaf use-package)))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )


(require 'package)
(autoload 'package-run "package")

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

;;(package-initialize)
(with-eval-after-load 'package
  (package-initialize))

;; Package manager
;; Run Installation and initialization by writing these codes
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

(straight-use-package 'use-package)
(straight-use-package 'leaf)
(straight-use-package 'flycheck)
(straight-use-package 'company)
(straight-use-package 'popwin)
(straight-use-package 'yasnippet)
(straight-use-package 'ido-vertical-mode)
(straight-use-package 'flycheck-grammarly)

;; 検索の補完機能
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; ;; タイトルバーに時計とファイル名表示
(when (window-system)
  ;; display-timeより先にsetしておかないとdefaultの書式?になる workaround
  (setq display-time-string-forms
    '((format "%s/%s/%s" year month day)
      (format "(%s:%s)" 24-hours minutes)))
  (display-time) ;; display-time-stringの有効化
  ;; タイトルバーの書式設定 global-mode-stringにdisplay-time-stringが入っている
  ;; バッファがファイルのときはフルパス、でなければバッファ名表示
  ;; if(buffer-file-name) の評価がsetq時で終わらないよう:eval
  (setq frame-title-format '("" global-mode-string
                             (:eval (if (buffer-file-name) " %f" " %b"))) ) )


;;tree-undo
(when(require 'undo-tree nil t)
(global-undo-tree-mode))

;; alpha
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;;set language as japanese
(set-language-environment 'Japanese)
;;coding UTF8
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)


;;tab
(setq-default tab-width 4 indent-tabs-mode nil)


;; buffer の最後でカーソルを動かそうとしても音をならなくする
(defun next-line (arg)
  (interactive "p")
  (condition-case nil
      (line-move arg)
    (end-of-buffer)))

;; Terminal 化
(setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
(global-set-key (kbd "C-c o") 'shell-pop)

(add-hook 'c-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'c++-mode-hook (lambda () (setq tab-width 4)))


(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind (c-mode-base-map
         ("C-c c" . compile))
  ;; :mode-hook
  ;; (c-mode-hook . ((c-set-style "bsd")
  ;;                 (setq c-basic-offset 4)))
  ;; (c++-mode-hook . ((c-set-style "bsd")
  ;;                   (setq c-basic-offset 4)))
  )

(defun my-c-c++-mode-init ()
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)
;; paren
(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)
;;files
(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))
;; simple
(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))
;; startup
;; (leaf startup
;;   :doc "process Emacs shell arguments"
;;   :tag "builtin" "internal"
;;   :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))



;; ;; ;; flycheck
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-28.1"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

;; company (入力補完)
(leaf company
  :doc "Modular text completion framework"
  ;;:req "emacs-24.3"
  :req "emacs-28.1"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
;;  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

;;(add-to-list 'default-frame-alist '(background-color . "white"))

;; https://github.com/emacs-jp/emacs-jp.github.com/issues/38
(setq-default indicate-empty-lines t)  
(when (require 'hiwin nil t)
  (hiwin-activate)                            ;; hiwin-modeを有効化
(set-face-background 'hiwin-face "gray10"))  ;; 非アクティブバッファの背景色を設定

;; (when (require 'fill-column-indicator nil t)
;;   (setq fci-rule-color "gray")  ;; 縦線の色
;;   (define-globalized-minor-mode global-fci-mode fci-mode turn-on-fci-mode)
;;   (global-fci-mode)
;;   )
;; 2023 cmnt out ====================================================================================================
;; (when (string= system-type "darwin")
;;   (setq dired-use-ls-dired t
;;         insert-directory-program "/usr/local/bin/gls"
;;         dired-listing-switches "-aBhl --group-directories-first"))
;; 2023 cmnt out ====================================================================================================

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))


(leaf oj
  :ensure t
  :custom ((oj-default-online-judge . 'codeforces)))
(leaf oj
  :ensure t
  :custom ((oj-default-online-judge . 'atcoder)))

(leaf oj
  :doc "Competitive programming tools client for AtCoder, Codeforces"
  :req "emacs-26.1" "quickrun-2.2"
  :tag "convenience" "emacs>=26.1"
  :url "https://github.com/conao3/oj.el"
  :emacs>= 26.1
  :ensure t
  :custom ((oj-compiler-c . "gcc")
           (oj-compiler-python . "cpython")
           (oj-default-online-judge . 'atcoder)
           (oj-shell-program . "zsh")
           (oj-home-dir . "/home/hide/デスクトップ/myproject20220217/oj")
           ))


(setq mozc-candidate-style 'echo-area)
(setq skk-show-annotation nil)


;; flycheck
(use-package flycheck  
  :init
  (add-hook 'yatex-mode-hook 'flycheck-mode)
  :config
  (global-flycheck-mode t)
  )
;; flycheck-grammarly
(use-package flycheck-grammarly  
  :ensure t  
  :after flycheck  
  :config  
  (setq flycheck-grammarly-check-time 0.8)  
  (add-to-list 'flycheck-checkers 'grammarly))
(with-eval-after-load 'flycheck
  (flycheck-grammarly-setup))
(add-hook 'yatex-mode-hook 'flymake-grammarly-load)
