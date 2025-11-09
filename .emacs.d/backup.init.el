;;; package --- Summary
;;; Commentary:
;;

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

(require 'package)
(autoload 'package-run "package")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://mail.gnu.org/archive/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

;;(package-initialize)
(with-eval-after-load 'package (package-initialize))

;; Package manager
;; Run Installation and initialization by writing these codes
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'leaf)
(straight-use-package 'popwin)
(straight-use-package 'yasnippet)
(straight-use-package 'ido-vertical-mode)
(straight-use-package 'zenburn-theme)
(straight-use-package 'auto-complete)
(straight-use-package 'xcscope)
(require 'auto-complete-config)
(ac-config-default)
;; Research completion
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; ;; Display the clock and file name on title
(when (window-system)
  ;; this needs before display-time
  (setq display-time-string-forms
    '((format "%s/%s/%s" year month day)
      (format "(%s:%s)" 24-hours minutes)))
  (display-time) ;; valid display-time-string
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


;; buffer no sounds
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

;; https://github.com/emacs-jp/emacs-jp.github.com/issues/38
(setq-default indicate-empty-lines t)
(when (require 'hiwin nil t)
  (hiwin-activate)                            ;; valid hiwin-mode
(set-face-background 'hiwin-face "gray10"))  ;; decide the color non-valid buffers

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(setq mozc-candidate-style 'echo-area)
(setq skk-show-annotation nil)


;; dont make buckup files etc
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq
;; the number of scroll lines you can decide 
mouse-wheel-scroll-amount '(5 ((shift) . 2) ((control)))
;; ignore the speed
mouse-wheel-progressive-speed nil)

(load "~/.emacs.d/p4-mode.el")

;; good vibes the color of window
(load-theme 'zenburn t)
(set-face-attribute 'highlight nil :foreground 'unspecified)


;;; 複数行移動
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;;
;; Mozc
;; 
;; First, Install mozc by command to use Japanese
;; sudo apt install -y mozc-ibus
;; sudo apt install -y mozc-utils-gui (?)
;; /usr/lib/mozc/mozc_tool --mode=config_dialog # setting

(require 'mozc)                                 ; 
(set-language-environment "Japanese")           ; 
(setq default-input-method "japanese-mozc")     ; 
(prefer-coding-system 'utf-8)                   ;
(global-set-key (kbd "C-\\") 'toggle-input-method)





(load "~/.emacs.d/mytex.el")






;; ここが肝：C-c j（= typeset）で latexmk を実行
(setq tex-command "latexmk -pdfdvi")   ;; .latexmkrcの設定でuplatex→dvipdfmx→PDF

;; ビューア（PDFを開く）
(setq dvi2-command "xdg-open")   ;; C-c v で xdg-open <生成物> を開く


;;; 一行が 100 字以上になった時には自動改行する
(setq fill-column 100)
(setq text-mode-hook 'turn-on-auto-fill)
(setq yatex-mode-hook 'turn-on-auto-fill)
(setq default-major-mode 'text-mode)


(setq windmove-wrap-around t)
;;(define-key global-map (kbd "C-z") 'eshell)
(define-key global-map (kbd "C-j") 'switch-to-next-buffer)
(define-key global-map (kbd "C-o") 'switch-to-prev-buffer)
(define-key global-map (kbd "C-q") 'other-window)

(global-set-key (kbd "C-c f") 'fill-region)
(defun fill-whole-buffer ()
  "Fill the entire buffer."
  (interactive)
  (fill-region (point-min) (point-max)))

(global-set-key (kbd "C-c F") 'fill-whole-buffer)


(defvar my-bufnav-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-j") #'switch-to-next-buffer)
    (define-key m (kbd "C-o") #'switch-to-prev-buffer)
    m)
  "Keymap for `my-bufnav-mode'.")

(define-minor-mode my-bufnav-mode
  "Force C-j/C-o to switch buffers everywhere."
  :init-value t :lighter "" :keymap my-bufnav-mode-map)

(my-bufnav-mode 1)

;; ミニバッファでは邪魔しない（任意）
(add-hook 'minibuffer-setup-hook (lambda () (my-bufnav-mode -1)))
(add-hook 'minibuffer-exit-hook  (lambda () (my-bufnav-mode  1)))
