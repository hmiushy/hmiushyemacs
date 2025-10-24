;;; mytex.el --- YaTeX setup: C-c t j/v, color on, per-buffer engines -*- lexical-binding: t; -*-

;; init.el 側は (load "~/.emacs.d/mytex.el") だけ。重複YaTeX設定は消してください。

;; 1) インストール & ロード
(when (require 'straight nil t)
  (straight-use-package 'yatex))
(add-to-list 'load-path "~/.emacs.d/straight/repos/yatex")
(require 'yatex)  ;; ← 先にロードして順序問題を潰す

;; 2) モード起動 & 基本
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(dolist (rx '("\\.tex\\'" "\\.ltx\\'" "\\.cls\\'" "\\.sty\\'" "\\.clo\\'" "\\.bbl\\'"))
  (add-to-list 'auto-mode-alist (cons rx 'yatex-mode)))

(setq YaTeX-kanji-code 4
      YaTeX-latex-message-code 'utf-8
      YaTeX-use-LaTeX2e t
      YaTeX-use-AMS-LaTeX t
      YaTeX-inhibit-prefix-letter t
      YaTeX-prefix "C-c t")  ;; ★ 前と同じ: C-c t

;; 3) 色（font-lock）を常に有効
(setq YaTeX-use-font-lock t)
(setq font-lock-maximum-decoration t)
(add-hook 'yatex-mode-hook #'turn-on-font-lock)
(global-font-lock-mode 1)

;; 4) バッファ単位のエンジン切替
(defvar-local my-tex-engine 'uplatex
  "uplatex | platex | lualatex | xelatex | pdflatex")

(defun my/yatex-apply-engine ()
  (interactive)
  (pcase my-tex-engine
    ('uplatex
     (setq-local tex-command  "latexmk -e '$latex=q/uplatex %O %S/' -e '$dvipdf=q/dvipdfmx %O %S/' -pdfdvi"
                 bibtex-command "upbibtex"
                 makeindex-command "mendex %s"
                 dvi2-command "evince %d.pdf"))
    ('platex
     (setq-local tex-command  "latexmk -e '$latex=q/platex %O %S/' -e '$dvipdf=q/dvipdfmx %O %S/' -pdfdvi"
                 bibtex-command "pbibtex"
                 makeindex-command "mendex %s"
                 dvi2-command "evince %d.pdf"))
    ('lualatex
     (setq-local tex-command  "latexmk -lualatex"
                 bibtex-command "bibtex"
                 makeindex-command "makeindex %s"
                 dvi2-command "evince %d.pdf"))
    ('xelatex
     (setq-local tex-command  "latexmk -xelatex"
                 bibtex-command "bibtex"
                 makeindex-command "makeindex %s"
                 dvi2-command "evince %d.pdf"))
    ('pdflatex
     (setq-local tex-command  "latexmk -pdf"
                 bibtex-command "bibtex"
                 makeindex-command "makeindex %s"
                 dvi2-command "evince %d.pdf"))))
(add-hook 'yatex-mode-hook #'my/yatex-apply-engine)

(defun my/yatex-choose-engine ()
  (interactive)
  (setq my-tex-engine
        (intern (completing-read "TeX engine: "
                                 '("uplatex" "platex" "lualatex" "xelatex" "pdflatex")
                                 nil t)))
  (my/yatex-apply-engine)
  (message "[YaTeX] Engine -> %s" my-tex-engine))

;; 5) キーバインド（前と同じ操作感）
(add-hook 'yatex-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t j") #'YaTeX-typeset-buffer) ;; コンパイル
            (local-set-key (kbd "C-c t v") #'YaTeX-view)           ;; プレビュー
            (local-set-key (kbd "C-c t e") #'my/yatex-choose-engine))) ;; エンジン切替

;; 6) プレビュー設定（PDF拡張子/ビューア/行ジャンプ）
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-pdfview-command "evince"
      dvi2-command "evince")

(with-eval-after-load 'yatexprc
  (defun YaTeX-preview-jump-line ()
    (interactive)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((pf (or YaTeX-parent-file
                       (save-excursion (YaTeX-visit-main t) (buffer-file-name))))
               (pdir (file-name-directory pf))
               (bnr (substring pf 0 (string-match "\\....$" pf)))
               (cf  (buffer-file-name))
               (line (count-lines (point-min) (point-end-of-line)))
               (previewer (YaTeX-preview-default-previewer))
               (cmd (cond
                     ((string-match "Skim" previewer)
                      (format "%s %d '%s.pdf' '%s'"
                              YaTeX-cmd-displayline line bnr cf))
                     ((string-match "evince" previewer)
                      (format "%s '%s.pdf' %d '%s'"
                              "fwdevince" bnr line cf))
                     ((string-match "sumatra" previewer)
                      (format "%s \"%s.pdf\" -forward-search \"%s\" %d"
                              previewer bnr cf line))
                     ((string-match "zathura" previewer)
                      (format "%s --synctex-forward '%d:0:%s' '%s.pdf'"
                              previewer line cf bnr))
                     ((string-match "qpdfview" previewer)
                      (format "%s '%s.pdf#src:%s:%d:0'"
                              previewer bnr cf line))
                     ((string-match "okular" previewer)
                      (format "%s '%s.pdf#src:%d %s'"
                              previewer bnr line (expand-file-name cf))))))
          (YaTeX-system cmd "jump-line" 'noask pdir))))))

;; 7) 便利フック
(add-hook 'yatex-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'yatex-mode-hook
          (lambda ()
            (reftex-mode 1)
            (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
            (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;;; --- failsafe: いつでも動く自前プレフィックス C-c t ---
;; YaTeXが未ロードでも、またキーがどこかで上書きされても動くようにする
(defvar my-yatex-prefix (make-sparse-keymap) "My YaTeX prefix map.")

(defun my/yatex--ensure-engine ()
  "エンジン設定を未適用のとき適用。"
  (when (and (boundp 'my-tex-engine)
             (eq major-mode 'yatex-mode))
    (my/yatex-apply-engine)))

(defun my/yatex-typeset ()
  "YaTeX がいればそれを使い、無ければ latexmk でタイプセット。"
  (interactive)
  (my/yatex--ensure-engine)
  (cond
   ((fboundp 'YaTeX-typeset-buffer)
    (call-interactively 'YaTeX-typeset-buffer))
   (t
    (save-buffer)
    (let ((cmd (or (and (boundp 'tex-command) tex-command)
                   "latexmk -pdf")))
      (compile (format "%s %s" cmd (shell-quote-argument (buffer-file-name))))))))

(defun my/yatex-view ()
  "YaTeX がいればそれを使い、無ければ evince で PDF を開く。"
  (interactive)
  (cond
   ((fboundp 'YaTeX-view)
    (call-interactively 'YaTeX-view))
   (t
    (let* ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
           (viewer (or (and (boundp 'tex-pdfview-command) tex-pdfview-command) "evince")))
      (when (file-exists-p pdf)
        (start-process "my-tex-view" nil viewer pdf))))))

(defun my/yatex-setup-prefix ()
  "C-c t を自前プレフィックスにして j/v/e を割り当てる。"
  (define-key my-yatex-prefix (kbd "j") #'my/yatex-typeset)       ;; C-c t j
  (define-key my-yatex-prefix (kbd "v") #'my/yatex-view)          ;; C-c t v
  (define-key my-yatex-prefix (kbd "e") #'my/yatex-choose-engine) ;; C-c t e
  (local-set-key (kbd "C-c t") my-yatex-prefix))

(add-hook 'yatex-mode-hook #'my/yatex-setup-prefix)
;; YaTeX じゃないテキストバッファでも使いたいなら↓を有効化
;; (add-hook 'text-mode-hook #'my/yatex-setup-prefix)

;;; --- Add mirror keymap: C-c C-t behaves same as C-c t ---
;; C-c t と同じメニューを C-c C-t にも割り当てる
(add-hook 'yatex-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-t") 'YaTeX-typeset-menu)))

(provide 'mytex)
;;; mytex.el ends here
