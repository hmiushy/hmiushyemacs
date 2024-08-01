# My emacs setting
| Command | mean |
| ---- | ---- |
| C-j | change the buffer to left |
| C-o | change the buffer to right |
| C-q | change the another window |
## Requirement?
- Flycheck: emacs26
- [straight](https://github.com/radian-software/straight.el): emacs25?
## If installed emacs24, Getting emacs26 or something
1. add repo
```bash
sudo add-apt-repository ppa:kelleyk/emacs
```
2. update and install
```bash
sudo apt-get update
sudo apt-get install emacs26 # (or emacs2*)
```
## If already installed emacs,
```bash
sudo update-alternatives --config emacs
```
And switch to version 26.
## Bash script
```bash
#!/bin/bash
set -eu

## Get emacs28
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt-get update
sudo apt-get install emacs28 -y

## Get original setting by using git
sudo apt install git -y
git clone https://github.com/hmiushy/hmiushyemacs
cp -r hmiushyemacs/.emacs.d $HOME
emacs &
rm -rf hmiushyemacs
```
# Memo of error
sudo apt update <br>
[ref](https://superuser.com/questions/1697045/some-index-files-failed-to-download-they-have-been-ignored-or-old-ones-used-in)
```bash
## memo
cp /etc/resolv.conf ./ # backup
# nameserver 8.8.8.8
# nameserver 8.8.4.4
# nameserver 1.1.1.1
```

## Memo: error `W : Target * is configured multiple times *`
### memo [ref](https://askubuntu.com/questions/760896/how-can-i-fix-apt-error-w-target-packages-is-configured-multiple-times)
```bash
sudo apt install python3-apt
wget https://github.com/davidfoerster/aptsources-cleanup/releases/tag/v0.1.7.5.2/aptsources-cleanup.pyz
chmod a+x aptsources-cleanup.pyz
sudo ./aptsources-cleanup.pyz
```
### outdated solution
Crate `apt-remove-duplicate-source-entries.py` and run `sudo python3 apt-remove-duplicate-source-entries.py`.
```bash
cat << EOF > apt-remove-duplicate-source-entries.py
#!/usr/bin/python3
"""
Detects and interactively deactivates duplicate Apt source entries.

Usage: sudo python3 apt-remove-duplicate-source-entries.py
"""

from __future__ import print_function
import aptsources.sourceslist

EMPTY_COMPONENT_LIST = (None,)

def get_duplicates(sourceslist):
    """
    Detects and returns duplicate Apt source entries.
    """

    sentry_map = dict()
    duplicates = list()
    for se in sourceslist.list:
        if not se.invalid and not se.disabled:
            for c in (se.comps or EMPTY_COMPONENT_LIST):
                key = (se.type, se.uri, se.dist, c)
                previous_se = sentry_map.setdefault(key, se)
                if previous_se is not se:
                    duplicates.append((se, previous_se))
                    break

    return duplicates


if __name__ == '__main__':
    try:
        input = raw_input
    except NameError:
        pass

    sourceslist = aptsources.sourceslist.SourcesList(False)
    duplicates = get_duplicates(sourceslist)

    if duplicates:
        for dupe, orig in duplicates:
            print(
                'Overlapping source entries:\n'
                '  1. {0}: {1}\n'
                '  2. {2}: {3}\n'
                'I disabled the latter entry.'.format(
                    orig.file, orig, dupe.file, dupe),
                end='\n\n')
            dupe.disabled = True

        print('\n{0} source entries were disabled:'.format(len(duplicates)),
            *[dupe for dupe, orig in duplicates], sep='\n  ', end='\n\n')
        if input('Do you want to save these changes? (y/N) ').upper() == 'Y':
            sourceslist.save()

    else:
        print('No duplicated entries were found.')
EOF
sudo python3 apt-remove-duplicate-source-entries.py 
```

## Memo: 日本語フォント関連
Ubuntuコマンド
```
sudo apt install -y emacs-mozc emacs-mozc-bin
sudo apt install -y fonts-ipafont
fc-cache -fv
```
init.elに追加
```
...
...
(straight-use-package 'mozc) ;
;;; mozc
(require 'mozc)                                 ; 
(set-language-environment "Japanese")           ; 
(setq default-input-method "japanese-mozc")     ; 
(prefer-coding-system 'utf-8)                   ;

(global-set-key (kbd "C-\\") 'toggle-input-method)
```

## mozc
```bash
sudo apt install -y ibus-mozc
sudo apt install -y mozc-utils-gui # (?)
/usr/lib/mozc/mozc_tool --mode=config_dialog # setting
```

## get_emacs.sh
```
#!/bin/bash
set -eu

## Get emacs28
sudo add-apt-repository ppa:kelleyk/emacs -y
sudo apt-get update
sudo apt-get install emacs28 -y

## Get original setting by using git
sudo apt install git -y
git clone https://github.com/hmiushy/hmiushyemacs
cp -r hmiushyemacs/.emacs.d $HOME
emacs &
rm -rf hmiushyemacs
```

## Yatex
Ubuntu
```
sudo apt install -y texlive-lang-japanese  texlive-latex-extra xdvik-ja evince
sudo apt install -y yatex
```
init.el
```
...
...
;;
;; YaTeX
;;
(straight-use-package 'yatex)
(add-to-list 'load-path "~/.emacs.d/straight/repos/yatex")
(load "~/.emacs.d/mytex.el")
(setq YaTeX-kanji-code 4   ; 1: SJIS, 2: JIS, 3: EUC, 4: UTF-8
      YaTeX-latex-message-code 'utf-8  ; 文字化けしないようにする
      )
```

mytex.el
```
;;
;; YaTeX
;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
;(setq tex-command "lualatex -synctex=1")
;(setq tex-command "latexmk")
;(setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/bibtexu %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/bibtexu %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
(setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/bibtexu %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/bibtexu %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq dvi2-command "xdg-open")
(setq dvi2-command "evince")
;(setq dvi2-command "atril")
;(setq dvi2-command "okular --unique")
;(setq dvi2-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq dvi2-command "qpdfview --unique")
;(setq dvi2-command "texworks")
;(setq dvi2-command "texstudio --pdf-viewer-only")
;(setq tex-pdfview-command "xdg-open")
(setq tex-pdfview-command "evince")
;(setq tex-pdfview-command "atril")
;(setq tex-pdfview-command "okular --unique")
;(setq tex-pdfview-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq tex-pdfview-command "qpdfview --unique")
;(setq tex-pdfview-command "texworks")
;(setq tex-pdfview-command "texstudio --pdf-viewer-only")
(setq dviprint-command-format "wine64 cmd /c start Acrobat.exe `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")
(with-eval-after-load 'yatexprc
  (defun YaTeX-preview-jump-line ()
    "Call jump-line function of various previewer on current main file"
    (interactive)
    (save-excursion
      (save-restriction
        (widen)
        (let*((pf (or YaTeX-parent-file
                      (save-excursion (YaTeX-visit-main t) (buffer-file-name))))
              (pdir (file-name-directory pf))
              (bnr (substring pf 0 (string-match "\\....$" pf)))
              ;(cf (file-relative-name (buffer-file-name) pdir))
              (cf (buffer-file-name)) ;2016-01-08
              (buffer (get-buffer-create " *preview-jump-line*"))
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
                             previewer bnr line (expand-file-name cf)))
                    )))
          (YaTeX-system cmd "jump-line" 'noask pdir))))))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))
;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
```
