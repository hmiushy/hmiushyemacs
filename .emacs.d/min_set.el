1;; Maximize the frame
 2(toggle-frame-maximized)
 3
 4;; setting to C-h is backspace
 5(keyboard-translate ?\C-h ?\C-?)
 6
 7;; the above definition may not have been defined within Mini buffer
 8(global-set-key "\C-h" nil)
 9
10;; setting of windowsize
11;; (setq default-frame-alist (append (list '(cursor-color . "purple"))
12;;                default-frame-alist))
13
14;; do not display the menu bar
15(menu-bar-mode 0)
16
17;; do not display startup messages
18(setq inhibit-startup-message t)
19
20;; display the name of a function at the current point
21(which-function-mode 1)
22
23;; set language as japanese
24(set-language-environment 'Japanese)
25;; coding UTF8
26(set-language-environment 'utf-8)
27(prefer-coding-system 'utf-8)
28
29;; automatically complete parantheses
30(electric-pair-mode 1)
31
32;; display full-path at the title bar
33(setq frame-title-format "%f")
34
35;; do not create backup files
36(setq make-backup-files nil)
37(setq make-save-default nil)
38
39;; display the line num
40(require 'linum)
41(global-linum-mode 1)
42;; light up the line
43(global-hl-line-mode t)
44
45;; high light
46(require 'hl-line)
47(set-face-background 'hl-line "white")
48
49;; tab
50(setq-default tab-width 4 indent-tabs-mode nil)
51(setq windmove-wrap-around t)
52;;(define-key global-map (kbd "C-z") 'eshell)
53(define-key global-map (kbd "C-j") 'switch-to-next-buffer)
54(define-key global-map (kbd "C-o") 'switch-to-prev-buffer)
55(define-key global-map (kbd "C-q") 'other-window)
