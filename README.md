# My emacs setting
| Command | mean |
| ---- | ---- |
| C-j | change the buffer to left |
| C-o | change the buffer to right |
| C-q | change the another window |
  
# Install emacs-28
1. add repo
```bash
sudo add-apt-repository ppa:kelleyk/emacs
```
2. update and install
```bash
sudo apt-get update
sudo apt-get install emacs28
```
## If already installed emacs,
```bash
sudo update-alternatives --config emacs
```
And switch to version 26.

## Bash script summary
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

# mozc，フォント関連
1. 日本語関係インストール?
```bash
sudo apt install -y ibus-mozc
sudo apt install -y mozc-utils-gui # (?)
/usr/lib/mozc/mozc_tool --mode=config_dialog # setting
```
2. フォント追加 (必要ないかも)
```bash
sudo apt install -y emacs-mozc emacs-mozc-bin
sudo apt install -y fonts-ipafont
fc-cache -fv
```
3. Mozcをemacs環境にインストール
```bash
emacs &           ## Open emacs
M-x list-packages ## M-x = Alt + x
C-s mozc          ## C-s = Ctrl + s
i                 ## Check 
x                 ## Install
```
4. init.elに追加
```bash
(straight-use-package 'mozc) ;
;;; mozc
(require 'mozc)                                 ; 
(set-language-environment "Japanese")           ; 
(setq default-input-method "japanese-mozc")     ; 
(prefer-coding-system 'utf-8)                   ;
(global-set-key (kbd "C-\\") 'toggle-input-method)
```
5. 日本語フォルダ名を英語に
```bash
LANG=C xdg-user-dirs-gtk-update
```

# Yatex
1. 端末を開いてコマンドでインストール
```bash
sudo apt install -y texlive-lang-japanese  texlive-latex-extra xdvik-ja evince
sudo apt install -y yatex
```
2. init.elについか
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

# 機械学習環境設定
1. ドライバ
下記コマンドでRecommendedのドライバをインストール
```
sudo add-apt-repository ppa:graphics-drivers/ppa
sudo apt update
ubuntu-drivers devices
sudo apt install nvidia-driver-*
sudo reboot -h now
```
2. CUDA toolkit
`nvidia-smi`コマンドでCUDA Versionを調べ，<a href="https://developer.nvidia.com/cuda-toolkit-archive">ここ</a>でそのバージョンを見つけサイトに従いインストール<br>
パスを通す
```bash
- `.bashrc`内
export PATH=/usr/local/cuda/bin:${PATH}
export LD_LIBRARY_PATH=/usr/local/cuda/lib64:/usr/local/cuda-*/lib64:${LD_LIBRARY_PATH}
```
3. cuDNN
<a href="https://developer.nvidia.com/cudnn-downloads">サイト</a>の手順に従う


# Ubuntuにpyenvをインストール
1. 依存関係インストール
```bash
sudo apt-get install -y build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev git
sudo apt-get install libfreeimage3 libfreeimage-dev # for "FreeImage.h"
```
2. gitでインストール
```bash
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
```
3. 環境変数の設定，反映
```
echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc
echo 'command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(pyenv init -)"' >> ~/.bashrc
. .bashrc
```
4. python環境構築など
   - python系を調べ，インストール，反映
   ```
   pyenv install --list
   pyenv install 3.8.6 # "3.8.6"はなんでもOK
   pyenv global 3.8.6
   ```
<a href="https://qiita.com/middle_aged_rookie_programmer/items/0eb574e92a52c923e7ec">Ubuntuにpyenvをインストールする</a>
5. Test
<a href="https://docs.nvidia.com/deeplearning/cudnn/latest/installation/linux.html">ここ</a>に従いテストしてみる



# Memo
<a href="https://qiita.com/reoring/items/506399b8489517c1129f">Ubuntuでnvidiaのエラーが出たときのなおしかた</a><br>
## WSL
<a href="https://learn.microsoft.com/ja-jp/windows/wsl/tutorials/gpu-compute">WSL での ML の GPU アクセラレーションの概要</a> <br>
<a href="https://qiita.com/nujust/items/d7cd395baa0c5dc94fc5">Ubuntu on WSL2でのDocker Engineの最短インストール手順</a>
## Ubuntu-18.04
<a href="https://qiita.com/ReoNagai/items/bafeceab77642ca9bc9e">Geforce RTX2080 SUPER を搭載したUbuntu18.04のPCでCUDA・Nvida-Driver・cuDNNの環境を整える</a><br>
<a href="https://developer.nvidia.com/cuda-10.0-download-archive?target_os=Linux&target_arch=x86_64&target_distro=Ubuntu&target_version=1804&target_type=deblocal">CUDA Toolkit 10.0 Download</a><br>
<a href="https://shirowanisan.com/entry/2020/11/13/224908#google_vignette">Ubuntu18.04のインストールからGPUで機械学習をするまで</a>
## Ubuntu-22.04
# MATLAB
<a href="https://jp.mathworks.com/matlabcentral/answers/1619455-matlab-unable-to-install-r2021b-unable-to-write-to-selected-folder-in-ubuntu-20-04"> Matlab unable to install R2021b: "unable to write to selected folder" in Ubuntu 20.04 </a><br>
## Install xrdp to Ubuntu-22.04
<a href="https://orenda.co.jp/blog/rdp-ubuntu%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%9F%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%89/">[windows + Linux]RDP + Ubuntuを使った開発環境構築</a><br>
<a href="https://qiita.com/koba-jon/items/019a3b4eac4f60ca89c9">Ubuntu 20.04 LTS インストール方法（外付けドライブ用）</a>


# Error memo
`sudo apt update`時のエラー <br>
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
