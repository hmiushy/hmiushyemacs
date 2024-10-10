# My emacs setting
| Command | mean |
| ---- | ---- |
| C-j | change the buffer to left |
| C-o | change the buffer to right |
| C-q | change the another window |
  
# Install emacs-28
- Add repo
  ```bash
  sudo add-apt-repository ppa:kelleyk/emacs
  ```

- Update and install
  ```bash
  sudo apt-get update
  sudo apt-get install emacs28
  ```
  
- If already installed emacs,
  ```bash
  sudo update-alternatives --config emacs
  ```
  And switch to version 26.
- Bash script summary
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
- Additional
  - rainbow-mode
    ```bash
    (require 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'less-mode-hook 'rainbow-mode)
    (add-hook 'web-mode-hook 'rainbow-mode)
    (add-hook 'html-mode-hook 'rainbow-mode)
    (add-hook 'python-mode-hook 'rainbow-mode)
    ```
# Ubuntu-22.04インストール後
- アップデート&アップグレード後，再起動
  ```bash
  sudo apt update
  sudo apt upgrade
  sudo reboot -h now
  ```

- 日本語フォルダ名を英語に
  ```bash
  LANG=C xdg-user-dirs-gtk-update
  ```
  `/etc/default/locale`を開き変更
  ```bash
  # File generated by update-locale
  - LANG="ja_JP.UTF-8"
  +  LANG="en_US.UTF-8"
  + LANGUAGE="en_US:en"
  ```
  (コピーしやすいように↓)
  ```bash
  LANG="en_US.UTF-8"
  LANGUAGE="en_US:en"
  ```
- 句読点の変更<br>
  Mozc設定ツールをインストール
  ```
  sudo apt install mozc-utils-gui -y
  ```
  Mozc設定ツールをアクティビティから開いた後，簡単に変更可能

- xrdpインストール
  
  コマンドでインストール
  ```bash
  # XRDP系をインストール
  sudo apt install xrdp xserver-xorg-core xorgxrdp -y
  ```
  設定を反映 (RDPでログインしたとき，ターミナルの設定がいつもと違う場合などはこれを実行)
  ```
  # 設定1
  cat <<EOF > ~/.xsessionrc
  export GNOME_SHELL_SESSION_MODE=ubuntu
  export XDG_CURRENT_DESKTOP=ubuntu:GNOME
  export XDG_DATA_DIRS=/usr/share/ubuntu:/usr/local/share:/usr/share:/var/lib/snapd/desktop
  export XDG_CONFIG_DIRS=/etc/xdg/xdg-ubuntu:/etc/xdg
  EOF
  
  # 設定2
  sudo sed -e 's/^new_cursors=true/new_cursors=false/g' -i /etc/xrdp/xrdp.ini
  
  cat <<EOF | \
  sudo tee /etc/polkit-1/localauthority/50-local.d/xrdp-color-manager.pkla
  [Netowrkmanager]
  Identity=unix-user:*
  Action=org.freedesktop.color-manager.create-device
  ResultAny=no
  ResultInactive=no
  ResultActive=yes
  EOF
  
  sudo systemctl restart xrdp
  sudo systemctl restart polkit
  ```
  [参照] <a href="https://orenda.co.jp/blog/rdp-ubuntu%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%9F%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%89/">[windows + Linux]RDP + Ubuntuを使った開発環境構築</a><br>
  - xrdp接続時，画面が真っ暗な場合<br>
  同じユーザですでにUbuntuniログイン済みの場合，真っ暗な画面が表示される．<br>
  Ubuntu側で直接操作し一度ログアウトするか，SSH接続し以下のコマンドでセッションを終了させる．
    ```bash
    pkill gnome-session
    ```

  
- Emacs28をインストール
  ```bash
  # リポジトリ追加
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
- 日本語関係インストール?
  ```bash
  sudo apt install -y ibus-mozc
  sudo apt install -y mozc-utils-gui # (?)
  /usr/lib/mozc/mozc_tool --mode=config_dialog # setting
  ```
- matplotlibでTimes New Romanが使えないとき (Ubuntu)
  ```bash
  sudo apt install msttcorefonts -qq
  rm ~/.cache/matplotlib -rf
  ```
- フォント追加 (必要ないかも)
  ```bash
  sudo apt install -y emacs-mozc emacs-mozc-bin
  sudo apt install -y fonts-ipafont
  fc-cache -fv
  ```
- Mozcをemacs環境にインストール
  ```bash
  emacs &           ## Open emacs
  M-x list-packages ## M-x = Alt + x
  C-s mozc          ## C-s = Ctrl + s
  i                 ## Check 
  x                 ## Install
  ```
  
- init.elに追加
  ```bash
  (straight-use-package 'mozc) ;
  ;;; mozc
  (require 'mozc)                                 ; 
  (set-language-environment "Japanese")           ; 
  (setq default-input-method "japanese-mozc")     ; 
  (prefer-coding-system 'utf-8)                   ;
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  ```

- 日本語フォルダ名を英語に
  ```bash
  LANG=C xdg-user-dirs-gtk-update
  ```
  `/etc/default/locale`をOpen
  ```bash
  sudo emacs /etc/default/locale
  ```

  内容変更
  ```bash
  # File generated by update-locale
  - LANG="ja_JP.UTF-8"
  + LANG="en_US.UTF-8"
  + LANGUAGE="en_US:en"
  ```

# 機械学習環境設定
- ドライバ
  下記コマンドでRecommendedのドライバをインストール
  ```
  sudo add-apt-repository ppa:graphics-drivers/ppa
  sudo apt update
  ubuntu-drivers devices
  sudo apt install nvidia-driver-*
  sudo reboot -h now
  ```
- CUDA toolkit
  `nvidia-smi`コマンドでCUDA Versionを調べ，<a href="https://developer.nvidia.com/cuda-toolkit-archive">ここ</a>でそのバージョンを見つけサイトに従いインストール<br>
  パスを通す
  ```bash
  # .bashrcに追加
  # *をバージョンに合わせて変更する
  export PATH=/usr/local/cuda/bin:${PATH} >> .bashrc
  export LD_LIBRARY_PATH=/usr/local/cuda/lib64:/usr/local/cuda-*/lib64:${LD_LIBRARY_PATH} >> .bashrc
  ```
- cuDNN
  <a href="https://developer.nvidia.com/cudnn-downloads">サイト</a>の手順に従う


# Ubuntuにpyenvをインストール
- 依存関係インストール
  ```bash
  sudo apt-get install -y build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev git
  sudo apt-get install -y libfreeimage3 libfreeimage-dev # for "FreeImage.h"
  ```

- gitでインストール
  ```bash
  git clone https://github.com/pyenv/pyenv.git ~/.pyenv
  ```

- 環境変数の設定，反映
  ```
  echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc
  echo 'command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc
  echo 'eval "$(pyenv init -)"' >> ~/.bashrc
  . .bashrc
  ```

- python系を調べ，インストール，反映
   ```
   pyenv install --list
   pyenv install 3.8.6 # "3.8.6"は任意のバージョン
   pyenv global 3.8.6
   ```
   
  <a href="https://qiita.com/middle_aged_rookie_programmer/items/0eb574e92a52c923e7ec">Ubuntuにpyenvをインストールする</a><br>

- venvで仮想環境
  ```
  ## Setup python venv
  python -m venv .venv
  ## Activeate
  . .venv/bin/activate

  ## Install packages
  pip install -r requirements.txt
  # or
  pip install "YouWnatInstallPack"
  ```

- Test (GPU)
  
  <a href="https://docs.nvidia.com/deeplearning/cudnn/latest/installation/linux.html">ここ</a>に従いテストしてみる


# Yatex
- 端末を開いてコマンドでインストール
  ```bash
  sudo apt install -y texlive-lang-japanese  texlive-latex-extra xdvik-ja evince
  sudo apt install -y yatex
  ```
  
- `init.el`に追加
  ```
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

# Memo
  <a href="https://qiita.com/reoring/items/506399b8489517c1129f">Ubuntuでnvidiaのエラーが出たときのなおしかた</a><br>
- Memo > WSL <br>
  <a href="https://learn.microsoft.com/ja-jp/windows/wsl/tutorials/gpu-compute">WSL での ML の GPU アクセラレーションの概要</a> <br>
  <a href="https://qiita.com/nujust/items/d7cd395baa0c5dc94fc5">Ubuntu on WSL2でのDocker Engineの最短インストール手順</a>
- Memo > Ubuntu-18.04 <br>
  <a href="https://qiita.com/ReoNagai/items/bafeceab77642ca9bc9e">Geforce RTX2080 SUPER を搭載したUbuntu18.04のPCでCUDA・Nvida-Driver・cuDNNの環境を整える</a><br>
  <a href="https://developer.nvidia.com/cuda-10.0-download-archive?target_os=Linux&target_arch=x86_64&target_distro=Ubuntu&target_version=1804&target_type=deblocal">CUDA Toolkit 10.0 Download</a><br>
  <a href="https://shirowanisan.com/entry/2020/11/13/224908#google_vignette">Ubuntu18.04のインストールからGPUで機械学習をするまで</a>
- Memo > Ubuntu-22.04 <br>
- Memo > MATLAB <br>
  <a href="https://jp.mathworks.com/matlabcentral/answers/1619455-matlab-unable-to-install-r2021b-unable-to-write-to-selected-folder-in-ubuntu-20-04"> Matlab unable to install R2021b: "unable to write to selected folder" in Ubuntu 20.04 </a><br>
- Install xrdp to Ubuntu-22.04 <br>
  <a href="https://orenda.co.jp/blog/rdp-ubuntu%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%9F%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%89/">[windows + Linux]RDP + Ubuntuを使った開発環境構築</a><br>
  <a href="https://qiita.com/koba-jon/items/019a3b4eac4f60ca89c9">Ubuntu 20.04 LTS インストール方法（外付けドライブ用）</a>


# Error memo
  - Emacs: `package--check-signature: Failed to verify signature 
    ascii-art-to-unicode-1.9.el.sig: ("No public key 
    for 474F05837FBDEF9B created at 2014-09-24T16:20:01+0200 
    using DSA")` <br>
     [ref](https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure)
     ```bash
      # Emacs
      1. `M-: (setq package-check-signature nil)` RET.
      2. `M-x package-install` RET `gnu-elpa-keyring-update` RET.
      3. `M-: (setq package-check-signature 'allow-unsigned)` RET.
     ```
  - `sudo apt update`時のエラー <br>
    [ref](https://superuser.com/questions/1697045/some-index-files-failed-to-download-they-have-been-ignored-or-old-ones-used-in)
    ```bash
    ## Error > memo <br>
    cp /etc/resolv.conf ./ # backup
    # nameserver 8.8.8.8
    # nameserver 8.8.4.4
    # nameserver 1.1.1.1
    ```

  - Error Memo > error `W : Target * is configured multiple times *` <br>
    - memo [ref](https://askubuntu.com/questions/760896/how-can-i-fix-apt-error-w-target-packages-is-configured-multiple-times) <br>
      ```bash
      sudo apt install python3-apt
      wget https://github.com/davidfoerster/aptsources-cleanup/releases/tag/v0.1.7.5.2/aptsources-cleanup.pyz
      chmod a+x aptsources-cleanup.pyz
      sudo ./aptsources-cleanup.pyz
      ```
    - outdated solution <br>
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
- MatplotlibでTime New Romanなどが使えないとき
  ```
  sudo apt install msttcorefonts -qq
  rm ~/.cache/matplotlib -rf
  ```
