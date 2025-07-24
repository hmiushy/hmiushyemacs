# 目次
1. [Install emacs-28](#emacs28)
2. [Ubuntu-22.04インストール後にやるといいこと](#ubuntu22)
3. [フォント関係(emacsの設定含む)](#font)
4. [機械学習環境設定](#ml)
5. [pyenvとvenv](#pyenv)
6. [Yatexの設定](#yatex)
7. [Ubuntuの見た目](#folder)
8. [Memo](#memo)


# My emacs setting
| Command | mean |
| ---- | ---- |
| C-j | change the buffer to left |
| C-o | change the buffer to right |
| C-q | change the another window |

<a id="emacs28"></a>
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
  And switch to version 2x.
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
- [ほかの方法](https://www.kkaneko.jp/tools/win/emacs.html)
  [ここ](https://www.gnu.org/software/emacs/download.html)の`nearby GNU mirror`からインストールしたいemacsのバージョンをダウンロード
  ```
  wget https://ftp.kaist.ac.kr/gnu/emacs/emacs-29.4.tar.gz # バージョンは好きなやつ
  ```
  ファイルを解凍
  ```
  # ファイル名は適宜変更
  tar zxf emacs-29.4.tar.gz
  ```
  必要なパッケージをインストールしておく
  ```
  sudo apt install texinfo libtinfo-dev pkg-config libgnutls28-dev
  sudo apt install build-essential libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff-dev libncurses-dev
  ```
  ビルド
  ```
  cd emacs-29.4
  sudo ./configure
  sudo make
  sudo make install
  ```
- Additional
  - rainbow-mode (エディタ上のカラーコードにその色を付ける) <br>
    `M-x package-list-packages RET, C-s rainbow-mode, x, i`
    ```bash
    (require 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'less-mode-hook 'rainbow-mode)
    (add-hook 'web-mode-hook 'rainbow-mode)
    (add-hook 'html-mode-hook 'rainbow-mode)
    (add-hook 'python-mode-hook 'rainbow-mode)
    ```
  - Error and Solution
    - Error  
    ```
    home@home:~$ sudo apt install emacs28 -y
    Reading package lists... Done
    Building dependency tree       
    Reading state information... Done
    The following packages were automatically installed and are no longer required:
      linux-image-5.4.0-189-generic linux-modules-5.4.0-189-generic
      linux-modules-extra-5.4.0-189-generic
    Use 'sudo apt autoremove' to remove them.
    The following NEW packages will be installed:
      emacs28
    0 upgraded, 1 newly installed, 0 to remove and 0 not upgraded.
    Need to get 0 B/22.5 MB of archives.
    After this operation, 99.2 MB of additional disk space will be used.
    (Reading database ... 220826 files and directories currently installed.)
    Preparing to unpack .../emacs28_28.1~1.git5a223c7f2e-kk3+20.04_amd64.deb ...
    Unpacking emacs28 (28.1~1.git5a223c7f2e-kk3+20.04) ...
    dpkg: error processing archive /var/cache/apt/archives/emacs28_28.1~1.git5a223c7f2e-kk3+20.04_amd64.deb (--unpack):
     trying to overwrite '/usr/include/emacs-module.h', which is also in package emacs-common 1:26.3+1-1ubuntu2
    dpkg-deb: error: paste subprocess was killed by signal (Broken pipe)
    Errors were encountered while processing:
     /var/cache/apt/archives/emacs28_28.1~1.git5a223c7f2e-kk3+20.04_amd64.deb
    E: Sub-process /usr/bin/dpkg returned an error code (1)
    ```
    - Solution
    ```
    sudo apt remove --autoremove emacs emacs-common
    sudo apt install emacs28 -y
    ```
- UbuntuではなくDebianでemacs27をインストールする場合
  ```bash
  sudo vim /etc/apt/sources.list
  ```
  以下を追加
  ```
  Deb http://deb.debian.org/debian bullseye-backports main
  ```
  以下を実行
  ```
  sudo apt update
  sudo apt -t bullseye-backports install emacs
  ```
  emacsの設定 (~/.emacs.d/init.elに以下を追加)
  ```
  ;; Maximize the frame
  (toggle-frame-maximized)
 
  ;; setting to C-h is backspace
  (keyboard-translate ?\C-h ?\C-?)
  
  ;; the above definition may not have been defined within Mini buffer
  (global-set-key "\C-h" nil)
  
  ;; setting of windowsize
  ;; (setq default-frame-alist (append (list '(cursor-color . "purple"))
  ;;                default-frame-alist))

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
  (setq windmove-wrap-around t)
  ;;(define-key global-map (kbd "C-z") 'eshell)
  (define-key global-map (kbd "C-j") 'switch-to-next-buffer)
  (define-key global-map (kbd "C-o") 'switch-to-prev-buffer)
  (define-key global-map (kbd "C-q") 'other-window)
  ```
  - 古い鍵の削除
  ```bash
  ## 任意のIPを指定
  ssh-keygen -R 172.28.234.223
  ```
<a id="ubuntu22"></a>    
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
<a id="font"></a>
# mozc，フォント関連
- 日本語関係インストール?
  ```bash
  sudo apt install -y ibus-mozc
  sudo apt install -y mozc-utils-gui # (?)
  /usr/lib/mozc/mozc_tool --mode=config_dialog # setting
  ```
  WSLを使用するとき、日本語が文字化け：対応
  ```bash
  sudo apt install fonts-noto-cjk fonts-ipafont-gothic fonts-ipafont-mincho
  ```
  これを実行したらフォントが正しく表示された。
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
- PythonのMatplotlibにおいて，凡例内のフォントなどを指定 (Memo)
  ```
  ...
  fig.legend(prop={'style':'italic', 'family':'Times New Roman'})
  ...
  ```
<a id="ml"></a>
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

<a id="pyenv"></a>
# Ubuntuにpyenvをインストール
最近だと，uvがいいかも．一応，過去の環境構築方法をメモ．
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
  . ~/.bashrc
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
  
  ## Upgrade pip
  pythom -m pip install --upgrade pip
  ## Install packages
  pip install -r requirements.txt
  # or
  pip install "PacksYouWantoInstall"
  ```

- Test (GPU)
  
  <a href="https://docs.nvidia.com/deeplearning/cudnn/latest/installation/linux.html">ここ</a>に従いテストしてみる

<a id="yatex"></a>
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
<a id="folder"></a>
# Ubuntuの見た目が変化してしまった

### **1. テーマやアイコンが削除されたか確認**
#### **影響を受ける可能性があるパッケージ**
- `yaru-theme-gtk`
- `yaru-theme-icon`
- `yaru-theme-sound`
- `gnome-themes-extra`
- `gnome-themes-extra-data`
- `gnome-accessibility-themes`
- `gtk2-engines-murrine`
- `gtk2-engines-pixbuf`
  
**影響:**  
- デスクトップ環境のアイコンやテーマが変わる可能性
- 例えば、ウィンドウのボタン（閉じる、最小化など）やフォルダのアイコンが変化する可能性

---

### **2. フォント関連のパッケージが削除**
#### **影響を受ける可能性があるパッケージ**
- `fonts-kacst`
- `fonts-kacst-one`
- `fonts-khmeros-core`
- `fonts-lao`
- `fonts-lklug-sinhala`
- `fonts-sil-abyssinica`
- `fonts-thai-tlwg`
- `fonts-tibetan-machine`

**影響:**  
- 削除されたフォントを使っていた場合、文字の表示が変わる可能性
- 一部の言語（アラビア語、クメール語、ラオス語、チベット語など）のフォントが削除されたため、それらの言語が表示されなくなる、または違うフォントに置き換わる可能性

---

### **3. Nautilus（ファイルマネージャ）関連のパッケージが削除**
#### **影響を受ける可能性があるパッケージ**
- `nautilus-share`（Nautilusのファイル共有機能）
- `baobab`（ディスク使用状況を可視化するツール）

**影響:**  
- Nautilus（ファイルマネージャ）でのファイル共有機能が使えなくなる可能性
- `baobab` が削除されたため、ディスク使用状況を視覚的に確認するツールがなくなる可能性

---

### **4. GNOMEアプリやツールが削除**
#### **影響を受ける可能性があるパッケージ**
- `gnome-calculator`（電卓）
- `gnome-calendar`（カレンダー）
- `gnome-characters`（特殊文字入力ツール）
- `gnome-disk-utility`（ディスク管理ツール）
- `gnome-font-viewer`（フォントビューア）
- `gnome-mahjongg`（ゲーム）
- `gnome-mines`（ゲーム）
- `gnome-power-manager`（電源管理）
- `gnome-sudoku`（ゲーム）
- `gnome-system-monitor`（タスクマネージャ）
- `gnome-todo`（ToDoリスト管理）

**影響:**  
- GNOMEデスクトップ環境を使っている場合、一部のアプリが消えたことで、機能の一部が利用不可能に
- `gnome-system-monitor` が削除されたため、プロセスの管理（タスクマネージャ的な操作）ができなくなる
- `gnome-disk-utility` の削除で、USBやHDDのフォーマットやディスクの情報をGUIで確認不可能に

---

### **5. LibreOffice関連の削除**
#### **影響を受ける可能性があるパッケージ**
- `libreoffice-calc`（Excelのような表計算）
- `libreoffice-draw`（図形描画）
- `libreoffice-gnome`
- `libreoffice-gtk3`
- `libreoffice-impress`（PowerPointのようなプレゼンテーションツール）
- `libreoffice-ogltrans`
- `libreoffice-pdfimport`

**影響:**  
- LibreOffice関連のアプリがすべて削除された可能性
- デスクトップ環境の統合（GTK3対応）がなくなり、他のオフィスソフトと見た目が異なる可能性

---

### **6. リズムボックス（Rhythmbox）やオーディオ関連の削除**
#### **影響を受ける可能性があるパッケージ**
- `rhythmbox`（音楽プレイヤー）
- `rhythmbox-data`
- `rhythmbox-plugin-alternative-toolbar`
- `rhythmbox-plugins`

**影響:**  
- 音楽プレイヤーが削除され、音楽ファイルの再生が不便に
- GNOMEのメディアキー（再生/一時停止ボタンなど）が機能しなくなる可能性

---

### **7. その他、影響がありそうなパッケージ**
- `usb-creator-gtk` → UbuntuのUSB作成ツール
- `simple-scan` → スキャナアプリ（スキャナを使う場合影響あり）
- `seahorse` → GNOMEのパスワード管理ツール
- `shotwell` → 画像ビューア・管理ソフト
- `transmission-common` → BitTorrentクライアント
- `fwupd`（ファームウェア更新ツール）が削除されたため、一部のハードウェアのファームウェア更新ができなくなる可能性がある。

---

**対策**
- `yaru-theme-gtk`, `yaru-theme-icon`, `gnome-themes-extra` などを再インストール
- `gnome-tweaks` をインストールし、テーマやアイコンを変更
- `sudo apt install --reinstall gnome-themes-extra yaru-theme-gtk yaru-theme-icon fonts-kacst fonts-lao` などで一部の削除されたパッケージを復元できる可能性

フォルダの見た目が変わってしまった場合、これらを試す

<a id="memo"></a>
# Memo
- Tex関係
  <a href=https://qiita.com/shohirose/items/52f778ebd21f8e5f5c0e>LaTeXにおいて体裁を整える</a><br>
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
  ```bash
  ## Error: Authorization required, but no authorization protocol specified
  xhost +SI:localuser:root # これだけ
  sudo ./install
  ```
  WSLでmatlabをインストール
  ```
  #.isoファイルをコピー
  # マウント用のフォルダ作成
  mkdir ./mnt_iso
  # マウント & 移動
  sudo mount -o loop ./R2024b_Linux.iso ./mnt_iso
  cd mnt_iso
  
  # GUI操作を可能にするライブラリ等をインストール
  sudo apt update
  sudo apt install libnss3 libxss1 libasound2 libatk1.0-0 libgtk-3-0 \
  libxcomposite1 libxrandr2 libgbm1 libpangocairo-1.0-0 libglu1-mesa
  # インストール
  sudo ./install # これでインストーラが起動
  ```
- Install xrdp to Ubuntu-22.04 <br>
  <a href="https://orenda.co.jp/blog/rdp-ubuntu%E3%82%92%E4%BD%BF%E3%81%A3%E3%81%9F%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%89/">[windows + Linux]RDP + Ubuntuを使った開発環境構築</a><br>
  <a href="https://qiita.com/koba-jon/items/019a3b4eac4f60ca89c9">Ubuntu 20.04 LTS インストール方法（外付けドライブ用）</a>
- Windows apache <br>
  リスタート: `httpd -k restart`
- WindowsでNASを自動でドライブ割当<br>
  <a href="https://www.77-lifework.com/entry/nas-startup">参照</a> </br>
  - コード作成
    Zドライブにホスト192.168.100.100のshareフォルダを割り当てる場合
    ```bash
    On Error Resume Next
    WScript.sleep 5000
    Set objNetwork = CreateObject("WScript.Network")
    objNetWork.MapNetworkDrive"Z:","\\192.168.100.100\share"
    ```
    以上のコードを".vbs"拡張子で保存，実行．スタートアップに登録しておくと起動時に自動で実行する．<\br>
  - スタートアップ登録</br>
    1. `Windows + R` で"ファイル名を指定して実行"</br>
    2. `shell:startup`と入力しEnter</br>
    3. フォルダが開くのでそこに先ほど作成した"*.vbs"ファイルを配置
    4. 終わり
       
- **Ubuntu Desktop**<br>
  同じUbuntuでも，異なるデスクトップ環境（Desktop Environment）を使用すると，起動時のデスクトップやファイルシステムの見た目，操作感が異なることがある．
  デスクトップ環境は，ウィンドウマネージャーやシステムアプリケーション，ファイルマネージャーなどを含む一連のツールの集合で，ユーザーのインターフェースを構成する．

  ###  よく使われるデスクトップ環境
    - **GNOME**: Ubuntuのデフォルトのデスクトップ環境．モダンで直感的なインターフェースが特徴．
    - **KDE Plasma**: カスタマイズ性が高く．多機能なデスクトップ環境．
    - **Xfce**: 軽量でシンプルなデスクトップ環境．古いハードウェアでも動作が軽快．
    - **LXQt**: 非常に軽量なデスクトップ環境．低スペックのハードウェアに最適．
    - **Cinnamon**: Linux Mintから派生した．使いやすさとカスタマイズ性のバランスが取れたデスクトップ環境．

  ### 確認方法
  使用しているデスクトップ環境の確認
    ```sh
      echo $XDG_CURRENT_DESKTOP
    ```

  ### 変更方法
  デスクトップ環境を変更したい場合は，以下の手順を参考に：
  1. **新しいデスクトップ環境をインストール**：
       ```sh
       sudo apt update
       sudo apt install ubuntu-desktop
       ```

  2. **ログイン画面で選択**：
    - ログアウトし，ログイン画面でユーザー名を選択した後，歯車アイコンをクリックしてインストールしたデスクトップ環境を選択．
    
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
