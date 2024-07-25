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
