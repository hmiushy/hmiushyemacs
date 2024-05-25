# My emacs setting
| Command | mean |
| ---- | ---- |
| C-j | change the buffer to left |
| C-o | change the buffer to right |
| C-q | change the another window |
## If installed emacs24, Getting emacs25 or something
1. add repo
```bash
sudo add-apt-repository ppa:kelleyk/emacs
```
2. update and install
```bash
sudo apt-get update
sudo apt-get install emacs25 # (or emacs2*)
```
## If already installed emacs,
```bash
sudo update-alternatives --config emacs
```
And switch to version 25.
