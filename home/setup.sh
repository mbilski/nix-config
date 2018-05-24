if [[ $# -eq 0 ]] ; then
    echo 'usage: ./setup.sh <home_directory>'
    exit 1
fi

HOME=$1
PWD=`pwd`

# Emacs
mkdir -p $HOME/.emacs.d/
ln -fs $PWD/emacs/lisp $HOME/.emacs.d/lisp
ln -fs $PWD/emacs/init.el $HOME/.emacs.d/init.el

# Tmux
mkdir -p $HOME/.tmux
ln -fs $PWD/tmux/tmux.conf $HOME/.tmux.conf
ln -fs $PWD/tmux/theme $HOME/.tmux/basic.tmuxtheme

if [ ! -e "$HOME/.tmux/yank" ]; then
    git clone https://github.com/tmux-plugins/tmux-yank $HOME/.tmux/yank
fi

# i3
ln -fs $PWD/i3 $HOME/.i3
mkdir -p $HOME/.config/rofi
ln -fs $PWD/i3/rofi $HOME/.config/rofi/config

# vim
ln -fs $PWD/vim/vimrc $HOME/.vimrc

# xorg
ln -fs $PWD/xorg/Xmodmap $HOME/.Xmodmap
ln -fs $PWD/xorg/Xresources $HOME/.Xresources
