if [[ $# -eq 0 ]] ; then
    echo 'usage: ./setup.sh <home_directory>'
    exit 1
fi

HOME=$1
PWD=`pwd`

# Emacs
ln -nfs $PWD/emacs $HOME/.doom.d

# Tmux
mkdir -p $HOME/.tmux
ln -nfs $PWD/tmux/tmux.conf $HOME/.tmux.conf
ln -nfs $PWD/tmux/theme $HOME/.tmux/basic.tmuxtheme

if [ ! -e "$HOME/.tmux/yank" ]; then
    git clone https://github.com/tmux-plugins/tmux-yank $HOME/.tmux/yank
fi

# i3
ln -nfs $PWD/i3 $HOME/.config/i3
mkdir -p $HOME/.config/rofi
ln -nfs $PWD/i3/rofi $HOME/.config/rofi/config

# polybar
ln -nfs $PWD/polybar $HOME/.config/polybar

# vim
ln -nfs $PWD/vim/vimrc $HOME/.vimrc

# xorg
ln -nfs $PWD/xorg/Xmodmap $HOME/.Xmodmap
ln -nfs $PWD/xorg/Xresources $HOME/.Xresources

# zsh
ln -nfs $PWD/zsh $HOME/.zsh
ln -nfs $PWD/zshrc $HOME/.zshrc
