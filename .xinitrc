#!/bin/sh
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources &
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi &

[[ -z "$(pidof urxvtd)" ]] && urxvtd -q -o -f &
udiskie &
thunar --daemon &
xsetroot -cursor_name left_ptr &
numlockx &
mpd ~/.config/mpd/mpd.conf &
nitrogen --restore &
compton -f -D5 -I0.05 -O0.05 &
exec xmonad
