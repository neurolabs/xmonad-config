#!/bin/sh

PATH=/home/ole/bin:$PATH

xscreensaver -no-splash &

tray &

xv -root -quit etc/images/hornet-1920x1080.png

xsetroot -cursor_name left_ptr

#urxvtd -q -o -f
#urxvtc

klipper
zim

eval "$(gpg-agent --daemon)"

xmonad

