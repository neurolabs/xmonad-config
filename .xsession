#!/bin/sh

PATH=/home/ole/bin:$PATH

xscreensaver -no-splash &

tray &

xv -root -quit etc/images/hornet-1400x1050.jpg

xsetroot -cursor_name left_ptr

#urxvtd -q -o -f
#urxvtc

klipper

eval "$(gpg-agent --daemon)"

xmonad

