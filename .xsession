#!/bin/sh

PATH=/home/ole/bin:$PATH

xscreensaver -no-splash &

tray &

xv -root -quit etc/images/hornet-1920x1080.png

xsetroot -cursor_name left_ptr

#urxvtd -q -o -f
#urxvtc

# turn off trackpoint by default
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 8 0

klipper
zim --plugin trayicon
kmix --keepvisibility

eval "$(gpg-agent --daemon)"

xmonad

