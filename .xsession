#!/bin/sh

PATH=/home/ole/bin:$PATH

 ## test for an existing bus daemon, just to be safe
# if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
#    ## if not found, launch a new one
#    eval `dbus-launch --sh-syntax --exit-with-session`
#    echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
# fi

/usr/libexec/polkit-gnome-authentication-agent-1 &
xscreensaver -no-splash &

tray &

xv -root -quit etc/images/hornet-1920x1080.png

xsetroot -cursor_name left_ptr

#urxvtd -q -o -f
#urxvtc

# fix libreoffice flicker, see http://code.google.com/p/xmonad/issues/detail?id=200
export SAL_USE_VCLPLUGIN=gen

# turn off trackpoint by default
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Device Enabled" 8 0

klipper &
zim --plugin trayicon &
kmix --keepvisibility &
nm-applet &

eval "$(gpg-agent --daemon)"

exec ck-launch-session dbus-launch xmonad

