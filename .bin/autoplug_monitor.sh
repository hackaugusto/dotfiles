#!/usr/bin/zsh

set -e 

export DISPLAY=:0.0

function connect-left-side(){
    xrandr \
        --output HDMI-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --scale 1.3x1.3 \
        --output eDP-1 --mode 3200x1800 --pos 2496x0 --rotate normal
}

function connect-up-side(){
    xrandr \
        --output HDMI-1 --primary --mode 1920x1080 --pos 328x0 --rotate normal --scale 1.3x1.3 \
        --output eDP-1 --mode 3200x1800 --pos 0x1404 --rotate normal
}

function disconnect(){
    xrandr --output HDMI-1 --off
}

xrandr | grep "HDMI-1 connected" &> /dev/null && connect-up-side || disconnect
