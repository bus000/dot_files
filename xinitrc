# Choose the cursor for xmonad.
xsetroot -cursor_name left_ptr

xrdb -merge .Xresources

setxkbmap gb

# Set background.
exec feh --bg-scale /usr/share/backgrounds/background.png &

# Lanch xmonad.
exec xmonad
