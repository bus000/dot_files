# Choose the cursor for xmonad.
xsetroot -cursor_name left_ptr

xrdb -merge .Xresources

# Bachground picture.
xpmroot ~/pictures/background/14_06_2015.png

setxkbmap gb

# British keyboard layout.
#setxkbmap -layout en-gb

# Lanch xmonad.
exec xmonad
