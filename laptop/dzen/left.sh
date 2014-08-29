x=`xrandr | head -n 1 | cut -d' ' -f8`
width=150
font='Koruri:size=10'
dzen2 -p -ta l -e 'button3=' -fn $font -fg "#D8D8D8" -bg "#1C1C1C" -w $width -h 16
