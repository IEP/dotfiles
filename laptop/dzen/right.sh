xa=`xrandr | head -n 1 | cut -d' ' -f8`
x=150
width=$(($xa-$x))
font='Koruri:size=10'

conky -c /home/deega/conky/right | dzen2 -p -ta r -e 'button3=' -fn $font -fg "#D8D8D8" -bg "#1C1C1C" -w $width -h 16 -x 150
