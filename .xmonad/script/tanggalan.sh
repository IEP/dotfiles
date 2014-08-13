calen=`cal | sed -e "s/$(date | cut -d' ' -f3)/^fg(#FF8000)$(date | cut -d' ' -f3)^fg()/"`

(echo "$calen"; sleep 5) | dzen2 -x 1180 -y 30 -l 8 -w 170 -h 16 -e 'onstart=uncollapse;button1=exit;button3=exit' -fn "Meslo LG S for Powerline:size=10" -sa c -bg "#3F3F3F" -fg "#B3B3B3"
