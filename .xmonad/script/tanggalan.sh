if [ -f /tmp/dzen-cal ]; then
        exit
fi

calen=`cal | sed -e "s/$(date | cut -d' ' -f3)/^fg(#FF8000)$(date | cut -d' ' -f3)^fg()/"`
touch /tmp/dzen-cal
(echo "$calen"; sleep 5) | dzen2 -x 1180 -y 30 -l 8 -w 170 -h 16 -e 'onstart=uncollapse;button1=exit,exec:rm /tmp/dzen-cal;button3=exit,exec:rm /tmp/dzen-cal' -fn "Meslo LG S for Powerline:size=10" -sa c -bg "#3F3F3F" -fg "#B3B3B3"
rm /tmp/dzen-cal
