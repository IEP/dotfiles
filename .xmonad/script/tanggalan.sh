if [ -f /tmp/dzen-cal ]; then
        exit
fi

calen=`cal | sed -e "s/ $(date | cut -d' ' -f3) / ^fg(#007FFF)$(date | cut -d' ' -f3)^fg() /"`
touch /tmp/dzen-cal
(echo "$calen"; sleep 5) | dzen2 -x 1220 -y 30 -l 8 -w 130 -h 14 -e 'onstart=uncollapse;button1=exit,exec:rm /tmp/dzen-cal;button3=exit,exec:rm /tmp/dzen-cal' -fn "Meslo LG S for Powerline:Bold:size=8" -sa c -bg "#EEEEEE" -fg "#0D0B0D"
rm /tmp/dzen-cal
