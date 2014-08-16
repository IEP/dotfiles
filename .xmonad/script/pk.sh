fn="Koruri:Bold:size=8"
bg="#eeeeee"
fg="#333333"

conky -c /home/deega/.xmonad/script/pk.conkyrc | dzen2 -p -e 'button3=' -ta r -x 800 -h 20 -w 560 -fn "$fn" -bg "$bg" -fg "$fg"
