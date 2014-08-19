fn="Koruri:size=10"
bg="#eeeeee"
fg="#333333"

conky -c /home/deega/.xmonad/script/pk.conkyrc | dzen2 -p -e 'button3=' -ta r -x 700 -h 20 -w 660 -fn "$fn" -bg "$bg" -fg "$fg"
