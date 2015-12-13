#!/bin/bash
osascript <<EOD
	tell application "Finder"
		set bds to bounds of the window of the desktop
		set scrn_width to item 3 of bds
		if scrn_width > 1920 then set scrn_width to 1920
		set scrn_height to item 4 of bds
	end tell
	tell application "iTerm"
		activate
		set the bounds of front window to {0, 0, scrn_width, scrn_height}
	end tell
EOD
/usr/local/sicstus4.3.2/bin/sicstus -l /Users/Angie/Documents/MIEIC/3A1S/PLOG/practice/project_2/src/magnets.pl
