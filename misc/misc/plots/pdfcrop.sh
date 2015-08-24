#!/bin/sh
#
# Crop PDF files with unneeded boundary, such as the output of MATLAB's print('dpdf',*)


if [ `ls -1 *.pdf | wc -l` -ge 1 ]; then
    for FILE in *.pdf; do
        if [ -n $FILE ]; then
	    echo -n "Cropping $FILE..."
	    pdfcrop "$FILE" "$FILE-temp" 1> /dev/null
	    mv -f "$FILE-temp" "$FILE"
	    echo " done."
        fi
    done
fi
