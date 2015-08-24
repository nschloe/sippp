#!/bin/sh
#
# Crop PDF files with unneeded boundary, such as the output of MATLAB's print('dpdf',*)


if [ `ls -1 *.ps | wc -l` -ge 1 ]; then
    for FILE in *.ps; do
        PDF=`echo "$FILE" | sed  's/\.ps/\.pdf/'`
        if [ -n $FILE ]; then
	    ps2pdf "$FILE"
	    echo -n "Cropping $PDF..."
	    pdfcrop "$PDF" "$PDF-temp" 1> /dev/null
	    mv -f "$PDF-temp" "$PDF"
	    echo " done."
        fi
    done
fi
