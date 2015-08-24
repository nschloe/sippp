#!/bin/sh
#
# Convert EPS files to JPEG compressed PDF files in the current directory.
# Also trims the image to fit nicely in the borders.

if [ `ls -1 *.eps | wc -l` -ge 1 ]; then
    for FILE in *.eps; do
        PDF=`echo "$FILE"|sed 's/\.eps$/\.pdf/'`
        TIF=`echo "$FILE"|sed 's/\.eps$/\.tif/'`
	echo -n "Processing $FILE..."
        if [ -n $PDF ]; then
	    convert -trim -compress JPEG -quality 80 "$FILE" "$TIF" 2> /dev/null
	    tiff2pdf "$TIF" > "$PDF"
	    rm -f "$TIF"
        fi
	echo " done."
    done
fi
