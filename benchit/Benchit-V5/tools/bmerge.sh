#! /bin/sh

# test wether there are at least the mandatory args
if [[ "$#" -lt "2" ]] ;
  then
    clear
    echo ""
    echo "bgmerge  --file1 \"NameOfFileOne\"  --file2 \"NameOfFileTwo\""
    echo ""
    echo "optional:"
    echo ""
    echo "--newfile     \"NameOfNewFile\""
    echo "--header1 or"
    echo "--header2     specifies file from which to take the header"
    echo "--file1start  \"NumberOfStartValue\" "
    echo "--file1end    \"NumberOfEndValue\""
    echo "--file2start  \"NumberOfStartValue\" "
    echo "--file2end    \"NumberOfEndValue\""
    exit 127 ;
fi

# set some values in advance
header=0
file1start=0
file2start=0
file1end=0
file2end=0
newfile=""

# get the input
while [[ "$#" -gt "0" ]]
do
   case $1 in
      --file1|-file1 )
         shift
         file1=$1
      ;;
      --file2|-file2 )
         shift
	 file2=$1
      ;;
      --file1start|-file1start )
         shift
         file1start=$1
      ;;
      --file2start|-file2start )
         shift
         file2start=$1
      ;;
      --file1end|-file1end )
         shift
         file1end=$1
      ;;
      --file2end|-file2end )
         shift
         file2end=$1
      ;;
      --header1|-header1 )
         shift
         header=1
      ;;
      --header2|-header2 )
         shift
         header=2
      ;;
      --newfile|-newfile )
         shift
         newfile=$1
      ;;
      --help|-help|* )
	 echo "mandatory:"
         echo "bgmerge --file1 \"NameOfFileOne\" --file2 \"NameOfFileTwo\""
	 echo "optional:"
	 echo "--newfile \"NameOfNewFile\""
	 echo "--header1 or --header2 specifies file from which to take header"
	 echo " --file1start \"NumberOfStartValue\" --file1end \"NumberOfEndValue\""
	 echo " --file2start \"NumberOfStartValue\" --file2end \"NumberOfEndValue\""
	 break
      ;;
   esac
   shift
done

#set name of output file if none was given 
if [[ ${newfile} == "" ]] ;
  then 
       newfile="${file1}${file2}" ;
fi

# fill the output with the rigth header
if [[ "${header}" -lt "2" ]] ;
  then
    headerfile=${file1} ;
  else
    headerfile=${file2} ;
fi
beginline=`cat ${headerfile} | grep --line-number beginofdata | cut -d ':' -f 1`

#ToDo check the mins / max and build new header
# for now we will just take the old one...
head --lines=${beginline} ${headerfile} >${newfile}

# fill in the values
beginline=`cat ${file1} | grep --line-number beginofdata | cut -d ':' -f 1`
endline=`cat ${file1} | grep --line-number endofdata | cut -d ':' -f 1`

# calculate the number of values available
let "nrvalues = ${endline} - ${beginline} - 1"
let "nrlines = ${nrvalues} + 1"

# use the start/endvalues to cut of not-wanted-values
if [[ "${file1start}" -gt "0"  &&  "${file1end}" -gt "0" && "${file1end}" -ge "${file1start}" ]] ;
  then
    let "mynrvalues = ${file1end} - ${file1start} + 1"
    let "beginline += ${file1start} - 1"
    let "nrvalues = ${endline} - ${beginline} - 1"
    if [[ "${nrvalues}" -lt "${mynrvalues}" ]] ;
      then 
        echo "your linevalues are not sane"
	exit 127 ;
      else
        nrvalues=${mynrvalues}
	let "nrlines = ${endline} - ${beginline}" ;
    fi
fi

tail --lines=${nrlines} ${file1} | head --lines=${nrvalues} >>${newfile}

# and now the other values
beginline=`cat ${file2} | grep --line-number beginofdata | cut -d ':' -f 1`
endline=`cat ${file2} | grep --line-number endofdata | cut -d ':' -f 1`

# calculate the number of values available
let "nrvalues = ${endline} - ${beginline} - 1"
let "nrlines = ${nrvalues} + 1"

# use the start/endvalues to cut of not-wanted-values
if [[ "${file2start}" -gt "0"  &&  "${file2end}" -gt "0" && "${file2end}" -ge "${file2start}" ]] ;
  then
    let "mynrvalues = ${file2end} - ${file2start} + 1"
    let "beginline += ${file2start} - 1"
    let "nrvalues = ${endline} - ${beginline} - 1"
    if [[ "${nrvalues}" -lt "${mynrvalues}" ]] ;
      then 
        echo "your linevalues are not sane"
	exit 127 ;
      else
        nrvalues=${mynrvalues}
	let "nrlines = ${endline} - ${beginline}" ;
    fi
    tail --lines=${nrlines} ${file2} | head --lines=${nrvalues} >>${newfile}
    tail --lines=1 ${file2} >>${newfile} ;
    
  else
    tail --lines=${nrlines} ${file2} >>${newfile} ;
fi
