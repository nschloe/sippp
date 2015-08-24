#! /bin/sh

#####
# Go to the directory of makedocu.sh
#####
cd "`dirname ${0}`" || exit 1



#####
# set path to BenchIT-source and BenchIT-docu
#####
doc=`pwd`
echo "Doc: ${doc}"
cd ..
src=`pwd`
echo "Source: ${src}"
cd ${doc}



#####
# test wether $src and $doc really exist
#####
if [ -d ${doc} ]
 then echo "${doc} exists, good :o]"
 else echo "$doc does not exist, very bad :o["
  exit 127
fi
if [ -d ${src} ] 
 then echo "${src} exists, good :o]"
 else echo "${src} does not exist, very bad :o["
  exit 127
fi



#####
# search for robodoc and doxygen
#####
robo=`which robodoc`
doxy=`which doxygen`
if [ ${doxy} = "" ] 
 then echo "doxygen not found, exit"
  exit 127
 else echo "${doxy} found, good"
fi
if [ ${robo} = "" ] 
 then echo "robodoc not found, very bad"
   echo "exit"
  exit 127
 else echo "${robo} found, good"
fi



#####
# remove old documentation if it exists
#####
if [ -d ${doc}/doc ] 
 then echo "${doc}/doc exists, deleting old docu"
  rm -r ${doc}/doc
 else echo "no old docu found"
fi
mkdir ${doc}/doc
mkdir ${doc}/doc/robodoc
mkdir ${doc}/doc/doxygen
mkdir ${doc}/doc/gui



#####
# set up the configuration for robodoc
echo "
    --one_file_per_header
    --tell
    --nopre
    --cmode

    doc
" >/dev/null
#####
echo "options:
    --src ${src}
    --doc ${doc}/doc/robodoc
    --html
    --multidoc
    --toc
    --index
    --sections
    --documenttitle 
	\"Documentation of non-C Parts in BenchIT\"
ignore files:
    CVS
    help" >${doc}/robodoc.rc
 
echo ""



#####
# make part of docu with robodoc
#####
if [ -f ${doc}/robodoc.rc ] 
 then echo "roboroboroboroboroboroboroboroboroboroboroboroboroboroboroborobo" 
  echo "what robodoc has to say:"
  ${robo} >${doc}/robodoc.log
  echo "end of robodoc-messages;"
  echo "roboroboroboroboroboroboroboroboroboroboroboroboroboroboroborobo" 
 else echo "something wicked happened to robodoc.rc :-/" 
 exit 127
fi

echo ""



#####
# adding gui-docs to documentation
#####
if [ -d ${src}/gui/doc ]
   then rm -r ${src}/gui/doc
fi

echo "${src}/gui/doc does not exist, therefor creating doc "
cd ${src}/gui/bin
./doc.sh
cp -r ${src}/gui/doc ${doc}/doc/gui/
cd ${doc}

if [ -d ${src}/gui/help ]
 then cp -r ${src}/gui/help ${doc}/doc/gui/
 else echo "${src}/gui/help does not exist, tragic"
fi


#####
# make doxygen-docu
#####
echo "doxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxy" 
echo "what doxygen has to say:"
cd ${src}
${doxy} Doxyfile >${doc}/doxygen.log
echo "end of doxygen-messages;"
echo "doxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxydoxy" 

echo ""



#####
# making the zip'ed version
#####
#if [ -f ${doc}/doc.tar.gz ] 
#then rm ${doc}/doc.tar.gz
#fi
#cd ${doc}
#tar czf doc.tar.gz doc/ index.html


#####
# update web
#####
if [[ $1 == "-web" ]]
then
mkdir public_html
cp -r doc/ public_html/
cp doc.tar.gz public_html/
cp index.html public_html/
cd public_html/
# change access-policies for the web
chmod -R o+r doc/
chmod -R o+x doc/
chmod o+x index.html
chmod o+r index.html
chmod o+x doc.tar.gz
chmod o+r doc.tar.gz
cd ..
scp -r public_html/ william@rzhr2.urz.tu-dresden.de:./
sync
rm -r public_html
fi



echo "thats all - you now have you documentation"
echo "if anything wicked happened - please have a look into"
echo "${doc}/robodoc.log"
echo "${doc}/doxygen.log"
#echo "first - befor mailing to BenchIT"
