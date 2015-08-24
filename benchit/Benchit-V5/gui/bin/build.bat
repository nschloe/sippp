@echo off

mkdir ..\temp

echo "###### compiling #######"

javac -d ..\temp -classpath BenchIT.jar ..\src\*.java ..\src\system\*.java ..\src\gui\*.java ..\src\admin\*.java ..\src\plot\*.java ..\src\conn\*.java

install.bat

echo "####### done ############"