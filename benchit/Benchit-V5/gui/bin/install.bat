@echo off
echo "###### installing #######"

cd ..\temp
jar uf ..\bin\BenchIT.jar *.class gui\*.class admin\*.class conn\*.class plot\*.class system\*.class

del *.class
del gui\*.class
del admin\*.class
del conn\*.class
del plot\*.class
del system\*.class
rd gui
rd admin
rd conn
rd plot
rd system
cd ..\bin

echo "####### done ############"