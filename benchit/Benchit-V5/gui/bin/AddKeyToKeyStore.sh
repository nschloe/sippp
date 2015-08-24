#!/bin/sh
echo 'Plese set the password to "BenchIT" (if you choose anothe one you got to change the GUI.SH with the trustStorePassword)) and add the certificate to the trusted.'
echo 'The program will expect an answer whether to add the key or not answer this Question with yes in the given language.'

keytool > /dev/null 2> /dev/null
if test $? = "127" ; then
	echo 'JAVA keytool could not be found, please check your JAVA installation'
else
	keytool -import -alias BenchITcert -file ../cfg/server.certs -keystore ../cfg/client.trusts
	echo 'Certificates, you trust:'
	keytool -list -v -keystore ../cfg/client.trusts
fi
