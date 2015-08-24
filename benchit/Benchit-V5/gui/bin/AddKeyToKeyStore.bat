@echo off
echo 'Plese set the password to "BenchIT" (if you choose another one you got to change the GUI.SH with the trustStorePassword)) and add the certificate to the trusted.'
echo 'The program will expect an answer whether to add the key or not answer this Question with yes in the given language.'
keytool -import -alias BenchITcert -file ..\cfg\server.certs -keystore ..\cfg\client.trusts
echo 'Certificates, you trust:'
keytool -list -v -keystore ..\cfg\client.trusts