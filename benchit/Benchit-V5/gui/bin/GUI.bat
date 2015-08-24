echo ### execute java GUI ###
java -Xmx128m -Xms128m -jar -Dusessl=true -Djavax.net.ssl.trustStore=..\cfg\client.trusts -Djavax.net.ssl.trustStorePassword=BenchIT BenchIT.jar
