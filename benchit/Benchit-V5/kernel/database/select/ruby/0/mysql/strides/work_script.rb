require 'getoptlong'
require 'dbi'

def grepArgv

	opts = GetoptLong.new(
  	['--DBServer',      GetoptLong::REQUIRED_ARGUMENT],
  	['--DBName',        GetoptLong::REQUIRED_ARGUMENT],
		['--DBUser',        GetoptLong::REQUIRED_ARGUMENT],
		['--DBPass',        GetoptLong::REQUIRED_ARGUMENT],
		['--BIMode',        GetoptLong::REQUIRED_ARGUMENT],
		['--BIProblemSize', GetoptLong::REQUIRED_ARGUMENT] )
	
	$PARAMETERS = {}

	# read the parameters into parameter hash	
	opts.each_option do |name,arg|
 	  $PARAMETERS[ name[2..-1] ] = arg;
	end
end

def measurementOverhead
  puts "nothing yet\n"
end

def measurementInit
  $dbconnection.do( "truncate table test" )

  i = 0
  
  while i < Integer( $PARAMETERS['BIProblemSize'] ) do
    value = rand( 1024 )
    $dbconnection.do( "insert into test \
      ( value ) values ( #{value} )" )
    i += 1
  end
end

def measurementRead
  $dbconnection.execute( "select * from test limit 1, #{$PARAMETERS['BIProblemSize']} " )
   
end

grepArgv

$dbconnection = DBI.connect("dbi:Mysql:#{$PARAMETERS['DBName']}:#{$PARAMETERS['DBServer']}", $PARAMETERS['DBUser'], $PARAMETERS['DBPass'] )

case $PARAMETERS['BIMode']
	when 'overhead'
	  measurementOverhead
	when 'init'
	  measurementInit
	when 'read'
	  measurementRead
	else
	  puts "Invalid measurementmode supplied!\n"
end

$dbconnection.disconnect if $dbconnection
