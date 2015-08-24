#!/usr/bin/perl

use Getopt::Long;
use DBI;

# this function loads the availiable parameters
GetOptions( 
	"DBServer=s" 			=> \$DBServer,
	"DBName=s" 				=> \$DBName,
	"DBUser=s" 				=> \$DBUser,
	"DBPass=s"				=> \$DBPass,
	"BIMode=s" 				=> \$BIMode,
	"BIProblemSize=s" => \$BIProblemSize
 );

# this sub initializes the database connection
sub initDBConnection
{
	$dbconnection = DBI->connect( "dbi:mysql:$DBName", $DBUser, $DBPass ) ||
     die "can't connect to database: $DBI::errstr\n";
}

# this sub closes the database connection
sub closeDBConnection
{
	$dbconnection->disconnect;
}

# this sub does the database preparation
sub measurement_init
{
  initDBConnection();
  
  my $query = $dbconnection->prepare( "truncate table test" ) ||
	  die "can't prepare statement: $DBI::errstr\n";

  $query->execute || 
  	die "can't process query:  $DBI::errstr\n";

	$query->finish;

  my $counter = 1;
  while( $counter <= $BIProblemSize )
  {
    my $value = int( rand( 1024 ) );
    
    my $query = $dbconnection->prepare( "insert into test ( value ) values ( $value )" ) ||
	    die "can't prepare statement: $DBI::errstr\n";

    $query->execute || die "Kann Abfrage nicht ausfuehren:  $DBI::errstr\n";
	  
	  $query->finish;
	  
	  $counter++;
  }
  
  closeDBConnection();
}

# this sub does the main measurement routine
sub measurement_reading
{
  initDBConnection();
  
	my $query = $dbconnection->prepare( 
		"select * from test limit 1, $BIProblemSize" ) ||
			die "Kann Statement nicht vorbereiten: $DBI::errstr\n";
  
	$query->execute ||
		die "can't process query:  $DBI::errstr\n";

	$query->finish;
	
	closeDBConnection();
}

# build function pointers
%MeasurementModes = (
	'init'     	=> \&measurement_init,
	'read'			=> \&measurement_reading
);

# run measurement or exit
if( $MeasurementModes{ $BIMode } )
{
	$MeasurementModes{ $BIMode }->();
}
else
{
	print "Invalid measurementmode supplied!\n";
}
