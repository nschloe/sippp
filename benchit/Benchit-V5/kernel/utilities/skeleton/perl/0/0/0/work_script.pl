#!/usr/bin/perl

use Getopt::Long;
use DBI;

# this function loads the availiable parameters
GetOptions( 
	"BIMode=s" 				=> \$BIMode,
	"BIProblemSize=s" => \$BIProblemSize
 );

# this sub does the database preparation
sub measurement_init
{
	# initialise the measurement if needed
}

# this sub does the main measurement routine
sub measurement
{
  # do the measurement
}

# build function pointers
%MeasurementModes = (
	'init'     	=> \&measurement_init,
	'read'			=> \&measurement
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
