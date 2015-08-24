<?php

  function grepArgv( $argv, $argc )
  {
    $PARAMETERS = array();

    for( $i=1; $i<$argc; $i=$i+2 )
    {
      if( !ereg( "^-", $argv[$i] ) )
      {
        echo "Unknown parameter: ".$argv[$i]."\n";
        
      }
      elseif( !ereg( "^-", $argv[$i+1] ) )
      {
        $PARAMETERS[ substr( $argv[$i], 1 ) ] = $argv[$i+1]; 
      }
      else
      {
        echo "Invalid value for parameter: ".$argv[$i]."\n";
        return false;
      }
    }

    return $PARAMETERS;
  }

  function measurement_init( $PARAMETERS )
  {
    // doing some work to initialize measurement if needed
  }
  
  function measurement_reading( $PARAMETERS )
  {
    // do the measurement
  }
  
  if ( $PARAMETERS = grepArgv( $argv, $argc ) )
  {
    switch( $PARAMETERS['BIMode'] )
    {
    	case 'overhead':
        // run the overhead measurement only
    		break;
      	
      case 'init':
        // run the initialisation routine
        measurement_init( $PARAMETERS );
        break;
                  	
      case 'read':
      	// run the readspeed benchmark
        measurement_reading( $PARAMETERS );
        break;
        	  
      default:
      	echo "Invalid measurementmode supplied\n";
        break;
    }
  }
  else
  {
    exit;
  }
?>
