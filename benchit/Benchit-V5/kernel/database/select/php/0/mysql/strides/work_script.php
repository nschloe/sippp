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

  function initDBConnection( $PARAMETERS )
  {
    // connect to the database
    if( !mysql_connect( $PARAMETERS['DBServer'], $PARAMETERS['DBUser'], 
    											$PARAMETERS['DBPass'] ) )
    {
      return false;
    }
    else
    {
      // choose database from server
      return mysql_select_db( $PARAMETERS['DBName'] );
    }
  }

  function measurement_init( $PARAMETERS )
  {
		// clean table
    $sql = "truncate table test";
    mysql_query( $sql );

    // fill with data
  	for( $i=0; $i<$PARAMETERS['BIProblemSize']; $i++ )
    {
      $sql = "insert into test ( value ) values ( ".rand( 0, 1024 )." )";
      mysql_query( $sql );
    }
  }
  
  function measurement_reading( $PARAMETERS )
  {
    $sql = "select * from test limit 1, ".$PARAMETERS['BIProblemSize'] ;
    mysql_query( $sql );
  }
  
  if ( $PARAMETERS = grepArgv( $argv, $argc ) )
  {
    if( $t = initDBConnection( $PARAMETERS ) )
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
      echo "Could not connect to Database\n";
      exit;
    }
  }
  else
  {
    exit;
  }
?>
