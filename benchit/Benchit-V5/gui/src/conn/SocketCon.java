/**
 * Title:SocketCon
 * Description:	Starts a SocketConnection
 * Copyright:    Copyright (c) 2003
 * Company:		ZHR (Center for High Performance Computing)
 * @author: Robert Schoene (rschoene@zhr.tu-dresden.de)
 * @version 1.0
 */
package conn ;

import java.io.* ;
import java.net.* ;
import java.util.* ;
import javax.net.ssl.* ;
import java.security.*;
import java.security.cert.*;

public class SocketCon
{
   /*SSL*/
   Socket server ; //on this we define the Streams in and out
   int port ; //the port on the server to communicate with
   BufferedReader in ; //the outputStream of the Server
   PrintWriter out ; //the inputStream of the Server
   boolean debug = false ;
   private char stopSig='\t';

   MyJPanel root ;
   gui.BIGGUI gui=null;

   /**
    * Constructor, that connects to server on port  8080 and listens to
    * which port it shall finaly connect
    * @param ip the ip or name of the server
    * @param username the username to login
    * @param password the users password
    * @param proPanel a panel, which can show the state of a actual process. not needed, might be null
    * @param root the MyJPanel of this program
    * @param gui the BIGGUI of the program
    * @throws java.lang.IOException is thrown if the connection to the server failed
    * @throws java.lang.Exception for any other exception
    */
   public SocketCon(  String ip , String username ,
                     String password , ProgressPanel proPanel , MyJPanel root , gui.BIGGUI gui)
       throws Exception
   {
      this.root = root ;
      this.gui=gui;
      //creates a connection to 141.30.63.10:8080
      try
      {
         // get trustmanager
         TrustManagerFactory tm = TrustManagerFactory.getInstance(
             TrustManagerFactory.getDefaultAlgorithm() ) ;
         // read certificate
         InputStream in = new FileInputStream( new File( system.BIGInterface.
             getInstance().getBenchItPath() + File.separator + "gui" +
             File.separator + "cfg" + File.separator + "server.certs" ) ) ;
         CertificateFactory f = CertificateFactory.getInstance( "X.509" ) ;
         Collection c = f.generateCertificates( in ) ;
         Iterator it = c.iterator() ;
         // key store for encrypted coomunication
         KeyStore ks = KeyStore.getInstance( "JKS" ) ;
         // init
         ks.load( null , null ) ;
         // write certificate to keystore
         while ( it.hasNext() )
         {
            Object o = it.next() ;
            java.security.cert.X509Certificate ce = ( java.security.cert.
                X509Certificate ) o ;
            ks.setCertificateEntry( "BenchITcert" , ce ) ;
            tm.init( ks ) ;
         }
         // trust keystore
         TrustManagerFactory tmf = TrustManagerFactory.getInstance(
             TrustManagerFactory.
             getDefaultAlgorithm() ) ;

         tmf.init( ks ) ;
         // init ssl
         SSLContext x = SSLContext.getInstance( "SSL" ) ;
         x.init( null , tm.getTrustManagers() , null ) ;
         // connecting to server
         SSLSocketFactory fac = x.getSocketFactory() ;
         server = fac.createSocket( ip , 8080 ) ;
         ( ( SSLSocket ) server ).startHandshake() ;
      }
      catch(SSLHandshakeException sslhse)
      {
         // shouldn't be thrown again AddKey... doesn't exist any longer
        System.err.println(sslhse);
         throw new IOException(
             "No SSL found. Please start the AddKeyToKeyStore-Script" ) ;
      }
      catch ( IOException ex )
      {
          ex.printStackTrace();
         throw new IOException( "server not found." ) ;
      }
      catch(Exception e)
      {
          e.printStackTrace();
          throw e;
      }
      //for debugging
      if ( debug ) System.out.println( " connected to " +
                                       server.getInetAddress() ) ;
      //defining the Streams
      in = new BufferedReader( new InputStreamReader(
          server.getInputStream() ) ) ;
      out = new PrintWriter( server.getOutputStream() ) ;
      // send username
      try
      {
         out.write( username+'\n' ) ;
         out.flush() ;
      }
      catch ( Exception ex1 )
      {
         System.err.println(ex1);
         throw ex1;
      }
      // send password
      out.write( password+'\n' ) ;
      out.write( this.stopSig+"\n" ) ;
      out.flush();
      //getting the new port to connect with
      String temp = this.in.readLine();
      if ( temp.indexOf( 'E' ) != -1 )
      {
         System.err.println( "no E" ) ;
         throw new IOException( "username/password not correct" ) ;
      }
      //for debugging
      if ( debug ) System.out.println( temp ) ;
      //storing the port into an int not needed, i'd say
      int newPort = new Integer( temp ).intValue() ;
      //storing it in this.port
      this.port = newPort ;
      System.out.println( "Connected to Server" ) ;
   }

   /**
    * receives a Vector (filled with Strings) from the Server
    * @return the received Vector
    * @throws java.lang.IOException connection-error
    */
   private String[] receiveVector()
       throws IOException
   {
      // receive the string
      String workString = this.receiveString() + this.stopSig ;
      // how many lines are in the string
      int numberOfStrings = 0 ;
      // check for stopSigs
      for ( int i = 0 ; i < workString.length() ; i++ )
         if ( workString.charAt( i ) == this.stopSig )
            numberOfStrings++ ;
      // the number of lines and the lines
      String[] returnField = new String[ numberOfStrings ] ;
      int whereInReturnField = 0 ;
      StringBuffer tempStringBuffer = new StringBuffer() ;
      //we search through the incoming answer
      for ( int i = 0 ; i < workString.length() ; i++ )
      {
         //if there was no this.stopSig the itemsname goes on
         if ( workString.charAt( i ) != this.stopSig )
         {
            tempStringBuffer.append( workString.substring( i , i + 1 ) ) ;
         } // a this.stopSig divides the items
         else
         {
            //we add the item to the choice
            // not found in db
            if ( ( tempStringBuffer.toString() ).equals( "" ) )
               tempStringBuffer = new StringBuffer( "<unknown>" ) ;
            // invalid item. remove it
            if ( ( tempStringBuffer.toString() ).equals( "null" ) )
            {
               // rewrite length of returnfield
               numberOfStrings--;
               // rewrite returnfield
               String[] newReturnField = new String[ numberOfStrings ] ;
               for (int j=0;j<whereInReturnField;j++)
                  newReturnField[j]=returnField[j];
               returnField=newReturnField;
               tempStringBuffer = new StringBuffer() ;
            }
            // valid item
            else
            {
               returnField[ whereInReturnField ] = tempStringBuffer.toString() ;
               whereInReturnField++ ;
               //and clear the tempString
               tempStringBuffer = new StringBuffer() ;
            }
         }
      }
      return returnField ;
   }

   /**
    * sends a request to the server
    * @param a the request to send
    * @throws java.lang.IOException for connection-errors
    */
   public void send( String a )
       throws IOException
   {
      // start with stopsig
      StringBuffer sendSB = new StringBuffer() ;
      if ( a.charAt( 0 ) != this.stopSig )
      {
         sendSB.append( this.stopSig ) ;
      }
      // then a
      sendSB.append( a ) ;
      // end with '\n'
      if ( a.charAt( sendSB.toString().length() - 1 ) != '\n' )
      {
         sendSB.append( "\n" ) ;
      }
      this.out.write( sendSB.toString() ) ;
      this.out.flush();
   }

   /**
    * receives a String from the Server
    * @return the received String
    * @throws java.lang.IOException for connection-errors
    */
   private String receiveString()
       throws IOException
   {
      // read from stream
      String input = this.in.readLine() ;
      String output = new String() ;
      //position of stopsig
      int away = input.indexOf( this.stopSig ) ;
      // if it is the first-> compatible string formats
      if ( away == 0 )
      {
          return input.substring( 1 , input.length() ) ;
        }
      // else remove every n'ths char
      for ( int i = away ; i < input.length() ; i = i + away + 1 )
      {
         output = output + input.charAt( i ) ;
      }
      output = output.substring( 1 , output.length() ) ;
      // if server sent internal error
      if ( output.length() > 6 )
         if ( output.substring( 0 , 6 ).equals( "Error:" ) )
         {
            System.err.println( "-----------------------" ) ;
            System.err.println( output ) ;
            System.err.println( "-----------------------" ) ;
            System.err.println(
                "Please send an email with the error to robert.schoene@zhr.tu-dresden.de" ) ;
            javax.swing.JOptionPane.showMessageDialog( gui , output ) ;
            this.root.closeSocketCon() ;
         }
      return output ;
   }

   /**
    * initializes the Data
    *   elementAt(0) String[] identifiers
    *   elementAt(1) Vector[] (all contain the String "<any>" and nothing more)
    * @return a Vector, that contains a String-Array filled with identifiers
    * @throws java.lang.IOException for connection-errors
    */
   public Vector startInitializing()
       throws IOException
   {
      // just one communication at a time
      synchronized(this)
      {
         // what is sent by server
         Vector returnVector = new Vector() ;
         gui.getStatusLabel().setText( "Sending Request" ) ;
         // initialize
         send( this.stopSig + "Initialize" ) ;
         // send anything. nothing. or whatever you like ;)
         send( this.stopSig + "ovjn" ) ;
         // vector for received Strings
         Vector containsAllVectors = new Vector() ;
         // receive all identifiers
         do
         {
            containsAllVectors.add( this.receiveString() ) ;
            gui.getStatusLabel().setText( "Receive Answer " +
                                       containsAllVectors.size() ) ;
            // until server sent STOP
         }
         while ( ! ( ( ( String ) containsAllVectors.lastElement() ).equals(
             "STOP" ) ) ) ;
         gui.getStatusLabel().setText( "Calculating identifiers" ) ;
         // always -1 because of STOP
         // bring received stuff into right shape
         String[] identifiers = new String[containsAllVectors.size() - 1 ] ;
         Vector[] comboBoxEntries = new Vector[containsAllVectors.size() - 1 ] ;
         gui.getStatusLabel().setText( "Setting identifiers" ) ;
         for ( int i = 0 ; i < containsAllVectors.size() - 1 ; i++ )
         {
            identifiers[ i ] = ( ( ( String ) containsAllVectors.elementAt( i ) ) ) ;
            comboBoxEntries[ i ] = new Vector() ;
            comboBoxEntries[ i ].add( 0 , "<any>" ) ;
         }
         returnVector.add( identifiers ) ;
         returnVector.add( comboBoxEntries ) ;
         gui.getStatusLabel().setText( "done" ) ;
         return returnVector ;
      }
   }

   /**
    * sends a file to the server
    * @param file the File to send
    * @return returns, what the Server say to the file (Okay/Error occured/...)
    * @throws Exception if sth with the connection isnt correct or the server didnt accept the file
    */
   /* public String uploadFileToServer(File file) throws Exception
    {
      FileInputStream bla = null;
      try {
        bla = new java.io.FileInputStream(file);
      }
      catch (FileNotFoundException ex) {
        throw new Exception("File not found");
      }
      DataInputStream bla3 = new java.io.DataInputStream(bla);
      Vector tempVector = new Vector();
      String entry=bla3.readLine();
      if (entry.length()<0) throw new Exception("Not a BenchItFile");
      while (entry.length()>-1)
      {
        tempVector.add(entry);
        entry=bla3.readLine();
      }
      String[] fileAsStringArray = new String [tempVector.size()];
      for (int i=0;i<fileAsStringArray.length;i++)
      {
        fileAsStringArray[i]=(String)tempVector.elementAt(i);
      }
      return uploadFileToServer(fileAsStringArray);
    }
    /**
     * uploads a file to the Server
     * @param fileContent the lines of the file
     * @return a String that should say Okay but not Error occured
     * @throws java.lang.Exception if sth happened to the connection
     */
    /*  public String uploadFileToServer(String[] fileContent) throws Exception
      {
        // means "U"pload
        this.send("U");
        // sends the file
        for (int i=0;i<fileContent.length;i++)
        {
          this.send(fileContent[i]);
        }
        //waiting for an okay...
        return this.receiveString();
      }
      /**
      * receives some functions from the Server, that where specified by:
      * @param identifiers the Identifiers
      * @param settings and the Settings to the Identifiers
      * @return the functions, that were received
      * @throws java.lang.IOException for connection-errors
      */
     protected Graph[] receiveGraphsFromServer( String[] identifiers ,
                                                String[] settings )
         throws IOException
     {
        // only one server interaction at a time
        synchronized(this)
        {
           gui.getStatusLabel().setText( "Send Request" ) ;
           // we want a graph
           this.send( this.stopSig + "G" ) ;
           // with this settings
           this.sendIdentifiersAndSettings( identifiers , settings ) ;
           Vector receivedIdentifiers = new Vector() ;
           Vector receivedSettings = new Vector() ;
           // receiving identifiers and settings
           do
           {
              String[] v = this.receiveVector() ;
              receivedIdentifiers.add( ( String ) v[ 0 ] ) ;
              gui.getStatusLabel().setText( "Receiving Identifier" +
                                         receivedIdentifiers.size() ) ;
           // if it is not known
              if ( v.length > 1 )
              {
                 receivedSettings.add( v[ v.length - 1 ] ) ;
              }
              else
              {
                 receivedSettings.add( "<any>" ) ;
              }
           }
           while ( ! ( ( ( String ) receivedIdentifiers.lastElement() ).equals(
               "STOP" ) ) ) ;
           gui.getStatusLabel().setText( "Calculating Content" ) ;
           // this will contain all identifiers and settings
           String[][] identifiersAndSettings = new String[2 ][
               receivedIdentifiers.size() - 1 ] ;
           // writing content for all identifiers and their settings
           for ( int i = 0 ; i < identifiersAndSettings[ 0 ].length ; i++ )
           {
              identifiersAndSettings[ 0 ][ i ] = ( String ) receivedIdentifiers.
                  elementAt( i ) ;
              identifiersAndSettings[ 1 ][ i ] = ( String ) receivedSettings.
                  elementAt( i ) ;
           }
           gui.getStatusLabel().setText( "Receiving File" ) ;
           // receiving file
           Vector fileVector = new Vector() ;
           do
           {
              fileVector.add( this.receiveString() ) ;
           }
           while ( ! ( ( ( String ) fileVector.lastElement() ).equals(
               "endofdata" ) ) ) ;
           String[] file = new String[fileVector.size() ] ;
           // writing file
           for ( int i = 0 ; i < file.length ; i++ )
           {
              file[ i ] = ( String ) fileVector.elementAt( i ) ;
           }
           gui.getStatusLabel().setText( "Finding Functions" ) ;
           // which Function is needed
           String measureValue = "<any>" ;
           // if description isnt set, add all  functions from the file
           for ( int i = 0 ; i < identifiers.length ; i++ )
           {
              if ( identifiers[ i ].equals( "Description" ) )
              {
                 measureValue = settings[ i ] ;
                 break ;
              }
           }
           // calculating points
           Vector definingGraphVector = this.getXsYsAndFunctionNamesFromFile(
               measureValue , file ) ;
           String[] measureValues = ( String[] ) definingGraphVector.elementAt(
               0 ) ;
           String xAxisText = ( String ) definingGraphVector.elementAt( 1 ) ;
           String[] yAxisText = ( String[] ) definingGraphVector.elementAt( 2 ) ;
           Double[][] dpoints = ( ( Double[][] ) definingGraphVector.elementAt(
               3 ) ) ;
           // Double to double
           double[][] points = new double[dpoints.length ][ dpoints[ 0 ].length ] ;
           for ( int i = 0 ; i < dpoints.length ; i++ )
           {
              for ( int j = 0 ; j < dpoints[ 0 ].length ; j++ )
              {
                 points[ i ][ j ] = dpoints[ i ][ j ].doubleValue() ;
              }
           }
           gui.getStatusLabel().setText( "creating Graphs" ) ;
           Graph[] returnGraphs = new Graph[points.length - 1 ] ;
           // for all graphs
           for ( int i = 0 ; i < returnGraphs.length ; i++ )
           {
              returnGraphs[ i ] = new Graph() ;
              returnGraphs[ i ].setFileContent( file ) ;
              returnGraphs[ i ].setPoints( points[ 0 ] , points[ i + 1 ] ) ;
             returnGraphs[ i ].setIdentifiersAndSettings(
                 identifiersAndSettings ) ;
              returnGraphs[ i ].setXAxisText( xAxisText ) ;
              returnGraphs[ i ].setYAxisText( yAxisText[ i ] ) ;
              returnGraphs[ i ].setGraphName( returnGraphs[ i ].getSetting(
                  "Processor name" ) + " - " +
                  returnGraphs[ i ].getSetting( "Language" ) +
                       " - " +
                       returnGraphs[ i ].getSetting(
                           "Kernelname" ) +
                       " - " + measureValues[ i ] ) ;
           }
           // done
           gui.getStatusLabel().setText( "done" ) ;
           return returnGraphs ;
        }
     }

   /**
    * gets a File, which is specified by:
    * @param identifiers some identifiers and
    * @param settings their settings
    * @return the File as lines of the file
    * @throws java.lang.IOException for connection-errors
    */
   public String[] getFile( String[] identifiers , String[] settings )
       throws IOException
   {
      // only one communication at a time
      synchronized (this)
      {
         gui.getStatusLabel().setText( "Sending Request" ) ;
         StringBuffer sendSB = new StringBuffer( this.stopSig + "F" +
                                                 this.stopSig + "WholeFile" ) ;
         // what we want
         this.send( sendSB.toString() ) ;
         // what we know
         this.sendIdentifiersAndSettings( identifiers , settings ) ;
         gui.getStatusLabel().setText( "Receiving File from Server" ) ;
         Vector v = new Vector() ;
         // receive all lines of the file
         do
         {
            v.add( this.receiveString() ) ;
         }
         while ( ( ( String ) v.elementAt( v.size() - 1 ) ).equals( "endofdata" ) ) ;
         // Vector -> String[]
         String[] returnStringArray = new String[v.size() ] ;
         for ( int i = 0 ; i < v.size() ; i++ )
         {
            returnStringArray[ i ] = ( String ) v.elementAt( i ) ;
         }
         // done
         return returnStringArray ;
      }
   }

   /**
    * sends the identifiers and their settings to the server
    * (reduces code)
    * @param identifiers the identifiers to send
    * @param settings their settings
    * @throws java.lang.IOException for connection-errors
    */
   private void sendIdentifiersAndSettings( String[] identifiers ,
                                            String[] settings )
       throws IOException
   {
         StringBuffer sendSB = new StringBuffer() ;
         for ( int i = 0 ; i < identifiers.length ; i++ )
         {
            // devides parts
            sendSB.append( this.stopSig ) ;
            // identifier
            sendSB.append( identifiers[ i ] ) ;
            //devides identifier and setting
            sendSB.append( ":" ) ;
            // setting
            sendSB.append( settings[ i ] ) ;
         }
         this.send( sendSB.toString() ) ;
   }

   /**
    * send ComboBoxRequest and receives remaining Entries for ComboBoxes
    * elementAt(0): String[] identifiersOfThecomboBoxes
    * elementAt(1): Vector[] contentOfThe ComboBoxes as Strings
    * @param identifiersOfTheComboBoxesToSet the identifiers, for which the
    * remaining content shall be getted
    * @param settedIdentifiers the identifiers that have already been set
    * @param settedSettings and their settings
    * @return a Vector that is described some lines above
    * @throws java.lang.IOException for connection-errors
    */
   public Vector getComboBoxContent( String[] identifiersOfTheComboBoxesToSet ,
                                     String[] settedIdentifiers ,
                                     String[] settedSettings )
       throws IOException
   {
      // only one communication at a time
      synchronized(this)
      {
         gui.getStatusLabel().setText( "Sending Request" ) ;
         Vector returnVector = new Vector() ;
         // the server may change order of the identifiers
         String[] returnIdentifiers = new String[
             identifiersOfTheComboBoxesToSet.
             length ] ;
         // content for the comboboxes
         Vector[] returnComboContent = new Vector[
             identifiersOfTheComboBoxesToSet.
             length ] ;
         // what we want
         StringBuffer sendSB = new StringBuffer( this.stopSig + "S" ) ;
         for ( int i = 0 ; i < identifiersOfTheComboBoxesToSet.length ; i++ )
         {
            sendSB.append( this.stopSig ) ;
            sendSB.append( identifiersOfTheComboBoxesToSet[ i ] ) ;
         }
         send( sendSB.toString() ) ;
         // what we know
         this.sendIdentifiersAndSettings( settedIdentifiers , settedSettings ) ;
         int i = -1 ;
         // receive content of all comboboxes
         do
         {
            i++ ;
            gui.getStatusLabel().setText( "Receiving Content " + i + "/" +
                                          identifiersOfTheComboBoxesToSet.
                                          length ) ;
            String[] v = this.receiveVector() ;
            // received STOP -> end processing
            if ( ( ( String ) ( v[ v.length - 1 ] ) ).equals( "STOP" ) )
            {
               break ;
            }
            // first is the identifier
             returnIdentifiers[ i ] = v[ 0 ] ;
             // we can use the new space for "<any>" :)
             v[ 0 ] = "<any>" ;
             // following is content of comboboxes as String[]
             returnComboContent[ i ] = new Vector() ;
            for ( int j = 0 ; j < v.length ; j++ )
               returnComboContent[ i ].add( v[ j ] ) ;
         }
         while ( true ) ;
         returnVector.add( returnIdentifiers ) ;
         returnVector.add( returnComboContent ) ;
         gui.getStatusLabel().setText( "done" ) ;
         return returnVector ;
      }
   }

   /**
    * gets a filename from the server
    * @param settedIdentifiers the setted identifiers
    * @param settedSettings and their settings
    * @return the filename
    * @throws java.lang.IOException for connection-errors
    */
   public String getFilename( String[] settedIdentifiers ,
                              String[] settedSettings )
       throws IOException
   {
      // what we want
      StringBuffer sendSB = new StringBuffer( this.stopSig+"F"+this.stopSig+"FileName" ) ;
      this.send( sendSB.toString() ) ;
      // what we know
      this.sendIdentifiersAndSettings( settedIdentifiers , settedSettings ) ;
      return this.receiveString() ;
   }
   /**
    * shortens the sql-query. filenames should be unique
    * @param filename String
    * @return String[][]
    * @throws IOException
    */
   public String[][] getIdentifiersAndSettings( String filename )
       throws IOException
   {
      // we want identifiers for this file
      this.send( this.stopSig+"S"+this.stopSig+"FN"+this.stopSig+ filename ) ;
      // and get them
      String[] identifiers = this.receiveVector() ;
      String[] settings = this.receiveVector() ;
      String[][] returnString = new String[2 ][ identifiers.length ] ;
      returnString[ 0 ] = identifiers ;
      returnString[ 1 ] = settings ;
      return returnString ;
   }

   /**
    * used for getting functions without informations (e.g.preview)
    * returns a Vector:
    *	elementAt(0)=Double[][] d (d:[0][]->xValues,d[n][]->yValues of function n)
    *	elementAt(1)=String[] s (s[n-1] name of function n-1)
    * @param measureValue the functionname of the special function to get out of a file
    * (e.g. "sin" or "ijk" ) or "<any>" for all functions (also if the special name is not found)
    * @param settedIdentifiers the identifiers, that specify the file (and function)
    * @param settedSettings and their settings
    * @return a Vector as described some lines above
    * @throws java.lang.IOException for connection-errors
    */
   public Vector getXsYsAndFunctionNamesFromServer( String measureValue ,
       String[] settedIdentifiers , String[] settedSettings )
       throws IOException
   {
      // only one communication at a time
      synchronized(this)
      {
         // for non existing y-values (maybe the first graph has x-values 1,3,5,...
         // and the second one has 2,4,6,..., all x-values have to be stored,
         // but not all y-values exist for every graph. Because they shall not be
         // shown, it must be a negative value (out of sight, out of sense ;) )
         Double errorDou = new Double( -1.0 ) ;
         try
         {
            // getting negativ value selected by user
            errorDou = new Double( system.BIGInterface.getInstance().
                                   getBIGConfigFileParser().
                                   intCheckOut( "errorInt" ) ) ;
         }
         catch ( Exception ignored )
         {
         }
         // we want the whole file
         StringBuffer sendSB = new StringBuffer( this.stopSig + "F" +
                                                 this.stopSig + "WholeFile" ) ;
         this.send( sendSB.toString() ) ;
         // and this are specifications for it
         this.sendIdentifiersAndSettings( settedIdentifiers , settedSettings ) ;
         Vector dataStringsVector = new Vector() ;
         do
         { // the first incoming Strings are tlegendfunction<n>= the number of the function (functon1/function2...)
            // the next are data
            dataStringsVector.add( this.receiveString() ) ;
            // and the last is endofdata
         }
         while ( ! ( ( ( String ) dataStringsVector.lastElement() ).equals(
             "endofdata" ) ) ) ;
         // says which function is selected
         Vector legendfunction = new Vector() ;
         int whichFunction = 0 ;
         int beginOfData = 0 ;
         // for all received lines
         for ( int i = 0 ; i < dataStringsVector.size() ; i++ )
         {
            // if it is a legend for a graph
            if ( ( ( String ) dataStringsVector.elementAt( i ) ).indexOf(
                "tlegendfunction" ) > -1 )
            {
               // store legend (surrounded by '\"'s) in legendfunction
               legendfunction.add( ( ( String ) dataStringsVector.elementAt( i ) ).
                                   substring( ( ( String ) dataStringsVector.
                                                elementAt( i ) ).indexOf( '\"' ) ,
                                              ( ( String ) dataStringsVector.
                                                elementAt( i ) ).length() - 1 ) ) ;
               if ( ( ( String ) dataStringsVector.elementAt( i ) ).indexOf(
                   measureValue ) > -1 )
               {
                  // store the function number
                  whichFunction = legendfunction.size() ;
               }
            }
            // if beginofdata is read, we start parsing the data
            if ( ( ( String ) dataStringsVector.elementAt( i ) ).indexOf(
                "beginofdata" ) > -1 )
            {
               beginOfData = i ;
               break ;
            }
         }
         // starts in the line after begin of data
         beginOfData++ ;
         // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         // if no function (legend/description) is selected, take all!!!
         // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         if ( whichFunction == 0 )
         {
            // the legend names
            String[] returnNames = new String[legendfunction.size() ] ;
            Double[][] returnPoints = new Double[legendfunction.size() +
                1 ][ dataStringsVector.size() - beginOfData - 1 ] ;
            // setting names
            for ( int i = 0 ; i < returnNames.length ; i++ )
            {
               returnNames[ i ] = ( String ) legendfunction.elementAt( i ) ;
            }
            // setting points for all data-lines
            for ( int i = beginOfData ; i < dataStringsVector.size() - 1 ; i++ )
            {
               // workstring is the actual line minus the data which has been parsed
               String workString = ( String ) dataStringsVector.elementAt( i ) ;
               int where = 0 ;
               while ( workString.indexOf( '\t' ) != -1 )
               {
                  // all lines are build this way : D1\tD2\t(Dn\t)*
                  // D<n> double (1..x-values,2 and following y-values)
                  // \t tabulator
                  try
                  {
                     // read Dn
                     returnPoints[ where ][ i -
                         beginOfData ] = new Double(
                             workString.substring( 0 , workString.indexOf( '\t' ) ) ) ;
                  }
                  catch ( NumberFormatException ex )
                  {
                     // not found
                     returnPoints[ where ][ i -
                         beginOfData ] = errorDou ;

                  }
                  // cutting D<where>\t, now processing D<where+1>\t
                  workString = workString.substring( workString.indexOf( '\t' ) +
                      1 , workString.length() ) ;
                  where++ ;
               }
            }
            // finalize return structure
            Vector returnVector = new Vector() ;
            returnVector.add( returnPoints ) ;
            returnVector.add( returnNames ) ;
            return returnVector ;
         }
         // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         // if only one function (legend/description) is selected !
         // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         //setting the functionName
         String[] functionName = new String[1 ] ;
         functionName[ 0 ] = ( String ) legendfunction.elementAt( whichFunction -
             1 ) ;
         Double[][] returnPoints = new Double[2 ][ dataStringsVector.size() -
             beginOfData - 1 ] ;
         // setting points
         for ( int i = beginOfData ; i < ( dataStringsVector.size() - 1 ) ; i++ )
         {
            // actual line in output-file
            String workString = ( String ) dataStringsVector.elementAt( i ) ;
            int where = 0 ;
            try
            {
               // parsing data lines
               while ( workString.indexOf( '\t' ) > -1 )
               {
                  // first (here zero) data is xValue, so take it
                  if ( where == 0 )
                  {
                     // take the value
                     try
                     {
                        returnPoints[ 0 ][ i -
                            beginOfData ] = new Double( workString.
                            substring( 0 ,
                                       workString.indexOf( '\t' ) ) ) ;
                     }
                     // set to invalid if not parseable
                     catch ( NumberFormatException ex1 )
                     {
                        returnPoints[ 0 ][ i -
                            beginOfData ] = errorDou ;
                     }
                  }
                  // take just the y-value of selected function
                  if ( where == whichFunction )
                  {
                     // take the value
                     try
                     {
                        returnPoints[ 1 ][ i -
                            beginOfData ] = new Double( workString.
                            substring( 0 ,
                                       workString.indexOf( '\t' ) ) ) ;
                     }
                     // set to invalid if not parseable
                     catch ( NumberFormatException ex2 )
                     {
                        returnPoints[ 1 ][ i -
                            beginOfData ] = errorDou ;
                     }
                  }
                  // take next value
                  workString = workString.substring( workString.indexOf( '\t' ) +
                      1 , workString.length() ) ;
                  where++ ;
               }
               // if we have no appending tab. ! this shouldn't be so, the file should
               // be invalid, but we can still parse it
               if ( where == whichFunction )
               {
                  // take the value
                  try
                  {
                     returnPoints[ 1 ][ i -
                         beginOfData ] = new Double( workString ) ;
                  }
                  // set to invalid if not parseable
                  catch ( NumberFormatException ex3 )
                  {
                     returnPoints[ 1 ][ i - beginOfData ] = errorDou ;
                  }
               }
            }
            catch ( Exception e )
            {
               // I hope this isn't the case :)
            }
         }
         // building return structure
         Vector returnVector = new Vector() ;
         returnVector.add( returnPoints ) ;
         returnVector.add( functionName ) ;
         return returnVector ;
      }
   }

   /**
    * getXsAndYsFromFile
    * should be implemented in plot.BIGOutputParser one day
    * @param measureValue = "<any>" (or not found)  ->all points (e.g. sin+cos)
    * e.g measureValue = <specified> (e.g. "ijk") ->points for specified function (ijk)
    * @param file[] contains all lines of a file
    * returned Vector.elementAt(0) ->String[] functionNames
    * returned Vector.elementAt(1) ->String xAxisText
    * returned Vector.elementAt(2) ->String[] yAxisText
    * returned Vector.elementAt(3) ->Double[][] points
    */
   public Vector getXsYsAndFunctionNamesFromFile( String measureValue ,
                                                  String[] file )
   {
      // if it isn't valid, it shouldn't be shown
      Double errorDou=new Double(-1.0);
      try
      {
         errorDou = new Double(system.BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "errorInt" )) ;
      }
      catch ( Exception ignored )
      {
         // we have still the standard value
      }
      gui.getStatusLabel().setText( "Finding number of Functions" ) ;
      // looking for measureValue
      // -1 for <any> or not found
      // 1,2,... for a special one
      int whichFunction = -1 ;
      Vector returnVector = new Vector() ;
      // only one function is selected
      if ( !measureValue.equals( "<any>" ) )
      {
         // for all lines in the file
         for ( int i = 0 ; i < file.length ; i++ )
         {
            int indexOfFirst = file[ i ].indexOf( '\"' ) ;
            // if one line contains tlegendfunction and the content
            // of measureValue is between the first two " 's
            if ( ( ( file[ i ] ).indexOf( "tlegendfunction" ) != -1 ) &&
                 ( ( ( file[ i ] ).substring( indexOfFirst + 1 ,
                                              ( file[ i ] ).
                                              indexOf( '\"' , indexOfFirst + 1 ) ) ).
                   equals( measureValue ) ) )
            {
               try
               { //maybe its the 10th function (or above )
                  whichFunction = ( ( new Integer( ( file[ i ] ).substring( (
                      file[ i ] ).indexOf( '=' ) - 2 ,
                      ( file[ i ] ).indexOf( '=' ) ) ) ).intValue() ) ;
               }
               catch ( Exception e )
               { //but it could also be the 9th or less
                  whichFunction = ( ( new Integer( ( file[ i ] ).substring( (
                      file[ i ] ).indexOf( '=' ) - 1 ,
                      ( file[ i ] ).indexOf( '=' ) ) ) ).intValue() ) ;
               }
               break ;
            }
         }
      }
      gui.getStatusLabel().setText( "Setting Names of Functions" ) ;
      int beginOfData = 0 ;
      int numberOfFunctions = 0 ;
      // getting begin of data and number of functions
      for ( beginOfData = 0 ; beginOfData < file.length ; beginOfData++ )
      {
         if ( ( ( file[ beginOfData ] ).indexOf( "numfunctions" ) ) != -1 )
         {
            numberOfFunctions = ( new Integer( ( file[ beginOfData ] ).
                                               substring( ( file[ beginOfData ] ).
                indexOf( '=' ) + 1 , ( file[ beginOfData ] ).length() ) ) ).
                intValue() ;
         }
         if ( ( file[ beginOfData ] ).equals( "beginofdata" ) )
         {
            beginOfData = beginOfData + 1 ;
            break ;
         }
      }
      // setting xAxisAndYAxis
      gui.getStatusLabel().setText( "Setting AxisText" ) ;
      String xAxisText = new String() ;
      String[] yAxisText = new String[ numberOfFunctions ] ;
      // for all lines of data
      for ( int i = 0 ; i < beginOfData ; i++ )
      {
         // for all resulting values in a line: parse line
         for ( int j = 0 ; j < numberOfFunctions ; j++ )
         {
            // the very old benchit files had it this way
            if ( file[ i ].indexOf( "yaxistext" + ( j + 1 ) ) != -1 )
            {
               yAxisText[ j ] = file[ i ].substring( file[ i ].indexOf( '"' ) +
                   1 , file[ i ].length() - 1 ) ;
            }
            // now yaxis are described this way
            else if ( file[ i ].indexOf( "y" + ( j + 1 ) + "axistext" ) != -1 )
            {
               yAxisText[ j ] = file[ i ].substring( file[ i ].indexOf( '"' ) +
                   1 , file[ i ].length() - 1 ) ;
            }
            // if xaxistext is found set it
            if ( file[ i ].indexOf( "xaxistext" ) != -1 )
            {
               xAxisText = file[ i ].substring( file[ i ].indexOf( '"' ) + 1 ,
                                                file[ i ].length() - 1 ) ;
            }
         }
      }
      // if add all functions
      if ( whichFunction == -1 )
      {
         // store for names
         String[] functionNames = new String[ numberOfFunctions ] ;
         // for all lines before the result-values
         for ( int i = 0 ; i < beginOfData ; i++ )
         {
            // if a legend is found store it
            if ( file[ i ].indexOf( "tlegendfunction" ) != -1 )
            {
               int which = ( new Integer( file[ i ].substring( 15 ,
                   file[ i ].indexOf( '=' ) ) ) ).intValue() - 1 ;
               String name = ( file[ i ].substring( file[ i ].indexOf( '\"' ) +
                   1 , file[ i ].length() - 1 ) ) ;
               functionNames[ which ] = name ;
            }
         }
         // now store the result-values
         Double[][] returnDou = new Double[ numberOfFunctions +
             1 ][ file.length - ( beginOfData + 1 ) ] ;
         // for all lines containing results (last one is endofdata)
         for ( int i = beginOfData ; i < file.length - 1 ; i++ )
         {
            // the line
            String workString = ( file[ i ] ).substring( 0 ,
                ( file[ i ] ).length() ) ;
            // for all values within the line
            for ( int j = 0 ; j < numberOfFunctions ; j++ )
            {
               // parse 'em, store 'em
               try
               {
                  returnDou[ j ][ i -
                      beginOfData ] = ( new Double( workString.
                      substring( 0 ,
                                 workString.indexOf( '\t' ) ) ) ) ;
               }
               catch ( NumberFormatException ex )
               {
                  returnDou[ j ][ i -
                      beginOfData ] = errorDou;
               }
               // go to the next one
               workString = workString.substring( workString.indexOf( '\t' ) +
                                                  1 , workString.length() ) ;
            }
            // store the last one
            try
            {
               returnDou[ numberOfFunctions ][ i -
                   beginOfData ] = ( new Double( workString ) ) ;
            }
            catch ( NumberFormatException ex1 )
            {
                  returnDou[ numberOfFunctions ][ i -
                      beginOfData ] = errorDou;

            }
         }
         // building return structure
         returnVector.add( functionNames ) ;
         returnVector.add( xAxisText ) ;
         returnVector.add( yAxisText ) ;
         returnVector.add( returnDou ) ;
         gui.getStatusLabel().setText( "Trying to add" ) ;
         return returnVector ;
      }
      // else: only one function is selected
      Double[][] returnDou = new Double[ 2 ][ file.length - ( beginOfData + 1 ) ] ;
      String[] functionName = new String[ 1 ] ;
      functionName[ 0 ] = measureValue ;
      // for all lines with result-values
      for ( int i = beginOfData ; i < file.length - 1 ; i++ )
      {
         // is the function setted after for-loop?
         boolean functionSet = false ;
         // the line in the file
         String workString = ( file[ i ] ).substring( 0 , ( file[ i ] ).length() ) ;
         // for all values in this line
         for ( int j = 0 ; j < numberOfFunctions ; j++ )
         {
            // if it is the x-value: store it
            if ( j == 0 )
            {
               try
               {
                  returnDou[ 0 ][ i -
                      beginOfData ] = ( new Double( workString.
                      substring( 0 ,
                                 workString.indexOf( '\t' ) ) ) ) ;
               }
               catch ( NumberFormatException ex2 )
               {
                  returnDou[ 0 ][ i -
                      beginOfData ]=errorDou;
               }
            }
            // if it is the selected function, store it
            if ( j == whichFunction )
            {
               try
               {
                  returnDou[ 1 ][ i -
                      beginOfData ] = ( new Double( workString.
                      substring( 0 ,
                                 workString.indexOf( '\t' ) ) ) ) ;
               }
               catch ( NumberFormatException ex3 )
               {
                  returnDou[ 1 ][ i -
                      beginOfData ]=errorDou;

               }
               // it is set! :)
               functionSet = true ;
            }
            // delete processed data from line
            workString = workString.substring( workString.indexOf( '\t' ) + 1 ,
                                               workString.length() ) ;
         }
         // function is the last one, so: not parsed yet. do it now!
         if ( !functionSet )
         {
            try
            {
               returnDou[ 1 ][ i - beginOfData ] = ( new Double( workString ) ) ;
            }
            catch ( NumberFormatException ex4 )
            {
                  returnDou[ 1 ][ i -
                      beginOfData ]=errorDou;

            }
         }
      }
      // build return structure
      returnVector.add( functionName ) ;
      returnVector.add( xAxisText ) ;
      returnVector.add( yAxisText ) ;
      returnVector.add( returnDou ) ;
      return returnVector ;
   }

   /**
    * starts some work for the KernelSelectPanel
    * @param kernelName the kernelName to select from
    * @return a Vector containing:
    * elementAt(0): String[] ->Identifiers
    * elementAt(1): String[] ->filenames
    * elementAt(2): String[] description
    * @throws java.lang.IOException for connection-errors
    */
   public Vector startKernelSelectWork( String kernelName )
       throws IOException
   {
      // only one communication at a time
      synchronized(this)
      {
         // kernel selected
         this.send( this.stopSig + "K" ) ;
         String[] identifier = new String[1 ] ;
         String[] setting = new String[1 ] ;
         identifier[ 0 ] = "Kernelname" ;
         setting[ 0 ] = kernelName ;
         // THIS kernel selected
         this.sendIdentifiersAndSettings( identifier , setting ) ;
         Vector all = new Vector() ;
         // receive descriptions and so on
         do
         {
            all.add( this.receiveVector() ) ;
            // if STOP is received. stop receiving and delete this STOP
            if ( ( ( ( String[] ) all.lastElement() )[ ( ( ( String[] ) all.
                lastElement() ).length - 1 ) ] ).equals( "STOP" ) )
            {
               all.removeElementAt( all.size() - 1 ) ;
               break ;
            }
         }
         while ( true ) ;
         // all contains now:
         // elementAt x: Vector v: elementAt0:
         // v: elementAt[0]="filename";
         // v: elementAt[1]="nodename";
         // v: elementAt[2]="processorname";
         // v: elementAt[3]="processorclockrate";
         // v: elementAt[4]="language";
         String[] descriptions = this.receiveVector() ;
         // description elementAt(x) a possible description(legends/functions) to set
         String[] returnIdentifiers = new String[all.size() ] ;
         // bitmask mask available descriptions for each file
         String[] bitMasks = new String[all.size() ] ;
         // for all elements of description (see above for details)
         for ( int i = 0 ; i < all.size() ; i++ )
         {
            // use stringbuffer for faster processing
            StringBuffer sb = new StringBuffer() ;
            // build a describing string out of nodename,processorname,processorclockrate and language
            for ( int j = 1 ;
                  j < ( ( String[] ) all.elementAt( i ) ).length - 1 ; j++ )
            {
               sb.append( ( String ) ( ( ( String[] ) all.elementAt( i ) )[ j ] ) ) ;
               sb.append( "," ) ;
            }
            returnIdentifiers[ i ] = sb.substring( 0 ,  sb.length() - 1 ) ;
            // available descriptions for this file.
            bitMasks[ i ] = ( ( String[] ) all.elementAt( i ) )[ ( ( String[] )
                all.elementAt( i ) ).length - 1 ] ;
         }
         // filenames as id :)
         String[] returnFilenames = new String[all.size() ] ;
         for ( int i = 0 ; i < all.size() ; i++ )
         {
            returnFilenames[ i ] = ( ( String ) ( ( ( String[] ) all.elementAt(
                i ) )[
                                                  0 ] ) ) ;
         }
         // building return structure
         Vector returnVector = new Vector() ;
         returnVector.add( returnIdentifiers ) ;
         returnVector.add( returnFilenames ) ;
         returnVector.add( descriptions ) ;
         returnVector.add( bitMasks ) ;
         return returnVector ;
      }
   }

   /**
    * returns any possible Settings for a specified identifier
    * @param settedIdentifier the special identifier
    * @return all settings
    * @throws java.lang.IOException for connection-errors
    */
   public String[] startArchitectureSelectWork1( String settedIdentifier )
       throws IOException
   {
      // start architecture select for a single identifier
      String[] identifiers = new String[ 1 ] ;
      identifiers[ 0 ] = settedIdentifier ;
      String[] settings = new String[ 1 ] ;
      settings[ 0 ] = "<any>" ;
      // its the same, so use this method
      Vector v = ( ( ( Vector[] ) ( this.getComboBoxContent( identifiers ,
          identifiers , settings ).elementAt( 1 ) ) )[ 0 ] ) ;
      String[] returnString = new String[ v.size() - 1 ] ;
      // and change data
      for ( int i = 1 ; i < v.size() ; i++ )
      {
         returnString[ i - 1 ] = ( String ) v.elementAt( i ) ;
      }
      return returnString ;
   }

   /**
    * gets all programnames (e.g. "MatrixMultiply" / "jacobi"/"numerical.matmul.C.0.0.double")
    * @param settedIdentifier one setted identifier (e.g. "processorname")
    * @param settedAttributes with several selected attributes (e.g."AthlonXP","Pentium4")
    * @return the programnames, which where found in the db
    * @throws java.lang.IOException for connection-error
    */
   public String[] startArchitectureSelectWork2( String settedIdentifier ,
                                                 String[] settedAttributes )
       throws IOException
   {
      // get all programnames for given identifier and possible attributes
      String[] identifiers = new String[ 1 ] ;
      identifiers[ 0 ] = settedIdentifier ;
      String[] toGetIdentifiers = new String[ 1 ] ;
      toGetIdentifiers[ 0 ] = "Kernelname" ;
      String[] settings = new String[ 1 ] ;
      Vector programNames = new Vector() ;
      // receive programnames for every setted attribute
      for ( int i = 0 ; i < settedAttributes.length ; i++ )
      {
         // get it
         settings[ 0 ] = settedAttributes[ i ] ;
         Vector v = ( ( Vector[] ) ( this.getComboBoxContent( toGetIdentifiers ,
             identifiers , settings ).elementAt( 1 ) ) )[ 0 ] ;
         // for all received programnames
         for ( int j = 0 ; j < v.size() ; j++ )
         {
            boolean found = false ;
            // had the programname been added before?
            for ( int k = 0 ; k < programNames.size() ; k++ )
            {
               if ( ( ( String ) v.elementAt( j ) ).equals( ( ( String ) (
                   programNames.elementAt( k ) ) ) ) )
               {
                  found = true ;
               }
            }
            // if not: add it
            if ( ( !found ) &&
                 ( ! ( ( String ) v.elementAt( j ) ).equals( "<any>" ) ) )
            {
               programNames.add( ( ( String ) v.elementAt( j ) ) ) ;
            }
         }
      }
      // return it
      String[] returnNames = new String[ programNames.size() ] ;
      for ( int i = 0 ; i < programNames.size() ; i++ )
      {
         returnNames[ i ] = ( String ) programNames.elementAt( i ) ;
      }
      return returnNames ;
   }

   /**
    * used for getting the informations for the last showed panel in the Architecture/Kernel-Select
    * @param settedIdentifier the setted Identifier
    * @param settedAttributes with his Attributes
    * @param settedProgramName and the program name
    * @return a Vector, which contains:
    * elementAt(0): String[] ->Identifiers (not like the other identifiers, so dont in any of the functions here)
    * elementAt(1): String[] ->filenames the filenames, which ae possible to select from
    * elementAt(2): String[] description thespecial functionName (e.g. ijk or sin)
    * elementAt(3): String...contains a Long with the Bitmask for each (elementAt0/1) which descriptions are supported
    * @throws java.lang.IOException for connection-errors
    */
   public Vector startArchitectureSelectWork3( String settedIdentifier ,
                                               String[] settedAttributes ,
                                               String settedProgramName )
       throws IOException
   {
      // only one communication at a time
      synchronized (this)
      {
         // send first part
         this.send( this.stopSig + "A" + this.stopSig + settedIdentifier +
                    this.stopSig + settedProgramName ) ;
         // build second part
         StringBuffer sb2 = new StringBuffer() ;
         for ( int i = 0 ; i < settedAttributes.length ; i++ )
         {
            sb2.append( this.stopSig ) ;
            sb2.append( settedAttributes[ i ] ) ;
            sb2.append( ":" ) ;
         }
         // send second part
         this.send( sb2.toString() ) ;
         // receive answers
         Vector all = new Vector() ;
         do
         {
            all.add( this.receiveVector() ) ;
            // until STOP is received
            if ( ( ( String ) ( ( ( String[] ) all.lastElement() )[ ( ( String[] )
                all.lastElement() ).length - 1 ] ) ).equals( "STOP" ) )
            {
               all.removeElementAt( all.size() - 1 ) ;
               break ;
            }
         }
         while ( true ) ;
         // all contains now:
         // elementAt x: Vector v: elementAt0:
         // v: elementAt[0]="filename";
         // v: elementAt[1]="nodename";
         // v: elementAt[2]="processorname";
         // v: elementAt[3]="processorclockrate";
         // v: elementAt[4]="language";
         // v: elementAt[5]=bitmask
         String[] descriptions = this.receiveVector() ;
         // description elementAt(x) a possible description to set
         String[] bitMasks = new String[all.size() ] ;
         String[] returnIdentifiers = new String[all.size() ] ;
         // for all received combinations:
         for ( int i = 0 ; i < all.size() ; i++ )
         {
            StringBuffer sb = new StringBuffer() ;
            // build describing string out of nodenam, processorname...
            for ( int j = 1 ;
                  j < ( ( String[] ) all.elementAt( i ) ).length - 1 ; j++ )
            {
               sb.append( ( String ) ( ( ( String[] ) all.elementAt( i ) )[ j ] ) ) ;
               sb.append( "," ) ;
            }
            returnIdentifiers[ i ] = ( sb.toString() ).substring( 0 ,
                ( sb.toString() ).length() - 1 ) ;
            // building bitmask, which desciption(legendname) is available for which file
            bitMasks[ i ] = ( ( String[] ) all.elementAt( i ) )[ ( ( String[] )
                all.elementAt( i ) ).length - 1 ] ;

         }
         // filenames as unique id
         String[] returnFilenames = new String[all.size() ] ;
         for ( int i = 0 ; i < all.size() ; i++ )
         {
            returnFilenames[ i ] = ( ( String ) ( ( ( String[] ) all.elementAt(
                i ) )[
                                                  0 ] ) ) ;
         }
         // build returnstructure
         Vector returnVector = new Vector() ;
         returnVector.add( returnIdentifiers ) ;
         returnVector.add( returnFilenames ) ;
         returnVector.add( descriptions ) ;
         returnVector.add( bitMasks ) ;
         return returnVector ;
      }
   }

   /**
    * gets all Graphs specified by
    * @param identifiers some identifiers
    * @param settings and their settings
    * @return the Graphs
    * @throws java.lang.IOException for connection-errors
    */
   public Vector getAllGraphs( String[] identifiers , String[] settings )
       throws IOException
   {
      Vector returnVector = new Vector() ;
   //   String file[] = getFile( identifiers , settings ) ;
      // is mesureValue that what it should be
      String measureValue = "<any>" ;
      for ( int i = 0 ; i < identifiers.length ; i++ )
      {
         if ( identifiers[ i ].equals( "measurevalue" ) ) measureValue =
             settings[ i ] ;
      }
      Vector points = this.getXsYsAndFunctionNamesFromServer( measureValue ,
          identifiers , settings ) ;
      String[][] identifiersAndSettings = this.getIdentifiersAndSettings( this.
          getFilename( identifiers , settings ) ) ;
      // setting all Graphs
      for ( int i = 0 ;
            i < ( ( Double[][] ) ( points.elementAt( 1 ) ) )[ 0 ].length - 1 ;
            i++ )
      {
         Graph g = new Graph() ;
         g.setIdentifiersAndSettings( identifiersAndSettings ) ;
         double[][] graphPoints = new double[ 2 ][ ( ( Double[][] ) points.
             elementAt( 1 ) )[ 0 ].length ] ;
         for ( int k = 0 ;
               k < ( ( Double[][] ) points.elementAt( 1 ) )[ 0 ].length ; k++ )
         {
            graphPoints[ 0 ][ k ] = ( ( ( Double[][] ) points.elementAt( 1 ) )[
                                      0 ][ k ] ).doubleValue() ;
            graphPoints[ 1 ][ k ] = ( ( ( Double[][] ) points.elementAt( 1 ) )[
                                      i ][ k ] ).doubleValue() ;
         }
         g.setPoints( graphPoints ) ;
         returnVector.add( g ) ;
      }
      return returnVector ;
   }

   /**
    * closes the connections to the server
    * @throws java.lang.Exception throw if failed
    */
   public void close()
       throws Exception
   {
      out.close() ;
      in.close() ;
   }


}
