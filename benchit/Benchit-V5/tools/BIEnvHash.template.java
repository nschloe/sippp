/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
*
* Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*
* $Revision: 1.4 $
* $Date: 2005/07/21 19:42:13 $
* $State: Exp $
*
***********************************************************************/
import java.util.HashMap;
import java.util.Iterator;
import java.io.*;

public class BIEnvHash
{
   private static BIEnvHash instance = null;
   private HashMap table = null;
   private BIEnvHash()
   {
      bi_initTable();
      bi_fillTable();
   }
   public static BIEnvHash getInstance()
   {
      if ( instance == null ) instance = new BIEnvHash();
      return instance;
   }
   /** Puts a new key value mapping into the map and returns the
     * previously assigned value to the key, or null, if it's a
     * new mapping. */
   public String bi_setEnv( String key, String value )
   {
      return bi_put( key, value );
   }
   /** Puts a new key value mapping into the map and returns the
     * previously assigned value to the key, or null, if it's a
     * new mapping. */
   public String bi_put( String key, String value )
   {
      if ( table == null ) bi_initTable();
      Object ret = table.put( key, value );
      if ( ret == null ) return null;
      return (String)ret;
   }
   public String bi_getEnv( String key )
   {
      return bi_get( key );
   }
   public String bi_get( String key )
   {
      if ( table == null ) return null;
      return (String)(table.get( key ));
   }
   public void bi_dumpTable()
   {
      if ( table == null ) return;
      printf( "\nHashtable dump of all known environment variables at compiletime:" );
      printf( "\n Key            | Value" );
      printf( "\n----------------------------------" );
      Iterator it = table.keySet().iterator();
      while ( it.hasNext() )
      {
         String key = (String)(it.next());
         String value = (String)(table.get( key ));
         printf( "\n " + key + " | " + value );
      }
      printf( "\n" + bi_size() + " entries in total.\n" );
      flush();
   }
   public void dumpToOutputBuffer( StringBuffer outputBuffer )
   {
      outputBuffer.append( "beginofenvironmentvariables\n" );
      Iterator it = table.keySet().iterator();
      while ( it.hasNext() )
      {
         String key = (String)(it.next());
         String value = (String)(table.get( key ));
         StringBuffer b = new StringBuffer();
         for ( int i = 0; i < value.length(); i++ )
         {
            if ( value.charAt( i ) == '"' ) {
               /* check equal number of escape chars->insert escape char */
               int cnt = 0;
               for ( int j = i - 1;  j >= 0; j-- ) {
                  if ( value.charAt( j ) == '\\' ) cnt++;
               }
               /* escape quote */
               if ( cnt % 2 == 0 ) {
                  b.append( '\\' );
               }
            }
            b.append( value.charAt( i ) );
         }
         value = b.toString();
         outputBuffer.append( key + "=\"" + value + "\"\n" );
      }
      outputBuffer.append( "endofenvironmentvariables\n" );
   }
   public int bi_size()
   {
      if ( table != null ) return table.size();
      return -1;
   }
   public void bi_initTable()
   {
      table = new HashMap( 1009 );
   }
   private void printf( String str )
   {
      System.out.print( str );
   }
   private void flush()
   {
      System.out.flush();
      System.err.flush();
   }
   /** Takes a PARAMETER file as argument and adds it's variables
     * to the environment HashMap. */
   public boolean bi_readParameterFile( String fileName )
   {
      boolean retval = false;
      File textFile = null;
      try {
         textFile = new File( fileName );
      }
      catch ( NullPointerException npe ) {
         return retval;
      }
      if ( textFile != null ) {
         if ( textFile.exists() ) {
            FileReader in;
            char ch = '\0';
            int size = (int)(textFile.length());
            int read = 0;
            char inData[] = new char[size + 1];
            try {
               in = new FileReader( textFile );
               in.read( inData, 0, size );
               in.close();
            }
            catch( IOException ex ) {
               return retval;
            } // scan inData for environment variables
            int scanned = 0;
            int it = 0;
            ch = inData[scanned];
            StringBuffer buffer = null;
            while ( scanned < size ) {
               buffer = new StringBuffer();
               // read a line
               while ( ch != '\n' ) {
                  if ( ch != '\r' ) {
                     buffer.append( ch );
                  }
                  scanned += 1;
                  ch = inData[scanned];
               }
               // trim leading and trailing white spaces
               String lineValue = buffer.toString().trim();
               // now check the line
               int eqIndex = -1;
               try {
                  eqIndex = lineValue.indexOf( "=" );
               }
               catch ( NullPointerException npe ) {
                  return retval;
               }
               // only check lines containing an equals sign, no $ sign,
               // and the start with a capital letter
               if ( eqIndex >= 0 && lineValue.indexOf( "$" ) < 0 &&
                  lineValue.substring( 0, 1 ).matches( "[A-Z]" ) ) {
                  String varname = null;
                  String varvalue = null;
                  try {
                     varname = lineValue.substring( 0 , eqIndex );
                     varvalue = lineValue.substring( eqIndex + 1 );
                  }
                  catch ( IndexOutOfBoundsException ioobe ) {
                  }
                  if ( ( varname != null ) && ( varvalue != null ) ) {
                     boolean st1=false, en1=false, st2=false, en2=false;
                     boolean st3=false, en3=false;
                     try {
                        st1=varvalue.startsWith( "'" );
                        en1=varvalue.endsWith( "'" );
                        st2=varvalue.startsWith( "(" );
                        en2=varvalue.endsWith( ")" );
                        st3=varvalue.startsWith( "\"" );
                        en3=varvalue.endsWith( "\"" );
                     }
                     catch ( NullPointerException npe ) {
                        return retval;
                     }
                     if ( ( ( st1 == en1 ) && ( ( st1 != st2 ) && ( st1!=st3 ) ) ) ||
                          ( ( st2 == en2 ) && ( ( st2 != st1 ) && ( st2!=st3 ) ) ) ||
                          ( ( st3 == en3 ) && ( ( st3 != st1 ) && ( st3!=st2 ) ) ) ||
                          ( ( st1 == en1 ) && ( st2 == en2 ) && ( st3 == en3 )
                          && ( st1 == st2 ) && ( st2 == st3 ) && ( st3 == false ) ) ) {
                        table.put( varname, varvalue );
                     }
                  }
               }
               scanned += 1;
               ch = inData[scanned];
               it++;
            }
         }
         else {
            System.err.println( "BenchIT: PARAMETER file \"" + fileName
               + "\" doesn't exist." );
            return retval;
         }
      }
      else {
         System.err.println( "BenchIT: PARAMETER file \"" + fileName
               + "\" not found." );
         return retval;
      }
      return true;
   }
   /* special case: generated from outside and code will be
      appended to this file */
   /** Fills the table with predefined content. */
   public void bi_fillTable()
   {
/*****************************************************************************
Log-History

$Log: BIEnvHash.template.java,v $
Revision 1.4  2005/07/21 19:42:13  wloch
debugged parameter file parsing

Revision 1.3  2005/07/21 13:32:04  wloch
implemented hashtable dump into bit file

Revision 1.2  2005/07/19 12:17:59  wloch
added cvs footer

Revision 1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree


*****************************************************************************/
