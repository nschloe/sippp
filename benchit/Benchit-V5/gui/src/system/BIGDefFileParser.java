/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGDefFileParser.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.4 $
 *  $Date: 2007/05/08 09:41:53 $
 *
 ******************************************************************************/
package system ;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.Observable;

/**
 * The parser for reading and writing the DefFiles.
 *
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @see BIGInterface
 **/
public class BIGDefFileParser
{

   private class BIGObservable
       extends Observable
   {
      private int oldValue = -1 ;
      public void setProgress( int value )
      {
         if ( oldValue != value )
         {
            oldValue = value ;
            setChanged() ;
            notifyObservers( new Integer( value ) ) ;
            try
            {
               Thread.sleep( 0 , 1 ) ;
            }
            catch ( InterruptedException ie )
            {}
         }
      }
   }

   private boolean debug = false ;

   private BIGInterface db ;

   private BIGObservable progress ;

   /**
    * Constructs a BIGDefFileParser.
    **/
   public BIGDefFileParser()
   {
      db = BIGInterface.getInstance() ;
      if ( db.getDebug( "BIGDefFileParser" ) > 0 )
         debug = true ;
      progress = new BIGObservable() ;
   }

   private Object convertToType( String org , BIGEntry entry )
       throws NumberFormatException
   {
      if ( org == null )
         return null ;
      if ( entry == null )
         return null ;
      org = org.trim() ;

      // kill comments
      String comment ;
      int commentPos = org.length() ;
      if ( org.startsWith( "\"" ) )
      {
         comment = org.replaceAll( "\\\"" , "--" ) ;
         commentPos = comment.indexOf( " #" , comment.indexOf( "\"" , 1 ) + 1 ) ;
      }
      else
      {
         comment = org ;
         commentPos = comment.indexOf( " #" ) ;
      }
      if ( commentPos > 0 ) org = org.substring( 0 , commentPos ) ;
      // end of kill comments

      Object result = null ;
      // no type
      if ( entry.getType() == BIGInterface.NONE )
      {
         entry.setType( BIGInterface.STRING ) ;
         result = org ;
      }
      // string
      if ( entry.getType() == BIGInterface.STRING )
      {
         if ( org.startsWith( "\"" ) && org.endsWith( "\"" ) )
         {
            result =
                ( org.substring( 1 , org.length() - 1 ) ).replaceAll( "\"\"" ,
                "\"" ) ;
         }
         else
         {
            result = org ;
         }
      }
      // integer
      if ( entry.getType() == BIGInterface.INTEGER )
      {
         if (org.equals(""))
            result=null;
         else
            result = Integer.decode( org ) ;
      }
      // float
      if ( entry.getType() == BIGInterface.FLOAT )
      {
         result = Double.valueOf( org ) ;
      }
      // boolean
      if ( entry.getType() == BIGInterface.BOOLEAN )
      {
         if ( org.equals( "1" )
              || org.toUpperCase().equals( "TRUE" )
             )
         {
            result = Boolean.TRUE ;
         }
         else
         {
            result = Boolean.FALSE ;
         }
      }
      // multiple
      if ( entry.getType() == BIGInterface.MULTIPLE )
      {
         if ( org.startsWith( "\"" ) && org.endsWith( "\"" ) )
         {
            result =
                ( org.substring( 1 , org.length() - 1 ) ).replaceAll(
                    "\"\"" ,
                    "\"" ) ;
         }
         else
         {
            result = org ;
         }
      }
      // list
      if ( entry.getType() == BIGInterface.LIST )
      {
         result = new BIGStrings() ;
         ( ( BIGStrings ) result ).setCommaText( org ) ;
      }
      // datetime
      if ( entry.getType() == BIGInterface.DATETIME )
      {
         result = new GregorianCalendar() ;
      }
      // vector
      if ( entry.getType() == BIGInterface.VECTOR )
      {
         result = new BIGStrings() ;
         ( ( BIGStrings ) result ).setCommaText( org ) ;
      }
      // free text
      if ( entry.getType() == BIGInterface.FREE_TEXT )
      {
         result = new String(org) ;
      }


      return result ;
   }

   private String convertToString( BIGEntry entry )
   {
      if ( entry.getValue() == null )
         return "" ;
      String result = "" ;
      // string
      if ( entry.getType() == BIGInterface.STRING )
      {
         result = new String( ( String ) entry.getValue() ) ;
         result = "\"" + result.replaceAll( "\"" , "\"\"" ) + "\"" ;
      }
      // integer
      if ( entry.getType() == BIGInterface.INTEGER )
      {
         if (entry.getValue()!=null)
            result = ( ( Integer ) entry.getValue() ).toString() ;
      }
      // float
      if ( entry.getType() == BIGInterface.FLOAT )
      {
         result = ( ( Double ) entry.getValue() ).toString() ;
      }
      // boolean
      if ( entry.getType() == BIGInterface.BOOLEAN )
      {
         if ( ( ( Boolean ) entry.getValue() ).booleanValue() )
         {
            result = "1" ;
         }
         else
         {
            result = "0" ;
         }
      }
      // multiple
      if ( entry.getType() == BIGInterface.MULTIPLE )
      {
         result = new String( ( String ) entry.getValue() ) ;
         result = "\"" + result.replaceAll( "\"" , "\"\"" ) + "\"" ;
      }
      // list
      if ( entry.getType() == BIGInterface.LIST )
      {
         result = ( ( BIGStrings ) entry.getValue() ).getCommaText() ;
      }
      // datetime
      if ( entry.getType() == BIGInterface.DATETIME )
      {
         result = "ignored" ;
      }
      // vector
      if ( entry.getType() == BIGInterface.VECTOR )
      {
         result = ( ( BIGStrings ) entry.getValue() ).getCommaText() ;
      }
      return result ;
   }

   private void printText( BIGStrings text )
   {
      Iterator it = text.iterator() ;
      while ( it.hasNext() )
      {
         System.out.println( ( String ) it.next() ) ;
      }
   }

   /**
    * Parses a given text into the database, BUT SHOULD NOT BE USED.
    * This is for testing only, because it adds also parameters which
    * are not in the config file. For adding all use:
    * <code>parse(text,dbFilename,BIGInterface.getInstance().getAllEntrynames());</code>
    *
    * @param   text            the text given as an <code>BIGStrings</code>
    *                          of <code>String</code>s
    * @param   dbFilename      the name of the file as it should
    *                          be stored in the database
    **/
   public void parse( BIGStrings text , String dbFilename )
       throws BIGParserException
   {
      if ( debug )
         System.out.println(
             "BIGDefFileParser: starting parsing on " + dbFilename ) ;

      BIGStrings errorText = new BIGStrings() ;
      Iterator textIterator = text.iterator() ;
      String line , name = null , value = "" ;

      while ( textIterator.hasNext() )
      {
         line = ( String ) textIterator.next() ;
         line.trim() ;
         // search for free texts
         if ( line.startsWith( "#start of " ) )
         {
            String tempLine ;
            tempLine = line.substring( "#start of ".length() ) ;
            tempLine.trim() ;
            name = tempLine ;
            while ( textIterator.hasNext() )
            {
               tempLine = ( String ) textIterator.next() ;
               if ( tempLine.equals( "#end of " + name ) )
                  break ;
               value = value + "\n" + tempLine ;
            }
         }
         else
         if ( ( !line.startsWith( "#" ) )
              && ( !line.equals( "" ) )
              && ( line.indexOf( "=" ) >= 0 ) )
         {
            // name extraction
            name = line.substring( 0 , line.indexOf( "=" ) ) ;
            while ( name.endsWith( " " ) )
               name = name.substring( 0 , name.length() - 1 ) ;

            // value extraction
            value = line.substring( line.indexOf( "=" ) +
                                    1 ) ;
            while ( value.startsWith( " " ) )
               value = value.substring( 1 ) ;
            while ( value.endsWith( " " ) )
               value = value.substring( 0 , value.length() - 1 ) ;
            // inseration in database
         }
         if ( name != null )
         {
            BIGEntry entry = db.getEntry( dbFilename , name ) ;
            if ( entry == null )
            {
               entry = new BIGEntry( name ) ;
               try
               {
                  entry.setValue( convertToType( value , entry ) ) ;
               }
               catch ( Exception e )
               {
                  entry.setValue( null ) ;
                  errorText.add( "type convertion failed on " + name ) ;
               }
               db.addEntry( dbFilename , entry ) ;
               if ( debug )
                  System.out.println(
                      "BIGDefFileParser: ##ADDED## "
                      + entry.getName()
                      + ":"
                      + entry.getValue() ) ;
            }
            else
            {
               try
               {
                  entry.setValue( convertToType( value , entry ) ) ;
               }
               catch ( Exception e )
               {
                  entry.setValue( null ) ;
                  errorText.add( "type convertion failed on " + name ) ;
               }
               if ( debug )
                  System.out.println(
                      "BIGDefFileParser: ##CHANGED## "
                      + entry.getName()
                      + ":"
                      + entry.getValue() ) ;
            }
            if ( debug ) System.out.println( "BIGDefFileParser: " + line +
                                             " -> [" +
                                             name + "]:=[" + value + "]" ) ;
         }
         else
         {
            if ( debug ) System.out.println( "BIGDefFileParser: " + line +
                                             " -> FILTERED" ) ;
         }
      }
      if ( errorText.size() > 0 )
      {
         throw new BIGParserException(
             "BIGDefFileParser:\n" + errorText.toString() ) ;
      }
      if ( debug )
         System.out.println( "BIGDefFileParser: work done on " + dbFilename ) ;
   }

   /**
    * Parses a given text into the database.
    *
    * @param   text            the text given as an <code>BIGStrings</code>
    *                          of <code>String</code>s
    * @param   dbFilename      the name of the file as it should
    *                          be stored in the database
    * @param   entrynames      a list of all names of entries that should be
    *                          recognized, all others are ignored
    **/
   public void parse( BIGStrings text , String dbFilename ,
                      BIGStrings entrynames )
       throws BIGParserException
   {
      if ( debug )
         System.out.println( "BIGDefFileParser: starting parsing on "
                             + dbFilename ) ;

      BIGStrings errorText = new BIGStrings() ;
      Iterator textIterator = text.iterator() ;
      int percentDone = 0 ;
      String line ;
      BIGInterface db = BIGInterface.getInstance() ;

      // filter some things
      int i = 0 ;
      while ( i < entrynames.size() )
      {
         BIGEntry entry = db.getEntry( dbFilename , entrynames.get( i ) ) ;
         if ( entry == null )
         {
            entrynames.remove( i ) ;
         }
         else
         {
            if ( entry.getType() == BIGInterface.NONE )
            {
               entrynames.remove( i ) ;
            }
            else
            {
               i++ ;
            }
         }
      }

      // set all entries to prototype
      Iterator defaultIt = entrynames.iterator() ;
      while ( defaultIt.hasNext() )
      {
         BIGEntry entry = db.getEntry( dbFilename , ( ( String ) defaultIt.next() ) ) ;
         if ( ( entry != null ) && ( entry.getPrototypeValue() != null ) )
         {
            entry.setValue( entry.getPrototypeValue() ) ;
         }
         else
         {
            switch ( entry.getType() )
            {
               case BIGInterface.NONE:
                  break ;
               case BIGInterface.BOOLEAN:
                  entry.setValue( Boolean.FALSE ) ;
                  break ;
               case BIGInterface.FLOAT:
                  entry.setValue( new Double( Double.NaN ) ) ;
                  break ;
               case BIGInterface.INTEGER:
                  entry.setValue( null ) ;
                  break ;
               case BIGInterface.LIST:
                  entry.setValue( new BIGStrings() ) ;
                  break ;
               case BIGInterface.MULTIPLE:
                  entry.setValue( new String() ) ;
                  break ;
               case BIGInterface.STRING:
                  entry.setValue( new String() ) ;
                  break ;
               case BIGInterface.VECTOR:
                  entry.setValue( new BIGStrings() ) ;
                  break ;
               case BIGInterface.FREE_TEXT:
                  entry.setValue( new String() ) ;
                  break ;

            }
         }
      }
      // e.g. name = BENCHIT_CC , value = gcc
      String name=null, value="";
      //long time=System.currentTimeMillis();
      //System.err.println("parse2:"+(System.currentTimeMillis()-time));
      while ( ( textIterator.hasNext() ) && ( entrynames != null ) )
      {
         name=null;
         value="";
         line = ( ( String ) textIterator.next() ).trim() ;
         percentDone++ ;
         progress.setProgress( ( 100 * percentDone ) / text.size() ) ;
         // look for free text
         if (line.startsWith("#start of "))
         {
            String tempLine ;
            tempLine = line.substring( "#start of ".length() ) ;
            tempLine.trim() ;
            name = tempLine ;
            while ( textIterator.hasNext() )
            {
               tempLine = ( String ) textIterator.next() ;
               if ( tempLine.equals( "#end of " + name ) )
                  break ;
               value = value  + tempLine +"\n" ;
            }
            // remove last \n
            tempLine=tempLine.substring(0,tempLine.length()-1);
         }
         else
         if ( ( !line.startsWith( "#" ) )
              && ( !line.equals( "" ) )
              && ( line.indexOf( "=" ) >= 0 ) )
         {
            // name extraction
            name = line.substring( 0 , line.indexOf( "=" ) ).trim() ;

            // value extraction
            value = line.substring( line.indexOf( "=" ) + 1 ).trim() ;
         }
            // inseration in database
            if ( entrynames.contains( name ) )
            {
               BIGEntry entry = db.getEntry( dbFilename , name ) ;
               if ( entry == null )
               {
                  entry = new BIGEntry( name ) ;

                  //try {
                  try
                  {
                     entry.setValue(
                         convertToType(
                             value , entry ) ) ;
                  }
                  catch (
                      NumberFormatException
                      ex )
                  {
                  }
                  catch (
                      BIGAccessViolationException
                      ex )
                  {
                  }
                  /*} catch (Exception e) {
                   //entry.setValue(null);
                   errorText.add("type convertion failed on " + name);
                         }*/
                  db.addEntry( dbFilename , entry ) ;
                  if ( debug )
                     System.out.println(
                         "BIGDefFileParser: ##ADDED## "
                         + entry.getName()
                         + ":"
                         + entry.getValue() ) ;
               }
               else
               {

                  //     try {
                  //System.err.println("parse3:"+(System.currentTimeMillis()-time));
                  try
                  {
                     entry.setValue(
                         convertToType(
                             value , entry ) ) ;
                  }
                  catch (
                      NumberFormatException
                      ex1 )
                  {
                  }
                  catch (
                      BIGAccessViolationException
                      ex1 )
                  {
                  }

                  //System.err.println("parse4:"+(System.currentTimeMillis()-time));
                  /*	} catch (Exception e) {
                    //entry.setValue(null);
                    errorText.add(
                     "type convertion failed on "
                      + name
                      + "\n["
                      + e
                      + "]");
                   }*/
                  if ( debug )
                     System.out.println(
                         "BIGDefFileParser: ##CHANGED## "
                         + entry.getName()
                         + ":"
                         + entry.getValue() ) ;
               }
            }
            if ( debug )
               System.out.println(
                   "BIGDefFileParser: "
                   + line
                   + " -> ["
                   + name
                   + "]:=["
                   + value
                   + "]" ) ;
               if ( name == null )
                  if ( debug )
                     System.out.println(
                         "BIGDefFileParser: " + line + " -> FILTERED" ) ;
      }

      //   System.err.println("parse2:"+(System.currentTimeMillis()-time));
      progress.setProgress( 100 ) ;
      if ( errorText.size() > 0 )
      {
         throw new BIGParserException(
             "BIGDefFileParser:\n" + errorText.toString() ) ;
      }
      if ( debug ) System.out.println( "BIGDefFileParser: work done on " +
                                       dbFilename ) ;
   }

   /**
    * Reads a Def-File in the database, BUT SHOULD NOT BE USED.
    * This is for testing only, because it adds also parameters which
    * are not in the config file. For adding all use:
    * <code>read(realfilename,dbFilename,BIGInterface.getInstance().getAllEntrynames());</code>
    *
    * @param   realFilename    the name of the file on the disk
    * @param   dbFilename      the name of the file as it should
    *                          be stored in the database
    **/
   public void read( String realFilename , String dbFilename )
       throws BIGParserException
   {
      BIGStrings text = new BIGStrings() ;
      try
      {
         text.readFromFile( realFilename ) ;
      }
      catch ( FileNotFoundException fnfe )
      {
         System.err.println(
             "BIGDefFileParser: File ["
             + realFilename
             + "] not found, skipping." ) ;
      }
      catch ( IOException ioe )
      {
         System.err.println(
             "BIGDefFileParser: Error during reading file ["
             + realFilename
             + "], skipping." ) ;
      }
      parse( text , dbFilename ) ;
   }

   /**
    * Reads a Def-File in the database.
    *
    * @param   realFilename    the name of the file on the disk
    * @param   dbFilename      the name of the file as it should
    *                          be stored in the database
    * @param   entrynames      a list of all names of entries that should be
    *                          recognized, all others are ignored
    **/
   public void read(
       String realFilename ,
       String dbFilename ,
       BIGStrings entrynames )
       throws BIGParserException
   {
      BIGStrings text = new BIGStrings() ;
      try
      {
         text.readFromFile( realFilename ) ;
      }
      catch ( FileNotFoundException fnfe )
      {
         System.err.println(
             "BIGDefFileParser: File ["
             + realFilename
             + "] not found, skipping." ) ;
      }
      catch ( IOException ioe )
      {
         System.err.println(
             "BIGDefFileParser: Error during reading file ["
             + realFilename
             + "], skipping." ) ;
      }
      parse( text , dbFilename , entrynames ) ;
   }

   /**
    * Renders a part of the database in given text.
    *
    * @param   originalText    the original text to render in given as an
    *                          <code>BIGStrings</code> of <code>String</code>s
    * @param   dbFilename      the name of the file in database what should
    *                          be rendered
    * @param   entrynames      a list of all names of entries that should be
    *                          recognized, all others are ignored
    **/
   public BIGStrings render(
       BIGStrings originalText ,
       String dbFilename ,
       BIGStrings entrynames )
       throws BIGParserException
   {
      // var declaration
      BIGStrings errorText = new BIGStrings() ;
      BIGInterface db = BIGInterface.getInstance() ;
      int i ;
      int lineNr = 0 ;
      // used for free_text
      int startLine=0,endLine=0;
      BIGStrings newText = new BIGStrings() ;
      BIGStrings prototypeText = new BIGStrings() ;
      BIGStrings doneEntrynames = new BIGStrings() ;
      String line ;
      if ( entrynames == null )
         entrynames = new BIGStrings() ;

      try
      {
         prototypeText.readFromFile( db.getBenchItPath() + "\\LOCALDEFS\\" +
                                     dbFilename.replaceAll( "<hostname>" ,
             "PROTOTYPE" ) ) ;
      }
      catch ( IOException e )
      {
         prototypeText = new BIGStrings() ;
      }

      // filter some things
      i = 0 ;
      while ( i < entrynames.size() )
      {
         BIGEntry entry = db.getEntry( dbFilename , entrynames.get( i ) ) ;
         if ( entry == null )
         {
            entrynames.remove( i ) ;
         }
         else
         {
            if ( entry.getType() == BIGInterface.NONE )
            {
               entrynames.remove( i ) ;
            }
            else
            {
               i++ ;
            }
         }
      }
      // normally replacing phase
      while ( lineNr < originalText.size() )
      {
         progress.setProgress( ( 50 * lineNr ) / originalText.size() ) ;
         line = originalText.get( lineNr ) ;
         line = line.trim() ;
         String name = null , value = "" ;
         if ( line.startsWith( "#start of " ) )
         {
            startLine=lineNr;
            String tempLine ;
            tempLine = line.substring( "#start of ".length() ) ;
            tempLine.trim() ;
            name = tempLine ;
            if (entrynames.contains(name))
            {
               boolean end = false ;
               while ( lineNr < originalText.size() && !end )
               {
                  lineNr++ ;
                  tempLine = ( String ) originalText.get( lineNr ) ;
                  if ( tempLine.equals( "#end of " + name ) )
                     end = true ;
                  else
                     value = value + tempLine + "\n" ;
               }
               // remove last \n
               endLine = lineNr ;
               tempLine = tempLine.substring( 0 , tempLine.length() - 1 ) ;
            }
         }
         else

         if ( ( !line.startsWith( "#" ) )
              && ( !line.equals( "" ) )
              && ( line.indexOf( "=" ) >= 0 ) )
         {
            // name extraction
            name = line.substring( 0 , line.indexOf( "=" ) ).trim() ;

            // value extraction
            value = line.substring( line.indexOf( "=" ) + 1 ).trim() ;
         }
            // replacing old value
            if ( entrynames.contains( name ) )
            {
               doneEntrynames.add( name ) ;
               BIGEntry entry = db.getEntry( dbFilename , name ) ;

               if ( entry == null )
               {
                  // should change value but there is no entry in database
                  errorText.add(
                      "should change a value what's not in database: "
                      + name ) ;
                  // so do nothing
                  newText.add( originalText.get( lineNr ) ) ;

               }
               else
               {
                  if ( entry.getType() == BIGInterface.FREE_TEXT )
                  {
                     try
                     {
                        newText.add( originalText.get( startLine ) ) ;
                        newText.add( "" + entry.getValue().toString() ) ;
                        newText.add( originalText.get( endLine ) ) ;
                     }
                     catch ( Exception ex )
                     {
                        ex.printStackTrace();
                     }
                  }
                  else
                  {
                     /*System.err.println( "Save A:" +
                                         originalText.get( lineNr ).substring(
                                             0 ,
                                             originalText.get( lineNr ).indexOf(
                         "=" ) + 1 )
                                         + convertToString( entry ));*/

                     newText.add(
                         originalText.get( lineNr ).substring(
                             0 ,
                             originalText.get( lineNr ).indexOf( "=" ) + 1 )
                         + convertToString( entry ) ) ;
                  }
                  if ( debug )
                     System.out.println(
                         "BIGDefFileParser: ##CHANGED## "
                         + entry.getName()
                         + ":"
                         + entry.getValue() ) ;
               }
            }
            else
            {
                // should not touch this value
                //System.err.println( originalText.get( lineNr ));
                if (name!=null)
                   newText.add( originalText.get( lineNr ) ) ;
            }
            if ( debug )
               System.out.println(
                   "BIGDefFileParser: "
                   + line
                   + " -> ["
                   + name
                   + "]:=["
                   + value
                   + "]" ) ;
         if (name==null)
         {
            newText.add( originalText.get( lineNr ) ) ;
            if ( debug )
               System.out.println(
                   "BIGDefFileParser: " + line + " -> FILTERED" ) ;
         }
         lineNr++ ;
      }

      // filter some things
      i = 0 ;
      while ( i < entrynames.size() )
      {
         BIGEntry entry = db.getEntry( dbFilename , entrynames.get( i ) ) ;
         if ( entry == null )
         {
            entrynames.remove( i ) ;
         }
         else
         {
            if ( ( ( entry.getValue() != null ) &&
                   ( entry.getValue().equals( entry.getPrototypeValue() ) ) )
                 || ( entry.getType() == BIGInterface.NONE ) )
            {
               entrynames.remove( i ) ;
            }
            else
            {
               i++ ;
            }
         }
      }
      // adding phase
      Iterator doneIterator = entrynames.iterator() ;
      String addName ;
      int percentDone = 0 ;
      while ( doneIterator.hasNext() )
      {
         percentDone++ ;
         progress.setProgress( ( ( 50 * percentDone ) / entrynames.size() ) +
                               50 ) ;
         addName = ( String ) doneIterator.next() ;

         if ( !doneEntrynames.contains( addName ) )
         {
            // searching for the correct position to add
            String name ;
            // first we search the position of the paramenter to in the prototype file
            int namePos = prototypeText.find( addName ) ;

            boolean found = false ;
            while ( ( !found ) && ( namePos > -1 ) )
            {
               line = prototypeText.get( namePos ).trim() ;
               if ( line.startsWith( "#start of "+addName ) )
               {
                  startLine = namePos ;
                  endLine=startLine;
                  while ( endLine < prototypeText.size() )
                  {
                     endLine++;
                     if (prototypeText.get( endLine ).trim().equals("#end of "+addName))
                        break;
                  }

                  found=true;
               }
               else

               if ( ( !line.startsWith( "#" ) )
                    && ( !line.equals( "" ) )
                    && ( line.indexOf( "=" ) >= 0 ) )
               {
                  // name extraction
                  name = line.substring( 0 , line.indexOf( "=" ) ).trim() ;
                  if ( name.equals( addName ) )
                  {
                     // found it
                     found = true ;
                  }
                  else
                  {
                     // wasn't the right one
                     namePos = prototypeText.find( addName , namePos + 1 ) ;
                  }
               }
               else
               {
                  // ups a comment or something
                  namePos = prototypeText.find( addName , namePos + 1 ) ;
               }
            }
            if ( !found )
            {
               // could not find paramenter in prototype, then add at the end
               BIGEntry entry = db.getEntry( dbFilename , addName ) ;
               if ( entry == null )
               {
                  // should change value but there is no entry in database
                  errorText.add(
                      "should add a value what's not in database: "
                      + addName ) ;
               }
               else
               {
                  if (entry.getType()==BIGInterface.FREE_TEXT)
                  {
                     newText.add("#start of "+entry.getName());
                     newText.add(entry.getValue().toString());
                     newText.add("#end of "+entry.getName());
                  }
                  else
                     newText.add( addName + "=" + convertToString( entry ) ) ;
               }
            }
            // if found
            else
            {
               // then we search the last marker like "set a+" in the prototype file
               found = false ;
               line = new String() ;
               int markerPos = namePos - 1 ;
               while ( ( !found ) && ( markerPos >= 0 ) )
               {
                  line = prototypeText.get( markerPos ).trim() ;
                  if ( line.startsWith( "#start of " + addName ) )
                     found = true ;
                  else
                  if ( ( !line.startsWith( "#" ) )
                       && ( !line.equals( "" ) )
                       && ( line.indexOf( "=" ) < 0 ) )
                  {
                     // ok it's not a comment, it's not an empty line and there is no =
                     // so it has to be something like a marker
                     found = true ;
                  }
                  else
                  {
                     markerPos-- ;
                  }
               }
               if ( !found )
               {
                  // ok no marker before variable so simply add it at the beginning
                  BIGEntry entry = db.getEntry( dbFilename , addName ) ;
                  if ( entry == null )
                  {
                     // should change value but there is no entry in database
                     errorText.add(
                         "should add a value what's not in database: "
                         + addName ) ;
                  }
                  else
                  {

                     newText.add(
                         0 ,
                         addName + "=" + convertToString( entry ) ) ;
                  }
               }
               else
               {
                  // now we have to search the marker in the new text
                  String markerName = new String( line ) ;
                  markerName.replaceAll( " " , "" ) ;
                  String orgMarker = new String( line ) ;
                  lineNr = 0 ;
                  boolean added = false ;
                  while ( ( lineNr < newText.size() ) && ( !added ) )
                  {
                     line = new String( newText.get( lineNr ) ).trim() ;
                     if ( line.startsWith( "#start of " + addName ) )
                     {
                        BIGEntry entry =
                            db.getEntry( dbFilename , addName ) ;
                        newText.add(
                                  lineNr + 1 ,convertToString( entry ));
                        added = true ;
                     }
                     else

                     if ( ( !line.startsWith( "#" ) )
                          && ( !line.equals( "" ) )
                          && ( line.indexOf( "=" ) < 0 ) )
                     {
                        // ok it's not a comment, it's not an empty line and there is no =
                        // so it has to be something like a marker
                        line.replaceAll( " " , "" ) ;
                        if ( markerName.equals( line ) )
                        {
                           // juhu marker found
                           added = true ;
                           BIGEntry entry =
                               db.getEntry( dbFilename , addName ) ;
                           if ( entry == null )
                           {
                              // should change value but there is no entry in database
                              errorText.add(
                                  "should add a value what's not in database: "
                                  + addName ) ;
                           }
                           else
                           {
                              newText.add(
                                  lineNr + 1 ,
                                  addName
                                  + "="
                                  + convertToString( entry ) ) ;
                           }
                        }
                        else
                        {
                           // was another marker
                           lineNr++ ;
                        }
                     }
                     else
                     {
                        lineNr++ ;
                     }
                  }
                  if ( !added )
                  {
                     // the marker doesn't exist -> create it
                     BIGEntry entry = db.getEntry( dbFilename , addName ) ;
                     if ( entry == null )
                     {
                        // should change value but there is no entry in database
                        errorText.add(
                            "should add a value what's not in database: "
                            + addName ) ;
                     }
                     else
                     {

                        newText.add( orgMarker ) ;
                        newText.add(
                            addName + "=" + convertToString( entry ) ) ;
                     }
                  }
               }
            }
         }
      }
      progress.setProgress( 100 ) ;
      if ( errorText.size() > 0 )
      {
         throw new BIGParserException(
             "BIGDefFileParser\n" + errorText.toString() ) ;
      }
      //if (debug) System.out.println("BIGDefFileParser - FULL TEXT\n" + newText);
      //System.err.println("BIGDefFileParser - FULL TEXT\n" + newText);
      return newText ;
   }

   /**
    * Writes a Def-File to the disk.
    *
    * @param   realFilename    the name of the file on the disk
    * @param   dbFilename      the name of the file as it is
    *                          stored in the database
    * @param   entrynames      a list of all names of entries that should be
    *                          recognized, all others are ignored
    **/
   public void write(
       String realFilename ,
       String dbFilename ,
       BIGStrings entrynames )
       throws BIGParserException
   {
      BIGStrings orgText = new BIGStrings() ;

      try
      {
         orgText.readFromFile( realFilename ) ;
      }
      catch ( Exception eOrg )
      {
         try
         {
            orgText.readFromFile(
                db.getBenchItPath()
                + "\\LOCALDEFS\\"
                + dbFilename.replaceAll( "<hostname>" , "PROTOTYPE" ) ) ;
         }
         catch ( Exception eProto )
         {
            orgText = new BIGStrings() ;
            orgText.add( "# this file is BIG generated and must not work" ) ;
         }
      }

      BIGStrings newText = render( orgText , dbFilename , entrynames ) ;

      try
      {
         newText.saveToFile( realFilename ) ;
      }
      catch ( Exception e )
      {
         throw new BIGParserException( "BIGDefFileParser:\n" + e.toString() ) ;
      }
   }

   public Observable getObservable()
   {
      return progress ;
   }

}
/*****************************************************************************
 Log-History

 *****************************************************************************/
