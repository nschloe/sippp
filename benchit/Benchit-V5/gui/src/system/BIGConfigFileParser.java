/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGConfigFileParser.java
 *
 *  Author: Robert Schoene
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.9 $
 *  $Date: 2007/05/08 09:43:30 $
 *
 ******************************************************************************/
package system ;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Calendar;
import java.util.Vector;

public class BIGConfigFileParser
{

   //this Vector contains the attributes names as Strings
   private Vector whichAttribute = new Vector() ;
   // this Vector contains the attributes values as Strings
   private Vector whichSet = new Vector() ;
   // the options dialog
   private gui.BIGOptionsDialog optionsDialog = null ;

   private File file = null ;

   // new Options stored as String[2]
   private Vector newOptions=new Vector();

   public BIGConfigFileParser( String datei )
   {
      // first we read the file (and store its important content to the Vectors)
      this.ReadIt( datei ) ;
      // then we save a link to the file in this.file
      this.file = new File( datei ) ;
   }

   public void showConfigDialog( java.awt.Frame f , gui.BIGKernelTree tree )
   {
      if ( optionsDialog == null )
      {
         optionsDialog = new gui.BIGOptionsDialog( f , whichAttribute ,
             whichSet ,
             tree ) ;
      }
      else
      {
         optionsDialog.setVisible( true ) ;
      }
   }

   /**
    * this method sets a new Value to a given attribute
    */

   public void set( String a , String b )
   {
      boolean foundIt = false ;
      // we search in the attribute-names
      for ( int i = 0 ; i < this.whichAttribute.size() ; i++ )
      {
         //until we found a
         if ( ( ( String )this.whichAttribute.get( i ) ).equals( a ) )
         {
            //now we set the content of the belonging entry in whichSet to b
            this.whichSet.setElementAt( b , i ) ;
            foundIt = true ;
            // and end the for loop
            break ;
         }
      }
      if ( !foundIt )
      { // it's a new attribute -> add it to the config file
         whichAttribute.add( a ) ;
         whichSet.add( b ) ;
      }
   }

   /*
    * this method rewrites the file with the given attributes
    */
   public int save()
   {
      BufferedReader in = null ;
      String line = new String() ;
      String cfgPath = BIGInterface.getInstance().getBenchItPath()
          + File.separator + "gui" + File.separator + "cfg"
          + File.separator ;
      // first we write the new content of BGUI.cfg to tmp.tmp
      String content=new String();
      try
      {
         // we read files with in
         in = new BufferedReader( new FileReader( this.file ) ) ;
         // and write files with out
         while ( ( line = in.readLine() ) != null )
         {
            // this String will contain the content before the first '#'
            String notAComment = "" ;
            // this String the conten from the first '#' to the end
            String comment = "" ;
            // if there is a '#'
            if ( line.indexOf( '#' ) != -1 )
            {
               //we divide the read line into notAComment
               notAComment = line.substring( 0 , line.indexOf( '#' ) ) ;
               // and the comment
               comment = line.substring( line.indexOf( '#' ) , line.length() ) ;
            }
            else
            {
               // elsewise the whole line is notAComment
               notAComment = line ;
            }
            // notAComment could be a blank line. Then we don't have to
            // parse the line
            if ( notAComment.length() > 0 )
            {
               // if there is no '=',we don't have to parse
               if ( ( notAComment.indexOf( '=' ) ) != -1 )
               {
                  // this contains all the ' 's and '\t's before the attributes name
                  String preName = "" ;
                  // the ' 's and '\t's after the attributes name
                  String afterName = "" ;
                  // this contains the attributes name
                  String name = notAComment.substring( 0 ,
                      notAComment.indexOf( '=' ) ) ;
                  // the whole as with name, but with the value/setting
                  String preSet = "" ;
                  String afterSet = "" ;
                  String set = notAComment.substring( notAComment.indexOf( '=' ) +
                      1 , notAComment.length() ) ;
                  //first we set the pren-/aftern-/N-ame
                  while ( name.startsWith( "\t" ) )
                  {
                     name = name.substring( 1 , name.length() ) ;
                     preName = preName + "\t" ;
                  }
                  while ( name.startsWith( " " ) )
                  {
                     name = name.substring( 1 , name.length() ) ;
                     preName = preName + " " ;
                  }
                  while ( name.endsWith( "\t" ) )
                  {
                     name = name.substring( 0 , name.length() - 1 ) ;
                     afterName = "\t" + afterName ;
                  }
                  while ( name.endsWith( " " ) )
                  {
                     name = name.substring( 0 , name.length() - 1 ) ;
                     afterName = " " + afterName ;
                  }
                  // then the whole for the value
                  while ( set.startsWith( "\t" ) )
                  {
                     set = set.substring( 1 , set.length() ) ;
                     preSet = preSet + "\t" ;
                  }
                  while ( set.startsWith( " " ) )
                  {
                     set = set.substring( 1 , set.length() ) ;
                     preSet = preSet + " " ;
                  }
                  while ( set.endsWith( "\t" ) )
                  {
                     set = set.substring( 0 , set.length() - 1 ) ;
                     afterSet = "\t" + afterSet ;
                  }
                  while ( set.endsWith( " " ) )
                  {
                     set = set.substring( 0 , set.length() - 1 ) ;
                     afterSet = " " + afterSet ;
                  }
                  //now we set the set to the actual set
                  set = this.stringCheckOut( name ) ;
                  // and rewrite the notAComment part of the line
                  notAComment = preName + name + afterName + "=" + preSet + set +
                      afterSet ;
               }
            }
            // we add the \n, that was cut out by readLine()
            line = notAComment + comment + "\n" ;
               content=content+line;

         }
         // we close the streams
         in.close() ;

      }
      catch ( Exception e )
      {
      }
      
      BIGFileHelper.saveToFile(content,this.file);
      // Now we write the content of tmp.tmp to the BGUI.cfg
      // comments??? Look at it!
      if ( newOptions.size() > 0 )
      {
         try
         {
            content = content + "# new entries from " +
                ( java.util.Calendar.getInstance() ).get( Calendar.YEAR ) + "/" +
                ( java.util.Calendar.getInstance() ).get( Calendar.MONTH ) +
                "/" +
                ( java.util.Calendar.getInstance() ).get( Calendar.DATE ) +
                "\n" ;
            for ( int i = 0 ; i < this.newOptions.size() ; i++ )
            {
               content = content + "\t " +
                   ( ( String[] ) newOptions.get( i ) )[ 0 ] +
                   " = " + stringCheckOut(( ( String[] ) newOptions.get( i ) )[ 0 ]) + "\n" ;
            }
         }
         catch ( Exception e )
         {
            e.printStackTrace() ;
         }
      }
      BIGFileHelper.saveToFile( content , this.file ) ;

      return 1 ;

   }

   /**
    * returns true, if the setting to the variable "whichToCheckOut" is >0
    * returns false, if the setting is 0
    * and an Exception if the setting is not an Integer or the variable "whichToCheckOut" does not exist
    */

   public boolean boolCheckOut( String whichToCheckOut )
       throws Exception
   {
      for ( int i = 0 ; i < this.whichAttribute.size() ; i++ )
      {
         if ( ( ( String ) ( this.whichAttribute.get( i ) ) ).equals(
             whichToCheckOut ) )
         {
            boolean returnBoolean = false ;
            try
            {
               int j = ( new Integer( ( ( String )this.whichSet.get( i ) ) ) ).
                   intValue() ;
               if ( j > 0 )
               {
                  returnBoolean = true ;
               }

            }
            catch ( Exception e )
            {
               if (this.stringCheckOut(whichToCheckOut).equals("true"))
                  return true;
               return false;
            }
            return returnBoolean ;
         }
      }
      throw new Exception() ;
   }

   /**
    * returns the value, if the setting to the variable "whichToCheckOut" is >0
    * and an Exception if the setting is not an Integer or there is no variable "whichToCheckOut"
    */

   public int intCheckOut( String whichToCheckOut )
       throws Exception
   {
      for ( int i = 0 ; i < this.whichAttribute.size() ; i++ )
      {
         if ( ( ( String ) ( this.whichAttribute.get( i ) ) ).equals(
             whichToCheckOut ) )
         {
            int returnInteger = 0 ;
            try
            {
               int j = ( new Integer( ( ( String ) whichSet.get( i ) ) ) ).
                   intValue() ;
               returnInteger = j ;

            }
            catch ( Exception e )
            {}
            return returnInteger ;
         }
      }
      throw new Exception() ;
   }
   /**
    * returns the value, if the setting to the variable "whichToCheckOut" is >0
    * and an Exception if the setting is not an Integer or there is no variable "whichToCheckOut"
    */

   public double doubleCheckOut( String whichToCheckOut )
       throws Exception
   {
      for ( int i = 0 ; i < this.whichAttribute.size() ; i++ )
      {
         if ( ( ( String ) ( this.whichAttribute.get( i ) ) ).equals(
             whichToCheckOut ) )
         {
            try
            {
               double j = ( new Double( ( ( String ) whichSet.get( i ) ) ) ).
                   doubleValue() ;
               return j ;

            }
            catch ( Exception e )
            {}
         }
      }
      throw new Exception() ;
   }


   /**
    * returns the setting to the variable "whichToCheckOut"
    * and an Exception if there is no variable "whichToCheckOut"
    */

   public String stringCheckOut( String whichToCheckOut )
       throws Exception
   {
      for ( int i = 0 ; i < this.whichAttribute.size() ; i++ )
      {
         if ( ( ( String ) ( this.whichAttribute.get( i ) ) ).equals(
             whichToCheckOut ) )
         {
            String returnString = new String() ;
            try
            {
               returnString = ( ( String ) whichSet.get( i ) ) ;
            }
            catch ( Exception e )
            {}
            return returnString ;
         }
      }
      throw new Exception() ;
   }

   public Color colorCheckOut( String whichToCheckOut )
       throws Exception
   {
      return new Color(this.intCheckOut(whichToCheckOut));
   }
   /*
   public Color getColorFromString(String s)
   {
      if (s.length()!=6)
         return null;
      String red=s.substring(0,2);
      String green=s.substring(2,4);
      String blue=s.substring(4,6);
      return new Color(getIntFromHex(red),getIntFromHex(green),getIntFromHex(blue));
   }
   public String getStringFromColor(Color c)
   {
      return getHexFromInt(c.getRed())+getHexFromInt(c.getGreen())+c.
      if (s.length()!=6)
         return null;
      String red=s.substring(0,2);
      String green=s.substring(2,4);
      String blue=s.substring(4,6);
      return new Color(getIntFromHex(red),getIntFromHex(green),getIntFromHex(blue));
   }

   private int getIntFromHex(String s)
   {
      int i=0;
      StringBuffer sb = new StringBuffer(s);
      while (sb.length()>1)
      {
         i=i*16;
         char c = sb.charAt(0);
         sb.deleteCharAt(0);
         switch (c)
         {
            case '0':
               break;
            case '1':
               i = i + 1 ;
            case '2' :
               i = i + 2 ;
               break;
            case '3' :
               i = i + 3 ;
               break;
            case '4' :
               i = i + 4 ;
               break;
            case '5' :
               i = i + 5 ;
               break;
            case '6' :
               i = i + 6 ;
               break;
            case '7' :
               i = i + 7 ;
               break;
            case '8' :
               i = i + 8 ;
               break;
            case '9' :
               i = i + 9 ;
               break;
            case 'A' :
               i = i + 10 ;
               break;
            case 'B' :
               i = i + 11 ;
               break;
            case 'C' :
               i = i + 12 ;
               break;
            case 'D' :
               i = i + 13 ;
               break;
            case 'E' :
               i = i + 14 ;
               break;
            case 'F' :
               i = i + 15 ;
               break;
            default:
               break;
         }
      }
      return i;
   }
   private String getHexFromInt(int s)
   {
      int i=0;
      int next=0;
      StringBuffer sb = new StringBuffer();
      boolean stop=false;
      while (!stop)
      {
         next=s%16;
         s=s/16;
         if (s==0) stop=true;
         switch (next)
         {
            case 0:
               sb.insert(0,'0');
               break;

            case 1:
               sb.insert(0,'1') ;
               break;
            case  2  :
               sb.insert(0,'2') ;
               break;
            case  3  :
               sb.insert(0,'3') ;
               break;
            case  4  :
               sb.insert(0,'4') ;
               break;
            case  5  :
               sb.insert(0,'5') ;
               break;
            case  6  :
               sb.insert(0,'6') ;
               break;
            case  7  :
               sb.insert(0,'7') ;
               break;
            case  8  :
               sb.insert(0,'8') ;
               break;
            case  9  :
               sb.insert(0,'9') ;
               break;
            case  10  :
               sb.insert(0,'A') ;
               break;
            case  11  :
               sb.insert(0,'B') ;
               break;
            case  12  :
               sb.insert(0,'C') ;
               break;
            case  13  :
               sb.insert(0,'D') ;
               break;
            case  14  :
               sb.insert(0,'E') ;
               break;
            case  15  :
               sb.insert(0,'F') ;
               break;
            default:
               break;
         }
      }
      return sb.toString();
   }
*/

   private void ReadIt( String datei )
   {
      whichAttribute=new Vector();
      whichSet=new Vector();
      // this will contain the content of a line
      String line ;
      try
      {
         // cleaning file (linebreaks :( )
         if (BIGInterface.getSystem()==BIGInterface.WINDOWS_SYSTEM)
         {
             String s=BIGFileHelper.getFileContent(new File(datei));
             if (s!= null)
             {
	             if (s.indexOf("\r\n")==-1)
	             {
	                 for (int i=0;i<s.length();i++)
	                 {
	                     if (s.charAt(i)=='\n')
	                     {
	                         s=s.substring(0,i)+"\r\n"+s.substring(i+1,s.length());
	                         i++;
	                     }
	                 }
	             }
	             BIGFileHelper.saveToFile(s,new File(datei));
             }
         }
         // he will read the file
         BufferedReader d = new BufferedReader( new FileReader( datei ) ) ;
         try
         {
            // to the end of the file
            while ( ( line = d.readLine() ) != null )
            {
               // we just parse the line, if there exists a '=' in the line
               if ( line.indexOf( '=' ) != -1 )
               {
                  // first we cut everything from the '#'
                  while ( line.indexOf( '#' ) != -1 )
                  {
                     line = line.substring( 0 , line.indexOf( '#' ) ) ;
                  }
                  // then we divide the remaining string
                  // (if there is a '=' in it)
                  if ( line.indexOf( '=' ) != -1 )
                  {
                     // to the attributes name
                     String addToAttribute = line.substring( 0 ,
                         line.indexOf( '=' ) ) ;

                     while ( addToAttribute.startsWith( "\t" ) )
                     {
                        addToAttribute = addToAttribute.substring( 1 ,
                            addToAttribute.length() ) ;
                     }
                     while ( addToAttribute.startsWith( " " ) )
                     {
                        addToAttribute = addToAttribute.substring( 1 ,
                            addToAttribute.length() ) ;
                     }
                     while ( addToAttribute.endsWith( "\t" ) )
                     {
                        addToAttribute = addToAttribute.substring( 0 ,
                            addToAttribute.length() - 1 ) ;
                     }
                     while ( addToAttribute.endsWith( " " ) )
                     {
                        addToAttribute = addToAttribute.substring( 0 ,
                            addToAttribute.length() - 1 ) ;
                     }

                     whichAttribute.add( addToAttribute ) ;
                     // and to the attributes value
                     String addToSet = line.substring( line.indexOf( '=' ) + 1 ,
                         line.length() ) ;
                     while ( addToSet.startsWith( "\t" ) )
                     {
                        addToSet = addToSet.substring( 1 , addToSet.length() ) ;
                     }
                     while ( addToSet.startsWith( " " ) )
                     {
                        addToSet = addToSet.substring( 1 , addToSet.length() ) ;
                     }
                     while ( addToSet.endsWith( "\t" ) )
                     {
                        addToSet = addToSet.substring( 0 ,
                            addToSet.length() - 1 ) ;
                     }
                     while ( addToSet.endsWith( " " ) )
                     {
                        addToSet = addToSet.substring( 0 ,
                            addToSet.length() - 1 ) ;
                     }
                     whichSet.add( addToSet ) ;
                  }
               }

            } //Ende while
         }
         catch ( IOException ignored )
         {}
      }
      catch ( FileNotFoundException notignored )
      {
         System.out.println( "file not found" ) ;
      }
   }

   public void addEntry(String entry,String setting)
   {
      this.whichAttribute.add(entry);
      this.whichSet.add(setting);
      String[] newOpts=new String[2];
      newOpts[0]=entry;
      newOpts[1]=setting;
      this.newOptions.add(newOpts);
   }
   public File getFile()
   {
      return this.file;
   }
}

