package plot ;

/**
 * <p>Überschrift: BenchIT</p>
 *
 * <p>Beschreibung: Represents an Object which can be plotted by BIGPlot</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organisation: ZHR TU Dresden</p>
 *
 * @author Robert Schoene
 * @version 1.0
 */
import org.jfree.chart.plot.XYPlot ;
import java.awt.*;
import javax.swing.*;
import java.util.*;
import system.BIGInterface;
public abstract class BIGPlotable
{
    /**
     * fill the shapes for yaxis 1 and 2?
     */
    public boolean fillShapes[]={false,false};
    /**
     * draw lines between shapes for yaxis 1 and 2?
     */
    public boolean drawLines[]={false,false};
    //-----------------------------------------------------------------
    /**
     * scale axis text for yaxis 1 and 2?
     */
    public boolean scaleYAxisText[]={false,false};
    //-----------------------------------------------------------------
   /**
    * text for this plotable
    */
   String text="";
   /**
    * how many ticks for yaxis? -1 means set automatically
    */
   int yTicks[]={-1};
   /**
    * how many ticks for yaxis default? -1 means set automatically
    */
   int yTicksDefault[]={-1};
   /**
    * how many ticks for xaxis? -1 means set automatically
    */
   int xTicks=-1;
   /**
    * how many ticks for xaxis default? -1 means set automatically
    */
   int xTicksDefault=-1;
   /**
    * logBase for yaxis. if <2 use numerical axis, not logarithmic
    */
   int[] yLog ={0};
   /**
    * default logBase for yaxis. if <2 use numerical axis, not logarithmic
    */
   int[] yLogDefault ={0};
   /**
    * logBase for xaxis. if <2 use numerical axis, not logarithmic
    */
   int xLog = 0 ;
   /**
    * default logBase for xaxis. if <2 use numerical axis, not logarithmic
    */
   int xLogDefault = 0 ;
   /**
    * displayedDataSets means:
    * data[displayedDataSets[0]] used for first yaxis,
    * data[displayedDataSets[1]] used for second yaxis
    */
   int[] displayedDataSets={0};
   /**
    * plot for this plotable
    */
   XYPlot xyplot=null;
   String title="";
   /**
    * label-text for xaxis
    */
   String xAxisText="";
   /**
    * label-texts for yaxis
    */
   String[] yAxisText=null;
   /**
    * default title
    */
   String titleDefault="";
   /**
    * default label-text for xaxis
    */
   String xAxisTextDefault="";
   /**
    * default label-texts for yaxis
    */
   String[] yAxisTextDefault=null;
   /**
    * data sets
    */
   BIGDataSet[] data=null;
   /**
    * colors for datasets/yaxis/...
    */
   Color[][] colors=null;
   /**
    * shapes for datasets/yaxis/...
    */
   Shape [][] shapes=null;
   /**
    * used for progress
    */
   JProgressBar progressBar = new JProgressBar() ;
   /**
    * used for progress
    */
   JLabel progressLabel = new JLabel() ;
   /**
    * which fonts are read from config
    */
   public final static String[] fontNames =
       {
       "plotComment" , "title" , "legend" , "XAxis" , "XAxisTick" ,
       "YAxis" , "YAxisTick"} ;
   /**
    * here the fonts are saved
    */
   Font[] fonts = new Font[fontNames.length ] ;
   /**
    * where to display the comment
    */
   double commentX = -1 ;
   double commentY=-1;
   /**
    * how large are the insets for the plot (space to borders to north,west,east and south (other orde))
    */
   java.awt.Insets insets=new java.awt.Insets(4,8,4,32);

   /**
    * must be implemented, add a function
    * @param xAndYValues double[][] double[2][] with [0][] as xValues and [1][] as yValues
    * @param yAxisName String yAxisName
    * @param name String name for the function
    * @return boolean true, whe added
    */
   abstract public boolean addFunction(double[][] xAndYValues,String yAxisName,String name);
   /**
    * must be implemented, remove a function
    * @param name String name of the function
    */
   abstract public void removeFunction(String name);
   /**
    * must be implemented, initialize plotable
    * @param f File where information is saved
    */
   public abstract void init(java.io.File f);
   /**
    * must be implemented, save plotable
    * @param f File where information shall be saved
    */
   abstract public void save(java.io.File f);
   
   //------
   /**
    * must be impemented
    */
   public abstract void changeName(int whichDataSet, int index, String newName);
   //------
   
   /**
    * initializes standards like fonts or insets
    * @param f File where to read these values from
    */
   public void initFonts(java.io.File f)
   {
      // default insets
      try
      {

         insets = new Insets(
             BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "plotInsetsTop" ) ,
             BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "plotInsetsLeft" ) ,
             BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "plotInsetsBottom" ) ,
             BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "plotInsetsRight" )
             ) ;
         // use standard if exception
      }
      catch ( Exception ex )
      {
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "plotInsetsTop" , "4" ) ;
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "plotInsetsLeft" , "8" ) ;
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "plotInsetsBottom" , "4" ) ;
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "plotInsetsRight" , "32" ) ;
         insets = new Insets( 4 , 8 , 4 , 32 ) ;
      }
      // default fillShapes and drawLines
      try
      {

             fillShapes[0]=BIGInterface.getInstance().getBIGConfigFileParser().
             boolCheckOut( "shapes1Filled" ) ;
             drawLines[0]=BIGInterface.getInstance().getBIGConfigFileParser().
             boolCheckOut( "shapes1Lines" ) ;
             fillShapes[1]=BIGInterface.getInstance().getBIGConfigFileParser().
             boolCheckOut( "shapes2Filled" );
             drawLines[1]=BIGInterface.getInstance().getBIGConfigFileParser().
             boolCheckOut( "shapes2Lines" );
         // use standard if exception
      }
      catch ( Exception ex )
      {
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "shapes1Filled" , "0" ) ;
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "shapes1Lines" , "0" ) ;
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "shapes2Filled" , "0" ) ;
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
             "shapes2Lines" , "0" ) ;
      }
      
      //-----------------------------------------------------------------
      // default scaleYAxisText
      try
      {
    	  scaleYAxisText[0]=BIGInterface.getInstance().getBIGConfigFileParser().
    	  boolCheckOut( "yAxisText1Scaled" );
    	  scaleYAxisText[1]=BIGInterface.getInstance().getBIGConfigFileParser().
    	  boolCheckOut( "yAxisText2Scaled" );
      }
      catch ( Exception ex )
      {
    	  BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
    			  "yAxisText1Scaled" , "0" );
    	  BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
    			  "yAxisText2Scaled" , "0" );
      }
      //-----------------------------------------------------------------

      // read standard fonts
      Font temp=null;
      java.io.File std=BIGInterface.getInstance().getBIGConfigFileParser().getFile();
      for (int i=0;i<fonts.length;i++)
      {
         fonts[i]=system.BIGUtility.getFont(fontNames[i],std);

      }


         this.commentX = .8 ;
         try
         {
            this.commentX = BIGInterface.getInstance().
                getBIGConfigFileParser().intCheckOut( "plotCommentPercentX" ) *
                .01 ;
         }
         catch ( Exception ex )
         {
         }
         this.commentY = .9 ;
         try
         {
            this.commentY = BIGInterface.getInstance().
                getBIGConfigFileParser().intCheckOut( "plotCommentPercentY" ) *
                .01 ;
         }
         catch ( Exception ex )
         {
         }
         try
         {
            this.setAnnComment( BIGInterface.getInstance().
                                getBIGConfigFileParser().stringCheckOut(
                "plotComment" ) ) ;
         }
         catch ( Exception ex3 )
         {
         }
      // read fonts from f
      if ((f!=null)&&(f.exists()))
      {
         // check whether to use std fonts
         boolean standardFont=false;
         try
         {
            standardFont = BIGInterface.getInstance().
                getBIGConfigFileParser().
                boolCheckOut( "useStandardFonts" ) ;
         }
         catch ( Exception ex2 )
         {
            BIGInterface.getInstance().getBIGConfigFileParser().addEntry("useStandardFonts","0");

         }
         if (!standardFont)
         for ( int i = 0 ; i < fonts.length ; i++ )
         {
            temp = system.BIGUtility.getFont( fontNames[ i ] , f ) ;
            if ( temp != null )
               fonts[ i ] = temp ;
         }
         system.BIGConfigFileParser p=new system.BIGConfigFileParser(f.getAbsolutePath());
         try
         {
            this.setCommentPos(p.intCheckOut( "plotCommentPercentX" ),
                p.intCheckOut( "plotCommentPercentY" ));
            this.setAnnComment(p.stringCheckOut( "plotComment" ));
         }
         catch ( Exception ex1 )
         {
         }

         // and now the insets (space around the plot itself)
         try
         {

            insets = new java.awt.Insets(
                p.intCheckOut( "plotInsetsTop" ) ,
                p.intCheckOut( "plotInsetsLeft" ) ,
                p.intCheckOut( "plotInsetsBottom" ) ,
                p.intCheckOut( "plotInsetsRight" )
                ) ;
            // use standard if exception
         } catch (Exception ignored){}

         try
         {

                fillShapes[0]=p.boolCheckOut( "shapes1Filled" ) ;
                drawLines[0]=p.boolCheckOut( "shapes1Lines" ) ;
                fillShapes[1]=p.boolCheckOut( "shapes2Filled" );
                drawLines[1]=p.boolCheckOut( "shapes2Lines" );
            // use standard if exception
         }
         catch ( Exception ignored )
         {
         }
         //--------------------------------------------
         try
         {
        	 scaleYAxisText[0]=p.boolCheckOut( "yAxisText1Scaled" );
        	 scaleYAxisText[1]=p.boolCheckOut( "yAxisText2Scaled" );
         }
         catch (Exception ignored )
         {
        	 // use standard if exception
         }
         //---------------------------------------------

      }
      // if fonts could be initialized nowhere: use standard SansSerif,BOLD,14
      for (int i=0;i<fonts.length;i++)
      {
         if (fonts[i]==null)
         {
        	if (i == 1)
        	{
        		// title-font gets a size of 16pt
        		fonts[i]=new Font("SansSerif", Font.BOLD, 16);
        	}
        	else
        	{
        		// all other fonts get a size of 14pt
        		fonts[i]=new Font("SansSerif", Font.BOLD, 14);
        	}
         }
      }
   }
   /**
    * append information about fonts, insets... (everything in initFonts)
    * @param sb StringBuffer where to add the information
    */
   public void appendFontInformation(StringBuffer sb)
   {
      // fonts
      sb.append("\n#font informations\n");
      boolean standardFont = false;
      Font localFonts[] = this.fonts;
      try
      {
         standardFont = BIGInterface.getInstance().
             getBIGConfigFileParser().
             boolCheckOut( "useStandardFonts" ) ;
      }
      catch ( Exception ex2 )
      {
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry("useStandardFonts","0");

      }
      if (standardFont)
      {
    	  Font temp;
    	  for ( int i = 0 ; i < fonts.length ; i++ )
          {
             temp = system.BIGUtility.getFont( fontNames[ i ] , this.getSaveFile() ) ;
             if ( temp != null )
                localFonts[ i ] = temp ;
          }
      }
      for (int i=0;i<this.fonts.length;i++)
      {
         sb.append(this.fontNames[i]+"Font="+localFonts[i].getName()+"\n");
         sb.append(this.fontNames[i]+"FontStyle="+localFonts[i].getStyle()+"\n");
         sb.append(this.fontNames[i]+"FontSize="+localFonts[i].getSize()+"\n");
      }
      // comment
      sb.append("\n# plotComment\n");
      sb.append("plotComment="+this.annComment+"\n");
      sb.append("plotCommentPercentX="+this.getCommentXPercent()+"\n");
      sb.append("plotCommentPercentY="+this.getCommentYPercent()+"\n");
      // insets
      sb.append("\n# Insets\n");
      sb.append("plotInsetsTop="+this.xyplot.getInsets().top+"\n");
      sb.append("plotInsetsLeft="+this.xyplot.getInsets().left+"\n");
      sb.append("plotInsetsBottom="+this.xyplot.getInsets().bottom+"\n");
      sb.append("plotInsetsRight="+this.xyplot.getInsets().right+"\n");
      // shapes and lines
      sb.append("\n# Shapes\n");
      sb.append("shapes1Filled=");
      if(fillShapes[0])
         sb.append("1\n");
      else
         sb.append("0\n");
      sb.append("shapes1Lines=");
      if(drawLines[0])
         sb.append("1\n");
      else
         sb.append("0\n");
      //---------------------------------------
      sb.append("yAxisText1Scaled=");
      if(scaleYAxisText[0])
    	  sb.append("1\n");
      else
    	  sb.append("0\n");
      //---------------------------------------
      sb.append("shapes2Filled=");
      if(fillShapes[1])
         sb.append("1\n");
      else
         sb.append("0\n");
      sb.append("shapes2Lines=");
      if(drawLines[1])
         sb.append("1\n");
      else
         sb.append("0\n");
      //---------------------------------------
      sb.append("yAxisText2Scaled=");
      if (scaleYAxisText[1])
    	  sb.append("1\n");
      else
    	  sb.append("0\n");
      //---------------------------------------


   }
   /**
    * get the number of funtions from a dataset
    * @param i int which dataset to use
    * @return int length of data[i] or 0 (if data[i] is not available)
    */
   public int getNumberOfFunctionsForDataset(int i)
   {
      // if !exists
      if (this.data.length<=i||i<0)
         // return 0;
         return 0;
      else
         // else return length
         return data[i].getSeriesCount();
   }
   /**
    * gets the number of total functions used in this plotable
    * @return int number of functions
    */
   public int getTotalFunctions()
   {
      // if no data return 0
      if (data==null)
         return 0;
      // else add sizes for single datasets
      int size=0;
      for (int i=0;i<data.length;i++)
         size=size+data[i].getSeriesCount();
      return size;
   }
   /**
    * gets y-Values for a specific function
    * @param functionName String name of the function
    * @return double[] y Values
    */
   public double[] getDataForFunction(String functionName)
   {
      // not avail? return nothing
      if (data==null)
         return new double[0];
      // find function
      for (int i=0;i<data.length;i++)
      {
         for ( int j = 0 ; j < data[ i ].getSeriesCount() ; j++ )
         {
            // return data
            if ( data[ i ].getSeriesName( j ).equals( functionName ) )
               return data[ i ].getValues()[ j + 1 ] ;
         }
      }
      // not found return nothing
      return new double[0];
   }
   /**
    * renames a specific function
    * @param oldName String old name
    * @param newName String new name
    */
   public void rename(String oldName,String newName)
   {
      // not avail? do nothing
      if (data==null)
         return ;
      // find it
      for (int i=0;i<data.length;i++)
         for (int j=0;j<data[i].getSeriesCount();j++)
         {
            if ( data[ i ].getSeriesName( j ).equals( oldName ) )
            {
               // rename it
               data[ i ].setSeriesName( j , newName ) ;
               return;
            }
         }


   }
   /**
    * sets progressBar and progressLabel (used for init once for guis progress)
    * @param progressBar JProgressBar progressbar from gui
    * @param progressLabel JLabel progressLabel from gui
    */
   public void setProgress(JProgressBar progressBar,JLabel progressLabel)
   {
      this.progressBar=progressBar;
      this.progressLabel=progressLabel;
   }
   /**
    * gets the annotation for the plot
    * @return XYTextAnnotation
    */
   public org.jfree.chart.annotations.XYTextAnnotation getAnnotationComment(  )
   {
       // get the font for annot.
       Font f=this.getFont("plotComment");
       // not found? use std.
       if (f==null)
         f=system.BIGUtility.getFont("plotComment");
      // compute position
      // procentual between min and max
      double xMax=this.xyplot.getHorizontalValueAxis().getMaximumAxisValue();
      double xMin=this.xyplot.getHorizontalValueAxis().getMinimumAxisValue();
      double yMax=this.xyplot.getVerticalValueAxis().getMaximumAxisValue();
      double yMin=this.xyplot.getVerticalValueAxis().getMinimumAxisValue();
      // where to display (value at the axis)
      double xComment= xMin+( xMax - xMin ) * this.commentX;
      double yComment= yMin+( yMax - yMin ) * this.commentY;/*
      if (this.xLog>1)
      {
         xComment=xMin+Math.pow(xLog,(Math.log(xMax)-Math.log(xMin))*this.commentX);
      }
      if (this.yLog[0]>1)
      {
         yComment=yMin+(yMax-yMin)*Math.pow(this.yLog[0],this.commentY );
      }
      System.err.println(xComment+","+yComment+","+Math.log(this.commentX )+","+Math.log(this.commentY ));
      */// set position, name, font
      org.jfree.chart.annotations.XYTextAnnotation an = new org.jfree.chart.
          annotations.XYTextAnnotation( this.getExtComment() , f ,
                                        xComment ,
                                        yComment ) ;
     return an;
   }
   private String annComment="";
   /**
    * sets the comment for the plot
    * @param s String comment text
    */
   public void setAnnComment(String s)
   {
      // remove beginnig and ending whitespaces
      // save orig
      this.annComment=s.trim();
      // real name will be written here
      StringBuffer sb=new StringBuffer();
      // increments over characters
      for (int i=0;i<s.length();i++)
      {
         // starting a flag like <processorname>
         if ( s.charAt( i ) != '<' )
            sb.append( s.charAt( i ) ) ;
         // ending this flag
         else
         {
            if ( s.indexOf( ">" , i ) == -1 )
               sb.append( s.charAt( i ) ) ;
            else
            {
               // gets the setting for e.g. processorname
               sb.append( getSetting( s.substring( i+1 , s.indexOf( ">" , i ) ) ) ) ;
               i = s.indexOf( ">" , i ) ;
            }
         }
      }
      // save parsed
      pComment=sb.toString();
   }
   /**
    * gets the parsed commet. if not avail, use stdComment
    * @return String
    */
   public String getExtComment()
   {
      if (pComment!=null)
         return pComment;
      return annComment;
   }
   /**
    * get std. Comment (not parsed)
    * @return String
    */
   public String getAnnComment()
   {
      return annComment;
   }
   /**
    * get x-pos (percentual) for comment
    * @return int eg 56 for 56%
    */
   public int getCommentXPercent()
   {
      return Math.round((float)(this.commentX*100.0));
   }
   /**
    * get y-pos (percentual) for comment
    * @return int eg 56 for 56%
    */

   public int getCommentYPercent()
   {
      return Math.round((float)(this.commentY*100.0));
   }
   /**
    * sets position for comment (percentual, e.g. 61 for 61%)
    * @param x int new x-Pos
    * @param y int new y-Pos
    */
   public void setCommentPos(int x, int y)
   {
      this.commentX=(1.0*x)*0.01;
      this.commentY=(1.0*y)*0.01;
   }

   /**
    * get parsed title. if not avail, get orig title
    * @return String
    */
   public String getExtTitle()
   {
      if (pTitle!=null)
         return pTitle;
      return title;

   }
   /**
    * set title to s
    * @param s String new title
    */
   public void setTitle(String s)
   {
      // see setComment
      this.title=s.trim();

      StringBuffer sb=new StringBuffer();
      for (int i=0;i<s.length();i++)
      {
         if ( s.charAt( i ) != '<' )
            sb.append( s.charAt( i ) ) ;
         else
         {
            if ( s.indexOf( ">" , i ) == -1 )
               sb.append( s.charAt( i ) ) ;
            else
            {
               sb.append( getSetting( s.substring( i+1 , s.indexOf( ">" , i ) ) ) ) ;
               i = s.indexOf( ">" , i ) ;
            }
         }
      }
      pTitle=sb.toString();
   }
   /**
    * get setting: used for get setting(processorname) or get(processorclockrate|5|2)
    * 5 means shift it (must be a number!) 5 positions 2 means set numbers after
    * point to 2
    * @param temp String name of the item in the resultfile
    * @return String setting of the item
    */
   private String getSetting(String temp)
   {
      // sth wrong
      boolean error=false;
      // how many shifts
      int shift=0;
      // after point
      int after=0;
      // if there is an |
      if (temp.indexOf('|')>-1)
      {
         // not at the beginning
         if (temp.lastIndexOf('|')>1)
         {
            // at least two different |'s
            if (temp.lastIndexOf('|')>temp.indexOf('|'))
            {
               // get shifts
               try
               {
                  shift = Integer.parseInt( temp.substring( temp.indexOf( '|' ) +
                      1 , temp.lastIndexOf( '|' ) ) ) ;
               }
               catch ( NumberFormatException ex )
               {
                  System.err.print("Argument 2 must be an Integer.");
                  error=true;
               }
               // get after-point
               try
               {
                  after = Integer.parseInt( temp.substring( temp.lastIndexOf( '|' ) +
                      1 , temp.length() ) ) ;
               }
               catch ( NumberFormatException ex )
               {
                  System.err.print("Argument 3 must be an Integer.");
                  error=true;
               }
               // item before first |
               temp=temp.substring(0,temp.indexOf('|'));
            }
            else
            {
               // just one |
               error=true;
            }
         }
      }
      if (error)
      {
         // display usage information
         System.err.println( "Wrong usage" ) ;
         System.err.println( "Example: \"<processorclockrate|-9|1> \" means " ) ;
         System.err.println(
             "         print in GHz (/10^9) with one digit after point." ) ;
         System.err.println("Or simple: <processorname>");
      }
      // you cant display less then no numbers after the point
      if (after<0)
         after=0;
      // get the setting for the item
      String s = plot.BIGOutputParser.getValue( temp , text ) ;
      // if it wasn't found: return <itemname>
      if ( s.equals( "" ) )return '<' + temp + '>' ;
      // shift?
      if ( shift != 0 )
      {
         // if there was an error do nothin
         if ( !error )
         {
            // used for 1!E10!
            String e = null ;
            // setting as number
            double d = 0.0 ;
            try
            {
               d = Double.parseDouble( s ) ;
            }
            catch ( NumberFormatException ex1 )
            {
               System.err.println( "The setting for " + temp +
                                   " is not a  number." ) ;
               return s ;
            }
            // shift it according to to setted shift
            d = d * Math.pow( 10.0 , 1.0 * shift ) ;
            s = "" + d ;
            // remove the E10 part
            if ( s.indexOf( 'E' ) > -1 )
            {
               e = s.substring( s.indexOf( 'E' ) ) ;
               s = s.substring( 0 , s.indexOf( 'E' ) ) ;
            }
            // removing ending numbers (after point) according to  setted after
            int commaPos = s.indexOf( '.' ) ;
            if ( ( commaPos == -1 ) && ( after != 0 ) )
               s = s + "." ;
            commaPos = s.indexOf( '.' ) ;
            if ( commaPos > -1 )
            {
               // cut
               if ( ( s.length() - 1 - commaPos ) > after )
               {
                  s = s.substring( 0 , commaPos + after + 1 ) ;
               }
               else
               {
                  //append zeros
                  if ( ( s.length() - 1 - commaPos ) < after )
                  {
                     int x = after - ( s.length() - commaPos ) + 1 ;
                     for ( int i = 0 ; i < x ; i++ )
                        s = s + "0" ;

                  }
               }
               if ( s.charAt( s.length() - 1 ) == '.' )
                  s = s.substring( 0 , s.length() - 1 ) ;
            }
            // add the E10 part
            if ( e != null )
            {
               s = s + e ;
            }
         }
      }
      // return the setting
      return s ;
   }
   /**
    * sets the default-configs-standard-title as title
    */
   public void setStandardTitle()
   {

      try
      {

         String t= BIGInterface.getInstance().
                          getBIGConfigFileParser().stringCheckOut(
             "standardTitle" );
         if (t.trim().length()==0)
            return;
         this.setTitle( t ) ;

      }
      catch ( Exception ex )
      {
      }

   }
   /**
    * get the font for the item with name which or null
    * @param which String name
    * @return Font font for item
    */
   public Font getFont(String which)
   {
      for (int i=0;i<this.fontNames.length;i++)
         if (fontNames[i].equals(which))
         {
            return fonts[ i ] ;
         }

      return null;
   }
   /**
    * get all saved fonts
    * @return Font[] all saved fonts
    */
   public Font[] getAllFonts ()
   {
	   return fonts;
   }
   /**
    * sets the font for a specific item
    * @param which String item name
    * @param f Font font for item
    */
   public void setFont(String which,Font f)
   {
      for (int i=0;i<this.fontNames.length;i++)
         if (fontNames[i].equals(which))
          fonts[i]=f;
      return ;
   }
   /**
    * should return the File this plotable is saving to
    * @return File
    */
   public abstract java.io.File getSaveFile();

   // parsed comment
   private String pComment=null;
   // parsed title
   private String pTitle=null;

   private Vector annots=new Vector();
   public void addAnnotation(String text, double x, double y)
   {
      annots.add(new org.jfree.chart.annotations.XYTextAnnotation(text,x,y));
   }
}

