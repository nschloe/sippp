package plot ;

import java.io.File ;
import java.util.* ;
import system.* ;
import java.awt.Color ;
import java.awt.datatransfer.*;
import java.io.IOException ;
import java.awt.Shape;

/**
 *
 * <p>Überschrift: BenchIT - BIGOutputFile</p>
 *
 * <p>Beschreibung: Contains a lot of information to an outputfile</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organisation: ZHR TU Dresden</p>
 *
 * @author Robert Schoene
 * @version 1.0
 */
public class BIGOutputFile
    extends BIGPlotable implements Transferable
{
   /**
    * output file (ends with .bit)
    */
   private File outputFile ;
   /**
    * type (e.g.numerical)
    */
   private String type ;
   /**
    * name (e.g.matmul)
    */

   private String name ;
   /**
    * name (e.g.C)
    */
   private String language ;
   /**
    * parallel libs (e.g.MPI)
    */
   private String parLibs ;
   /**
    * other libs (e.g. glut)
    */
   private String libs ;
   /**
    * type / datatype (e.g. double)
    */
   private String dataType ;
   /**
    * name of the file without the .bit
    */
   private String fileNameWithoutExtension ;
   /**
    * when functions for different axis are read,
    * we need to know the order of the original
    * output-file. This is saved here.
    * file.bit y<order[1]>name means file.bit.gui y2name
    * (1 because the count starts in JAVA with 0, in the
    * bit-file with 1)
    * Also the order-entry is modified:
    * <numberofaxis-1>*100+order
    */
   int[] order=new int[0];
   /**
    * build a new BIGOutputFile by parsing the file f
    * @param f File
    */
   public BIGOutputFile( File f )
   {
      // setting member variables
      this.outputFile = f ;
      this.fileNameWithoutExtension = f.getName().substring( 0 ,
          f.getName().lastIndexOf( "." ) ) ;
      File temp = f.getParentFile() ;
      dataType = temp.getName() ;
      temp = temp.getParentFile() ;
      libs = temp.getName() ;
      temp = temp.getParentFile() ;
      parLibs = temp.getName() ;
      temp = temp.getParentFile() ;
      language = temp.getName() ;
      temp = temp.getParentFile() ;
      name = temp.getName() ;
      temp = temp.getParentFile() ;
      type = temp.getName() ;
   }

   /**
    * gets filename without .bit
    * @return String filename without .bit
    */
   public String getFilenameWithoutExtension()
   {
      return this.fileNameWithoutExtension ;
   }

   /**
    * gets the bit file
    * @return File the bit file for this
    */
   public File getFile()
   {
      return this.outputFile ;
   }

   /**
    * gets the name of the algorithm
    * @return String name of algorithm
    */
   public String getName()
   {
      return name ;
   }

   /**
    * gets the type of the algorithm
    * @return String type of algorithm
    */
   public String getType()
   {
      return this.type ;
   }

   /**
    * gets the other libraries of the algorithm
    * @return String other libraries of algorithm
    */
   public String getLibraries()
   {
      return this.libs ;
   }

   /**
    * gets the parallel libraries of the algorithm
    * @return String parallel libraries of algorithm
    */
   public String getParallelLibraries()
   {
      return this.parLibs ;
   }

   /**
    * gets the programming language of the algorithm
    * @return String programming language of algorithm
    */
   public String getSourceLanguage()
   {
      return this.language ;
   }

   /**
    * gets the datatype of the algorithm
    * @return String datatype of algorithm
    */
   public String getDataType()
   {
      return this.dataType ;
   }

   public void removeSavedInformations()
   {
      if ( ( new File( this.outputFile.getAbsolutePath() + ".gui" ) ).exists() )
      {
         ( new File( this.outputFile.getAbsolutePath() + ".gui" ) ).delete() ;
      }
   }

   /**
    * gets the names of the settings with the "sorting"-part leading
    * @param sorting int sort from which
    * @return String[] names for settings
    */
   public String[] getSortedNames( int sorting )
   {
      String[] sortedNames = new String[6 ] ;
      // initial setting
      sortedNames[ 0 ] = type ;
      sortedNames[ 1 ] = name ;
      sortedNames[ 2 ] = language ;
      sortedNames[ 3 ] = parLibs ;
      sortedNames[ 4 ] = libs ;
      sortedNames[ 5 ] = dataType ;
      // sort
      String tempString = sortedNames[ sorting ] ;
      for ( int i = sorting ; i > 0 ; i-- )
         sortedNames[ i ] = sortedNames[ i - 1 ] ;
      sortedNames[ 0 ] = tempString ;
      return sortedNames ;
   }

   /**
    * gets the name of the algorithm with leading setting i
    * @param sorting int which part shall lead the sorting
    * @return String the sorted name of the algorithm
    */
   public String getNameAfterSorting( int sorting )
   {
      // get parts
      String[] names = this.getSortedNames( sorting ) ;
      String name = new String() ;
      // combine them with dots
      for ( int i = 0 ; i < names.length ; i++ )
      {
         name = name + "." + names[ i ] ;
      }
      return name.substring( 1 ) ;
   }

   /**
    * used for JTree
    * @return String returns filename without extension
    */
   public String toString()
   {
      return this.fileNameWithoutExtension ;
   }

   /**
    * parses output-file for "comment=" and returns setting (used for popup)
    * @return String the setting for comment
    */
   public String getComment()
   {
      return BIGOutputParser.getValue( "comment" ,
                                       system.BIGFileHelper.getFileContent( this.
          getFile() ) ) ;

   }

   /**
    * saves .bit.gui-file with settings
    * @param f File
    */
   public void save( File f )
   {
      // if the bit file doesnt exist, dont save gui file
      if ( !this.outputFile.exists() )
         return ;
      // if this plot is empty dont save
      if ( this.data == null )
         return ;
      if ( this.data[ 0 ] == null )
         return ;
      // content for gui file
      StringBuffer sb = new StringBuffer( "#autoGenerated by BIGGUI\n" ) ;
      int whichFunction = 1 ;
      // write data, which is needed once
      sb.append( "title=" + title + "\n" ) ;
      sb.append( "xaxistext=" + this.xAxisText + "\n" ) ;
      sb.append( "xoutmin=" +
                 data[ 0 ].getMinimumDomainValue() + "\n" ) ;
      sb.append( "xoutmax=" +
                 data[ 0 ].getMaximumDomainValue() + "\n" ) ;
      sb.append( "xaxislogbase=" + this.xLog + "\n" ) ;
      sb.append( "xaxisticks=" + this.xTicks + "\n" ) ;
      // now the yaxis/funtion data
      for ( int k = 0 ; k < order.length ; k++ )
      {
         // which dataset
         int i=order[k]/100;
         // which function
         int j=order[k]%100;
         int howManyBefore=0;
         for (int l=0;l<k;l++)
         {
            if (order[l]/100!=i)
               howManyBefore++;
         }
         j=j-howManyBefore-1;
         // pre means nano/milli...
         int pre = data[ i ].getPre() ;
         data[ i ].setPre( 3 ) ;

            //System.err.println(whichFunction+"       "+i+"  "+j+"  "+order[whichFunction-1]);
            sb.append( "y" + order[whichFunction-1]%100 + "axistext=" + this.yAxisText[ i ] +
                       "\n" ) ;
            // System.out.println(this.yAxisText[i]);
            sb.append( "y" + order[whichFunction-1]%100 + "outmin=" +
                       data[ i ].getMinimumRangeValue() + "\n" ) ;
            sb.append( "y" + order[whichFunction-1]%100 + "outmax=" +
                       data[ i ].getMaximumRangeValue() + "\n" ) ;
            sb.append( "y" + order[whichFunction-1]%100 + "axislogbase=" + this.yLog[ i ] +
                       "\n" ) ;
            sb.append( "y" + order[whichFunction-1]%100 + "axisticks=" + this.yTicks[ i ] +
                       "\n" ) ;
            sb.append( "tlegendfunction" + order[whichFunction-1]%100 + "=" +
                       data[ i ].getSeriesName( j ) + "\n" ) ;
            sb.append( "y" + order[whichFunction-1]%100 + "color=" +
                       this.colors[ i ][ j ].getRGB() + "\n" ) ;
            //-------------------------------------------------------------------------------
            int tempIndex = -1;
            for (int index = 0; index < BIGPlotRenderer.BIG_SHAPES.length; index++)
            {
            	if (this.shapes[i][j] == BIGPlotRenderer.BIG_SHAPES[index])
            		tempIndex = index;
            }
            sb.append( "y" + order[whichFunction-1]%100 + "shape=" +
            			tempIndex + "\n");
            //-------------------------------------------------------------------------------

            sb.append( "y" + order[whichFunction-1]%100 + "axispre=" +
                       pre + "\n" ) ;

            whichFunction++ ;

      }
      sb.append( "numfunctions=" + ( whichFunction - 1 ) + "\n" ) ;
      super.appendFontInformation( sb ) ;
      sb.append( "\n" ) ;
      // fonts and stuff
      system.BIGFileHelper.saveToFile( sb.toString() , f ) ;

   }

   /**
    * reads the .bit File, set standard settings and init default settings
    * @param f File the .bit File
    */
   public void init( File f )
   {
      // if f exists
      if ( f == null )
         f = this.outputFile ;
      // init a lot of fonts and stuff
      super.initFonts( new File( f.getAbsolutePath() + ".gui" ) ) ;
      // the settings for the file
      int[] pre ;
      this.progressBar.setMinimum( 0 ) ;
      this.progressBar.setMaximum( 10 ) ;
      this.progressLabel.setText( "Init: Setting defaults" ) ;
      this.progressBar.setValue( 0 ) ;
      double xMin = 0.0 ;
      double xMax = 0.0 ;
      double[] yMin = null ;
      double[] yMax = null ;
      double xMinDefault = 0.0 ;
      double xMaxDefault = 0.0 ;
      double[] yMinDefault = null ;
      double[] yMaxDefault = null ;
      this.titleDefault = this.outputFile.getName() ;
      this.title = titleDefault ;
      this.text = system.BIGFileHelper.getFileContent( f ) ;
      super.setStandardTitle() ;
      this.progressLabel.setText( "Init: Parsing outputfile" ) ;
      this.progressBar.setValue( 1 ) ;
      /*
              First:
         parsing the output File
       */
      // open the parser
      BIGOutputParser parser = null ;
      try
      {
         parser = new BIGOutputParser( f.getAbsolutePath() ) ;
      }
      catch ( BIGParserException ex )
      {
         System.err.println( "Error while parsing " + f.getName() +
                             ":" + ex.getMessage() ) ;
         return ;
      }
      // setting xValues (thank god, they are just once)
      this.xAxisText = parser.getValue( "xaxistext" ) ;
      this.xAxisTextDefault = this.xAxisText ;
      try
      {
         xMin = Double.parseDouble( parser.getValue( "xoutmin" ) ) ;
      }
      catch ( NumberFormatException ex7 )
      {
      }
      if ( xMin < 0.0 )
         xMin = 0.0 ;
      xMinDefault = xMin ;
      try
      {
         xMax = Double.parseDouble( parser.getValue( "xoutmax" ) ) ;
      }
      catch ( NumberFormatException ex6 )
      {
      }
      if ( xMax <= 0.0 )
         xMax = 1.0 ;
      xMaxDefault = xMax ;
      this.xLog = Integer.parseInt( parser.getValue( "xaxislogbase" ) ) ;
      this.xLogDefault = this.xLog ;
      this.xTicks = Integer.parseInt( parser.getValue( "xaxisticks" ) ) ;
      // now the tough stuff the yaxis
      int numberOfFunctions = -1 ;
      // maybe the file is filled out correctly :)
      try
      {
         numberOfFunctions = Integer.parseInt( parser.getValue( "numfunctions" ) ) ;
      }
      catch ( Exception ex1 )
      {}
      // if not check for the data
      if ( numberOfFunctions == -1 )
      {
         this.saveInit() ;
         return ;
      }
      BIGStrings searchedValues = new BIGStrings() ;
      for ( int i = 1 ; i <= numberOfFunctions ; i++ )
      {
         searchedValues.add( "y" + numberOfFunctions + "axistext" ) ;
         searchedValues.add( "y" + numberOfFunctions + "outmin" ) ;
         searchedValues.add( "y" + numberOfFunctions + "outmax" ) ;
         searchedValues.add( "y" + numberOfFunctions + "axislogbase" ) ;
         searchedValues.add( "y" + numberOfFunctions + "axisticks" ) ;
         searchedValues.add( "y" + numberOfFunctions + "color" ) ;
	 searchedValues.add( "y" + numberOfFunctions + "shape" );
         searchedValues.add( "tlegendfunction" + numberOfFunctions ) ;
      }
      // get number of differen y axis
      Vector yNames = new Vector() ;
      Vector yMins = new Vector() ;
      Vector yMaxs = new Vector() ;
      Vector yLogs = new Vector() ;
      Vector yTicks = new Vector() ;
      Vector whichFunctionsToWhichAxis = new Vector() ;
      order=new int[numberOfFunctions];
      for ( int i = 1 ; i <= numberOfFunctions ; i++ )
      {
         boolean found = false ;
         for ( int j = 0 ; j < yNames.size() ; j++ )
            if ( yNames.get( j ).equals( parser.getValue( "y" + i + "axistext" ) ) )
            {/*
               System.err.println("Funktion "+i+" hat text "+parser.getValue( "y" + i + "axistext" ));
               System.err.println("Das == "+yNames.get( j ));
               System.err.println("Alle Namen:");
               for (int jj=0;jj<yNames.size();jj++)
                  System.err.println("\t"+yNames.get( jj ));*/
               ( ( Vector ) whichFunctionsToWhichAxis.get( j ) ).add( new
                   Integer( i ) ) ;
               if ( ( ( Double ) yMins.get( j ) ).compareTo( Double.valueOf(
                   parser.getValue( "y" +
                                    i + "outmin" ) ) ) > 0 )
               {
                  yMins.remove( j ) ;
                  yMins.add( j ,
                             new Double( ( String ) parser.getValue( "y" + i +
                      "outmin" ) ) ) ;
               }
               if ( ( ( Double ) yMaxs.get( j ) ).compareTo( Double.valueOf(
                   parser.getValue( "y" +
                                    i + "outmax" ) ) ) < 0 )
               {
                  yMaxs.remove( j ) ;
                  yMaxs.add( j ,
                             new Double( ( String ) parser.getValue( "y" + i +
                      "outmax" ) ) ) ;
               }
               if ( ( ( Integer ) yLogs.get( j ) ).compareTo( Integer.valueOf(
                   parser.getValue( "y" +
                                    i + "axislogbase" ) ) ) > 0 )
               {
                  yLogs.remove( j ) ;
                  yLogs.add( j ,
                             new Integer( ( String ) parser.getValue( "y" + i +
                      "axislogbase" ) ) ) ;
               }
               if ( ( ( Integer ) yTicks.get( j ) ).compareTo( Integer.valueOf(
                   parser.getValue( "y" +
                                    i + "axisticks" ) ) ) < 0 )
               {
                  yTicks.remove( j ) ;
                  yTicks.add( j ,
                              new Integer( ( String ) parser.getValue( "y" + i +
                      "axisticks" ) ) ) ;
               }
               order[i-1]=(j)*100+i ;

               found = true ;
            }
         if ( !found )
         {
            if ( parser.getValue( "y" + i + "axistext" ) == null )
            {
               this.saveInit() ;
               return ;
            }
            yNames.add( parser.getValue( "y" + i + "axistext" ) ) ;
            Vector whichFuntionToThisAxis = new Vector() ;
            whichFuntionToThisAxis.add( new Integer( i ) ) ;
            whichFunctionsToWhichAxis.add( whichFuntionToThisAxis ) ;
            try
            {
               yMins.add( new Double( ( String ) parser.getValue( "y" + i +
                   "outmin" ) ) ) ;
            }
            catch ( NumberFormatException ex3 )
            {
               yMins.add( new Double( 0.0 ) ) ;
            }
            try
            {
               yMaxs.add( new Double( ( String ) parser.getValue( "y" + i +
                   "outmax" ) ) ) ;
            }
            catch ( NumberFormatException ex4 )
            {
               yMaxs.add( new Double( 0.0 ) ) ;
            }
            yLogs.add( new Integer( ( String ) parser.getValue( "y" + i +
                "axislogbase" ) ) ) ;
            yTicks.add( new Integer( ( String ) parser.getValue( "y" + i +
                "axisticks" ) ) ) ;
            order[i-1]=(whichFunctionsToWhichAxis.size()-1)*100+i;
         }
      }/*
         System.err.println( whichFunctionsToWhichAxis.size() +
                             " unterschiedl. yAchsen" ) ;
         for ( int i = 0 ; i < whichFunctionsToWhichAxis.size() ; i++ )
         {
            System.err.println( "yAchse " + i + ":" +
                                ( ( Vector ) whichFunctionsToWhichAxis.get( i ) ).
                                size() ) ;
         }*/
      // s has now all different yAxisNames as keys
      // initializing this' data
      this.yAxisText = new String[yNames.size() ] ;
      this.yAxisTextDefault = new String[yNames.size() ] ;
      yMin = new double[yNames.size() ] ;
      yMax = new double[yNames.size() ] ;
      yMinDefault = new double[yNames.size() ] ;
      yMaxDefault = new double[yNames.size() ] ;
      this.data = new BIGDataSet[yNames.size() ] ;
      this.colors = new Color[yNames.size() ][] ;
      this.shapes = new Shape[yNames.size()][] ;
      yLog = new int[yNames.size() ] ;
      yLogDefault = new int[yNames.size() ] ;
      this.yTicks = new int[yNames.size() ] ;
      this.yTicksDefault = new int[yNames.size() ] ;
      pre = new int[yNames.size() ] ;
      for ( int i = 0 ; i < yNames.size() ; i++ )
      {
         yAxisText[ i ] = ( String ) yNames.get( i ) ;
         yAxisTextDefault[ i ] = yAxisText[ i ] ;
         yMin[ i ] = ( ( Double ) yMins.get( i ) ).doubleValue() ;
         yMinDefault[ i ] = yMin[ i ] ;
         yMax[ i ] = ( ( Double ) yMaxs.get( i ) ).doubleValue() ;
         yMaxDefault[ i ] = yMax[ i ] ;
         yLog[ i ] = ( ( Integer ) yLogs.get( i ) ).intValue() ;
         this.yTicks[ i ] = ( ( Integer ) yTicks.get( i ) ).intValue() ;
         yLogDefault[ i ] = ( ( Integer ) yLogs.get( i ) ).intValue() ;
         this.yTicksDefault[ i ] = ( ( Integer ) yTicks.get( i ) ).intValue() ;

         pre[ i ] = 3 ;
      }
      this.progressLabel.setText( "Init: Parsing outputfiles data" ) ;
      this.progressBar.setValue( 3 ) ;

      double[][] values = parser.parseOutputToData() ;
      this.progressLabel.setText( "Init: Open saved informations" ) ;
      this.progressBar.setValue( 5 ) ;

      /*
             Loading the .gui file
       */
      boolean loadGuiFile = false ;
      if ( ( new File( f.getAbsolutePath() + ".gui" ) ).exists() )
         try
         {
            parser = new BIGOutputParser( f.getAbsolutePath() +
                                          ".gui" ) ;

            loadGuiFile = true ;
         }
         catch ( BIGParserException ex2 )
         {
            System.err.println( "Error while parsing " +
                                f.getName() + ".gui" +
                                ":" + ex2.getMessage() ) ;

         }
      if ( loadGuiFile )
      {
         this.xAxisText = parser.getValue( "xaxistext" ) ;
         this.xAxisTextDefault = this.xAxisText ;
         xMin = Double.parseDouble( parser.getValue( "xoutmin" ) ) ;
         xMax = Double.parseDouble( parser.getValue( "xoutmax" ) ) ;
         this.xLog = Integer.parseInt( parser.getValue( "xaxislogbase" ) ) ;
         this.xTicks = Integer.parseInt( parser.getValue( "xaxisticks" ) ) ;

         setTitle( parser.getValue( "title" ) ) ;
         yNames = new Vector() ;
         yMins = new Vector() ;
         yMaxs = new Vector() ;
         yLogs = new Vector() ;
         yTicks = new Vector() ;
         Vector yColors = new Vector() ;
	 Vector yShapes = new Vector() ;
         Vector yPre = new Vector() ;
         //whichFunctionsToWhichAxis = new Vector() ;
         for ( int i = 1 ; i <= numberOfFunctions ; i++ )
         {
            boolean found = false ;
            for ( int j = 0 ; j < yNames.size() ; j++ )
               if ( yNames.get(j).equals( parser.getValue( "y" + i + "axistext" ) ) )
               {
                 /* if ( ( ( Double ) yMins.get( j ) ).compareTo( new Double(
                      parser.getValue( "y" +
                                       i +
                                       "outmin" ) ) ) > 0 )
                  {
                     yMins.remove( j ) ;
                     yMins.add( j ,
                                new Double( ( String ) parser.getValue( "y" + i +
                         "outmin" ) ) ) ;
                  }
                  if ( ( ( Double ) yMaxs.get( j ) ).compareTo( new Double(
                      parser.getValue( "y" +
                                       i +
                                       "outmax" ) ) ) < 0 )
                  {
                     yMaxs.remove( j ) ;
                     yMaxs.add( j ,
                                new Double( ( String ) parser.getValue( "y" + i +
                         "outmax" ) ) ) ;
                  }
                  if ( ( ( Integer ) yLogs.get( j ) ).compareTo( new Integer(
                      parser.getValue(
                          "y" +
                          i +
                          "axislogbase" ) ) ) > 0 )
                  {
                     yLogs.remove( j ) ;
                     yLogs.add( j ,
                                new Integer( ( String ) parser.getValue( "y" +
                         i +
                         "axislogbase" ) ) ) ;
                  }
                  if ( ( ( Integer ) yTicks.get( j ) ).compareTo( new Integer(
                      parser.getValue(
                          "y" +
                          i +
                          "axisticks" ) ) ) < 0 )
                  {
                     yTicks.remove( j ) ;
                     yTicks.add( j ,
                                 new Integer( ( String ) parser.getValue( "y" +
                         i +
                         "axisticks" ) ) ) ;
                  }*/

                  /*( ( Vector ) whichFunctionsToWhichAxis.get( j ) ).add( new
                      Integer(
                          i ) ) ;*/
                  ( ( Vector ) yColors.get( j ) ).add( new
                      Color(
                          Integer.parseInt( ( String ) parser.getValue( "y" +
                      i +
                      "color" ) ) ) ) ;

                  try
                  {
                     ( ( Vector ) yShapes.get( j ) ).add(
                         new Integer( ( String ) parser.getValue( "y" + i +
                         "shape" ) ) ) ;
                  }
                  catch ( NumberFormatException ex5 )
                  {
                     ( ( Vector ) yShapes.get( j ) ).add( new Integer( ( i - 1 ) %
                         BIGPlotRenderer.BIG_SHAPES.length ) ) ;
                  }
                  found = true ;
               }
            if ( !found )
            {
               if ( parser.getValue( "y" + i + "axistext" ) == null )
               {
                  this.saveInit() ;
                  return ;
               }
               yNames.add( parser.getValue( "y" + i + "axistext" ) ) ;
               yPre.add( new Integer( parser.getValue( "y" + i + "axispre" ) ) ) ;
               //Vector whichFuntionToThisAxis = new Vector() ;
               //whichFuntionToThisAxis.add( new Integer( i ) ) ;
               //whichFunctionsToWhichAxis.add( whichFuntionToThisAxis ) ;
               Vector colorsForThisYAxis = new Vector() ;
               colorsForThisYAxis.add( new Color( Integer.parseInt( ( String )
                   parser.getValue( "y" + i + "color" ) ) ) ) ;
               yColors.add( colorsForThisYAxis ) ;
               Vector shapesForThisYAxis = new Vector() ;
               try
               {
                  shapesForThisYAxis.add( new Integer( ( String ) parser.
                      getValue( "y" + i + "shape" ) ) ) ;
                  yShapes.add( shapesForThisYAxis ) ;
               }
               catch ( NumberFormatException e )
               {
                  shapesForThisYAxis.add( new Integer( i %
                      BIGPlotRenderer.BIG_SHAPES.length ) ) ;
                  yShapes.add( shapesForThisYAxis ) ;
               }
               yMins.add( new Double( ( String ) parser.getValue( "y" + i +
                   "outmin" ) ) ) ;
               yMaxs.add( new Double( ( String ) parser.getValue( "y" + i +
                   "outmax" ) ) ) ;
               yLogs.add( new Integer( ( String ) parser.getValue( "y" + i +
                   "axislogbase" ) ) ) ;
               yTicks.add( new Integer( ( String ) parser.getValue( "y" + i +
                   "axisticks" ) ) ) ;
            }
         }
         // s has now all different yAxisNames as keys
         // initializing this' data
         this.yAxisText = new String[yNames.size() ] ;
         yMin = new double[yNames.size() ] ;
         yMax = new double[yNames.size() ] ;
         yLog = new int[yNames.size() ] ;
         pre = new int[yNames.size() ] ;
         this.yTicks = new int[yNames.size() ] ;

         for ( int i = 0 ; i < yNames.size() ; i++ )
         {
            pre[ i ] = ( ( Integer ) yPre.get( i ) ).intValue() ;
            yAxisText[ i ] = ( String ) yNames.get( i ) ;
            yMin[ i ] = ( ( Double ) yMins.get( i ) ).doubleValue() ;
            yMax[ i ] = ( ( Double ) yMaxs.get( i ) ).doubleValue() ;
            this.yLog[ i ] = ( ( Integer ) yLogs.get( i ) ).intValue() ;
            this.yTicks[ i ] = ( ( Integer ) yTicks.get( i ) ).intValue() ;
            Vector thisYColors = ( ( Vector ) yColors.get( i ) ) ;
            this.colors[ i ] = new Color[thisYColors.size() ] ;
            for ( int j = 0 ; j < colors[ i ].length ; j++ )
            {
               this.colors[ i ][ j ] = ( Color ) thisYColors.get( j ) ;
            }
            Vector thisYShapes = ( (Vector) yShapes.get( i ) );
            this.shapes[ i ] = new Shape[thisYShapes.size()];
            try
            {
                for (int j = 0; j < shapes[ i ].length; j++)
                {
                	int index = ((Integer) thisYShapes.get( j )).intValue();
                	this.shapes[ i][ j ] = BIGPlotRenderer.BIG_SHAPES[index];
                }
            }
            catch(IndexOutOfBoundsException iobe)
            {
            	// TODO:  Is there something to do?
            }
         }
      }
      /*
             Building the dataSets

       */
      if ( yNames.size() > 1 )
      {
         this.displayedDataSets = new int[2 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
         this.displayedDataSets[ 1 ] = 1 ;
      }
      else
      {
         this.displayedDataSets = new int[1 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
      }
      this.progressLabel.setText( "Init: Initializing Data" ) ;
      this.progressBar.setValue( 9 ) ;
      for ( int i = 0 ; i < whichFunctionsToWhichAxis.size() ; i++ )
      {
         Vector whichFuntionToThisAxis = ( Vector ) whichFunctionsToWhichAxis.
             get( i ) ;
         if ( this.colors[ i ] == null )
            this.colors[ i ] = new Color[whichFuntionToThisAxis.size() ] ;
         if ( this.shapes[ i ] == null )
        	 this.shapes[ i ] = new Shape[whichFuntionToThisAxis.size() ];
         double[][] thisAxissData = new double[whichFuntionToThisAxis.size() +
             1 ][
             values[ 0 ].length ] ;
         // xData
         thisAxissData[ 0 ] = values[ 0 ] ;
         String[] namesForThisAxis = new String[whichFuntionToThisAxis.size() ] ;
         //System.err.println(i+"/"+namesForThisAxis.length+"("+yNames.get(i)+")");
         for ( int j = 0 ; j < namesForThisAxis.length ; j++ )
         {
            // find in order
            //int inOrder=0;
            int whichFun=( ( Integer ) whichFuntionToThisAxis.get( j ) ).
                intValue();
            //System.err.print( "    " + whichFun ) ;
            /*for ( int k = 0 ; k < order.length ; k++ )
            {
               if ( ( whichFun - 1 ) == ( k ) )
               {
                  inOrder = ( order[ k ] % 100 ) ;
                  System.err.print( "    " + order[ k ] ) ;
               }
            }*/
            thisAxissData[ j +
                1 ] = values[ whichFun ] ;
            //System.err.print("    "+inOrder);
            namesForThisAxis[ j ] = ( String ) parser.getValue(
                "tlegendfunction" +whichFun ) ;
            //System.err.print("    "+namesForThisAxis[ j ]);
            //System.err.println();
         }
         this.data[ i ] = new BIGDataSet( thisAxissData , namesForThisAxis ) ;
         this.data[ i ].setDefaultMinsAndMaxs( xMinDefault , xMaxDefault ,
                                               yMinDefault[ i ] ,
                                               yMaxDefault[ i ] ) ;
         this.data[ i ].setMinimumDomainValue( xMin ) ;
         this.data[ i ].setMaximumDomainValue( xMax ) ;
         this.data[ i ].setMinimumRangeValue( yMin[ i ] ) ;
         this.data[ i ].setMaximumRangeValue( yMax[ i ] ) ;
         //System.err.println("DataSet"+i+" hat "+namesForThisAxis.length+" LEGENDEN");
         this.data[ i ].setPre( pre[ i ] ) ;
      }
      this.progressLabel.setText( "done" ) ;
      this.progressBar.setValue( 0 ) ;

   }

   /**
    * init which
    *  only needs beginOfData and endOfData and the data between
    */
   public void saveInit()
   {
      double[][] data = BIGOutputParser.parseOutputToData( this.outputFile ) ;
      this.yAxisText = new String[1 ] ;
      this.yAxisText[ 0 ] = "y" ;
      this.yAxisTextDefault = new String[1 ] ;
      this.yAxisTextDefault[ 0 ] = "y" ;
      this.data = new BIGDataSet[1 ] ;
      this.colors = new Color[1 ][ data.length - 1 ] ;
      this.shapes = new Shape[ 1 ][ data.length - 1 ] ;
      String[] names = new String[data.length - 1 ] ;
      for ( int i = 0 ; i < data.length - 1 ; i++ )
      {
         names[ i ] = "" + i ;
      }
      this.data[ 0 ] = new BIGDataSet( data , names ) ;
      this.displayedDataSets = new int[1 ] ;
      this.displayedDataSets[ 0 ] = 0 ;
      this.progressLabel.setText( "done" ) ;
      this.progressBar.setValue( 0 ) ;

   }
   /**
    * does nothing (cant remove from a file)
    * @param s String dont care
    */
   public void removeFunction( String s )
   {
      return ;
   }
   /**
    * returns false (cant add new to a file)
    * @param d double[][] dont care
    * @param y String dont care
    * @param s String dont care
    * @return boolean false
    */
   public boolean addFunction( double[][] d , String y , String s )
   {
      return false ;
   }
   /**
    * returns the file, which is used to save information
    */
   public File getSaveFile()
   {
      return new File(this.getFile().getAbsolutePath()+".gui");
   }

   public DataFlavor[] getTransferDataFlavors()
   {
      DataFlavor[]df=new DataFlavor[1];
      df[0]=this.getDataFlavorObject();
      return df ;
   }

   public boolean isDataFlavorSupported( DataFlavor flavor )
   {
      if (flavor.equals(this.getDataFlavorObject()))
         return true;
      return false ;
   }

   public Object getTransferData( DataFlavor flavor )
       throws UnsupportedFlavorException , IOException
   {
      if (!flavor.equals(this.getDataFlavorObject()))
         return null ;
      return this;
   }
   public DataFlavor getDataFlavorObject()
   {
      return new DataFlavor(BIGOutputFile.class,"A BenchIT-resultfile "+this.fileNameWithoutExtension);
   }

   //-----
   public void changeName(int whichDataSet, int index, String newName) {
	   String oldNames[] = this.data[this.displayedDataSets[ whichDataSet ]].getNames();
	   oldNames[ index ] = newName;
   }
   //-----
}
