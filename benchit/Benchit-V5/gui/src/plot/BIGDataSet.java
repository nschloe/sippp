/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGDataSet.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.13 $
 *  $Date: 2007/02/27 12:39:03 $
 *
 ******************************************************************************/
package plot ;

import org.jfree.data.XYDataset ;
import org.jfree.data.AbstractSeriesDataset ;
import org.jfree.data.Range ;
import org.jfree.data.DomainInfo ;
import org.jfree.data.RangeInfo ;

import conn.Graph;
import system.* ;

/**
 * an org.jfree.data.dataset implementation
 * made for the BenchITGUI
 * http://www.jfree.org/jfreechart/index.html
 *
 * @author <a href="mailto:pisi@pisi.de">Christoph Mueller</a>
 */
public class BIGDataSet
    extends AbstractSeriesDataset implements XYDataset , DomainInfo , RangeInfo
{
   // 3 means no modifier. modifiers are set in sets and modifiers
   private int pre = 3 ;
   /**
    * debugmessages en- or disabled
    * (may be overwritten in the constructor)
    */
   private boolean debug = false ;
   // display the pre also on legends?
   private boolean preSelected=false;
   /**
    * values [column][row]
    * first column ([0]) are the x values
    * all series have the same x values
    * for each series there is a column for the y values
    * so we have (series.count + 1) columns
    */
   private double[][] values ;

   /**
    * the names of the series
    * names[i] belongs to values[i+1][x]
    */
   private String[] names ;

   /**
    * min and max values for the achses
    * these valuess will be calculated in the constructor
    * the values ending with an a are the ones that should not be modified
    * the values ending with a b are for custom viewAreas
    */
   private double maxXa ;
   private double minXa ;
   private double minYa ;
   private double maxYa ;
   private double maxXb ;
   private double minXb ;
   private double minYb ;
   private double maxYb ;
   /**
    * Strings, how the single y-functions can be modified
    */
   public static String[] sets =
       {
       "n (nano)" , "µ (micro)" , "m (milli)" ,
       " (normal) " , "k (kilo)" , "M (Mega)" , "G (Giga)" , "T (Terra)"} ;
   /**
    * the corresponing modiers
    * to turn 5,000,000 FLOPS to 5 MFLOPS look in sets where M is (=6) and multiply the
    * values by setModifiers[6]
    */
   public static double[] setModifiers =
       {
       1.0E+9 , 1.0E+6 , 1.0E+3 , 1.0E+0 ,
       1.0E-3 , 1.0E-6 , 1.0E-9 , 1.0E-12} ;

   /**
    * construct a dataset
    *
    * the size of the second dimension of double[] is always the same
    * the size of String[] should be the size of the first dimension of double[] plus 1
    *
    * @param values a double[][] containing the values (first column ([0]) are the x values - all series have the same x values - for each series there is a column for the y values)
    * @param names a String[] containing the names of the series
    */
   public BIGDataSet( double[][] values , String[] names )
   {
      this.init( values , names ) ;
   }
   /**
    * the same as the constructor. re-initialize
    * @param values double[][] containing the values (first column ([0]) are the x values - all series have the same x values - for each series there is a column for the y values)
    * @param names String[] containing the names of the series
    */
   public void init( double[][] values , String[] names )
   {
      // print debug?
      if ( BIGInterface.getInstance().getDebug( "BIGDataSet" ) > 0 )
         debug = true ;
      if ( debug )
         System.out.println( "BIGDataSet()" ) ;
      this.names = names ;
      this.values = values ;
      int i , j ;
      // getting minima and maxima for x
      double max = Double.NEGATIVE_INFINITY ;
      double min = Double.POSITIVE_INFINITY ;

      //look for min and max in first col (x values)
      for ( i = 0 ; i < values[ 0 ].length ; i++ )
      {
         if ( values[ 0 ][ i ] > max )
            max = values[ 0 ][ i ] ;
         if ( values[ 0 ][ i ] < min )
            min = values[ 0 ][ i ] ;
      }

      //if we have only one value, min and max are equal and this will caus problems
      if ( max == min )
      {
         min-- ;
         max++ ;
      }
      if ( max < 0.0 )
         max = 0.0 ;
      if ( min < 0.0 )
         min = 0.0 ;
      if ( min == max )
         max = max + 1.0 ;

      maxXa = max ;
      maxXb = maxXa ;
      minXa = min ;
      minXb = minXa ;

      // getting minima and maxima for y
      max = Double.NEGATIVE_INFINITY ;
      min = Double.POSITIVE_INFINITY ;

      //look for min and max in all other cols (y values)
      for ( i = 1 ; i < values.length ; i++ )
      {
         for ( j = 0 ; j < values[ i ].length ; j++ )
         {
            if ( values[ i ][ j ] > max )
               max = values[ i ][ j ] ;
            if ( values[ i ][ j ] < min )
               min = values[ i ][ j ] ;
         }
      }
      //if we have only one value, min and max are equal and this will caus problems
      if ( max == min )
      {
         min-- ;
         max++ ;
      }

      if ( max < 0.0 )
         max = 0.0 ;
      if ( min < 0.0 )
         min = 0.0 ;
      if ( min == max )
         max = max + 1.0 ;
      maxYa = max ;
      maxYb = maxYa ;
      minYa = min ;
      minYb = minYa ;

   }

   /**
    * get the number of series
    * @return number of series
    */
   public int getSeriesCount()
   {
      return values.length - 1 ;
   }

   /**
    * get the name of a serie with index i
    * @param i index
    * @return name of a serie
    */
   public String getSeriesName( int i )
   {
      String preString = "" ;
      if (preSelected)
         preString=sets[pre].substring(0,1).trim();
      return preString + names[ i ] ;
   }
   /**
    * sets a new name for a series
    * @param i int number of series
    * @param s String new name
    */
   public void setSeriesName( int i , String s )
   {
      this.names[ i ] = s ;
   }

   /**
    * returns the number of results or items
    * because all series have the same count of items
    * we do not care for the argument
    * @param arg0 ignored
    * @return number of items for one row
    */
   public int getItemCount( int arg0 )
   {
      return values[ 0 ].length ;
   }

   /**
    * get the x value of an item
    * @param series ignored
    * @param item position of the x
    * @return a Double with the value of the x at position item
    */
   public Number getXValue( int series , int item )
   {
      return new Double( values[ 0 ][ item ] ) ;
   }

   /**
    * get the y value of an item
    * @param series serie number
    * @param item position of the y
    * @return a Double with the value of the y from serie series at position item
    */
   public Number getYValue( int series , int item )
   {
      //we have to add a 1 to series because series 0
      //is our x value and not a series
      if ( values[ series + 1 ][ item ] < 0 )
         return null ;
      return new Double( values[ series +
                         1 ][ item ] * this.setModifiers[pre] ) ;
   }

   /**
    * get the minimum x value
    * @return the smallest value in the x series
    */
   public Number getMinimumDomainValue()
   {
      return new Double( minXb ) ;
   }

   /**
    * get the maximum x value
    * @return the highest value in the x series
    */
   public Number getMaximumDomainValue()
   {
      return new Double( maxXb ) ;
   }

   /**
    * get x range as org.jfree.data.Range
    * @return the Range (from this.getMaximumDomainValue() to this.getMinimumDomainValue())
    */
   public Range getDomainRange()
   {
      return new Range( minXb , maxXb ) ;
   }

   /**
    * get minimum Y value
    * @return the smallest y value from all series
    */
   public Number getMinimumRangeValue()
   {
      return new Double( minYb * Math.pow( 10.0 , -3.0 * ( pre - 3.0 ) ) ) ;
   }

   /**
    * get maximum y value
    * @return the highest y value from all series
    */
   public Number getMaximumRangeValue()
   {
      return new Double( maxYb * Math.pow( 10.0 , -3.0 * ( pre - 3.0 ) ) ) ;
   }

   /**
    * get y range as org.jfree.data.Range
    * @return the Range (from this.getMaximumRangeValue() to this.getMinimumRangeValue())
    */
   public Range getValueRange()
   {
      return new Range( minYb * Math.pow( 10.0 , -3.0 * ( pre - 3.0 ) ) ,
                        maxYb * Math.pow( 10.0 , -3.0 * ( pre - 3.0 ) ) ) ;
   }

   /**
    * set the min value for the Range (vertical achsis)
    * @param d the new minimum for range
    */
   public void setMinimumRangeValue( double d )
   {
      if ( d < 0.0 )
         return ;

      minYb = d / this.setModifiers[ this.pre ] ;
      if ( debug ) System.out.println( "BIGDataSet: min y set to :" + d ) ;
   }

   /**
    * set the max value for the Range (vertical achsis)
    * @param d the new maximum for range
    */
   public void setMaximumRangeValue( double d )
   {
      if ( d < 0.0 )
         return ;

      maxYb = d / this.setModifiers[ this.pre ] ;
      if ( debug ) System.out.println( "BIGDataSet: max y set to :" + d ) ;
   }

   /**
    * set the min value for the Domain (horizontal achsis)
    * @param d the new minimum for domain
    */
   public void setMinimumDomainValue( double d )
   {
      if ( d < 0.0 )
         return ;

      minXb = d ;
      if ( debug ) System.out.println( "BIGDataSet: min x set to :" + d ) ;
   }

   /**
    * set the max value for the Domain (horizontal achsis)
    * @param d the new maximum for domain
    */
   public void setMaximumDomainValue( double d )
   {
      if ( d < 0.0 )
         return ;
      maxXb = d ;
      if ( debug ) System.out.println( "BIGDataSet: max x set to :" + d ) ;
   }

   /**
    * reset the view range to default (set when parsing the data when creating this object)
    */
   public void resetAchsisValues()
   {
      minXb = minXa ;
      maxXb = maxXa ;
      minYb = minYa ;
      maxYb = maxYa ;
      if ( debug ) System.out.println(
          "BIGDataSet: AchsisValues resetted to:\"" +
          minXa + "\",\"" + maxXa + "\",\"" + minYa +
          "\",\"" + maxYa + "\"" ) ;
   }
   /**
    * sets the defaults min and maxs, when reset is used. the mins and maxs will be set
    * to these values
    * @param minX double minimum for x
    * @param maxX double maximum for x
    * @param minY double minimum for y
    * @param maxY double maximum for y
    */
   public void setDefaultMinsAndMaxs( double minX , double maxX , double minY ,
                                      double maxY )
   {
      if ( minXa >= 0.0 )
         minXa = minX ;
      if ( maxX >= 0.0 )
         maxXa = maxX ;
      if ( minY >= 0.0 )
         minYa = minY ;
      if ( maxYa >= 0.0 )
         maxYa = maxY ;
      resetAchsisValues() ;
   }
   /**
    * get all values from this dataset
    * @return double[][] the values, including x
    */
   public double[][] getValues()
   {
      return this.values ;
   }
   /**
    * get the legendnames
    * @return String[] the legendnames
    */
   public String[] getNames()
   {
      return this.names ;
   }
   /**
    * sets the modifier to change display
    * @param i int see this.sets
    */
   public void setPre( int i )
   {
      // 0=nano
      //...
      // 3=normal
      // ...
      // 6= GIGA
      // 7 = Peta
      this.pre = i ;
   }
   /**
    * gets the actual modifier
    * @return int position in this.see
    */
   public int getPre()
   {
      return this.pre ;
   }
   /**
    * turns the containing x and y-values to sth. plotable (Benchit-compatible):
    * xValue1 \t yValue1 \t yValue2 ... \n
    * xValue2 ...
    * @return String all values as string
    */
   public String getPlotableFileContent()
   {
      StringBuffer sb = new StringBuffer() ;
      for ( int i = 0 ; i < this.values[ 0 ].length ; i++ )
      {
         for ( int j = 0 ; j < this.values.length ; j++ )
         {
            sb.append( this.values[ j ][ i ] ) ;
            sb.append( '\t' ) ;
         }
         sb.append( '\n' ) ;
      }
      return sb.toString() ;
   }
   
   //-----------------
   /**
    * checks, if there is already a function name in names[] which equals to newName
    * @param names String[] array of function names
    * @param newName String function name which should be unique in names[]
    * @return boolean returns true if and only if newName doesn't already exists in names[]
    */
   private boolean checkForExistence(String[] names, String newName)
   {
	   int index;
	   boolean alreadyExists = false;
	   for(index = 0; index < names.length; index++)
	   {
		   alreadyExists = newName.equals( names[ index ] );
		   if (alreadyExists)
		   {
			   return alreadyExists;
		   }
	   }
	   return alreadyExists;
   }
   
   /**
    * add a new function
    * @param newValues double[][] x and y values of new function.
    * newValues.length==2 means add function with xs=newValues[0], ys=newValues[1]
    * else the xValues will be updated (only newValues[0] are used)
    * @param Graph the graph which is represented by newValues
    * @return boolean could be added?
    */
   public boolean addFromGraph( double[][] newValues , Graph graph )
   {
	   String newName = graph.getGraphName();
      // set standard for non-valid values
      double errorDou = -1.0 ;
      try
      {
         errorDou = 1.0 *
             system.BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "errorInt" ) ;
      }
      catch ( Exception ignored )
      {
      }

      // if its a function
      if ( newValues.length == 2 )
      {
         // if a function with this name exists, rename it by adding a number
         String newFunctionRenamed = newName ;
         boolean alreadyExists;
         String[] newNames = new String[this.names.length + 1 ] ;

         // check for existing
         alreadyExists = checkForExistence( this.names, newFunctionRenamed );
         while(alreadyExists)
         {
        	 // add number
             if ( newFunctionRenamed.equals( newName ) )
             {
          	   	newFunctionRenamed = newName + "1" ;
          	   	alreadyExists = checkForExistence( this.names, newFunctionRenamed );             		
             }
             else
             {
                int ending = Integer.parseInt( newFunctionRenamed.substring(
                    newName.length() ) ) + 1 ;
                newFunctionRenamed = newName + ending ;
                alreadyExists = checkForExistence( this.names, newFunctionRenamed );
             }
         }
         for ( int i = 0 ; i < this.names.length ; i++ )
         {
            newNames[ i ] = this.names[ i ] ;
         }
         /*for ( int i = 0 ; i < this.names.length ; i++ )
         {
            newNames[ i ] = this.names[ i ] ;
            System.out.println("1:\tnewNames[ " + i + " ] : " + newNames[i]);
            System.out.println("2:\tnewName = " +  newName );
            System.out.println("3:\tnewFunctionRenamed = " + newFunctionRenamed );
            if ( names[ i ].equals( newFunctionRenamed ) )
            {
               // add number
               if ( newFunctionRenamed.equals( newName ) )
               {
            	   	newFunctionRenamed = newName + "1" ;
               		//graph.setGraphName( newFunctionRenamed );
               		
               }
               else
               {
                  int ending = Integer.parseInt( newFunctionRenamed.substring(
                      newName.length() ) ) + 1 ;
                  newFunctionRenamed = newName + ending ;
                  //graph.setGraphName( newFunctionRenamed );
               }
            }
         }*/
         newName = newFunctionRenamed ;
         newNames[ newNames.length - 1 ] = newName ;
         graph.setGraphName( newName );
         this.names = newNames ;
         // set to data, that doesnt exist or is wrong
         // the new x-axis. will be shorter later, but now the maximum
         double[] nextXValues = new double[this.values[ 0 ].length +
             newValues[ 0 ].length ] ;
         // which element is the one to write
         int whichWriteNext = 0 ;
         // setting all to errorDou
         for ( int i = 0 ; i < nextXValues.length ; i++ )
         {
            nextXValues[ i ] = errorDou ;
         }
         // we get all possible Values
         int oldSize = this.values[ 0 ].length ;
         int gSize = newValues[ 0 ].length ;
         int oldIndex = 0 , gIndex = 0 ;
         double[] gXValues = newValues[ 0 ] ;
         double[] oldXValues = this.values[ 0 ] ;
         // go through all values in oldXValues and gXValues
         while ( ( oldIndex < oldSize ) || ( gIndex < gSize ) )
         {
            // if we have all old values
            if ( oldIndex == oldSize )
            {
               nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
               gIndex++ ;
            }
            else
            {
               // if we have all newValues
               if ( gIndex == gSize )
               {
                  nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                  oldIndex++ ;
               }
               else
               {
                  // check which is the next x-Value
                  if ( oldXValues[ oldIndex ] < gXValues[ gIndex ] )
                  {
                     nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                     oldIndex++ ;
                  }
                  else
                  {
                     if ( oldXValues[ oldIndex ] > gXValues[ gIndex ] )
                     {

                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;

                     }
                     // are both x-values the same
                     else
                     {
                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;
                        oldIndex++ ;
                     }
                  }
               }
            }
            whichWriteNext++ ;
         }
         // first getLength. now the xValues will get shorter (in most cases)
         int newLength = whichWriteNext ;
         // writing new data for old graphs
         double[][] newDataValues = new double[this.values.length + 1 ][] ;
         double xValues[] = new double[newLength ] ;
         for ( int i = 0 ; i < newLength ; i++ )
            xValues[ i ] = nextXValues[ i ] ;
         newDataValues[ 0 ] = xValues ;
         // writing new yValues (or errorDou)
         for ( int i = 1 ; i < this.values.length ; i++ )
         {
            double yValues[] = new double[newLength ] ;
            for ( int j = 0 ; j < newLength ; j++ )
            {
               yValues[ j ] = this.getY( values , i , xValues[ j ] ) ;
            }
            newDataValues[ i ] = yValues ;
         }
         // ... and new graph
         double yValues[] = new double[newLength ] ;
         for ( int j = 0 ; j < newLength ; j++ )
         {
            yValues[ j ] = this.getY( newValues , 1 , xValues[ j ] ) ;
         }
         newDataValues[ newDataValues.length - 1 ] = yValues ;
         this.values = newDataValues ;
         // finaly return
         return true ;
      }
      // else just update xValues
      else
      {
         double[] nextXValues = new double[this.values[ 0 ].length +
             newValues[ 0 ].length ] ;
         // which element is the one to write
         int whichWriteNext = 0 ;
         // setting all to errorDou
         for ( int i = 0 ; i < nextXValues.length ; i++ )
         {
            nextXValues[ i ] = errorDou ;
         }
         // we get all possible Values
         int oldSize = this.values[ 0 ].length ;
         int gSize = newValues[ 0 ].length ;
         int oldIndex = 0 , gIndex = 0 ;
         double[] gXValues = newValues[ 0 ] ;
         double[] oldXValues = this.values[ 0 ] ;
         // see above.
         while ( ( oldIndex < oldSize ) || ( gIndex < gSize ) )
         {
            if ( oldIndex == oldSize )
            {
               nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
               gIndex++ ;
            }
            else
            {
               if ( gIndex == gSize )
               {
                  nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                  oldIndex++ ;
               }
               else
               {

                  if ( oldXValues[ oldIndex ] < gXValues[ gIndex ] )
                  {
                     nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                     oldIndex++ ;
                  }
                  else
                  {
                     if ( oldXValues[ oldIndex ] > gXValues[ gIndex ] )
                     {

                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;

                     }
                     else
                     {
                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;
                        oldIndex++ ;
                     }
                  }
               }
            }
            whichWriteNext++ ;
         }
         // first getLength
         int newLength = whichWriteNext ;
         // writing new data for old graphs
         double[][] newDataValues = new double[this.values.length ][] ;
         double xValues[] = new double[newLength ] ;
         for ( int i = 0 ; i < newLength ; i++ )
            xValues[ i ] = nextXValues[ i ] ;
         newDataValues[ 0 ] = xValues ;
         // writing new yValues (or errorDou)
         for ( int i = 1 ; i < this.values.length ; i++ )
         {
            double yValues[] = new double[newLength ] ;
            for ( int j = 0 ; j < newLength ; j++ )
            {
               yValues[ j ] = this.getY( values , i , xValues[ j ] ) ;
            }
            newDataValues[ i ] = yValues ;
         }
         this.values = newDataValues ;
         // finaly return
         return true ;

      }
   }
   //----------------
   
   /**
    * add a new function
    * @param newValues double[][] x and y values of new function.
    * newValues.length==2 means add function with xs=newValues[0], ys=newValues[1]
    * else the xValues will be updated (only newValues[0] are used)
    * @param newName String legend/name of function
    * @return boolean could be added?
    */
   public boolean add( double[][] newValues , String newName )
   {
      // set standard for non-valid values
      double errorDou = -1.0 ;
      try
      {
         errorDou = 1.0 *
             system.BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "errorInt" ) ;
      }
      catch ( Exception ignored )
      {
      }

      // if its a function
      if ( newValues.length == 2 )
      {
         // if a function with this name exists, rename it by adding a number
         String newFunctionRenamed = newName ;
         String[] newNames = new String[this.names.length + 1 ] ;
         // check for existing
         for ( int i = 0 ; i < this.names.length ; i++ )
         {
            newNames[ i ] = this.names[ i ] ;
            if ( names[ i ].equals( newFunctionRenamed ) )
            {
               // add number
               if ( newFunctionRenamed.equals( newName ) )
                  newFunctionRenamed = newName + "1" ;
               else
               {
                  int ending = Integer.parseInt( newFunctionRenamed.substring(
                      newName.length() ) ) + 1 ;
                  newFunctionRenamed = newName + ending ;
               }
            }
         }
         newName = newFunctionRenamed ;
         newNames[ newNames.length - 1 ] = newName ;
         this.names = newNames ;
         // set to data, that doesnt exist or is wrong
         // the new x-axis. will be shorter later, but now the maximum
         double[] nextXValues = new double[this.values[ 0 ].length +
             newValues[ 0 ].length ] ;
         // which element is the one to write
         int whichWriteNext = 0 ;
         // setting all to errorDou
         for ( int i = 0 ; i < nextXValues.length ; i++ )
         {
            nextXValues[ i ] = errorDou ;
         }
         // we get all possible Values
         int oldSize = this.values[ 0 ].length ;
         int gSize = newValues[ 0 ].length ;
         int oldIndex = 0 , gIndex = 0 ;
         double[] gXValues = newValues[ 0 ] ;
         double[] oldXValues = this.values[ 0 ] ;
         // go through all values in oldXValues and gXValues
         while ( ( oldIndex < oldSize ) || ( gIndex < gSize ) )
         {
            // if we have all old values
            if ( oldIndex == oldSize )
            {
               nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
               gIndex++ ;
            }
            else
            {
               // if we have all newValues
               if ( gIndex == gSize )
               {
                  nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                  oldIndex++ ;
               }
               else
               {
                  // check which is the next x-Value
                  if ( oldXValues[ oldIndex ] < gXValues[ gIndex ] )
                  {
                     nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                     oldIndex++ ;
                  }
                  else
                  {
                     if ( oldXValues[ oldIndex ] > gXValues[ gIndex ] )
                     {

                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;

                     }
                     // are both x-values the same
                     else
                     {
                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;
                        oldIndex++ ;
                     }
                  }
               }
            }
            whichWriteNext++ ;
         }
         // first getLength. now the xValues will get shorter (in most cases)
         int newLength = whichWriteNext ;
         // writing new data for old graphs
         double[][] newDataValues = new double[this.values.length + 1 ][] ;
         double xValues[] = new double[newLength ] ;
         for ( int i = 0 ; i < newLength ; i++ )
            xValues[ i ] = nextXValues[ i ] ;
         newDataValues[ 0 ] = xValues ;
         // writing new yValues (or errorDou)
         for ( int i = 1 ; i < this.values.length ; i++ )
         {
            double yValues[] = new double[newLength ] ;
            for ( int j = 0 ; j < newLength ; j++ )
            {
               yValues[ j ] = this.getY( values , i , xValues[ j ] ) ;
            }
            newDataValues[ i ] = yValues ;
         }
         // ... and new graph
         double yValues[] = new double[newLength ] ;
         for ( int j = 0 ; j < newLength ; j++ )
         {
            yValues[ j ] = this.getY( newValues , 1 , xValues[ j ] ) ;
         }
         newDataValues[ newDataValues.length - 1 ] = yValues ;
         this.values = newDataValues ;
         // finaly return
         return true ;
      }
      // else just update xValues
      else
      {
         double[] nextXValues = new double[this.values[ 0 ].length +
             newValues[ 0 ].length ] ;
         // which element is the one to write
         int whichWriteNext = 0 ;
         // setting all to errorDou
         for ( int i = 0 ; i < nextXValues.length ; i++ )
         {
            nextXValues[ i ] = errorDou ;
         }
         // we get all possible Values
         int oldSize = this.values[ 0 ].length ;
         int gSize = newValues[ 0 ].length ;
         int oldIndex = 0 , gIndex = 0 ;
         double[] gXValues = newValues[ 0 ] ;
         double[] oldXValues = this.values[ 0 ] ;
         // see above.
         while ( ( oldIndex < oldSize ) || ( gIndex < gSize ) )
         {
            if ( oldIndex == oldSize )
            {
               nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
               gIndex++ ;
            }
            else
            {
               if ( gIndex == gSize )
               {
                  nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                  oldIndex++ ;
               }
               else
               {

                  if ( oldXValues[ oldIndex ] < gXValues[ gIndex ] )
                  {
                     nextXValues[ whichWriteNext ] = oldXValues[ oldIndex ] ;
                     oldIndex++ ;
                  }
                  else
                  {
                     if ( oldXValues[ oldIndex ] > gXValues[ gIndex ] )
                     {

                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;

                     }
                     else
                     {
                        nextXValues[ whichWriteNext ] = gXValues[ gIndex ] ;
                        gIndex++ ;
                        oldIndex++ ;
                     }
                  }
               }
            }
            whichWriteNext++ ;
         }
         // first getLength
         int newLength = whichWriteNext ;
         // writing new data for old graphs
         double[][] newDataValues = new double[this.values.length ][] ;
         double xValues[] = new double[newLength ] ;
         for ( int i = 0 ; i < newLength ; i++ )
            xValues[ i ] = nextXValues[ i ] ;
         newDataValues[ 0 ] = xValues ;
         // writing new yValues (or errorDou)
         for ( int i = 1 ; i < this.values.length ; i++ )
         {
            double yValues[] = new double[newLength ] ;
            for ( int j = 0 ; j < newLength ; j++ )
            {
               yValues[ j ] = this.getY( values , i , xValues[ j ] ) ;
            }
            newDataValues[ i ] = yValues ;
         }
         this.values = newDataValues ;
         // finaly return
         return true ;

      }
   }

   public boolean remove( String oldFunction )
   {
      // find function
      int index = -1 ;
      for ( int i = 0 ; i < this.names.length ; i++ )
      {
         if ( names[ i ].equals( oldFunction ) )
         {
            index = i ;
            break ;
         }
      }
      if ( index == -1 )
      {
         System.err.println( "A function with this name does not exist here" ) ;
         return false ;
      }
      // writing new legends
      String[] newNames = new String[this.names.length - 1 ] ;
      for ( int i = 0 ; i < index ; i++ )
      {
         newNames[ i ] = names[ i ] ;
      }
      for ( int i = index + 1 ; i < names.length ; i++ )
      {
         newNames[ i - 1 ] = names[ i ] ;
      }
      this.names = newNames ;
      // ! x axis is one additional
      index = index + 1 ;
      // writing new values
      double[][] newValues = new double[this.values.length -
          1 ][ this.values[ 0 ].length ] ;
      for ( int i = 0 ; i < index ; i++ )
      {
         newValues[ i ] = this.values[ i ] ;
      }
      for ( int i = index + 1 ; i < values.length ; i++ )
      {
         newValues[ i - 1 ] = this.values[ i ] ;
      }
      this.values = newValues ;

      return true ;

   }
   /**
    * gets the y-value to a corresponding x-Value
    * @param values double[][] double[n][] values to search in
    * @param yPos int 1<=yPos<n position
    * @param x double x-Value to the searched y-value
    * @return double the y-value in values, column yPos, belonging to x
    */
   private static double getY( double[][] values , int yPos , double x )
   {
      // try to find
      for ( int i = 0 ; i < values[ 0 ].length ; i++ )
      {
         if ( values[ 0 ][ i ] == x )
         {
            return values[ yPos ][ i ] ;
         }
      }
      // return invalid
      double errorDou = -1.0 ;
      try
      {
         errorDou = 1.0 *
             system.BIGInterface.getInstance().getBIGConfigFileParser().
             intCheckOut( "errorInt" ) ;
      }
      catch ( Exception ignored )
      {
      }
      return errorDou ;
   }
   /**
    * also add sets[i] to legends?
    * @param preSel boolean set it=true, don't = false
    */
   public void setPreNamesSelected(boolean preSel)
   {
      this.preSelected=preSel;
   }
}
/*****************************************************************************
 Log-History

 *****************************************************************************/
