package plot ;

import java.awt.geom.Rectangle2D ;
import org.jfree.chart.axis.Tick ;
import org.jfree.chart.axis.ValueAxis ;
import java.awt.Insets ;
import java.awt.font.FontRenderContext ;
import java.text.NumberFormat ;
import java.awt.Graphics2D ;
import java.awt.Font ;
import java.awt.font.LineMetrics ;

/**
 * <p>Überschrift: BenchIT</p>
 *
 * <p>Beschreibung: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organisation: ZHR TU Dresden</p>
 *
 * @author Robert Schoene
 * @version 1.0
 */
public class BIGHorizontalNumberAxis
    extends org.jfree.chart.axis.HorizontalNumberAxis
{
    /**
     * the ticks for this axis
     */
    private Double[] tickvals ;

    /**
     * constructs a new instance of BIGHorizontalNumberAxis
     * @param label String name for the axis
     * @param absolutetickvals Double[] ticks (you could use any numbers where ticks shall be painted)
     * @param plotable BIGPlotable the plotable for this axis plot
     */
    public BIGHorizontalNumberAxis( String label , Double[] absolutetickvals, BIGPlotable plotable )
   {
      this( label ,plotable) ;

      setAutoTickUnitSelection( false ) ;
      this.tickvals = absolutetickvals ;


   }
   /**
    * constructs a new BIGHorizontalNumberAxis with standard ticks
    * @param label String name for axis
    * @param plotable BIGPlotable plotable for axis plot
    */
   public BIGHorizontalNumberAxis( String label , BIGPlotable plotable)
   {
      super( label ) ;
      // set formatter
      NumberFormat formatter = getNumberFormatOverride() ;
      formatter = new java.text.DecimalFormat() ;
      formatter.setGroupingUsed( true ) ;
      setNumberFormatOverride( formatter ) ;
      // getting setted fonts
       Font f=plotable.getFont("XAxisTick");
       if (f==null)
       {
           f=this.getTickLabelFont();
           plotable.setFont("XAxisTick",f);
       }
       this.setTickLabelFont(f);
       f=plotable.getFont("XAxis");
       if (f==null)
       {
           f=this.getTickLabelFont();
           plotable.setFont("XAxis",f);
       }
       this.setLabelFont(f);

      this.tickvals = null ;

   }

   /**
    * set the Ticks specifically
    * @param absolutetickvals Double[] where ticks shall be painted if you set it to null, standard will be used
    */
   public void setTicks( Double[] absolutetickvals )
   {
      this.tickvals = absolutetickvals ;
   }
   /**
    * just set the number of ticks which will be painted in constant space
    * @param numberOfTicks int number of ticks, that shall be displayed
    */
   public void setTicks( int numberOfTicks )
   {
      if ( numberOfTicks < 0 )
      {
         this.tickvals = null ;
         return ;
      }
      double min = getMinimumAxisValue() ;
      double max = getMaximumAxisValue() ;
      double yDifference = max - min ;
      Double[] ticks = new Double[numberOfTicks + 1 ] ;
      for ( int i = 0 ; i < numberOfTicks + 1 ; i++ )
         ticks[ i ] = new Double( min +
                                  ( ( 1.0 * i ) / numberOfTicks ) *
                                  yDifference ) ;

      this.setTicks( ticks ) ;
   }

   /**
    * Calculates the positions of the tick labels for the axis, storing the results in the
    * tick label list (ready for drawing).
    *
    * @param g2  the graphics device.
    * @param plotArea  the area in which the plot (inlcuding axes) should be drawn.
    * @param dataArea  the area in which the data should be drawn.
    * @param location  the location of the axis.
    */
   public void refreshTicks( Graphics2D g2 ,
                             Rectangle2D plotArea , Rectangle2D dataArea ,
                             int location )
   {
      if ( this.getMinimumAxisValue() < 0.0 )
         this.setMinimumAxisValue( 0.0 ) ;
      if ( this.tickvals == null )
      {
         super.refreshTicks( g2 , plotArea , dataArea , location ) ;
         return ;
      }
      getTicks().clear() ;
      this.setTicks( this.tickvals.length - 1 ) ;

      Font tickLabelFont = getTickLabelFont() ;
      g2.setFont( tickLabelFont ) ;
      FontRenderContext frc = g2.getFontRenderContext() ;

      if ( isAutoTickUnitSelection() )
      {
         selectAutoTickUnit( g2 , plotArea , dataArea ) ;
      }

      double size = getTickUnit().getSize() ;
      int count = tickvals.length ; //calculateVisibleTickCount();
      double lowestTickValue = calculateLowestVisibleTickValue() ;

      if ( count <= ValueAxis.MAXIMUM_TICK_COUNT )
      {
         for ( int i = 0 ; i < count ; i++ )
         {
            // get current tickValue
            double currentTickValue = ( ( Double ) tickvals[ i ] ).doubleValue() ;
            // find position
            double xx = translateValueToJava2D( currentTickValue , dataArea ) ;
            // label for tick
            String tickLabel ;
            // formatter for tick
            NumberFormat formatter = getNumberFormatOverride() ;

            tickLabel = formatter.format( currentTickValue ) ;
            double tempCurrentValue = currentTickValue ;
            // how many shifts of the number
            int moves = 0 ;
            // if the current isn't zero
            if ( tempCurrentValue != 0.0 )
               // but smaller 10 (0<currentTickValue<10)
               if ( tempCurrentValue < 10.0 )
               {
                  // do until it is larger then 1000
                  while ( tempCurrentValue < 1000.0 )
                  {
                     // take it *3
                     tempCurrentValue = tempCurrentValue * 10.0 ;
                     moves++ ;
                  }
                  // 3 numbers after point
                  // rounding last number
                  int iValue = ( int ) tempCurrentValue ;
                  if ( iValue % 10 < 5 )
                     iValue = ( int ) iValue / 10 ;
                  else
                     iValue = ( int ) iValue / 10 + 1 ;
                  // 2 number after point
                  String number = iValue + "" ;
                  // if it was a number between 1 and 999.999..
                  if ( moves < 4 )
                     // write it with a total length of 4 (including point)
                     tickLabel = number.substring( 0 , 4 - moves ) + "." +
                         number.substring( 4 - moves ,
                                           number.length() ) ;
                  else
                  {
                     // start with 0.
                     tickLabel = "0." ;
                     // fill more zeros
                     for ( int j = 4 ; j < moves ; j++ )
                        tickLabel = tickLabel + "0" ;
                     // now we have a position for the number
                     tickLabel = tickLabel + number ;
                  }
               }
            // while the length is larger then 1 () and a point is in the number
            // and the last char is a zero or a point
            while ( ( tickLabel.length() > 1 ) &&
                    ( tickLabel.indexOf( "." ) > -1 ) &&
                    ( ( tickLabel.charAt( tickLabel.length() - 1 ) ==
                        '0' ) || ( tickLabel.charAt( tickLabel.length() - 1 ) ==
                                   '.' ) ) )
               // remove the last char (used for reducing 50.00 to 50 or 0.100 to 0.1)
               tickLabel = tickLabel.substring( 0 ,
                                                tickLabel.length() - 1 ) ;
            // now take the tickLabel and check if we can use it for a
            // presentation like 1E9 instead of 1,000,000,000
            if ( tickLabel.length() > 8 )
            {
               // set standard representation (formatter inserted ,'s)
               tickLabel = "" + currentTickValue ;
               // where E is placed
               int posOfE = tickLabel.indexOf( "E" ) ;
               // has no E
               if ( posOfE == -1 )
               {
                  // how many shifts
                  int shift = 0 ;
                  // position of point
                  int posOfComma = tickLabel.indexOf( "." ) ;
                  // no point (e.g. 1E38429659834)
                  // insert a ".0"
                  if ( posOfComma == -1 )
                  {
                     tickLabel = tickLabel + ".0" ;
                     posOfComma = tickLabel.indexOf( "." ) ;
                  }
                  // starts with minus? remove it
                  String minus = "" ;
                  if ( tickLabel.startsWith( "-" ) )
                  {
                     minus = "-" ;
                     tickLabel = tickLabel.substring( 1 ) ;
                  }
                  // do until pos. of comma is second (e.g. 4.3 not 43.1)
                  // do x=x/10 posOfComma--
                  while ( ( posOfComma > 1 ) )
                  {
                     tickLabel = "" + Double.parseDouble( tickLabel ) / 10.0 ;
                     shift++ ;
                     posOfComma-- ;
                  }
                  // the new tickLabel is tickLabel (e.g 4.3)+E+shift (e.g.10)
                  // 4.3E10
                  tickLabel = tickLabel + "E" + shift ;
                  posOfE = tickLabel.indexOf( "E" ) ;
                  // add minus if needed
                  tickLabel = minus + tickLabel ;
               }
               // after e (including e)
               String e = tickLabel.substring( tickLabel.indexOf( "E" ) ) ;
               // before e
               tickLabel = tickLabel.substring( 0 , tickLabel.indexOf( "E" ) ) ;
               // do before e = before e*10000
               double valuet = Double.parseDouble( tickLabel ) * 10000.0 ;
               // round it
               valuet = Math.round( valuet ) ;
               valuet = valuet / 10000.0 ;
               // replace it
               tickLabel = valuet + e ;
            }
            // position
            Rectangle2D tickLabelBounds = tickLabelFont.getStringBounds(
                tickLabel , frc ) ;
            LineMetrics metrics = tickLabelFont.getLineMetrics( tickLabel ,
                frc ) ;
            float x = 0.0f ;
            float y = 0.0f ;
            Insets tickLabelInsets = getTickLabelInsets() ;
            if ( isVerticalTickLabels() )
            {
               x = ( float ) ( xx + tickLabelBounds.getHeight() / 2 ) ;
               if ( location == TOP )
               {
                  y = ( float ) ( dataArea.getMinY() -
                                  tickLabelInsets.bottom
                                  - tickLabelBounds.getWidth() ) ;
               }
               else
               {
                  y = ( float ) ( dataArea.getMaxY() + tickLabelInsets.top
                                  + tickLabelBounds.getWidth() ) ;
               }
            }
            else
            {
               x = ( float ) ( xx - tickLabelBounds.getWidth() / 2 ) ;
               if ( location == TOP )
               {
                  y = ( float ) ( dataArea.getMinY() -
                                  tickLabelInsets.bottom
                                  - metrics.getLeading()
                                  - metrics.getDescent() ) ;
               }
               else
               {
                  y = ( float ) ( dataArea.getMaxY() + tickLabelInsets.top
                                  + tickLabelBounds.getHeight() ) ;
               }
            }
            Tick tick = new Tick( new Double( currentTickValue ) , tickLabel ,
                                  x ,
                                  y ) ;
            getTicks().add( tick ) ;
         }
      }
   }

}
