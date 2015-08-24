package plot ;

import java.awt.geom.Rectangle2D ;
import java.awt.geom.AffineTransform ;
import org.jfree.chart.axis.Tick ;
import java.awt.Insets ;
import org.jfree.chart.axis.ValueAxis ;
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
public class BIGVerticalNumberAxis
    extends org.jfree.chart.axis.VerticalNumberAxis
{

   /**
    * A utility method intended for use by subclasses that are 'vertical' axes (for example,
    * VerticalCategoryAxis, VerticalNumberAxis and VerticalDateAxis.
    *
    * @param label  the label.
    * @param vertical  rotate the label to vertical?
    * @param g2  the graphics device.
    * @param plotArea  the plot area.
    * @param dataArea  the data area.
    * @param location  the axis location (TOP or BOTTOM).
    *
    */
   protected void drawVerticalLabel( String label , boolean vertical ,
                                     Graphics2D g2 , Rectangle2D plotArea ,
                                     Rectangle2D dataArea ,
                                     int location )
   {
      if ( label == null ? false : !label.equals( "" ) )
      {
         Font labelFont = plotable.getFont("YAxis") ;
         Insets labelInsets = getLabelInsets() ;
         g2.setFont( labelFont ) ;
         g2.setPaint( getLabelPaint() ) ;

         Rectangle2D labelBounds = labelFont.getStringBounds( label ,
             g2.getFontRenderContext() ) ;
         if ( location == LEFT )
         {
            if ( vertical )
            {
               double xx = plotArea.getX() + labelInsets.left +
                   labelBounds.getHeight() ;
               double yy = plotArea.getY() + dataArea.getHeight() / 2
                   + ( labelBounds.getWidth() / 2 ) ; /*
               RefineryUtilities.drawRotatedString( label , g2 ,
                              ( float ) xx , ( float ) yy , -Math.PI / 2 ) ;*/

              AffineTransform saved = g2.getTransform() ;

               // apply the rotation...
               AffineTransform rotate = AffineTransform.getRotateInstance( -
                   Math.PI / 2 , ( float ) xx , ( float ) yy ) ;
               g2.transform( rotate ) ;

               // replaces this code...
               g2.drawString( label , ( float ) xx , ( float ) yy ) ;
               g2.setTransform( saved ) ;

            }
            else
            {
               double xx = plotArea.getX() + labelInsets.left ;
               double yy = plotArea.getY() + plotArea.getHeight() / 2
                   - labelBounds.getHeight() / 2 ;
               g2.drawString( label , ( float ) xx , ( float ) yy ) ;
            }
         }
         else
         {
            if ( vertical )
            {
               double xx = plotArea.getMaxX() - labelInsets.right -
                   labelBounds.getHeight() ;
               double yy = plotArea.getMinY() + dataArea.getHeight() / 2
                   - ( labelBounds.getWidth() / 2 ) ;

               AffineTransform saved = g2.getTransform() ;

               // apply the rotation...
               AffineTransform rotate = AffineTransform.getRotateInstance( Math.
                   PI / 2 , ( float ) xx , ( float ) yy ) ;
               g2.transform( rotate ) ;

               // replaces this code...
               g2.drawString( label , ( float ) xx , ( float ) yy ) ;
               g2.setTransform( saved ) ;
            }
            else
            {
               double xx = plotArea.getMaxX() - labelInsets.right -
                   labelBounds.getWidth() ;
               double yy = plotArea.getMinY() + plotArea.getHeight() / 2
                   + labelBounds.getHeight() / 2 ;
               g2.drawString( label , ( float ) xx , ( float ) yy ) ;
            }
         }
      }

   }

   private Double[] tickvals ;
   BIGPlotable plotable = null;
   /** Creates a new instance of HorizontalNumberAxisET */
   public BIGVerticalNumberAxis( String label , Double[] absolutetickvals , BIGPlotable plotable)
   {
      this( label , plotable) ;
      setAutoTickUnitSelection( false ) ;
      this.tickvals = absolutetickvals ;

   }

   /** Creates a new instance of HorizontalNumberAxisET that acts like a normal
    * HorizontalNumberAxis
    * */
   public BIGVerticalNumberAxis( String label , BIGPlotable plotable)
   {
      super( label ) ;
      this.plotable=plotable;
      Font f=plotable.getFont("YAxisTick");
      if (f==null)
      {
          f=this.getTickLabelFont();
          plotable.setFont("YAxisTick",f);
      }
      this.setTickLabelFont(f);
      f=plotable.getFont("YAxis");
      if (f==null)
      {
          f=this.getTickLabelFont();
          plotable.setFont("YAxis",f);
      }
      this.setLabelFont(f);

      NumberFormat formatter = getNumberFormatOverride() ;
      formatter = new java.text.DecimalFormat() ;
      formatter.setGroupingUsed( true ) ;
      setNumberFormatOverride( formatter ) ;

      this.tickvals = null ;

   }

   public void setTicks( Double[] absolutetickvals )
   {
      this.tickvals = absolutetickvals ;
   }

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
      this.setTicks( this.tickvals.length - 1 ) ;
      getTicks().clear() ;
      Font tickLabelFont = plotable.getFont("YAxisTick"); 
    	  // getTickLabelFont() ;
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
            double currentTickValue = ( ( Double ) tickvals[ i ] ).doubleValue() ;
            double yy = translateValueToJava2D( currentTickValue , dataArea ) ;
            String tickLabel ;
            NumberFormat formatter = getNumberFormatOverride() ;
            tickLabel = formatter.format( currentTickValue ) ;
            double tempCurrentValue = currentTickValue ;
            int moves = 0 ;
            if ( tempCurrentValue != 0.0 )
               if ( tempCurrentValue < 10.0 )
               {
                  //  System.err.println(1);
                  while ( tempCurrentValue < 1000.0 )
                  {
                     //    System.err.println(2);
                     tempCurrentValue = tempCurrentValue * 10.0 ;
                     //   System.err.println(tempCurrentValue);
                     moves++ ;
                  }
                  int iValue = ( int ) tempCurrentValue ;
                  if ( iValue % 10 < 5 )
                     iValue = ( int ) iValue / 10 ;
                  else
                     iValue = ( int ) iValue / 10 + 1 ;
                  String number = iValue + "" ;
                  if ( moves < 4 )
                     tickLabel = number.substring( 0 , 4 - moves ) + "." +
                         number.substring( 4 - moves ,
                                           number.length() ) ;
                  else
                  {
                     tickLabel = "0." ;
                     for ( int j = 4 ; j < moves ; j++ )
                        tickLabel = tickLabel + "0" ;
                     tickLabel = tickLabel + number ;
                  }
               }
            while ( ( tickLabel.length() > 1 ) &&
                    ( tickLabel.indexOf( "." ) > -1 ) &&
                    ( ( tickLabel.charAt( tickLabel.length() - 1 ) ==
                        '0' ) || ( tickLabel.charAt( tickLabel.length() - 1 ) ==
                                   '.' ) ) )
               tickLabel = tickLabel.substring( 0 ,
                                                tickLabel.length() - 1 ) ;
            if ( tickLabel.length() > 8 )
            {
               tickLabel = "" + currentTickValue ;
               int posOfE = tickLabel.indexOf( "E" ) ;
               if ( posOfE == -1 )
               {
                  int shift = 0 ;
                  int posOfComma = tickLabel.indexOf( "." ) ;
                  if ( posOfComma == -1 )
                  {
                     tickLabel = tickLabel + ".0" ;
                     posOfComma = tickLabel.indexOf( "." ) ;
                  }
                  String minus = "" ;
                  if ( tickLabel.startsWith( "-" ) )
                  {
                     minus = "-" ;
                     tickLabel = tickLabel.substring( 1 ) ;
                  }
                  while ( ( posOfComma > 1 ) )
                  {
                     tickLabel = "" + Double.parseDouble( tickLabel ) / 10.0 ;
                     shift++ ;
                     posOfComma-- ;
                  }
                  tickLabel = tickLabel + "E" + shift ;
                  posOfE = tickLabel.indexOf( "E" ) ;
                  tickLabel = minus + tickLabel ;
               }
               String e = tickLabel.substring( tickLabel.indexOf( "E" ) ) ;
               tickLabel = tickLabel.substring( 0 , tickLabel.indexOf( "E" ) ) ;
               double valuet = Double.parseDouble( tickLabel ) * 10000.0 ;
               valuet = Math.round( valuet ) ;
               valuet = valuet / 10000.0 ;
               tickLabel = valuet + e ;
            }

            Rectangle2D tickLabelBounds = tickLabelFont.getStringBounds(
                tickLabel , frc ) ;
            LineMetrics lm = tickLabelFont.getLineMetrics( tickLabel , frc ) ;
            float x ;
            if ( location == LEFT )
            {
               x = ( float ) ( dataArea.getX()
                               - tickLabelBounds.getWidth() -
                               getTickLabelInsets().right ) ;
            }
            else
            {
               x = ( float ) ( dataArea.getMaxX() + getTickLabelInsets().left ) ;
            }
            float y = ( float ) ( yy + ( lm.getAscent() / 2 ) ) ;
            Tick tick = new Tick( new Double( currentTickValue ) , tickLabel ,
                                  x , y ) ;
            getTicks().add( tick ) ;
            //System.err.println(this.getMinimumAxisValue()+"\t"+this.getMaximumAxisValue()+"\t"+ tick.getText());
         }
      }
   }

}
