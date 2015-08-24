/*********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGVerticalLogarithmicAxis.java
 *
 *  Author: Robert Schoene
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.10 $
 *  $Date: 2007/05/08 09:48:35 $
 *
 *********************************************************************/


package plot;

import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;
import java.text.DecimalFormat;

import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.VerticalValuePlot;
import org.jfree.data.Range;
import org.jfree.chart.axis.*;
import java.awt.Font;
import java.awt.geom.AffineTransform;
/**
 * A logarithmic value axis, for values displayed vertically.  Display
 * of positive values arbitrarily close to zero is supported, as well as
 * negative values (if 'allowNegativesFlag' flag set).
 * based on JFreeChart 0.9.8
 * @author Robert Schoene
 */
public class BIGVerticalLogarithmicAxis extends VerticalNumberAxis implements Serializable  {

    /** Useful constant for log(10). */
    public double LOG_VALUE = Math.log(10);

    public int log=10;
    /** Smallest arbitrarily-close-to-zero value allowed. */
    public double SMALL_LOG_VALUE = 1e-100;

    /** Flag set true for "10^n"-style tick labels. */
    private boolean logTickLabelsFlag;

    /** Flag set true to allow negative values in data. */
    private boolean allowNegativesFlag;

    /** Helper flag for log axis processing. */
    private boolean smallLogFlag = false;

    /** Number formatter for generating numeric strings. */
    private final DecimalFormat numberFormatterObj = new DecimalFormat("0.00000");

    private BIGPlotable plotable=null;
    /**
     * Constructs a vertical logarithmic axis, using default values where necessary.
     *
     * @param label  the axis label (null permitted).
     */
    public BIGVerticalLogarithmicAxis(String label,int newLog, BIGPlotable plotable) {

       this( label ,plotable) ;
        this.setLog(newLog);

    }
    /**
     * Constructs a vertical logarithmic axis, using default values where necessary.
     *
     * @param label  the axis label (null permitted).
     */
    public BIGVerticalLogarithmicAxis(String label, BIGPlotable plotable) {

       super(label);
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

        //this.logTickLabelsFlag = true;    //save flags
        this.allowNegativesFlag = false;
        //if (!autoRange) {                 //if not auto-ranging then
        //    setupSmallLogFlag();          //setup flag based on bounds values
        //}

    }

    /**
     * Sets the 'log10TickLabelsFlag' flag; true for "10^n"-style tick
     * labels, false for regular numeric labels.
     *
     * @param flag  the flag.
     */
    public void setLogTickLabelsFlag(boolean flag) {
        logTickLabelsFlag = flag;
    }

    /**
     * Returns the 'log10TickLabelsFlag' flag; true for "10^n"-style tick
     * labels, false for regular numeric labels.
     *
     * @return the flag.
     */
    public boolean getLogTickLabelsFlag() {
        return logTickLabelsFlag;
    }

    /**
     * Sets the 'allowNegativesFlag' flag; true to allow negative values
     * in data, false to be able to plot positive values arbitrarily close
     * to zero.
     *
     * @param flag  the flag.
     */
    public void setAllowNegativesFlag(boolean flag) {
        allowNegativesFlag = flag;
    }

    /**
     * Returns the 'allowNegativesFlag' flag; true to allow negative values
     * in data, false to be able to plot positive values arbitrarily close
     * to zero.
     *
     * @return the flag.
     */
    public boolean getAllowNegativesFlag() {
        return allowNegativesFlag;
    }

    /**
     * Overridden version that calls original and then sets up flag for
     * log axis processing.
     *
     * @param range  the range.
     */
    public void setRange(Range range) {
        super.setRange(range);      // call parent method
        setupSmallLogFlag();        // setup flag based on bounds values
    }

    /**
     * Sets up flag for log axis processing.
     */
    protected void setupSmallLogFlag() {
        //set flag true if negative values not allowed and the
        // lower bound is between 0 and 10:
        double lowerVal = getRange().getLowerBound();
        if (lowerVal==0.0)
        {
           if (getRange().getUpperBound()<10.0)
            lowerVal = 1E-10;

        }
        smallLogFlag = (!allowNegativesFlag && lowerVal < 10.0 && lowerVal > 0.0);
    }

    /**
     * Converts a data value to a coordinate in Java2D space, assuming that the axis runs along
     * one edge of the specified plot area.
     * <p>
     * Note that it is possible for the coordinate to fall outside the dataArea.
     *
     * @param value The data value.
     * @param dataArea The area for plotting the data.
     *
     * @return The Java2D coordinate.
     */
    public double translateValueToJava2D(double value, Rectangle2D dataArea) {

        Range range = getRange();
        double axisMin = switchedLog(range.getLowerBound());
        double axisMax = switchedLog(range.getUpperBound());

        double maxY = dataArea.getMaxY();
        double minY = dataArea.getMinY();

        value = switchedLog(value);

        if (isInverted()) {
            return minY + (((value - axisMin) / (axisMax - axisMin)) * (maxY - minY));
        }
        else {
            return maxY - (((value - axisMin) / (axisMax - axisMin)) * (maxY - minY));
        }

    }

    /**
     * Converts a coordinate in Java2D space to the corresponding data value, assuming that the
     * axis runs along one edge of the specified dataArea.
     *
     * @param java2DValue The coordinate in Java2D space.
     * @param dataArea The area in which the data is plotted.
     *
     * @return The data value.
     */
    public double translateJava2DtoValue(float java2DValue, Rectangle2D dataArea) {

        Range range = getRange();
        double axisMin = switchedLog(range.getLowerBound());
        double axisMax = switchedLog(range.getUpperBound());

        double plotY = dataArea.getY();
        double plotMaxY = dataArea.getMaxY();

        if (isInverted()) {
             return Math.pow(this.log, axisMin
                 + ((java2DValue - plotY) / (plotMaxY - plotY)) * (axisMax - axisMin));
        }
        else {
            return Math.pow(this.log, axisMax
                 - ((java2DValue - plotY) / (plotMaxY - plotY)) * (axisMax - axisMin));
        }
    }

    /**
     * Rescales the axis to ensure that all data is visible.
     */
    public void autoAdjustRange() {

        Plot plot = getPlot();
        if (plot == null) {
            return;  // no plot, no data.
        }

        if (plot instanceof VerticalValuePlot) {
            VerticalValuePlot vvp = (VerticalValuePlot) plot;

            Range r = vvp.getVerticalDataRange(this);
            if (r == null) {
                r = new Range(DEFAULT_LOWER_BOUND, DEFAULT_UPPER_BOUND);
            }

            double lower = computeLogFloor(r.getLowerBound());

            if (!allowNegativesFlag && lower >= 0.0 && lower < SMALL_LOG_VALUE) {
                //negatives not allowed and lower range bound is zero
                lower = r.getLowerBound();    //use data range bound instead
            }

            double upper = r.getUpperBound();
            if (!allowNegativesFlag && upper < 1.0 && upper > 0.0 && lower > 0.0) {
                //negatives not allowed and upper bound between 0 & 1
                //round up to nearest significant digit for bound:
                //get negative exponent:
                double expVal = Math.log(upper) / LOG_VALUE;
                expVal = Math.ceil(-expVal + 0.001); //get positive exponent
                expVal = Math.pow(this.log, expVal);      //create multiplier value
                //multiply, round up, and divide for bound value:
                upper = (expVal > 0.0) ? Math.ceil(upper * expVal) / expVal : Math.ceil(upper);
            }
            else {
                //negatives allowed or upper bound not between 0 & 1
                upper = computeLogCeil(upper);  //use nearest log value
            }
            // ensure the autorange is at least <minRange> in size...
            double minRange = getAutoRangeMinimumSize();
            if (upper - lower < minRange) {
              upper = (upper + lower + minRange) / 2;
              lower = (upper + lower - minRange) / 2;
            }

            setRangeAttribute(new Range(lower, upper));

            setupSmallLogFlag();       //setup flag based on bounds values
        }
    }

    /**
     * Returns the smallest (closest to negative infinity) double value that is
     * not less than the argument, is equal to a mathematical integer and
     * satisfying the condition that log base 10 of the value is an integer
     * (i.e., the value returned will be a power of 10: 1, 10, 100, 1000, etc.).
     *
     * @param upper a double value above which a ceiling will be calcualted.
     *
     * @return 10<sup>N</sup> with N .. { 1 ... }
     */
    protected double computeLogCeil(double upper) {

        double logCeil;
        if (allowNegativesFlag) {
            //negative values are allowed
            if (upper > this.log) {
                //parameter value is > 10
                // The Math.log() function is based on e not 10.
                logCeil = Math.log(upper) / LOG_VALUE;
                logCeil = Math.ceil(logCeil);
                logCeil = Math.pow(this.log, logCeil);
            }
            else if (upper < -this.log) {
                //parameter value is < -10
                //calculate log using positive value:
                logCeil = Math.log(-upper) / LOG_VALUE;
                //calculate ceil using negative value:
                logCeil = Math.ceil(-logCeil);
                //calculate power using positive value; then negate
                logCeil = -Math.pow(this.log, -logCeil);
            }
            else {
               //parameter value is -10 > val < 10
               logCeil = Math.ceil(upper);     //use as-is
            }
        }
        else {
            //negative values not allowed
            if (upper > 0.0) {
                //parameter value is > 0
                // The Math.log() function is based on e not 10.
                logCeil = Math.log(upper) / LOG_VALUE;
                logCeil = Math.ceil(logCeil);
                logCeil = Math.pow(this.log, logCeil);
            }
            else {
                //parameter value is <= 0
                logCeil = Math.ceil(upper);     //use as-is
            }
        }
        return logCeil;

    }

    /**
     * Returns the largest (closest to positive infinity) double value that is
     * not greater than the argument, is equal to a mathematical integer and
     * satisfying the condition that log base 10 of the value is an integer
     * (i.e., the value returned will be a power of 10: 1, 10, 100, 1000, etc.).
     *
     * @param lower a double value below which a floor will be calcualted.
     *
     * @return 10<sup>N</sup> with N .. { 1 ... }
     */
    protected double computeLogFloor(double lower) {

        double logFloor;
        if (allowNegativesFlag) {
            //negative values are allowed
            if (lower > this.log) {   //parameter value is > 10
                // The Math.log() function is based on e not 10.
                logFloor = Math.log(lower) / LOG_VALUE;
                logFloor = Math.floor(logFloor);
                logFloor = Math.pow(this.log, logFloor);
            }
            else if (lower < this.log) {   //parameter value is < -10
                //calculate log using positive value:
                logFloor = Math.log(-lower) / LOG_VALUE;
                //calculate floor using negative value:
                logFloor = Math.floor(-logFloor);
                //calculate power using positive value; then negate
                logFloor = -Math.pow(this.log, -logFloor);
            }
            else {
                //parameter value is -10 > val < 10
                logFloor = Math.floor(lower);   //use as-is
            }
        }
        else {
            //negative values not allowed
            if (lower > 0.0) {   //parameter value is > 0
                // The Math.log() function is based on e not 10.
                logFloor = Math.log(lower) / LOG_VALUE;
                logFloor = Math.floor(logFloor);
                logFloor = Math.pow(this.log, logFloor);
            }
            else {
                //parameter value is <= 0
                logFloor = Math.floor(lower);   //use as-is
            }
        }
        return logFloor;

    }

    /**
     * Calculates the positions of the tick labels for the axis, storing the
     * results in the tick label list (ready for drawing).
     *
     * @param g2  the graphics device.
     * @param plotArea  the area in which the plot and the axes should be drawn.
     * @param dataArea  the area in which the plot should be drawn.
     * @param location  the axis location.
     */
    public void refreshTicks(Graphics2D g2, Rectangle2D plotArea, Rectangle2D dataArea,
                             int location) {

        getTicks().clear();
        this.setTickLabelFont(plotable.getFont("YAxisTick"));
        //get lower bound value:
        double lowerBoundVal = getRange().getLowerBound();
        //if small log values and lower bound value too small
        // then set to a small value (don't allow <= 0):
        if (smallLogFlag && lowerBoundVal < SMALL_LOG_VALUE) {
            lowerBoundVal = SMALL_LOG_VALUE;
        }
        //get upper bound value
        final double upperBoundVal = getRange().getUpperBound();

        //get log10 version of lower bound and round to integer:
        final int iBegCount = (int) Math.rint(switchedLog(lowerBoundVal));
        //get log10 version of upper bound and round to integer:
        final int iEndCount = (int) Math.rint(switchedLog(upperBoundVal));

        double tickVal;
        String tickLabel;
        boolean zeroTickFlag = false;
        for (int i = iBegCount; i <= iEndCount; i++) {
            //for each tick with a label to be displayed
            int jEndCount = (int)this.log;
            if (i == iEndCount) {
                jEndCount = 1;
            }

            for (int j = 0; j < jEndCount; j++) {
                //for each tick to be displayed
                if (smallLogFlag) {
                    //small log values in use
                    tickVal = Math.pow(this.log, i) + (Math.pow(this.log, i) * j);
                    if (j == 0) {
                        //first tick of group; create label text
                        if (logTickLabelsFlag) {
                            //if flag then
                            tickLabel = this.log+"^" + i;      //create "log10"-type label
                        }
                        else {    //not "log10"-type label
                            if (i >= 0) {   //if positive exponent then make integer
                                tickLabel =  Long.toString((long) Math.rint(tickVal));
                            }
                            else {
                                //negative exponent; create fractional value
                                //set exact number of fractional digits to be shown:
                                numberFormatterObj.setMaximumFractionDigits(-i);
                                //create tick label:
                                tickLabel = numberFormatterObj.format(tickVal);
                            }
                        }
                    }
                    else {   //not first tick to be displayed
                        tickLabel = "";     //no tick label
                    }
                }
                else { //not small log values in use; allow for values <= 0
                    if (zeroTickFlag) {      //if did zero tick last iter then
                        --j;
                    }               //decrement to do 1.0 tick now
                    tickVal = (i >= 0) ? Math.pow(this.log, i) + (Math.pow(this.log, i) * j)
                                       : -(Math.pow(this.log, -i) - (Math.pow(this.log, -i - 1) * j));
                    if (j == 0) {  //first tick of group
                        if (!zeroTickFlag) {     //did not do zero tick last iteration
                            if (i > iBegCount && i < iEndCount
                                              && Math.abs(tickVal - 1.0) < 0.0001) {
                                //not first or last tick on graph and value is 1.0
                                tickVal = 0.0;        //change value to 0.0
                                zeroTickFlag = true;  //indicate zero tick
                                tickLabel = "0";      //create label for tick
                            }
                            else {
                                //first or last tick on graph or value is 1.0
                                //create label for tick ("log10" label if flag):
                                tickLabel = logTickLabelsFlag
                                            ? (((i < 0) ? "-" : "") + this.log +"^" + Math.abs(i))
                                            : Long.toString((long) Math.rint(tickVal));
                            }
                        }
                        else {     // did zero tick last iteration
                            tickLabel = "";         //no label
                            zeroTickFlag = false;   //clear flag
                        }
                    }
                    else {       // not first tick of group
                        tickLabel = "";           //no label
                        zeroTickFlag = false;     //make sure flag cleared
                    }
                }

                if (tickVal > upperBoundVal) {
                    return;     //if past highest data value then exit method
                }
                //get Y-position for tick:
                double yy = translateValueToJava2D(tickVal, dataArea);
                //get bounds for tick label:
                Rectangle2D tickLabelBounds
                    = getTickLabelFont().getStringBounds(tickLabel, g2.getFontRenderContext());
                //get X-position for tick label:
                Insets tickLabelInsets = getTickLabelInsets();
                float x;
                if (location == LEFT) {
                    x = (float) (dataArea.getX()
                                 - tickLabelBounds.getWidth() - getTickLabelInsets().right);
                }
                else {
                    x = (float) (dataArea.getMaxX() + getTickLabelInsets().left);
                }
                //get Y-position for tick label:
                float y = (float) (yy + (tickLabelBounds.getHeight() / 3));

                      //create tick object and add to list:
                      if ( ( tickVal >= this.getMinimumAxisValue() ) &&
                           ( tickVal <= this.getMaximumAxisValue() ) )
                         getTicks().add( new Tick( new Double( tickVal ) , tickLabel , x , y ) ) ;
                   }
                   }
    }

    /**
     * Returns the log10 value, depending on if values between 0 and
     * 1 are being plotted.
     *
     * @param val  the value.
     *
     * @return ??
     */
    protected double switchedLog(double val) {
      return smallLogFlag ? Math.log(val) / LOG_VALUE : adjustedLog(val);
    }

    /**
     * Returns an adjusted log10 value for graphing purposes.  The first
     * adjustment is that negative values are changed to positive during
     * the calculations, and then the answer is negated at the end.  The
     * second is that, for values less than 10, an increasingly large
     * (0 to 1) scaling factor is added such that at 0 the value is
     * adjusted to 1, resulting in a returned result of 0.
     *
     * @param val the value.
     * @return the adjusted value.
     */
    public double adjustedLog(double val) {
        final boolean negFlag;
        negFlag = (val < 0.0);
        if ( negFlag ) {
            val = -val;          //if negative then set flag and make positive
        }
        if (val < log) {                  //if < 10 then
            val += (log - val) / log;        //increase so 0 translates to 0
        }
        //return value; negate if original value was negative:
        return negFlag ? -(Math.log(val) / LOG_VALUE) : (Math.log(val) / LOG_VALUE);
    }
    public void setLog(int newLog)
    {
        this.log=newLog;
        LOG_VALUE = Math.log(newLog);
    }


    /**
     * A utility method intended for use by subclasses that are 'vertical' axes (for example,
     * VerticalCategoryAxis, VerticalNumberAxis and VerticalDateAxis.
     * had to be reimplemented to remove file-output-bug under java 1.5
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
          Font labelFont = plotable.getFont("YAxis"); 
        	  //getLabelFont() ;
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


}
