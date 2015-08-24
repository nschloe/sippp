package plot;

import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.geom.Rectangle2D;
import java.io.Serializable;
import java.text.DecimalFormat;

import org.jfree.chart.plot.HorizontalValuePlot;
import org.jfree.chart.plot.Plot;
import org.jfree.data.Range;
import org.jfree.chart.axis.*;

/**
 * A logarithmic value axis, for values displayed horizontally.  Display
 * of positive values arbitrarily close to zero is supported, as well as
 * negative values (if 'allowNegativesFlag' flag set).
 *
 * @author Eric Thomas
 */
public class BIGHorizontalLogarithmicAxis extends org.jfree.chart.axis.HorizontalNumberAxis implements Serializable {

    public int log=10;
    /** Useful constant for log(10). */
    public double LOG_VALUE = Math.log(10);

    /** Flag set true for "10^n"-style tick labels. */
    private boolean logTickLabelsFlag;

    /** Smallest arbitrarily-close-to-zero value allowed. */
    public double SMALL_LOG_VALUE = 1e-25;

    /** Flag set true to allow negative values in data. */
    private boolean allowNegativesFlag;

    /** Helper flag for log axis processing. */
    private boolean smallLogFlag = false;

    /** Number formatter for generating numeric strings. */
    private final DecimalFormat numberFormatterObj = new DecimalFormat("0.00000");

    BIGPlotable plotable=null;
    /**
     * Constructs a horizontal logarithmic axis, using default attribute values
     * where necessary.
     *
     * @param label  the axis label (null permitted).
     * @param plotable the BIGPlotable for this axis plot
     */
    public BIGHorizontalLogarithmicAxis(String label, BIGPlotable plotable) {

        // set the default min/max axis values for a logarithmic scale.
        super(label);
        this.plotable=plotable;
        // get and set fonts
        java.awt.Font f=plotable.getFont("XAxisTick");
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

        this.allowNegativesFlag = false;  //save flag
        this.setLog(10);
    }
    public BIGHorizontalLogarithmicAxis(String label,int log,BIGPlotable plotable) {
        // set the default min/max axis values for a logarithmic scale.
        this(label,plotable);
        this.allowNegativesFlag = false;  //save flag
        this.setLog(log);

    }

    public void setLog(int newLog)
    {
        this.log=newLog;
        this.LOG_VALUE=Math.log(newLog);
    }

    /**
     * Sets the 'allowNegativesFlag' flag; true to allow negative values
     * in data, false to be able to plot positive values arbitrarily close to zero.
     *
     * @param flgVal  the new value of the flag.
     */
    public void setAllowNegativesFlag(boolean flgVal) {
        allowNegativesFlag = flgVal;
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
     * Overridden version that calls original and then sets up flag for
     * log axis processing.
     *
     * @param range  the new range.
     */
    public void setRange(Range range) {
        super.setRange(range);      // call parent method
        setupSmallLogFlag();        // setup flag based on bounds values
    }

    /**
     * Sets up flag for log axis processing.
     */
    protected void setupSmallLogFlag() {
        // set flag true if negative values not allowed and the
        // lower bound is between 0 and 10:
        double lowerVal = getRange().getLowerBound();
        if ((lowerVal==0.0)&&(getRange().getUpperBound()<10.0))
        {
            lowerVal = 1E-10;
            this.setRange(lowerVal,getRange().getUpperBound());
        }
        smallLogFlag = (!allowNegativesFlag && lowerVal < 10.0 && lowerVal > 0.0);
    }


    /**
     * Converts a data value to a coordinate in Java2D space, assuming that
     * the axis runs along one edge of the specified plotArea.
     * Note that it is possible for the coordinate to fall outside the
     * plotArea.
     *
     * @param value  the data value.
     * @param plotArea  the area for plotting the data.
     *
     * @return the Java2D coordinate.
     */
    public double translateValueToJava2D(double value, Rectangle2D plotArea) {

        Range range = getRange();
        double axisMin = switchedLog(range.getLowerBound());
        double axisMax = switchedLog(range.getUpperBound());

        double maxX = plotArea.getMaxX();
        double minX = plotArea.getMinX();

        value = switchedLog(value);

        if (isInverted()) {
            return maxX - (((value - axisMin) / (axisMax - axisMin)) * (maxX - minX));
        }
        else {
            return minX + (((value - axisMin) / (axisMax - axisMin)) * (maxX - minX));
        }

    }

    /**
     * Converts a coordinate in Java2D space to the corresponding data
     * value, assuming that the axis runs along one edge of the specified plotArea.
     *
     * @param java2DValue  the coordinate in Java2D space.
     * @param plotArea  the area in which the data is plotted.
     *
     * @return the data value.
     */
    public double translateJava2DtoValue(float java2DValue, Rectangle2D plotArea) {

        Range range = getRange();
        double axisMin = switchedLog(range.getLowerBound());
        double axisMax = switchedLog(range.getUpperBound());

        double plotX = plotArea.getX();
        double plotMaxX = plotArea.getMaxX();

        if (isInverted()) {
          return Math.pow(log, axisMax
                 - ((java2DValue - plotX) / (plotMaxX - plotX)) * (axisMax - axisMin));
        }
        else {
          return Math.pow(log, axisMin
                 + ((java2DValue - plotX) / (plotMaxX - plotX)) * (axisMax - axisMin));
        }
    }

    /**
     * Rescales the axis to ensure that all data is visible.
     */
    public void autoAdjustRange() {

        Plot plot = getPlot();
        if (plot instanceof HorizontalValuePlot) {
            HorizontalValuePlot hvp = (HorizontalValuePlot) plot;

            Range r = hvp.getHorizontalDataRange(this);
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
              expVal = Math.pow(log, expVal);      //create multiplier value
                        //multiply, round up, and divide for bound value:
              upper = (expVal > 0.0) ? Math.ceil(upper * expVal) / expVal : Math.ceil(upper);
            }
            else {  //negatives allowed or upper bound not between 0 & 1
              upper = Math.ceil(upper);     //use nearest integer value
            }
            // ensure the autorange is at least <minRange> in size...
            double minRange = getAutoRangeMinimumSize();
            if (upper - lower < minRange) {
              upper = (upper + lower + minRange) / 2;
              lower = (upper + lower - minRange) / 2;
            }

            setRangeAttribute(new Range(lower, upper));

            setupSmallLogFlag();      //setup flag based on bounds values
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
     * @return 10<sup>N</sup> with N ... { 1 .. MAX_LONG }.
     */
    protected double computeLogCeil(double upper) {

        double logCeil;
        if (upper > log) {     //parameter value is > 10
            // The Math.log() function is based on e not 10.
            logCeil = Math.log(upper) / LOG_VALUE;
            logCeil = Math.ceil(logCeil);
            logCeil = Math.pow(log, logCeil);
        }
        else {
            if (upper < -log) {     //parameter value is < -10
                     //calculate log using positive value:
                logCeil = Math.log(-upper) / LOG_VALUE;
                     //calculate ceil using negative value:
                logCeil = Math.ceil(-logCeil);
                     //calculate power using positive value; then negate
                logCeil = -Math.pow(log, -logCeil);
            }
            else {
                 //parameter value is -10 > val < 10
                logCeil = Math.ceil(upper);       //use as-is
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
     * @return 10<sup>N</sup> with N ... { 1 .. MAX_LONG }.
     */
    protected double computeLogFloor(double lower) {

        double logFloor;
        if (lower > log) {     //parameter value is > 10
            // The Math.log() function is based on e not 10.
            logFloor = Math.log(lower) / LOG_VALUE;
            logFloor = Math.floor(logFloor);
            logFloor = Math.pow(log, logFloor);
        }
        else {
            if (lower < -log) {     //parameter value is < -10
                   //calculate log using positive value:
                logFloor = Math.log(-lower) / LOG_VALUE;
                   //calculate floor using negative value:
                logFloor = Math.floor(-logFloor);
                   //calculate power using positive value; then negate
                logFloor = -Math.pow(log, -logFloor);
            }
            else {
                 //parameter value is -10 > val < 10
                logFloor = Math.floor(lower);     //use as-is
            }
        }
        return logFloor;          //return zero
    }

    /**
     * Calculates the positions of the tick labels for the axis, storing the results in the
     * tick label list (ready for drawing).
     *
     * @param g2  the graphics device.
     * @param drawArea  the area in which the plot and the axes should be drawn.
     * @param dataArea  the area in which the plot should be drawn.
     * @param location  the location of the axis.
     */
    public void refreshTicks(Graphics2D g2, Rectangle2D drawArea, Rectangle2D dataArea,
                             int location) {

        getTicks().clear();

        Range range = getRange();

        //get lower bound value:
        double lowerBoundVal = range.getLowerBound();
              //if small log values and lower bound value too small
              // then set to a small value (don't allow <= 0):
        if (smallLogFlag && lowerBoundVal < SMALL_LOG_VALUE) {
            lowerBoundVal = SMALL_LOG_VALUE;
        }

        //get upper bound value
        final double upperBoundVal = range.getUpperBound();

        //get log10 version of lower bound and round to integer:
        final int iBegCount = (int) Math.rint(switchedLog(lowerBoundVal));
        //get log10 version of upper bound and round to integer:
        final int iEndCount = (int) Math.rint(switchedLog(upperBoundVal));

        double currentTickValue;
        String tickLabel=null;
        double tickVal;
        boolean zeroTickFlag = false;
       /* for (int i = iBegCount; i <= iEndCount; i++) {
            //for each power of 10 value; create ten ticks
            for (int j = 0; j < log; ++j) {*/
               for (int i = iBegCount; i <= iEndCount; i++) {
                           //for each tick with a label to be displayed
                           int jEndCount = (int)this.log;
                           if (i == iEndCount) {
                               jEndCount = 1;
                           }

                           for (int j = 0; j < jEndCount; j++) {
                              //for each tick to be displayed
                              if ( smallLogFlag )
                              {
                                 //small log values in use
                                 tickVal = Math.pow( this.log , i ) + ( Math.pow( this.log , i ) * j ) ;
                                 if ( j == 0 )
                                 {
                                    //first tick of group; create label text
                                    if ( logTickLabelsFlag )
                                    {
                                       //if flag then
                                       tickLabel = this.log + "^" + i ; //create "log10"-type label
                                    }
                                    else
                                    { //not "log10"-type label
                                       if ( i >= 0 )
                                       { //if positive exponent then make integer
                                          tickLabel = Long.toString( ( long ) Math.rint( tickVal ) ) ;
                                       }
                                       else
                                       {
                                          //negative exponent; create fractional value
                                          //set exact number of fractional digits to be shown:
                                          numberFormatterObj.setMaximumFractionDigits( -i ) ;
                                          //create tick label:
                                          tickLabel = numberFormatterObj.format( tickVal ) ;
                                       }
                                    }
                                 }
                                 else
                                 { //not first tick to be displayed
                                    tickLabel = "" ; //no tick label
                                 }
                              }
                              else
                              { //not small log values in use; allow for values <= 0
                                 if ( zeroTickFlag )
                                 { //if did zero tick last iter then
                                    --j ;
                                 } //decrement to do 1.0 tick now
                                 tickVal = ( i >= 0 ) ?
                                     Math.pow( this.log , i ) + ( Math.pow( this.log , i ) * j )
                                     : - ( Math.pow( this.log , -i ) - ( Math.pow( this.log , -i - 1 ) * j ) ) ;
                                 if ( j == 0 )
                                 { //first tick of group
                                    if ( !zeroTickFlag )
                                    { //did not do zero tick last iteration
                                       if ( i > iBegCount && i < iEndCount
                                            && Math.abs( tickVal - 1.0 ) < 0.0001 )
                                       {
                                          //not first or last tick on graph and value is 1.0
                                          tickVal = 0.0 ; //change value to 0.0
                                          zeroTickFlag = true ; //indicate zero tick
                                          tickLabel = "0" ; //create label for tick
                                       }
                                       else
                                       {
                                          //first or last tick on graph or value is 1.0
                                          //create label for tick ("log10" label if flag):
                                          tickLabel = logTickLabelsFlag
                                              ? ( ( ( i < 0 ) ? "-" : "" ) + this.log + "^" + Math.abs( i ) )
                                              : Long.toString( ( long ) Math.rint( tickVal ) ) ;
                                       }
                                    }
                                    else
                                    { // did zero tick last iteration
                                       tickLabel = "" ; //no label
                                       zeroTickFlag = false ; //clear flag
                                    }
                                 }
                                 else
                                 { // not first tick of group
                                    tickLabel = "" ; //no label
                                    zeroTickFlag = false ; //make sure flag cleared
                                 }
                              }

                              if ( tickVal > upperBoundVal )
                              {
                                 return ; //if past highest data value then exit method
                              }

                              double xx = translateValueToJava2D( tickVal , dataArea ) ;
                              Rectangle2D tickLabelBounds = getTickLabelFont().getStringBounds(
                                  tickLabel , g2.getFontRenderContext() ) ;
                              float x = 0.0f ;
                              float y = 0.0f ;
                              Insets tickLabelInsets = getTickLabelInsets() ;
                              if ( isVerticalTickLabels() )
                              {
                                 x = ( float ) ( xx + tickLabelBounds.getHeight() / 2 ) ;
                                 if ( location == TOP )
                                 {
                                    y = ( float ) ( dataArea.getMinY() - tickLabelInsets.bottom
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
                                    y = ( float ) ( dataArea.getMinY() - tickLabelInsets.bottom ) ;
                                 }
                                 else
                                 {
                                    y = ( float ) ( dataArea.getMaxY() + tickLabelInsets.top
                                                    + tickLabelBounds.getHeight() ) ;
                                 }
                              }
                              if ( ( tickVal >= this.getMinimumAxisValue() ) &&
                                   ( tickVal <= this.getMaximumAxisValue() ) )
                                 getTicks().add( new Tick( new Double( tickVal ) ,
                                                           tickLabel , x , y ) ) ;

                              //if on last tick then exit method
                              if ( i >= iEndCount )
                              {
                                 return ;
                              }
                           }
            }

    }
    /**
     * Returns the log10 value, depending on if values between 0 and
     * 1 are being plotted.
     *
     * @param val  value for which log10 should be calculated.
     *
     * @return log<sub>10</sub>(val).
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
     * @param val  value for which log10 should be calculated.
     *
     * @return an adjusted log<sub>10</sub>(val).
     */
    public double adjustedLog(double val) {
        final boolean negFlag;
        negFlag = (val < 0.0);
        if ( negFlag ) {
            val = -val;                  // if negative then set flag and make positive
        }
        if (val < log) {                // if < 10 then
            val += (log - val) / log;    //increase so 0 translates to 0
        }
        //return value; negate if original value was negative:
        return negFlag ? -(Math.log(val) / LOG_VALUE) : (Math.log(val) / LOG_VALUE);
    }


}
