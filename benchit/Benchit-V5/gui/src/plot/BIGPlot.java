/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGPlot.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.52 $
 *  $Date: 2007/07/03 11:25:30 $
 *
 ******************************************************************************/
package plot ;

import gui.BIGEditor ;
import gui.BIGFontDialog ;
import gui.BIGInsetsPanel ;
import gui.BIGTextField ;

import java.awt.BorderLayout ;
import java.awt.Color ;
import java.awt.Component ;
import java.awt.Dimension ;
import java.awt.FlowLayout ;
import java.awt.Graphics ;
import java.awt.Graphics2D ;
import java.awt.GridBagConstraints ;
import java.awt.GridBagLayout ;
import java.awt.Insets ;
import java.awt.Paint ;
import java.awt.Shape ;
import java.awt.event.ActionEvent ;
import java.awt.event.ActionListener ;
import java.awt.event.ItemEvent ;
import java.awt.event.ItemListener ;
import java.awt.event.MouseAdapter ;
import java.awt.event.MouseEvent ;
import java.awt.image.BufferedImage ;
import java.io.ByteArrayOutputStream ;
import java.io.File ;
import java.io.FileOutputStream ;
import java.io.IOException ;
import java.util.StringTokenizer ;

import javax.swing.AbstractAction ;
import javax.swing.JButton ;
import javax.swing.JCheckBox ;
import javax.swing.JColorChooser ;
import javax.swing.JComboBox ;
import javax.swing.JComponent ;
import javax.swing.JFileChooser ;
import javax.swing.JFrame ;
import javax.swing.JLabel ;
import javax.swing.JList ;
import javax.swing.JMenuItem ;
import javax.swing.JOptionPane ;
import javax.swing.JPanel ;
import javax.swing.JScrollPane ;
import javax.swing.JTabbedPane ;
import javax.swing.JTextField ;
import javax.swing.ListCellRenderer ;
import javax.swing.MenuElement ;
import javax.swing.SwingConstants ;

import org.freehep.graphicsio.pdf.PDFGraphics2D ;
import org.freehep.graphicsio.ppm.PPMEncoder ;
import org.freehep.graphicsio.ps.PSGraphics2D ;
import org.jfree.chart.ChartFactory ;
import org.jfree.chart.ChartPanel ;
import org.jfree.chart.JFreeChart ;
import org.jfree.chart.Marker ;
import org.jfree.chart.axis.HorizontalNumberAxis ;
import org.jfree.chart.axis.VerticalNumberAxis ;
import org.jfree.chart.event.ChartChangeEvent ;
import org.jfree.chart.event.ChartChangeListener ;
import org.jfree.chart.event.LegendChangeEvent ;
import org.jfree.chart.event.PlotChangeEvent ;
import org.jfree.chart.ChartMouseListener ;
import org.jfree.chart.ChartMouseEvent ;

import system.BIGFileHelper ;
import system.BIGInterface ;


/**
 * a plotting class derived from ScatterPlotDemo.java
 * <br>
 * we get the data in values, a double[][]<br>
 * the first dimension are the columns, the second the rows<br>
 * all columns have the same number of rows<br>
 * the first column are the x values of the points<br>
 * all other columns are the corresponding y values of the points<br>
 * each other row is called a series<br>
 * the name of the series are stored in names, a String[]<br>
 * we have also n series and n+1 columns and m rows
 *
 * @author <a href="mailto:pisi@pisi.de">Christoph Mueller</a>
 */
public class BIGPlot
{
   // shapeSize
   double shapeSize = 6.0 ;
   // for output of debug information
   private boolean debug = false ;
   // button size
   private static final int BUTTON_SIZE = 500 ;
   // number of columns for a small textfield (e.g. inXmin, inYmax ...)
   private static final int SMALL_TEXTFIELD_COLUMNS = 8 ;
   // number of columns for a smaller textfield (e.g. inXticks, inYticks ...)
   private static final int SMALLER_TEXTFIELD_COLUMNS = 4 ;
   // for drawing the cross hair lines following the mouse cursor
   private boolean currentAxisTrace = false ;
   // button for switching on/off the cross hair
   private JButton crosshairButton ;

   // that's the chart panel
   ChartPanel chartPanel ;
   // tabPane contains the chartPanel and a config panel
   // textScrollPane contains the result file's content
   BIGEditor textScrollPane ;
   // the display panels
   JComponent[] displayPanels ; // [0]->view, [1]->text

   // in the beginnig, when building the window, a lot is changed
   private boolean selfChanging = true ;

   private BIGPlotable plotable ;
   /**
    * creates new Panels, where the plotable is plotted, setup is available,...
    * @param newPlotable BIGPlotable
    */
   public BIGPlot( BIGPlotable newPlotable )
   {
      this.plotable = newPlotable ;
      plotable.init( null ) ;
      this.setupConfig() ;
   }

   /**
    * redraws the whole bigplot with this.plotable's settings
    */
   public void setupConfig()

   {
      // if axistext isn't found
      if ( plotable.xAxisText == null )
         plotable.xAxisText = "x" ;
      if ( plotable.xAxisTextDefault == null )
         plotable.xAxisTextDefault = "x" ;
      // get data for first y-axis
      BIGDataSet data = plotable.data[ plotable.displayedDataSets[ 0 ] ] ;
      // get colors for first y-axis
      Color[] colors1 = plotable.colors[ plotable.displayedDataSets[ 0 ] ] ;

      //------------------------------------------------------------------------------------
      // get shapes for the first y-axis
      Shape[] shapes1 = plotable.shapes[ plotable.displayedDataSets[ 0 ] ] ;
      //------------------------------------------------------------------------------------
      //------------------------------------------------------------------------------------
      // contains the y1 axis text (!!! without pre !!!)
      String yAxisTextWithoutPre = plotable.yAxisText[ plotable.
          displayedDataSets[ 0 ] ] ;
      // contains the y1 axis text, which is displayed by the plot
      String yAxisText = null ;
      if ( plotable.scaleYAxisText[ plotable.displayedDataSets[ 0 ] ] )
      {
         // y1 axis text should be scaled
         yAxisText = BIGDataSet.sets[ plotable.data[ plotable.displayedDataSets[
             0 ] ]
             .getPre() ].substring( 0 , 1 ).trim() +
             yAxisTextWithoutPre ;
      }
      else
      {
         // y1 axis text should not be scaled
         yAxisText = yAxisTextWithoutPre ;
      }

      //------------------------------------------------------------------------------------
      // if second y-axis is available, init it there
      BIGDataSet data2 = null ;
      Color[] colors2 = null ;
      //------------------------------------------------------------------------------------
      // get shapes for the second y-axis
      Shape[] shapes2 = null ;
      //------------------------------------------------------------------------------------

      // contains the y2 axis text (!!! withou pre !!!)
      String yAxisText2WithoutPre = null ;
      // contains the y2 axis text, which is displayed by the plot
      String yAxisText2 = null ;
      if ( plotable.displayedDataSets.length == 2 )
      {
         yAxisText2WithoutPre = plotable.yAxisText[ plotable.displayedDataSets[
             1 ] ] ;
         if ( plotable.scaleYAxisText[ plotable.displayedDataSets[ 1 ] ] )
         {
            // y2 axis text should be scaled
            yAxisText2 = BIGDataSet.sets[ plotable.data[ plotable.
                displayedDataSets[ 1 ] ].
                getPre() ].substring( 0 , 1 ).trim() +
                yAxisText2WithoutPre ;
         }
         else
         {
            // y2 axis text should not be scaled
            yAxisText2 = yAxisText2WithoutPre ;
         }
         data2 = plotable.data[ plotable.displayedDataSets[ 1 ] ] ;
         colors2 = plotable.colors[ plotable.displayedDataSets[ 1 ] ] ;
      }
      if ( debug ) System.err.println( "build" ) ;
      if ( debug ) System.err.println( plotable.title + " - " +
                                       plotable.xAxisText + " - " +
                                       plotable.yAxisText ) ;
      // set the shape size (size of shapes in Plot)
      try
      {
         shapeSize = system.BIGInterface.getInstance().
             getBIGConfigFileParser().
             intCheckOut( "scaleShape" ) ;
      }
      catch ( Exception ex )
      {
      }
      // text is needed for: switch between text(file-content) and graphic(plot)
      String text = plotable.text ;
      // create plot
      // attr.: title, xaxistext,1st yaxistext,dataset, display tooltips, display legends, create urls

      final JFreeChart chart = ChartFactory.createScatterPlot(
          plotable.getExtTitle() , plotable.xAxisText , yAxisText , data , true , true , false ) ;
// second yaxis?
      if ( data2 != null )
         chart.getXYPlot().setSecondaryDataset( data2 ) ;
      // get plot
      this.plotable.xyplot = chart.getXYPlot() ;
      this.plotable.xyplot.setInsets( this.plotable.insets ) ;
      // set some defaults
      if ( data.getMinimumRangeValue().doubleValue() < 0.0 )
         data.setMinimumRangeValue( 0.0 ) ;
      if ( data2 != null )
         if ( data2.getMinimumRangeValue().doubleValue() < 0.0 )
            data2.setMinimumRangeValue( 0.0 ) ;
      // build panel for chart with enabled offscreen buffer (double buffer???)
      chartPanel = new ChartPanel( chart ) ;
      // this is how you could display, that this plot was made with benchit
      // dont use images, there's a problem with save them as eps/pdf...
      /*{
        public void paintComponent(Graphics g)
        {
           super.paintComponent(g);
           g.setColor(Color.LIGHT_GRAY);

           g.drawString("www.benchit.org",(int)getSize().getWidth()-120,20);
        }
                } ;*/
      // whatever it is. its not needed.
      // quote from javadoc:
      //true enables trace lines for the mouse pointer on the (...) axis
      // zoom is allowed
      chartPanel.setVerticalZoom( false ) ;
      chartPanel.setHorizontalZoom( false ) ;

      chartPanel.setLocation( 100 , 100 ) ;
      chartPanel.setVerticalZoom( true ) ;
      chartPanel.setHorizontalZoom( true ) ;
      PlotListen l = new PlotListen( chart , chartPanel ) ;
      chartPanel.addMouseListener( l ) ;
      chartPanel.addMouseMotionListener( l ) ;
      chartPanel.addMouseWheelListener( l ) ;
      chartPanel.addMouseListener( new MouseAdapter()
      {

         public void mouseClicked( MouseEvent evt )
         {
            if ( javax.swing.SwingUtilities.isMiddleMouseButton( evt ) )
            {
               //selfChanging=true;
               double finalY = chart.getXYPlot().getVerticalValueAxis().
                   translateJava2DtoValue( evt.getY() ,
                                           chartPanel.getChartRenderingInfo().
                                           getDataArea() ) ;

               double finalX = chart.getXYPlot().getHorizontalValueAxis().
                   translateJava2DtoValue( evt.getX() ,
                                           chartPanel.getChartRenderingInfo().
                                           getDataArea() ) ;
               if ( !evt.isAltGraphDown() )
               {
                  if ( evt.isShiftDown() )
                  {

                     if ( evt.isControlDown() )
                     {
                        try
                        {
                           finalY = Double.parseDouble( JOptionPane.
                               showInputDialog( chartPanel ,
                                                "Please specify the exact y position for the marker" ,
                                                new Double( finalY ) ) ) ;
                        }
                        catch ( Exception ex )
                        {
                           return ;
                        }

                     }
                     else
                     {
                        try
                        {
                           finalX = Double.parseDouble( JOptionPane.
                               showInputDialog( chartPanel ,
                                                "Please specify the exact x position for the marker" ,
                                                new Double( finalX ) ) ) ;
                        }
                        catch ( Exception ex1 )
                        {
                           return ;
                        }
                     }

                  }
                  if ( evt.isControlDown() )
                  {
                     chart.getXYPlot().addRangeMarker( new Marker( finalY ) ) ;

                  }
                  else
                  {
                     chart.getXYPlot().addDomainMarker( new Marker( finalX ) ) ;
                  }
               }
               /*if ( evt.isAltGraphDown() )
                                   {
                  String text = JOptionPane.showInputDialog( chartPanel ,
                      "Please insert Text" ,
                      "" ) ;
                  if ( text == null || text.equals( "" ) )
                  {

                     selfChanging = false ;
                     return ;
                  }
                  chart.getXYPlot().addAnnotation(new org.jfree.chart.annotations.XYTextAnnotation(,0,0));
                                   }*/
               selfChanging = false ;
            }
         }
      } ) ;
      // very compute-expensive
      chart.setAntiAlias( false ) ;
      // standard, but set it
      chart.setBackgroundPaint( Color.white ) ;
      // own renderer for colors,
      chart.getXYPlot().setRenderer( new BIGPlotRenderer( colors1 , shapes1 ,
          true , plotable.drawLines[ 0 ] , plotable.fillShapes[ 0 ] ) ) ;
      // chart.getXYPlot().setRenderer( new BIGPlotRenderer( colors1 , plotable.shapes[plotable.displayedDataSets[ 0 ]],
      //	true , plotable.drawLines[ 0 ] , plotable.fillShapes[ 0 ] ) ) ;
      if ( data2 != null )
         chart.getXYPlot().setSecondaryRenderer( new BIGPlotRenderer( colors2 ,
             shapes2 ,
             true , plotable.drawLines[ 1 ] , plotable.fillShapes[ 1 ] ) ) ;
      //chart.getXYPlot().setSecondaryRenderer( new BIGPlotRenderer( colors2 , plotable.shapes[plotable.displayedDataSets[ 1 ]],
      //	true , plotable.drawLines[ 1 ] , plotable.fillShapes[ 1 ] ) ) ;
      // decare it final for acces from actions (inner classes)
      // decare it final for acces from actions (inner classes)
      final HorizontalNumberAxis xAxis ;
      // if log is set to 1,0, negativ values we have a on-logarithmic scale-axis
      if ( plotable.xLog < 2 )
      {
         xAxis = new BIGHorizontalNumberAxis(
             plotable.xAxisText , plotable ) ;
         ( ( BIGHorizontalNumberAxis ) xAxis ).setTicks( plotable.xTicks ) ;
      }
      // otherwise not
      else
      {
         xAxis = new BIGHorizontalLogarithmicAxis( plotable.xAxisText ,
             plotable.xLog , plotable ) ;

      }
      // set mins and maxs
      xAxis.setMaximumAxisValue( data.getMaximumDomainValue().doubleValue() ) ;

      xAxis.setMinimumAxisValue( data.getMinimumDomainValue().doubleValue() ) ;
      // set the axis
      chart.getXYPlot().setDomainAxis( xAxis ) ;
      // the same for the left yaxis
      final VerticalNumberAxis y1 ;
      if ( plotable.yLog[ plotable.displayedDataSets[ 0 ] ] < 2 )
      {
         y1 = new BIGVerticalNumberAxis( yAxisText , plotable ) ;
         ( ( BIGVerticalNumberAxis ) y1 ).setTicks( plotable.yTicks[ plotable.
             displayedDataSets[ 0 ] ] ) ;
      }
      else
      {
         y1 = new BIGVerticalLogarithmicAxis( yAxisText ,
                                              plotable.yLog[ plotable.
                                              displayedDataSets[ 0 ] ] ,
                                              plotable ) ;
      }
      y1.setMaximumAxisValue( data.getMaximumRangeValue().doubleValue() ) ;

      y1.setMinimumAxisValue( data.getMinimumRangeValue().doubleValue() ) ;
      chart.getXYPlot().setRangeAxis( y1 ) ;
      chart.plotChanged( new PlotChangeEvent( chart.getXYPlot() ) ) ;
      // and (if necessary) the right y-axis
      if ( data2 != null )
      {
         final VerticalNumberAxis y2 ;
         if ( plotable.yLog[ plotable.displayedDataSets[ 1 ] ] < 2 )
         {
            y2 = new BIGVerticalNumberAxis( yAxisText2 , plotable ) ;
         }
         else
         {
            y2 = new BIGVerticalLogarithmicAxis( yAxisText2 ,
                                                 plotable.yLog[ plotable.
                                                 displayedDataSets[ 1 ] ] ,
                                                 plotable ) ;
         }

         y2.setMaximumAxisValue( data2.getMaximumRangeValue().doubleValue() ) ;

         y2.setMinimumAxisValue( data2.getMinimumRangeValue().doubleValue() ) ;
         //y2.setTicks( data2.getY1AxisNumberOfTicks() ) ;
         chart.getXYPlot().setSecondaryRangeAxis( y2 ) ;
      }
      // chartListener if we zoom or some internals say "Hey! Chart changed!"
      chart.addChangeListener( new ChartChangeListener()
      {
         public void chartChanged( ChartChangeEvent evt )
         {
            // only if others are changing (i.e. zoom)
            if ( !selfChanging )
            {
               // set axis with self-finding ticks
               if ( chart.getXYPlot().getDomainAxis() instanceof
                    BIGHorizontalNumberAxis )
                  ( ( BIGHorizontalNumberAxis ) chart.getXYPlot().getDomainAxis() ).
                      setTicks( null ) ;
               if ( chart.getXYPlot().getRangeAxis() instanceof
                    BIGVerticalNumberAxis )
                  ( ( BIGVerticalNumberAxis ) chart.getXYPlot().getRangeAxis() ).
                      setTicks( null ) ;
               if ( BIGPlot.this.plotable.displayedDataSets.length == 2 )
                  if ( chart.getXYPlot().getSecondaryRangeAxis() instanceof
                       BIGVerticalNumberAxis )
                     ( ( BIGVerticalNumberAxis ) chart.getXYPlot().
                       getSecondaryRangeAxis() ).
                         setTicks( null ) ;
               // we are changing the whole thing ourself
               selfChanging = true ;
               // to paint an annotation (comment in the plot)
               chart.getXYPlot().clearAnnotations() ;
               chart.getXYPlot().addAnnotation( plotable.getAnnotationComment() ) ;
               // done
               selfChanging = false ;
            }
         }
      } ) ;
      // overriding the "save as" menuitem
      this.overrideMenu( chartPanel , data , data2 , text ) ;
      // the drawing supplier gets shapes in the setted size (shapeSize)
      // and its colors (Paint[]) are synchronized (or should be :P)
      // with those from the BIGPlotRenderer
      org.jfree.chart.renderer.DrawingSupplier ds = new org.jfree.chart.
          renderer.DefaultDrawingSupplier()
      {
         // the next shape. maybe one day set the shapes?
         private int nextShape = 0 ;
         // the next paint (can be set)
         private int nextPaint = 0 ;
         // used to get shapes
         private org.jfree.chart.SeriesShapeFactory factory = new org.jfree.
             chart.
             SeriesShapeFactory() ;
         // the sequence of colors to use
         private Paint[] PAINT_SEQUENCE = BIGPlot.getDefaultPaintSequece() ;
         // next paint used for filling shapes
         public Paint getNextPaint()
         {
            return PAINT_SEQUENCE[ nextPaint++ ] ;
         }

         // next outline paint used for drawing shapes
         public Paint getNextOutlinePaint()
         {
            return PAINT_SEQUENCE[ nextPaint++ ] ;
         }

         // next shape used as ... next shape (tadaa)
         public Shape getNextShape()
         {
            return factory.getShape( nextShape++ , 0 , 0.0 , 0.0 , shapeSize ) ;
         }

      } ;
      // set the supplier
      chart.getXYPlot().getRenderer().setDrawingSupplier( ds ) ;
      // the same supplier, so no colors are used twice
      if ( data2 != null )
      {
         chart.getXYPlot().getSecondaryRenderer().setDrawingSupplier( ds ) ;
      }
      // if axistext not set
      if ( plotable.xAxisText == null )
         plotable.xAxisText = "x" ;
      if ( plotable.xAxisTextDefault == null )
         plotable.xAxisTextDefault = "x" ;
      // if more then one yaxis
      if ( plotable.displayedDataSets.length > 1 )
      {
         yAxisText2 = plotable.yAxisText[ plotable.displayedDataSets[ 1 ] ] ;
         data2 = plotable.data[ plotable.displayedDataSets[ 1 ] ] ;
         colors2 = plotable.colors[ plotable.displayedDataSets[ 1 ] ] ;
      }
      // view text from file
      textScrollPane = new BIGEditor( this.plotable.text ) ;
      // benchit outputfiles have shell syntax
      textScrollPane.setTokenMarker( new org.syntax.jedit.tokenmarker.
                                     ShellScriptTokenMarker() ) ;
      // and shall NOT be edited (yeah write your own results. fine. do it. looser...)
      textScrollPane.setEditable( false ) ;
      // a panel with configuration options
      // config tab
      final JTabbedPane displayTabPane = new JTabbedPane( JTabbedPane.TOP ) ;
      // checkboxes for what the texts say?
      final JCheckBox yAxisFillShapes1 = new JCheckBox(
          "Fill Points for each function on Y1axis" , plotable.fillShapes[ 0 ] ) ;
      final JCheckBox yAxisFillShapes2 = new JCheckBox(
          "Fill Points for each function on Y2axis" , plotable.fillShapes[ 1 ] ) ;
      final JCheckBox yAxisShowLines1 = new JCheckBox(
          "Show Lines for each function on Y1axis" , plotable.drawLines[ 0 ] ) ;
      final JCheckBox yAxisShowLines2 = new JCheckBox(
          "Show Lines for each function on Y2axis" , plotable.drawLines[ 1 ] ) ;
      // do settings
      final JButton setButton ;
      // mins, maxs and ticks for all axis
      final BIGTextField inXmin =
          new BIGTextField( 0 , BIGTextField.DOUBLE ) ;

      final BIGTextField inXmax =
          new BIGTextField( 0 , BIGTextField.DOUBLE ) ;

      final BIGTextField inXticks =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;

      final BIGTextField inYmin =
          new BIGTextField( 0 , BIGTextField.DOUBLE ) ;

      final BIGTextField inYmax =
          new BIGTextField( 0 , BIGTextField.DOUBLE ) ;

      final BIGTextField inYticks =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;

      final BIGTextField inYmin2 =
          new BIGTextField( 0 , BIGTextField.DOUBLE ) ;

      final BIGTextField inYmax2 =
          new BIGTextField( 0 , BIGTextField.DOUBLE ) ;

      final BIGTextField inYticks2 =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;
      // logarithmic scale for axis
      final BIGTextField xAxisLogarithmic =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;
      final BIGTextField yAxisLogarithmic =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;
      final BIGTextField yAxisLogarithmic2 =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;
      // names for axis
      final JTextField xAxisTextField = new JTextField( plotable.xAxisText , 0 ) ;
      final JTextField yAxisTextField = new JTextField( yAxisText , 0 ) ;
      final JTextField titleTextField = new JTextField( plotable.title , 0 ) ;
      // set the pre (n for nano...) also in the beginning of the Legends
      // i.e. pre=M (for Mega) legend= FLOPS(ijk) -> MFLOPS(ijk)
      final JCheckBox yPreAlsoLegends = new JCheckBox( "Scale also legends" ) ;
      // do it live
      yPreAlsoLegends.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            plotable.data[ plotable.displayedDataSets[ 0 ] ].
                setPreNamesSelected(
                    ( ( JCheckBox ) evt.getSource() ).isSelected() ) ;
         }
      } ) ;
      final JCheckBox scaleYAxisText = new JCheckBox( "Scale y1 axis text" ) ;
      scaleYAxisText.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            plotable.scaleYAxisText[ 0 ] = ( ( JCheckBox ) evt.getSource() ).
                isSelected() ;
            // System.out.println("scale y1 axis text is now : " + plotable.scaleYAxisText[0]);
            // this soluztionis not very pretty
            // a better solution should use a listener, who reacts on a specific event
            if ( plotable.scaleYAxisText[ plotable.displayedDataSets[ 0 ] ] )
            {
               // y1 axis text should be scaled
               yAxisTextField.setText( BIGDataSet.sets[ plotable.data[ plotable.
                                       displayedDataSets[ 0 ] ]
                                       .getPre() ].substring( 0 , 1 ).trim() +
                                       plotable.yAxisText[ plotable.
                                       displayedDataSets[ 0 ] ] ) ;
            }
            else
            {
               // y1 axis text should not be scaled
               yAxisTextField.setText( plotable.yAxisText[ plotable.
                                       displayedDataSets[ 0 ] ] ) ;
            }
            yAxisTextField.revalidate() ;
         }
      } ) ;
      scaleYAxisText.setSelected( plotable.scaleYAxisText[ 0 ] ) ;

      // the very same for the 2nd y-axis
      final JTextField yAxisTextField2 = new JTextField() ;
      final JComboBox ySet2 = new JComboBox( BIGDataSet.sets ) ;
      final JCheckBox yPreAlsoLegends2 = new JCheckBox(
          "Scale also legends" ) ;
      yPreAlsoLegends2.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            plotable.data[ plotable.displayedDataSets[ 1 ] ].
                setPreNamesSelected(
                    ( ( JCheckBox ) evt.getSource() ).isSelected() ) ;
         }
      } ) ;
      final JCheckBox scaleYAxisText2 = new JCheckBox( "Scale y2 axis text" ) ;
      scaleYAxisText2.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            plotable.scaleYAxisText[ plotable.displayedDataSets[ 1 ] ] =
                ( ( JCheckBox ) evt.getSource() ).isSelected() ;
            // not satisfying -> see comment for y1 solution
            if ( plotable.scaleYAxisText[ plotable.displayedDataSets[ 1 ] ] )
            {
               // y2 axis text should be scaled
               yAxisTextField2.setText(
                   BIGDataSet.sets[ plotable.data[ plotable.displayedDataSets[
                   1 ] ]
                   .getPre() ].substring( 0 , 1 ).trim() +
                   plotable.yAxisText[ plotable.displayedDataSets[ 1 ] ] ) ;
            }
            else
            {
               // y2 axis text should not be scaled
               yAxisTextField2.setText( plotable.yAxisText[ plotable.
                                        displayedDataSets[ 1 ] ] ) ;
            }
            yAxisTextField2.revalidate() ;
         }
      } ) ;
      if ( data2 != null )
      {
         scaleYAxisText2.setSelected( plotable.scaleYAxisText[ plotable.
                                      displayedDataSets[ 1 ] ] ) ;
      }

      // set some defaults
      // xAxisLogarithmic.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      xAxisLogarithmic.setText( "0" ) ;
      // yAxisLogarithmic.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      yAxisLogarithmic.setText( "0" ) ;

      inXmin.setColumns( SMALL_TEXTFIELD_COLUMNS ) ;
      // inXmin.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inXmin.setText( "" + data.getMinimumDomainValue() ) ;

      inXmax.setColumns( SMALL_TEXTFIELD_COLUMNS ) ;
      // inXmax.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inXmax.setText( "" + data.getMaximumDomainValue() ) ;

      inXticks.setColumns( SMALLER_TEXTFIELD_COLUMNS ) ;
      // inXticks.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inXticks.setText( "" + plotable.xTicks ) ;
      xAxisLogarithmic.setValue( "" + plotable.xLog ) ;

      inYmin.setColumns( SMALL_TEXTFIELD_COLUMNS ) ;
      // inYmin.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inYmin.setText( "" + data.getMinimumRangeValue() ) ;

      inYmax.setColumns( SMALL_TEXTFIELD_COLUMNS ) ;
      // inYmax.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inYmax.setText( "" + data.getMaximumRangeValue() ) ;

      inYticks.setColumns( SMALLER_TEXTFIELD_COLUMNS ) ;
      // inYticks.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inYticks.setText( "" + plotable.yTicks[ plotable.displayedDataSets[ 0 ] ] ) ;
      yAxisLogarithmic.setColumns( SMALLER_TEXTFIELD_COLUMNS ) ;
      yAxisLogarithmic.setValue( "" +
                                 plotable.yLog[ plotable.displayedDataSets[ 0 ] ] ) ;

      inYmin2.setColumns( SMALL_TEXTFIELD_COLUMNS ) ;
      // inYmin2.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inYmax2.setColumns( SMALL_TEXTFIELD_COLUMNS ) ;
      // inYmax2.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      inYticks2.setColumns( SMALLER_TEXTFIELD_COLUMNS ) ;
      // inYticks2.setPreferredSize( new Dimension( 100 , 20 ) ) ;
      yAxisLogarithmic2.setColumns( SMALLER_TEXTFIELD_COLUMNS ) ;
      // defaults for 2nd yaxis
      if ( data2 != null )
      {

         inYmin2.setText( "" + data2.getMinimumRangeValue() ) ;

         inYmax2.setText( "" + data2.getMaximumRangeValue() ) ;

         inYticks2.setText( "" + plotable.yTicks[ plotable.displayedDataSets[ 1 ] ] ) ;
         yAxisLogarithmic2.setValue( "" +
                                     plotable.yLog[ plotable.displayedDataSets[
                                     1 ] ] ) ;
         if ( plotable.scaleYAxisText[ plotable.displayedDataSets[ 1 ] ] )
         {
            // y2 axis text should be scaled
            yAxisTextField2.setText( BIGDataSet.sets[ plotable.data[ plotable.
                                     displayedDataSets[ 1 ] ]
                                     .getPre() ].substring( 0 , 1 ).trim() +
                                     plotable.yAxisText[ plotable.
                                     displayedDataSets[ 1 ] ] ) ;
         }
         else
         {
            // y2 axis text should not be scaled
            yAxisTextField2.setText( plotable.yAxisText[ plotable.
                                     displayedDataSets[ 1 ] ] ) ;
         }
         /*yAxisTextField2.setText( plotable.yAxisText[ plotable.
                                  displayedDataSets[ 1 ] ] ) ;*/
         yAxisTextField2.setColumns( SMALL_TEXTFIELD_COLUMNS ) ;

      }
      // set used for nano/micro.../Giga/...
      final JComboBox ySet = new JComboBox( BIGDataSet.sets ) ;
      ySet.setSelectedIndex( plotable.data[ plotable.displayedDataSets[ 0 ] ].
                             getPre() ) ;
      // do also live if this is changed
      ySet.addItemListener( new ItemListener()
      {
         public void itemStateChanged( ItemEvent evt )
         {
            int pre = ( ( JComboBox ) evt.getSource() ).getSelectedIndex() ;
            plotable.data[ plotable.displayedDataSets[ 0 ] ].setPre( pre ) ;
            inYmin.setValue( plotable.data[ plotable.displayedDataSets[ 0 ] ].
                             getMinimumRangeValue().toString() ) ;
            inYmax.setValue( plotable.data[ plotable.displayedDataSets[ 0 ] ].
                             getMaximumRangeValue().toString() ) ;
            // set text = pre +text (pre=only one char)
            if ( plotable.scaleYAxisText[ 0 ] )
            {
               yAxisTextField.setText( BIGDataSet.sets[ pre ].substring( 0 , 1 ).
                                       trim() +
                                       plotable.yAxisText[ plotable.
                                       displayedDataSets[
                                       0 ] ] ) ;
            }
            inYmin.revalidate() ;
            inYmax.revalidate() ;
            yAxisTextField.revalidate() ;

         }
      } ) ;
      // we had it for the 1st yaxis. now here's the 2nd
      if ( data2 != null )
      {
         ySet2.setSelectedIndex( plotable.data[ plotable.displayedDataSets[ 1 ] ].
                                 getPre() ) ;
         ySet2.addItemListener( new ItemListener()
         {
            public void itemStateChanged( ItemEvent evt )
            {
               int pre = ( ( JComboBox ) evt.getSource() ).getSelectedIndex() ;
               plotable.data[ plotable.displayedDataSets[ 1 ] ].setPre( pre ) ;
               inYmin2.setValue( plotable.data[ plotable.displayedDataSets[ 1 ] ].
                                 getMinimumRangeValue().toString() ) ;
               inYmax2.setValue( plotable.data[ plotable.displayedDataSets[ 1 ] ].
                                 getMaximumRangeValue().toString() ) ;
               if ( plotable.scaleYAxisText[ 1 ] )
               {
                  yAxisTextField2.setText( BIGDataSet.sets[ pre ].substring( 0 ,
                      1 ).
                                           trim() +
                                           plotable.yAxisText[ plotable.
                                           displayedDataSets[
                                           1 ] ] ) ;
               }
               inYmin2.revalidate() ;
               inYmax2.revalidate() ;
               yAxisTextField2.revalidate() ;

            }
         } ) ;
      }
      // Checkboxes to set whether a shape shall be filled or lines should be
      // drawn between shapes
      yAxisShowLines1.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            if ( debug ) System.err.println( ( ( JCheckBox ) ae.getSource() ).
                                             getText() + ":" +
                                             ( ( JCheckBox ) ae.getSource() ).
                                             isSelected() ) ;

            ( ( BIGPlotRenderer ) chart.getXYPlot().getRenderer() ).
                setPlotLines(
                    yAxisShowLines1.isSelected() ) ;
            plotable.drawLines[ 0 ] = yAxisShowLines1.isSelected() ;
         }
      } ) ;
      yAxisFillShapes1.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            if ( debug ) System.err.println( ( ( JCheckBox ) ae.getSource() ).
                                             getText() + ":" +
                                             ( ( JCheckBox ) ae.getSource() ).
                                             isSelected() ) ;

            ( ( BIGPlotRenderer ) chart.getXYPlot().getRenderer() ).
                setDefaultShapeFilled(
                    yAxisFillShapes1.isSelected() ) ;
            plotable.fillShapes[ 0 ] = yAxisFillShapes1.isSelected() ;
         }
      } ) ;

      yAxisShowLines2.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            if ( debug ) System.err.println( ( ( JCheckBox ) ae.getSource() ).
                                             getText() + ":" +
                                             ( ( JCheckBox ) ae.getSource() ).
                                             isSelected() ) ;

            ( ( BIGPlotRenderer ) chart.getXYPlot().getSecondaryRenderer() ).
                setPlotLines(
                    yAxisShowLines2.isSelected() ) ;
            plotable.drawLines[ 1 ] = yAxisShowLines2.isSelected() ;
         }
      } ) ;
      yAxisFillShapes2.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            if ( debug ) System.err.println( ( ( JCheckBox ) ae.getSource() ).
                                             getText() + ":" +
                                             ( ( JCheckBox ) ae.getSource() ).
                                             isSelected() ) ;

            ( ( BIGPlotRenderer ) chart.getXYPlot().getSecondaryRenderer() ).
                setDefaultShapeFilled(
                    yAxisFillShapes2.isSelected() ) ;
            plotable.fillShapes[ 1 ] = yAxisFillShapes2.isSelected() ;
         }
      } ) ;
      // annotation in the plot
      final JTextField commentTextField = new JTextField( plotable.
          getAnnComment() , 40 ) ;
      // position of annotation in the plot
      final BIGTextField commentX =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;
      commentX.setText( "" + plotable.getCommentXPercent() ) ;
      final BIGTextField commentY =
          new BIGTextField( 0 , BIGTextField.INTEGER ) ;
      commentY.setText( "" + plotable.getCommentYPercent() ) ;
      // insets for space around plot
      final BIGInsetsPanel insetsPanel = new BIGInsetsPanel( plotable.insets ) ;
      final JPanel configPanel = new JPanel() ;
      final JPanel resetButtonPanel = new JPanel() ;
      final JPanel crosshairButtonPanel = new JPanel() ;
      // set comment
      chart.getXYPlot().addAnnotation( this.plotable.getAnnotationComment() ) ;

      setButton = new JButton(
          new AbstractAction( "set" )
      {
         public void actionPerformed( ActionEvent e )
         {
            selfChanging = true ;
            // get the setted values
            double xMin = inXmin.getDoubleValue().doubleValue() ;
            double xMax = inXmax.getDoubleValue().doubleValue() ;
            double yMin = inYmin.getDoubleValue().doubleValue() ;
            double yMax = inYmax.getDoubleValue().doubleValue() ;
            // set defaults
            double yMin2 = 0 ;
            double yMax2 = 1 ;
            // if setted
            if ( plotable.displayedDataSets.length == 2 )
            {
               yMin2 = inYmin2.getDoubleValue().doubleValue() ;
               yMax2 = inYmax2.getDoubleValue().doubleValue() ;
            }
            // default numerical axis
            int logBase = 0 ;
            try
            {
               logBase = yAxisLogarithmic.getIntegerValue().intValue() ;
               plotable.yLog[ plotable.displayedDataSets[ 0 ] ] = logBase ;
            }
            catch ( Exception ex )
            {
               System.err.println( "Only Integers are allowed as log-base." ) ;
            }
            // text for yaxis
            if ( plotable.scaleYAxisText[ plotable.displayedDataSets[ 0 ] ] )
            {
               // y1 axis text is scaled
               plotable.yAxisText[ plotable.displayedDataSets[ 0 ] ] =
                   yAxisTextField.getText().substring(
                       plotable.data[ plotable.displayedDataSets[ 0 ] ]
                       .sets[ plotable.data[ plotable.displayedDataSets[ 0 ] ]
                       .getPre() ].substring( 0 , 1 ).trim().length() ,
                       yAxisTextField.getText().length() ) ;
            }
            else
            {
               // y1 axis text is not scaled
               plotable.yAxisText[ plotable.displayedDataSets[ 0 ] ] =
                   yAxisTextField.getText() ;
            }
            /*plotable.yAxisText[ plotable.displayedDataSets[ 0 ] ] =
                yAxisTextField.getText().substring( plotable.data[ plotable.
                displayedDataSets[ 0 ] ].sets[ plotable.data[ plotable.
                displayedDataSets[ 0 ] ].getPre() ].
                substring( 0 , 1 ).trim().length() ,
                yAxisTextField.getText().length() ) ;*/
            // if logarithmic
            if ( logBase > 1 )
            {
               BIGVerticalLogarithmicAxis ax = new
                   BIGVerticalLogarithmicAxis(
                       plotable.yAxisText[ plotable.displayedDataSets[ 0 ] ] ,
                       logBase , plotable ) ;
               chartPanel.getChart().getXYPlot().setRangeAxis( ax ) ;

            }
            // else numerical
            else
            {
               chartPanel.getChart().getXYPlot().setRangeAxis( new
                   BIGVerticalNumberAxis( plotable.yAxisText[ plotable.
                                          displayedDataSets[ 0 ] ] , plotable ) ) ;
            }
            // do the same for yaxis
            if ( plotable.displayedDataSets.length == 2 )
            {

               logBase = 0 ;
               try
               {
                  logBase = yAxisLogarithmic2.getIntegerValue().intValue() ;
                  plotable.yLog[ plotable.displayedDataSets[ 1 ] ] = logBase ;

               }
               catch ( Exception ex )
               {
                  System.err.println( "Only Integers are allowed as log-base." ) ;
               }
               // text for y2 axis
               if ( plotable.scaleYAxisText[ plotable.displayedDataSets[ 1 ] ] )
               {
                  // y2 axis text is scaled
                  plotable.yAxisText[ plotable.displayedDataSets[ 1 ] ] =
                      yAxisTextField2.getText().substring(
                          plotable.data[ plotable.displayedDataSets[ 1 ] ]
                          .sets[ plotable.data[ plotable.displayedDataSets[ 1 ] ]
                          .getPre() ].substring( 0 , 1 ).trim().length() ,
                          yAxisTextField2.getText().length() ) ;
               }
               else
               {
                  plotable.yAxisText[ plotable.displayedDataSets[ 1 ] ] =
                      yAxisTextField2.getText() ;
               }
               /*plotable.yAxisText[plotable.displayedDataSets[1]]=
                yAxisTextField2.getText().substring( plotable.data[ plotable.
                displayedDataSets[ 1 ] ].sets[ plotable.data[ plotable.
                displayedDataSets[ 1 ] ].getPre() ].
                substring( 0 , 1 ).trim().length() ,
                yAxisTextField2.getText().length() );*/

               if ( logBase > 1 )
               {
                  BIGVerticalLogarithmicAxis ax = new
                      BIGVerticalLogarithmicAxis(

                          plotable.yAxisText[ plotable.displayedDataSets[ 1 ] ] ,
                          logBase , plotable ) ;
                  chartPanel.getChart().getXYPlot().setSecondaryRangeAxis( ax ) ;

               }
               else
               {
                  chartPanel.getChart().getXYPlot().setSecondaryRangeAxis( new
                      BIGVerticalNumberAxis( plotable.yAxisText[ plotable.
                                             displayedDataSets[ 1 ] ] ,
                                             plotable ) ) ;
               }
            }
            // and xaxis
            logBase = 0 ;
            try
            {
               logBase = xAxisLogarithmic.getIntegerValue().intValue() ;
               plotable.xLog = logBase ;
            }
            catch ( Exception ex )
            {
               System.err.println( "Only Integers are allowed as log-base." ) ;
            }
            plotable.xAxisText = xAxisTextField.getText() ;
            if ( logBase > 1 )
            {
               BIGHorizontalLogarithmicAxis ax = new
                   BIGHorizontalLogarithmicAxis(
                       plotable.xAxisText , logBase , plotable ) ;
               chartPanel.getChart().getXYPlot().setDomainAxis( ax ) ;

            }
            else
            {
               chartPanel.getChart().getXYPlot().setDomainAxis( new
                   BIGHorizontalNumberAxis(
                       plotable.xAxisText , plotable ) ) ;
            }
            // now that we have some axis with names and ticks and stuff
            // we can set their minima and maxima
            // first set it to +inf
            chartPanel.getChart().getXYPlot().getDomainAxis().
                setMaximumAxisValue( Double.MAX_VALUE ) ;
            // then set the min
            chartPanel.getChart().getXYPlot().getDomainAxis().
                setMinimumAxisValue( xMin ) ;
            // then the max
            chartPanel.getChart().getXYPlot().getDomainAxis().
                setMaximumAxisValue( xMax ) ;
            //why did we do this (setting to inf)
            //just think: if we would set first min, then max, this could happen:
            // old min/max = 2/3 new min/max=4/5
            // the new min is larger then the old max. but both are set!
            // this can lead to problems (even if this is for a short time)
            // lets continue
            // set the mins and maxs in the plotable-structures
            for ( int i = 0 ; i < plotable.data.length ; i++ )
            {
               plotable.data[ i ].setMinimumDomainValue( xMin ) ;
               plotable.data[ i ].setMaximumDomainValue( xMax ) ;
            }
            // do the same for yaxis
            chartPanel.getChart().getXYPlot().getRangeAxis().
                setMaximumAxisValue( Double.MAX_VALUE ) ;
            chartPanel.getChart().getXYPlot().getRangeAxis().
                setMinimumAxisValue( yMin ) ;
            chartPanel.getChart().getXYPlot().getRangeAxis().
                setMaximumAxisValue( yMax ) ;
            plotable.data[ plotable.displayedDataSets[ 0 ] ].
                setMinimumRangeValue( yMin ) ;
            plotable.data[ plotable.displayedDataSets[ 0 ] ].
                setMaximumRangeValue( yMax ) ;
            // and also for the 2nd yaxis
            if ( plotable.displayedDataSets.length == 2 )
            {
               chartPanel.getChart().getXYPlot().getSecondaryRangeAxis().
                   setMaximumAxisValue( Double.MAX_VALUE ) ;
               chartPanel.getChart().getXYPlot().getSecondaryRangeAxis().
                   setMinimumAxisValue( yMin2 ) ;
               chartPanel.getChart().getXYPlot().getSecondaryRangeAxis().
                   setMaximumAxisValue( yMax2 ) ;
               plotable.data[ plotable.displayedDataSets[ 1 ] ].
                   setMinimumRangeValue( yMin2 ) ;
               plotable.data[ plotable.displayedDataSets[ 1 ] ].
                   setMaximumRangeValue( yMax2 ) ;

            }
            // if its not a numerical axis set also the ticks
            if ( chart.getXYPlot().getDomainAxis() instanceof
                 BIGHorizontalNumberAxis )
            {
               try
               {
                  plotable.xTicks = inXticks.getIntegerValue().
                      intValue() ;
                  ( ( BIGHorizontalNumberAxis ) chart.getXYPlot().
                    getDomainAxis() ).setTicks( plotable.xTicks ) ;
               }
               catch ( Exception ex1 )
               {
               }
               // if its logarithmic select the displayed numbers:
               // display: n^m or the computed value of n^m
            }
            else
            {
               try
               {
                  plotable.xTicks = inXticks.getIntegerValue().
                      intValue() ;
                  if ( plotable.xTicks < 0 )
                     ( ( BIGHorizontalLogarithmicAxis ) chart.getXYPlot().
                       getDomainAxis() ).setLogTickLabelsFlag( false ) ;
                  else
                     ( ( BIGHorizontalLogarithmicAxis ) chart.getXYPlot().
                       getDomainAxis() ).setLogTickLabelsFlag( true ) ;
               }
               catch ( Exception ex1 )
               {
               }
            }
            // also for all other axis
            if ( chart.getXYPlot().getRangeAxis() instanceof
                 BIGVerticalNumberAxis )
            {
               plotable.yTicks[ plotable.displayedDataSets[ 0 ] ] = inYticks.
                   getIntegerValue().
                   intValue() ;
               ( ( BIGVerticalNumberAxis ) chart.getXYPlot().getRangeAxis() ).
                   setTicks( plotable.yTicks[ plotable.displayedDataSets[ 0 ] ] ) ;
            }
            else
            {
               try
               {
                  plotable.yTicks[ plotable.displayedDataSets[ 0 ] ] = inYticks.
                      getIntegerValue().
                      intValue() ;
                  if ( plotable.yTicks[ plotable.displayedDataSets[ 0 ] ] < 0 )
                     ( ( BIGVerticalLogarithmicAxis ) chart.getXYPlot().
                       getRangeAxis() ).setLogTickLabelsFlag( false ) ;
                  else
                     ( ( BIGVerticalLogarithmicAxis ) chart.getXYPlot().
                       getRangeAxis() ).setLogTickLabelsFlag( true ) ;
               }
               catch ( Exception ex1 )
               {
               }
            }

            if ( plotable.displayedDataSets.length == 2 )
               if ( chart.getXYPlot().getRangeAxis() instanceof
                    BIGVerticalNumberAxis )
               {
                  plotable.yTicks[ plotable.displayedDataSets[ 1 ] ] =
                      inYticks2.getIntegerValue().
                      intValue() ;

                  ( ( BIGVerticalNumberAxis ) chart.getXYPlot().
                    getSecondaryRangeAxis() ).
                      setTicks( plotable.yTicks[ plotable.displayedDataSets[ 1 ] ] ) ;
               }
               else
               {
                  try
                  {
                     plotable.yTicks[ plotable.displayedDataSets[ 1 ] ] =
                         inYticks.getIntegerValue().
                         intValue() ;
                     if ( plotable.yTicks[ plotable.displayedDataSets[ 1 ] ] <
                          0 )
                        ( ( BIGVerticalLogarithmicAxis ) chart.getXYPlot().
                          getSecondaryRangeAxis() ).setLogTickLabelsFlag( false ) ;
                     else
                        ( ( BIGVerticalLogarithmicAxis ) chart.getXYPlot().
                          getSecondaryRangeAxis() ).setLogTickLabelsFlag( true ) ;
                  }
                  catch ( Exception ex1 )
                  {
                  }
               }
            chart.getXYPlot().setInsets( insetsPanel.getTheInsets() ) ;

            //repaint the chart
            chartPanel.chartChanged( new org.jfree.chart.event.
                                     ChartChangeEvent(
                                         chartPanel.getChart() ) ) ;
            // set the y-axis text
            chartPanel.getChart().getXYPlot().getRangeAxis().setLabel(
                yAxisTextField.getText() ) ;
            if ( plotable.displayedDataSets.length == 2 )
               chartPanel.getChart().getXYPlot().getSecondaryRangeAxis().
                   setLabel(
                       yAxisTextField2.getText() ) ;
            //and the title
            plotable.setTitle( titleTextField.getText() ) ;
            chartPanel.getChart().getTitle().setFont( plotable.getFont( "title" ) ) ;
            // also fonts
            chartPanel.getChart().getTitle().setText( plotable.getExtTitle() ) ;
            ( ( BIGPlotLegend ) ( chart.getLegend() ) ).setItemFont( plotable.
                getFont( "legend" ) ) ;
            // and for sure the annotation
            plotable.setAnnComment( commentTextField.getText() ) ;
            try
            {
               plotable.setCommentPos( Integer.parseInt( commentX.getText() ) ,
                                       Integer.parseInt( commentY.getText() ) ) ;
            }
            catch ( NumberFormatException ex2 )
            {
               System.err.println( "Please insert integer values." ) ;
            }
            // remove old annotation
            chart.getXYPlot().clearAnnotations() ;
            // and add new one
            chart.getXYPlot().addAnnotation( plotable.getAnnotationComment() ) ;
            selfChanging = false ;
            //----------------------------------------------------------------------
            if ( plotable instanceof BIGResultMixer) {
            	( (BIGResultMixer) plotable ).updateResultTree();
            }
	         //----------------------------------------------------------------------
         }
      } ) ;
      // reset to default-values stored in plotable
      JButton resetButton = new JButton(
          new AbstractAction( "reset" )
      {
         public void actionPerformed( ActionEvent e )
         {
            selfChanging = true ;
            int index = plotable.displayedDataSets[ 0 ] ;
            BIGDataSet data = plotable.data[ index ] ;
            data.resetAchsisValues() ;
            data.setPre( 3 ) ;
            data.setPreNamesSelected( false ) ;
            inXmin.setText( "" + data.getMinimumDomainValue() ) ;
            inXmax.setText( "" + data.getMaximumDomainValue() ) ;
            inXticks.setValue( "" + plotable.xTicksDefault ) ;
            xAxisLogarithmic.setValue( "" + plotable.xLogDefault ) ;
            ySet.setSelectedIndex( 3 ) ;
            inYticks.setValue( "" + plotable.yTicksDefault[ index ] ) ;
            yAxisLogarithmic.setValue( "" + plotable.yLogDefault[ index ] ) ;
            inYmax.setText( "" + data.getMaximumRangeValue() ) ;
            inYmin.setText( "" + data.getMinimumRangeValue() ) ;
            BIGDataSet data2 = null ;
            if ( plotable.displayedDataSets.length == 2 )
            {
               int index2 = plotable.displayedDataSets[ 1 ] ;
               data2 = plotable.data[ index ] ;
               data2.resetAchsisValues() ;
               data2.setPre( 3 ) ;
               data2.setPreNamesSelected( false ) ;
               inYmax2.setText( "" + data2.getMaximumRangeValue() ) ;
               inYmin2.setText( "" + data2.getMinimumRangeValue() ) ;
               inYticks.setValue( "" + plotable.yTicksDefault[ index2 ] ) ;
               yAxisLogarithmic.setValue( "" + plotable.yLogDefault[ index2 ] ) ;
               ySet2.setSelectedIndex( 3 ) ;
            }
            // insetsPanel.removeAll();
            insetsPanel.setTheInsets( plotable.
                                      insets ) ;
            insetsPanel.revalidate() ;
            configPanel.revalidate() ;
            //repaint the chart
            chartPanel.getChart().getXYPlot().datasetChanged(
                null ) ;
            xAxisTextField.setText( new String( plotable.xAxisTextDefault ) ) ;
            yAxisTextField.setText( new String( plotable.yAxisTextDefault[
                                                plotable.displayedDataSets[ 0 ] ] ) ) ;
            if ( plotable.displayedDataSets.length == 2 )
               yAxisTextField2.setText( new String( plotable.yAxisTextDefault[
                   plotable.displayedDataSets[ 1 ] ] ) ) ;
            titleTextField.setText( new String( plotable.titleDefault ) ) ;
            chartPanel.getChart().getXYPlot().getRangeAxis().setLabel(
                yAxisTextField.getText() ) ;
            chartPanel.getChart().getTitle().setText( titleTextField.getText() ) ;
            selfChanging = false ;
         }
      } )
          {
         public Dimension getMinimumSize()
         {
            return getPreferredSize() ;
         }
         public Dimension getPreferredSize()
         {
            return new Dimension( BIGPlot.BUTTON_SIZE ,
                                  super.getPreferredSize().height ) ;
         }
         public Dimension getMaximumSize()
         {
            return getPreferredSize() ;
         }
      }
       ;

      //a button for disabling and enabling the 2 blue lines following the mouse
      setAxisTrace( currentAxisTrace ) ;
      crosshairButton = new JButton(
          new AbstractAction( "crosshair On" )
      {
         public void actionPerformed( ActionEvent e )
         {
            currentAxisTrace = !currentAxisTrace ;
            setAxisTrace( currentAxisTrace ) ;
            // rename the button
            crosshairButton.setText(
                "crosshair " +
                ( !currentAxisTrace ? "On" : "Off" ) ) ;
            // deleting the crosshair by repainting
            chartPanel.repaint() ;
         }
      } )
          {
         public Dimension getMinimumSize()
         {
            return getPreferredSize() ;
         }
         public Dimension getPreferredSize()
         {
            return new Dimension( BIGPlot.BUTTON_SIZE ,
                                  super.getPreferredSize().height ) ;
         }
         public Dimension getMaximumSize()
         {
            return getPreferredSize() ;
         }
      }
       ;

      //layout of configtab
      GridBagLayout gridbag = new GridBagLayout() ;
      GridBagConstraints c = new GridBagConstraints() ;
      Insets insetsForGridbag = new Insets( 5 , 5 , 5 , 5 ) ;
      c.insets = insetsForGridbag ;
      configPanel.setLayout( gridbag ) ;
      c.fill = GridBagConstraints.BOTH ;
      int row = 0 ;

      //first row
      // and following. building the config tab...

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "x min:" ) , gridbag , c ) ;
      setConstraints( c , 1 , row , 1 , 1 ) ;
      addComponent( configPanel , inXmin , gridbag , c ) ;

      setConstraints( c , 2 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "x max:" , SwingConstants.RIGHT ) ,
                    gridbag , c ) ;
      setConstraints( c , 3 , row , 1 , 1 ) ;
      addComponent( configPanel , inXmax , gridbag , c ) ;

      setConstraints( c , 4 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "x ticks:" ) , gridbag , c ) ;
      setConstraints( c , 5 , row , 1 , 1 ) ;
      addComponent( configPanel , inXticks , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "x axis text:" ) , gridbag , c ) ;
      setConstraints( c , 1 , row , 3 , 1 ) ;
      addComponent( configPanel , xAxisTextField , gridbag , c ) ;

      setConstraints( c , 4 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "x axis logbase:" ) , gridbag , c ) ;
      setConstraints( c , 5 , row , 1 , 1 ) ;
      addComponent( configPanel , xAxisLogarithmic , gridbag , c ) ;

      row++ ;

      setConstraints( c , 0 , row , GridBagConstraints.REMAINDER , 1 ) ;
      // add a spacer between the x-section and the following y-section
      addComponent( configPanel ,
                    new JPanel()
      {
         public Dimension getMinimumSize()
         {
            return getPreferredSize() ;
         }

         public Dimension getPreferredSize()
         {
            return new Dimension( super.getPreferredSize().width , 10 ) ;
         }

         public Dimension getMaximumSize()
         {
            return getPreferredSize() ;
         }
      } ,
          gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "y1 min:" ) , gridbag , c ) ;
      setConstraints( c , 1 , row , 1 , 1 ) ;
      addComponent( configPanel , inYmin , gridbag , c ) ;

      setConstraints( c , 2 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "y1 max:" , SwingConstants.RIGHT ) ,
                    gridbag , c ) ;
      setConstraints( c , 3 , row , 1 , 1 ) ;
      addComponent( configPanel , inYmax , gridbag , c ) ;

      setConstraints( c , 4 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "y1 ticks:" ) , gridbag , c ) ;
      setConstraints( c , 5 , row , 1 , 1 ) ;
      addComponent( configPanel , inYticks , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "y1 axis text:" ) , gridbag , c ) ;
      setConstraints( c , 1 , row , 3 , 1 ) ;
      addComponent( configPanel , yAxisTextField , gridbag , c ) ;

      setConstraints( c , 4 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "y1 axis logbase:" ) , gridbag ,
                    c ) ;
      setConstraints( c , 5 , row , 1 , 1 ) ;
      addComponent( configPanel , yAxisLogarithmic , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "y1 scaling factor:" ) , gridbag ,
                    c ) ;
      setConstraints( c , 1 , row , 1 , 1 ) ;
      addComponent( configPanel , ySet , gridbag , c ) ;

      setConstraints( c , 2 , row , 2 , 1 ) ;
      scaleYAxisText.setHorizontalAlignment( SwingConstants.LEFT ) ;
      addComponent( configPanel , scaleYAxisText , gridbag , c ) ;

      setConstraints( c , 4 , row , 2 , 1 ) ;
      yPreAlsoLegends.setHorizontalAlignment( SwingConstants.LEFT ) ;
      addComponent( configPanel , yPreAlsoLegends , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 2 , 1 ) ;
      yAxisFillShapes1.setHorizontalAlignment( SwingConstants.LEFT ) ;
      addComponent( configPanel , yAxisFillShapes1 , gridbag , c ) ;

      setConstraints( c , 2 , row , 4 , 1 ) ;
      yAxisShowLines1.setHorizontalAlignment( SwingConstants.LEFT ) ;
      addComponent( configPanel , /*showLinesPanel1*/ yAxisShowLines1 , gridbag ,
                    c ) ;
      row++ ;

      // set a spacer between this y-section and the following y2-section (if data2 exists)
      // or button-section (if data2 not exists)
      setConstraints( c , 0 , row , GridBagConstraints.REMAINDER , 1 ) ;
      addComponent( configPanel ,
                    new JPanel()
      {
         public Dimension getMinimumSize()
         {
            return getPreferredSize() ;
         }

         public Dimension getPreferredSize()
         {
            return new Dimension( super.getPreferredSize().width , 10 ) ;
         }

         public Dimension getMaximumSize()
         {
            return getPreferredSize() ;
         }
      } ,
          gridbag , c ) ;
      row++ ;

      if ( data2 != null )
      {
         setConstraints( c , 0 , row , 1 , 1 ) ;
         addComponent( configPanel , new JLabel( "y2 min:" ) , gridbag , c ) ;
         setConstraints( c , 1 , row , 1 , 1 ) ;
         addComponent( configPanel , inYmin2 , gridbag , c ) ;

         setConstraints( c , 2 , row , 1 , 1 ) ;
         addComponent( configPanel ,
                       new JLabel( "y2 max:" , SwingConstants.RIGHT ) , gridbag ,
                       c ) ;
         setConstraints( c , 3 , row , 1 , 1 ) ;
         addComponent( configPanel , inYmax2 , gridbag , c ) ;

         setConstraints( c , 4 , row , 1 , 1 ) ;
         addComponent( configPanel , new JLabel( "y2 ticks:" ) , gridbag , c ) ;
         setConstraints( c , 5 , row , 1 , 1 ) ;
         addComponent( configPanel , inYticks2 , gridbag , c ) ;
         row++ ;

         setConstraints( c , 0 , row , 1 , 1 ) ;
         addComponent( configPanel , new JLabel( "y2 axis text:" ) , gridbag ,
                       c ) ;
         setConstraints( c , 1 , row , 3 , 1 ) ;
         addComponent( configPanel , yAxisTextField2 , gridbag , c ) ;

         setConstraints( c , 4 , row , 1 , 1 ) ;
         addComponent( configPanel , new JLabel( "y2 axis logbase:" ) , gridbag ,
                       c ) ;
         setConstraints( c , 5 , row , 1 , 1 ) ;
         addComponent( configPanel , yAxisLogarithmic2 , gridbag , c ) ;
         row++ ;

         setConstraints( c , 0 , row , 1 , 1 ) ;
         addComponent( configPanel , new JLabel( "y2 scaling factor:" ) ,
                       gridbag , c ) ;
         setConstraints( c , 1 , row , 1 , 1 ) ;
         addComponent( configPanel , ySet2 , gridbag , c ) ;

         setConstraints( c , 2 , row , 2 , 1 ) ;
         scaleYAxisText2.setHorizontalAlignment( SwingConstants.LEFT ) ;
         addComponent( configPanel , scaleYAxisText2 , gridbag , c ) ;

         setConstraints( c , 4 , row , 2 , 1 ) ;
         yPreAlsoLegends2.setHorizontalAlignment( SwingConstants.LEFT ) ;
         addComponent( configPanel , /*scaleLegendsPanel2*/ yPreAlsoLegends2 ,
                       gridbag , c ) ;
         row++ ;

         setConstraints( c , 0 , row , 2 , 1 ) ;
         yAxisFillShapes2.setHorizontalAlignment( SwingConstants.LEFT ) ;
         addComponent( configPanel , yAxisFillShapes2 , gridbag , c ) ;
         setConstraints( c , 2 , row , 2 , 1 ) ;
         yAxisShowLines2.setHorizontalAlignment( SwingConstants.LEFT ) ;
         addComponent( configPanel , yAxisShowLines2 , gridbag , c ) ;
         row++ ;

         // add a spacer between this y2-section and the follwong button-section
         setConstraints( c , 0 , row , GridBagConstraints.REMAINDER , 1 ) ;
         addComponent( configPanel ,
                       new JPanel()
         {
            public Dimension getMinimumSize()
            {
               return getPreferredSize() ;
            }

            public Dimension getPreferredSize()
            {
               return new Dimension( super.getPreferredSize().width , 10 ) ;
            }

            public Dimension getMaximumSize()
            {
               return getPreferredSize() ;
            }
         } ,
             gridbag , c ) ;
         row++ ;
      }

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "Title:" ) , gridbag , c ) ;
      setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
      addComponent( configPanel , titleTextField , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "Comment:" ) , gridbag , c ) ;
      setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
      addComponent( configPanel , commentTextField , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "X-Pos of comment (%):" ) ,
                    gridbag , c ) ;
      setConstraints( c , 1 , row , 1 , 1 ) ;
      addComponent( configPanel , commentX , gridbag , c ) ;

      setConstraints( c , 2 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "Y-Pos of comment (%):" ) ,
                    gridbag , c ) ;
      setConstraints( c , 3 , row , 1 , 1 ) ;
      addComponent( configPanel , commentY , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , 1 , 1 ) ;
      addComponent( configPanel , new JLabel( "Insets:" ) , gridbag , c ) ;
      setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
      addComponent( configPanel , insetsPanel , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , GridBagConstraints.REMAINDER , 1 ) ;
      resetButtonPanel.add( resetButton , BorderLayout.CENTER ) ;
      addComponent( configPanel , resetButtonPanel , gridbag , c ) ;
      row++ ;

      setConstraints( c , 0 , row , GridBagConstraints.REMAINDER , 1 ) ;
      crosshairButtonPanel.add( crosshairButton , BorderLayout.CENTER ) ;
      addComponent( configPanel , crosshairButtonPanel , gridbag , c ) ;
      row++ ;

      // ! BEGIN COLOR - Select
      JPanel colorSetPanel = new JPanel() ;
      setConstraints( c , 0 , row , GridBagConstraints.REMAINDER , 1 ) ;
      JButton colorButton = new JButton(
          "Change colors, names and shapes for single functions" )
      {
         public Dimension getMinimumSize()
         {
            return getPreferredSize() ;
         }

         public Dimension getPreferredSize()
         {
            return new Dimension( BIGPlot.BUTTON_SIZE ,
                                  super.getPreferredSize().height ) ;
         }

         public Dimension getMaximumSize()
         {
            return getPreferredSize() ;
         }
      } ;
      colorSetPanel.add( colorButton , BorderLayout.CENTER ) ;
      colorButton.setToolTipText(
          "Set the color, names and shapes for the shown functions." ) ;
      // set colors and legends
      colorButton.addActionListener( new ActionListener()
      {
         int lastIndex = 0 ;
         int j = 0 ;

         public void actionPerformed( ActionEvent evt )
         {
            // one yaxis
            if ( plotable.displayedDataSets.length < 2 )
            {
               // get the names of the legends for 1st yaxis
               // this means its a refference! so if you change it here
               // it will also be changed in plotable
               final String[] names = plotable.data[ plotable.displayedDataSets[
                   0 ] ].getNames() ;
               // show the plot
               displayTabPane.setSelectedIndex( 0 ) ;
               displayTabPane.revalidate() ;
               // build a new frame, where the stuff can be selected
               JFrame jf = new JFrame( "Color" ) ;
               final JPanel buttonPanel = new JPanel() ;
               buttonPanel.setLayout( new FlowLayout() ) ;

               // set button in the new frame
               final JButton set = new JButton( "Set name and color for \"" +
                                                names[ 0 ] + "\"" ) ;
               // a colorchooser for choosing colors
               final JColorChooser jcc = new JColorChooser( ( Color ) chart.
                   getXYPlot().getRenderer().getItemPaint( 0 , 0 , 0 ) ) ;
               // combobox for the legends
               final JComboBox jcbSelect = new JComboBox( names ) ;
               //-------------------------------------------------------------------
               // combobox for the shapes
               final ShapeComboBox shapeComboBox = new BIGPlot.ShapeComboBox(
                   BIGPlotRenderer.BIG_SHAPES ) ;
               int row = 0 ;
               // panel for the shape selection
               final JPanel scbPanel = new JPanel() ;
               scbPanel.setLayout( new FlowLayout() ) ;
               //-------------------------------------------------------------------
               // can be edited to change legends
               jcbSelect.setEditable( true ) ;
               // if in the combobox is selected sth else
               jcbSelect.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent ae )
                  {
                     // which is the selected legend/function
                     j = jcbSelect.getSelectedIndex() ;
                     // if none is selected
                     if ( j == -1 )
                     {
                        // select the last or default (0)
                        j = lastIndex ;
                        // set name
                        //names[ j ] = ( String ) jcbSelect.getSelectedItem() ;
                        //------
                        plotable.changeName( 0 , j ,
                                             ( String ) jcbSelect.getSelectedItem() ) ;
                        //System.out.println("Plot");
                        //------
                        // remove all items
                        while ( jcbSelect.getItemCount() > 0 )
                           jcbSelect.removeItemAt( 0 ) ;
                        // and add the new ones (one was renamed)
                        for ( int i = 0 ; i < names.length ; i++ )
                           jcbSelect.addItem( names[ i ] ) ;
                        // revalidate the combobox
                        jcbSelect.revalidate() ;
                     }
                     // set the color for this function
                     jcc.setColor( ( Color )
                                   chart.getXYPlot().getRenderer().
                                   getItemPaint( 0 ,
                                                 j , 0 )
                         ) ;
                     jcc.setToolTipText( "Color for " + names[ j ] ) ;
                     jcc.revalidate() ;
                     // rebuild the set button (set the name for actual legend)
                     set.setText( "Set name and color for \"" + names[ j ] +
                                  "\"" ) ;

                     // rebuild the shape selection
                     Shape actualShape = chart.getXYPlot().getRenderer().
                         getSeriesShape( 0 , j ) ;
                     for ( int index = 0 ;
                           index < BIGPlotRenderer.BIG_SHAPES.length ; index++ )
                     {
                        // System.out.println("FOR: " + index);
                        if ( BIGPlotRenderer.BIG_SHAPES[ index ].equals(
                            actualShape ) )
                        {
                           shapeComboBox.setSelectedIndex( index ) ;
                           // System.out.println("IF: " +  index );
                        }
                     }
                     // shapeComboBox.setSelectedItem( chart.getXYPlot().getRenderer().getSeriesShape( 0 ,j ) );
                     shapeComboBox.revalidate() ;

                     // revalidate it
                     set.revalidate() ;
                     // do the action from set-button
                     set.getActionListeners()[ 0 ].actionPerformed( null ) ;
                     lastIndex = j ;
                  }
               } ) ;
               buttonPanel.add( new JLabel( "select actual Function:" ) ) ;
               buttonPanel.add( jcbSelect ) ;
               JPanel main = new JPanel() ;
               GridBagLayout gridbag = new GridBagLayout() ;
               GridBagConstraints c = new GridBagConstraints() ;
               c.fill = GridBagConstraints.BOTH ;
               c.weightx = 1.0 ;
               main.setLayout( gridbag ) ;

               JScrollPane jsp = new JScrollPane( buttonPanel ) ;
               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , jsp , gridbag , c ) ;
               row++ ;

               jcc.setToolTipText( "Color for " + names[ 0 ] ) ;
               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , jcc , gridbag , c ) ;
               row++ ;

               // sets action: set color, new name and new shape
               set.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent evt )
                  {
                     chart.getXYPlot().getRenderer().setSeriesPaint( j ,
                         jcc.getColor() ) ;
                     chart.getXYPlot().datasetChanged(
                         new org.jfree.data.DatasetChangeEvent(
                             new Object() ,
                             plotable.data[ plotable.displayedDataSets[ 0 ] ] ) ) ;

                  }
               } ) ;

               // a listener, that handles changes of the selected item
               shapeComboBox.addItemListener( new ItemListener()
               {
                  public void itemStateChanged( ItemEvent evt )
                  {
                     int index = ( ( ShapeComboBox ) evt.getSource() ).
                         getSelectedIndex() ;
                     // draw the new shape
                     ( ( BIGPlotRenderer ) ( chart.getXYPlot().getRenderer() ) ).
                         setSeriesShape( j ,
                                         ( ( ShapeComboBox ) evt.getSource() ).
                                         getShape( index ) ) ;
                     chart.getXYPlot().datasetChanged(
                         new org.jfree.data.DatasetChangeEvent(
                             new Object() ,
                             plotable.data[ plotable.displayedDataSets[ 0 ] ] ) ) ;
                  }
               } ) ;
               // add shapeComboBox to a panel
               scbPanel.add( new JLabel( "select a shape for actual function:" ) ) ;
               scbPanel.add( shapeComboBox ) ;
               // add the panel to main panel
               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , scbPanel , gridbag , c ) ;
               row++ ;

               // finaly end frame building
               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , set , gridbag , c ) ;
               row++ ;

               jf.setDefaultCloseOperation( JFrame.DISPOSE_ON_CLOSE ) ;
               jf.setContentPane( new JScrollPane( main ) ) ;
               jf.pack() ;
               // jf.setSize( 640 , 480 ) ;
               jf.setVisible( true ) ;
               // by showing it
               jf.toFront() ;
            }
            // the same for 2 yaxis
            else
            {
               final String[] names1 = plotable.data[ plotable.
                   displayedDataSets[ 0 ] ].
                   getNames() ;
               final String[] names2 = plotable.data[ plotable.
                   displayedDataSets[ 1 ] ].
                   getNames() ;
               final String[] names = new String[names1.length + names2.length ] ;
               for ( int i = 0 ; i < names1.length ; i++ )
               {
                  names[ i ] = names1[ i ] ;
               }
               for ( int i = 0 ; i < names2.length ; i++ )
               {
                  names[ i + names1.length ] = names2[ i ] ;
               }

               displayTabPane.setSelectedIndex( 0 ) ;
               JFrame jf = new JFrame( "Color" ) ;
               final JScrollPane content = new JScrollPane() ;
               final JPanel buttonPanel = new JPanel() ;
               buttonPanel.setLayout( new FlowLayout() ) ;
               final JButton set = new JButton( "Set name and color for \"" +
                                                names[ 0 ] + "\"" ) ;
               final JColorChooser jcc = new JColorChooser( ( Color ) chart.
                   getXYPlot().getRenderer().getItemPaint( 0 , 0 , 0 ) ) ;

               //-------------------------------------------------------------------
               // combobox for the shapes
               final ShapeComboBox shapeComboBox = new BIGPlot.ShapeComboBox(
                   BIGPlotRenderer.BIG_SHAPES ) ;
               int row = 0 ;
               // panel for the shape selection
               final JPanel scbPanel = new JPanel() ;
               scbPanel.setLayout( new FlowLayout() ) ;
               //-------------------------------------------------------------------
               final JComboBox jcbSelect = new JComboBox( names ) ;
               jcbSelect.setEditable( true ) ;
               jcbSelect.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent ae )
                  {
                     j = jcbSelect.getSelectedIndex() ;
                     //System.err.println(j);
                     if ( jcbSelect.getSelectedIndex() < 0 )
                     {

                        j = lastIndex ;
                        names[ j ] = ( String ) jcbSelect.getSelectedItem() ;
                        if ( j < names1.length )
                           //names1[ j ] = names[ j ] ;
                           plotable.changeName( 0 , j , names[ j ] ) ;
                        else
                           //names2[ j - names1.length ] = names[ j ] ;
                           plotable.changeName( 1 , j - names1.length ,
                                                names[ j ] ) ;
                        while ( jcbSelect.getItemCount() > 0 )
                           jcbSelect.removeItemAt( 0 ) ;
                        for ( int i = 0 ; i < names.length ; i++ )
                           jcbSelect.addItem( names[ i ] ) ;
                        jcbSelect.revalidate() ;

                        // rebuild the shape selection
                        Shape actualShape ;
                        if ( j < names1.length )
                        {
                           actualShape = chart.getXYPlot().getRenderer().
                               getSeriesShape( 0 , j ) ;
                        }
                        else
                        {
                           actualShape = chart.getXYPlot().getSecondaryRenderer().
                               getSeriesShape( 0 , j - names1.length ) ;
                        }
                        for ( int index = 0 ;
                              index < BIGPlotRenderer.BIG_SHAPES.length ; index++ )
                        {
                           // System.out.println("FOR: " + index);
                           if ( BIGPlotRenderer.BIG_SHAPES[ index ].equals(
                               actualShape ) )
                           {
                              shapeComboBox.setSelectedIndex( index ) ;
                              // System.out.println("IF: " +  index );
                           }
                        }
                        shapeComboBox.revalidate() ;

                        set.setText( "Set name and color for \"" + names[ j ] +
                                     "\"" ) ;
                        set.revalidate() ;
                        set.getActionListeners()[ 0 ].actionPerformed( null ) ;
                        return ;

                     }

                     if ( j < names1.length )
                     {

                        jcc.setColor( ( Color )
                                      chart.getXYPlot().getRenderer().
                                      getItemPaint( 0 , j ,
                            0 ) ) ;
                        jcc.setToolTipText( "Color for" + names1[ j ] ) ;
                        jcc.revalidate() ;
                        // rebuild the shape selection
                        Shape actualShape = chart.getXYPlot().getRenderer().
                            getSeriesShape( 0 , j ) ;
                        for ( int index = 0 ;
                              index < BIGPlotRenderer.BIG_SHAPES.length ; index++ )
                        {
                           if ( BIGPlotRenderer.BIG_SHAPES[ index ].equals(
                               actualShape ) )
                           {
                              shapeComboBox.setSelectedIndex( index ) ;
                           }
                        }
                        shapeComboBox.revalidate() ;
                        // rebuild the set button
                        set.setText( "Set name and color for \"" + names[ j ] +
                                     "\"" ) ;
                        set.revalidate() ;
                        set.getActionListeners()[ 0 ].actionPerformed( null ) ;

                     }
                     else
                     {
                        jcc.setColor( ( Color )
                                      chart.getXYPlot().getSecondaryRenderer().
                                      getItemPaint(
                                          0 , j - names1.length , 0 ) ) ;
                        jcc.setToolTipText( "Color for" + names2[ j -
                                            names1.length ] ) ;
                        jcc.revalidate() ;
                        // rebuild the shape selection
                        Shape actualShape = chart.getXYPlot().
                            getSecondaryRenderer().getSeriesShape( 0 ,
                            j - names1.length ) ;
                        for ( int index = 0 ;
                              index < BIGPlotRenderer.BIG_SHAPES.length ; index++ )
                        {
                           if ( BIGPlotRenderer.BIG_SHAPES[ index ].equals(
                               actualShape ) )
                           {
                              shapeComboBox.setSelectedIndex( index ) ;
                           }
                        }
                        shapeComboBox.revalidate() ;
                        // rebuild the set button
                        set.setText( "Set name and color for \"" + names[ j ] +
                                     "\"" ) ;
                        set.revalidate() ;
                        set.getActionListeners()[ 0 ].actionPerformed( null ) ;

                     }
                     lastIndex = j ;
                  }
               } ) ;
               buttonPanel.add( new JLabel( "select actual Function:" ) ) ;
               buttonPanel.add( jcbSelect ) ;
               JPanel main = new JPanel() ;
               GridBagLayout gridbag = new GridBagLayout() ;
               GridBagConstraints c = new GridBagConstraints() ;
               c.fill = GridBagConstraints.BOTH ;
               c.weightx = 1.0 ;
               main.setLayout( gridbag ) ;

               JScrollPane jsp = new JScrollPane( buttonPanel ) ;
               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , jsp , gridbag , c ) ;
               row++ ;

               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , jcc , gridbag , c ) ;
               row++ ;
               /*
                main.add( new JScrollPane( buttonPanel ) , BorderLayout.NORTH ) ;
                         main.add( jcc , BorderLayout.CENTER ) ;*/

               set.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent evt )
                  {
                     if ( j < names1.length )
                        chart.getXYPlot().getRenderer().setSeriesPaint( 0 , j ,
                            jcc.getColor() ) ;
                     else
                        chart.getXYPlot().getSecondaryRenderer().setSeriesPaint(
                            0 ,
                            j - names1.length ,
                            jcc.getColor() ) ;

                     chart.getXYPlot().datasetChanged(
                         new org.jfree.data.DatasetChangeEvent(
                             new Object() ,
                             plotable.data[ plotable.displayedDataSets[ 0 ] ] ) ) ;

                     chart.getXYPlot().datasetChanged(
                         new org.jfree.data.DatasetChangeEvent(
                             new Object() ,
                             plotable.data[ plotable.displayedDataSets[ 1 ] ] ) ) ;

                  }
               } ) ;

               // a listener, that handles changes of the selected item
               shapeComboBox.addItemListener( new ItemListener()
               {
                  public void itemStateChanged( ItemEvent evt )
                  {
                     int index = ( ( ShapeComboBox ) evt.getSource() ).
                         getSelectedIndex() ;
                     // draw the new shape
                     if ( j < names1.length )
                     {
                        ( ( BIGPlotRenderer ) ( chart.getXYPlot().getRenderer() ) ).
                            setSeriesShape( j ,
                                            ( ( ShapeComboBox ) evt.getSource() ).
                                            getShape( index ) ) ;
                     }
                     else
                     {
                        ( ( BIGPlotRenderer ) ( chart.getXYPlot().
                                                getSecondaryRenderer() ) ).
                            setSeriesShape( j - names1.length ,
                                            ( ( ShapeComboBox ) evt.getSource() ).
                                            getShape( index ) ) ;
                     }

                     chart.getXYPlot().datasetChanged(
                         new org.jfree.data.DatasetChangeEvent(
                             new Object() ,
                             plotable.data[ plotable.displayedDataSets[ 0 ] ] ) ) ;
                     chart.getXYPlot().datasetChanged(
                         new org.jfree.data.DatasetChangeEvent(
                             new Object() ,
                             plotable.data[ plotable.displayedDataSets[ 1 ] ] ) ) ;
                  }
               } ) ;

               // add shapeComboBox to a panel
               scbPanel.add( new JLabel( "select a shape for actual function:" ) ) ;
               scbPanel.add( shapeComboBox ) ;
               // add the panel to main panel
               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , scbPanel , gridbag , c ) ;
               row++ ;

               setConstraints( c , 1 , row , GridBagConstraints.REMAINDER , 1 ) ;
               addComponent( main , set , gridbag , c ) ;
               row++ ;
               // main.add( set , BorderLayout.SOUTH ) ;

               content.setViewportView( main ) ;

               jf.setContentPane( content ) ;
               jf.pack() ;
               // jf.setSize( 640 , 480 ) ;
               jf.setVisible( true ) ;

            }
         }
      } ) ;
      addComponent( configPanel , colorSetPanel , gridbag , c ) ;
      // END COLOR - Select
      // start font

      row++ ;
      // ! BEGIN Font - Select
      JPanel fontSetPanel = new JPanel() ;
      setConstraints( c , 0 , row , GridBagConstraints.REMAINDER , 1 ) ;
      JButton fButton = new JButton( "Set Plot Fonts" )
      {
         public Dimension getMinimumSize()
         {
            return getPreferredSize() ;
         }

         public Dimension getPreferredSize()
         {
            return new Dimension( BIGPlot.BUTTON_SIZE ,
                                  super.getPreferredSize().height ) ;
         }

         public Dimension getMaximumSize()
         {
            return getPreferredSize() ;
         }
      } ;
      fontSetPanel.add( fButton , BorderLayout.CENTER ) ;
      fButton.setToolTipText(
          "Set the fonts for this plot." ) ;
      // when the button is pressed a frame will be opened, where fonts can be selected
      fButton.addActionListener(
          new ActionListener()
      {
         public void actionPerformed( ActionEvent aevt )
         {
            JFrame f = new BIGFontDialog( plotable , displayTabPane ,
                                          setButton.getActionListeners()[ 0 ] ) ;
            f.setVisible( true ) ;
         }
      } ) ;

      addComponent( configPanel , fontSetPanel , gridbag , c ) ;
      // these panels are for an integrated GUI
      displayTabPane.addTab( "plot" , chartPanel ) ;
      JScrollPane scPan = new JScrollPane( configPanel ) ;
      scPan.getVerticalScrollBar().setBlockIncrement( 50 ) ;
      displayTabPane.addTab( "config" , scPan ) ;
      displayPanels = new JComponent[2 ] ;
      displayPanels[ 0 ] = ( JComponent ) displayTabPane ;
      displayPanels[ 1 ] = ( JComponent ) textScrollPane ;
      // when switching ...
      displayTabPane.addChangeListener( new javax.swing.event.ChangeListener()
      {
         public void stateChanged( javax.swing.event.ChangeEvent evt )
         {
            // to the plot-tab
            if ( ( ( JTabbedPane ) evt.getSource() ).getSelectedIndex() == 0 )
            {
               // do, as if the setbutton was pressed
               setButton.getAction().actionPerformed( new ActionEvent( new
                   Object() ,
                   0 , "" ) ) ;
               ( ( ChartPanel ) ( ( ( JTabbedPane ) evt.getSource() ).
                                  getComponentAt( 0 ) ) ).revalidate() ;
               // then repaint it
               ( ( JTabbedPane ) evt.getSource() ).revalidate() ;
               ( ( JTabbedPane ) evt.getSource() ).repaint() ;

            }
         }
      } ) ;
      // at last set title font (never "touched" title before, so why not here?)
      chart.getTitle().setFont( this.plotable.getFont( "title" ) ) ;
      // set the plotable for legends
      chart.setLegend( new BIGPlotLegend( chart ) ) ;
      ( ( BIGPlotLegend ) ( chart.getLegend() ) ).setPlotable( this.plotable ) ;
      ( ( BIGPlotLegend ) ( chart.getLegend() ) ).setDisplaySeriesShapes( true ) ;
      ( ( BIGPlotLegend ) ( chart.getLegend() ) ).setItemFont( this.plotable.
          getFont( "legend" ) ) ;
      chart.getXYPlot().clearAnnotations() ;
      plotable.setAnnComment( plotable.getAnnComment() ) ;
      chart.getXYPlot().addAnnotation( plotable.getAnnotationComment() ) ;

      // legend has changed. the 2 lines above.
      chartPanel.getChart().legendChanged( new LegendChangeEvent( chart.
          getLegend() ) ) ;
      // we dont change anymore
      selfChanging = false ;
      // because this ends.
   }

   /**
    * dont use this constructor
    */
   public BIGPlot(
       )
   {

   }

   /**
    * gets the display-panels ([0]plot, [1] text)
    * @return JComponent[] the panels
    */
   public JComponent[] getDisplayPanels()
   {
      return displayPanels ;
   }

   /**
    * gets the plotted Panel
    * @return ChartPanel the chartPanel of this BIGPlot
    */
   public ChartPanel getChartPanel()
   {
      return this.chartPanel ;
   }

   private void addComponent(
       JComponent container , JComponent component ,
       GridBagLayout gridbag , GridBagConstraints c )
   {
      gridbag.setConstraints( component , c ) ;
      container.add( component ) ;
   }

   private void setConstraints( GridBagConstraints c ,
                                int x , int y , int gw , int gh )
   {
      c.gridx = x ;
      c.gridy = y ;
      c.gridwidth = gw ;
      c.gridheight = gh ;
   }

   private void setAxisTrace( boolean value )
   {
      chartPanel.setVerticalAxisTrace( value ) ;
      chartPanel.setHorizontalAxisTrace( value ) ;
   }

   /**
    * looks for the "Save as"-entry in the menuitem of the plot and calls it
    */
   public void saveAsRequest()
   {
      // starting a thread
      ( new Thread()
      {
         public void run()
         {

            // getting the entries of the popUpMenu
            MenuElement[] menuele = chartPanel.getPopupMenu().getSubElements() ;
            // getting the saveAs...
            JMenuItem saveAs = null ;
            for ( int i = 0 ; i < menuele.length ; i++ )
               if ( ( ( JMenuItem ) menuele[ i ].getComponent() ).getText().
                    equals(
                        "Save as..." ) )
                  saveAs = ( ( JMenuItem ) menuele[ i ].getComponent() ) ;
            if ( saveAs == null )
               return ;
            else
               saveAs.getActionListeners()[ 0 ].actionPerformed( null ) ;
         }
      } ).start() ;
   }

   /**
    * overrides the menu of the chartPanel to change (and expand) save as
    * @param chartPanel ChartPanel
    * @param dataSet1 BIGDataSet
    * @param dataSet2 BIGDataSet
    * @param text String
    */
   private void overrideMenu( final ChartPanel chartPanel ,
                              final BIGDataSet dataSet1 ,
                              final BIGDataSet dataSet2 ,
                              final String text )
   {
      final double[][] values1 = dataSet1.getValues() ;
      double[][] values2Temp = new double[0 ][ 0 ] ;
      if ( dataSet2 != null )
         values2Temp = dataSet2.getValues() ;
      final double[][] values2 = values2Temp ;
      final String[] names1 = dataSet1.getNames() ;
      String[] names2Temp = new String[0 ] ;
      if ( dataSet2 != null )
         names2Temp = dataSet2.getNames() ;
      final String[] names2 = names2Temp ;

      // getting the entries of the popUpMenu
      MenuElement[] menuele = chartPanel.getPopupMenu().getSubElements() ;
      // getting the saveAs...
      JMenuItem saveAs = null ;
      for ( int i = 0 ; i < menuele.length ; i++ )
         if ( ( ( JMenuItem ) menuele[ i ].getComponent() ).getText().equals(
             "Save as..." ) )
            saveAs = ( ( JMenuItem ) menuele[ i ].getComponent() ) ;
      // setting his new Action
      saveAs.removeActionListener( saveAs.getActionListeners()[ 0 ] ) ;
      saveAs.addActionListener( new ActionListener()
      {
         int height = 480 ;
         int width = 640 ;
         public void actionPerformed( ActionEvent evt )
         {
            // starting a thread
            ( new Thread()
            {
               public void run()
               {
                  // a filechooser
                  final JFrame f = new JFrame() ;
                  f.setSize( 600 , 400 ) ;
                  JFileChooser fch = new JFileChooser()
                  {
                     // when we selected a file
                     public void approveSelection()
                     {
                        BIGInterface.getInstance().setLastWorkPath( this.
                            getCurrentDirectory() ) ;
                        if ( debug ) System.err.println( this.
                            getCurrentDirectory() ) ;
                        if ( getSelectedFile().isDirectory() )
                        {
                           System.err.println(
                               "The selected output-file is a directory" ) ;
                        }
                        // if there is the File with the same name
                        if ( getSelectedFile().exists() )
                        {
                           // we bring an optionDialog
                           String[] options =
                               {
                               "Okay" , "Cancel"} ;
                           int result = javax.swing.JOptionPane.
                               showOptionDialog(
                                   chartPanel ,
                                   "The selected file exists. Do you want to overwrite it?" ,
                                   "File exists" ,
                                   JOptionPane.DEFAULT_OPTION ,
                                   JOptionPane.INFORMATION_MESSAGE , null ,
                                   options ,
                                   options[ 0 ] ) ;
                           // if cancel was pressed
                           if ( result == 1 )
                           {
                              return ;
                           }
                        }
                        // this will contain the extension of the file
                        final String extension = this.getFileFilter().
                            getDescription().
                            substring( 2 ,
                                       this.getFileFilter().
                                       getDescription().
                                       length() ) ;
                        String name = getSelectedFile().getName() ;
                        // if you've selected a file a.jpg name is a not a.jpg
                        if ( getSelectedFile().getName().endsWith( extension ) )
                           name = name.substring( 0 ,
                                                  name.length() - extension.length() -
                                                  1 ) ;
                        // the name of the new File
                        final String nameOfFile = getCurrentDirectory() +
                            java.io.File.separator +
                            name ;
                        // but if the extension is okay
                        f.setVisible( false ) ;
                        // saving csv
                        if ( extension.equals( "csv" ) )
                        {
                           ( new Thread()
                           {
                              public void run()
                              {
                                 // save data
                                 int where1 = plotable.displayedDataSets[ 0 ] ;
                                 int where2 = plotable.displayedDataSets[ 0 ] ;
                                 // more then 1 yaxis
                                 if ( plotable.displayedDataSets.length > 1 )
                                    where2 = plotable.displayedDataSets[ 1 ] ;
                                 // names of the xaxis and functions
                                 String[] theNames = new String[
                                     names1.length +
                                     names2.length + 1 ] ;
                                 theNames[ 0 ] = plotable.xAxisText ;
                                 for ( int i = 0 ; i < names1.length ;
                                       i++ )
                                    theNames[ i +
                                        1 ] = names1[ i ] +
                                        plotable.yAxisText[ where1 ] ;
                                 for ( int i = 0 ; i < names2.length ;
                                       i++ )
                                    theNames[ i + 1 +
                                        names1.length ] = names2[
                                        i ] +
                                        plotable.yAxisText[ where2 ] ;
                                 // preparig the values
                                 double[][] theValues = new double[
                                     values1.length +
                                     values2.length -
                                     1 ][ values1[ 0 ].length ] ;
                                 // write values (copy)
                                 // if you want to improve it use System.arrayCopy?
                                 for ( int i = 0 ; i < values1.length ;
                                       i++ )
                                    for ( int j = 0 ;
                                          j < values1[ 0 ].length ;
                                          j++ )
                                       theValues[ i ][ j ] = values1[ i ][
                                           j ] ;
                                 for ( int i = 0 ; i < values2.length ;
                                       i++ )
                                    for ( int j = 0 ;
                                          j < values2[ 0 ].length ;
                                          j++ )
                                       theValues[ i +
                                           values1.length -
                                           1 ][ j ] = values1[ i ][
                                           j ] ;
                                 // another info
                                 String[] additionalInfo =
                                     {
                                     plotable.getExtTitle()} ;
                                 // start export
                                 startFileExporting( nameOfFile + "." +
                                     extension , this ) ;
                                 system.BIGUtility.saveToCsv( new
                                     File( nameOfFile ) ,
                                     theValues ,
                                     theNames ,
                                     additionalInfo ) ;
                              }
                           } ).start() ;
                           return ;
                        }
                        // asking for resolution of jpg/png (only av for these ext.)
                        javax.swing.JOptionPane heightAndWidth = new
                            javax.
                            swing.
                            JOptionPane() ;
                        // used sth else then int for resolution
                        boolean threwException = false ;
                        // now temp, not temp will be final
                        int tempWidth = 0 , tempHeight = 0 ;
                        if ( ( extension.equals( "jpg" ) ) ||
                             ( extension.equals( "png" ) ) )
                           do
                           {
                              threwException = false ;
                              // building option dialog
                              Object[][] message = new Object[2 ][ 2 ] ;
                              message[ 0 ][ 0 ] = new javax.swing.JLabel(
                                  "Width" ) ;
                              message[ 0 ][ 1 ] = new javax.swing.
                                  JTextField( "" +
                                              BIGInterface.getInstance().getSaveWidth() ) ;
                              message[ 1 ][ 0 ] = new javax.swing.JLabel(
                                  "Height" ) ;
                              message[ 1 ][ 1 ] = new javax.swing.
                                  JTextField( "" +
                                              BIGInterface.getInstance().getSaveHeight() ) ;
                              String[] options = new String[2 ] ;
                              options[ 0 ] = "Okay" ;
                              options[ 1 ] = "Cancel" ;
                              int result = javax.swing.JOptionPane.
                                  showOptionDialog(
                                      chartPanel ,
                                      message ,
                                      "Please insert your data" ,
                                      heightAndWidth.DEFAULT_OPTION ,
                                      heightAndWidth.
                                      INFORMATION_MESSAGE , null ,
                                      options ,
                                      options[ 0 ] ) ;
                              // if cancel was pressed
                              if ( result == 1 )
                              {
                                 return ;
                              }
                              // okay was pressed
                              // get height
                              try
                              {
                                 tempHeight = ( new Integer( ( (
                                     JTextField ) message[
                                     1 ][ 1 ] ).
                                     getText() ) ).intValue() ;
                              }
                              // not parseable
                              catch ( NumberFormatException ex )
                              {
                                 threwException = true ;
                              }
                              // get width
                              try
                              {
                                 tempWidth = ( new Integer( ( (
                                     JTextField ) message[
                                     0 ][ 1 ] ).
                                     getText() ) ).intValue() ;
                              }
                              // not parseable
                              catch ( NumberFormatException ex )
                              {
                                 threwException = true ;
                              }
                              // should have the values, which are expected
                              if ( threwException )
                                 javax.swing.JOptionPane.
                                     showMessageDialog(
                                         chartPanel ,
                                         "Please select the image-size in integer value" ) ;
                           }
                           while ( threwException ) ;
                        // final height and width
                        final int height = tempHeight ;
                        final int width = tempWidth ;
                        // will be stored and used next time when saving
                        BIGInterface.getInstance().setSaveHeight( height ) ;
                        BIGInterface.getInstance().setSaveWidth( width ) ;
                        // writing jpg-file
                        if ( extension.equals( "jpg" ) )
                        {
                           ( new Thread()
                           {
                              public void run()
                              {
                                 // display in status label
                                 startFileExporting( nameOfFile + "." +
                                     extension , this ) ;
                                 boolean cme = false ;
                                 try
                                 {
                                    do
                                    {
                                       cme = false ;
                                       // save
                                       try
                                       {
                                          org.jfree.chart.ChartUtilities.
                                              saveChartAsJPEG( new
                                              File(
                                                  nameOfFile + "." +
                                                  extension ) ,
                                              chartPanel.getChart() ,
                                              width ,
                                              height ) ;
                                       }
                                       catch ( java.util.
                                               ConcurrentModificationException
                                               exc )
                                       {
                                          cme = true ;
                                       }

                                    }
                                    while ( cme ) ;
                                 }
                                 catch ( IOException ex1 )
                                 {
                                    System.err.println( ex1 ) ;
                                 }
                              }
                           } ).start() ;
                           return ;
                        }
                        // writing png-file
                        if ( extension.equals( "png" ) )
                        {
                           ( new Thread()
                           {
                              public void run()
                              {
                                 // display in status label
                                 startFileExporting( nameOfFile + "." +
                                     extension , this ) ;
                                 boolean cme = false ;
                                 try
                                 {
                                    do
                                    {
                                       cme = false ;
                                       // save
                                       try
                                       {

                                          org.jfree.chart.ChartUtilities.
                                              saveChartAsPNG( new
                                              File(
                                                  nameOfFile + "." +
                                                  extension ) ,
                                              chartPanel.getChart() ,
                                              width ,
                                              height ) ;
                                       }
                                       catch ( java.util.
                                               ConcurrentModificationException
                                               exc )
                                       {
                                          cme = true ;
                                       }

                                    }
                                    while ( cme ) ;
                                 }
                                 catch ( IOException ex1 )
                                 {
                                    System.err.println( ex1 ) ;
                                 }

                              }
                           } ).start() ;
                           return ;

                        } // writing eps-file (can be very large when using large numbers of shapes)
                        if ( extension.equals( "eps" ) )
                        {
                           ( new Thread()
                           {
                              public void run()
                              {
                                 // status label
                                 startFileExporting( nameOfFile +
                                     "." + extension , this ) ;

                                 boolean notDone = true ;
                                 while ( notDone )
                                 {
                                    try
                                    {
                                       // write to...
                                       FileOutputStream out = new
                                           FileOutputStream(
                                               nameOfFile + "." +
                                               extension ) ;
                                       // freehep export
                                       PSGraphics2D eg = new
                                           PSGraphics2D( out ,
                                           chartPanel.getSize() ) ;
                                       eg.writeHeader() ;

                                       chartPanel.paintComponent( eg ) ;
                                       eg.writeTrailer() ;
                                       eg.closeStream() ;
                                       out.close() ;
                                       notDone = false ;

                                    }
                                    catch ( Exception e )
                                    {
                                       // concurrent modification :/
                                       // can happen when painted to screen and file
                                       // at the same time
                                    }
                                 }
                              }
                           } ).start() ;
                           return ;
                        }
                        // writing pdf-file
                        if ( extension.equals( "pdf" ) )
                        {
                           ( new Thread()
                           {
                              public void run()
                              {
                                 // status label
                                 startFileExporting( nameOfFile +
                                     "." + extension , this ) ;
                                 boolean notDone = true ;
                                 while ( notDone )
                                 {
                                    try
                                    {
                                       // open file
                                       FileOutputStream out = new
                                           FileOutputStream(
                                               nameOfFile + "." +
                                               extension ) ;
                                       // freehep export
                                       PDFGraphics2D eg = new
                                           PDFGraphics2D( out ,
                                           chartPanel.getSize() ) ;
                                       eg.writeHeader() ;

                                       eg.writeTrailer() ;
                                       eg.closeStream() ;
                                       out.close() ;

                                       chartPanel.paintComponent( eg ) ;
                                       notDone = false ;
                                    }
                                    catch ( Exception e )
                                    {
                                       e.printStackTrace() ;
                                    }
                                 }
                              }
                           } ).start() ;
                           return ;
                        }

                        // writing ppm-file
                        if ( extension.equals( "ppm" ) )
                        {
                           ( new Thread()
                           {
                              public void run()
                              {

                                 startFileExporting( nameOfFile +
                                     "." + extension , this ) ;
                                 boolean notDone = true ;
                                 while ( notDone )
                                 {
                                    try
                                    {
                                       FileOutputStream out = new
                                           FileOutputStream(
                                               nameOfFile + "." +
                                               extension ) ;
                                       BufferedImage i = new
                                           BufferedImage( ( int )
                                           chartPanel.getSize().
                                           getWidth() ,
                                           ( int ) chartPanel.
                                           getSize().getHeight() ,
                                           BufferedImage.
                                           TYPE_3BYTE_BGR ) ;

                                       chartPanel.paintComponent( i.
                                           createGraphics() ) ;
                                       PPMEncoder pme = new PPMEncoder(
                                           i , out ) ;
                                       pme.encode() ;
                                       out.close() ;

                                       notDone = false ;
                                    }
                                    catch ( Exception e )
                                    {

                                    }
                                 }
                              }
                           } ).start() ;
                           return ;
                        }

                        // writing emf-file
                        if ( extension.equals( "emf" ) )
                        {
                           ( new Thread()
                           {
                              public void run()
                              {

                                 try
                                 {

                                    FileOutputStream out = new
                                        FileOutputStream(
                                            nameOfFile + "." +
                                            extension ) ;
                                    ByteArrayOutputStream bas = new
                                        ByteArrayOutputStream() ;
                                    startFileExporting( nameOfFile +
                                        "." + extension , this ) ;
                                    org.freehep.graphicsio.emf.
                                        EMFGraphics2D gra = new
                                        org.freehep.graphicsio.
                                        emf.EMFGraphics2D( bas ,
                                        chartPanel )
                                    {
                                       public void writeBackground()
                                       {}

                                    } ;
                                    gra.startExport() ;

                                    boolean notDone = true ;
                                    while ( notDone )
                                    {
                                       try
                                       {
                                          chartPanel.paintComponent( gra ) ;
                                          notDone = false ;
                                       }
                                       catch ( Exception e )
                                       {

                                       }
                                    }
                                    gra.endExport() ;
                                    bas.writeTo( out ) ;
                                    gra.closeStream() ;
                                    bas.close() ;
                                    out.close() ;

                                 }
                                 catch ( IOException ex1 )
                                 {
                                    System.err.println( ex1 ) ;
                                 }
                              }
                           } ).start() ;
                           return ;
                        }

                     }

                     public void cancelSelection()
                     {
                        f.setVisible( false ) ;
                     }
                  } ;
                  // we want to save
                  fch.setDialogType( fch.SAVE_DIALOG ) ;
                  // the satndardname is the title
                  String standardFile = fch.getCurrentDirectory().
                      getAbsolutePath() +
                      java.io.File.separator +
                      chartPanel.getChart().getTitle().getText() ;
                  fch.setSelectedFile( new java.io.File( standardFile ) ) ;
                  // the filter just accepts csv,jpg,png,emf,pdf,ppm

                  fch.addChoosableFileFilter( new javax.swing.filechooser.
                                              FileFilter()
                  {
                     public boolean accept( java.io.File f )
                     {
                        if ( f.isDirectory() )
                           return true ;
                        String extension = ( f.getName() ).substring( ( f.
                            getName() ).lastIndexOf( '.' ) + 1 ) ;
                        if ( f != null )
                        {
                           if ( extension.equals( "csv" ) )
                              return true ;
                        }
                        return false ;
                     }

                     //The description of this filter
                     public String getDescription()
                     {
                        return "*.csv" ;
                     }

                  } ) ;
                  fch.addChoosableFileFilter( new javax.swing.filechooser.
                                              FileFilter()
                  {
                     public boolean accept( java.io.File f )
                     {
                        if ( f.isDirectory() )
                           return true ;
                        String extension = ( f.getName() ).substring( ( f.
                            getName() ).lastIndexOf( '.' ) + 1 ) ;
                        if ( f != null )
                        {
                           if ( extension.equals( "jpg" ) )
                              return true ;
                        }
                        return false ;
                     }

                     //The description of this filter
                     public String getDescription()
                     {
                        return "*.jpg" ;
                     }

                  } ) ;
                  fch.addChoosableFileFilter( new javax.swing.filechooser.
                                              FileFilter()
                  {
                     public boolean accept( java.io.File f )
                     {
                        if ( f.isDirectory() )
                           return true ;
                        String extension = ( f.getName() ).substring( ( f.
                            getName() ).lastIndexOf( '.' ) + 1 ) ;
                        if ( f != null )
                        {
                           if ( extension.equals( "png" ) )
                              return true ;
                        }
                        return false ;
                     }

                     //The description of this filter
                     public String getDescription()
                     {
                        return "*.png" ;
                     }

                  } ) ;
                  fch.addChoosableFileFilter( new javax.swing.filechooser.
                                              FileFilter()
                  {
                     public boolean accept( java.io.File f )
                     {
                        if ( f.isDirectory() )
                           return true ;
                        String extension = ( f.getName() ).substring( ( f.
                            getName() ).lastIndexOf( '.' ) + 1 ) ;
                        if ( f != null )
                        {
                           if ( extension.equals( "eps" ) )
                              return true ;
                        }
                        return false ;
                     }

                     //The description of this filter
                     public String getDescription()
                     {
                        return "*.eps" ;
                     }

                  } ) ;
                  fch.addChoosableFileFilter( new javax.swing.filechooser.
                                              FileFilter()
                  {
                     public boolean accept( java.io.File f )
                     {
                        if ( f.isDirectory() )
                           return true ;
                        String extension = ( f.getName() ).substring( ( f.
                            getName() ).lastIndexOf( '.' ) + 1 ) ;
                        if ( f != null )
                        {
                           if ( extension.equals( "pdf" ) )
                              return true ;
                        }
                        return false ;
                     }

                     //The description of this filter
                     public String getDescription()
                     {
                        return "*.pdf" ;
                     }

                  } ) ;
                  fch.addChoosableFileFilter( new javax.swing.filechooser.
                                              FileFilter()
                  {
                     public boolean accept( java.io.File f )
                     {
                        if ( f.isDirectory() )
                           return true ;
                        String extension = ( f.getName() ).substring( ( f.
                            getName() ).lastIndexOf( '.' ) + 1 ) ;
                        if ( f != null )
                        {
                           if ( extension.equals( "ppm" ) )
                              return true ;
                        }
                        return false ;
                     }

                     //The description of this filter
                     public String getDescription()
                     {
                        return "*.ppm" ;
                     }

                  } ) ;
                  fch.addChoosableFileFilter( new javax.swing.filechooser.
                                              FileFilter()
                  {
                     public boolean accept( java.io.File f )
                     {
                        if ( f.isDirectory() )
                           return true ;
                        String extension = ( f.getName() ).substring( ( f.
                            getName() ).lastIndexOf( '.' ) + 1 ) ;
                        if ( f != null )
                        {
                           if ( extension.equals( "emf" ) )
                              return true ;
                        }
                        return false ;
                     }

                     //The description of this filter
                     public String getDescription()
                     {
                        return "*.emf" ;
                     }

                  } ) ;
                  // set default directory
                  fch.setCurrentDirectory( BIGInterface.getInstance().
                                           getLastWorkPath() ) ;
                  if ( debug ) System.err.println( BIGInterface.getInstance().
                      getLastWorkPath() ) ;

                  fch.setAcceptAllFileFilterUsed( false ) ;
                  // default
                  fch.setFileFilter( fch.getChoosableFileFilters()[ 2 ] ) ;
                  // add file chooser
                  f.getContentPane().add( fch , BorderLayout.CENTER ) ;
                  f.setVisible( true ) ;
               }
            } ).start() ;

         }
      } ) ;

      // END overriding "Save as..."
      // detache means open a frame with this plot as content
      JMenuItem detach = new JMenuItem( "Detach" ) ;
      detach.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            // new frame
            JFrame jf = new JFrame() ;
            // new plot
            plotable.save( plotable.getSaveFile() ) ;
            BIGPlot bp = new BIGPlot( plotable ) ;
            jf.getContentPane().add( bp.getDisplayPanels()[ 0 ] ,
                                     BorderLayout.CENTER ) ;
            jf.setSize( 512 , 384 ) ;
            // show
            jf.setVisible( true ) ;
         }
      } ) ;
      chartPanel.getPopupMenu().add( detach ) ;

   }

   /**
    * repaints the Status Label with the actual filesize until the thread is dead
    * @param filename String filename, which is written
    * @param t Thread thread, wich writes the file
    */
   private void startFileExporting( final String filename , final Thread t )
   {
      // get the file
      final File f = new File( filename ) ;
      // get the label
      final JLabel jl = BIGInterface.getInstance().getStatusLabel() ;
      ( new Thread()
      {
         public void run()
         {
            // while it is exporting
            while ( t.isAlive() )
            {
               // write the actual size to the label
               jl.setText( "Exporting " + filename + ":" +
                           ( int ) ( f.length() / 1024 ) + "kB" ) ;
               jl.validate() ;
               try
               {
                  this.sleep( 10 ) ;
               }
               catch ( InterruptedException ex )
               {
               }
            }
            jl.setText( "Done" ) ;
         }
      } ).start() ;
   }

   /**
    * gets the default sequence of colors. they are stored in cfg/stdColors.cfg
    * @return Paint[] the colors
    */
   public static Paint[] getDefaultPaintSequece()
   {
      // standard colors with 35 possible entries
      Paint[] p = new Paint[35 ] ;
      if ( ! ( new File( BIGInterface.getInstance().
                         getBenchItPath() +
                         File.separator + "gui" +
                         File.separator + "cfg" +
                         File.separator + "stdColors.cfg" ) ).exists() )
      {
         setDefaultPaintSequece( ( new org.jfree.chart.renderer.
                                   DefaultDrawingSupplier() ).
                                 DEFAULT_PAINT_SEQUENCE ) ;
      }
      // read file content to string
      String s = BIGFileHelper.getFileContent( new File( BIGInterface.
          getInstance().
          getBenchItPath() +
          File.separator + "gui" +
          File.separator + "cfg" +
          File.separator + "stdColors.cfg" ) ) ;
      // separate by \ns
      StringTokenizer st = new StringTokenizer( s , "\n" ) ;
      // build colors from ints
      for ( int i = 0 ; i < p.length ; i++ )
         p[ i ] = new java.awt.Color( Integer.parseInt( st.nextToken() ) ) ;
      // return
      return p ;
   }

   /**
    * saves the default sequence of colors to a file (cfg/stdColors.cfg)
    * @param p Paint[] the colors to save
    */
   public static void setDefaultPaintSequece( Paint[] p )
   {
      // creating filecontent
      StringBuffer sb = new StringBuffer() ;
      for ( int i = 0 ; i < p.length ; i++ )
      {
         // adding rgb-value and a line-break
         sb.append( ( ( Color ) ( p[ i ] ) ).getRGB() ) ;
         sb.append( '\n' ) ;
      }
      // deleting last linebreak
      sb.deleteCharAt( sb.length() - 1 ) ;
      // saving file
      BIGFileHelper.saveToFile( sb.toString() ,
                                new File( BIGInterface.getInstance().
                                          getBenchItPath() +
                                          File.separator + "gui" +
                                          File.separator + "cfg" +
                                          File.separator + "stdColors.cfg" )
          ) ;
   }

   /**
    *
    * <p>Überschrift: BenchIT-Renderer for Shapes</p>
    *
    * <p>Beschreibung: Uses setted Colors to draw the Shapes </p>
    *
    * <p>Copyright: Copyright (c) 2004</p>
    *
    * <p>Organisation: ZHR TU Dresden</p>
    *
    * @author Robert Schoene
    * @version 1.0
    */
   /*public class BIGPlotRenderer
       extends org.jfree.chart.renderer.StandardXYItemRenderer
       {
      // setted colors
      private Paint[] colors = null ;
      /**
     * Constructor
     * @param colors Paint[] setted Color sequence
     * @param plotShapes boolean whether to plot shapes (should be true)
     * @param plotLines boolean whether to draw lines between shapes
     * @param fillShapes boolean whether to fill the shapes
     */
    /*public BIGPlotRenderer(Paint[] colors, boolean plotShapes ,
                                 boolean plotLines , boolean fillShapes )
           {
       super();
       // set colors
       this.colors=colors;
       //calls super.*();
       this.setPlotShapes(plotShapes);
       this.setPlotLines(plotLines);
       this.setDefaultShapeFilled(fillShapes);
           }
           public Paint getItemPaint( int i , int j , int k )
           {
     return this.getSeriesPaint(i,j);
           }
          public Paint getSeriesPaint(int i, int j)
          {
      if ( colors == null )
          colors=new Color[j+1];
          if ( colors.length > j )
          {
             if ( colors[ j ] != null )
             {
     if (((Color)colors[j]).getRGB()!=((Color)super.getSeriesPaint(i,j)).getRGB())
                   setSeriesPaint(j,new Color(((Color)colors[j]).getRGB()));
                return colors[ j ] ;
             }
          }
          else
          {
             Paint[] newColors = new Paint[j + 1 ] ;
             for ( int l = 0 ; l < this.colors.length ; l++ )
             {
                newColors[ l ] = this.colors[ l ] ;
             }
             for ( int l = colors.length ; l <= j ; l++ )
             {
                newColors[ l ] = super.getSeriesPaint( i , l ) ;
             }
             this.colors = newColors ;


          }
       colors[j]=super.getSeriesPaint( i , j  );
       return  colors[j];
          }

          public void setSeriesPaint(int i, int j, Paint color)
          {
      colors[j]=color;
       super.setSeriesPaint( i,j , color ) ;
          }
           public void setSeriesPaint( int j , Paint
                                color )
           {
       colors[ j ] = color ;
       super.setSeriesPaint( j , color ) ;
           }

        }*/

    /**
     *
     * <p>Überschrift: BenchIT</p>
     *
     * <p>Beschreibung: Maybe someday, you can select shapes with this</p>
     *
     * <p>Copyright: Copyright (c) 2004</p>
     *
     * <p>Organisation: ZHR TU Dresden</p>
     *
     * @author Robert Schoene
     * @version 1.0
     */
    class ShapeComboBox
        extends JComboBox
    {
       //------------------------------------------------------------------------------------
       //Shape[] s=org.jfree.chart.renderer.DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE;
       Shape[] s = null ;
       //public ShapeComboBox()
       public ShapeComboBox( Shape[] shapes )
       {
          s = shapes ;
          //------------------------------------------------------------------------------------
          for ( int i = 0 ; i < s.length ; i++ )
          {
             this.addItem( i + ":" ) ;
          }
          this.setSelectedIndex( 0 ) ;
          this.setRenderer( new ListCellRenderer()
          {
             public Component getListCellRendererComponent(
                 final JList list ,
                 Object value ,
                 final int index ,
                 final boolean isSelected ,
                 final boolean cellHasFocus )
             {
                JLabel lab = new JLabel() ;
                if ( index >= 0 )
                   lab = new JLabel( s[ index ].toString() )
                   {
                      public void paintComponent( Graphics g )
                      {
                         int realindex = index ;
                         g.translate( 10 , 10 ) ;
                         ( ( Graphics2D ) g ).scale( 2 , 2 ) ;
                         if ( index == -1 )
                         {
                            realindex = 0 ;
                         }
                         if ( realindex >= 0 )
                         {
                            ( ( Graphics2D ) g ).setBackground( isSelected ?
                                Color.red : Color.white ) ;
                            ( ( Graphics2D ) g ).setColor( isSelected ?
                                Color.white :
                                Color.black ) ;
                            ( ( Graphics2D ) g ).draw( s[ realindex ] ) ;
                            // ( ( Graphics2D ) g ).fill( s[ realindex ] ) ;
                         }
                      }
                   } ;
                // width ,height
                lab.setPreferredSize( new Dimension( 20 , 20 ) ) ;
                return lab ;
             }

          } ) ;
       }

       public void paint( Graphics g )
       {
          super.paint( g ) ;
          g.translate( 10 , 10 ) ;
          ( ( Graphics2D ) g ).scale( 2 , 2 ) ;
          ( ( Graphics2D ) g ).draw( s[ getSelectedIndex() ] ) ;
          // ( ( Graphics2D ) g ).fill( s[ getSelectedIndex() ] ) ;

       }

       //-----------------------------------------------------------------------------------
       /**
        * Returns the standard shape sequence
        * @return - the shape sequence
        */
       public Shape[] getShapeSequence()
       {
          return s ;
       }

       /**
        * Returns the shape at the given index
        * @param index - the position of the requested shape
        * @return - the shape at the given position
        */
       public Shape getShape( int index )
       {
          return s[ index ] ;
       }
       //-----------------------------------------------------------------------------------
    }

}

/*****************************************************************************
 Log-History

 */
