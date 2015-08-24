 package plot;


import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.Rectangle2D;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;

    /**
     *
     * <p>Überschrift: BenchIT</p>
     *
     * <p>Beschreibung: </p>
     * Implements cool zoom (Mousewheel) and move (Mouse-drag) features
     * when holding shift
     * <p>Copyright: Copyright (c) 2007</p>
     *
     * <p>Organisation: ZIH TU Dresden</p>
     *
     * @author Robert Schoene
     * @version 1.0
     */
   public class PlotListen
       implements MouseListener , MouseMotionListener , MouseWheelListener
   {
      private JFreeChart chart ;
      private ChartPanel pan ;
      /**
       * create a new Mouse(Motion/Wheel)Listener
       * @param chart JFreeChart the chart for the cool new zoom and move features
       * @param pan ChartPanel the charts panel
       */
      public PlotListen( JFreeChart chart , ChartPanel pan )
      {
         this.chart = chart ;
         this.pan = pan ;
      }


      /**
       * if drag and drop: do change min and max
       * @param evt MouseEvent
       */
      public void mouseDragged( MouseEvent evt )
      {
         if ( evt.isShiftDown() )
         {
            actualizePlot( evt );
         }
         else
         {

            pan.setVerticalZoom( true ) ;
            pan.setHorizontalZoom( true ) ;

         }
            evt.consume() ;

      }

      double x = -1 ;
      double y = -1 ;
      /**
       * if started drag and drop: save position
       * @param evt MouseEvent
       */
      public void mousePressed( MouseEvent evt )
      {
         if ( evt.isShiftDown() )
         {
            pan.setVerticalZoom( false ) ;
            pan.setHorizontalZoom( false ) ;

            if ( x < 0 && y < 0 )
            {
               x = chart.getXYPlot().getHorizontalValueAxis().
                   translateJava2DtoValue( evt.getX() ,
                                           pan.
                                           getChartRenderingInfo().
                                           getDataArea() ) ;

               y = chart.getXYPlot().getVerticalValueAxis().
                   translateJava2DtoValue( evt.getY() ,
                                           pan.
                                           getChartRenderingInfo().
                                           getDataArea() ) ;
            }
            else
            {
            }
            evt.consume() ;
         }
         else
         {

            pan.setVerticalZoom( true ) ;
            pan.setHorizontalZoom( true ) ;
            evt.consume();
         }

      }

      /**
       * if ended drag and drop: do change min and max a last time
       * @param evt MouseEvent
       */
      public void mouseReleased( MouseEvent evt )
      {
         if ( evt.isShiftDown() && ( x >= 0 ) && ( y >= 0 ) )
         {
        	 actualizePlot( evt );
         }
      }

      public void mouseMoved( MouseEvent e )
      {
      }

      public void mouseClicked( MouseEvent e )
      {
      }

      public void mouseEntered( MouseEvent e )
      {
      }

      public void mouseExited( MouseEvent e )
      {
      }
      /**
       * if mousewheel: do zoom at this position
       * @param e MouseWheelEvent
       */
      public void mouseWheelMoved( MouseWheelEvent e )
      {
         if ( e.isShiftDown() )
         {
            double i = 1.0 * e.getScrollAmount() * e.getWheelRotation() ;
            double xsize = ( chart.getXYPlot().getDomainAxis().
                             getMaximumAxisValue()
                             -
                             chart.getXYPlot().getDomainAxis().getMinimumAxisValue() ) *
                ( 1.0 + 0.05 * i ) ;
            double ysize = ( chart.getXYPlot().getRangeAxis().
                             getMaximumAxisValue()
                             -
                             chart.getXYPlot().getRangeAxis().getMinimumAxisValue() ) *
                ( 1.0 + 0.05 * i ) ;
            double xMid = chart.getXYPlot().getHorizontalValueAxis().
                translateJava2DtoValue( e.getX() ,
                                        pan.getChartRenderingInfo().
                                        getDataArea() ) ;
            double yMid = chart.getXYPlot().getVerticalValueAxis().
                translateJava2DtoValue( e.getY() ,
                                        pan.getChartRenderingInfo().
                                        getDataArea() ) ;
            double toLeft = ( xMid -
                              chart.getXYPlot().getDomainAxis().getMinimumAxisValue() )
                / ( chart.getXYPlot().getDomainAxis().getMaximumAxisValue()
                    - chart.getXYPlot().getDomainAxis().getMinimumAxisValue() ) ;

            double toDown = ( yMid -
                              chart.getXYPlot().getRangeAxis().getMinimumAxisValue() )
                / ( chart.getXYPlot().getRangeAxis().getMaximumAxisValue()
                    - chart.getXYPlot().getRangeAxis().getMinimumAxisValue() ) ;

            double xmin = xMid - xsize * toLeft ;
            if ( xmin < 0 )
               xmin = 0 ;
            double ymin = yMid - ysize * toDown ;
            if ( ymin < 0 )
               ymin = 0 ;
            chart.getXYPlot().getDomainAxis().setMinimumAxisValue( 0 ) ;
            chart.getXYPlot().getDomainAxis().setMaximumAxisValue( xmin + xsize ) ;
            chart.getXYPlot().getDomainAxis().setMinimumAxisValue( xmin ) ;

            chart.getXYPlot().getRangeAxis().setMinimumAxisValue( 0 ) ;
            chart.getXYPlot().getRangeAxis().setMaximumAxisValue( ymin + ysize ) ;
            chart.getXYPlot().getRangeAxis().setMinimumAxisValue( ymin ) ;
         }
      }
      
      private void actualizePlot(MouseEvent evt) {
    	  pan.setVerticalZoom( false ) ;
          pan.setHorizontalZoom( false ) ;
          /*double yshift = chart.getXYPlot().getVerticalValueAxis().
              translateJava2DtoValue( evt.getY() ,
                                      pan.getChartRenderingInfo().
                                      getDataArea() ) - y ;*/
          double yshift = computeYShift( evt.getY(),
          					pan.getChartRenderingInfo().getDataArea(),
          					y );
          
          y = chart.getXYPlot().getVerticalValueAxis().
              translateJava2DtoValue( evt.getY() ,
                                      pan.getChartRenderingInfo().
                                      getDataArea() ) ;

          /*double xshift = chart.getXYPlot().getHorizontalValueAxis().
              translateJava2DtoValue( evt.getX() ,
                                      pan.getChartRenderingInfo().
                                      getDataArea() ) - x ;*/
          double xshift = computeXShift( evt.getX(),
          					pan.getChartRenderingInfo().getDataArea(),
          					x );

          x = chart.getXYPlot().getHorizontalValueAxis().
              translateJava2DtoValue( evt.getX() ,
                                      pan.getChartRenderingInfo().
                                      getDataArea() ) ;
          
          double min = chart.getXYPlot().getDomainAxis().getMinimumAxisValue() ;
          if ( min + xshift < 0 )
          {
             xshift = 0-min ;
          }
          chart.getXYPlot().getDomainAxis().setMinimumAxisValue( 0 ) ;
          chart.getXYPlot().getDomainAxis().setMaximumAxisValue(
              chart.getXYPlot().getDomainAxis().getMaximumAxisValue() +
              xshift ) ;
          chart.getXYPlot().getDomainAxis().setMinimumAxisValue(
              min + xshift ) ;
          
          min = chart.getXYPlot().getRangeAxis().getMinimumAxisValue() ;
          if ( min + yshift < 0 )
          {
             yshift = 0-min ;
          }
          chart.getXYPlot().getRangeAxis().setMinimumAxisValue( 0 ) ;
          chart.getXYPlot().getRangeAxis().setMaximumAxisValue(
              chart.getXYPlot().getRangeAxis().getMaximumAxisValue() + 
              yshift ) ;
          chart.getXYPlot().getRangeAxis().setMinimumAxisValue(
              min + yshift ) ;
      }
      
      private double computeXShift(float f, Rectangle2D rect, double xValue) {
    	  /*System.err.println("(XSHIFT): " + 
    			  ( chart.getXYPlot().getHorizontalValueAxis().
	  				translateJava2DtoValue( f , rect ) - xValue)
    			  );*/
    	  double shift = chart.getXYPlot().getHorizontalValueAxis().
								translateJava2DtoValue( f , rect ) - xValue ;
    	  /*double shift = xValue - chart.getXYPlot().getHorizontalValueAxis().
			translateJava2DtoValue( f , rect );*/
    	  // System.err.println("XSHIFT: " + shift );
    	  /*return chart.getXYPlot().getHorizontalValueAxis().
    	  				translateJava2DtoValue( f , rect ) - xValue ;*/
    	  return shift;
      }
      
      private double computeYShift(float f, Rectangle2D rect, double yValue) {
    	  /*System.err.println("(YSHIFT): " +
    			  ( chart.getXYPlot().getVerticalValueAxis().
	  				translateJava2DtoValue( f , rect ) - yValue )
    			  );*/
    	  double shift = chart.getXYPlot().getVerticalValueAxis().
								translateJava2DtoValue( f , rect ) - yValue ;
    	  /*double shift = yValue - chart.getXYPlot().getVerticalValueAxis().
			translateJava2DtoValue( f , rect );*/
    	  // System.err.println("YSHIFT: " + shift );
    	  /*return chart.getXYPlot().getVerticalValueAxis().
						translateJava2DtoValue( f , rect ) - yValue ;*/
    	  return shift;
      }
   }
