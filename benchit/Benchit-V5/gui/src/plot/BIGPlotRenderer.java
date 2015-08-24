package plot;

import java.awt.Color;
import java.awt.Paint;
import java.awt.Shape;

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
   public class BIGPlotRenderer
       extends org.jfree.chart.renderer.StandardXYItemRenderer
   {
      // setted colors
      private Paint[] colors = null ;
      
      // array of shapes that will be used
      public static final Shape[] BIG_SHAPES = org.jfree.chart.renderer.DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE;
      
      // setted shapes
      private Shape[] settedShapes = null;
      
      /*
      /**
       * Constructor
       * @param colors Paint[] setted Color sequence
       * @param plotShapes boolean whether to plot shapes (should be true)
       * @param plotLines boolean whether to draw lines between shapes
       * @param fillShapes boolean whether to fill the shapes
       */
      /*
      public BIGPlotRenderer(Paint[] colors, boolean plotShapes ,
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
      */
      
      //-------------------------------------------------------------------------------
      /**
       * Constructor
       * @param colors Paint[] setted Color sequence
       * @param shapes Shape[] setted Shape sequence
       * @param plotShapes boolean whether to plot shapes (should be true)
       * @param plotLines boolean whether to draw lines between shapes
       * @param fillShapes boolean whether to fill the shapes
       */
      public BIGPlotRenderer(Paint[] colors, Shape[] settedShapes, boolean plotShapes ,
                                   boolean plotLines , boolean fillShapes )
      {
         super();
         // set colors
         this.colors=colors;
         //set shapes
         this.settedShapes = settedShapes;
         //calls super.*();
         this.setPlotShapes(plotShapes);
         this.setPlotLines(plotLines);
         this.setDefaultShapeFilled(fillShapes);
      }
      //------------------------------------------------------------------------------
      
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
      
      //--------------------------------------------------------------------------------------------
      public void setSeriesShape(int j , int index)
      {
    	  Shape actualShape = BIG_SHAPES[ index ];
    	  this.settedShapes[ j ] = actualShape;
    	  super.setSeriesShape(j, actualShape);
      }
      
      /**
       * BIG-Implementation for setting the shapes of a selected series 
       * @param j int - the series to which the shape is setted
       * @param shape Shape - the shape that is setted  
       */
      public void setSeriesShape(int j, Shape shape)
      {
    	  this.settedShapes[ j ] = shape;    	  
    	  super.setSeriesShape(j, shape);
      }
      
      /**
       * BIG-Implementation for setting the shapes of a selected series 
       * @param i int - ???
       * @param j int - ???
       * @param shape Shape - the shape that will be setted  
       */
      public void setSeriesShape(int i, int j, Shape shape)
      {
    	  this.settedShapes[ j ] = shape; 
    	  super.setSeriesShape(i, j, shape);
      }      
      
      /**
       * Returns the shape of the given series
       * @param i int - ???
       * @param j int - ???
       * @return the shape of the given series
       */
      public Shape getSeriesShape(int i, int j)
      {
    	  // System.out.println("Test1: " + i + " Test2: " + j + "\n");
    	  if (settedShapes == null)
    	  {
    		  settedShapes = new Shape[j + 1];
    	  }
    	  if (settedShapes.length > j)
    	  {
    		  if (settedShapes[j] != null)
    		  {
                  if ( ! settedShapes[j].equals(super.getSeriesShape(i, j) ) )
                      setSeriesShape(i, j, settedShapes[j] );
                  return settedShapes[ j ] ;
    		  }
    	  }
    	  else
          {
              Shape[] newShapes = new Shape[ j + 1 ] ;
              for ( int l = 0 ; l < this.settedShapes.length ; l++ )
              {
                 newShapes[ l ] = this.settedShapes[ l ] ;
              }
              for ( int l = settedShapes.length ; l <= j ; l++ )
              {
                 newShapes[ l ] = super.getSeriesShape( i , l ) ;
              }
              this.settedShapes = newShapes ;
          }
          settedShapes[ j ] = super.getSeriesShape( i , j  );
          return  settedShapes[ j ];
      }
      
      /**
       * @param i int
       * @param j int
       * @param k int
       * @return the Shape at the selected position
       */
      public Shape getItemShape(int i, int j, int k)
      {
    	  return super.getItemShape(i, j, k);
      }
      //--------------------------------------------------------------------------------------------      
   }