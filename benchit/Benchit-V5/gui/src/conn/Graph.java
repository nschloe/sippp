/**
 * Title:Graph
 * Description:	contains all informations for one Graph
 * Copyright:    Copyright (c) 2003
 * Company:ZHR (Center for High Performance Computing)
 * @author: Robert Schoene (rschoene@zhr.tu-dresden.de)
 * @version 1.0																*
 */
package conn ;

import java.awt.datatransfer.*;

public class Graph implements Transferable
{
   /** id. should be unique ;) */
   int graphID ;
   /** name (should also be unique) */
   String graphName = new String() ;
   /** name for horizontal axis*/
   String xAxisName = new String() ;
   /** name for vertical axis */
   String yAxisName = new String() ;
   /** x-Values */
   double[] xWerte ;
   /** y-Values */
   double[] yWerte ;
   /** file-content in lines */
   String[] fileContent ;
   // settings[0][x] are the identifiers , // settings[1][x] are the settings to the identifier
   /** settings read from the server */
   String[][] settings ;

   /**
    * Costructor
    */
   public Graph()
   {
   }

   /**
    * Sets the points of the function
    * @param xs x-values
    * @param ys y-values
    */
   public void setPoints( double[] xs , double[] ys )
   {
      this.xWerte = xs ;
      this.yWerte = ys ;
   }

   /**
    * sets the number of the identifiers of the function
    * also removes all previously setted identifiers with there settings
    * @param i the new number of identifiers
    */
   public void setNumberOfIdentifiers( int i )
   {
      settings = new String[2 ][ i ] ;
   }

   /**
    * gets the identifier at position i
    * @param i the position
    * @return the identifier at position
    */
   public String getIdentifier( int i )
   {
      return this.settings[ 0 ][ i ] ;
   }

   /**
    * gets the setting for an identifier
    * @param identifier the identifier
    * @return the setting for the identifier
    */
   public String getSetting( String identifier )
   {
      for ( int i = 0 ; i < this.settings[ 0 ].length ; i++ )
      {
         if ( this.settings[ 0 ][ i ].equals( identifier ) )
         {
            return this.settings[ 1 ][ i ] ;
         }
      }
      return "" ;
   }

   /**
    * sets the setting for an identifier
    * @param identifier the identifier
    * @param setting the setting for the identifier
    */
   public void setIdentifierSettings( String identifier , String setting )
   {
      if ( setting == null ) setting = "" ;
      int free = -1 ;
      for ( int i = 0 ; i < settings[ 0 ].length ; i++ )
      {
         if ( settings[ 0 ][ i ] == null )
         {
            free = i ;
            break ;
         }
         if ( settings[ 0 ][ i ].equals( identifier ) )
         {
            settings[ 1 ][ i ] = setting ;
            return ;
         }
      }
      //if settings is full
      if ( free == -1 )
      {
         String[][] newSettings = new String[2 ][ this.settings.length + 1 ] ;
         for ( int i = 0 ; i < this.settings[ 0 ].length ; i++ )
         {
            newSettings[ 0 ][ i ] = this.settings[ 0 ][ i ] ;
            newSettings[ 1 ][ i ] = this.settings[ 1 ][ i ] ;
         }
         newSettings[ 0 ][ this.settings.length ] = identifier ;
         newSettings[ 1 ][ this.settings.length ] = setting ;
         this.settings = newSettings ;
      }
      else //or not
      {
         this.settings[ 0 ][ free ] = identifier ;
         this.settings[ 1 ][ free ] = setting ;
      }

   }

   /**
    * gets the identifiers
    * @return the identifiers
    */
   public String[] getIdentifiers()
   {
      return this.settings[ 0 ] ;
   }

   /**
    * gets the settings
    * @return the settings
    */
   public String[] getSettings()
   {
      return this.settings[ 1 ] ;
   }

   /**
    * sets the fileContent for this Graph
    * @param f the file (in lines)
    */
   public void setFileContent( String[] f )
   {
      this.fileContent = f ;
   }

   /**
    * gets the file as lines
    * @return the file (separated in lines)
    */
   public String[] getFileContent()
   {
      if ( this.fileContent != null )
         return this.fileContent ;
      return new String[0 ] ;
   }

   /**
    * sets the identifiers and settings
    * @param iAnds iAnds[0]...identifiers,iAnds[1]...settings
    */
   public void setIdentifiersAndSettings( String[][] iAnds )
   {
      this.settings = iAnds ;
   }

   /**
    * sets the points for the function
    * @param d d[0]..x-Values,d[1]...y-Values
    */
   public void setPoints( double[][] d )
   {
      this.xWerte = d[ 0 ] ;
      this.yWerte = d[ 1 ] ;
   }

   /**
    * sets the xaxis-text
    * @param s the new xaxis-text
    */
   public void setXAxisText( String s )
   {
      this.xAxisName = s ;
   }

   /**
    * sets the yaxis-text
    * @param s the new yaxis-text
    */
   public void setYAxisText( String s )
   {
      this.yAxisName = s ;
   }

   /**
    *
    * @return the xaxis-text of this
    */
   public String getXAxisText()
   {
      return this.xAxisName ;
   }

   /**
    *
    * @return the yaxis-text of this
    */
   public String getYAxisText()
   {
      return this.yAxisName ;
   }

   /**
    * Sets the name of this (is shown in the functionList)
    * @param name the new name of this
    */
   public void setGraphName( String name )
   {
      this.graphName = name ;
   }

   /**
    *
    * @return this graphs name
    */
   public String getGraphName()
   {
      return this.graphName ;
   }

   /**
    *
    * @return the number of points of this graph
    */
   public int getNumberOfPoints()
   {
      return this.yWerte.length ;
   }

   /**
    *
    * @return the x-Values of this
    */
   public double[] getXValues()
   {
      return this.xWerte ;
   }

   /**
    *
    * @return the y-Values of this
    */
   public double[] getYValues()
   {
      return this.yWerte ;
   }

   /**
    * sets the x-Values
    * @param d the new x-Values
    */
   public void setXValues( double[] d )
   {
      this.xWerte = d ;
   }

   /**
    * sets the y-Values
    * @param d the new y-Values
    */
   public void setYValues( double[] d )
   {
      this.yWerte = d ;
   }

   /**
    *
    * @param xValue the value to get the yValue from
    * @return the yValue to xValue
    */
   public double getYValue( double xValue )
   {
      for ( int i = 0 ; i < this.xWerte.length ; i++ )
         if ( xValue == xWerte[ i ] )
            return yWerte[ i ] ;
      return -1E20 ;
   }

   public String toString()
   {
      return this.graphName ;
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
       throws UnsupportedFlavorException , java.io.IOException
   {
      if (!flavor.equals(this.getDataFlavorObject()))
         return null ;
      return this;
   }
   public DataFlavor getDataFlavorObject()
   {
      return new DataFlavor(Graph.class,"A single result line "+graphName);
   }
   
   //--------
   public Graph getCopyOfGraph()
   {
	   Graph newGraph = new Graph();
	   newGraph.setFileContent( this.fileContent );
	   newGraph.setGraphName( this.graphName );
	   newGraph.setPoints( this.xWerte, this.yWerte );
	   newGraph.setIdentifiersAndSettings( this.settings );
	   newGraph.setXAxisText( this.xAxisName );
	   newGraph.setYAxisText( this.yAxisName );
	   //etc...
	   return newGraph;
   }
   //--------

}
