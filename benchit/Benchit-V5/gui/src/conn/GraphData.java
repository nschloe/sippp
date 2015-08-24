package conn ;

import java.util.* ;
import java.io.* ;

/**
 * <p>Überschrift: BenchIT</p>
 * <p>Beschreibung: Contains the Graphs</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR TU Dresden</p>
 * @author Robert Schoene
 * @version 1.0
 */
public class GraphData
    extends Vector
{

   private final static int MINVALUE = 0 ;
   private final static int MAXVALUE = 1 ;

   private SocketCon socketCon ;
   private boolean secondYAxisActivated ;
   private int selectedGraphPosition ;
   private String firstYAxis ;
   private String secondYAxis ;

   public final static int OKAY = 0 ;
   public final static int SECONDYAXIS = 1 ;
   public final static int XS_DONT_MATCH = 2 ;
   public final static int NUMBER_OF_DATA_DOESNT_MATCH = 3 ;
   public final static int X_AXIS_MISMATCH = 4 ;
   public final static int Y_AXIS_MISMATCH = 5 ;

   ProgressPanel progressPanel ;

   /**
    * Constructor
    * @param progressPanel the panel, that shows whats going on
    */
   public GraphData( ProgressPanel progressPanel )
   {
      this();
      this.progressPanel = progressPanel ;
   }

   public GraphData()
   {
   }

   /**
    * @return selected graph
    */
   public Graph getSelectedGraph()
   {
      if ( this.size() == 0 )return null ;
      return ( Graph )this.elementAt( selectedGraphPosition ) ;
   }

   /**
    * @return the position of the selected graph
    */
   public int getSelectedGraphPosition()
   {
      if ( this.size() == 0 )return -1 ;
      return selectedGraphPosition ;
   }

   /**
    * sets the selectedGraph to g
    * @param g the Graph to select
    */
   public void setSelectedGraph( Graph g )
   {
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         if ( ( ( Graph )this.elementAt( i ) ) == g )
         {
            this.selectedGraphPosition = i ;
            break ;
         }
      }
   }

   /**
    * sets the selected Graph to the first Graph, thats graph.Name is s
    * @param s the name of the Graph to select
    */
   public void setSelectedGraph( String s )
   {
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         if ( ( ( String ) ( ( ( Graph )this.elementAt( i ) ).graphName ) ).
              equals( s ) )
         {
            this.selectedGraphPosition = i ;
            break ;
         }
      }
   }

   /**
    * sets the selected Graph to the Graph at Position i
    * @param i the position of the Graph to select
    */
   public void setSelectedGraph( int i )
   {
      this.selectedGraphPosition = i ;
   }

   /**
    * @return the Graph with ID id and selects it
    * if no Graph with id is found returns null
    * @param id the id of the graph
    */
   public Graph getGraphWithID( int id )
   {
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         if ( ( ( Graph ) ( this.elementAt( i ) ) ).graphID == id )
         {
            this.setSelectedGraph( i ) ;
            return ( ( Graph ) ( this.elementAt( i ) ) ) ;
         }
      }
      return null ;
   }

   /**
    * @return the Graph at position i
    * @param i the position in this
    */
   public Graph getGraph( int i )
   {
      return ( Graph )this.elementAt( i ) ;
   }

   /**
    * sets the selected Graph to g
    * (removes the selected and adds g in its position)
    * @param g the Graph g to add
    */
   public void editSelectedGraph( Graph g )
   {

      this.add( selectedGraphPosition , g ) ;
   }

   /**
    * checks a Graph for any mismatches with all other Graphs
    * @param g1 the Graph to check
    * @return	this.OKAY(0)
    *		this.SECONDYAXIS(1)
    *		this.XS_DONT_MATCH(2)
    *		this.NUMBER_OF_DATA_DOESNT_MATCH(3)
    *		this.X_AXIS_MISMAXCH(4)
    *		this.Y_AXIS_MISMATCH(5)
    */
   public int checkGraphsData( Graph g1 )
   {
      return this.checkGraphsData( g1 , this.size() + 1 ) ;
   }

   /**
    * checks a Graph for any mismatches with all other Graphs except the Graph at position i
    * @param g1 the Graph to check
    * @param k the position of the Graph to ignore
    * @returns	this.OKAY(0)
    *		this.SECONDYAXIS(1)
    *		this.XS_DONT_MATCH(2)
    * 		this.NUMBER_OF_DATA_DOESNT_MATCH(3)
    *		this.X_AXIS_MISMAXCH(4)
    *		this.Y_AXIS_MISMATCH(5)
    */
   public int checkGraphsData( Graph g1 , int k )
   {
      if ( this.size() == 0 )
         return OKAY ;
      Graph referenceGraph = this.getGraph( 0 ) ;
      int differentYs = 1 ;
      if ( !referenceGraph.getXAxisText().equals( g1.getXAxisText() ) )
         return X_AXIS_MISMATCH ;
      if ( !g1.getYAxisText().equals( referenceGraph.getYAxisText() ) )
      {
         if ( ( this.secondYAxisActivated ) &&
              ( !g1.getYAxisText().equals( this.secondYAxis ) ) )
         {
            return Y_AXIS_MISMATCH ;
         }
         else
         {
            differentYs = 2 ;
         }
      }
      int max = referenceGraph.xWerte.length ;
      if ( referenceGraph.xWerte.length > g1.xWerte.length ) max = g1.xWerte.
          length ;
      for ( int j = 0 ; j < max ; j++ )
      {
         // the x-Values dont match
         if ( referenceGraph.xWerte[ j ] != g1.xWerte[ j ] )
         {
            return this.XS_DONT_MATCH ;
         }
      }
      if ( g1.xWerte.length != referenceGraph.xWerte.length )
      {
         //the number of points dont match
         return this.NUMBER_OF_DATA_DOESNT_MATCH ;
      }
      if ( differentYs == 2 )
      {
         // we have a 2nd y axis
         return this.SECONDYAXIS ;
      }
      // everything is allright
      return this.OKAY ;
   }

   /**
    *
    * @return this' socketCon
    */
   public SocketCon getSocketCon()
   {
      return this.socketCon ;
   }

   /**
    * sets this' socketCon
    * @param s the new socketCon
    */
   public void setSocketCon( SocketCon s )
   {
      this.socketCon = s ;
   }

   /**
    * removes Graph 1 and sets Graph 2 in this position
    * if this is not possible, we dont do this
    * @param g1 the old graph
    * @param g2 the new graph
    */
   public void editGraph( Graph g1 , Graph g2 )
   {
      this.setSelectedGraph( g1 ) ;
      int i = this.checkGraphsData( g2 , getSelectedGraphPosition() ) ;
      if ( i == this.OKAY )
      {
         this.setElementAt( g2 , getSelectedGraphPosition() ) ;
      }
      else
      {

      }
   }

   /**
    * adds the graph (if possible)
    * @param g the graph to add
    */
   public void addGraph( Graph g )
   {
      int i = this.checkGraphsData( g ) ;
      if ( i == this.OKAY )
      {
         if ( !this.isEmpty() )
         {
            g.graphID = ( ( Graph ) ( this.elementAt( this.size() - 1 ) ) ).
                graphID + 1 ;
         }
         else
         {
            g.graphID = 0 ;
         }
         this.add( g ) ;
      }
      else
      {
         boolean added = false ;
         if ( ( i == 2 ) || ( i == 3 ) )
         {
            added = this.tryToAddWithResorting( g ) ;
            //System.err.println( "added" ) ;
         }
         if ( i == 1 )
         {
            if ( !secondYAxisActivated )
            {
               this.secondYAxisActivated = true ;
               this.secondYAxis = g.getYAxisText() ;
            }
            if ( !this.isEmpty() )
            {
               g.graphID = ( ( Graph ) ( this.elementAt( this.size() - 1 ) ) ).
                   graphID + 1 ;
            }
            else
            {
               g.graphID = 0 ;
            }
            this.add( g ) ;
            added = true ;
         }
         if ( !added )
         {
            String s = "unknown" ;
            switch ( i )
            {
               case SECONDYAXIS:
                  s = "More then 1 Y-Axis" ;
                  break ;
               case XS_DONT_MATCH:
                  s = "Xs dont match" ;
                  break ;
               case NUMBER_OF_DATA_DOESNT_MATCH:
                  s = "Number of Points dont match" ;
                  break ;
               case X_AXIS_MISMATCH:
                  s = "X-Axis doesnt match" ;
                  break ;
               case Y_AXIS_MISMATCH:
                  s = "Would need a third Y-Axis" ;
                  break ;
            }
               System.err.println( "Could not edit Graph because: " +
                                   s ) ;
         }
      }
   }

   /**
    * removes the selected graph
    */
   public void removeSelectedGraph()
   {
      if ( this.mixer != null )
         this.mixer.removeFunction( this.getSelectedGraph().getGraphName() ) ;

      String yAxis = this.getSelectedGraph().getYAxisText() ;
      if ( ( this.size() == 1 ) )
      {
         this.remove( this.selectedGraphPosition ) ;
         this.selectedGraphPosition = 0 ;
         return ;
      }
      int numberOfFunctionsWithTheSameName = 0 ;
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         if ( i != this.getSelectedGraphPosition() )
         {
            if ( yAxis.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
               numberOfFunctionsWithTheSameName++ ;
         }
      }
      if ( numberOfFunctionsWithTheSameName == 0 )
      {
         secondYAxisActivated = false ;
      }
      this.remove( this.selectedGraphPosition ) ;
      this.selectedGraphPosition = 0 ;
      this.zipPoints() ;
      return ;
   }

   public void removeAllElements()
   {
      super.removeAllElements() ;
      this.secondYAxisActivated = false ;
      this.mixer = null ;
   }

   private void zipPoints()
   {
      boolean[] newPoints = new boolean[this.getGraph( 0 ).getXValues().length ] ;
      for ( int i = 0 ; i < newPoints.length ; i++ )
         newPoints[ i ] = true ;
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         double[] g = this.getGraph( i ).getYValues() ;
         for ( int j = 0 ; j < newPoints.length ; j++ )
            if ( g[ j ] < 0.0 )
               newPoints[ j ] = newPoints[ j ] && true ;
            else
               newPoints[ j ] = newPoints[ j ] && false ;
      }
      // counting points to remove
      int removes = 0 ;
      for ( int i = 0 ; i < newPoints.length ; i++ )
         if ( !newPoints[ i ] )
            removes++ ;

      for ( int i = 0 ; i < this.size() ; i++ )
      {
         Graph g = this.getGraph( i ) ;
         int nextPosition = 0 ;
         double[] newXValues = new double[removes ] ;
         double[] newYValues = new double[removes ] ;
         double[] oldXValues = g.getXValues() ;
         double[] oldYValues = g.getYValues() ;
         for ( int j = 0 ; j < newPoints.length ; j++ )
         {
            if ( !newPoints[ j ] )
            {
               newXValues[ nextPosition ] = oldXValues[ j ] ;
               newYValues[ nextPosition ] = oldYValues[ j ] ;
               nextPosition++ ;
            }
         }
         g.setXValues( newXValues ) ;
         g.setYValues( newYValues ) ;
      }

   }

   /**
    * returns x and y values for jfreechart or preview
    * @return the points of this in a viewable style
    */
   public double[][] getViewablePoints()
   {
      if ( this.size() == 0 )
      {
         return new double[0 ][ 0 ] ;
      }
      int numberOfFunctionsForYAxis1 = 1 ;
      String yAxis1 = ( ( Graph )this.get( 0 ) ).getYAxisText() ;
      for ( int i = 1 ; i < this.size() ; i++ )
      {
         if ( yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
            numberOfFunctionsForYAxis1++ ;
      }
      double[][] data = new double[numberOfFunctionsForYAxis1 +
          1 ][ ( ( Graph )this.elementAt( 0 ) ).getNumberOfPoints() ] ;
      data[ 0 ] = ( ( Graph )this.elementAt( 0 ) ).getXValues() ;
      numberOfFunctionsForYAxis1 = 0 ;
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         if ( yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
         {
            numberOfFunctionsForYAxis1++ ;
            data[ numberOfFunctionsForYAxis1 ] = ( ( Graph )this.elementAt( i ) ).
                getYValues() ;
         }
      }
      return data ;
   }

   public double[][] getViewablePointsFor2ndYAxis()
   {
      if ( this.size() == 0 )
      {
         return new double[0 ][ 0 ] ;
      }
      int numberOfFunctionsForYAxis2 = 0 ;
      String yAxis1 = ( ( Graph )this.get( 0 ) ).getYAxisText() ;
      for ( int i = 1 ; i < this.size() ; i++ )
      {
         if ( !yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
            numberOfFunctionsForYAxis2++ ;
      }
      if ( numberOfFunctionsForYAxis2 == 0 )
         return new double[0 ][ 0 ] ;
      double data[][] = new double[numberOfFunctionsForYAxis2 +
          1 ][ ( ( Graph ) ( this.elementAt( 0 ) ) ).getNumberOfPoints() ] ;
      data[ 0 ] = ( ( Graph )this.elementAt( 0 ) ).getXValues() ;
      numberOfFunctionsForYAxis2 = 0 ;
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         if ( !yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
         {
            numberOfFunctionsForYAxis2++ ;
            data[ numberOfFunctionsForYAxis2 ] = ( ( Graph )this.elementAt( i ) ).
                getYValues() ;
         }
      }
      return data ;
   }

   public String[] getNamesForFirstYAxis()
   {
      if ( this.size() == 0 )
      {
         return new String[0 ] ;
      }
      if ( !this.has2ndYAxis() )
      {
         String[] names = new String[this.size() ] ;
         for ( int i = 0 ; i < this.size() ; i++ )
         {
            names[ i ] = ( ( Graph )this.elementAt( i ) ).getGraphName() ;
         }
         return names ;
      }
      else
      {

         int numberOfFunctionsForYAxis1 = 1 ;
         String yAxis1 = ( ( Graph )this.get( 0 ) ).getYAxisText() ;
         for ( int i = 1 ; i < this.size() ; i++ )
         {
            if ( yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
               numberOfFunctionsForYAxis1++ ;
         }
         String[] names = new String[numberOfFunctionsForYAxis1 ] ;
         int whereInField = 0 ;
         for ( int i = 0 ; i < this.size() ; i++ )
         {
            if ( yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
               names[ whereInField++ ] = ( ( Graph )this.elementAt( i ) ).
                   getGraphName() ;
         }
         return names ;
      }
   }

   public String[] getNamesForSecondYAxis()
   {
      if ( this.size() == 0 )
      {
         return new String[0 ] ;
      }
      if ( !this.has2ndYAxis() )
      {
         return new String[0 ] ;
      }
      else
      {

         int numberOfFunctionsForYAxis2 = 0 ;
         String yAxis1 = ( ( Graph )this.get( 0 ) ).getYAxisText() ;
         for ( int i = 1 ; i < this.size() ; i++ )
         {
            if ( !yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
               numberOfFunctionsForYAxis2++ ;
         }
         //!!!System.err.println("Number of 2nd:"+numberOfFunctionsForYAxis2);
         String[] names = new String[numberOfFunctionsForYAxis2 ] ;
         int whereInNames = 0 ;
         for ( int i = 0 ; i < this.size() ; i++ )
         {
            if ( !yAxis1.equals( ( ( Graph )this.elementAt( i ) ).getYAxisText() ) )
               names[ whereInNames++ ] = ( ( Graph )this.elementAt( i ) ).
                   getGraphName() ;
         }
         return names ;
      }
   }

   public boolean has2ndYAxis()
   {
      return this.secondYAxisActivated ;
   }

   public String get2ndYAxisName()
   {
      return this.secondYAxis ;
   }

   /**
    * e.g. if Graph 1 has the x-values 1,2,3,4,5 and Graph 2 has the x-values 1,2,3
    * the x-values of Graph 2 are expanded to 1,2,3,4,5 and the y-values at 4,5
    * become -1
    * @param g the Graph we try to add
    * @return whether the adding was successfull
    */
   private boolean tryToAddWithResorting( Graph g )
   {
      // set to data, that doesnt exist or is wrong
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
      // the new x-axis
      double[] nextXValues = new double[this.getGraph( 0 ).getNumberOfPoints() +
          g.getNumberOfPoints() ] ;
      // which element is the one to write
      int whichWriteNext = 0 ;
      // setting all to errorDou
      //dSystem.err.println("setting all to errorDou");
      for ( int i = 0 ; i < nextXValues.length ; i++ )
      {
         nextXValues[ i ] = errorDou ;
      }
      //dSystem.err.println("we get all possible Values");
      // we get all possible Values
      boolean ready = false ;
      int oldSize = this.getGraph( 0 ).getNumberOfPoints() ;
      int gSize = g.getNumberOfPoints() ;
      int oldIndex = 0 , gIndex = 0 ;
      double[] gXValues = g.getXValues() ;
      double[] oldXValues = this.getGraph( 0 ).getXValues() ;

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
      //dSystem.err.println("cut");
      // first getLength
      int newLength = whichWriteNext ;
      //dSystem.err.println("writing new data for old graphs");
      // writing new data for old graphs
      double xValues[] = new double[newLength ] ;
      for ( int i = 0 ; i < newLength ; i++ )
         xValues[ i ] = nextXValues[ i ] ;
      // writing new yValues (or errorDou)
      for ( int i = 0 ; i < this.size() ; i++ )
      {
         Graph workGraph = this.getGraph( i ) ;
         //dSystem.err.println("Graph:"+workGraph.getGraphName());
         double yValues[] = new double[newLength ] ;
         for ( int j = 0 ; j < newLength ; j++ )
         {
            yValues[ j ] = workGraph.getYValue( xValues[ j ] ) ;
         }
         workGraph.setPoints( xValues , yValues ) ;
      }
      //dSystem.err.println("writing new data for new graph");
      // ... and new graph
      double yValues[] = new double[newLength ] ;
      for ( int j = 0 ; j < newLength ; j++ )
      {
         yValues[ j ] = g.getYValue( xValues[ j ] ) ;
      }
      g.setPoints( xValues , yValues ) ;
      this.addGraph( g ) ;
      // finaly return true
      //dSystem.err.println("Those points, that have an y-VAlue of -1 are invalid, but must be created.");
      return true ;
   }

   /**
    * adds the functions from a file
    * @param file the file to get the graphs from
    * @deprecated
    */
   public void addGraphFromFile( File file )
   {

      Vector v = new Vector() ;
      BufferedReader in = null ;
      try
      {
         in = new BufferedReader( new FileReader( file ) ) ;
      }
      catch ( Exception e )
      {
         System.err.println( "cannot load file" ) ;
      }
      try
      {
         String line ;
         while ( ( line = in.readLine() ) != null )
         {
            v.add( line ) ;
         }
      }
      catch ( Exception e )
      {
         System.err.println( "cannot load file" ) ;
      }
      String[] fileString = new String[v.size() ] ;
      for ( int i = 0 ; i < v.size() ; i++ )
      {
         fileString[ i ] = ( String ) v.elementAt( i ) ;
      }
      v = this.socketCon.getXsYsAndFunctionNamesFromFile( "<any>" , fileString ) ;
      Graph[] graphs = new Graph[ ( ( String[] ) v.elementAt( 0 ) ).length ] ;
      double[] xValues = new double[ ( ( Double[][] ) ( v.elementAt( 3 ) ) )[ 0 ].
          length ] ;
      System.err.println( "Setting xValues" ) ;
      for ( int i = 0 ; i < xValues.length ; i++ )
      {
         xValues[ i ] = ( ( ( Double[][] ) ( v.elementAt( 3 ) ) )[ 0 ][ i ] ).
             doubleValue() ;
      }
      for ( int i = 0 ; i < graphs.length ; i++ )
      {
         graphs[ i ] = new Graph() ;
         graphs[ i ].setGraphName( file.getName() + " - " +
                                   ( ( String[] ) v.elementAt( 0 ) )[ i ] ) ;
         graphs[ i ].setXAxisText( ( String ) v.elementAt( 1 ) ) ;
         graphs[ i ].setYAxisText( ( ( String[] ) v.elementAt( 2 ) )[ i ] ) ;
         graphs[ i ].setXValues( xValues ) ;
         double[] yValues = new double[ ( ( Double[][] ) ( v.elementAt( 3 ) ) )[
             0 ].length ] ;
         System.err.println( "Setting yValues" ) ;
         for ( int j = 0 ; j < yValues.length ; j++ )
         {
            yValues[ j ] = ( ( ( Double[][] ) ( v.elementAt( 3 ) ) )[ i +
                             1 ][ j ] ).doubleValue() ;
         }
         graphs[ i ].setYValues( yValues ) ;
         System.err.println( "Trying to add Graph" ) ;
         try
         {
            this.addGraph( graphs[ i ] ) ;
         }
         catch ( Exception e )
         {
            System.err.println( e ) ;
         }
         System.err.println( "Ready" ) ;
      }
   }

   /**
    * Writes the identifier plus the function number
    * @param strOutstring the StringBuffer where sth will be appended
    * @param strIdentifier Identifier we want to append
    * @param iFunctionNumber the Number of the function
    */
   /*private void writeIdentifier( StringBuffer strOutstring ,
                                 String strIdentifier ,
                                 int iFunctionNumber )
   {
      strOutstring.append( strIdentifier ) ;
      strOutstring.append( "-Function-" ) ;
      strOutstring.append( iFunctionNumber + 1 ) ;
      strOutstring.append( "=" ) ;
   }*/

   /**
    * Search the original files for the specified identifier
    * @param strOriginalFile the original file (its lines)
    * @param strIdentifier the identifier we search for
    * @return the value of the searched identifier or ""
    */
  /* private String searchForIdentifier( String[] strOriginalFile ,
                                       String strIdentifier )
   {
      String strIdent = strIdentifier + "=" ;
      int iIdentLength = strIdent.length() ;
      // Search the file...
      for ( int j = 0 ; j < strOriginalFile.length ; j++ )
      {
         if ( strOriginalFile[ j ].indexOf( strIdent ) == 0 )
            return strOriginalFile[ j ].substring( iIdentLength ) ;
      }

      return "" ;
   }*/

   /**
    * Search the min- or max-value of a specified identifier
    * @param Graphs the Graphs to get the bounds for
    * @param iGraphCount the lenght of Graphs
    * @param strIdentifier the identifier to get the bounds for
    * @param iType can be this.MINVALUE (0) or this.MAXVALUE (1)
    * @return the bounded Value
    */
  /* private double getBoundedValue( Graph[] Graphs , int iGraphCount ,
                                   String strIdentifier ,
                                   int iType )
   {
      String strTemp ;
      double dTemp , dValue = 0.0 ;
      boolean bSet = false ;
      // Search all graphs
      for ( int k = 0 ; k < iGraphCount ; k++ )
      {
         // Get the value of the specified identifier
         strTemp = searchForIdentifier( Graphs[ k ].getFileContent() ,
                                        strIdentifier ) ;
         try
         {
            dTemp = ( new Double( strTemp ) ).doubleValue() ;
            // First value?
            if ( !bSet )
            {
               dValue = dTemp ;
               bSet = true ;
            }
            // Check criterion
            else
            {
               // Criterion = Minimum?
               if ( iType == MINVALUE )
                  if ( dTemp < dValue )
                     dValue = dTemp ;
               // Criterion = Maximum?
               if ( iType == MAXVALUE )
                  if ( dTemp > dValue )
                     dValue = dTemp ;
            }
         }
         catch ( NumberFormatException exception )
         {
         }
      }

      return dValue ;
   }*/

   /**
    * Search the original files for the specified identifier (display option)
    * @param strOutstring the StringBuffer on which we append sth.
    * @param strOriginalFile the original file (as line)
    * @param strIdentifier the identifier to search
    * @param iFunctionNumber the functionNumber
    */
   /*private void searchAndWriteDisplayInfo( StringBuffer strOutstring ,
                                           String[] strOriginalFile ,
                                           String strIdentifier ,
                                           int iFunctionNumber )
   {
      String strIdent = "display" + strIdentifier + "=" ;
      int iIdentLength = strIdent.length() , iPos = -1 ;
      // Search the file...
      for ( int j = 0 ; j < strOriginalFile.length ; j++ )
      {
         if ( strOriginalFile[ j ].indexOf( strIdent ) == 0 )
         {
            iPos = j ;
            break ;
         }
      }
      // Isn't there!
      if ( iPos == -1 )
         return ;

      // Write the display-flag
      strIdent = "display" + strIdentifier + "-Function-" + iFunctionNumber +
          "=" ;
      strOutstring.append( strIdent ) ;
      strOutstring.append( "0\n" ) ;

      // Write the comment
      iPos++ ;
      strOutstring.append( strOriginalFile[ iPos ] ) ;
      strOutstring.append( "\n" ) ;

      // Write "t"
      strIdent = "t" + strIdentifier + "-Function-" + iFunctionNumber + "=" ;
      strOutstring.append( strIdent ) ;
      iPos++ ;
      try
      {
         strOutstring.append( strOriginalFile[ iPos ].substring(
             strOriginalFile[ iPos ].indexOf( "=" ) + 1 ) ) ;
      }
      catch ( Exception exception )
      {
      }
      strOutstring.append( "\n" ) ;

      // Write "x"
      strIdent = "x" + strIdentifier + "-Function-" + iFunctionNumber + "=" ;
      strOutstring.append( strIdent ) ;
      iPos++ ;
      try
      {
         strOutstring.append( strOriginalFile[ iPos ].substring(
             strOriginalFile[ iPos ].indexOf( "=" ) + 1 ) ) ;
      }
      catch ( Exception exception )
      {
      }
      strOutstring.append( "\n" ) ;

      // Write "y"
      strIdent = "y" + strIdentifier + "-Function-" + iFunctionNumber + "=" ;
      strOutstring.append( strIdent ) ;
      iPos++ ;
      try
      {
         strOutstring.append( strOriginalFile[ iPos ].substring(
             strOriginalFile[ iPos ].indexOf( "=" ) + 1 ) ) ;
      }
      catch ( Exception exception )
      {
      }
      strOutstring.append( "\n" ) ;

      // Write "fonttyp"
      strIdent = "fonttyp" + strIdentifier + "-Function-" + iFunctionNumber +
          "=" ;
      strOutstring.append( strIdent ) ;
      iPos++ ;
      try
      {
         strOutstring.append( strOriginalFile[ iPos ].substring(
             strOriginalFile[ iPos ].indexOf( "=" ) + 1 ) ) ;
      }
      catch ( Exception exception )
      {
      }
      strOutstring.append( "\n" ) ;

      // Write "fontsize"
      strIdent = "fontsize" + strIdentifier + "-Function-" + iFunctionNumber +
          "=" ;
      strOutstring.append( strIdent ) ;
      iPos++ ;
      try
      {
         strOutstring.append( strOriginalFile[ iPos ].substring(
             strOriginalFile[ iPos ].indexOf( "=" ) + 1 ) ) ;
      }
      catch ( Exception exception )
      {
      }
      strOutstring.append( "\n" ) ;
      strOutstring.append( "\n" ) ;
   }*/

   /**
    * Writes this class to the specified file.
    * @param fileName the name of the file to save to
    */
   /*public void writeToFile( String fileName )
   {
      // Try to create the output stream...
      FileWriter outputStream = null ;
      try
      {
         outputStream = new FileWriter( system.BIGInterface.getInstance().
                                        getBenchItPath() +
                                        java.io.File.separator + "output" +
                                        java.io.File.separator + fileName ) ;
         System.err.println( system.BIGInterface.getInstance().getBenchItPath() +
                             java.io.File.separator + "output" +
                             java.io.File.separator + fileName ) ;
      }
      // Any errors?
      catch ( IOException exception )
      {
         try
         {
            java.io.File outDir = new java.io.File( system.BIGInterface.
                getInstance().getBenchItPath() +
                java.io.File.separator + "output" ) ;
            outDir.mkdir() ;
            outputStream = new FileWriter( system.BIGInterface.getInstance().
                                           getBenchItPath() +
                                           java.io.File.separator + "output" +
                                           java.io.File.separator + fileName ) ;
            System.err.println( system.BIGInterface.getInstance().
                                getBenchItPath() +
                                java.io.File.separator + "output" +
                                java.io.File.separator + fileName ) ;
         }
         catch ( IOException exception2 )
         {
            System.err.println( "Couldn't write file! No rights?" ) ;
            return ;
         }
      }
      if ( outputStream == null )
      {
         System.err.println( "Couldn't write file!" ) ;
         return ;
      }

      //
      // Prepare the graphs
      //
      int iCount = this.size() , i , j , iTemp ;
      Graph[] Graphs = new Graph[this.size() ] ;
      String strTemp = null ;

      for ( i = 0 ; i < this.size() ; i++ )
         Graphs[ i ] = ( Graph )this.elementAt( i ) ;

      //
      // Lets go...
      //
      StringBuffer strOutput = new StringBuffer() ;

      strOutput.append( "# BenchIT-Resultfile\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append(
          "# feel free to fill in more architectural information\n" ) ;
      strOutput.append(
          "#########################################################\n" ) ;

      //
      // Write the measurement information
      //
      System.err.println( "Writing the measurement information..." ) ;
      strOutput.append( "beginofmeasurementinfos\n" ) ;

      // Write the name of the graphs
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "kernelstring" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "programname" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the date
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "date" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "date" ) ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the number of processors
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "numberofprocessorsused" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "numberofprocessorsused" ) ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the size of the memory
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "memorysizeused" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "memorysizeused" ) ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the comments
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "comment" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "comment" ) ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the language
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "language" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "language" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the compiler
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "compiler" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "compiler" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the compiler
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "compiler" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "compiler" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the compilerflags
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "compilerflags" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "compilerflags" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the kernelisparallel
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "kernelisparallel" , i ) ;
         strTemp = Graphs[ i ].getSetting( "isparallel" ) ;
         iTemp = -1 ;
         if ( strTemp.equals( "t" ) )
            iTemp = 1 ;
         if ( strTemp.equals( "f" ) )
            iTemp = 0 ;
         if ( iTemp != -1 )
            strOutput.append( iTemp ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the codesequence
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "codesequence" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "codesequence" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the x-min value
      strOutput.append( "xinmin=" ) ;
      strOutput.append( getBoundedValue( Graphs , iCount , "xinmin" ,
                                         GraphData.MINVALUE ) ) ;
      strOutput.append( "\n" ) ;
      // Write the x-max value
      strOutput.append( "xinmax=" ) ;
      strOutput.append( getBoundedValue( Graphs , iCount , "xinmax" ,
                                         GraphData.MAXVALUE ) ) ;
      strOutput.append( "\n" ) ;

      // Write the y-min value
      strOutput.append( "yinmin=" ) ;
      strOutput.append( getBoundedValue( Graphs , iCount , "yinmin" ,
                                         GraphData.MINVALUE ) ) ;
      strOutput.append( "\n" ) ;
      // Write the y-max value
      strOutput.append( "yinmax=" ) ;
      strOutput.append( getBoundedValue( Graphs , iCount , "yinmax" ,
                                         GraphData.MAXVALUE ) ) ;
      strOutput.append( "\n" ) ;

      strOutput.append( "endofmeasurementinfos\n" ) ;
      System.err.println(
          "Writing the measurement information... Finished!" ) ;
      strOutput.append(
          "#########################################################\n" ) ;

      //
      // Write the architecture
      //
      System.err.println( "Writing the architecture information..." ) ;
      strOutput.append( "beginofarchitecture\n" ) ;
      strOutput.append( "# architecture information\n" ) ;

      // Write the nodename
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "nodename" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "nodename" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      // Write the hostname
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "hostname" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "hostname" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "###############################################################################\n" ) ;
      strOutput.append( "#                                                                             #\n" ) ;
      strOutput.append( "#  B e n c h I T - Performance Measurement for Scientific Applications        #\n" ) ;
      strOutput.append( "#                                                                             #\n" ) ;
      strOutput.append( "#  Architectural information about your system, used to compare your system   #\n" ) ;
      strOutput.append( "#  to others. Please fill out the information - your resultfile will not be   #\n" ) ;
      strOutput.append( "#  accepted on the project homepage without this information.                 #\n" ) ;
      strOutput.append( "#                                                                             #\n" ) ;
      strOutput.append( "#  Authors: Guido Juckeland (juckeland@zhr.tu-dresden.de)                     #\n" ) ;
      strOutput.append( "#           Stefan Pflüger  (pflueger@zhr.tu-dresden.de)                      #\n" ) ;
      strOutput.append( "#  $Revision: 1.5 $                                                           #\n" ) ;
      strOutput.append( "#  $Date: 2007/01/30 09:46:33 $                                               #\n" ) ;
      strOutput.append( "#                                                                             #\n" ) ;
      strOutput.append( "###############################################################################\n" ) ;
      strOutput.append( "\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append( "# feel free to insert your data\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append( "# quotation marks have to be `escaped'!!!\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append(
          "# Note: The examples are from very different systems.\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append( "\n" ) ;
      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Manufacturer of the mainboard, for example: \"MSI\"\n" ) ;

      // Write the mainboardmanufacturer
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "mainboardmanufacturer" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "mainboardmanufacturer" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Type of the mainboard without manufacturer's name, for example: \"845PE Max3 (800 Edition)\"\n" ) ;
      // Write the mainboardtype
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "mainboardtype" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "mainboardtype" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Chipset of the mainboard, for example: \"Intel i845PE\"\n" ) ;
      // Write the mainboardchipset
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "mainboardchipset" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "mainboardchipset" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Total number of Processors\n" ) ;
      // Write the numberofprocessors
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "numberofprocessors" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "numberofprocessors" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Name of the processor, for example: \"Intel Pentium 4\"\n" ) ;
      // Write the processorname
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorname" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorname" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Processor Serial Number, if available\n" ) ;
      // Write the processorserialnumber
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorserialnumber" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorserialnumber" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Processor version, for example: \"x86, Family 6, Model 5, Stepping 1\"\n" ) ;
      // Write the processorversion
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorversion" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorversion" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Instruction Set Architecture (ISA) of the processor, for example: \"IA-32\" or \"MIPS\"\n" ) ;
      // Write the processorisaset
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorisaset" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorisaset" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Several processors are using exact descriptions of their ISA level, for example: \"IV\" (for MIPS)\n" ) ;
      // Write the processorisalevel
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorisalevel" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorisalevel" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Extensions of the ISA with an additional number; extendable\n" ) ;
      strOutput.append( "# for example: \n" ) ;
      strOutput.append( "# processorisaextension1=\"MMX\"\n" ) ;
      strOutput.append( "# processorisaextension2=\"SSE\"\n" ) ;
      strOutput.append( "# processorisaextension3=\"SSE2\"\n" ) ;
      // Write the processorisaextension1
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorisaextension1" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorisaextension1" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Clock rate of the processor in Hz, for example: 3060000000\n" ) ;
      // Write the processorclockrate
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorclockrate" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorclockrate" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Instruction length in bit, for example: 32 [for RISC] or 0 [for CISC or variable length]\n" ) ;
      // Write the instructionlength
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "instructionlength" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "instructionlength" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Processor word length or Word format or xx-bit architecture in bit, for example: 32\n" ) ;
      // Write the processorwordlength
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorwordlength" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorwordlength" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Number of instructions issued to the functional units per clock, for example: 6\n" ) ;
      // Write the instructionissue
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "instructionissue" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "instructionissue" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Number of separate integer units, for example: 2\n" ) ;
      // Write the integerunits
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "integerunits" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "integerunits" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Number of separate floating point units, for example: 3\n" ) ;
      // Write the floatingpointunits
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "floatingpointunits" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "floatingpointunits" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Number of separate load/store units, for example: 1\n" ) ;
      // Write the loadstoreunits
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "loadstoreunits" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "loadstoreunits" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Support of multithreading, for example: 1 [0 for no | 1 for yes] [Hyperthreading --> 1]\n" ) ;
      // Write the multithreading
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "multithreading" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "multithreading" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# L1 instruction cache size in byte, for example: 12288\n" ) ;
      // Write the level1icachesize
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level1icachesize" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level1icachesize" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# L1 data cache size in byte, for example: 8192\n" ) ;
      // Write the level1dcachesize
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level1dcachesize" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level1dcachesize" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# L2 cache size in byte, for example: 524288\n" ) ;
      // Write the level2cachesize
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level2cachesize" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level2cachesize" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# L3 cache size in byte, for example:  33554432\n" ) ;
      // Write the level3cachesize
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level3cachesize" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level3cachesize" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# L1 instruction cache organization, for example: \"Execution Trace Cache\"\n" ) ;
      // Write the level1icachetype
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level1icachetype" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level1icachetype" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# L1 data cache organization, for example: \"4-way set-associative, write-through, dual ported\"\n" ) ;
      // Write the level1dcachetype
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level1dcachetype" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level1dcachetype" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# L2 cache organization, for example: \"8-way set-associative, cache line size 32 words\"\n" ) ;
      // Write the level2cachetype
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level2cachetype" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level2cachetype" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# L3 cache organization, for example: \"8-way set-associative, cache line size 128 words with 4 128-byte sectors\"\n" ) ;
      // Write the level3cachetype
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level3cachetype" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level3cachetype" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# L2 cache location, for example: \"on-chip\" or \"on-board\"\n" ) ;
      // Write the level2cachelocation
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "level2cachelocation" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "level2cachelocation" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Support of prefetching, for example: 1 [0 for no | 1 for yes]\n" ) ;
      // Write the prefetching
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "prefetching" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "prefetching" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Support of preloading, for example: 1 [0 for no | 1 for yes]\n" ) ;
      // Write the preloading
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "preloading" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "preloading" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Clock rate of the processor side bus in Hz, for example: 133000000\n" ) ;
      // Write the processorsidebusclockrate
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "processorsidebusclockrate" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "processorsidebusclockrate" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Clock rate of the backside bus in Hz, for example: 200000000\n" ) ;
      // Write the backsideclockrate
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "backsideclockrate" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "backsideclockrate" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Structure details of the technology in m, for example: 0.000000130\n" ) ;
      // Write the technologylevel
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "technologylevel" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "technologylevel" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Memory chip type, for example: \"PC-100 SDRAM\" or \"RIMM-4200\"\n" ) ;
      // Write the memorychiptype
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "memorychiptype" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "memorychiptype" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Memory bus type, for example: \"SDRAM bus\" or \"RAMBUS\"\n" ) ;
      // Write the memorybustype
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "memorybustype" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "memorybustype" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append(
          "# Clock rate of the memory bus in Hz, for example: 100000000\n" ) ;
      // Write the memorybusclockrate
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "memorybusclockrate" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "memorybusclockrate" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "\n" ) ;
      strOutput.append( "# Total size of available memory\n" ) ;
      // Write the memorysize
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "memorysize" , i ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getSetting( "memorysize" ) ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "endofarchitecture\n" ) ;
      System.err.println(
          "Writing the architecture information... Finished!" ) ;
      strOutput.append(
          "#########################################################\n" ) ;

      //
      // Write the display information
      //
      System.err.println( "Writing the display information..." ) ;
      strOutput.append( "beginofdisplay\n" ) ;
      strOutput.append( "\n" ) ;

      // Write the xoutmin value
      strOutput.append( "xoutmin=" ) ;
      strOutput.append( getBoundedValue( Graphs , iCount , "xoutmin" ,
                                         GraphData.MINVALUE ) ) ;
      strOutput.append( "\n" ) ;
      // Write the xoutmax value
      strOutput.append( "xoutmax=" ) ;
      strOutput.append( getBoundedValue( Graphs , iCount , "xoutmax" ,
                                         GraphData.MAXVALUE ) ) ;
      strOutput.append( "\n" ) ;
      // Write the xaxisticks
      strOutput.append( "xaxisticks=" ) ;
      strOutput.append( searchForIdentifier( Graphs[ 0 ].getFileContent() ,
                                             "xaxisticks" ) ) ;
      strOutput.append( "\n" ) ;
      // Write the xaxislogarithmic
      strOutput.append( "xaxislogarithmic=" ) ;
      strOutput.append( searchForIdentifier( Graphs[ 0 ].getFileContent() ,
                                             "xaxislogarithmic" ) ) ;
      strOutput.append( "\n" ) ;
      // Write the xaxislogbase
      strOutput.append( "xaxislogbase=" ) ;
      strOutput.append( searchForIdentifier( Graphs[ 0 ].getFileContent() ,
                                             "xaxislogbase" ) ) ;
      strOutput.append( "\n" ) ;
      strOutput.append( "\n" ) ;

      // Write the youtmin
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "youtmin" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "youtmin" ) ) ;
         strOutput.append( "\n" ) ;
      }
      // Write the youtmax
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "youtmax" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "youtmax" ) ) ;
         strOutput.append( "\n" ) ;
      }
      // Write the yaxisticks
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "yaxisticks" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "yaxisticks" ) ) ;
         strOutput.append( "\n" ) ;
      }
      // Write the yaxislogarithmic
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "yaxislogarithmic" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "yaxislogarithmic" ) ) ;
         strOutput.append( "\n" ) ;
      }
      // Write the yaxislogbase
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "yaxislogbase" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "yaxislogbase" ) ) ;
         strOutput.append( "\n" ) ;
      }
      strOutput.append( "\n" ) ;

      // Write the xaxistext
      for ( i = 0 ; i < iCount ; i++ )
      {
         writeIdentifier( strOutput , "xaxistext" , i ) ;
         strOutput.append( searchForIdentifier( Graphs[ i ].getFileContent() ,
                                                "xaxistext" ) ) ;
         strOutput.append( "\n" ) ;
      }
      strOutput.append( "xaxistext=" + this.getGraph( 0 ).getXAxisText() + "\n" ) ;
      // Write the yaxistext1
      for ( i = 0 ; i < iCount ; i++ )
      {
         strOutput.append( "y" + ( i + 1 ) + "axistext=\"" +
                           Graphs[ i ].getYAxisText() + "\"" ) ;
         strOutput.append( "\n" ) ;
      }
      strOutput.append( "\n" ) ;

      // Write the numabscissae
      strOutput.append( "numabscissae=" ) ;
      strOutput.append( Graphs[ 0 ].getXValues().length ) ;
      strOutput.append( "\n" ) ;
      // Write the numfunctions
      strOutput.append( "numfunctions=" ) ;
      strOutput.append( iCount ) ;
      strOutput.append( "\n" ) ;
      strOutput.append( "\n" ) ;

      strOutput.append( "## Fuer Architektur- oder Messmerkmal : abc\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append(
          "#   displayabc=1                                           # Steuervariable\n" ) ;
      strOutput.append(
          "#   #Textfeld-Eigenschaften des date-Strings               # Kommentar\n" ) ;
      strOutput.append(
          "#   tabc=\"ABC-Merkmal: 3 Tm\"                               # Text: Wert\n" ) ;
      strOutput.append(
          "#   xabc=                                                  # x-Position\n" ) ;
      strOutput.append(
          "#   yabc=                                                  # y-Position\n" ) ;
      strOutput.append(
          "#   fonttypabc=                                            # Schriftart\n" ) ;
      strOutput.append(
          "#   fontsizeabc=                                           # Schriftgroesse\n" ) ;

      // Write information about the functions
      for ( i = 0 ; i < iCount ; i++ )
      {
         // Write the name of the function
         strOutput.append( "tlegendfunction" ) ;
         strOutput.append( ( i + 1 ) ) ;
         strOutput.append( "=" ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( Graphs[ i ].getGraphName() ) ;
         strOutput.append( "\"" ) ;
         strOutput.append( "\n" ) ;

         // Write stuff
         strOutput.append( "xlegendfunction" ) ;
         strOutput.append( ( i + 1 ) ) ;
         strOutput.append( "=\n" ) ;

         strOutput.append( "ylegendfunction" ) ;
         strOutput.append( ( i + 1 ) ) ;
         strOutput.append( "=\n" ) ;

         strOutput.append( "fonttypelegendfunction" ) ;
         strOutput.append( ( i + 1 ) ) ;
         strOutput.append( "=\n" ) ;

         strOutput.append( "fontsizelegendfunction" ) ;
         strOutput.append( ( i + 1 ) ) ;
         strOutput.append( "=\n" ) ;

         strOutput.append( "\n" ) ;
      }

      // Write some <deutsche> comments...
      strOutput.append(
          "#### alle optionalen Anzeigen ueber Wahrheitswerte display***=1 aktivierbar\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append( "# Optionale Angaben zu den Messprogrammen:\n" ) ;
      strOutput.append( "#\n" ) ;
      strOutput.append(
          "# Ausblenden des kernelstrings sinnvoll, wenn separate Bildunterschrift\n" ) ;
      strOutput.append( "# verwendet werden soll!\n" ) ;

      // TODO: Make it dynamic!
      strOutput.append( "displaykernelstring=1\n" ) ;
      strOutput.append( "# topkernelstring=1|0: Bild-Ueber-|Unterschrift\n" ) ;
      strOutput.append( "topkernelstring=1\n" ) ;
      strOutput.append( "fonttypkernelstring=\n" ) ;
      strOutput.append( "fontsizekernelstring=\n" ) ;

      // Write comment info
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "comment" , i ) ;

      // Write date info
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "date" , i ) ;

      // Write date language
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "language" , i ) ;

      // Write date compiler
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "compiler" , i ) ;

      // Write date compilerflags
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "compilerflags" , i ) ;

      // Write date codesequence
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "codesequence" , i ) ;

      strOutput.append(
          "# Angaben zu den Architekturmerkmalen, Namen etc. (alle Angaben OPTIONAL):\n" ) ;
      strOutput.append( "\n" ) ;

      // Write date hostname
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "hostname" , i ) ;

      // Write date nodename
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "nodename" , i ) ;

      // Write date mainboardmanufacturer
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "mainboardmanufacturer" , i ) ;

      // Write date mainboardtype
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "mainboardtype" , i ) ;

      // Write date mainboardchipset
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "mainboardchipset" , i ) ;

      // Write date processorname
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorname" , i ) ;

      // Write date processorserialnumber
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorserialnumber" , i ) ;

      // Write date processorversion
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorversion" , i ) ;

      // Write date processorisaset
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorisaset" , i ) ;

      // Write date processorisalevel
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorisalevel" , i ) ;

      // Write date processorisaextension1
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorisaextension1" , i ) ;

      // Write date processorisaextension2
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorisaextension2" , i ) ;

      // Write date processorisaextension3
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorisaextension3" , i ) ;

      // Write date processorclockrate
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorclockrate" , i ) ;

      // Write date instructionlength
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "instructionlength" , i ) ;

      // Write date processorwordlength
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "processorwordlength" , i ) ;

      // Write date instructionissue
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "instructionissue" , i ) ;

      // Write date integerunits
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "integerunits" , i ) ;

      // Write date floatingpointunits
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "floatingpointunits" , i ) ;

      // Write date loadstoreunits
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "loadstoreunits" , i ) ;

      // Write date level1icachesize
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level1icachesize" , i ) ;

      // Write date level1icachetype
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level1icachetype" , i ) ;

      // Write date level1dcachesize
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level1dcachesize" , i ) ;

      // Write date level1dcachetype
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level1dcachetype" , i ) ;

      // Write date level2cachesize
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level2cachesize" , i ) ;

      // Write date level2cachetype
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level2cachetype" , i ) ;

      // Write date level2cachelocation
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level2cachelocation" , i ) ;

      // Write date level3cachesize
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level3cachesize" , i ) ;

      // Write date level3cachetype
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level3cachetype" , i ) ;

      // Write date level3cachelocation
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "level3cachelocation" , i ) ;

      // Write date prefetching
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "prefetching" , i ) ;

      // Write date preloading
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "preloading" , i ) ;

      // Write date backsideclockrate
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "backsideclockrate" , i ) ;

      // Write date technologylevel
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "technologylevel" , i ) ;

      // Write date memorychiptype
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "memorychiptype" , i ) ;

      // Write date memorybustype
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "memorybustype" , i ) ;

      // Write date memorybusclockrate
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "memorybusclockrate" , i ) ;

      // Write date memorybusclockrate
      for ( i = 0 ; i < iCount ; i++ )
         searchAndWriteDisplayInfo( strOutput , Graphs[ i ].getFileContent() ,
                                    "memorybusclockrate" , i ) ;

      strOutput.append( "endofdisplay\n" ) ;
      System.err.println( "Writing the display information... Finished!" ) ;
      strOutput.append(
          "#########################################################\n" ) ;

      //
      // Write the indentifier string information
      //
      System.err.println(
          "Writing the indentifier string information..." ) ;
      strOutput.append( "beginofidentifierstrings\n" ) ;
      strOutput.append(
          "# Ersetzung der identifiers durch Strings in den Menu-Windows\n" ) ;

      // Write the replacement strings
      for ( i = 0 ; i < iCount ; i++ )
      {
         strOutput.append( "hostname-Function-" + i + "=\"Hostname\"\n" ) ;
         strOutput.append( "nodename-Function-" + i + "=\"Nodename\"\n" ) ;
         strOutput.append( "mainboardmanufacturer-Function-" + i +
                           "=\"Manufacturer of  mainboard\"\n" ) ;
         strOutput.append( "mainboardtype-Function-" + i +
                           "=\"Mainboard type\"\n" ) ;
         strOutput.append( "mainboardchipset-Function-" + i +
                           "=\"Mainboard Chipset\"\n" ) ;
         strOutput.append( "processorname-Function-" + i +
                           "=\"Processor name\"\n" ) ;
         strOutput.append( "processorserialnumber-Function-" + i +
                           "=\"Processor Serial No.\"\n" ) ;
         strOutput.append( "processorversion-Function-" + i +
                           "=\"Processor Version\"\n" ) ;
         strOutput.append( "processorisaset-Function-" + i +
                           "=\"Instruction Set Architecture\"\n" ) ;
         strOutput.append( "processorisalevel-Function-" + i +
                           "=\"ISA Level\"\n" ) ;
         strOutput.append( "processorisaextension1-Function-" + i +
                           "=\"ISA Extension\"\n" ) ;
         strOutput.append( "processorisaextension2-Function-" + i +
                           "=\"ISA Extension\"\n" ) ;
         strOutput.append( "processorisaextension3-Function-" + i +
                           "=\"ISA Extension\"\n" ) ;
         strOutput.append( "processorclockrate-Function-" + i +
                           "=\"Processor Clock Rate\"\n" ) ;
         strOutput.append( "instructionlength-Function-" + i +
                           "=\"Instruction Length\"\n" ) ;
         strOutput.append( "processorwordlength-Function-" + i +
                           "=\"Processor Word Length\"\n" ) ;
         strOutput.append( "instructionissue-Function-" + i +
                           "=\"Instruction Issue\"\n" ) ;
         strOutput.append( "integerunits-Function-" + i +
                           "=\"Number of Integer Units\"\n" ) ;
         strOutput.append( "floatingpointunits-Function-" + i +
                           "=\"Number of Floating Point Units\"\n" ) ;
         strOutput.append( "loadstoreunits-Function-" + i +
                           "=\"Number of Load/Store Units\"\n" ) ;
         strOutput.append( "level1icachesize-Function-" + i +
                           "=\"L1 Instruction Cache Size\"\n" ) ;
         strOutput.append( "level1icachetype-Function-" + i +
                           "=\"L1 Instruction Cache Type\"\n" ) ;
         strOutput.append( "level1dcachesize-Function-" + i +
                           "=\"L1 Data Cache Size\"\n" ) ;
         strOutput.append( "level1dcachetype-Function-" + i +
                           "=\"L1 Data Cache Type\"\n" ) ;
         strOutput.append( "level2cachesize-Function-" + i +
                           "=\"L2 Data Cache Size\"\n" ) ;
         strOutput.append( "level2cachetype-Function-" + i +
                           "=\"L2 Cache Type\"\n" ) ;
         strOutput.append( "level2cachelocation-Function-" + i +
                           "=\"L2 Cache Location\"\n" ) ;
         strOutput.append( "level3cachesize-Function-" + i +
                           "=\"L3 Data Cache Size\"\n" ) ;
         strOutput.append( "level3cachetype-Function-" + i +
                           "=\"L3 Cache Type\"\n" ) ;
         strOutput.append( "level3cachelocation-Function-" + i +
                           "=\"L3 Cache Location\"\n" ) ;
         strOutput.append( "prefetching-Function-" + i + "=\"Prefetching\"\n" ) ;
         strOutput.append( "preloading-Function-" + i + "=\"Preloading\"\n" ) ;
         strOutput.append( "backsideclockrate-Function-" + i +
                           "=\"Clock Rate of the Backside Bus\"\n" ) ;
         strOutput.append( "technologylevel-Function-" + i +
                           "=\"Technology Level\"\n" ) ;
         strOutput.append( "memorychiptype-Function-" + i +
                           "=\"Memory Chip Type\"\n" ) ;
         strOutput.append( "memorybustype-Function-" + i +
                           "=\"Memory Bus Type\"\n" ) ;
         strOutput.append( "memorybusclockrate-Function-" + i +
                           "=\"Memory Bus Clock Rate\"\n" ) ;
      }

      strOutput.append( "endofidentifierstrings\n" ) ;
      System.err.println(
          "Writing the indentifier string information... Finished!" ) ;
      strOutput.append(
          "#########################################################\n" ) ;

      //
      // Write the data
      //
      System.err.println( "Writing the data..." ) ;
      strOutput.append( "beginofdata\n" ) ;

      // Get the x-values
      double[] Values = Graphs[ 0 ].getXValues() ;
      for ( int k = 0 ; k < Values.length ; k++ )
      {
         // Write the x-value
         strOutput.append( Values[ k ] ) ;
         // Write the y-values
         for ( i = 0 ; i < iCount ; i++ )
         {
            strOutput.append( "\t" ) ;
            strOutput.append( Graphs[ i ].getYValues()[ k ] ) ;
         }
         strOutput.append( "\n" ) ;
      }

      strOutput.append( "endofdata\n" ) ;
      System.err.println( "Writing the data... Finished!" ) ;

      //
      // Finally... Write the data!
      //
      try
      {
         outputStream.write( strOutput.toString() ) ;
         outputStream.flush() ;
         outputStream.close() ;
      }
      catch ( IOException exception )
      {
         System.err.println(
             "Error while writing data to file! No rights?" ) ;
         return ;
      }
   }
*/
   private plot.BIGResultMixer mixer = null ;
   public void setMixer( plot.BIGResultMixer mixer )
   {
      this.mixer = mixer ;
   }

   public plot.BIGResultMixer getMixer()
   {
      return this.mixer ;
   }

}
