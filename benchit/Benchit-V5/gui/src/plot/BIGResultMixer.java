package plot ;

/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGResultMixer.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.34 $
 *  $Date: 2007/07/03 10:50:43 $
 *
 ******************************************************************************
 */
import gui.BIGResultTree;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import system.BIGInterface;
import system.BIGParserException;
import system.BIGStrings;
import conn.Graph;

public class BIGResultMixer extends BIGPlotable
{
   // contains the data
   //protected GraphData gd = new GraphData() ;
   protected JPanel plotPanel = null ;
   protected JPanel dummyPanel ;
   protected BIGResultTree resultTree = null ;
   protected DefaultMutableTreeNode node = null ;
   protected Dimension screenSize ;
   protected String lastPath = null ;
   //----
   protected Hashtable namesAndGraphs = new Hashtable(); 
   //----
   
   /**
    * constructor
    * @param plotPanel the panel, the graphical result will be shown on
    * @param textPanel the panel, which will contain nothing, but remove existing information
    * @param title the title of this new ResultMixer
    */
   public BIGResultMixer( JPanel plotPanel , JPanel textPanel , String title )
   {
      super.initFonts( null ) ;
      screenSize = Toolkit.getDefaultToolkit().getScreenSize() ;
      dummyPanel = textPanel ;
      this.title = title ;
      node = new DefaultMutableTreeNode( this ) ;
      this.plotPanel = plotPanel ;
   }

   /**
    * gets the x-Axis-Name from bit-File f
    * @param f File the bit-File
    * @throws FileNotFoundException if File does not exist
    * @throws IOException if sth. happened while reading the file
    * @return String the xaxisname
    * @see getXAxisName(BIGStrings bs)
    */
   public String getXAxisName( File f )
       throws FileNotFoundException , IOException
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      return this.getXAxisName( bs ) ;
   }

   /**
    * returns the x-Axis-Name from a File, stored in BIGStrings
    * @param bs BIGStrings the content of the bit-File
    * @return String the x-Axis-Name
    */
   public String getXAxisName( BIGStrings bs )
   {
      String line = bs.get( bs.find( "xaxistext" ) ) ;
      line = line.substring( line.indexOf( "=" ) + 2 , line.length() - 1 ) ;
      return line ;
   }

   /**
    *
    * @param f File
    * @throws FileNotFoundException if File does not exist
    * @throws IOException if sth. happened while reading the file
    * @return String[]
    */
   public String[] getYAxisNames( File f )
       throws FileNotFoundException , IOException
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      return this.getYAxisNames( bs ) ;
   }

   public int getYAxisCount( File f )
       throws FileNotFoundException , IOException
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      return this.getYAxisCount( bs ) ;
   }

   public int getYAxisCount( BIGStrings bs )
   {
      int foundAt = 0 ;
      int findWhichNumber = 1 ;
      do
      {
         foundAt = bs.find( "y" + findWhichNumber + "axistext" ) ;
         findWhichNumber++ ;
      }
      while ( foundAt != -1 ) ;
      return findWhichNumber - 2 ;
   }

   public void cleanUp()
   {
      //this.gd=new GraphData();
      this.plot = null ;
      this.plotPanel = new JPanel() ;
   }

   public String[] getYAxisNames( BIGStrings bs )
   {
      String[] textField = bs.toArray() ;
      String returnField[] = new String[getYAxisCount( bs ) ] ;
      int startYaxisTexts = bs.find( "y1axistext" ) ;
      int endYaxisTexts = bs.find( "numfunctions" ) ;
      int index = -1 ;
      for ( int i = 0 ; i < returnField.length ; i++ )
      {
         for ( int j = startYaxisTexts ; j < endYaxisTexts ; j++ )
         {
            index = textField[ j ].indexOf( "y" + ( i + 1 ) + "axistext" ) ;
            if ( index != -1 )
            {
               returnField[ i ] = textField[ j ].substring( textField[ j ].
                   indexOf( "=" ) + 2 , textField[ j ].length() - 1 ) ;
            }

         }
      }
      return returnField ;
   }

   public int getNumberOfFunctions()
   {
      int size = 0 ;
      for ( int i = 0 ; i < this.data.length ; i++ )
         size = size + data[ i ].getSeriesCount() ;
      return size ;
   }

   public void clearData()
   {
      //this.gd = new GraphData() ;
      this.title = "Mixer title" ;
      this.node = new DefaultMutableTreeNode( this ) ;
   }

   public void removeFunction( String fileName , String functionLegend )
   {
      this.removeSubNode( fileName + "-" + functionLegend ) ;
   }

   public void addFunctionsFromFile( File f , String[] functions )
       throws FileNotFoundException , IOException , Exception
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      this.addFunctionsFromFile( bs , functions , f.getName() ) ;
   }

   public void addFunctionsFromFile( BIGStrings bs , String[] functions ,
                                     String name )
       throws Exception
   {
      //long time=System.currentTimeMillis();
      double[] xData = this.getXData( bs ) ;
      //time=System.currentTimeMillis()-time;
      //System.err.println("XData:"+time);
      for ( int i = 0 ; i < functions.length ; i++ )
      {
         //time=System.currentTimeMillis();
         Graph g = new Graph() ;
         //time=System.currentTimeMillis()-time;
         //System.err.println("new Graph:"+time);
         //time=System.currentTimeMillis();
         g.setGraphName( name + "-" + functions[ i ] ) ;
         //time=System.currentTimeMillis()-time;
         //System.err.println("GraphName:"+time);
         //time=System.currentTimeMillis();
         g.setXAxisText( this.getXAxisName( bs ) ) ;
         //time=System.currentTimeMillis()-time;
         //System.err.println("XAxisText:"+time);
         //System.err.println("XAxisText:"+g.getXAxisText());
         //time=System.currentTimeMillis();
         g.setYAxisText( this.getYAxisNames( bs )[ this.getFunctionIndex( bs ,
             functions[ i ] ) - 1 ] ) ;
         //System.err.println("XAxisText:"+g.getYAxisText());
         //time=System.currentTimeMillis()-time;
         //System.err.println("YAxisText:"+time);
         //time=System.currentTimeMillis();
         g.setPoints( xData ,
                      this.getFunctionData( bs ,
                                            this.getFunctionIndex( bs ,
             functions[ i ] ) ) ) ;
         //time=System.currentTimeMillis()-time;
         //System.err.println("Set Points:"+time);
         //time=System.currentTimeMillis();
         /*System.err.println(
             (
             (this.getFunctionData( bs ,
                                   this.getFunctionIndex( bs ,
             functions[ i ] ) ))[ 0 ])
             ) ;
                   System.err.println(g.getYValues()[0]);

                   System.err.println("--------------------------");
                   for (int ii=0;ii<g.getYValues().length;ii++)
                   System.err.println(g.getYValues()[ii]);
                   System.err.println("--------------------------");
          */
         //this.gd.addGraph( g ) ;
         this.addGraph( g ) ; /*
         double[][] vals=new double[2][g.getNumberOfPoints()];
                                   vals[0]=g.getXValues();
                                   vals[1]=g.getYValues();
                  this.addFunction(vals,g.getYAxisText(),g.getGraphName());
         this.node.add( new DefaultMutableTreeNode( g ) ) ;
         ( ( DefaultTreeModel ) resultTree.getModel() ).reload( this.node ) ;
                                    resultTree.showBRM(BIGResultMixer.this);*/
        //time=System.currentTimeMillis()-time;
        //System.err.println("ADDGraph:"+time);
      }
   }

   public void addWholeFile( File f )
       throws FileNotFoundException , IOException , Exception
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      this.addWholeFile( bs , f.getName() ) ;

   }

   public void addWholeFile( BIGStrings bs , String name )
       throws Exception
   {
      String[] legends = this.getLegends( bs ) ;
      this.addFunctionsFromFile( bs , legends , name ) ;
   }

   public void addGraph( Graph g )
   {
	   //-----
	   DefaultMutableTreeNode dmtnode;
	   //-----
	   
      double[][] vals = new double[2 ][ g.getNumberOfPoints() ] ;
      vals[ 0 ] = g.getXValues() ;
      vals[ 1 ] = g.getYValues() ;
      if ( !this.addFunctionFromGraph( vals , g.getYAxisText() , g ) )
         return ;
      
      if ( ( this.xAxisText == null ) || ( this.xAxisText.equals( "" ) ) )
         this.xAxisText = g.getXAxisText() ;
      //this.node.add( new DefaultMutableTreeNode( g ) ) ;
      //-----
      dmtnode = new DefaultMutableTreeNode( g );
      this.node.add( dmtnode ) ;
      namesAndGraphs.put(g.getGraphName(), dmtnode);
      //-----
      
      if ( this.plot == null )
         this.plot = new BIGPlot( this ) ;
      this.plot.setupConfig() ;
      // System.out.println( this.plot.toString() );
      
      if ( resultTree != null )
      {
         ( ( DefaultTreeModel ) resultTree.getModel() ).reload( this.node ) ;
         resultTree.showBRM( BIGResultMixer.this ) ;
      }
   }
   
   //-----
   
   public void changeName(int whichDataSet, int index, String newName) {
	   //System.out.println("BIGResultMixer.changeName( " + whichDataSet + ", " + index + ", " + newName + ")" );
	   String oldNames[] = this.data[this.displayedDataSets[ whichDataSet ]].getNames();
	   String oldName = oldNames[ index ];
	   graphNameChanged( oldName, newName );
	   oldNames[ index ] = newName;
   }
   
   public void graphNameChanged(String oldGraphName, String newGraphName) {
	   //System.out.println("BIGResultMixer.graphNameChanged( " + oldGraphName + ", " + newGraphName + ")" );
	   DefaultMutableTreeNode node = null;
	   Graph graph = null;
	   if ( namesAndGraphs.containsKey( oldGraphName ) ) {
		   node = (DefaultMutableTreeNode) namesAndGraphs.get( oldGraphName );
		   namesAndGraphs.remove( oldGraphName );
		   graph = (Graph) node.getUserObject();
		   graph.setGraphName( newGraphName );
		   namesAndGraphs.put(newGraphName, node);
		   ( ( DefaultTreeModel ) resultTree.getModel() ).reload( this.node ) ;
	         resultTree.showBRM( BIGResultMixer.this ) ;
		   
	   }
   }
   //------

   public String[] getLegends( File f )
       throws FileNotFoundException , IOException
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      return this.getLegends( bs ) ;
   }

   public String[] getLegends( BIGStrings bs )
   {
      String[] legends = new String[this.getYAxisNames( bs ).length ] ;
      for ( int i = 0 ; i < legends.length ; i++ )
      {
         legends[ i ] = bs.get( bs.find( "tlegendfunction" + ( i + 1 ) + "=" ) ) ;
         legends[ i ] = legends[ i ].substring( legends[ i ].indexOf( "=" ) + 2 ,
                                                legends[ i ].length() - 1 ) ;
      }
      return legends ;
   }

   public double[] getXData( File f )
       throws FileNotFoundException , IOException
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      return this.getXData( bs ) ;
   }

   public double[] getXData( BIGStrings bs )
   {
      String[] file = bs.toArray() ;
      int dataStartsAt = bs.find( "beginofdata" ) + 1 ;
      int dataEndsAt = bs.find( "endofdata" ) ;
      double[] xData = new double[dataEndsAt - dataStartsAt ] ;
      for ( int i = dataStartsAt ; i < dataEndsAt ; i++ )
      {
         try
         {
            xData[ i -
                dataStartsAt ] = ( new Double( file[ i ].substring( 0 ,
                file[ i ].indexOf( "\t" ) ) ) ).doubleValue() ;
         }
         catch ( Exception e )
         {
            System.err.println( "Error:" + file[ i ] ) ;
         }
      }
      return xData ;
   }

   public int getFunctionIndex( File f , String functionName )
       throws FileNotFoundException , IOException
   {
      BIGStrings bs = new BIGStrings() ;
      bs.readFromFile( f.getAbsolutePath() ) ;
      return this.getFunctionIndex( bs , functionName ) ;
   }

   public int getFunctionIndex( BIGStrings bs , String functionName )
   {
      int lineIndex = bs.find( "=\"" + functionName + "\"" ) ;
      String line = bs.get( lineIndex ) ;
      while ( ( !line.startsWith( "tlegendfunction" ) ) )
      {
         lineIndex = bs.find( "=\"" + functionName + "\"" , lineIndex + 1 ) ;
         if ( lineIndex < 0 )
         {
            System.err.println( "wrong name:" + functionName ) ;
            return -1 ;
         }
         line = bs.get( lineIndex ) ;
      }
      int index = line.indexOf( "=" ) ;
      // maybe more then 9
      try
      {
         return ( new Integer( line.substring( index - 2 , index ) ) ).
             intValue() ;
      }
      catch ( NumberFormatException ex )
      {
         try
         {
            return ( new Integer( line.substring( index - 1 , index ) ) ).
                intValue() ;
         }
         catch ( NumberFormatException ex2 )
         {
            System.err.println( "wrong name:" + functionName ) ;
            return -1 ;
         }
      }
   }

   /**
    * returns the data for function n
    * @param bs BIGStrings the file
    * @param functionIndex int >=1
    * @return double[] values
    */
   public double[] getFunctionData( BIGStrings bs , int functionIndex )
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

      //long time=System.currentTimeMillis();
      String[] file = bs.toArray() ;
      int dataStartsAt = bs.find( "beginofdata" ) + 1 ; // 1 becaus of the file,1 becaus of the data structure

      int dataEndsAt = bs.find( "endofdata" ) ;
      double[] yData = new double[dataEndsAt - dataStartsAt ] ;
      int startPosition = 0 ;
      for ( int i = dataStartsAt ; i < dataEndsAt ; i++ )
      {
         int end = 0 ;
         // search is needed, if the line doesnt end with \t, but \n
         String search = "\t" ;
         StringBuffer sb = new StringBuffer( file[ i ] ) ;
         //System.err.println("Line:"+file[i]);
         for ( int j = 0 ; j < functionIndex ; j++ )
         {
            startPosition = ( sb.indexOf( "\t" , startPosition ) ) + 1 ;

            //System.err.println("StartPosition:"+startPosition);
         }
         //System.err.println("Final StartPosition:"+startPosition);
         if ( sb.indexOf( "\t" , startPosition ) < 0 )
         {
            end = sb.toString().length() ;
         }
         else
         {
            end = sb.indexOf( "\t" , startPosition ) ;
         }
         try
         {
            yData[ i -
                dataStartsAt ] = ( new Double( sb.substring( startPosition ,
                end ) ) ).doubleValue() ;
         }
         catch ( Exception e )
         {
            //System.err.println( "Error in "+sb ) ;
            System.err.println( "The file seems invalid in result-line " + i ) ;
            yData[ i - dataStartsAt ] = errorDou ;
         }
         startPosition = 0 ;
      }
      //System.err.println("getFunctionData:"+(System.currentTimeMillis()-time));
      return yData ;
   }

   public JTabbedPane getPlot()
   {
      if ( ( data == null ) || ( data.length == 0 ) )
      {
         plot = null ;
         return new JTabbedPane() ;
      }
      if ( plot == null )
      {
         this.plot = new plot.BIGPlot( this ) ;
      }
      if ( plot != null )
      {
         if ( plot.chartPanel == null )
         {
            this.plot = new plot.BIGPlot( this ) ;
         }
      }
       return ( JTabbedPane )this.plot.getDisplayPanels()[ 0 ] ;
   }

   private BIGPlot plot = null ;

   /**
    * starts a Dialog, that asks which Functions shall be combined to a new
    * @param jf the jframe, which calls and will be set to background when an messageBox appears
    */
   public void combineFunctions( JFrame jf )
   {
      final JDialog jd = new JDialog( jf ) ;
      final String functionName = JOptionPane.showInputDialog(
          "set the Name for the new function" ) ;
      if ( functionName == null )return ;
      final String yAxisName = JOptionPane.showInputDialog(
          "set the y-Axis-Name for the function", this.yAxisText[displayedDataSets[0]] ) ;
      if ( yAxisName == null )return ;
      JPanel jp = new JPanel() ;
      jp.setLayout( new java.awt.GridLayout( 4 , 1 ) ) ;
      final String[] jComboBox1And3Entries = new String[this.getTotalFunctions() ] ;
      int nextEntry = 0 ;
      for ( int i = 0 ; i < this.data.length ; i++ )
      {
         for ( int j = 0 ; j < data[ i ].getSeriesCount() ; j++ )
         {
            jComboBox1And3Entries[ nextEntry ] = this.data[ i ].getSeriesName(
                j ) ;
            nextEntry++ ;
         }
      }
      String[] jComboBox2Entries = new String[8 ] ;
      jComboBox2Entries[ 0 ] = "/" ;
      jComboBox2Entries[ 1 ] = "*" ;
      jComboBox2Entries[ 2 ] = "+" ;
      jComboBox2Entries[ 3 ] = "-" ;
      jComboBox2Entries[ 4 ] = "max(y1,y2)" ;
      jComboBox2Entries[ 5 ] = "min(y1,y2)" ;
      jComboBox2Entries[ 6 ] = "max(all)" ;
      jComboBox2Entries[ 7 ] = "min(all)" ;

      final JComboBox combo1 = new JComboBox( jComboBox1And3Entries ) ;
      final JComboBox combo2 = new JComboBox( jComboBox2Entries ) ;
      final JComboBox combo3 = new JComboBox( jComboBox1And3Entries ) ;
      combo1.setEditable( true ) ;
      combo3.setEditable( true ) ;
      JButton okayButton = new JButton( "Okay" ) ;
      okayButton.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            double error = -1.0E99 ;
            try
            {
               error = BIGInterface.getInstance().
                   getBIGConfigFileParser().intCheckOut( "errorInt" ) ;
            }
            catch ( Exception ex1 )
            {
            }

            jd.setVisible( false ) ;
            boolean combo1Acceptable = false ;
            boolean combo1Number = true ;
            boolean combo3Acceptable = false ;
            boolean combo3Number = true ;
            double combo1Double = -1.0 ;
            double combo3Double = -1.0 ;
            double[][] allYData = new double[combo1.getItemCount() ][] ;
            int whichYData = 0 ;
            for ( int i = 0 ; i < data.length ; i++ )
            {
               for ( int j = 1 ; j < data[ i ].getValues().length ; j++ )
               {
                  allYData[ whichYData ] = data[ i ].getValues()[ j ] ;
                  whichYData++ ;
               }
            }
            if ( combo2.getSelectedIndex() > 5 )
            {
               Graph g = new Graph() ;
               g.setGraphName( functionName ) ;
               g.setXAxisText( xAxisText ) ;
               g.setXValues( data[ 0 ].getValues()[ 0 ] ) ;
               g.setGraphName( functionName ) ;
               g.setYAxisText( yAxisName ) ;
               double[] gYValues = new double[data[0 ].getValues()[ 0 ].
                   length ] ;
               if ( combo2.getSelectedIndex() == 6 )
               {
                  double max = Double.NEGATIVE_INFINITY ;
                  for ( int j = 0 ; j < allYData[ 0 ].length ; j++ )
                  {
                     max = Double.NEGATIVE_INFINITY ;

                     for ( int i = 0 ; i < allYData.length ; i++ )
                        if ( max < allYData[ i ][ j ] )
                           max = allYData[ i ][ j ] ;
                     for ( int i = 0 ; i < allYData.length ; i++ )
                        if ( allYData[ i ][ j ] < 0 )
                           max = error ;
                     gYValues[ j ] = max ;
                  }
               }
               else
               {
                  if ( combo2.getSelectedIndex() == 7 )
                  {
                     double min = Double.POSITIVE_INFINITY ;
                     for ( int j = 0 ; j < allYData[ 0 ].length ; j++ )
                     {
                        min = Double.POSITIVE_INFINITY ;
                        for ( int i = 0 ; i < allYData.length ; i++ )
                           if ( min > allYData[ i ][ j ] )
                              min = allYData[ i ][ j ] ;
                        for ( int i = 0 ; i < allYData.length ; i++ )
                           if ( allYData[ i ][ j ] < 0 )
                              min = error ;

                        gYValues[ j ] = min ;
                     }

                  }

               }
               g.setYValues( gYValues ) ;
               addGraph( g ) ;
               /*if ( BIGResultMixer.this.resultTree != null )
                               {
                  BIGResultMixer.this.resultTree.updateBRM() ;
                               }

                               if ( BIGResultMixer.this.resultTree != null )
                               {
                  BIGResultMixer.this.resultTree.showBRM() ;
                               }*/
               return ;

            }
            try
            {
               combo1Double = ( new Double( ( String ) combo1.getSelectedItem() ) ).
                   doubleValue() ;
               combo1Acceptable = true ;
            }
            catch ( NumberFormatException ex )
            {
               combo1Number = false ;
            }
            try
            {
               combo3Double = ( new Double( ( String ) combo3.getSelectedItem() ) ).
                   doubleValue() ;
               combo3Acceptable = true ;
            }
            catch ( NumberFormatException ex )
            {
               combo3Number = false ;
            }
            if ( !combo1Number )
               for ( int i = 0 ; i < jComboBox1And3Entries.length ; i++ )
                  if ( combo1.getSelectedItem() == jComboBox1And3Entries[ i ] )
                     combo1Acceptable = true ;
            if ( !combo3Number )
               for ( int i = 0 ; i < jComboBox1And3Entries.length ; i++ )
                  if ( combo3.getSelectedItem() == jComboBox1And3Entries[ i ] )
                     combo3Acceptable = true ;
            if ( ( !combo1Acceptable ) || ( !combo3Acceptable ) )
            {
               System.err.println( "Inacceptable input" ) ;
               return ;
            }
            Graph g = new Graph() ;
            g.setGraphName( functionName ) ;
            g.setXAxisText( xAxisText ) ;
            g.setXValues( data[ 0 ].getValues()[ 0 ] ) ;
            g.setGraphName( functionName ) ;
            g.setYAxisText( yAxisName ) ;
            double[] combineGraph1 = new double[g.getXValues().length ] ;
            double[] combineGraph2 = new double[g.getXValues().length ] ;

            if ( combo1Number )
               for ( int i = 0 ; i < combineGraph1.length ; i++ )
                  combineGraph1[ i ] = combo1Double ;
            else
            {
               combineGraph1 = allYData[ combo1.getSelectedIndex() ] ;
            }
            if ( combo3Number )
               for ( int i = 0 ; i < combineGraph2.length ; i++ )
               {
                  combineGraph2[ i ] = combo3Double ;
               }
            else
            {
               combineGraph2 = allYData[ combo3.getSelectedIndex() ] ;
            }
            double[] gYValues = new double[combineGraph1.length ] ;
            for ( int i = 0 ; i < combineGraph1.length ; i++ )
            {
               if ( ( combineGraph1[ i ] < 0 ) || ( combineGraph2[ i ] < 0 ) )
               {
                  gYValues[ i ] = error ;
               }
               else
               {
                  switch ( combo2.getSelectedIndex() )
                  {
                     case 0:

                        //System.err.println(combineGraph1[i]+"/"+combineGraph2[ i ]);
                        gYValues[ i ] = combineGraph1[ i ] / combineGraph2[ i ] ;
                        break ;
                     case 1:
                        gYValues[ i ] = combineGraph1[ i ] * combineGraph2[ i ] ;
                        break ;
                     case 2:
                        gYValues[ i ] = combineGraph1[ i ] + combineGraph2[ i ] ;
                        break ;
                     case 3:
                        gYValues[ i ] = combineGraph1[ i ] - combineGraph2[ i ] ;
                        break ;
                     case 4:
                        gYValues[ i ] = combineGraph1[ i ] ;
                        if ( gYValues[ i ] < combineGraph2[ i ] )
                           gYValues[ i ] = combineGraph2[ i ] ;
                        break ;
                     case 5:
                        gYValues[ i ] = combineGraph1[ i ] ;
                        if ( gYValues[ i ] > combineGraph2[ i ] )
                           gYValues[ i ] = combineGraph2[ i ] ;
                        break ;

                     default:
                        gYValues[ i ] = -1 ;
                  }
               }
               //System.err.println(combineGraph1[i]+"\t"+combineGraph2[ i ]+"\t"+gYValues[ i ]);
            }
            g.setYValues( gYValues ) ;
            addGraph( g ) ;
            /*  if ( BIGResultMixer.this.resultTree != null )
              {
                 BIGResultMixer.this.resultTree.updateBRM() ;
              }

              if ( BIGResultMixer.this.resultTree != null )
              {
                 BIGResultMixer.this.resultTree.showBRM() ;
              }*/

         }
      } ) ;
      JButton cancelButton = new JButton( "Cancel" ) ;
      cancelButton.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent ae )
         {
            jd.setVisible( false ) ;
         }
      } ) ;
      jp.add( combo1 ) ;
      jp.add( combo2 ) ;
      jp.add( combo3 ) ;
      JPanel jp2 = new JPanel() ;
      jp2.add( okayButton , java.awt.BorderLayout.WEST ) ;
      jp2.add( cancelButton , java.awt.BorderLayout.EAST ) ;
      jp.add( jp2 ) ;
      jd.setContentPane( jp ) ;
      jd.pack() ;
      Dimension frameSize = jd.getSize() ;
      if ( frameSize.height > screenSize.height )
      {
         frameSize.height = screenSize.height ;
      }
      if ( frameSize.width > screenSize.width )
      {
         frameSize.width = screenSize.width ;
      }
      jd.setLocation( ( screenSize.width - frameSize.width ) / 2 ,
                      ( screenSize.height - frameSize.height ) / 2 ) ;
      jd.setVisible( true ) ;
   }

   /**
    * show the dialog for adding a new function
    * @param jf JFrame on this the JDialog will be shown
    */
   public void showAddDialog( JFrame jf )
   {
      final JFileChooser jfc ;
      if ( lastPath == null )
         jfc = new JFileChooser( BIGInterface.getInstance().
                                 getBenchItPath() + File.separator +
                                 "output" ) ;
      else
         jfc = new JFileChooser( lastPath ) ;

      jfc.setFileFilter( new javax.swing.filechooser.FileFilter()
      {
         public boolean accept( File f )
         {
            if ( f.isDirectory() )return true ;
            String fileName = f.getName() ;
            if ( fileName.indexOf( "." ) < 0 )
               return false ;
            String extension = fileName.substring( fileName.lastIndexOf( "." ) +
                1 , fileName.length() ) ;
            if ( extension.equals( "bit" ) )
               return true ;
            return false ;
         }

         public String getDescription()
         {
            return "BenchIT ResultFiles *.bit" ;
         }

      } ) ;
      int select = jfc.showOpenDialog( jf ) ;
      if ( select == JFileChooser.APPROVE_OPTION )
      {
         showSelectFunctionsDialog( jfc.getSelectedFile() ) ;

      }
      else
      {
         return ;
      }
   }

   public void showSelectFunctionsDialog( File file )
       throws HeadlessException
   {

      lastPath = file.getPath() ;
      JList jl ;
      final BIGStrings bs = new BIGStrings() ;
      try
      {
         bs.readFromFile( file.getAbsolutePath() ) ;
      }
      catch ( FileNotFoundException ex )
      {
         System.err.println( "Couldn't find File" ) ;
         return ;
      }
      catch ( IOException ex )
      {
         System.err.println( "Couldn't read File" ) ;
         return ;
      }
      if ( this.getLegends( bs ).length == 1 )
      {
         try
         {
            addFunctionsFromFile( bs , this.getLegends( bs ) ,
                                  file.getName() ) ;
         }
         catch ( FileNotFoundException ex )
         {
            System.err.println( "Couldn't find File" ) ;
            return ;
         }
         catch ( IOException ex )
         {
            System.err.println( "Couldn't read File" ) ;
            return ;
         }
         catch ( Exception ex )
         {
            ex.printStackTrace() ;
            System.err.println( "Couldn't add legend" ) ;
            return ;
         }

         if ( BIGResultMixer.this.resultTree != null )
         {
            BIGResultMixer.this.resultTree.updateResultTree( null ) ;
            BIGResultMixer.this.resultTree.showBRM( BIGResultMixer.this ) ;
         }
         return ;
      }
      jl = new JList( this.getLegends( bs ) ) ;
      Object[] o = new Object[3 ] ;
      o[ 0 ] = file.getName() ;
      o[ 1 ] = jl ;
      int value = JOptionPane.showConfirmDialog( null , o ,
                                                 "select functions" ,
                                                 JOptionPane.OK_CANCEL_OPTION ,
                                                 JOptionPane.QUESTION_MESSAGE ) ;
      if ( value == JOptionPane.CANCEL_OPTION )
         return ;
      else
      {
         String[] legendNames = new String[jl.getSelectedValues().length ] ;
         for ( int i = 0 ; i < legendNames.length ; i++ )
            legendNames[ i ] = ( String ) jl.getSelectedValues()[ i ] ;
         try
         {
            addFunctionsFromFile( bs , legendNames ,
                                  file.getName() ) ;
         }
         catch ( FileNotFoundException ex )
         {
            System.err.println( "Couldn't find File" ) ;
            return ;
         }
         catch ( IOException ex )
         {
            System.err.println( "Couldn't read File" ) ;
            return ;
         }
         catch ( Exception ex )
         {
            ex.printStackTrace() ;
            System.err.println( "Couldn't add legend" ) ;
            return ;
         }
         if ( BIGResultMixer.this.resultTree != null )
         {
            BIGResultMixer.this.resultTree.showBRM( BIGResultMixer.this ) ;
         }
      }

   }

   /**
    * gets the root
    * @return DefaultMutableTreeNode the root node
    */
   public DefaultMutableTreeNode getTreeNode()
   {
      if ( node == null )
         node = new DefaultMutableTreeNode( this ) ;
      return node ;
   }

   /**
    * sets this.resultTree
    * @param tree BIGResultTree
    */
   public void setResultTree( BIGResultTree tree )
   {
      resultTree = tree ;
   }

   /**
    * gets the popupMenu for a BRM-node
    */
   public JPopupMenu getBIGPopupMenu( final JFrame jf , final Object o )
   {
      final BIGResultMixer mixer = this ;
      // o can be graph or this resultmixer
      // items for the popup menu
      JPopupMenu itemList = new JPopupMenu() ;
      if ( o instanceof BIGResultMixer )
      {
         JMenuItem item = new JMenuItem( "Add function from file" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               showAddDialog( jf ) ;
            }
         } ) ;
         itemList.add( item ) ;

         item = new JMenuItem( "Combine functions" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               combineFunctions( jf ) ;
            }
         } ) ;
         itemList.add( item ) ;

         item = new JMenuItem( "Change title" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               showTitleChangeDialog() ;
            }
         } ) ;
         itemList.add( item ) ;

         item = new JMenuItem( "Remove function" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               showRemoveDialog( jf ) ;
            }
         } ) ;
         itemList.add( item ) ;
         itemList.add( new JSeparator() ) ;
         item = new JMenuItem( "Save" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent evt )
            {
               JFileChooser jfc = new JFileChooser( BIGInterface.getInstance().
                   getLastWorkPath() ) ;
               jfc.setFileSelectionMode( JFileChooser.SAVE_DIALOG ) ;
               jfc.setFileFilter( new javax.swing.filechooser.FileFilter()
               {
                  public String getDescription()
                  {
                     return ".bmf BenchIT-GUI Mixer File" ;
                  }

                  public boolean accept( File f )
                  {
                     if ( f.isDirectory() )
                        return true ;
                     if ( f.getName().endsWith( ".bmf" ) )
                        return true ;
                     return false ;
                  }
               } ) ;

               int ret = jfc.showSaveDialog( jf ) ;
               if ( ret == JFileChooser.CANCEL_OPTION )
                  return ;
               BIGInterface.getInstance().setLastWorkPath( jfc.
                   getCurrentDirectory() ) ;
               if ( !jfc.getSelectedFile().getName().endsWith( ".bmf" ) )
                  jfc.setSelectedFile( new File( jfc.getSelectedFile().
                                                 getAbsolutePath() + ".bmf" ) ) ;
               save( jfc.getSelectedFile() ) ;

            }
         } ) ;
         itemList.add( item ) ;
         item = new JMenuItem( "Load" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent evt )
            {
               JFileChooser jfc = new JFileChooser( BIGInterface.getInstance().
                   getLastWorkPath() ) ;
               jfc.setFileSelectionMode( JFileChooser.OPEN_DIALOG ) ;
               jfc.setFileFilter( new javax.swing.filechooser.FileFilter()
               {
                  public String getDescription()
                  {
                     return ".bmf BenchIT-GUI Mixer File" ;
                  }

                  public boolean accept( File f )
                  {
                     if ( f.isDirectory() )
                        return true ;
                     if ( f.getName().endsWith( ".bmf" ) )
                        return true ;
                     return false ;
                  }
               } ) ;
               int ret = jfc.showOpenDialog( jf ) ;
               if ( ret == JFileChooser.CANCEL_OPTION )
                  return ;
               BIGInterface.getInstance().setLastWorkPath( jfc.
                   getCurrentDirectory() ) ;
               cleanUp() ;
               if (data!=null)
               while ( data.length > 0 )
               {
                  while ( ( data.length > 0 ) &&
                          ( data[ 0 ].getSeriesCount() > 0 ) )
                     removeSubNode( data[ 0 ].getSeriesName( 0 ) ) ;
               }
               plotPanel.removeAll() ;
               java.awt.Component com = getPlot() ;
               if ( com != null )
                  plotPanel.add( com ) ;
               plotPanel.revalidate() ;
               dummyPanel = new JPanel() ;
               dummyPanel.revalidate() ;
               init( jfc.getSelectedFile() ) ;
               if ( BIGResultMixer.this.resultTree != null )
               {
                  BIGResultMixer.this.resultTree.updateBRM( BIGResultMixer.this ) ;
               }

               if ( BIGResultMixer.this.resultTree != null )
               {
                  BIGResultMixer.this.resultTree.updateBRM( BIGResultMixer.this ) ;
               }

            }
         } ) ;
         itemList.add( item ) ;

      }
      else
      {
         JMenuItem item = new JMenuItem( "Remove" ) ;
         item.addActionListener( new ActionListener()
         {
            BIGResultTree tree = null ;
            public void actionPerformed( ActionEvent ae )
            {
               // get the name of the leave
               removeSubNode( o.toString() ) ;
            }
         } ) ;
         itemList.add( item ) ;
         item = new JMenuItem( "Rename" ) ;
         item.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent ae )
            {
               String oldName = o.toString() ;
               String newName = JOptionPane.showInputDialog( "Insert new Name" ,
                   oldName ) ;
               if ( newName == null )return ;
               // get the name of the leave
               BIGResultMixer.this.rename( oldName , newName ) ;
               plotPanel.removeAll() ;
               java.awt.Component com = getPlot() ;
               if ( com != null )
                  plotPanel.add( com ) ;
               plotPanel.revalidate() ;
               dummyPanel = new JPanel() ;
               dummyPanel.revalidate() ;
               if ( BIGResultMixer.this.resultTree != null )
               {
                  BIGResultMixer.this.resultTree.updateBRM( BIGResultMixer.this ) ;
               }
            }
         } ) ;
         itemList.add( item ) ;

      }
      return itemList ;
   }

   private void showRemoveDialog( JFrame jf )
   {
      String[] s = new String[this.getTotalFunctions() ] ;
      int where = 0 ;
      for ( int i = 0 ; i < this.data.length ; i++ )
      {
         for ( int j = 0 ; j < data[ i ].getSeriesCount() ; j++ )
         {
            s[ where ] = data[ i ].getSeriesName( j ) ;
            where++ ;
         }
      }
      JList jl = new JList( s ) ;
      Object o[] = new Object[2 ] ;
      o[ 0 ] = "Please select the functions you'd like to remove" ;
      o[ 1 ] = jl ;
      int value = JOptionPane.showConfirmDialog( null , o ,
                                                 "select functions to remove" ,
                                                 JOptionPane.OK_CANCEL_OPTION ,
                                                 JOptionPane.QUESTION_MESSAGE ) ;
      if ( value == JOptionPane.CANCEL_OPTION )
         return ;

      for ( int i = jl.getSelectedValues().length - 1 ; i >= 0 ; i-- )
      {
         removeSubNode( s[ jl.getSelectedIndices()[ i ] ] ) ;
      }
      System.gc() ;
      plotPanel.removeAll() ;
      java.awt.Component com = getPlot() ;
      if ( com != null )
         plotPanel.add( com ) ;
      plotPanel.revalidate() ;
      dummyPanel = new JPanel() ;
      dummyPanel.revalidate() ;
      if ( BIGResultMixer.this.resultTree != null )
      {
         BIGResultMixer.this.resultTree.updateBRM( this ) ;
      }

      if ( BIGResultMixer.this.resultTree != null )
      {
         BIGResultMixer.this.resultTree.updateBRM( this ) ;
      }

   }

   public void removeSubNode( String s )
   {
      for ( int i = 0 ; i < this.node.getChildCount() ; i++ )
      {
         if ( this.node.getChildAt( i ).toString().equals( s ) )
         {
            this.node.remove( i ) ;
            if ( resultTree != null )
            {
               ( ( DefaultTreeModel ) resultTree.getModel() ).reload( this.node ) ;
            }
            this.removeFunction( s ) ;
            if ( resultTree != null )
            {
               resultTree.showBRM( this ) ;
            }
            return ;
         }
      }
   }

   /**
    * Opens an input dialog where you can change the title of a plot.
    *
    */
   public void showTitleChangeDialog()
   {
	  // stores the result of the input dialog
	  String newPlotTitle = null;
	  if ( plot != null ) {
		  // if the plot exists, display the actual plot tilte as default input
		  newPlotTitle = JOptionPane.showInputDialog( "Set the new Title",
				  this.title ) ;
	  }
	  else {
		  // if the plot doesn't exist show an "empty" input dialog
		  newPlotTitle = JOptionPane.showInputDialog( "Set the new Title" );
	  }
	  
	  if ( newPlotTitle != null ) {
		  // if the input dialog wasn't canceled set the new title
		  this.title = newPlotTitle;
	      if ( plot != null )
	      {
	         plot.chartPanel.getChart().titleChanged( new org.jfree.chart.event.
	                                                  TitleChangeEvent( new org.
	             jfree.chart.TextTitle( this.title ) ) ) ;
	         plot.setupConfig() ;
	         
	         //----------------------------------------------------------------------
	         	// repaint the chart
	            plot.chartPanel.chartChanged( new org.jfree.chart.event.
	                                     ChartChangeEvent(
	                                         plot.chartPanel.getChart() ) ) ;
	            // set the title
	            this.setTitle( newPlotTitle ) ;
	            plot.chartPanel.getChart().getTitle().setFont(this.getFont("title"));
	            // also fonts
	            plot.chartPanel.getChart().getTitle().setText( this.getExtTitle() ) ;
	         //----------------------------------------------------------------------
	            
	         plotPanel.removeAll() ;
	         java.awt.Component com = getPlot() ;
	         if ( com != null )
	         {
	            plotPanel.add( com ) ;
	         }
	         plotPanel.revalidate() ;
	         dummyPanel = new JPanel() ;
	         dummyPanel.revalidate() ;
	      }
	      if ( BIGResultMixer.this.resultTree != null )
	      {
	         BIGResultMixer.this.resultTree.updateBRM( this ) ;
	      }
	  }   
   }

   /*public void load(int i)
       {
      File name=new File(BIGInterface.getInstance().getBenchItPath()+
          File.separator + "gui" + File.separator + "cfg"+
          File.separator + "mixer_"+i);
      if (this.plot==null)
         this.plot=new BIGPlot();
         this.plot.setOtherFile(name);
         this.plot.init();
         this.initGD();
       }
       public void initGD()
       {
      if (this.plot==null)
        {
        }
      if (this.plot.getDataSetY1()==null) return;
      for (int i=0;i<this.plot.getDataSetY1().getNames().length;i++)
      {
         //System.err.println(i+"/"+this.plot.getDataSetY1().getNames().length);
         Graph g= new Graph();
         //System.err.println(this.plot.getDataSetY1().getNames()[i]);
         g.setGraphName(this.plot.getDataSetY1().getNames()[i]);
         g.setXAxisText(this.plot.xAxisTextField.getText());
         g.setYAxisText(this.plot.yAxisTextField.getText());
         g.setXValues(this.plot.getDataSetY1().getValues()[0]);
         g.setYValues(this.plot.getDataSetY1().getValues()[i+1]);
         this.gd.addGraph(g);
         this.node.add(new DefaultMutableTreeNode(g));
      }
      if (this.plot.getDataSetY2()!=null)
      {
      for (int i=0;i<this.plot.getDataSetY2().getNames().length;i++)
      {
         //System.err.println(i);
         Graph g= new Graph();
         g.setGraphName(this.plot.getDataSetY2().getNames()[i]);
         g.setXAxisText(this.plot.xAxisTextField.getText());
         g.setYAxisText(this.plot.yAxisTextField2.getText());
         g.setXValues(this.plot.getDataSetY2().getValues()[0]);
         g.setYValues(this.plot.getDataSetY2().getValues()[i+1]);
         this.gd.addGraph(g);
         this.node.add(new DefaultMutableTreeNode(g));
      }


       }
       //( ( DefaultTreeModel ) resultTree.getModel() ).reload( this.node ) ;
       }

       public void save(int i)
       {
      File name=new File(BIGInterface.getInstance().getBenchItPath()+
          File.separator + "gui" + File.separator + "cfg"+
          File.separator + "mixer_"+i);
       if (this.gd.size()==0)
       {
      System.out.println( "mixer "+i+" not used" ) ;
      return;
       }

      if (plot!=null)
      {
         this.plot.setOtherFile( name ) ;
         this.plot.save();
      }
       else
       {
      System.out.println( "mixer "+i+" not used" ) ;
       }
       }*/
   /**
    * Returns the string representation of this mixer. The returned string
    * consists of the string "Mixer " followed by the title of this mixer. 
    * 
    * @return String string representation of this mixer
    */
   public String toString()
   {
      return "Mixer " + this.title ;
   }

   // NEW
   /**
    * This methode stores all necessary data of this mixer in the specified
    * file.
    * 
    * @param f - file for saving the informations of this mixer
    */
   public void save( File f )
   {
      if ( f == null )
         return ;
      if ( data == null )
      {
         if ( f.exists() )
            f.delete() ;
         return ;
      }
      if ( data.length == 0 )
         if ( f.exists() )
            f.delete() ;

      if ( data.length == 0 )return ;
      StringBuffer sb = new StringBuffer( "#autoGenerated by BIGGUI\n" ) ;
      int whichFunction = 1 ;
      sb.append( "title=" + this.title + "\n" ) ;

      sb.append( "xaxistext=" + this.xAxisText + "\n" ) ;
      sb.append( "xoutmin=" +
                 data[ 0 ].getMinimumDomainValue() + "\n" ) ;
      sb.append( "xoutmax=" +
                 data[ 0 ].getMaximumDomainValue() + "\n" ) ;
      sb.append( "xaxislogbase=" + this.xLog + "\n" ) ;
      sb.append( "xaxisticks=" + this.xTicks + "\n" ) ;
      for ( int i = 0 ; i < data.length ; i++ )
      {
         int pre = data[ i ].getPre() ;
         data[ i ].setPre( 3 ) ;
         for ( int j = 0 ; j < data[ i ].getSeriesCount() ; j++ )
         {
            sb.append( "y" + whichFunction + "axistext=" + this.yAxisText[ i ] +
                       "\n" ) ;
            sb.append( "y" + whichFunction + "outmin=" +
                       data[ i ].getMinimumRangeValue() + "\n" ) ;
            sb.append( "y" + whichFunction + "outmax=" +
                       data[ i ].getMaximumRangeValue() + "\n" ) ;
            sb.append( "y" + whichFunction + "axislogbase=" + this.yLog[ i ] +
                       "\n" ) ;
            sb.append( "y" + whichFunction + "axisticks=" + this.yTicks[ i ] +
                       "\n" ) ;
            sb.append( "tlegendfunction" + whichFunction + "=" +
                       data[ i ].getSeriesName( j ) + "\n" ) ;
            sb.append( "y" + whichFunction + "color=" +
                       this.colors[ i ][ j ].getRGB() + "\n" ) ;
            int tempIndex = -1;
            for (int index = 0; index < BIGPlotRenderer.BIG_SHAPES.length; index++)
            {
                if (this.shapes[i][j] == BIGPlotRenderer.BIG_SHAPES[index])
                        tempIndex = index;
            }
            sb.append( "y" + whichFunction + "shape=" +
                                tempIndex + "\n");

            sb.append( "y" + whichFunction + "axispre=" +
                       pre + "\n" ) ;

            whichFunction++ ;
         }

      }
      sb.append( "numfunctions=" + ( whichFunction - 1 ) + "\n" ) ;
      sb.append( "numfunctions=" + ( whichFunction - 1 ) + "\n" ) ;
      super.appendFontInformation( sb ) ;

      sb.append( "beginofdata\n" ) ;
      double[][] values = new double[whichFunction ][ this.data[ 0 ].getValues()[
          0 ].length ] ;
      values[ 0 ] = data[ 0 ].getValues()[ 0 ] ;
      whichFunction = 1 ;
      for ( int i = 0 ; i < data.length ; i++ )
      {
         for ( int j = 0 ; j < data[ i ].getSeriesCount() ; j++ )
         {
            values[ whichFunction ] = data[ i ].getValues()[ j + 1 ] ;
            whichFunction++ ;
         }
      }
      for ( int j = 0 ; j < values[ 0 ].length ; j++ )
      {

         for ( int i = 0 ; i < values.length ; i++ )
         {
            sb.append( values[ i ][ j ] ) ;
            sb.append( '\t' ) ;
         }
         sb.append( '\n' ) ;
      }
      sb.append( "endofdata\n" ) ;
      system.BIGFileHelper.saveToFile( sb.toString() , f ) ;

   }

   private File saveFile = null ;
   /**
    * This methode sets all informations stored in the specified file
    * to this mixer.
    * 
    * @param f - file with all necessary informations about the mixer
    */
   public void init( File f )
   {
      if ( f == null )return ;
      saveFile = f ;
      if ( !f.exists() )return ;
      super.initFonts( f ) ;
      double xMin = 0.0 ;
      double xMax = 0.0 ;
      double[] yMin = null ;
      double[] yMax = null ;
      double xMinDefault = 0.0 ;
      double xMaxDefault = 0.0 ;
      double[] yMinDefault = null ;
      double[] yMaxDefault = null ;
      this.titleDefault = "" ;
      this.title = titleDefault ;

      /*
              First:
         parsing the output File
       */
      // open the parser
      BIGOutputParser parser = null ;
      try
      {
         parser = new BIGOutputParser( f.getAbsolutePath() ) ;
      }
      catch ( BIGParserException ex )
      {
         System.err.println( "Error while parsing " + f.getName() +
                             ":" + ex.getMessage() ) ;
         return ;
      }
      // setting xValues (thank god, they are just once)
      this.xAxisText = parser.getValue( "xaxistext" ) ;
      this.xAxisTextDefault = this.xAxisText ;
      xMin = Double.parseDouble( parser.getValue( "xoutmin" ) ) ;
      xMinDefault = xMin ;
      xMax = Double.parseDouble( parser.getValue( "xoutmax" ) ) ;
      xMaxDefault = xMax ;
      this.xLog = Integer.parseInt( parser.getValue( "xaxislogbase" ) ) ;
      this.xLogDefault = this.xLog ;
      this.xTicks = Integer.parseInt( parser.getValue( "xaxisticks" ) ) ;
      this.xTicksDefault = this.xTicks ;
      this.titleDefault = parser.getValue( "title" ) ;
      this.title = this.titleDefault ;
      // now the tough stuff the yaxis
      int numberOfFunctions = -1 ;
      // maybe the file is filled out correctly :)
      try
      {
         numberOfFunctions = Integer.parseInt( parser.getValue( "numfunctions" ) ) ;
      }
      catch ( Exception ex1 )
      {}
      BIGStrings searchedValues = new BIGStrings() ;
      for ( int i = 1 ; i <= numberOfFunctions ; i++ )
      {
         searchedValues.add( "y" + numberOfFunctions + "axistext" ) ;
         searchedValues.add( "y" + numberOfFunctions + "outmin" ) ;
         searchedValues.add( "y" + numberOfFunctions + "outmax" ) ;
         searchedValues.add( "y" + numberOfFunctions + "axislogbase" ) ;
         searchedValues.add( "y" + numberOfFunctions + "axisticks" ) ;
         searchedValues.add( "y" + numberOfFunctions + "color" ) ;
         searchedValues.add( "y" + numberOfFunctions + "shape" ) ;
         searchedValues.add( "tlegendfunction" + numberOfFunctions ) ;
      }
      // get number of differen y axis
      Vector yNames = new Vector() ;
      Vector yMins = new Vector() ;
      Vector yMaxs = new Vector() ;
      Vector yLogs = new Vector() ;
      Vector yTicks = new Vector() ;
      Vector yColors = new Vector() ;
      Vector yShapes = new Vector() ;
      Vector yPre = new Vector() ;
      Vector whichFunctionsToWhichAxis = new Vector() ;
      for ( int i = 1 ; i <= numberOfFunctions ; i++ )
      {
         boolean found = false ;
         for ( int j = 0 ; j < yNames.size() ; j++ )
            if ( yNames.get( j ).equals( parser.getValue( "y" + i + "axistext" ) ) )
            {
               ( ( Vector ) whichFunctionsToWhichAxis.get( j ) ).add( new
                   Integer( i ) ) ;
               ( ( Vector ) yColors.get( j ) ).add( new
                   Color(
                       Integer.parseInt( ( String ) parser.getValue( "y" + i +
                   "color" ) ) ) ) ;
                  try
                  {
                     ( ( Vector ) yShapes.get( j ) ).add(
                         new Integer( ( String ) parser.getValue( "y" + i +
                         "shape" ) ) ) ;
                  }
                  catch ( NumberFormatException ex5 )
                  {
                     ( ( Vector ) yShapes.get( j ) ).add( new Integer( ( i - 1 ) %
                         BIGPlotRenderer.BIG_SHAPES.length ) ) ;
                  }



               /*System.err.println("Found at:"+j+"/"+yNames.size());
                System.err.println(parser.getValue( "y" + i + "axistext" ));
                               System.err.println("Inner:"+i);*/
               if ( ( ( Double ) yMins.get( j ) ).compareTo( Double.valueOf(
                   parser.getValue( "y" +
                                    i + "outmin" ) ) ) > 0 )
               {
                  yMins.remove( j ) ;
                  yMins.add( j ,
                             new Double( ( String ) parser.getValue( "y" + i +
                      "outmin" ) ) ) ;
               }
               if ( ( ( Double ) yMaxs.get( j ) ).compareTo( Double.valueOf(
                   parser.getValue( "y" +
                                    i + "outmax" ) ) ) < 0 )
               {
                  yMaxs.remove( j ) ;
                  yMaxs.add( j ,
                             new Double( ( String ) parser.getValue( "y" + i +
                      "outmax" ) ) ) ;
               }
               if ( ( ( Integer ) yLogs.get( j ) ).compareTo( Integer.valueOf(
                   parser.getValue( "y" +
                                    i + "axislogbase" ) ) ) > 0 )
               {
                  yLogs.remove( j ) ;
                  yLogs.add( j ,
                             new Integer( ( String ) parser.getValue( "y" + i +
                      "axislogbase" ) ) ) ;
               }
               if ( ( ( Integer ) yTicks.get( j ) ).compareTo( Integer.valueOf(
                   parser.getValue( "y" +
                                    i + "axisticks" ) ) ) < 0 )
               {
                  yTicks.remove( j ) ;
                  yTicks.add( j ,
                              new Integer( ( String ) parser.getValue( "y" + i +
                      "axisticks" ) ) ) ;
               }

               found = true ;
            }
         if ( !found )
         {
            yNames.add( parser.getValue( "y" + i + "axistext" ) ) ;
            try
            {
               yPre.add( new Integer( parser.getValue( "y" + i + "axispre" ) ) ) ;
            }
            catch ( Exception ex2 )
            {
               System.err.println( "Default" ) ;
               yPre.add( new Integer( 3 ) ) ;
            }
            //System.err.println("NEW TEXT ("+(yNames.size()-1)+"):"+parser.getValue( "y" + i + "axistext" ));
            Vector whichFuntionToThisAxis = new Vector() ;
            whichFuntionToThisAxis.add( new Integer( i ) ) ;
            //System.err.println("Outer"+i);
            whichFunctionsToWhichAxis.add( whichFuntionToThisAxis ) ;
            Vector colorsForThisYAxis = new Vector() ;
            colorsForThisYAxis.add( new Color( Integer.parseInt( ( String )
                parser.getValue( "y" + i + "color" ) ) ) ) ;
            yColors.add( colorsForThisYAxis ) ;
            Vector shapesForThisYAxis = new Vector() ;
            try
            {
               shapesForThisYAxis.add( new Integer( ( String ) parser.getValue(
                   "y" + i + "shape" ) ) ) ;
               yShapes.add( shapesForThisYAxis ) ;
            }
            catch ( NumberFormatException e )
            {
               shapesForThisYAxis.add( new Integer( i %
                   BIGPlotRenderer.BIG_SHAPES.length ) ) ;
               yShapes.add( shapesForThisYAxis ) ;
               // e.printStackTrace();
            }


            try
            {
               yMins.add( new Double( ( String ) parser.getValue( "y" + i +
                   "outmin" ) ) ) ;
            }
            catch ( NumberFormatException ex3 )
            {
               yMins.add( new Double( 0.0 ) ) ;
            }
            try
            {
               yMaxs.add( new Double( ( String ) parser.getValue( "y" + i +
                   "outmax" ) ) ) ;
            }
            catch ( NumberFormatException ex4 )
            {
               yMaxs.add( new Double( 0.0 ) ) ;
            }
            yLogs.add( new Integer( ( String ) parser.getValue( "y" + i +
                "axislogbase" ) ) ) ;
            //System.err.println(parser.getValue( "y" + i +"axislogbase" ));
            yTicks.add( new Integer( ( String ) parser.getValue( "y" + i +
                "axisticks" ) ) ) ;
         }
      }
      // s has now all different yAxisNames as keys
      // initializing this' data
      this.yAxisText = new String[yNames.size() ] ;
      this.yAxisTextDefault = new String[yNames.size() ] ;
      yMin = new double[yNames.size() ] ;
      yMax = new double[yNames.size() ] ;
      yMinDefault = new double[yNames.size() ] ;
      yMaxDefault = new double[yNames.size() ] ;
      this.data = new BIGDataSet[yNames.size() ] ;
      this.colors = new Color[yNames.size() ][] ;
      this.shapes = new Shape[yNames.size() ][] ;
      yLog = new int[yNames.size() ] ;
      yLogDefault = new int[yNames.size() ] ;
      this.yTicks = new int[yNames.size() ] ;
      this.yTicksDefault = new int[yNames.size() ] ;
      for ( int i = 0 ; i < yNames.size() ; i++ )
      {
         yAxisText[ i ] = ( String ) yNames.get( i ) ;
         yAxisTextDefault[ i ] = yAxisText[ i ] ;
         yMin[ i ] = ( ( Double ) yMins.get( i ) ).doubleValue() ;
         yMinDefault[ i ] = yMin[ i ] ;
         yMax[ i ] = ( ( Double ) yMaxs.get( i ) ).doubleValue() ;
         yMaxDefault[ i ] = yMax[ i ] ;
         //System.err.println("yLog+"+( ( Integer ) yLogs.get( i ) ).intValue());
         yLog[ i ] = ( ( Integer ) yLogs.get( i ) ).intValue() ;
         yLogDefault[ i ] = ( ( Integer ) yLogs.get( i ) ).intValue() ;
         this.yTicks[ i ] = ( ( Integer ) yTicks.get( i ) ).intValue() ;
         this.yTicksDefault[ i ] = ( ( Integer ) yTicks.get( i ) ).intValue() ;
         Vector thisYColors = ( ( Vector ) yColors.get( i ) ) ;
         this.colors[ i ] = new Color[thisYColors.size() ] ;
         for ( int j = 0 ; j < colors[ i ].length ; j++ )
         {
            this.colors[ i ][ j ] = ( Color ) thisYColors.get( j ) ;
         }
         Vector thisYShapes = ( ( Vector ) yShapes.get( i ) ) ;
         this.shapes[ i ] = new Shape[thisYShapes.size() ] ;
         try
         {
            for ( int j = 0 ; j < shapes[ i ].length ; j++ )
            {
               int index = ( ( Integer ) thisYShapes.get( j ) ).intValue() ;
               this.shapes[ i ][ j ] = BIGPlotRenderer.BIG_SHAPES[ index ] ;
            }
         }
         catch ( IndexOutOfBoundsException iobe )
         {
            // TODO:  Is there something to do?
         }



      }
      double[][] values = parser.parseOutputToData() ;
      /*
             Building the dataSets

       */
      if ( yNames.size() > 1 )
      {
         this.displayedDataSets = new int[2 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
         this.displayedDataSets[ 1 ] = 1 ;
      }
      else
      {
         this.displayedDataSets = new int[1 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
      }

      for ( int i = 0 ; i < whichFunctionsToWhichAxis.size() ; i++ )
      {
         Vector whichFuntionToThisAxis = ( Vector ) whichFunctionsToWhichAxis.
             get( i ) ;
         double[][] thisAxissData = new double[whichFuntionToThisAxis.size() +
             1 ][
             values[ 0 ].length ] ;
         // xData
         thisAxissData[ 0 ] = values[ 0 ] ;
         String[] namesForThisAxis = new String[whichFuntionToThisAxis.size() ] ;
         //System.err.println(i+"/"+namesForThisAxis.length+"("+yNames.get(i)+")");
         for ( int j = 0 ; j < namesForThisAxis.length ; j++ )
         {
            thisAxissData[ j +
                1 ] = values[ ( ( Integer ) whichFuntionToThisAxis.get( j ) ).
                intValue() ] ;
            namesForThisAxis[ j ] = "" + j ;
            namesForThisAxis[ j ] = ( String ) parser.getValue(
                "tlegendfunction" +
                ( ( Integer ) whichFuntionToThisAxis.get( j ) ).intValue() ) ;
            //System.err.println(""+j+":" + namesForThisAxis[j]);
         }
         /*for (int ii=0;ii<thisAxissData.length;ii++)
                   {
            for (int jj=0;jj<thisAxissData[ii].length;jj++)
               System.err.println(ii+" "+jj+" : "+thisAxissData[ii][jj]);
                   }*/
         this.data[ i ] = new BIGDataSet( thisAxissData , namesForThisAxis ) ;
         this.data[ i ].setDefaultMinsAndMaxs( xMinDefault , xMaxDefault ,
                                               yMinDefault[ i ] ,
                                               yMaxDefault[ i ] ) ;
         this.data[ i ].setPre(
             ( ( Integer ) yPre.get( i ) ).intValue() ) ;
      }

      this.initNodes() ;
   }

   private void initNodes()
   {
      for ( int i = 0 ; i < this.data.length ; i++ )
      {
         for ( int j = 0 ; j < this.data[ i ].getSeriesCount() ; j++ )
         {
            Graph g = new Graph() ;
            g.setGraphName( this.data[ i ].
                            getSeriesName( j ) ) ;
            g.setPoints( this.data[ i ].getValues()[ 0 ] ,
                         this.data[ i ].getValues()[ j + 1 ] ) ;
            g.setYAxisText( this.yAxisText[ i ] ) ;
            g.setXAxisText( this.xAxisText ) ;
            this.node.add( new DefaultMutableTreeNode( g ) ) ;
            if ( resultTree != null )
            {
               ( ( DefaultTreeModel ) resultTree.getModel() ).reload( this.node ) ;
            }
         }

      }
      this.getPlot() ;
   }

   /**
    * Removes a function from this mixer.
    * 
    * @param name - name of function that will be removed
    */
   public void removeFunction( String name )
   {
      boolean removed = false ;
      int index = -1 ;
      int index2 = -1 ;
      for ( int i = 0 ; i < this.data.length ; i++ )
      {
         for ( int j = 0 ; j < data[ i ].getSeriesCount() ; j++ )
            if ( data[ i ].getSeriesName( j ).equals( name ) )
            {
               index = j ;
               removed = data[ i ].remove( name ) ;
               if ( removed )
               {
                  index2 = i ;
               }
            }
      }
      if ( removed )
      {
         if ( data[ index2 ].getSeriesCount() == 0 )
         {
            this.removeDataSet( index2 ) ;
            return ;
         }
         Color[] newColors = new Color[this.colors[ index2 ].length - 1 ] ;
         Shape[] newShapes = new Shape[this.shapes[ index2 ].length-1];
         // and the yAxisText
         // also ticks and logs
         for ( int i = 0 ; i < index ; i++ )
         {
            newColors[ i ] = this.colors[ index2 ][ i ] ;
            newShapes[ i ] = this.shapes[ index2 ][ i ] ;
         }
         for ( int i = index + 1 ; i < this.colors[ index2 ].length ; i++ )
         {
            newColors[ i - 1 ] = this.colors[ index2 ][ i ] ;
            newShapes[ i - 1 ] = this.shapes[ index2 ][ i ] ;
         }
         this.colors[ index2 ] = newColors ;
         this.shapes[ index2 ] = newShapes ;

         for ( int i = 0 ; i < this.node.getChildCount() ; i++ )
            if ( this.node.getChildAt( i ).toString().equals( name ) )
               this.node.remove( i ) ;
         if ( plot != null )
            plot.setupConfig() ;
      }
   }

   //------------------------------------
   public boolean addFunctionFromGraph( double[][] values , String yName , Graph graph )
   {
	   String name = graph.getGraphName();
      if ( ( yAxisText == null ) || ( yAxisText.length == 0 ) )
      {
         this.yAxisText = new String[1 ] ;
         this.yAxisText[ 0 ] = yName ;
         this.yAxisTextDefault = new String[1 ] ;
         this.yAxisTextDefault[ 0 ] = yName ;
         this.xLog = 0 ;
         this.xTicks = -1 ;
         this.yLog = new int[1 ] ;
         yLog[ 0 ] = 0 ;
         this.yTicks = new int[1 ] ;
         yTicks[ 0 ] = -1 ;
         this.data = new BIGDataSet[1 ] ;
         this.colors = new Color[1 ][ 1 ] ;
         this.colors[ 0 ][ 0 ] = null ;
         this.shapes = new Shape[ 1 ][ 1 ];
         this.shapes[ 0 ][ 0 ] = null;
         this.displayedDataSets = new int[1 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
         String[] names = new String[1 ] ;
         names[ 0 ] = name ;
         this.data[ 0 ] = new BIGDataSet( values , names ) ;
         return true ;
      }
      for ( int i = 0 ; i < this.yAxisText.length ; i++ )
      {
         if ( yAxisText[ i ].equals( yName ) )
         {
            boolean retValue =
                data[ i ].addFromGraph( values , graph ) ;
            // update all xValues
            if ( retValue )
            {
               double[][] xVals = new double[1 ][] ;
               xVals[ 0 ] = values[ 0 ] ;
               for ( int j = 0 ; j < data.length ; j++ )
                  data[ j ].add( xVals , null ) ;
            }
            double min = Double.POSITIVE_INFINITY ;
            double max = Double.NEGATIVE_INFINITY ;
            for ( int j = 0 ; j < values[ 1 ].length ; j++ )
            {
               if ( min > values[ 1 ][ j ] )
                  min = values[ 1 ][ j ] ;
               if ( max < values[ 1 ][ j ] )
                  max = values[ 1 ][ j ] ;
            }
            if ( max > data[ i ].getMaximumRangeValue().doubleValue() )
               data[ i ].setMaximumRangeValue( max ) ;
            if ( min < data[ i ].getMinimumRangeValue().doubleValue() )
               data[ i ].setMinimumRangeValue( min ) ;
            Color[] newColors = new Color[data[i ].getSeriesCount() ] ;
            if ( colors[ i ] == null )
               colors[ i ] = new Color[0 ] ;
            for ( int j = 0 ; j < this.colors[ i ].length ; j++ )
               newColors[ j ] = this.colors[ i ][ j ] ;
            this.colors[ i ] = newColors ;
            Shape[] newShapes = new Shape[data[i ].getSeriesCount() ] ;
            if ( shapes[ i ] == null )
               shapes[ i ] = new Shape[0 ] ;
            for ( int j = 0 ; j < this.shapes[ i ].length ; j++ )
               newShapes[ j ] = this.shapes[ i ][ j ] ;
            this.shapes[ i ] = newShapes ;


            if ( this.plot == null )
               this.plot = new BIGPlot( this ) ;
            this.plot.setupConfig() ;
            return retValue ;

         }
      }
      if ( this.displayedDataSets.length == 1 )
      {
         String[] newYAxisText = new String[2 ] ;
         newYAxisText[ 0 ] = this.yAxisText[ 0 ] ;
         newYAxisText[ 1 ] = yName ;
         this.yAxisText = newYAxisText ;
         String[] newYAxisTextDefault = new String[2 ] ;
         newYAxisTextDefault[ 0 ] = this.yAxisTextDefault[ 0 ] ;
         newYAxisTextDefault[ 1 ] = yName ;
         this.yAxisTextDefault = newYAxisTextDefault ;
         BIGDataSet[] newData = new BIGDataSet[2 ] ;
         newData[ 0 ] = this.data[ 0 ] ;
         this.data = newData ;
         Color[][] newColors = new Color[2 ][] ;
         newColors[ 0 ] = this.colors[ 0 ] ;
         this.colors = newColors ;
         Shape[][] newShapes = new Shape[ 2 ][];
         newShapes[ 0 ] = this.shapes[ 0 ];
         this.shapes = newShapes;
         this.displayedDataSets = new int[2 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
         this.displayedDataSets[ 1 ] = 1 ;
         int[] newYLog = new int[2 ] ;
         newYLog[ 0 ] = this.yLog[ 0 ] ;
         newYLog[ 1 ] = 0 ;
         this.yLog = newYLog ;
         int[] newYTicks = new int[2 ] ;
         newYTicks[ 0 ] = this.yTicks[ 0 ] ;
         newYTicks[ 1 ] = 0 ;
         this.yTicks = newYTicks ;
         String[] names = new String[1 ] ;
         names[ 0 ] = name ;
         this.data[ 1 ] = new BIGDataSet( values , names ) ;
         this.plot.setupConfig() ;
         return true ;

      }
      return false ;
   }
   //-----------------------------
   
   public boolean addFunction( double[][] values , String yName , String name )
   {
      if ( ( yAxisText == null ) || ( yAxisText.length == 0 ) )
      {
         this.yAxisText = new String[1 ] ;
         this.yAxisText[ 0 ] = yName ;
         this.yAxisTextDefault = new String[1 ] ;
         this.yAxisTextDefault[ 0 ] = yName ;
         this.xLog = 0 ;
         this.xTicks = -1 ;
         this.yLog = new int[1 ] ;
         yLog[ 0 ] = 0 ;
         this.yTicks = new int[1 ] ;
         yTicks[ 0 ] = -1 ;
         this.data = new BIGDataSet[1 ] ;
         this.colors = new Color[1 ][ 1 ] ;
         this.colors[ 0 ][ 0 ] = null ;
         this.shapes = new Shape[ 1 ][ 1 ];
         this.shapes[ 0 ][ 0 ] = null;
         this.displayedDataSets = new int[1 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
         String[] names = new String[1 ] ;
         names[ 0 ] = name ;
         this.data[ 0 ] = new BIGDataSet( values , names ) ;
         return true ;
      }
      for ( int i = 0 ; i < this.yAxisText.length ; i++ )
      {
         if ( yAxisText[ i ].equals( yName ) )
         {
            boolean retValue =
                data[ i ].add( values , name ) ;
            // update all xValues
            if ( retValue )
            {
               double[][] xVals = new double[1 ][] ;
               xVals[ 0 ] = values[ 0 ] ;
               for ( int j = 0 ; j < data.length ; j++ )
                  data[ j ].add( xVals , null ) ;
            }
            double min = Double.POSITIVE_INFINITY ;
            double max = Double.NEGATIVE_INFINITY ;
            for ( int j = 0 ; j < values[ 1 ].length ; j++ )
            {
               if ( min > values[ 1 ][ j ] )
                  min = values[ 1 ][ j ] ;
               if ( max < values[ 1 ][ j ] )
                  max = values[ 1 ][ j ] ;
            }
            if ( max > data[ i ].getMaximumRangeValue().doubleValue() )
               data[ i ].setMaximumRangeValue( max ) ;
            if ( min < data[ i ].getMinimumRangeValue().doubleValue() )
               data[ i ].setMinimumRangeValue( min ) ;
            Color[] newColors = new Color[data[i ].getSeriesCount() ] ;
            if ( colors[ i ] == null )
               colors[ i ] = new Color[0 ] ;
            for ( int j = 0 ; j < this.colors[ i ].length ; j++ )
               newColors[ j ] = this.colors[ i ][ j ] ;
            this.colors[ i ] = newColors ;
            Shape[] newShapes = new Shape[data[i ].getSeriesCount() ] ;
            if ( shapes[ i ] == null )
               shapes[ i ] = new Shape[0 ] ;
            for ( int j = 0 ; j < this.shapes[ i ].length ; j++ )
               newShapes[ j ] = this.shapes[ i ][ j ] ;
            this.shapes[ i ] = newShapes ;


            if ( this.plot == null )
               this.plot = new BIGPlot( this ) ;
            this.plot.setupConfig() ;
            return retValue ;

         }
      }
      if ( this.displayedDataSets.length == 1 )
      {
         String[] newYAxisText = new String[2 ] ;
         newYAxisText[ 0 ] = this.yAxisText[ 0 ] ;
         newYAxisText[ 1 ] = yName ;
         this.yAxisText = newYAxisText ;
         String[] newYAxisTextDefault = new String[2 ] ;
         newYAxisTextDefault[ 0 ] = this.yAxisTextDefault[ 0 ] ;
         newYAxisTextDefault[ 1 ] = yName ;
         this.yAxisTextDefault = newYAxisTextDefault ;
         BIGDataSet[] newData = new BIGDataSet[2 ] ;
         newData[ 0 ] = this.data[ 0 ] ;
         this.data = newData ;
         Color[][] newColors = new Color[2 ][] ;
         newColors[ 0 ] = this.colors[ 0 ] ;
         this.colors = newColors ;
         Shape[][] newShapes = new Shape[ 2 ][];
         newShapes[ 0 ] = this.shapes[ 0 ];
         this.shapes = newShapes;
         this.displayedDataSets = new int[2 ] ;
         this.displayedDataSets[ 0 ] = 0 ;
         this.displayedDataSets[ 1 ] = 1 ;
         int[] newYLog = new int[2 ] ;
         newYLog[ 0 ] = this.yLog[ 0 ] ;
         newYLog[ 1 ] = 0 ;
         this.yLog = newYLog ;
         int[] newYTicks = new int[2 ] ;
         newYTicks[ 0 ] = this.yTicks[ 0 ] ;
         newYTicks[ 1 ] = 0 ;
         this.yTicks = newYTicks ;
         String[] names = new String[1 ] ;
         names[ 0 ] = name ;
         this.data[ 1 ] = new BIGDataSet( values , names ) ;
         this.plot.setupConfig() ;
         return true ;

      }
      return false ;
   }

   public void removeDataSet( int index )
   {
      int isDisplayed = -1 ;
      int[] newDisplays = null ;
      if ( this.data.length > 2 )
         newDisplays = new int[2 ] ;
      else
      if ( this.data.length == 2 )
         newDisplays = new int[1 ] ;
      else
      if ( data.length == 1 )
      {
         this.displayedDataSets = new int[0 ] ;
         this.plot = null ;
         this.plotPanel = new JPanel() ;
      }
      // remove colors
      Color[][] newColors = new Color[this.colors.length - 1 ][] ;
      // remove shapes
      Shape[][] newShapes = new Shape[this.shapes.length - 1][];

      // and the yAxisText
      String[] newYAxisText = new String[this.yAxisText.length - 1 ] ;
      String[] newYAxisTextDefault = new String[this.yAxisTextDefault.length -
          1 ] ;
      // also ticks and logs
      int[] newYAxisTicks = new int[this.yLog.length - 1 ] ;
      int[] newYAxisLogs = new int[this.yTicks.length - 1 ] ;
      BIGDataSet[] newData = new BIGDataSet[this.data.length - 1 ] ;
      for ( int i = 0 ; i < index ; i++ )
      {
         newData[ i ] = this.data[ i ] ;
         newYAxisTicks[ i ] = this.yTicks[ i ] ;
         newYAxisLogs[ i ] = this.yLog[ i ] ;
         newYAxisText[ i ] = this.yAxisTextDefault[ i ] ;
         newYAxisTextDefault[ i ] = this.yAxisTextDefault[ i ] ;
         newColors[ i ] = this.colors[ i ] ;
         newShapes[ i ] = this.shapes[ i ];
      }
      for ( int i = index + 1 ; i < this.colors.length ; i++ )
      {
         newData[ i - 1 ] = this.data[ i ] ;
         newYAxisTicks[ i - 1 ] = this.yTicks[ i ] ;
         newYAxisLogs[ i - 1 ] = this.yLog[ i ] ;
         newYAxisText[ i - 1 ] = this.yAxisTextDefault[ i ] ;
         newYAxisTextDefault[ i - 1 ] = this.yAxisTextDefault[ i ] ;
         newColors[ i - 1 ] = this.colors[ i ] ;
         newShapes[ i - 1 ] = this.shapes[ i ];
      }
      this.data = newData ;
      this.yTicks = newYAxisTicks ;
      this.yLog = newYAxisLogs ;
      this.yAxisText = newYAxisText ;
      this.yAxisTextDefault = newYAxisTextDefault ;
      this.colors = newColors ;
      this.shapes = newShapes;
      this.displayedDataSets = newDisplays ;
      if ( this.plot != null )
         this.plot.setupConfig() ;
      /*for ( int i = 0 ; i < this.displayedDataSets.length ; i++ )
             {
         if (this.displayedDataSets[i]==index)
         {
            isDisplayed=i;
         }
             }
             // if its the first yaxis
             if (isDisplayed==0)
             {
         this.plotPanel=new JPanel();
             }
             // if its the second yaxis
             if (isDisplayed==1)
             {
         this.plot.setupConfig();
             }*/
   }


   public void rename( String oldS , String newS )
   {
      super.rename( oldS , newS ) ;
      for ( int i = 0 ; i < this.node.getChildCount() ; i++ )
      {
         if ( this.node.getChildAt( i ).toString().equals( oldS ) )
         {
            ( (Graph)( ( ( DefaultMutableTreeNode ) ( this.node.getChildAt( i ) ) ).getUserObject() ) )
            .setGraphName(newS);
            this.plot.getChartPanel().getChart().legendChanged( new org.jfree.
                chart.event.LegendChangeEvent(new BIGPlotLegend( plot.
                getChartPanel().getChart() ) ) ) ;
            return ;
         }
      }
   }
   
   //----------------------------------------------------------------------
   public void updateResultTree() {
	   resultTree.updateUI();
   }
   //----------------------------------------------------------------------
   
   /**
    * gets the save-file-name
    */
   public File getSaveFile()
   {
      return saveFile;
   }
}
