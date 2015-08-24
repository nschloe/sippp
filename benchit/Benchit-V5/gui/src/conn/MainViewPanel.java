package conn ;

import javax.swing.JTabbedPane;

/**
 * <p>Überschrift: MainViewPanel</p>
 * <p>Beschreibung: shows the selected Graph(s) by using plot.BIGPlot</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR</p>
 * @author rschoene
 * @version 1.0
 */
public class MainViewPanel
{

   /**
    * Constructor
    */
   public MainViewPanel()
   {
   }
   /**
    * makes a tabbed pane out of graphs. this is pretty the same as plot.BIGPlot does.
    * but it combines the panels to a tabbed pane
    * @param graphData GraphData
    * @return JTabbedPane
    */

   public static JTabbedPane setData(GraphData graphData)
   {
      try
      {
         // get the mixer
          plot.BIGResultMixer plotable=graphData.getMixer();
          if (plotable==null)
          {
             // create empy mixer
              plotable= new plot.BIGResultMixer( null , null ,
                                                 "BenchIT-DB" ) ;
              // initfonts: set global fonts and stuff
              plotable.initFonts(null);
              // set this graphDatas Mixer to the empty one
              graphData.setMixer(plotable);
          }
          // add all graphdatas graphs to the mixer
         for ( int i = 0 ; i < graphData.size() ; i++ )
         {
             if (plotable.getDataForFunction(graphData.getGraph( i ).getGraphName())!=null)
                 plotable.addGraph( graphData.getGraph( i ) ) ;
         }
         // return the plot
         return plotable.getPlot() ;
      } catch (Exception e)
      {
         // print and return an empty panel
         e.printStackTrace();
         return new JTabbedPane();
      }
   }
   /**
    * repaints with the data from graphData, calls setData
    * @param graphData the GraphData, that contains all Graphs
    * @return a BIGPlot with the updated data
    */
   public static JTabbedPane update( GraphData graphData )
   {
         return MainViewPanel.setData(graphData ) ;
   }
}
