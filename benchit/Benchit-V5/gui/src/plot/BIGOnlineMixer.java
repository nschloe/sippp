package plot ;

import javax.swing.* ;
import javax.swing.tree.DefaultMutableTreeNode;
import java.util.Vector;
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
public class BIGOnlineMixer
    extends BIGResultMixer
{

   private Vector graphs=new Vector();
    /**
     * in principle this is a resultMixer with additional features
     * @param plotPanel JPanel plotPanel for this mixer
     * @param textPanel JPanel textPanel for this mixer
     * @param title String title for plot
     */
    public BIGOnlineMixer( JPanel plotPanel , JPanel textPanel , String title )
   {
      super( plotPanel , textPanel , title ) ;
      node=new DefaultMutableTreeNode(this);
   }
   /**
    * use this to add a new graph received from somewhere (db or local)
    * @param g Graph
    */
   public void addGraph(conn.Graph g)
   {
      // name
      String newFunctionRenamed = g.getGraphName() ;
      // if there is a function with the same name
      // add a number (starting with 1)
      for ( int i = 0 ; i < graphs.size() ; i++ )
      {
         if ( newFunctionRenamed.equals( ( ( conn.Graph ) graphs.get( i ) ).
                                         getGraphName() ) )
         {
            if ( newFunctionRenamed.equals( g.getGraphName() ) )
               newFunctionRenamed = g.getGraphName() + "1" ;
            else
            {
               int ending = Integer.parseInt( newFunctionRenamed.substring( g.
                   getGraphName().length() ) ) + 1 ;
               newFunctionRenamed = g.getGraphName() + ending ;
            }

         }
      }
      g.setGraphName( newFunctionRenamed ) ;
      // if the xaxis isn't compatible (e.g. vector length and acces size in bytes)
      // allow it, but show a message
      if ( !this.xAxisText.equals( g.getXAxisText() ) )
      {
         if ( ( this.xAxisText != null ) && ( ! ( this.xAxisText.equals( "" ) ) ) )
         {
            if ( this.data.length > 0 )
               JOptionPane.showMessageDialog( null ,
                                              "The xaxis-text differs. Using new one." +
                                              " Be sure, that the functions are compareable." ) ;
            this.xAxisText = g.getXAxisText() ;
         }
      }
      // add it
      super.addGraph( g ) ;
      graphs.add( g ) ;
   }

   /**
    * gets the popupMenu for a BRM-node
    */
   public JPopupMenu getBIGPopupMenu( final JFrame jf ,final Object o )
   {
      // use the standard popup
      JPopupMenu menu=super.getBIGPopupMenu(jf,o);
      // and add compare if this rootNode is used
      if ((o instanceof BIGOnlineMixer))
      {
         menu.addSeparator();
         JMenuItem i=new JMenuItem("Compare online measurements");
         i.addActionListener(new java.awt.event.ActionListener()
             {
                public void actionPerformed (java.awt.event.ActionEvent ae)
                {
                   showCompareDialog();
                }
             });
         menu.add(i);


      }
      // or view info, if a leaf was clicked
      else
      {
         menu.addSeparator();
         JMenuItem i=new JMenuItem("View info");
         i.addActionListener(new java.awt.event.ActionListener()
             {
                public void actionPerformed (java.awt.event.ActionEvent ae)
                {
                   for (int i=0;i<graphs.size();i++)
                   {

                      if (o.toString().equals(((conn.Graph)graphs.get(i)).getGraphName()))
                      {
                         new conn.GraphInfoFrame((conn.Graph)graphs.get(i));
                      }
                   }
                }
             });
         menu.add(i);
      }
      return menu;
   }
   /**
    * rename a function
    * @param oldS String old Name
    * @param newS String new Name
    */
   public void rename (String oldS, String newS)
   {
      super.rename( oldS , newS ) ;
      for ( int i = 0 ; i < graphs.size() ; i++ )
      {

         if ( oldS.equals( ( ( conn.Graph ) graphs.get( i ) ).getGraphName() ) )
         {
            ( ( conn.Graph ) graphs.get( i ) ).setGraphName(newS) ;
         }
      }

   }
   /**
    * remove a function
    * @param s String name of the function
    */
   public void removeSubNode(String s)
   {
      super.removeSubNode(s);
      for ( int i = 0 ; i < graphs.size() ; i++ )
      {

         if ( s.equals( ( ( conn.Graph ) graphs.get( i ) ).getGraphName() ) )
         {
             graphs.remove( i );
         }
      }


   }
   /**
    * compare measurements (same options are black, differences blue)
    */
   private void showCompareDialog(  )
   {
      // names of functions
      String[] s = new String[this.graphs.size()] ;
      for ( int i = 0 ; i < s.length ; i++ )
      {
         s[i]=((conn.Graph)this.graphs.get(i)).getGraphName();
      }
      // select which to compare
      JList jl = new JList( s ) ;
      Object o[] = new Object[2 ] ;
      o[ 0 ] = "Please select the functions you'd like to compare" ;
      o[ 1 ] = jl ;
      int value = JOptionPane.showConfirmDialog( null , o ,
                                                 "select functions to compare" ,
                                                 JOptionPane.OK_CANCEL_OPTION ,
                                                 JOptionPane.QUESTION_MESSAGE ) ;
      if ( value == JOptionPane.CANCEL_OPTION )
         return ;
      // which were selected
      conn.Graph[] selected = new conn.Graph[jl.getSelectedIndices().length];
      for (int i=0;i<jl.getSelectedIndices().length;i++)
      {
         for (int j=0;j<graphs.size();j++)
         {
            if (jl.getSelectedValues()[i].equals(((conn.Graph)graphs.get(j)).getGraphName()))
            {
               selected[i]=(conn.Graph)graphs.get(j);
            }
         }
      }
      // show it in new frame
      new conn.GraphInfoFrame(selected);
   }
}
