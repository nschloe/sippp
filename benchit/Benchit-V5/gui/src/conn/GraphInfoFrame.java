package conn;

import java.awt.*;
import javax.swing.*;

public class GraphInfoFrame
    extends JFrame {
  /**
   * Constructor
   * @param g the Graph to show Infos about in this frame
   * @todo could be done with a JTable
   */
  public GraphInfoFrame(Graph g) {
     // identifiers
    String ident[] = g.settings[0];
    JScrollPane content = new JScrollPane();
    JPanel panel = new JPanel();
    // layout
    GridBagLayout gridbag = new GridBagLayout();
    panel.setLayout(gridbag);
    GridBagConstraints gc = new GridBagConstraints();
    gc.anchor = GridBagConstraints.WEST;
    int i;
    gc.gridx = 0;
    gc.gridy = 0;
    // first line: name
    panel.add(new JLabel(g.graphName), gc);
    gc.gridy = 1;
    // following lines:
    // for each setting
    // identifier setting-for-identifier
    panel.add(new JLabel("" + g.graphID), gc);
    for (i = 0; i < ident.length; i++) {
      gc.gridx = 0;
      gc.gridy = i + 1;
      panel.add(new JLabel(ident[i]), gc);
      gc.gridx = 1;
      panel.add(new JLabel(g.getSetting(ident[i])), gc);
    }
    plot.BIGResultMixer m=new plot.BIGResultMixer(null,null,g.getGraphName());
    m.addGraph(g);
    gc.gridy=i+1;
    gc.gridx=0;
    gc.gridwidth=2;
    panel.add(m.getPlot().getComponentAt(0),gc);
    content.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    content.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
   content.getVerticalScrollBar().setUnitIncrement( 15 ) ;
    double[] temp = {
        .5, .5};
    // +3: one because an array starts with 0 and one because of the buttons and one because of (name)(id)
    double[] temp2 = new double[i + 4];
    for (int j = 0; j < temp2.length; j++) {
      temp2[i] = 1 / temp2.length;
    }
    gridbag.columnWeights = temp;
    gridbag.rowWeights = temp2;
    content.setViewportView(panel);
    this.setContentPane(content);
    this.pack();
    this.setVisible(true);
 }
 /**
  * Constructor
  * @param g the Graph to show Infos about in this frame
  * @todo could be done with a JTable
  */
 public GraphInfoFrame(Graph[] g) {
    if (g==null) return;
    if (g.length==0) return;
    // identifiers (see above)
   String ident[] = g[0].settings[0];
   JScrollPane content = new JScrollPane();
   JPanel panel = new JPanel();
   // layout
   GridBagLayout gridbag = new GridBagLayout();
   panel.setLayout(gridbag);
   GridBagConstraints gc = new GridBagConstraints();
   gc.anchor = GridBagConstraints.WEST;
   int i;
   gc.gridx = 1;
   gc.gridy = 0;
   // first line all functions/graphs (first row free for identfiers)
   for (int k=0;k<g.length;k++)
   {
      gc.gridx = k+1;
      panel.add( new JLabel( g[k].graphName ) , gc ) ;
   }
   gc.gridy = 1;
   // following lines: identifiers and settings
   for (i = 0; i < ident.length; i++) {
     gc.gridx = 0;
     gc.gridy = i + 1;
     panel.add( new JLabel( ident[ i ] ) , gc ) ;
     // difference means: if there are different settings for same identifiers
     // it is shown with a blue foreground
     boolean difference=false;
     String setting=g[ 0 ].getSetting( ident[ i ] );
     for ( int k = 1 ; k < g.length ; k++ )
     {
        if (!g[ k ].getSetting( ident[ i ] ).equals(setting) )
           difference=true;
     }

     for ( int k = 0 ; k < g.length ; k++ )
     {
        gc.gridx = k + 1 ;
        JLabel lab=new JLabel( g[ k ].getSetting( ident[ i ] ) );
        if (difference)
           lab.setForeground(java.awt.Color.BLUE);
        panel.add(  lab , gc ) ;
     }
  }
   plot.BIGResultMixer m=new plot.BIGResultMixer(null,null,"Comparison");
   for (int j=0;j<g.length;j++)
      m.addGraph(g[j]);
   gc.gridy=i+1;
   gc.gridx=0;
   gc.gridwidth=g.length+1;
   panel.add(m.getPlot().getComponentAt(0),gc);

   content.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS ) ;
   content.setHorizontalScrollBarPolicy( JScrollPane.
                                         HORIZONTAL_SCROLLBAR_AS_NEEDED ) ;
   content.getVerticalScrollBar().setUnitIncrement( 15 ) ;
   double[] temp = {
       .5, .5};
   // +3: one because an array starts with 0 and one because of the buttons and one because of (name)(id)
   double[] temp2 = new double[i + 4];
   for (int j = 0; j < temp2.length; j++) {
     temp2[i] = 1 / temp2.length;
   }
   gridbag.columnWeights = temp;
   gridbag.rowWeights = temp2;
   content.setViewportView(panel);
   panel.setBackground(java.awt.Color.WHITE);
   this.setContentPane(content);
   this.pack();
   this.setVisible(true);
 }

}
