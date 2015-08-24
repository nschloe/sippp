package conn ;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

/**
 * <p>Überschrift: BenchIT</p>
 * <p>Beschreibung: Here you can select your Graph by first defining the kernel</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR TU Dresden</p>
 * @author Robert Schoene
 * @version 1.0
 */
public class KernelEditPanel
    extends JScrollPane implements ActionListener
{

   /**
    * checkboxes for the last level. select with them, which function shall be displayed
    */
   private JCheckBox[][] checkBoxes = null ;
   /**
    * access the conn data
    */
   private MainEditPanel root ;
   /**
    * contains the actual level.
    * level 0: select algorithm
    */
   private int level = 0 ;
   /**
    * communicate with server
    */
   private SocketCon socketCon ;
   /**
    * which data was returned for level 1
    */
   private Vector level1Vector ;
   /**
    * reference to this
    */
   private KernelEditPanel content ;
   /**
    * for the selected kernel
    */
   private String kernelName = new String() ;

   /**
    * Constructor
    * @param socketCon the socketCon of this program
    * @param root the mainEditPanel of this program
    */
   public KernelEditPanel( SocketCon socketCon , MainEditPanel root )
   {
      this.socketCon = socketCon ;
      this.root = root ;
      content = this ;
      this.getVerticalScrollBar().setUnitIncrement( 15 ) ;
      this.initLayout() ;
   }

   /**
    * Constructor
    * @param socketCon the socketCon of this program
    * @param root the mainEditPanel of this program
    * @param identifiers all identifiers received from the server (not needed, you may use null)
    * @param comboBoxEntries the content of the jComboBoxes for the identifiers (elements of this are all String[]s) (not needed, you may use null)
    */

   public KernelEditPanel( SocketCon socketCon , MainEditPanel root ,
                           String[] identifiers , Vector[] comboBoxEntries )
   {
      this.socketCon = socketCon ;
      this.root = root ;
      content = this ;
      this.initLayout() ;
      this.getVerticalScrollBar().setUnitIncrement( 15 ) ;
   }

   /**
    * resets / repaints this panel
    */
   private void initLayout()
   {
      switch ( this.level )
      {
         // first select kernel
         case 0:
            final Thread t = new Thread()
            {
               public void run()
               {
                  // prepare for get available kernels
                  String[] s1 = new String[1 ] ;
                  String[] s2 = new String[1 ] ;
                  String[] s3 = new String[1 ] ;
                  s3[ 0 ] = "<any>" ;
                  s2[ 0 ] = "Kernelname" ;
                  s1[ 0 ] = "Kernelname" ;
                  // get them to v
                  Vector v = new Vector() ;
                  try
                  {
                     v = socketCon.getComboBoxContent( s1 , s2 , s3 ) ;
                  }
                  catch ( java.io.IOException e )
                  {
                     System.err.println( "Could not get anything" ) ;
                     return;
                  }
                  // all programnames
                  Vector allProgramNames = ( ( Vector[] ) v.elementAt( 1 ) )[ 0 ] ;
                  // we dont need no stinky <any>
                  allProgramNames.removeElementAt( 0 ) ;
                  //layout
                  final JPanel mainPanel = new JPanel() ;
                  GridBagLayout gridbag = new GridBagLayout() ;
                  mainPanel.setLayout( gridbag ) ;
                  GridBagConstraints gc = new GridBagConstraints() ;
                  gc.anchor = GridBagConstraints.WEST ;
                  gc.gridx = 0 ;
                  for ( int i = 0 ; i < allProgramNames.size() ; i++ )
                  {
                     // button for each kernelname
                     gc.gridy = i ;
                     JButton jb = new JButton( ( String ) allProgramNames.
                                               elementAt( i ) ) ;
                     jb.setToolTipText(
                         "Click here to add a function with kernel:" +
                         ( String ) allProgramNames.elementAt( i ) ) ;
                     jb.addActionListener( content ) ;
                     mainPanel.add( jb , gc ) ;
                  }
                  // repaint
                  Thread t = new Thread()
                  {
                     public void run()
                     {
                        content.setViewportView( mainPanel ) ;
                     }
                  } ;
                  SwingUtilities.invokeLater( t ) ;

               }
            } ;
            t.start() ;

            break ;
            // select functions/graphs
         case 1:
            // do layout
            final JPanel mainPanel = new JPanel() ;
            final GridBagLayout gridbag = new GridBagLayout() ;
            mainPanel.setLayout( gridbag ) ;
            final GridBagConstraints gc = new GridBagConstraints() ;
            gc.anchor = GridBagConstraints.WEST ;


            // setting legend left
            gc.gridx = 0 ;
            gc.gridy = 0 ;
            gc.weightx = 1 ;
            // for all computers, where the kernel is measured
            for ( int i = 0 ;
                  i < ( ( String[] ) level1Vector.elementAt( 0 ) ).length ; i++ )
            {
               gc.gridy = i + 2 ;
               // machine
               // computer definitions at the left
               // looks like a label, but its a button
               JButton jl = new JButton( ( ( String[] ) level1Vector.elementAt(
                   0 ) )[ i ] ) ;
               final int row = i ;
               jl.setBorder( BorderFactory.createEmptyBorder() ) ;
               jl.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent evt )
                  {
                     for ( int jIn = 0 ; jIn < checkBoxes.length ; jIn++ )
                        for ( int iIn = 0 ; iIn < checkBoxes[ 0 ].length ; iIn++ )
                           // if the measurement exists
                           if ( checkBoxes[ jIn ][ iIn ] != null )
                           {
                               // for all rows not in the actual selected
                              // and ctrl is NOT hold while pressing the button
                              if ( ( jIn != row ) &&
                                   (!( ( evt.getModifiers() &
                                       ActionEvent.CTRL_MASK ) > 0 )))

                              {
                                 // unselect all other
                                 checkBoxes[ jIn ][ iIn ].setSelected( false ) ;
                                 checkBoxes[ jIn ][ iIn ].revalidate() ;
                              }
                              else if (  jIn == row )
                              {
                                 // else let it selected
                                 // but always select the actual row
                                 checkBoxes[ jIn ][ iIn ].setSelected( true ) ;
                                 checkBoxes[ jIn ][ iIn ].revalidate() ;
                              }

                           }
                  }
               } ) ;
               gridbag.setConstraints( jl , gc ) ;
               mainPanel.add( jl , gc ) ;
            }

            // setting descriptions
            gc.gridy = 1 ;
            // now the columns
            for ( int i = 0 ;
                  i < ( ( String[] ) level1Vector.elementAt( 2 ) ).length ; i++ )
            {
               gc.gridx = i + 1 ;
               // description
               JButton jl = new JButton( ( ( String[] ) level1Vector.elementAt(
                   2 ) )[
                                         i ] ) ;
               jl.setBorder( BorderFactory.createEmptyBorder() ) ;
               final int column = i ;
               jl.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent evt )
                  {
                     for ( int jIn = 0 ; jIn < checkBoxes.length ; jIn++ )
                        for ( int iIn = 0 ; iIn < checkBoxes[ 0 ].length ; iIn++ )
                           if ( checkBoxes[ jIn ][ iIn ] != null )
                           {
                              // if NOT right column and NOT hold CTRL
                              if ( ( iIn != column ) &&
                                   (!( ( evt.getModifiers() &
                                       ActionEvent.CTRL_MASK ) > 0 )) )

                              {
                                 // unselect all others
                                 checkBoxes[ jIn ][ iIn ].setSelected( false ) ;
                                 checkBoxes[ jIn ][ iIn ].revalidate() ;
                              }
                              // if right column and NOT hold CTRL
                              else if (  iIn == column )
                              {
                                 // select the line
                                 checkBoxes[ jIn ][ iIn ].setSelected( true ) ;
                                 checkBoxes[ jIn ][ iIn ].revalidate() ;
                              }
                           }
                  }
               } ) ;

               gridbag.setConstraints( jl , gc ) ;
               mainPanel.add( jl , gc ) ;
            }

            // setting checkBoxes
            this.checkBoxes = new JCheckBox[ ( ( String[] ) level1Vector.
                                               elementAt( 0 ) ).
                length ][ ( ( String[] ) level1Vector.elementAt( 2 ) ).length ] ;
            for ( int i = 0 ;
                  i < ( ( String[] ) level1Vector.elementAt( 0 ) ).length ; i++ )
            {
               for ( int j = 0 ;
                     j < ( ( String[] ) level1Vector.elementAt( 2 ) ).length ;
                     j++ )
               {
                  String bitmask = Long.toBinaryString( ( new Long( ( ( (
                      String[] ) ( level1Vector.elementAt( 3 ) ) )[ i ] ) ) ).
                      longValue() ) ;
                  // got a bitmask
                  // 3 could mean 0011 but also 011 or 11 depending on number of descriptions (e.g. ijk or kij)
                  // which means ((String[])level1Vector.elementAt( 2 )).length
                  // add leading zeros
                  while ( bitmask.length() <
                          ( ( ( String[] ) level1Vector.elementAt( 2 ) ).length ) )
                     bitmask = '0' + bitmask ;
                  gc.gridy = i + 2 ;
                  gc.gridx = j + 1 ;
                  // if there is a graph available, set the checkbox
                  if ( bitmask.charAt( j ) == '1' )
                  {

                     this.checkBoxes[ i ][ j ] = new JCheckBox() ;
                     gridbag.setConstraints( this.checkBoxes[ i ][ j ] , gc ) ;
                     mainPanel.add( this.checkBoxes[ i ][ j ] , gc ) ;

                  }
                  // else don't
                  else
                     this.checkBoxes[ i ][ j ] = null ;

               }
            }

            // buttonPanel at the top
            JPanel buttonPanel = new JPanel() ;
            GridBagLayout buttonGridbag = new GridBagLayout() ;
            buttonPanel.setLayout( buttonGridbag ) ;
            GridBagConstraints buttonGc = new GridBagConstraints() ;
            buttonGc.anchor = GridBagConstraints.CENTER ;
            // go back to level 0
            JButton backButton = new JButton( "Back" ,
                                              new ImageIcon( "img" +
                java.io.File.separator + "back-16.png" ) ) ;
            backButton.setToolTipText( "Click here to select an other kernel" ) ;
            backButton.addActionListener(
                new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  level = 0 ;
                  initLayout() ;
               }
            }
            ) ;
            // add selected graphs/functions
            JButton addButton = new JButton( "Add" ) ;
            addButton.setToolTipText(
                "Click here add the Graphs, that are defined by your settings" ) ;
            addButton.addActionListener(
                new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  // as thread, for sure
                  final Thread t = new Thread()
                  {
                     public void run()
                     {
                        // disable button
                         setEnabled(false);
                        for ( int i = 0 ; i < checkBoxes.length ; i++ )
                        {
                         // how many selected checkboxes in this row
                           int numberOfSelected = 0 ;
                           for ( int j = 0 ; j < checkBoxes[ 0 ].length ; j++ )
                           {
                              if ( checkBoxes[ i ][ j ] != null )
                              {
                                 if ( checkBoxes[ i ][ j ].isSelected() )
                                 {
                                    numberOfSelected++ ;
                                 }
                              }
                           }
                           // more then one?
                           if ( numberOfSelected > 1 )
                           {
                              // the old trick. see conn.ArchitectureEditPanel
                              // short: get all graphs, add all, remove unnecessary, reload
                              String[] identifiers = new String[3 ] ;
                              String[] settings = new String[3 ] ;
                              identifiers[ 0 ] = "Filename" ;
                              identifiers[ 1 ] = "Description" ;
                              identifiers[ 2 ] = "Kernelname" ;
                              settings[ 0 ] = ( ( String[] ) level1Vector.
                                                get( 1 ) )[ i ] ;
                              settings[ 1 ] = "<any>" ;
                              settings[ 2 ] = ( kernelName ) ;
                              // add
                              root.addGraphWithoutReloading( identifiers ,
                                  settings ) ;
                              int base = root.root.graphData.size() ;
                              base = base - checkBoxes[ 0 ].length ;
                              for ( int j = checkBoxes[ 0 ].length - 1 ; j > -1 ;
                                    j-- )
                              {
                                 if ( checkBoxes[ i ][ j ] != null )
                                 {
                                    // remove unnecassary
                                    if ( !checkBoxes[ i ][ j ].isSelected() )
                                    {
                                       root.root.graphData.
                                           setSelectedGraph( root.root.
                                           graphData.getGraph( base + j ) ) ;
                                      root.root.graphData.
                                           removeSelectedGraph() ;
                                    }
                                 }
                              }
                              // reload
                              root.root.updateView( root.root.
                                  graphData ) ;

                           }
                           // just one selected in row
                           // (still see conn.ArchitectureEditPanel for more commenting)
                           else
                           {
                              for ( int j = 0 ; j < checkBoxes[ 0 ].length ; j++ )
                              {
                                 // if available
                                 if ( checkBoxes[ i ][ j ] != null )
                                 {
                                    // and selected
                                    if ( checkBoxes[ i ][ j ].isSelected() )
                                    {
                                       // get it
                                       String[] identifiers = new String[3 ] ;
                                       String[] settings = new String[3 ] ;
                                       identifiers[ 0 ] = "Filename" ;
                                       identifiers[ 1 ] = "Description" ;
                                       identifiers[ 2 ] = "Kernelname" ;
                                       settings[ 0 ] = ( ( String[] )
                                           level1Vector.
                                           get( 1 ) )[ i ] ;
                                       settings[ 1 ] = ( ( String[] )
                                           level1Vector.
                                           get( 2 ) )[ j ] ;
                                       settings[ 2 ] = ( kernelName ) ;
                                       root.addGraph( identifiers , settings ) ;

                                    }
                                 }

                              }
                           }

                        }
                        // enable button when done
                         setEnabled(true);
                         // repaint after each line
                        Thread t = new Thread()
                        {
                           public void run()
                           {
                              initLayout() ;
                           }
                        } ;
                        SwingUtilities.invokeLater( t ) ;

                     }
                  } ;
                  t.start() ;
               }
            }
            ) ;
            // back to begin, which means back
            JButton backToStartButton = new JButton( "Back to Begin" ,
                new
                ImageIcon( "img" +
                           java.io.File.separator + "backToBegin-16.png" ) ) ;
            backToStartButton.setToolTipText(
                "Click here to return to the beginning of the selection" ) ;
            backToStartButton.addActionListener( new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  level = 0 ;
                  initLayout() ;
               }
            }
            ) ;
            buttonGc.gridx = 0 ;
            buttonGc.gridy = 0 ;
            buttonPanel.add( backButton , buttonGc ) ;
            buttonGc.gridx = 2 ;
            buttonPanel.add( addButton , buttonGc ) ;
            buttonGc.gridx = 1 ;
            buttonPanel.add( backToStartButton , buttonGc ) ;
            double[] temp2 = new double[3 ] ;
            temp2[ 0 ] = 1.0 / 3.0 ;
            temp2[ 1 ] = 1.0 / 3.0 ;
            temp2[ 2 ] = 1.0 / 3.0 ;
            buttonGridbag.columnWeights = temp2 ;
            gc.gridx = 0 ;
            gc.gridy = ( ( String[] ) level1Vector.elementAt( 0 ) ).length + 2 ;
            mainPanel.add( buttonPanel , gc ) ;

            // button panels and buttons at the bottom
            buttonPanel = new JPanel() ;
            buttonGridbag = new GridBagLayout() ;
            buttonPanel.setLayout( buttonGridbag ) ;
            buttonGc = new GridBagConstraints() ;
            buttonGc.anchor = GridBagConstraints.CENTER ;
            JButton backButton2 = new JButton( "Back" ,
                                      new ImageIcon( "img" +
                java.io.File.separator +
                "back-16.png" ) ) ;
            backButton2.setToolTipText( "Click here to select an other kernel" ) ;
            backButton2.addActionListener(backButton.getActionListeners()[0]);

            JButton addButton2 = new JButton( "Add" ) ;
            addButton2.setToolTipText(
                "Click here add the Graphs, that are defined by your settings" ) ;
            addButton2.addActionListener(addButton.getActionListeners()[0]);

            JButton backToStartButton2 = new JButton( "Back to Begin" ,
                                             new ImageIcon( "img" +
                java.io.File.separator + "backToBegin-16.png" ) ) ;
            backToStartButton2.setToolTipText(
                "Click here to return to the beginning of the selection" ) ;
            backToStartButton2.addActionListener( backToStartButton.getActionListeners()[0]) ;

            buttonGc.gridx = 0 ;
            buttonGc.gridy = 0 ;
            buttonPanel.add( backButton2 , buttonGc ) ;
            buttonGc.gridx = 2 ;
            buttonPanel.add( addButton2 , buttonGc ) ;
            buttonGc.gridx = 1 ;
            buttonPanel.add( backToStartButton2 , buttonGc ) ;
            buttonGridbag.columnWeights = temp2 ;

            buttonGridbag.columnWeights = temp2 ;

            gc.gridx = 0 ;
            gc.gridy = 0 ;
            mainPanel.add( buttonPanel , gc ) ;
            content.setViewportView( mainPanel ) ;

      }
   }

   /**
    * performs some actions of this panel
    * @param evt the Event, that calls this
    */
   public void actionPerformed( final ActionEvent evt )
   {
      // at what level happened the event?
      // the only buttons, this is the ActionListener for,
      // are the kernelname-buttons at level 0
      switch ( this.level )
      {
         case 0:
            Thread t = new Thread()
            {
               public void run()
               {
                  try
                  {
                     // get the name
                     kernelName = ( ( JButton ) evt.getSource() ).getText() ;
                     // get information for level 1
                     level1Vector = socketCon.startKernelSelectWork( kernelName ) ;
                  }
                  catch ( java.io.IOException e )
                  {
                  }
                  // switch to next level
                  level = 1 ;
                  Thread t = new Thread()
                  {
                     public void run()
                     {
                        // repaint the panel i level 1
                        initLayout() ;
                     }
                  } ;
                  SwingUtilities.invokeLater( t ) ;

               }
            } ;
            t.start() ;
            break ;

      }

   }

}
