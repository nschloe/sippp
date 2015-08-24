package conn ;

import java.awt.* ;
import javax.swing.* ;
import java.awt.event.* ;
import java.util.* ;

/**
 *
 * <p>Überschrift: The ArchitectureEditPanel</p>
 * <p>Beschreibung: here you select your Graph with first defining the architecture</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR</p>
 * @author rschoene
 * @version 1.0
 */

public class ArchitectureEditPanel
    extends JScrollPane implements ActionListener
{
   /** at the last step a field of checkboxes is displayed. here it is ;) */
   private JCheckBox[][] checkBoxes = null ;

   /** the panel, which contains this */
   private MainEditPanel root ;
   /** which step is the actual  */
   private int level = 0 ;
   /** connection to the server */
   private SocketCon socketCon ;
   /** checkBoxes for the first step: which architecture-item shall be selected from */
   private JCheckBox[] level1CheckBoxes ;
   /** will contain a lot of stuff received from the server */
   private Vector level3Vector ;
   /** a link to this */
   private ArchitectureEditPanel content ;
   /** which identifier (selected architecture-item) */
   private String selectedIdentifier = new String() ;
   /** which programnames are selected */
   private String[] identifiers ;
   /** what are the available programnames */
   private String[] level2Programnames ;

   /**
    * Constructor
    * @param socketCon the socketConnection of the program
    * @param root the MainEditPanel of the program
    * @param identifiers the identifiers, that was received from the server
    * @param comboBoxEntries all entries to the identifiers
    */
   public ArchitectureEditPanel( SocketCon socketCon , MainEditPanel root ,
                                 String[] identifiers ,
                                 Vector[] comboBoxEntries )
   {
      this.socketCon = socketCon ;
      this.root = root ;
      content = this ;
      this.identifiers = identifiers ;
      this.getVerticalScrollBar().setUnitIncrement( 15 ) ;
      this.initLayout() ;
   }

   /**
    * resets / repaints the panel
    */
   private void initLayout()
   {
      // at which level are we?
      switch ( this.level )
      {
         // select which architecture-item
         case 0:
         {
            JPanel mainPanel = new JPanel() ;
            GridBagLayout gridbag = new GridBagLayout() ;
            mainPanel.setLayout( gridbag ) ;
            GridBagConstraints gc = new GridBagConstraints() ;
            gc.anchor = GridBagConstraints.WEST ;
            gc.gridx = 0 ;
            // add lots of buttons with the architecture items
            for ( int i = 0 ; i < identifiers.length ; i++ )
            {
               gc.gridy = i ;
               JButton jb = new JButton( identifiers[ i ] ) ;
               jb.setToolTipText( "Click this to select from identifier " +
                                  identifiers[ i ] ) ;
               jb.addActionListener( content ) ;
               mainPanel.add( jb , gc ) ;
            }
            content.setViewportView( mainPanel ) ;
         }
         break ;
         // select some of the architecture-items
         case 1:
         {
            JPanel mainPanel = new JPanel() ;
            GridBagLayout gridbag = new GridBagLayout() ;
            mainPanel.setLayout( gridbag ) ;
            GridBagConstraints gc = new GridBagConstraints() ;
            gc.anchor = GridBagConstraints.WEST ;
            gc.gridx = 0 ;
            gc.weightx = 1 ;
            // add lots of checkboxes, with the available settings
            // for the selected item from level 0
            for ( int i = 0 ; i < level1CheckBoxes.length ; i++ )
            {
               gc.gridy = i + 1 ;
               gridbag.setConstraints( level1CheckBoxes[ i ] , gc ) ;
               level1CheckBoxes[ i ].setToolTipText(
                   "Select this if you want to select" +
                   level1CheckBoxes[ i ].getText() + " for " +
                   selectedIdentifier ) ;
               mainPanel.add( level1CheckBoxes[ i ] , gc ) ;
            }
            // setting buttonsJPanel
            JPanel buttonPanel = new JPanel() ;
            GridBagLayout buttonGridbag = new GridBagLayout() ;
            buttonPanel.setLayout( buttonGridbag ) ;
            GridBagConstraints buttonGc = new GridBagConstraints() ;
            buttonGc.anchor = GridBagConstraints.CENTER ;
            // Button "Back"
            JButton backButton = new JButton( "Back" ,
                                              new ImageIcon( "img" +
                java.io.File.separator + "back-16.png" ) ) ;
            backButton.setToolTipText( "Click here to select an other kernel" ) ;
            // if button "Back is pressed"
            backButton.addActionListener(
                new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  // go to level 0
                  level = 0 ;
                  initLayout() ;
               }
            }
            ) ;
            // Button "Go on"
            JButton addButton = new JButton( "Go on" ,
                                             new ImageIcon( "img" +
                java.io.File.separator + "goOn-16.png" ) ) ;
            addButton.setToolTipText( "Click here to go on" ) ;

            // what to do when "go on" is pressed
            addButton.addActionListener(
                new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  // do it as thread
                  Thread t = new Thread()
                  {
                     public void run()
                     {
                        // empty the view! (nothing else can be pressed)
                        Thread t = new Thread()
                        {
                           public void run()
                           {
                              content.setViewportView( new JPanel() ) ;
                           }
                        } ;
                        SwingUtilities.invokeLater( t ) ;
                        // get the number of selected checkBoxes
                        int howManySelected = 0 ;
                        for ( int i = 0 ; i < level1CheckBoxes.length ; i++ )
                        {
                           if ( level1CheckBoxes[ i ].isSelected() )
                              howManySelected++ ;
                        }
                        // get the selected checkboxes
                        String[] selected = new String[howManySelected ] ;
                        howManySelected = 0 ;
                        for ( int i = 0 ; i < level1CheckBoxes.length ; i++ )
                        {
                           if ( level1CheckBoxes[ i ].isSelected() )
                           {
                              selected[ howManySelected ] = level1CheckBoxes[ i ].
                                  getText() ;
                              howManySelected++ ;
                           }
                        }
                        // now we can build level 2
                        try
                        {
                           // get programmnames from server
                           level2Programnames = socketCon.
                               startArchitectureSelectWork2(
                                   selectedIdentifier , selected ) ;
                           level = 2 ;
                           // build the panel
                           t = new Thread()
                           {
                              public void run()
                              {
                                 initLayout() ;
                              }
                           } ;
                           SwingUtilities.invokeLater( t ) ;

                        }
                        catch ( Exception e )
                        {
                           //System.err.println("EXCPETION");
                        }
                     }
                  } ;
                  t.start() ;
               }
            }
            ) ;
            // still buttonpanel
            buttonGc.gridx = 0 ;
            buttonGc.gridy = 0 ;
            buttonPanel.add( backButton , buttonGc ) ;
            buttonGc.gridx = 1 ;
            buttonPanel.add( addButton , buttonGc ) ;
            gc.gridx = 0 ;
            gc.gridy = level1CheckBoxes.length + 1 ;
            mainPanel.add( buttonPanel , gc ) ;
            // a button panel at the bottom
            // which is the same as above
            buttonPanel = new JPanel() ;
            buttonGridbag = new GridBagLayout() ;
            buttonPanel.setLayout( buttonGridbag ) ;
            buttonGc = new GridBagConstraints() ;
            buttonGc.anchor = GridBagConstraints.CENTER ;
            JButton backButton2 = new JButton( "Back" ,
                                      new ImageIcon( "img" +
                java.io.File.separator + "back-16.png" ) ) ;
            backButton2.setToolTipText( "Click here to select an other kernel" ) ;
            backButton2.addActionListener(backButton.getActionListeners()[0]);
            JButton addButton2 = new JButton( "Go on" ,
                                     new ImageIcon( "img" +
                java.io.File.separator + "goOn-16.png" ) ) ;
            addButton2.setToolTipText( "Click here to go on" ) ;
            addButton2.addActionListener(addButton.getActionListeners()[0]);
            buttonGc.gridx = 0 ;
            buttonGc.gridy = 0 ;
            buttonPanel.add( backButton2 , buttonGc ) ;
            buttonGc.gridx = 1 ;
            buttonPanel.add( addButton2 , buttonGc ) ;
            gc.gridx = 0 ;
            gc.gridy = 0 ;
            mainPanel.add( buttonPanel , gc ) ;
            content.setViewportView( mainPanel ) ;
         }
         break ;
         // architectureitem is selected and setted now a kernel is selected
         case 2:
         {
            JPanel mainPanel = new JPanel() ;
            GridBagLayout gridbag = new GridBagLayout() ;
            mainPanel.setLayout( gridbag ) ;
            GridBagConstraints gc = new GridBagConstraints() ;
            gc.anchor = GridBagConstraints.WEST ;
            gc.gridx = 0 ;
            JButton backButton = new JButton( "Back" ,
                                              new ImageIcon( "img" +
                java.io.File.separator + "back-16.png" ) ) ;
            backButton.setToolTipText( "Click here to select an other kernel" ) ;
            backButton.addActionListener(
                new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  level = 1 ;
                  initLayout() ;
               }
            }
            ) ;
            mainPanel.add( backButton , gc ) ;
            // select the kernel:
            // all kernelnames are displayed
            for ( int i = 0 ; i < level2Programnames.length ; i++ )
            {
               gc.gridy = i + 1 ;
               JButton jb = new JButton( level2Programnames[ i ] ) ;
               // content means this, so ArchitectureEditPanel.this.actionPerformed is called
               jb.addActionListener( content ) ;
               mainPanel.add( jb , gc ) ;
            }
            JButton backButton2 = new JButton( "Back" ,
                                               new ImageIcon( "img" +
                java.io.File.separator + "back-16.png" ) ) ;
            backButton2.setToolTipText( "Click here to select an other kernel" ) ;
            backButton2.addActionListener( backButton.getActionListeners()[ 0 ] ) ;
            gc.gridy = level2Programnames.length + 1 ;
            mainPanel.add( backButton2 , gc ) ;
            content.setViewportView( mainPanel ) ;
         }
         break ;
         // architecture is set, kernel is set. just select the functions
         case 3:

            // everything added to mainPanel
            JPanel mainPanel = new JPanel() ;

            // layout
            GridBagLayout gridbag = new GridBagLayout() ;
            mainPanel.setLayout( gridbag ) ;
            GridBagConstraints gc = new GridBagConstraints() ;
            gc.anchor = GridBagConstraints.WEST ;

            // setting legend left
            gc.gridx = 0 ;
            gc.gridy = 0 ;
            gc.weightx = 1 ;

            // still legend left
            // for all definitions of computers
            for ( int i = 0 ;
                  i < ( ( String[] ) level3Vector.elementAt( 0 ) ).length ;
                  i++ )
            {
               gc.gridy = i + 2 ;
               // looks like a label, bu its a button
               JButton jl = new JButton( ( ( String[] ) level3Vector.
                                           elementAt( 0 ) )[
                                         i ] ) ;

               final int row = i ;
               jl.setBorder( BorderFactory.createEmptyBorder() ) ;
               // if the button is pressed:
               jl.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent evt )
                  {
                     for ( int jIn = 0 ; jIn < checkBoxes.length ; jIn++ )
                        for ( int iIn = 0 ; iIn < checkBoxes[ 0 ].length ;
                              iIn++ )
                           if ( checkBoxes[ jIn ][ iIn ] != null )
                           {
                              // if CTRL is NOT pressed
                              // unselect all other selected functions
                              // and only select this row
                              if ( ( jIn != row ) &&
                                   ( ! ( ( evt.getModifiers() &
                                           ActionEvent.CTRL_MASK ) > 0 ) ) )
                              {
                                 checkBoxes[ jIn ][ iIn ].setSelected( false ) ;
                                 checkBoxes[ jIn ][ iIn ].revalidate() ;
                                 checkBoxes[ jIn ][ iIn ].repaint() ;
                              }
                              // if CTRL is selected
                              // select this row ADDITIONALLY
                              else if ( jIn == row )
                              {
                                 checkBoxes[ jIn ][ iIn ].setSelected( true ) ;
                                 checkBoxes[ jIn ][ iIn ].revalidate() ;
                                 checkBoxes[ jIn ][ iIn ].repaint() ;
                              }

                           }
                  }
               } ) ;
               // ending definig legend left
               gridbag.setConstraints( jl , gc ) ;
               mainPanel.add( jl , gc ) ;
            }

            // setting descriptions on the top
            gc.gridy = 1 ;

            // for all function descriptions (its the same like the rows above
            //, so not commented)
            for ( int i = 0 ;
                  i < ( ( String[] ) level3Vector.elementAt( 2 ) ).length ;
                  i++ )
            {
               gc.gridx = i + 1 ;
               JButton jl = new JButton( ( ( String[] ) level3Vector.
                                           elementAt( 2 ) )[
                                         i ] ) ;
               jl.setBorder( BorderFactory.createEmptyBorder() ) ;
               final int column = i ;
               // see comment above
               jl.addActionListener( new ActionListener()
               {
                  public void actionPerformed( ActionEvent evt )
                  {
                     for ( int jIn = 0 ; jIn < checkBoxes.length ; jIn++ )
                        for ( int iIn = 0 ; iIn < checkBoxes[ 0 ].length ;
                              iIn++ )
                           if ( checkBoxes[ jIn ][ iIn ] != null )
                           {
                              if ( ( iIn != column ) &&
                                   ( ! ( ( evt.getModifiers() &
                                           ActionEvent.CTRL_MASK ) > 0 ) ) )

                              {
                                 checkBoxes[ jIn ][ iIn ].setSelected( false ) ;
                                 checkBoxes[ jIn ][ iIn ].revalidate() ;
                              }
                              else if ( iIn == column )
                              {
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
            this.checkBoxes = new JCheckBox[ ( ( String[] ) level3Vector.
                                               elementAt( 0 ) ).
                length ][ ( ( String[] ) level3Vector.elementAt( 2 ) ).
                length ] ;

            for ( int i = 0 ;
                  i < ( ( String[] ) level3Vector.elementAt( 0 ) ).length ;
                  i++ )
            {
               for ( int j = 0 ;
                     j < ( ( String[] ) level3Vector.elementAt( 2 ) ).length ;
                     j++ )
               {
                  // we get a bitmask, which says which function is available for
                  // which system
                  // 5 could mean legend 0 isnt, but legend1, not legend2 but legend 3
                  // because 5 is an 0101
                  String bitmask = Long.toBinaryString( ( new Long( ( ( (
                      String[] ) ( level3Vector.elementAt( 3 ) ) )[ i ] ) ) ).
                      longValue() ) ;
                  // add beginning zeros
                  while ( bitmask.length() <
                          ( ( ( String[] ) level3Vector.elementAt( 2 ) ).
                            length ) )
                     bitmask = '0' + bitmask ;
                  gc.gridy = i + 2 ;
                  gc.gridx = j + 1 ;
                  if ( bitmask.charAt( j ) == '1' )
                  {
                     this.checkBoxes[ i ][ j ] = new JCheckBox() ;
                     gridbag.setConstraints( this.checkBoxes[ i ][ j ] , gc ) ;
                     mainPanel.add( this.checkBoxes[ i ][ j ] , gc ) ;

                  }
                  else
                     this.checkBoxes[ i ][ j ] = null ;

               }
            }

            // setting buttons
            JPanel buttonPanel = new JPanel() ;
            GridBagLayout buttonGridbag = new GridBagLayout() ;
            buttonPanel.setLayout( buttonGridbag ) ;
            GridBagConstraints buttonGc = new GridBagConstraints() ;
            buttonGc.anchor = GridBagConstraints.CENTER ;

            // back means go back to level 2
            JButton backButton = new JButton( "Back" ,
                                              new ImageIcon( "img" +
                java.io.File.separator + "back-16.png" ) ) ;
            backButton.setToolTipText(
                "Click here to select an other kernel" ) ;
            backButton.addActionListener(
                new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  // ... as you can see here
                  level = 2 ;
                  initLayout() ;
               }
            }
            ) ;

            // add all selected graphs
            JButton addButton = new JButton( "Add" ) ;
            addButton.setToolTipText(
                "Click here add the Graphs, that are defined by your settings" ) ;
            addButton.addActionListener(
                new ActionListener()
            {
               public void actionPerformed( ActionEvent evt )
               {
                  // for sure as a thread
                  final Thread t = new Thread()
                  {
                     public void run()
                     {
                        // do for every line!
                        // don't wait for everythings done, but do it "peu a peu"
                        for ( int i = 0 ; i < checkBoxes.length ; i++ )
                        {
                           // how many boxes in this line selected?
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
                           // if more then one is selected get all
                           // (little trick so dont get it twice or n-times :))
                           if ( numberOfSelected > 1 )
                           {
                              // we know the filename and the selected descriptions
                              String[] identifiers = new String[2 ] ;
                              String[] settings = new String[2 ] ;
                              identifiers[ 0 ] = "Filename" ;
                              identifiers[ 1 ] = "Description" ;
                              settings[ 0 ] = ( ( String[] ) level3Vector.
                                                get( 1 ) )[ i ] ;
                              // we set the description to <any>! (load it once, as i said)
                              settings[ 1 ] = "<any>" ;
                              // add
                              root.addGraphWithoutReloading( identifiers ,
                                  settings ) ;
                              // ohem... what's THAT???
                              // get it slow...
                              // first get the number of graphs in the functionlist
                              int base = root.root.graphData.
                                  size() ;
                              // then removing ...
                              // ... lets see...
                              // the number of possible graphs from the first file
                              base = base - checkBoxes[ 0 ].length ;
                              // then we do for the number of possible graphs from the first file
                              for ( int j = checkBoxes[ 0 ].length - 1 ;
                                    j > -1 ;
                                    j-- )
                              {
                                 // if there is a checkbox, so the graph exists
                                 if ( checkBoxes[ i ][ j ] != null )
                                 {
                                    // but the checkbox isn't selected
                                    if ( !checkBoxes[ i ][ j ].isSelected() )
                                    {
                                       // then we select the graph
                                       root.root.graphData.
                                           setSelectedGraph( root.root.

                                           graphData.getGraph( base + j ) ) ;
                                       // and remove it
                                       root.root.graphData.
                                           removeSelectedGraph() ;
                                    }
                                 }
                              }
                              // ... so ... got it?
                              // yeah it's simple. the trick we were beginning
                              // (you remember loading just once ;))
                              // we remove the not needed functions
                              // and when thats done
                              // update the view
                              root.root.updateView( root.root.
                                  graphData ) ;

                           }
                           // so now without the trick ...
                           // is it to simple?
                           else
                           {
                              // yah we know... for all files ... blah
                              for ( int j = 0 ; j < checkBoxes[ 0 ].length ;
                                    j++ )
                              {
                                 // check which is selected ... (BORING!)
                                 if ( checkBoxes[ i ][ j ] != null )
                                 {
                                    if ( checkBoxes[ i ][ j ].isSelected() )
                                    {
                                       String[] identifiers = new String[2 ] ;
                                       String[] settings = new String[2 ] ;
                                       identifiers[ 0 ] = "Filename" ;
                                       identifiers[ 1 ] = "Description" ;
                                       settings[ 0 ] = ( ( String[] )
                                           level3Vector.
                                           get( 1 ) )[ i ] ;
                                       settings[ 1 ] = ( ( String[] )
                                           level3Vector.
                                           get( 2 ) )[ j ] ;
                                       // aadd it
                                       root.addGraph( identifiers , settings ) ;

                                    }
                                 }

                              }
                           }
                        }
                        // a thread which repaints this panel
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
                  // start the add-tread
                  t.start() ;
               }
            }
            ) ;

            // go to level 0
            JButton backToStartButton = new JButton( "Back to Begin" ,
                new
                ImageIcon( "img" + java.io.File.separator +
                           "backToBegin-16.png" ) ) ;
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
            gc.weightx = ( ( String[] ) level3Vector.elementAt( 2 ) ).length ;
            gc.gridx = 0 ;
            gc.gridy = ( ( String[] ) level3Vector.elementAt( 0 ) ).length +
                2 ;

            // the top-buttons
            mainPanel.add( buttonPanel , gc ) ;

            // the bottom buttons
            buttonPanel = new JPanel() ;
            buttonGridbag = new GridBagLayout() ;
            buttonPanel.setLayout( buttonGridbag ) ;
            buttonGc = new GridBagConstraints() ;
            buttonGc.anchor = GridBagConstraints.CENTER ;
            JButton backButton2 = new JButton( "Back" ,
                                               new ImageIcon( "img" +
                java.io.File.separator + "back-16.png" ) ) ;
            backButton2.setToolTipText(
                "Click here to select an other kernel" ) ;
            backButton2.addActionListener( backButton.getActionListeners()[ 0 ] ) ;

            JButton addButton2 = new JButton( "Add" ) ;
            addButton2.setToolTipText(
                "Click here add the Graphs, that are defined by your settings" ) ;
            addButton2.addActionListener( addButton.getActionListeners()[ 0 ] ) ;
            JButton backToStartButton2 = new JButton( "Back to Begin" ,
                new ImageIcon( "img" +
                               java.io.File.separator + "backToBegin-16.png" ) ) ;
            backToStartButton2.setToolTipText(
                "Click here to return to the beginning of the selection" ) ;
            backToStartButton2.addActionListener( backToStartButton.
                                                  getActionListeners()[ 0 ] ) ;

            buttonGc.gridx = 0 ;
            buttonGc.gridy = 0 ;
            buttonPanel.add( backButton2 , buttonGc ) ;
            buttonGc.gridx = 2 ;
            buttonPanel.add( addButton2 , buttonGc ) ;
            buttonGc.gridx = 1 ;
            buttonPanel.add( backToStartButton2 , buttonGc ) ;

            gc.weightx = ( ( String[] ) level3Vector.elementAt( 2 ) ).length ;
            gc.gridx = 0 ;
            gc.gridy = 0 ;
            mainPanel.add( buttonPanel , gc ) ;
            content.setViewportView( mainPanel ) ;
            break ;

         }
             }
   /**
    * Performs some Actions, which can occure while working with this panel
    * @param evt the Event, that calls this procedure
    */
   public void actionPerformed( final ActionEvent evt )
   {
      // I'm a 5th level vice-president ;)
      switch ( this.level )
      {
         // begin
         case 0:

            // empty the viewport
            Thread t = new Thread()
            {
               public void run()
               {
                  content.setViewportView( new JPanel() ) ;

               }
            } ;
            SwingUtilities.invokeLater( t ) ;

            // do what should be done after level 0 (learn new classes? nah)
            t = new Thread()
            {
               public void run()
               {

                  try
                  {
                     // which architectureitem was selected?
                     selectedIdentifier = ( ( JButton ) evt.getSource() ).
                         getText() ;
                     // what tells us the server
                     String[] identifiers2 = socketCon.
                         startArchitectureSelectWork1(
                             selectedIdentifier ) ;
                     // okay, now we have everything for level 1
                     level1CheckBoxes = new JCheckBox[identifiers2.length ] ;
                     // we just have to create comboboxes for this
                     for ( int i = 0 ; i < identifiers2.length ; i++ )
                     {
                        level1CheckBoxes[ i ] = new JCheckBox( identifiers2[ i ] ) ;
                     }
                  }
                  catch ( Exception e )
                  {
                     // we never make mistkaes ;)
                  }
                  // new level.
                  level = 1 ;
                  // so the viewport should look like a level-1-vp
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
            break ;
         case 2:

            // need a little for level up, so hide and show empty
            t = new Thread()
            {
               public void run()
               {
                  content.setViewportView( new JPanel() ) ;
               }
            } ;
            SwingUtilities.invokeLater( t ) ;

            // now prepare ... for level-up
            ( new Thread()
            {
               public void run()
               {
                  // which program was selected?
                  String selectedProgram = ( ( JButton ) evt.getSource() ).
                      getText() ;
                  //from which selected architecture-items
                  int howManySelected = 0 ;
                  for ( int i = 0 ; i < level1CheckBoxes.length ; i++ )
                  {
                     if ( level1CheckBoxes[ i ].isSelected() )
                        howManySelected++ ;
                  }
                  String[] selected = new String[howManySelected ] ;
                  howManySelected = 0 ;
                  for ( int i = 0 ; i < level1CheckBoxes.length ; i++ )
                  {
                     if ( level1CheckBoxes[ i ].isSelected() )
                     {
                        selected[ howManySelected ] = level1CheckBoxes[ i ].
                            getText() ;
                        howManySelected++ ;
                     }
                  }
                  // ask the server for what files he has for this selection
                  try
                  {
                     level3Vector = socketCon.startArchitectureSelectWork3(
                         selectedIdentifier , selected , selectedProgram ) ;
                  }
                  catch ( Exception e )
                  {
                     // no exception
                  }
                  // now we are level 3. preparing fireball
                  level = 3 ;
                  // try to look nice with a lot of checkboxes (see initlayout ;))
                  Thread t = new Thread()
                  {
                     public void run()
                     {
                        initLayout() ;
                     }
                  } ;
                  SwingUtilities.invokeLater( t ) ;
               }
            } )
                .start() ;
            break ;

      }

   }

}
