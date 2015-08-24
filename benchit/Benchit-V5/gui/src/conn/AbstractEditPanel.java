package conn;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;


/**
 *
 * <p>Überschrift: The AbstractEditPanel</p>
 * <p>Beschreibung: Here you can select your Graph with the settings you search for</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR</p>
 * @author rschoene
 * @version 1.0
 */



public class AbstractEditPanel extends JScrollPane implements ItemListener,ActionListener
{
        /** needed for no doublefiring a changeEvent */

        private String lastSettedString=new String();
        /** needed for no doublefiring a changeEvent */
        private JComboBox lastChangedComboBox=new JComboBox();

        /** the panel, where all editpanels can be selected and so on */
        private MainEditPanel root;

        // this will contain
        //vector v: element at 0:String[] identifiers,elementAt 1:JComboBox[] entries
        private Vector selectedComboBoxes=new Vector();


        /** communicate over this */
        private SocketCon socketCon;

        /** clear selection */
	private JButton clearButton;
        /** add graph */
	private JButton addButton;
        /** not used */
	private JButton previewButton;


        /**
         * constructor
         * @param socketCon the socketCon of the program
         * @param root the MainEditPanel of the program
         * @param identifiers identifiers (if you have loaded them from the db)
         */
        public AbstractEditPanel(SocketCon socketCon, MainEditPanel root,String[] identifiers)
        {
                this.socketCon=socketCon;
                this.root=root;
                this.getVerticalScrollBar().setUnitIncrement( 15 );
                this.selectedComboBoxes.add(identifiers);
                this.selectedComboBoxes.add(new JComboBox[identifiers.length]);
                this.fillAllChoices();
        }

        /**
         * fills the jComboBoxes with selected <any>
         */
        private void fillAllChoices()
        {
           // start a thread, so the gui can run on...
           ( new Thread()
           {
              public void run()
              {
                 // if the length of comboboxes is zero (none are selected)
                 if ( ( ( String[] )
                        selectedComboBoxes.elementAt( 0 ) ).length == 0 )
                 {
                    initLayout() ;
                    enableItemListener() ;
                    return ;
                 }
                 // else: we add anys
                 // get name of comboboxes and the number of comboboxes
                 String[] anys = new String[ ( ( String[] ) selectedComboBoxes.
                                               elementAt( 0 ) ).length ] ;
                 // for all comboboxes
                 for ( int i = 0 ; i < anys.length ; i++ )
                 {
                    // if the setting for combobox i is not null
                    if ( ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[ i ] != null )
                       // it is disabled
                       ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[ i ].
                           setEnabled( false ) ;
                    // and the name of the combobox is setted to any
                    anys[ i ] = "<any>" ;
                 }
                 // get from the server
                 Vector v = null ;
                 try
                 {
                    // send him the combobox names (what is asked),
                    // the combobox names (what is defined)
                    // and the setting (definitions for 2nd argument)
                    v = socketCon.getComboBoxContent( ( ( String[] )
                        selectedComboBoxes.elementAt( 0 ) ) ,
                                                      ( ( String[] )
                        selectedComboBoxes.elementAt( 0 ) ) ,
                                                      anys ) ;
                 }
                 catch ( IOException ex )
                 {
                    // we hope no exception occures
                 }
                 // for all comboboxes
                 for ( int i = 0 ;
                       i < ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) ).
                       length ; i++ )
                 {
                    // set content of combobox i
                    ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[ i ] = new
                        JComboBox( ( ( Vector[] ) v.elementAt( 1 ) )[ i ] ) ;
                    // enable combobox i
                    ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[ i ].
                        setEnabled( true ) ;
                 }
                 // reinit layout
                 initLayout() ;
                 // enable change and item listener
                 enableItemListener() ;
              }
           } ).start() ;

        }
        /**
         * fills the jComboBoxes and lets selected what is
         */
        private void fillChoices()
        {
           // disable all comboboxes
           for ( int i = 0 ;
                 i <
                 ( ( JComboBox[] )this.selectedComboBoxes.elementAt( 1 ) ).length ;
                 i++ )
           {
              ( ( JComboBox[] )this.selectedComboBoxes.elementAt( 1 ) )[ i ].
                  setEnabled( false ) ;
           }
              // disable buttons
           try
           {
              this.addButton.setEnabled( false ) ;
              this.clearButton.setEnabled( false ) ;
              this.previewButton.setEnabled( false ) ;
           }
           // what should go wrong???
           catch ( Exception ignored )
           {}
           // to what is setted
           final String[] settedSettings = new String[ ( ( String[] )
               selectedComboBoxes.elementAt( 0 ) ).length ] ;
           // which comboboxes are set
           String[] settedIdentifiers = new String[ ( ( String[] )
               selectedComboBoxes.elementAt( 0 ) ).length ] ;
           // what is interesting to get
           String[] toGetIdentifiers = new String[ ( ( String[] )
               selectedComboBoxes.elementAt( 0 ) ).length ] ;
           // set setted identifiers
           settedIdentifiers = ( ( String[] ) selectedComboBoxes.elementAt( 0 ) ) ;
           // set which identifiers should be get
           toGetIdentifiers = ( ( String[] ) selectedComboBoxes.elementAt( 0 ) ) ;
           // set them to final, so an inner thread can access them
           final String[] finalSettedIdentifiers = settedIdentifiers ;
           final String[] finalToGetIdentifiers = toGetIdentifiers ;
           // set what is selected in the comboboxes
           for ( int i = 0 ; i < settedSettings.length ; i++ )
           {
              settedSettings[ i ] = ( String ) ( ( ( JComboBox[] )
                  selectedComboBoxes.elementAt( 1 ) )[ i ] ).getSelectedItem() ;
           }
           // the inner thread
           Thread t1 = new Thread()
           {
              public void run()
              {
                 try
                 {
                    // get what the server sends back
                    Vector nameAndEntries = null ;
                    try
                    {
                       // send him which comboboxes are interesting (shall be get)
                       // what is set and to what it is set
                       nameAndEntries = socketCon.getComboBoxContent(
                           finalToGetIdentifiers , finalSettedIdentifiers ,
                           settedSettings ) ;
                    }
                    catch ( NullPointerException ex )
                    {
                       // should not occur, but the server is still on :)
                       ex.printStackTrace() ;
                       JOptionPane.showMessageDialog( null ,
                           "The selected item caused an internal error. Please choose another one." ) ;
                       // enabling everything
                       for ( int i = 0 ;
                             i <
                             ( ( JComboBox[] ) selectedComboBoxes.
                               elementAt( 1 ) ).length ; i++ )
                       {
                          ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[
                              i ].setEnabled( true ) ;
                       }
                       addButton.setEnabled( true ) ;
                       clearButton.setEnabled( true ) ;
                       return ;

                    }
                    // the new comboboxes
                    JComboBox[] jcbs = new JComboBox[ ( ( Vector[] )
                        nameAndEntries.elementAt( 1 ) ).length ] ;
                    // set the contents of the new boxes
                    for ( int i = 0 ;
                          i <
                          ( ( Vector[] ) nameAndEntries.elementAt( 1 ) ).length ;
                          i++ )
                    {
                       jcbs[ i ] = new JComboBox( ( ( Vector[] ) nameAndEntries.
                           elementAt( 1 ) )[ i ] ) ;
                    }
                    // remove the old boxes
                    selectedComboBoxes.removeElementAt( 1 ) ;
                    // and set the new ones
                    selectedComboBoxes.add( jcbs ) ;
                 }
                 catch ( java.io.IOException e )
                 {
                    // AAAH an error! Which one?
                    System.err.println( e ) ;
                 }
                 // no event shall be occure
                 disableItemListener() ;
                 // find the corresponding received comboboxes
                 // and set the selected value
                 for ( int i = 0 ; i < finalSettedIdentifiers.length ; i++ )
                 {
                    for ( int j = 0 ;
                          j <
                          ( ( String[] ) selectedComboBoxes.elementAt( 0 ) ).length ;
                          j++ )
                    {
                       if ( finalSettedIdentifiers[ i ].equals( ( ( String[] )
                           selectedComboBoxes.elementAt( 0 ) )[ j ] ) )
                       {
                          ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[
                              j ].setSelectedItem( settedSettings[ i ] ) ;
                          ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[
                              j ].setToolTipText( settedSettings[ i ] ) ;
                       }
                    }
                 }
                 initLayout() ;
                 // now events are welcome again :)
                 enableItemListener() ;
			}
		};
                // start the thread above
		t1.start();

        }

        /**
         * repaints the panel / resets the panel
         */
        public void initLayout()
        {
           int i = 0 ;
           // where everything is added
           JPanel mainPanel = new JPanel() ;
           // this is just the scrollpane around the mainPanel
           this.setViewportView( mainPanel ) ;
           // layout
           GridBagLayout gridbag = new GridBagLayout() ;
           mainPanel.setLayout( gridbag ) ;
           GridBagConstraints gc = new GridBagConstraints() ;
           gc.anchor = GridBagConstraints.WEST ;
           // for: add all comboboxes to the mainpanel
           for ( i = 0 ;
                 i < ( ( String[] ) ( this.selectedComboBoxes.elementAt( 0 ) ) ).length ;
                 i++ )
           {

              // Layout:
              // 0	identifier[0] jComboBox(0)
              // 1	identifier[1] jComboBox(1)
              //		...
              // n	identifier[n] jComboBox(n)

              // activated ComboBox Vector
              // elementAt(n)=v1
              //		v1.elementAt(0)=identifier
              //		v1.elementAt(1)=jComboBox
              gc.gridx = 0 ;
              gc.gridy = i ;
              mainPanel.add( new JLabel( ( ( String[] ) ( this.selectedComboBoxes.
                  elementAt( 0 ) ) )[ i ] ) , gc ) ;
              gc.gridx = 1 ;
              mainPanel.add( ( ( ( JComboBox[] ) ( this.selectedComboBoxes.elementAt(
                  1 ) ) )[ i ] ) , gc ) ;
              ( ( ( JComboBox[] ) ( this.selectedComboBoxes.elementAt( 1 ) ) )[ i ] ).
                  addItemListener( this ) ;
              //this.selectedComboBoxes.add(actComBoxVector);
           }
           // add the buttonpanel to the mainpanel
           // first init
           JPanel buttonPanel = new JPanel() ;
           GridBagLayout buttonGridbag = new GridBagLayout() ;
           buttonPanel.setLayout( buttonGridbag ) ;
           GridBagConstraints buttonGc = new GridBagConstraints() ;
           buttonGc.anchor = GridBagConstraints.CENTER ;
           // second create buttons
           clearButton = new JButton( "Clear" ) ;
           clearButton.setToolTipText(
               "Click here to set all ComboBoxes to \"<any>\"" ) ;
           clearButton.addActionListener( this ) ;
           addButton = new JButton( "Add" ) ;
           addButton.setToolTipText(
               "Click here add the Graph that is defined by your settings" ) ;
           addButton.addActionListener( this ) ;
           previewButton = new JButton( "Preview" ) ;
           previewButton.addActionListener( this ) ;
           // third add all
           buttonGc.gridx = 0 ;
           buttonGc.gridy = 0 ;
           buttonPanel.add( clearButton ) ;
           buttonGc.gridx = 1 ;
           buttonPanel.add( new JPanel() ) ;
           buttonGc.gridx = 2 ;
           buttonPanel.add( addButton , buttonGc ) ;
           buttonGc.gridx = 3 ;
           gc.weightx = 2 ;
           gc.gridy = i + 1 ;
           // last add buttonPanel to mainPanel
           mainPanel.add( buttonPanel , gc ) ;
           // always display scrollbars
           this.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS ) ;
           this.setHorizontalScrollBarPolicy( JScrollPane.
                                              HORIZONTAL_SCROLLBAR_ALWAYS ) ;
           // more layout
           double[] temp =
               {
               .05 , .95} ;
           // +2: one because an array starts with 0 and one because of the buttons
           double[] temp2 = new double[i + 2 ] ;
           for ( int j = 0 ; j < temp2.length ; j++ )
           {
              temp2[ i ] = 1 / temp2.length ;
           }
           gridbag.columnWeights = temp ;
           gridbag.rowWeights = temp2 ;
        }

        /**
        * this enables the ItemListeners of all JComboBoxes
        */
       private void enableItemListener()
       {
          // for all comboboxes add itemlistener
          for ( int i = 0 ;
                i <
                ( ( String[] ) ( this.selectedComboBoxes.elementAt( 0 ) ) ).length ;
                i++ )
          {
             ( ( JComboBox[] ) ( this.selectedComboBoxes.elementAt( 1 ) ) )[ i ].
                 addItemListener( this ) ;
          }
       }
        /**
        * this disables the ItemListeners of all JComboBoxes
        */
       private void disableItemListener()
       {
          // for all comboboxes remove itemlistener
          for ( int i = 0 ;
                i <
                ( ( String[] ) ( this.selectedComboBoxes.elementAt( 0 ) ) ).length ;
                i++ )
          {
             ( ( JComboBox[] ) ( this.selectedComboBoxes.elementAt( 1 ) ) )[ i ].
                 removeItemListener( this ) ;
          }
        }
        /**
        * tis is what is called if we select s.th. in a comboBox
        * @param ie the Event, that happens, when s.th. was selected in the combobox
        */
        public void itemStateChanged(ItemEvent ie)
        {
           // it may be, that one changed comboBox fires more then one itemEvent
           synchronized ( this )
           {
              // it still may be!!! (do you know why?)
              if ( ! ( ( this.lastSettedString.equals( ( String ) ( ( JComboBox )
                  ie.getSource() ).getSelectedItem() ) ) &&
                       ( this.lastChangedComboBox == ie.getSource() ) ) )
              {
                 // a really interesting event ;)
                 // disable following events
                 this.disableItemListener() ;
                 // set the new selected item as last setted string
                 this.lastSettedString = ( String ) ( ( JComboBox ) ie.
                     getSource() ).getSelectedItem() ;
                 // and also save the setted box als last setted
                 this.lastChangedComboBox = ( JComboBox ) ie.getSource() ;
                 // rebuild (get from server ...)
                 this.fillChoices() ;
                 // enable itemlistener again
                 this.enableItemListener() ;
              }
           }
        }
        /**
         * performs some action from the buttons
         * @param evt the Event, which occured
         */
        public void actionPerformed( ActionEvent evt )
        {
           synchronized ( this )
           {
              // for all comboboxes: disable!!
              for ( int i = 0 ;
                    i <
                    ( ( JComboBox[] )this.selectedComboBoxes.elementAt( 1 ) ).length ;
                    i++ )
              {
                 ( ( JComboBox[] )this.selectedComboBoxes.elementAt( 1 ) )[ i ].
                     setEnabled( false ) ;
              }
              // disable buttons
              this.addButton.setEnabled( false ) ;
              this.clearButton.setEnabled( false ) ;
              this.previewButton.setEnabled( false ) ;
              // which button was pressed?
              String source = ( ( JButton ) evt.getSource() ).getText() ;
              if ( source.equals( "Clear" ) )
              {
                 fillAllChoices() ;
              }
              if ( source.equals( "Add" ) )
              {
                 addGraph() ;
              }
              if ( source.equals( "Preview" ) )
              {
                 // could be added one day. COULD!
                 //startPreviewWork();
              }
           }
        }


        /**
        * adds the selected Graph if possible
        * if not it prints s.th on System.err
        * THIS should be used ;)
        */
       private void addGraph()
       {
          ( new Thread()
          {
             public void run()
             {
                // settings
                String[] comboBoxSettings = new String[ ( ( String[] )
                    selectedComboBoxes.elementAt( 0 ) ).length ] ;
                // setted identifiers
                String[] comboBoxIdentifiers = new String[ ( ( String[] )
                    selectedComboBoxes.elementAt( 0 ) ).length ] ;
                // set 'em
                for ( int i = 0 ;
                      i < ( ( String[] ) selectedComboBoxes.elementAt( 0 ) ).length ;
                      i++ )
                {
                   comboBoxIdentifiers[ i ] = ( ( String[] ) selectedComboBoxes.
                                                elementAt( 0 ) )[ i ] ;
                   comboBoxSettings[ i ] = ( String ) ( ( ( ( JComboBox[] )
                       selectedComboBoxes.elementAt( 1 ) )[ i ] ).getSelectedItem() ) ;
                }
                // add the graph(s)
                root.addGraph( comboBoxIdentifiers , comboBoxSettings ) ;
                // enabling buttons
                addButton.setEnabled( true ) ;
                clearButton.setEnabled( true ) ;
                previewButton.setEnabled( true ) ;
                // enabling comboboxes
                for ( int i = 0 ;
                      i <
                      ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) ).length ;
                      i++ )
                {
                   ( ( JComboBox[] ) selectedComboBoxes.elementAt( 1 ) )[ i ].
                       setEnabled( true ) ;
                }
             }
          } ).start() ;
       }

}
