/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGOptionsDialog.java
*
*  Author: Robert Schoene
*  Last change by: $Author: tschuet $
*  $Revision: 1.4 $
*  $Date: 2007/07/10 10:41:30 $
*
******************************************************************************/
package gui;


import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

public class BIGOptionsDialog extends JDialog {
   private final ArrayList tabNames = new ArrayList();
   private final Map benchitGUIMap = new HashMap();
   private final Map networkMap = new HashMap();
   private final Map tipsMap = new HashMap();
   private final Map consoleMap = new HashMap();
   private final Map compileAndRunMap = new HashMap();
   private final Map remoteCompileAndRunMap = new HashMap();

   private Vector whichSet = null;
   private Vector whichAttribute = null;
   private BIGKernelTree kernelTree = null;
   /**
    * The constructor.
    * @param f the JFrame owning the dialog.
    * @param whichAttribute A Vector of attribute String names.
    * @param whichSet A Vector of attribute String values.
    **/
   public BIGOptionsDialog( Frame f,
      Vector whichAttribute, Vector whichSet, BIGKernelTree kernelTree )
   {
      super( f );
      this.kernelTree = kernelTree;
      this.whichSet = whichSet;
      this.whichAttribute = whichAttribute;
      // set up the content lists for the dialog
      fillMapsAndLists();
      // the content panel for the dialog
      JPanel panel = new JPanel( new GridBagLayout() );
      GridBagConstraints c = new GridBagConstraints();
//      c.fill = GridBagConstraints.HORIZONTAL;
      c.weightx = 1.0;
      c.weighty = 1.0;
      c.gridx = 0;
      c.gridy = 0;
      c.gridwidth = 2;
      final JTabbedPane tab = new JTabbedPane( JTabbedPane.LEFT );
/*      int maxRows = benchitGUIMap.size();
      if ( networkMap.size() > maxRows ) maxRows = networkMap.size();
      if ( tipsMap.size() > maxRows ) maxRows = tipsMap.size();
      if ( consoleMap.size() > maxRows ) maxRows = consoleMap.size();
      if ( adminGUIMap.size() > maxRows ) maxRows = adminGUIMap.size() ;
      if ( compileAndRunMap.size() > maxRows ) maxRows = compileAndRunMap.size() ;
      if ( remoteCompileAndRunMap.size() > maxRows ) maxRows =
          remoteCompileAndRunMap.size() ;*/

      // create the dialog GUI content for each tab
      for ( int i = 0; i < tabNames.size(); i ++ ) {
         // get the tab name
         String tabName = (String)tabNames.get( i );
         // create the tab's panel
         JPanel pan = new JPanel( );
         // add attribute components
         if ( tabName.compareTo( "BenchIT-GUI" ) == 0 ) {
            pan.setLayout(new GridLayout( benchitGUIMap.size(), 2 ) );
            addAttributeComponents( pan, benchitGUIMap );
         }
         if ( tabName.compareTo( "Execute" ) == 0 ) {
            pan.setLayout(new GridLayout( compileAndRunMap.size(), 2 ) );
            addAttributeComponents( pan, compileAndRunMap );
         }
         if ( tabName.compareTo( "Remote-Execute" ) == 0 ) {
            pan.setLayout(new GridLayout( remoteCompileAndRunMap.size(), 2 ) );
            addAttributeComponents( pan, remoteCompileAndRunMap );
         }
         else if ( tabName.compareTo( "Network" ) == 0 ) {
            pan.setLayout(new GridLayout( networkMap.size(), 2 ) );
            addAttributeComponents( pan, networkMap );
         }
         else if ( tabName.compareTo( "Tips" ) == 0 ) {
            pan.setLayout(new GridLayout( tipsMap.size(), 2 ) );
            addAttributeComponents( pan, tipsMap );
         }
         else if ( tabName.compareTo( "Console" ) == 0 ) {
            pan.setLayout(new GridLayout( consoleMap.size(), 2 ) );
            addAttributeComponents( pan, consoleMap );
         }
         tab.addTab( tabName, pan );
      }
      // add the tabbed pane to the panel
      panel.add( tab, c );
      // add the okay button to the bottom left
      c.gridwidth = 1;
      c.gridx = 0;
      c.gridy = 1;
//      c.fill = GridBagConstraints.HORIZONTAL;
      JButton button = new JButton("Okay");
      button.addActionListener( new java.awt.event.ActionListener()
      {
         public void actionPerformed( java.awt.event.ActionEvent aev )
         {
            for ( int j = 0; j < tab.getTabCount(); j++ )
            {
               JPanel pan = (JPanel)tab.getComponentAt( j );
               for ( int i = 0 ; i < pan.getComponentCount() - 1 ; i = i + 2 )
               {
                  JLabel lab = (JLabel)pan.getComponent( i );
                  JComponent com = (JComponent)pan.getComponent( i + 1 );
                  String tip = com.getToolTipText();
                  String choice = null;
                  if ( tip.startsWith( "Check " ) && ( com instanceof JCheckBox ) )
                  {
                     // it's a check box
                     JCheckBox box = (JCheckBox)com;
                     if ( box.isSelected() ) choice = "1";
                     else choice = "0";
                  }
                  else if ( tip.startsWith( "The " ) && ( com instanceof JTextField ) )
                  {
                     // it's a text field with a String
                     JTextField txt = (JTextField)com;
                     choice = txt.getText();
                  }
                  else if ( com instanceof JTextField )
                  {
                     // it's a text field with an Integer
                     JTextField txt = (JTextField)com;
                     try {
                        new Integer( txt.getText() );
                        choice = txt.getText();
                     }
                     catch ( NumberFormatException nfe ) {
                        choice = "0";
                     }
                  }
                  else
                  {
                     continue;
                  }
                  set( lab.getText(), choice );
               }
            }
            setVisible( false );
         }
      } );
      panel.add( button, c ) ;
      // add cancel button to the bottom right
      c.gridx = 1;
      button = new JButton( "Cancel" );
      button.addActionListener( new java.awt.event.ActionListener()
      {
         public void actionPerformed( java.awt.event.ActionEvent aev )
         {
            setVisible( false );
         }
      } );
      panel.add( button, c ) ;
      // set up the dialog
      setResizable( true );
      setContentPane( panel );
      setTitle( "BenchIT Preferences" );
      pack();
      setVisible( true );
   }

   /**
    * Adds the attributes in the map to the panel.
    **/
   private void addAttributeComponents( JPanel pan, Map map )
   {
      for ( int j = 0; j < whichSet.size(); j++ ) {
         // create the label
         String labText = (String)whichAttribute.get( j );
         JLabel lab = new JLabel( labText );
         // create the value's component
         if ( map.containsKey( labText ) ) {
            JComponent com = createValueComponent( map, labText, j );
            pan.add( lab );
            pan.add( com );
         }
      }
   }
   /**
    * Creates the Component displaying the attribute's value.
    * If the attribute's tool tip starts with "Check " the
    * Component will be a check box.
    * If the tool tip starts with "The " the component will
    * be a text field with a String.
    * Every other case will default to a text field for Integer.
    * @return null, if the map doesn't contain the attribute's name,
    * the JComponent else.
    **/
   private JComponent createValueComponent( Map map, String labText, int j )
   {
      JComponent com = null;
      String tip = (String)map.get( labText );
      if ( tip.startsWith( "Check " ) ) {
         com = new JCheckBox();
         if ( ((String)whichSet.get( j )).compareTo( "0" ) == 0 )
            ((JCheckBox)com).setSelected( false );
         else
            ((JCheckBox)com).setSelected( true );
      }
      else {
         com = new JTextField( (String)whichSet.get( j ) );
      }
      com.setToolTipText( tip );
      return com;
   }

   /**
    * This method sets a new Value to a given attribute.
    **/
   private void set( String a , String b )
   {
      boolean foundIt = false ;
      // we search in the attribute-names
      for ( int i = 0; i < whichAttribute.size(); i++ )
      {
         //until we found a
         if ( ( (String)whichAttribute.get( i ) ).equals( a ) )
         {
            //now we set the content of the belonging entry in whichSet to b
            boolean kernelTreeNeedsUpdate = false;
            if ( a.compareTo( "sourceLanguages" ) == 0 ) {
                // if there was a change here, we might need to update the tree
                if ( ((String)whichSet.get( i )).compareTo( b ) != 0 ) {
                    kernelTreeNeedsUpdate = true;
                }
            }
            whichSet.setElementAt( b, i );
            if ( kernelTreeNeedsUpdate ) {
                kernelTree.updateKernelTree();
            }
            foundIt = true;
            // and end the for loop
            break;
         }
      }
      if ( !foundIt ) { // it's a new attribute -> add it to the config file
         whichAttribute.add( a );
         whichSet.add( b );
      }
   }
   /**
    * Sets up the mappings of attribute names and their tool
    * tips as well as the names of the tabbs.
    **/
   private void fillMapsAndLists() {
      tabNames.add( "BenchIT-GUI" );
      tabNames.add( "Network" );
      tabNames.add( "Tips" );
      tabNames.add( "Console" );
      tabNames.add( "Execute" );
      tabNames.add( "Remote-Execute" );

      // do not display admin tool configuration as it's integrated into the GUI
//       tabNames.add( "Admin-GUI" );
      // If the tool tip starts with "Check " a checkbox will be shown.
      // If the tool tip starts with "The " it's refered to as String type.
      // If the tool tip starts with something else it's refered to as int type.
      // Comment put statements of gui options you don't want to see
      // in the option dialog!
      benchitGUIMap.put( "scaleShape" ,
                         "Size of the points in plots (default is 6)." ) ;
      benchitGUIMap.put( "errorInt" ,
                         "Which value to use for invalid points (should be << 0)." ) ;
      benchitGUIMap.put( "numberOfMixers" ,
                         "Insert the number of mixers, you'd like to use (updated with restart)." ) ;

      benchitGUIMap.put( "loadAndSaveSettings" ,
                         "Check this if you'd like to save and load changes in result-view." ) ;

      benchitGUIMap.put( "editorTextSize" ,
                         "Text size for editors." ) ;
      benchitGUIMap.put( "consoleTextSize" ,
         "Text size for console (needs restart)." ) ;

      benchitGUIMap.put( "kernelSourceEdit",
         "Check this to see and edit the kernel source files." );
      networkMap.put( "serverIP",
         "The server's IP or name for database access." );
      networkMap.put( "updateServer",
                      "The server directory, where the GUI can find updates.");
      networkMap.put( "updateEnabled",
         "Check to enable update-check when starting." );
      networkMap.put( "showRemoteTypeDialog",
         "Check this show \"enter password\" reminder dialog." );
      tipsMap.put( "showTipsOnStartUp",
         "Check this to display the daily tip on start up." );
      tipsMap.put( "nextTip",
         "This is the number of the next tip to show." );
      tipsMap.put( "tipFileBase",
         "The relative path to the tip html files." );
      tipsMap.put( "tipIconBase",
         "The relative path to the tip icon." );
      consoleMap.put( "openConsole",
         "Check to open the output console on start up in external window." );
      consoleMap.put( "consoleWindowXPos",
         "Console x-position of top left corner." );
      consoleMap.put( "consoleWindowYPos",
         "Console x-position of top left corner." );
      consoleMap.put( "consoleWindowXSize",
         "Console window width." );
      consoleMap.put( "consoleWindowYSize",
         "Console window height." );
      compileAndRunMap.put( "settedTargetActiveCompile" ,
                            "Check to activate the target flag for compiling" ) ;
      compileAndRunMap.put( "settedTargetCompile" ,
                            "The target flag for compiling" ) ;
      compileAndRunMap.put( "settedTargetActiveRun" ,
                            "Check to activate the target flag for running" ) ;
      compileAndRunMap.put( "settedTargetRun" ,
                            "The target flag for running" ) ;
      compileAndRunMap.put( "runWithoutParameter" ,
                            "Check to run executables with the Parameters from Compile-time" ) ;

      compileAndRunMap.put("shutDownForExec",
                           "Check to shutdown the GUI for local measurements.");

      compileAndRunMap.put("askForShutDownForExec",
                           "Check to activate a dialog which asks whether to shut-down, when starting a kernel locally.");


      remoteCompileAndRunMap.put( "settedTargetActiveCompileRemote" ,
                            "Check to activate the target flag for remote compiling" ) ;
      remoteCompileAndRunMap.put( "settedTargetCompileRemote" ,
                            "The target flag for remote compiling" ) ;
      remoteCompileAndRunMap.put( "settedTargetActiveRunRemote" ,
                                  "Check to activate the target flag for remote running" ) ;
      remoteCompileAndRunMap.put( "settedTargetRunRemote" ,
                                  "The target flag for remote running" ) ;
      remoteCompileAndRunMap.put( "runRemoteWithoutParameter" ,
                                  "Check to run remote-executables with the Parameters from Compile-time" ) ;

   }
}
