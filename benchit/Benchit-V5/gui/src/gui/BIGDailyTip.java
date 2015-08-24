/*********************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGDailyTip.java
*
*  Author: Robert Wloch
*  Last change by: $Author: tschuet $
*  $Revision: 1.2 $
*  $Date: 2007/05/08 09:39:56 $
*
*********************************************************************/
package gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Random;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import system.BIGInterface;

/** Definition of the daily tip dialog. */
public class BIGDailyTip extends JDialog {
   /** Random number generator. */
   public static final Random rand = new Random( System.currentTimeMillis() );
   private final int HEIGHT = 230;
   private final int INSET = 10;
   private GUIIconPanel iconPanel;
   private JEditorPane helpText;
   private URL[] helpTexts;
   private String helpTextBase = null;
   private String helpIconBase = null;
   private int nextHelpText = 0;
   private boolean showTips = false;

   private BIGInterface bigInterface = BIGInterface.getInstance();
   private system.BIGConfigFileParser parser;
   public static final String configFileKeyShow = "showTipsOnStartUp";
   public static final String configFileKeyNext = "nextTip";
   public static final String configFileTipFileBase = "tipFileBase";
   public static final String configFileTipIconBase = "tipIconBase";

   public BIGDailyTip( Frame parent ) {
      this( parent, false );
   }
   public BIGDailyTip( Frame parent, boolean modal ) {
      super( parent, modal );
      // loading BGUI.cfg file entries for the dialog
      parser = bigInterface.getBIGConfigFileParser();
      try {
         helpTextBase = bigInterface.getBenchItPath() +
             File.separator + "gui" + File.separator +
             parser.stringCheckOut( configFileTipFileBase ) ;
         helpIconBase = bigInterface.getBenchItPath() +
             File.separator + "gui" + File.separator +
             parser.stringCheckOut( configFileTipIconBase ) ;
      }
      catch ( Exception e ) {
         bigInterface.getConsole().postMessage(
            "ERROR no \"" + configFileTipFileBase + "\" or \"" +
            configFileTipIconBase + "\" entry in BGUI.cfg!",
            BIGConsole.ERROR );
         helpTextBase = null;
         helpIconBase = null;
         // no need to display the dialog -> close it right away
         setVisible( true );
         dispose();
      }
      try {
         nextHelpText = parser.intCheckOut( configFileKeyNext );
      }
      catch ( Exception e ) {
         nextHelpText = 0;
      }
      try {
         showTips = parser.boolCheckOut( configFileKeyShow );
      }
      catch ( Exception e ) {
         showTips = false;
      }

      scanForHelpTexts();

      JPanel mainPanel = new JPanel( new GridBagLayout() );
      mainPanel.setBorder( BorderFactory.createEmptyBorder(
         INSET, INSET, INSET, INSET ) );

      JPanel buttonPanel = new JPanel( new GridBagLayout() );
      GridBagConstraints c = new GridBagConstraints();
      // checkbox for deactivating startup daily tips
      JCheckBox checkBox = new JCheckBox( "Show tips on startup",
         showTips );
      // button to choose last tip
      JButton lastButton = new JButton( "Last tip" );
      // button to choose next tip
      JButton nextButton = new JButton( "Next tip" );
      // button to close the dialog
      JButton closeButton = new JButton( "Close" );

      c.anchor = GridBagConstraints.LAST_LINE_START;
      c.fill = GridBagConstraints.HORIZONTAL;
      c.weighty = 1.0;
      c.weightx = 1.0;
      c.gridx = 0;
      buttonPanel.add( checkBox, c );

      c.fill = GridBagConstraints.NONE;
      c.anchor = GridBagConstraints.LAST_LINE_END;
      c.weighty = 0.0;
      c.weightx = 0.0;
      c.gridx = 1;
      buttonPanel.add( lastButton, c );
      c.gridx = 2;
      buttonPanel.add( nextButton, c );

      c.gridx = 3;
      c.insets = new Insets( 0, 15, 0, 7 );
      buttonPanel.add( closeButton, c );

      JPanel panel = new JPanel( new BorderLayout() );
      panel.setBorder( BorderFactory.createLoweredBevelBorder() );

      // create the icon bar left of the help text
      iconPanel = new GUIIconPanel();

      // create the help text area
      helpText = new JEditorPane( );
      helpText.setEditable( false );
      loadNextTip( false );
      setTitle( "Tip of the Day: #" + nextHelpText );
      JScrollPane scrollPane = new JScrollPane( helpText );
      scrollPane.setVerticalScrollBarPolicy(
         JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );
      Dimension dim = new Dimension( 370, HEIGHT );
      scrollPane.setPreferredSize( dim );
      scrollPane.setMinimumSize( dim );

      panel.add( iconPanel, BorderLayout.WEST );
      panel.add( scrollPane, BorderLayout.CENTER );

      // Add panel to the content pane.
      c.insets = new Insets( 0, 0, 0, 0 );
      c.anchor = GridBagConstraints.CENTER;
      c.gridwidth = GridBagConstraints.REMAINDER;
      c.fill = GridBagConstraints.BOTH;
      c.weightx = 1.0;
      c.weighty = 1.0;
      c.gridx = 0;
      c.gridy = 0;
      mainPanel.add( panel, c );

      c.insets = new Insets( 7, 0, 0, 0 );
      c.fill = GridBagConstraints.HORIZONTAL;
      c.gridy = 1;
      c.weighty = 0.0;
      mainPanel.add( buttonPanel, c );

      c.insets = new Insets( 0, 0, 0, 0 );
      c.gridwidth = GridBagConstraints.REMAINDER;
      c.fill = GridBagConstraints.BOTH;
      c.weightx = 1.0;
      c.weighty = 1.0;
      getContentPane().setLayout( new GridBagLayout() );
      getContentPane().add( mainPanel, c );

      pack();
      setVisible( true );
      // is deprecated in java 1.5.0
      // show();

      // determine the bounds of the daily tip dialog
      int w = parent.getWidth();
      int h = parent.getHeight();
      int x = parent.getX() + ( ( w - getWidth() ) / 2 );
      int y = parent.getY() + ( ( h - getHeight() ) / 2 );
      w = getWidth();
      h = getHeight();
      // set the bounds
      setBounds( x, y, w, h );

      checkBox.addItemListener( new ItemListener() {
         public void itemStateChanged(ItemEvent ie) {
            if ( ie.getStateChange() == ItemEvent.SELECTED ) {
               showTips = true;
            }
            else {
               showTips = false;
            }
            if ( showTips ) {
               parser.set( configFileKeyShow, "1" );
            }
            else {
               parser.set( configFileKeyShow, "0" );
            }
         }
      } );
      closeButton.addActionListener( new ActionListener() {
         public void actionPerformed( ActionEvent ae ) {
            dispose();
         }
      } );
      lastButton.addActionListener( new ActionListener() {
         public void actionPerformed( ActionEvent ae ) {
            iconPanel.changeIcon();
            loadNextTip( true );
            setTitle( "Tip of the Day: #" + nextHelpText );
            repaint();
         }
      } );
      nextButton.addActionListener( new ActionListener() {
         public void actionPerformed( ActionEvent ae ) {
            iconPanel.changeIcon();
            loadNextTip( false );
            setTitle( "Tip of the Day: #" + nextHelpText );
            repaint();
         }
      } );
   }
   /** Loads the next daily tip. The next index should be
     * stored in the BGUI.cfg file. If not, 0 is used. If the
     * index stored is greater than the number of available
     * daily tips, the last daily tip will be choosen. If the
     * index points to the last daily tip before entering this
     * method, it will point to the first one after leaving.
     * @param prev true, if previous tip should be loaded, false else.
     **/
   private void loadNextTip( boolean prev ) {
      // just to make sure that we never exceed the array index bound
      // assure that a valid daily tip is choosen
      if ( prev ) {
         nextHelpText -= 2;
      }
      if ( nextHelpText < 0 ) {
         nextHelpText = helpTexts.length - 1;
      }
      if ( nextHelpText >= helpTexts.length ) {
         nextHelpText = 0;
      }
      try {
         helpText.setPage( helpTexts[nextHelpText++] );
      }
      catch ( IOException ioe ) {
         bigInterface.getConsole().postMessage(
            "ERROR loading the next daily tip: "
            + helpTexts[nextHelpText - 1], BIGConsole.ERROR );
      }
      // store the index for the next tip
      parser.set( configFileKeyNext,
         new Integer( nextHelpText ).toString() );
   }
   /** Scans a directory for available daily tip files.
     * The directory MUST be set in the BGUI.cfg file.
     * Valid daily tip files are all html files starting
     * with "tip". */
   private void scanForHelpTexts() {
      if ( helpTextBase == null ) return;
      try {
         File helpDir = new File( helpTextBase + "/" );
         File[] list = filter( helpDir.listFiles(), "tip", ".html" );
         helpTexts = new URL[list.length];
         for ( int i = 0; i < list.length; i++ ) {
            helpTexts[i] = list[i].toURL();
         }
      }
      catch ( NullPointerException npe ) {
         bigInterface.getConsole().postMessage(
            "ERROR (NullPointer) scanning for daily tip files at: "
            + helpTextBase );
         helpTexts = null;
      }
      catch ( MalformedURLException mue ) {
         bigInterface.getConsole().postMessage(
            "ERROR (MalformedURL) scanning for daily tip files at: "
            + helpTextBase );
         helpTexts = null;
      }
      if ( helpTexts != null ) {
         // just in case a help file was deleted and the counter points
         // to an array index that's not there anymore we change it down
         // to the last available daily tip help text
         if ( nextHelpText >= helpTexts.length ) {
            nextHelpText = helpTexts.length - 1;
            // store the index for the next tip
            parser.set( configFileKeyNext,
               new Integer( nextHelpText ).toString() );
         }
      }
   }
   /**
    * Filters and Sorts an array of Files.
    * @param inputList the input array.
    * @param beginning the file beginning to filter, or null if any.
    * @param ending the file ending to filter, or null if any.
    * @return a sorted and filtered array.
    **/
   private File[] filter( File[] inputList,
      String beginning, String ending ) {
      int count = 0;
      // in case no filtering is neccessary
      if ( ( beginning == null ) && ( ending == null ) ) {
         return inputList;
      }
      for ( int i = 0; i < inputList.length; i++ ) {
         if ( ( beginning == null ) &&
            ( inputList[i].getName().endsWith( ending ) ) ) {
            count++;
            continue;
         }
         else if ( ( ending == null ) &&
            ( inputList[i].getName().startsWith( beginning ) ) ) {
            count++;
            continue;
         }
         else if (
            ( inputList[i].getName().startsWith( beginning ) ) &&
            ( inputList[i].getName().endsWith( ending ) ) ) {
            count++;
            continue;
         }
      }
      File[] retval = new File[count];
      int j = 0;
      for ( int i = 0; i < inputList.length; i++ ) {
         if ( ( beginning == null ) &&
            ( inputList[i].getName().endsWith( ending ) ) ) {
            retval[j++] = inputList[i];
            continue;
         }
         else if ( ( ending == null ) &&
            ( inputList[i].getName().startsWith( beginning ) ) ) {
            retval[j++] = inputList[i];
            continue;
         }
         else if (
            ( inputList[i].getName().startsWith( beginning ) ) &&
            ( inputList[i].getName().endsWith( ending ) ) ) {
            retval[j++] = inputList[i];
            continue;
         }
      }
      java.util.Arrays.sort( retval );
      return retval;
   }
   /** Internal class representing the icon panel of the
     * daily tip dialog. */
   class GUIIconPanel extends JPanel {
      // current icon
      private ImageIcon icon;
      // list of all available icons
      private File[] list;
      /** The constructor. */
      GUIIconPanel() {
         super();
         scanForIcons();
         changeIcon();
      }
      /** If more than one icon exists in the icon base
        * directory a click on the next button will
        * cycle through them. */
      public void changeIcon() {
         int choice = rand.nextInt( list.length );
         String iconName = list[choice].getName();
         icon = new ImageIcon( helpIconBase +File.separator +iconName );
         Dimension dim = new Dimension( icon.getIconWidth() * 2, HEIGHT );
         setPreferredSize( dim );
         setMinimumSize( dim );
      }
      /** Overriding the paint method for custom drawing. */
      protected void paintComponent( Graphics g ) {
         int width = getWidth();
         int height = getHeight();
         int x = ( width - icon.getIconWidth() ) / 2;
         int y = icon.getIconHeight() / 2;
         g.setColor( Color.darkGray );
         g.fillRect( 0, 0, width, height );
         icon.paintIcon( this, g, x, y );
      }
      /** Scan a directory for availably icons. Icons must
        * confirm to the png file format, start with "tip"
        * and should not be higher than 150 points. The
        * directory must be stored in the BGUI.cfg file. */
      private void scanForIcons() {
         try {
            File graphicDir = new File( helpIconBase + File.separator );
            list = filter( graphicDir.listFiles(), "tip", ".png" );
         }
         catch ( NullPointerException npe ) {
            System.err.println(npe);
            list = null;
         }
      }
   }
}
