/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGMain.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.4 $
 *  $Date: 2007/05/08 09:35:42 $
 *
     ******************************************************************************/

import gui.BIGConsole;
import gui.BIGGUI;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FilenameFilter;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

import system.BIGArgsParser;
import system.BIGConfigFileParser;
import system.BIGDoubleIdentifierException;
import system.BIGInterface;
import system.BIGParserException;
// for checking of it's existance
//import org.apache.crimson.parser.* ;

/**
 * This is the main class for loading the application. Also you can
 * load the AdminTool if you add -admin to the command line arguments.
 * <br><br>
 * Enjoy the fine program :)
 *
     * @author Carsten, <a href="mailto:c.luxig@lmcsoft.com">c.luxig@lmcsoft.com</a>
 */
public class BIGMain
{
    /**
     * parses gui/cfg/BGUI.cfg
     */
    private BIGConfigFileParser fileParser=null;
    /**
     * console for stdout , stderr, dbg
     */
    private BIGConsole console ;

  //private BIGAdmin adminTool = null ;
  /**
   * the one and only BIGGUI. a frame, which contains all elements
   */
  private BIGGUI gui = null ;
  /**
   * the splash-screen
   */
  private JFrame startWindow ;
  /**
   * the splash-screens progress bar
   */
  private JProgressBar startProgress ;

  /**
   * the label for the splash-screens progressbar
   */
  private JLabel progressLabel;
  /**
   * a thread, which starts the gui, while the other one
   *  (handling the splash and init) ends
   */
  private Thread startThread ;
  /**
   * layout, not so important
   */
  private GridBagLayout gbLayout = new GridBagLayout() ;
  /**
   * also layout
   */
  private GridBagConstraints gbConst = new GridBagConstraints() ;
  /**
   * This method is needed, because, if there are no files, that define the System, the save doesnt really work
   * so this checks the files and (if needed) replaces them with the PROTOTYPE files
   * @param benchitPath foldername of Benchit (benchit.c should be there)
   * @param hostname hostname of computer BGUI starts on
   */
  private void copyPrototypeIfNeeded( String benchitPath , String hostname )
  {
    // here are the possible endings of the files
    String[] filenames = new String[ 3 ] ;
    filenames[ 0 ] = "" ;
    filenames[ 1 ] = "_input_architecture" ;
    filenames[ 2 ] = "_input_display" ;
    // so we check all endings
    for ( int i = 0 ; i < filenames.length ; i++ )
    {
      //less to write
      String filenameext = filenames[ i ] ;
      // if this throws an exception
      // the files dont exist
      try
      {
        BufferedReader in = new BufferedReader( new FileReader( benchitPath +
            File.separator + "LOCALDEFS" + File.separator + hostname +
            filenameext ) ) ;
      }
      catch ( Exception e )
      { // FIRSTTIME must be called here !!!!!!
        // and we got to make a copy
        FileOutputStream out = null ;
        FileInputStream in = null ;
        byte[] buffer = new byte[1024] ;
        // how many bytes are read
        int num=-1;
        try
        {
          // we read files with in
          in = new FileInputStream( benchitPath + File.separator +
              "LOCALDEFS" + File.separator + "PROTOTYPE" + filenameext )  ;
          // and write files with out
          out = new FileOutputStream( benchitPath + File.separator +
                                      "LOCALDEFS" + File.separator + hostname +
                                      filenameext ) ;
          while ( ( num = in.read(buffer) ) > -1 )
          {
              out.write(buffer,0,num ) ;
          }
          // we close the streams
          out.close() ;
          in.close() ;
        }
        catch ( Exception ex )
        {
        }
      }
    }
  }

   /** Creates a new instance of BIGMain.
     * @param args  the command line arguments.
     */
   public BIGMain( final String[] args ) {

      // opens a console window
      startThread = new Thread() {

         public void run() {
            // initialize the interface
            BIGInterface bigInterface = BIGInterface.getInstance() ;
            // and set the arguments given with prompt
            bigInterface.setArguments( args ) ;


            // we set this.fileParser (configFileParser) to the Interface
            // so we can use it again
            bigInterface.setBIGConfigFileParser( fileParser ) ;
            // set the hostname
            BIGInterface.getInstance().setHost(BIGInterface.getInstance().getOriginalHost());
            // setting default values (here should be the start-file)
            File relativeDir=new File(System.getProperties().getProperty("user.dir"));
            // we go up a folder until we find benchit.c
            while (true) {
                File[] filesInRelDir = relativeDir.listFiles(new FilenameFilter() {
                    public boolean accept(File f, String s) {
                        if (s.equals("benchit.c")) {
                            return true;
                        }
                        return false;
                    }
                });
                // if we find it, we have the benchit-folder
                if ((filesInRelDir!=null)&&(filesInRelDir.length>0))
                    break;
                relativeDir = relativeDir.getParentFile();
            }
            // and set the path
            bigInterface.setBenchItPath( relativeDir.getAbsolutePath() ) ;
            // we set the config file name for LOCALDEF-settings
            bigInterface.setConfigFilename( bigInterface.getBenchItPath() +
                                            File.separator + "gui" +
                                            File.separator + "cfg" +
                                            File.separator + "config.xml" ) ;
            // we set the config file name for gui settings
            fileParser = new BIGConfigFileParser( bigInterface.getBenchItPath() +
                                                  File.separator + "gui" +
                                                  File.separator + "cfg" +
                                                  File.separator + "BGUI.cfg" ) ;
            // and set it in the interface
            bigInterface.setBIGConfigFileParser( fileParser ) ;
            // we try to avoid things like "do you really want" "Oui/Non"-Dialogs
            java.util.Locale.setDefault( java.util.Locale.ENGLISH ) ;
            // VERY hacky, but will do. We use \n, because this is the unix-std
            // and all our files use them, also the measurement...
            System.setProperty("line.separator","\n");
            // the splash-screen
            startWindow = new JFrame() ;
            // for sure without a frame
            startWindow.setUndecorated(true);
            // icon means here picture of splash-screen
            ImageIcon teamIcon = new ImageIcon(bigInterface.getBenchItPath() +
                                               File.separator + "gui" +
                                               File.separator + "img" +
                                               File.separator + "splash.jpg" ) ;
            // once the team was displayed here. to salute the swt-peaople it stays ;)
            JLabel teamLabel = new JLabel( teamIcon ) ;
            teamLabel.setBounds( 0 , 0 ,
                                 teamIcon.getIconWidth() , teamIcon.getIconHeight() ) ;
            // set it to top of the display, when focusing it
            startWindow.addWindowFocusListener( new WindowAdapter() {
               public void windowGainedFocus( WindowEvent e ) {
                  startWindow.toFront() ;
               }
            } ) ;
            // display other cursor, people will see, the system works ;)
            startWindow.setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) ) ;
            // this panel will contain startProgress and progressLabel
            // its the bottom component of the splash-screen
            // i won't comment every line, so START setting some defaults
            JPanel progressPanel = new JPanel() ;
            progressPanel.setLayout( gbLayout ) ;
            progressPanel.setOpaque( false ) ;
            int spaceLeftRight = 30;
            int progHeight = 5;
            int progWidth = teamIcon.getIconWidth() - 2 * spaceLeftRight;

            startProgress = new JProgressBar( 0 , 14 ) ;
            startProgress.setOpaque( false ) ;
            startProgress.setBorderPainted( false ) ;
            startProgress.setForeground( new Color( 216 , 116 , 48 ) ) ;
            Dimension progressDim = new Dimension( progWidth, progHeight );
            startProgress.setPreferredSize( progressDim );
            startProgress.setMinimumSize( progressDim );
            startProgress.setMaximumSize( progressDim );
            progressLabel = new JLabel();
            progressLabel.setForeground( new Color( 216 , 116 , 48 ) );
            progressLabel.setFont(
               new Font( progressLabel.getFont().getName(),
                         progressLabel.getFont().getStyle(), 10 ) );
            progressDim = new Dimension( progWidth, 12 );
            progressLabel.setPreferredSize( progressDim );
            progressLabel.setMinimumSize( progressDim );
            progressLabel.setMaximumSize( progressDim );
            bigInterface.setStatusLabel(progressLabel);

            createStrut( 0 , 0 , 1 , 1 , spaceLeftRight , 10 , progressPanel ) ;
            addComponent( progressLabel , 0 , 1 , 1 , 1 , progressPanel ,
               GridBagConstraints.HORIZONTAL , GridBagConstraints.CENTER ,
               100 , 0 ) ;
            createStrut( 0 , 2 , 1 , 1 , spaceLeftRight , 10 , progressPanel ) ;
            createStrut( 1 , 0 , 1 , 1 , spaceLeftRight , 10 , progressPanel ) ;
            addComponent( startProgress , 1 , 1 , 1 , 1 , progressPanel ,
               GridBagConstraints.HORIZONTAL , GridBagConstraints.CENTER ,
               100 , 0 ) ;
            createStrut( 1 , 2 , 1 , 1 , spaceLeftRight , 10 , progressPanel ) ;

            startWindow.getContentPane().add( progressPanel , BorderLayout.SOUTH ) ;
            startWindow.getLayeredPane().add( teamLabel ,
               new Integer( Integer.MIN_VALUE ) ) ;
            ( (JPanel)startWindow.getContentPane() ).setOpaque( false ) ;
            // END setting some defaults
            // pack means set to best size
            startWindow.pack() ;
            // try to display the splash screen at the bottom of screen
            int w = startWindow.getGraphicsConfiguration().getDevice().getDisplayMode().getWidth();
            int h = startWindow.getGraphicsConfiguration().getDevice().getDisplayMode().getHeight();
            startWindow.setBounds(
               ( w - teamIcon.getIconWidth() ) / 2 , ( h - teamIcon.getIconHeight() ) / 2,
               teamIcon.getIconWidth() , teamIcon.getIconHeight() ) ;
            // finally, the user sees it
            startWindow.setVisible( true ) ;
           // (new MouseEvent(starWindow,12345,System.currentTimeMillis(),0,1,1,1,false,MouseEvent.BUTTON1))
            // Step 0 - create console window
            progressLabel.setText( "Creating message console" );
            startProgress.setValue( 0 ) ;
            try {
               Thread.sleep( 0 , 1 ) ;
            }
            catch ( InterruptedException ie ) {}

            try {
               // position
               int x = 615 ;
               int y = 10 ;
               // size
               int xSize = 400 ;
               int ySize = 300 ;
               // whether to open console in an extern window before the gui is started.
               // its a relikt from old times
               boolean openConsole = false ;
               try {
                  // we get the settings for the console
                  // x-Dimension
                  x = fileParser.intCheckOut( "consoleWindowXPos" ) ;
                  // y-Dimension
                  y = fileParser.intCheckOut( "consoleWindowYPos" ) ;
                  // size of the Window
                  xSize = fileParser.intCheckOut( "consoleWindowXSize" ) ;
                  // size of the Window
                  ySize = fileParser.intCheckOut( "consoleWindowYSize" ) ;
                  // whether to show the Window or not
                  openConsole = fileParser.boolCheckOut( "openConsole" ) ;
               }
               // we can ignore the Exception because if there is no BGUI.cfg
               // then there appears a 'file not found' on the command line
               // and afterwards you never see anything again (if you
               // ignore the exceptions)
               catch ( Exception ignored ) {}
               // initialize
               console = new BIGConsole( x , y ) ;
               console.setTitle( "BenchIT - Console" ) ;
               console.setBounds( x , y , xSize , ySize ) ;
               console.setVisible( openConsole ) ;
            }
            // should not occure - hopefully :)
            catch ( java.io.IOException ioe ) {
               System.err.println( "Can't open console!" ) ;
            }
            bigInterface.setConsole( console ) ;
            // in case the gui is started within a maximized console
            // window the start splash will not be displayed on to of
            // all other windows. there seems to be no way around it
            // so far.
            startWindow.toFront();
            startWindow.requestFocus();

            // Step 1 - create instance of BIGInterface
            progressLabel.setText( "Instanciating BIGInterface" );
            startProgress.setValue( startProgress.getValue() + 1 ) ;
            try {
               Thread.sleep( 0 , 1 ) ;
            }
            catch ( InterruptedException ie ) {}
            // Step 2 - evaluate command line arguments
            progressLabel.setText( "Checking command line arguments" );
            startProgress.setValue( startProgress.getValue() + 1 ) ;
            try {
               Thread.sleep( 0 , 1 ) ;
            }
            catch ( InterruptedException ie ) {}

            // analysing Arguments from the bash
            // also sets the hostname in bigInterface
            BIGArgsParser argsParser = new BIGArgsParser() ;
            try {
               argsParser.parse( args ) ;
            }
            catch ( BIGParserException bpe ) {
               console.postMessage(
                  "Cannot parse these command line arguments: " +
                  bpe, BIGConsole.ERROR ) ;
            }
            // if we are new on the system copy PROTOTYPE-LOCALDEFS to new LOCALDEFS
            // (the gui needs LOCALDEFS to work on)
            copyPrototypeIfNeeded( bigInterface.getBenchItPath() ,
               bigInterface.getHost() ) ;

            // Step 3
            // contains information about LOCALDEFS
            progressLabel.setText( "Loading config.xml" );
            startProgress.setValue( startProgress.getValue() + 1 ) ;
            try {
               Thread.sleep( 0 , 1 ) ;
            }
            catch ( InterruptedException ie ) {}
            // BIGInterface.load means the LOCALDEFs are loaded and parsed
            try {
                 bigInterface.load();
            }
            catch ( BIGParserException bpe ) {
               console.postMessage(
                  "error during loading database in BIGMain: " + bpe ,
                  BIGConsole.ERROR ) ;
               console = null ;
               System.gc() ;
               System.err.println(
                  "error during loading database in BIGMain: " + bpe ) ;
               System.exit( 1 ) ;
            }
            catch ( BIGDoubleIdentifierException bdie ) {
               console.postMessage(
                  "error by adding a file in BIGMain: " + bdie ,
                  BIGConsole.ERROR ) ;
               console = null ;
               System.gc() ;
               System.err.println( "error by adding a file in BIGMain: " + bdie ) ;
               System.exit( 1 ) ;
            }

            // Step 4
            progressLabel.setText( "Initializing GUI" );
            startProgress.setValue( startProgress.getValue() + 1 ) ;
            try {
               Thread.sleep( 0 , 1 ) ;
            }
            catch ( InterruptedException ie ) {}
               // standard settings
               // position
               int x = 10 ;
               int y = 10 ;
               // size
               int xSize = 600 ;
               int ySize = 600 ;
               // position of split-pane-borders (is this the right word???)
               int rightSplit = -1 , mainSplit = -1, detailLevel = 0 ;
               try {
                  // we get the settings for the BIGGUI
                  // x-Dimension
                  x = fileParser.intCheckOut( "mainWindowXPos" ) ;
                  // y-Dimension
                  y = fileParser.intCheckOut( "mainWindowYPos" ) ;
                  // size of the Window
                  xSize = fileParser.intCheckOut( "mainWindowXSize" ) ;
                  // size of the Window
                  ySize = fileParser.intCheckOut( "mainWindowYSize" ) ;
                  // splitter positions
                  rightSplit = fileParser.intCheckOut( "mainWindowRightSplit" ) ;
                  mainSplit = fileParser.intCheckOut( "mainWindowMainSplit" ) ;
                  detailLevel = fileParser.intCheckOut( "detailLevel" );
               }
               catch ( Exception ignored ) {}

               // the negoation of the boolean tells BIGGUI, wether it
               // has still to load sth or not.
               gui = new BIGGUI(
                  startProgress, progressLabel );
               gui.setValues( x , y , xSize , ySize , rightSplit , mainSplit,
                      detailLevel ) ;

            // Step 14
            progressLabel.setText( "Finishing initialization" );
            startProgress.setValue( startProgress.getMaximum() ) ;
            try {
               Thread.sleep( 1 ) ;
            }
            catch ( InterruptedException ie ) {}

            // show GUI
               gui.setVisible( true ) ;
               gui.toFront() ;
               // close splash-screen
            startWindow.dispose() ;
            // cleaning garbage collection
            System.gc() ;
         }
      } ;
      startThread.start() ;
   }
   /**
    * a way to add components to gridbag
    * @param comp Component component to add
    * @param ypos int x-position in gridbag
    * @param xpos int y-position in gridbag
    * @param height int height of component in gridbag
    * @param width int width of component in gridbag
    * @param panel JPanel panel containing the gridbag
    * @param fill int fill (see GridBagLayout)
    * @param anchor int anchor (see GridBagLayout)
    * @param wx int weight-x-dimension (see GridBagLayout)
    * @param wy int weight-y-dimension (see GridBagLayout)
    */
   private void addComponent( Component comp , int ypos , int xpos , int height ,
                             int width , JPanel panel , int fill , int anchor ,
                             int wx , int wy )
   {
      gbConst.gridx = xpos ;
      gbConst.gridy = ypos ;
      gbConst.gridheight = height ;
      gbConst.gridwidth = width ;
      gbConst.fill = fill ;
      gbConst.anchor = anchor ;
      gbConst.insets = new Insets( 1 , 3 , 1 , 3 ) ;
      gbConst.weightx = wx ;
      gbConst.weighty = wy ;
      gbLayout.setConstraints( comp , gbConst ) ;
      panel.add( comp ) ;
   }
   /**
    * helps placing panels on gridbag
    * @param ypos int y-position in gridbag
    * @param xpos int x-position in gridbag
    * @param gh int gridheight
    * @param gw int gridwidth
    * @param width int width of panel
    * @param height int heigth of panel
    * @param panel JPanel panel to add
    */
   private void createStrut( int ypos , int xpos , int gh , int gw , int width ,
                            int height , JPanel panel )
   {
      gbConst.gridx = xpos ;
      gbConst.gridy = ypos ;
      gbConst.gridheight = gh ;
      // align over to cells
      gbConst.gridwidth = gw ;
      // resize to max
      gbConst.weightx = 0 ;
      gbConst.weighty = 0 ;
      // in both directions
      gbConst.fill = GridBagConstraints.BOTH ;
      // always add on NORTH
      gbConst.anchor = GridBagConstraints.NORTH ;
      gbConst.insets = new Insets( 0 , 0 , 0 , 0 ) ;
      // create and add empty panel
      JPanel glue = new JPanel() ;
      glue.setPreferredSize( new Dimension( width , height ) ) ;
      glue.setOpaque( false ) ;
      gbLayout.setConstraints( glue , gbConst ) ;
      panel.add( glue ) ;
   }

   /**
      * @param args the command line arguments
      */
   public static void main( String[] args ) {

      new BIGMain( args ) ;
   }
}
/*****************************************************************************
 Log-History
*****************************************************************************/
