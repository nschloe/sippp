/*********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  BIGGUI.java
 *
 *  Author: SWTP Nagel 1
 *  Last change by: $Author: tschuet $
 *  $Revision: 1.43 $
 *  $Date: 2007/07/10 10:41:30 $
 *
 *********************************************************************/
package gui;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Paint;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Observable;
import java.util.TimerTask;
import java.util.TreeSet;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import plot.BIGOutputFile;
import plot.BIGPlot;
import plot.BIGPlotable;
import plot.BIGResultMixer;
import system.BIGConfigFileParser;
import system.BIGDefFileParser;
import system.BIGEntry;
import system.BIGExecute;
import system.BIGFileHelper;
import system.BIGInterface;
import system.BIGKernel;
import system.BIGParserException;
import system.BIGStrings;
import system.BIGTextfileUpdate;
import system.BIGUpdate;
import system.BIGUtility;
import admin.BIGAdmin;
import conn.MyJFrame;
//import org.jfree.ui.RefineryUtilities;

/**
 * The BIGGUI handles all possible entries by the given host in the
 * BIGInterface. You can change the value of each entry to specify
 * your local def file.<br>
 * The BIGGUI is also synchronized with the BIGEditor, so changing
 * values here changed also the value in the editor.<br><br>
 *
 * @see BIGEditor
 * @author Carsten Luxig
 *    <a href="mailto:c.luxig@lmcsoft.com">c.luxig@lmcsoft.com</a>
 * some changes by Robert Schoene
 *    <a href="mailto:roberts@zhr.tu-dresden.de">roberts@zhr.tu-dresden.de</a>
 * interface redesign by Robert Wloch
 *    <a href="mailto:wloch@zhr.tu-dresden.de">wloch@zhr.tu-dresden.de</a>
 */

public class BIGGUI extends JFrame {

   private Process executedProcess=null;
    private static final String[] detailString = {"low", "medium", "high"};
    // Variables declaration - do not modify
    private BIGInterface bigInterface = BIGInterface.getInstance();
    private BIGConsole console = bigInterface.getConsole();
//   private BIGHelp help = new BIGHelp();
    private BIGAboutWindow about = null;
//   private BIGExecuteWindow executeWindow;
    private static BIGQuickViewWindow quickViewWindow;
    private static BIGKernelScriptWindow kernelScriptWindow;
    private static String VIEWPANEL = "Graphical View";
    private static String TEXTPANEL = "Plain Text View";
    private static String KERNELS_TABB = "Kernels";
    private static String RESULTS_TABB = "Results";
    private static boolean initDone;
    private int dimx, dimy;
    private BIGResultMixer[] resultMixer;

    // Represents the admin state of the GUI.
    public static final int ADMIN = 1;
    // Represents the database state of the GUI.
    public static final int DATABASE = 2;
    // Represents the localdefs state of the GUI.
    public static final int LOCALDEFS = 3;
    // Represents the kernels state of the GUI.
    public static final int KERNELS = 4;
    // Represents the results state of the GUI.
    public static final int RESULTS = 5;
    // represents the current state of the GUI
    private int state;

    private system.RemoteDefinition session=null;

//   private JComponent adminPanel;
    private MyJFrame connectionFrame;
    private JComponent normalPanel;
    private JComponent[] resultViewPanels;

    private Action adminItemAction;
    private Action databaseItemAction;
    private Action localdefsItemAction;
//   private Action kernelsItemAction;
//   private Action resultsItemAction;
//   private Action graficalViewItemAction;
//   private Action textualViewItemAction;
    private Action toggleViewItemAction;
    private Action executeSelectedItemAction;
    private Action showResultItemAction;

    // Menu variables
    // new structure
    private JMenu fileMenu;
    private JMenu setupMenu;
    private JMenu measureMenu;
    private JMenu evaluateMenu;
    // old structure
    /*   private JMenu kernelMenu;
       private JMenu resultMenu;
       private JMenu localdefMenu;
     */private JMenu databaseMenu;
    // both menu strucures
    private JMenu viewMenu;
    private JMenu helpMenu;
    // remote menu is not present itself, but used to get it's menu items
    private BIGRemoteMenu remoteMenu;
    // menu items
    private JMenuItem adminMenuItem;
    private JMenuItem dbMenuItem;
    private JMenuItem localdefMenuItem;
//   private JMenuItem kernelsMenuItem;
//   private JMenuItem resultsMenuItem;
    private JMenuItem preferencesMenuItem;
    private JMenuItem quitMenuItem;
//   private JMenuItem graphicViewMenuItem;
//   private JMenuItem textViewMenuItem;
    private JMenuItem toggleViewMenuItem;
    private JMenuItem loadMenuItem;
    private JMenuItem saveMenuItem;
    private JMenuItem saveAsMenuItem;
    private JMenuItem executeMenuItem;
    private JMenuItem executeSelectedMenuItem;
//   private JMenuItem executeAllMenuItem;
    private JMenuItem updateKernelTreeMenuItem;
    private JMenuItem executeSelectedRemoteMenuItem;
    private JMenuItem showResultMenuItem;
    private JMenuItem loadUpMenuItem;
    private JMenuItem updateResultTreeMenuItem;
    private JMenuItem helpMenuItem;
    private JMenuItem tipMenuItem;
    private JMenuItem aboutMenuItem;
    private JMenuItem printMenuItem;

    private BIGAdmin adminTool;
    private JToolBar toolbar;
    private JToolBar statusBar;
    private JPanel mainViewPanel;
    private JSplitPane mainSplitPane;
    private JSplitPane rightSplitPane;
    private BIGPlot plotWindow;

    private JTabbedPane listTabbs;
    private JPanel cardsButtonPanel;
    private JPanel viewPanel;
    private JPanel textPanel;

    private BIGGUIObserverProgress statusProgress;
    private JLabel statusLabel;

//   private JButton viewButton;
//   private JButton textButton;
    private JComboBox localdefFilesComboBox;
    private JComboBox viewLevelComboBox;
//   private JCheckBox showCheckBox;
    private String localDefFilesComboBoxEntries[];
    private String viewLevelComboBoxEntries[];
    private String[] choiceTypes = {"compile only", "run only",
            "compile and run"};
    private int defaultSelected = 2;
    private JList choiceList;

    private String kernelRootName = "all kernels";
    private TreeSet kernels;
    private BIGKernelTree kernelTree;
    private JScrollPane kernelTreeScrollPane;
    private String resultRootName = "all results";
    private TreeSet results;
    private BIGResultTree resultTree;

    /*   private JTextPane editor;
       private JPopupMenu priorityPopup;
       private JButton priorityButton, quickviewButton;
       private JButton startButton, closeButton;*/
    private Font labelFont;
//   private JProgressBar progressBar;
    private int viewLevel = 0;
    /*   private ArrayList plotWindows = new ArrayList();
       private JFileChooser fileChooser;
     */private HashMap editorFileRelation = new HashMap();
    private boolean textViewMode;
    private int rightSplit, mainSplit;
    // debug
    private int debug = bigInterface.getDebug("BIGGUI");
    // added by rschoene for quickLoad
//   private boolean fullLoaded=false;
    // this will contain this is needed to give a reference to this in inner classes
    protected BIGGUI thisBIGGUI;
    private String staticTitle="BenchIT - GUI (@" + BIGInterface.getInstance().getOriginalHost() + ")";
    private JPanel consoleScrollPane;

    // End of variables declaration
    /** Creates new form BIGGUI.
     * @param startProgress progress in loading/creating will be shown here
     * @param progressLabel information about the progress will be shown here
     * */
    public BIGGUI( /*boolean fullLoad,*/
                    final JProgressBar startProgress,
                    final JLabel progressLabel) {
        // This is an undefined state, but it is neccessary to set it
        // at the beginning of the initialization.
        // The last statement of this constructor must set a defined
        // and allowed state.
        state = -1;
        kernelScriptWindow = null;
//      adminPanel = null;
        connectionFrame = null;
        normalPanel = null;
        resultViewPanels = null;
        initDone = false;
        rightSplit = -1;
        mainSplit = -1;
//      this.fullLoaded=fullLoad;
        this.thisBIGGUI = this;
        
        this.setIconImage( new ImageIcon( BIGInterface.getInstance().getBenchItPath()
        		+ File.separator + "gui" + File.separator + "img"
        		+ File.separator + "clock.png" ).getImage() );
        
        progressLabel.setText("Initializing GUI: GUI components");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}
        initComponents(startProgress, progressLabel);
        setTitle(staticTitle);
        pack();
        // enabling window-close
        this.enableEvents(AWTEvent.WINDOW_EVENT_MASK);
        // setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        // just for fun,
        // setting the windows look and feel by cmd arg: win
        if ( bigInterface.getLookAndFeel().equals( "win" ) )
              {
                 try
                 {
                    UIManager.setLookAndFeel(
                       "com.sun.java.swing.plaf.windows.WindowsLookAndFeel" );
                 } catch ( Exception ex )
                 {
                    System.out.println( ex.toString() );
                 }
                 SwingUtilities.updateComponentTreeUI( getContentPane() );
              }
        // just to prevent any unwanted start up state
        if (bigInterface.isRestart())
            setGUIState(RESULTS);
        else
            setGUIState(LOCALDEFS);

        // now start a thread to load the input_[architecture|display]
        // localdef files
        Thread loadThread = new Thread() {
            public void run() {
                 // check, if daily tip should be shown
                 boolean showDailyTip = false;
                 try {
                     BIGConfigFileParser parser = BIGInterface.getInstance().
                                                  getBIGConfigFileParser();
                     showDailyTip = parser.boolCheckOut(BIGDailyTip.
                             configFileKeyShow);
                 } catch (Exception e) {
                     showDailyTip = false;
                 }
                 if (showDailyTip && !bigInterface.isRestart()) {
                     BIGDailyTip dailyTip = new BIGDailyTip(BIGGUI.this);
                     dailyTip.requestFocus();
                 }
                 startProgress.setValue(100);
                 progressLabel.setText("done.");
             }
        };
        loadThread.start();

    }

    /**
     * this method is used for saving the settings to the config file
     * @param e only handled, when window is closed/BIG is closed
     **/
    protected void processWindowEvent(WindowEvent e) {

        super.processWindowEvent(e);
        if (e.getID() == WindowEvent.WINDOW_CLOSING) {
            this.saveAndExit();
        }

    }

    /** This method is called from within the constructor to
     * initialize the GUI and it's layout.
     * @param startProgress progress in loading will be visible there
     * @param progressLabel information about progress
     */
    private void initComponents(
            JProgressBar startProgress, JLabel progressLabel) {
        dimx = 1024;
        dimy = 768;
        // First of all get the current screen resolution
        GraphicsConfiguration gc = getGraphicsConfiguration();
        if (gc != null) {
            GraphicsDevice gd = gc.getDevice();
            if (gd != null) {
                dimx = gd.getDisplayMode().getWidth();
                dimy = gd.getDisplayMode().getHeight();
            }
        }
        progressLabel.setText("Initializing GUI: Creating menu items");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}

        // create menu items
        adminItemAction = new AdminItemAction(
                "Administrate LOCALDEF variables", null, true);
        adminMenuItem = new JMenuItem(adminItemAction);
        adminMenuItem.setText("Localdef Specification");

        databaseItemAction = new DatabaseItemAction(
                "View results stored in the BenchIT Web Database",
                KeyStroke.getKeyStroke(KeyEvent.VK_D,
                                       ActionEvent.CTRL_MASK), true);
        dbMenuItem = new JMenuItem(databaseItemAction);
        dbMenuItem.setText("BenchIT Database");

        localdefsItemAction = new LocaldefsItemAction(
                "Set the LOCALDEF parameters for the next kernel execution",
                KeyStroke.getKeyStroke(KeyEvent.VK_L,
                                       ActionEvent.CTRL_MASK), true);
        localdefMenuItem = new JMenuItem(localdefsItemAction);
        localdefMenuItem.setText("Localdefs");

        // part of old menu structure
        /*      kernelsItemAction = new KernelsItemAction(
         "Select and execute the kernels for benchmarking", null, true);
              kernelsMenuItem = new JMenuItem( kernelsItemAction );
              kernelsMenuItem.setText( "Kernels" );

              // part of old menu structure
              resultsItemAction = new ResultsItemAction(
                 "Select and display or up load a result file", null, true);
              resultsMenuItem = new JMenuItem( resultsItemAction );
              resultsMenuItem.setText( "Results" );*/

        preferencesMenuItem = new JMenuItem("Preferences");
        preferencesMenuItem.setToolTipText("Customize BenchIT GUI");
        preferencesMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
        preferencesMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                bigInterface.getBIGConfigFileParser().showConfigDialog(
                        thisBIGGUI,
                        BIGGUI.this.kernelTree);
            }
        });


        quitMenuItem = new JMenuItem("Quit", KeyEvent.VK_Q);
        quitMenuItem.setToolTipText("This will terminate the program");
        quitMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q,
                ActionEvent.CTRL_MASK));
        quitMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                saveAndExit();
            }
        });

        // part of old menu structure
        /*      graficalViewItemAction = new GraficalViewItemAction(
         "Displays the selected information in a grafical way", null, true );
              graphicViewMenuItem = new JMenuItem( graficalViewItemAction );
              graphicViewMenuItem.setText( "Grafical View" );*/

        // part of old menu structure
        /*      textualViewItemAction = new TextualViewItemAction(
         "Displays the selected information as plain text", null, true );
              textViewMenuItem = new JMenuItem( textualViewItemAction );
              textViewMenuItem.setText( "Textual View" );*/

        toggleViewItemAction = new ToggleViewItemAction(
                "Toggles display of selected information as plain text or "
                + "graphical components",
                KeyStroke.getKeyStroke(KeyEvent.VK_T, ActionEvent.CTRL_MASK), true);
        toggleViewMenuItem = new JMenuItem(toggleViewItemAction);
        toggleViewMenuItem.setText("Toggle View - Text/Graphic");

        loadMenuItem = new JMenuItem("Load");
        loadMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
                ActionEvent.CTRL_MASK));
        loadMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                loadRequest();
            }
        });

        saveMenuItem = new JMenuItem("Save");
        saveMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
                ActionEvent.CTRL_MASK));
        saveMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                saveRequest();
            }
        });

        saveAsMenuItem = new JMenuItem("Save As...");
        saveAsMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                saveAsRequest();
            }
         });

         printMenuItem = new JMenuItem( "Print" ) ;
         printMenuItem.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent e )
            {
               print() ;
            }
         } ) ;
         printMenuItem.setAccelerator(
             KeyStroke.getKeyStroke( KeyEvent.VK_P , ActionEvent.CTRL_MASK ) ) ;

         JMenuItem screenshot = new JMenuItem( "Screenshot" ) ;
         screenshot.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent e )
            {
               JFileChooser fc = new JFileChooser( bigInterface.getLastWorkPath() ) ;/*
               if (javax.imageio.ImageIO.getImageWritersBySuffix("png").hasNext())
                  fc.addChoosableFileFilter( new javax.swing.filechooser.
                                             FileFilter()
                  {
                     public boolean accept( File f )
                     {
                        if ( f.isFile() && f.getName().endsWith( ".png" ) )
                           return true ;
                        return false ;
                     }

                     public String getDescription()
                     {
                        return "PNG files" ;
                     }

                  } ) ;*/
               if (javax.imageio.ImageIO.getImageWritersBySuffix("jpg").hasNext())
                  fc.addChoosableFileFilter( new javax.swing.filechooser.
                                             FileFilter()
                  {
                     public boolean accept( File f )
                     {
                        if ( f.isFile() && f.getName().endsWith( ".jpg" ) )
                           return true ;
                        return false ;
                     }

                     public String getDescription()
                     {
                        return "JPEG files" ;
                     }

                  } ) ;
               else
                  System.out.println("no jpg");
               if (javax.imageio.ImageIO.getImageWritersBySuffix("png").hasNext())
                  fc.addChoosableFileFilter( new javax.swing.filechooser.
                                             FileFilter()
                  {
                     public boolean accept( File f )
                     {
                        if (f.isDirectory())
                           return true;
                        if ( f.isFile() && f.getName().endsWith( ".png" ) )
                           return true ;
                        return false ;
                     }

                     public String getDescription()
                     {
                        return "PNG files" ;
                     }

                  } ) ;
               else
                  System.out.println("no png");

               fc.setDialogTitle( "Select the filename" ) ;
               if (fc.showSaveDialog( null)!=JFileChooser.APPROVE_OPTION)
                  return;
               bigInterface.setLastWorkPath(fc.getCurrentDirectory());
               if (fc.getSelectedFile().isDirectory())
                  JOptionPane.showMessageDialog(BIGGUI.this,"The selected file is a directory.Canceling.");
               String name=fc.getSelectedFile().getAbsolutePath();
               String extension=null;
               if (name.toLowerCase().endsWith(".png"))
                  extension="png";
               if (name.toLowerCase().endsWith(".jpg"))
                  extension="jpg";
               if (extension==null)
               {
                  if(fc.getFileFilter().getDescription().indexOf("PNG")>-1)
                  {
                     name = name + ".png" ;
                     extension = "png" ;
                  }else
                  {
                     name = name + ".jpg" ;
                     extension = "jpg" ;
                  }
               }
               java.awt.image.BufferedImage bi = new java.awt.image.
                   BufferedImage( BIGGUI.this.getWidth() ,
                                  BIGGUI.this.getHeight() ,
                   java.awt.image.BufferedImage.TYPE_INT_RGB ) ;
               Graphics2D g2d = bi.createGraphics() ;
               BIGGUI.this.paintAll(g2d);
               g2d.dispose() ;
               try
               {
                  javax.imageio.ImageIO.write( bi , extension , new File( name ) ) ;
               }
               catch ( IOException ex )
               {
                  System.out.println("Writing screenshot failed");
               }
            }
         } ) ;


        // part of old menu structure
        executeMenuItem = new JMenuItem("Execute");
        executeMenuItem.setToolTipText(
                "Execute the selected kernel on the local machine");
        executeMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                execute();
            }
        });

        executeSelectedItemAction = new ExecuteSelectedItemAction(
                "Executes the selected kernels on the local machine",
                KeyStroke.getKeyStroke(KeyEvent.VK_E,
                                       ActionEvent.CTRL_MASK), true);
        executeSelectedMenuItem = new JMenuItem(executeSelectedItemAction);
        executeSelectedMenuItem.setText("Execute Selected");

        // part of old menu structure
        /*executeAllMenuItem = new JMenuItem( "Execute All" );
               executeAllMenuItem.setToolTipText(
           "Execute all kernels on the local machine" );
               executeAllMenuItem.addActionListener( new ActionListener() {
           public void actionPerformed( ActionEvent e ) {
              // select root node
              kernelTree.clearSelection();
              DefaultTreeModel model = (DefaultTreeModel)kernelTree.getModel();
              DefaultMutableTreeNode rootNode =
                 new DefaultMutableTreeNode( kernelRootName );
              TreePath path = new TreePath( model.getPathToRoot( rootNode ) );
              kernelTree.setSelectionPath( path );
              executeSelected();
           }
               } );*/

        executeSelectedRemoteMenuItem = new JMenuItem(
                "Execute Selected Remote");
        executeSelectedRemoteMenuItem.setToolTipText(
                "Execute the selected kernels on a distant machine");
        executeSelectedRemoteMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_R, ActionEvent.CTRL_MASK));
        executeSelectedRemoteMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                executeSelectedOnOtherSystem();
            }
        });

        updateKernelTreeMenuItem = new JMenuItem("Reload Kernel Tree");
        updateKernelTreeMenuItem.setToolTipText("Refresh the list of kernels");
        updateKernelTreeMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0));
        updateKernelTreeMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                kernelTree.updateKernelTree();
            }
        });

        // part of old menu structure
        showResultItemAction = new ShowResultItemAction(
                "Displays the selected result file's content as a graph", null, true);
        showResultMenuItem = new JMenuItem(showResultItemAction);
        showResultMenuItem.setText("Show Result");

        // to be implemented in the future
        loadUpMenuItem = new JMenuItem("Load Up");
        loadUpMenuItem.setToolTipText(
                "Load up the result file into the BenchIT Web Database");
        loadUpMenuItem.setEnabled(false);
        loadUpMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                //loadUp();
            }
        });

        updateResultTreeMenuItem = new JMenuItem("Reload Result Tree");
        updateResultTreeMenuItem.setToolTipText(
       "Refresh the tree of the result files" ) ;
        updateResultTreeMenuItem.setAccelerator(
            KeyStroke.getKeyStroke( KeyEvent.VK_F5 , 0 ) ) ;

        updateResultTreeMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                resultTree.updateResultTree(null);
            }
        });

        helpMenuItem = new JMenuItem("Help");
        helpMenuItem.setToolTipText("Get help about the program");
        helpMenuItem.setAccelerator(
                KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0));
        helpMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                BIGHelp help = new BIGHelp();
                help.setVisible(true);
            }
        });

        tipMenuItem = new JMenuItem("Tip of the day...");
        tipMenuItem.setToolTipText("Get tip of the day about the program");
        tipMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                new BIGDailyTip(BIGGUI.this);
            }
        });

        aboutMenuItem = new JMenuItem("About");
        aboutMenuItem.setToolTipText(
                "Get general information about the program");
        aboutMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (about == null)
                    about = new BIGAboutWindow(BIGGUI.this);
                else
                    about.setVisible(!about.isVisible());
            }
        });

        // instanciate an object of MyJFrame with creates and
        // contains all the GUI components needed to connect with
        // the BenchIT database.
        String server=null;
        try {
           server = BIGInterface.getInstance().getBIGConfigFileParser().
               stringCheckOut(
                   "serverIP" );
        } catch (Exception ex) {
            System.err.println("could not read the serverIP from BGUI.cfg");
            databaseMenu = new JMenu("Could not find serverIP in BGUI.cfg");
         }
         if ( server != null )
         {
            connectionFrame = new MyJFrame( server , this ) ;
            databaseMenu = connectionFrame.getMenu() ;
            databaseMenu.setMnemonic( KeyEvent.VK_D ) ;
         }

        progressLabel.setText("Initializing GUI: Creating menu bar");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}

        // now set up the menu bar
        JMenuBar menubar = new JMenuBar();
        /////////////////////////////////////////////////
        // this is the new menu structure
        /////////////////////////////////////////////////
        fileMenu = new JMenu("File");
        fileMenu.setMnemonic(KeyEvent.VK_F);
        setupMenu = new JMenu("Setup");
        setupMenu.setMnemonic(KeyEvent.VK_S);
        measureMenu = new JMenu("Measure");
        measureMenu.setMnemonic(KeyEvent.VK_M);
        evaluateMenu = new JMenu("Evaluate");
        evaluateMenu.setMnemonic(KeyEvent.VK_E);
        viewMenu = new JMenu("View");
        viewMenu.setMnemonic(KeyEvent.VK_V);
        helpMenu = new JMenu("Help");
        helpMenu.setMnemonic(KeyEvent.VK_H);
        /*JMenuItem setSession= new JMenuItem(new AbstractAction()
            {
               public void actionPerformed(ActionEvent evt)
               {
                  setSessionDialog();
               }
            });
            setSession.setText("Set Session");
        fileMenu.add(setSession);*/
        fileMenu.add(loadMenuItem);
        fileMenu.add(saveMenuItem);
        fileMenu.add(saveAsMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(printMenuItem);
        fileMenu.addSeparator();
        fileMenu.add(screenshot);
        fileMenu.addSeparator();
        fileMenu.add(quitMenuItem);
        menubar.add(fileMenu);

        // get the menu items for remote stuff
        remoteMenu = new BIGRemoteMenu(this, false);
        java.util.List[] remoteLists = this.remoteMenu.getRemoteMenus();
        // get the menu items for DB stuff
        java.util.List[] dbLists = this.connectionFrame.getDBMenus();

        setupMenu.add(adminMenuItem);
        setupMenu.add(localdefMenuItem);
        // add remote setup menu items to setup menu
        addListItems(remoteLists, setupMenu, 0);
        // add DB setup menu items to setup menu
        addListItems(dbLists, setupMenu, 0);
        setupMenu.addSeparator();
        setupMenu.add(preferencesMenuItem);
        JMenuItem item=new JMenuItem("Set Default Plot-Colors");
        item.addActionListener(new ActionListener()
            {
               public void actionPerformed(ActionEvent evt)
               {
                  final JFrame f = new JFrame(
                      "Choose the standard colors for plotting") ;
                  Container pane = f.getContentPane() ;
                  final Paint[] p=BIGPlot.getDefaultPaintSequece();
                  final Paint[] newP=new Paint[p.length];
                  for (int i=0;i<p.length;i++)
                     newP[i]=p[i];
                  final JComboBox jcomb=new JComboBox();
                  for (int i=0;i<p.length;i++)
                     jcomb.addItem(""+(i+1));
                  jcomb.setSelectedIndex(0);
                  final JColorChooser jcc=new JColorChooser((Color)p[0]);
                  jcomb.addActionListener(new ActionListener()
                      {
                         int oldSelected=0;
                         public void actionPerformed(ActionEvent ae)
                         {
                            newP[oldSelected]=jcc.getColor();
                            jcc.setColor((Color)newP[jcomb.getSelectedIndex()]);
                            jcc.revalidate();
                            oldSelected=jcomb.getSelectedIndex();
                         }
                      });
                  Button ok=new Button("Okay");
                  ok.addActionListener(new ActionListener()
                      {
                         public void actionPerformed(ActionEvent ae)
                         {
                            newP[jcomb.getSelectedIndex()]=jcc.getColor();
                            BIGPlot.setDefaultPaintSequece(newP);
                            f.setVisible(false);
                         }
                      });

                  Button cancel = new Button( "Cancel" ) ;
                  cancel.addActionListener( new ActionListener()
                  {
                     public void actionPerformed( ActionEvent ae )
                     {
                        f.setVisible( false ) ;
                     }
                  } ) ;

                  pane.setLayout( new BorderLayout() ) ;
                  JPanel buttonPan = new JPanel( new FlowLayout() ) ;
                  buttonPan.add( ok ) ;
                  buttonPan.add( cancel ) ;
                  JPanel comboPan = new JPanel( new FlowLayout() ) ;
                  comboPan.add( new JLabel( "Actual function" ) ) ;
                  comboPan.add( jcomb ) ;

                  pane.add( comboPan , BorderLayout.NORTH ) ;
                  pane.add( jcc , BorderLayout.CENTER ) ;

                  pane.add( buttonPan , BorderLayout.SOUTH ) ;
                  f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
                  f.pack();
                  f.setVisible(true);
               }
            }
         );
         setupMenu.add(item);
        item=new JMenuItem("Reset all Plot-Information");
        item.addActionListener(new ActionListener()
            {
               public void actionPerformed(ActionEvent evt)
               {
                  for (int i=0;i<resultMixer.length;i++)
                  {
                     resultMixer[i].cleanUp();
                  }
                  resultTree.removePlotInformations();
                  if (actualFile!=null)
                  {
//                      reloadOutput();
                      loadResult(actualFile,false);
                  }
               }
            }
         );
         setupMenu.add(item);
         item=new JMenuItem("Set Plot Fonts");
         item.addActionListener(new ActionListener()
             {
                public void actionPerformed(ActionEvent evt)
                {
                 // final JFrame f=new JFrame("Choose the Fonts for Axis");
                   // final JComboBox jcomb=new JComboBox(BIGPlotable.fontNames);
                   final Font fonts[]=new Font[BIGPlotable.fontNames.length];
                   // do not use BIGUtility.getFont for using the real defaults
                   fonts[0] = getFont("XAxis");
                   if (fonts[0]==null)
                   {
                      System.out.println("Axis Font settings not found in BGUI.cfg");
                      System.out.println("Inserting default");
                      Font lFont=(new Font( "SansSerif", Font.BOLD, 14 ));
                      Font tFont=(new Font( "SansSerif", Font.BOLD, 16 ));
                      bigInterface.getBIGConfigFileParser().save();
                      File bguicfg=bigInterface.getBIGConfigFileParser().getFile();
                      String content = BIGFileHelper.getFileContent(bguicfg);
                      StringBuffer sb=new StringBuffer(content);
                      sb.append("\n# start auto filled axis font settings\n");
                      for (int i = 0; i<BIGPlotable.fontNames.length; i++)
                      {
                         sb.append(BIGPlotable.fontNames[i]+"Font = "+lFont.getName()+"\n");
                         sb.append(BIGPlotable.fontNames[i]+"FontStyle = "+lFont.getStyle()+"\n");
                         if ( i == 1 )
                         {
                        	 sb.append(BIGPlotable.fontNames[i]+"FontSize = "+tFont.getSize()+"\n");
                         }
                         else
                         {
                        	 sb.append(BIGPlotable.fontNames[i]+"FontSize = "+lFont.getSize()+"\n");
                         }
                         sb.append(BIGPlotable.fontNames[i]+"Font = "+tFont.getName()+"\n");
                      }
                      sb.append("# end auto filled axis font settings");
                      BIGFileHelper.saveToFile( sb.toString() , bguicfg ) ;
                      bigInterface.setBIGConfigFileParser( new
                          BIGConfigFileParser( bguicfg.getAbsolutePath() ) ) ;
                      fonts[0]=lFont;
                   }
                   for (int i=0; i<BIGPlotable.fontNames.length; i++)
                   {
                      fonts[i]=BIGUtility.getFont(BIGPlotable.fontNames[i].toString());
                   }
                   JFrame frame = new BIGFontDialog( fonts );
                   frame.setVisible( true );

                   /*
                   final JLabel field = new JLabel("Test 1 2 3 2^20") ;

                   final JPanel fontPane=new JPanel(new BorderLayout());
                   final JComboBox fontBox=new JComboBox(
                        GraphicsEnvironment
                        .getLocalGraphicsEnvironment()
                        .getAvailableFontFamilyNames()
                       );
                   String [] siz={"8","9","10","12","14","16","18","20","24","28","32","40"};
                   final JComboBox size=new JComboBox(siz);
                   JPanel styl=new JPanel(new FlowLayout());
                   final JCheckBox ital=new JCheckBox("italic",fonts[0].isItalic());
                   final JCheckBox bol=new JCheckBox("bold",fonts[0].isBold());

                   styl.add(ital);
                   styl.add(bol);

                   for ( int i = 0 ; i < fontBox.getItemCount() ; i++ )
                   {
                      if ( ( fonts[ 0 ].getName().toUpperCase() ).equals( fontBox.
                          getItemAt(
                              i ).toString().toUpperCase() ) )
                         fontBox.setSelectedIndex( i ) ;
                   }
                   fontBox.setSelectedItem( fonts[0].getFontName() ) ;
                   for ( int i = 0 ; i < size.getItemCount() ; i++ )
                      if ( ( "" +
                             fonts[0].getSize() ).equals( size.getItemAt(
                                 i ) ) )
                         size.setSelectedIndex( i ) ;
                   ital.setSelected(fonts[0].isItalic());
                   bol.setSelected(fonts[0].isBold());

                   fontPane.add(fontBox,BorderLayout.CENTER);
                   fontPane.add(field,BorderLayout.NORTH);
                   fontPane.add(size,BorderLayout.EAST);
                   fontPane.add(styl,BorderLayout.SOUTH);
                   JPanel comboPane=new JPanel(new FlowLayout());
                   comboPane.add(new JLabel("set Font for"));
                   comboPane.add(jcomb);
                   f.getContentPane().add(comboPane,BorderLayout.NORTH);
                   JPanel fPan=new JPanel(new BorderLayout());
                   fPan.add(fontPane,BorderLayout.CENTER);
                   fPan.add(field,BorderLayout.SOUTH);
                   f.getContentPane().add(fPan,BorderLayout.CENTER);
                   JPanel buttonPane=new JPanel(new FlowLayout());
                   jcomb.addActionListener( new ActionListener()
                   {
                      int oldSelected = 0 ;
                      public void actionPerformed( ActionEvent ae )
                      {
                         int style=0;
                         if (ital.isSelected())
                            style=style+Font.ITALIC;
                         if (bol.isSelected())
                            style=style+Font.BOLD;
                         fonts[ oldSelected ] = new Font( ( String ) fontBox.
                             getSelectedItem() ,style,
                             Integer.parseInt( ( String ) size.getSelectedItem() )  ) ;
                         Font newFont=fonts[ jcomb.getSelectedIndex() ];
                         for ( int i = 0 ; i < fontBox.getItemCount() ; i++ )
                         {
                            if ( ( newFont.getName().toUpperCase() ).equals( fontBox.
                                getItemAt(
                                    i ).toString().toUpperCase() ) )
                               fontBox.setSelectedIndex( i ) ;
                   }
                         for ( int i = 0 ; i < size.getItemCount() ; i++ )
                            if ( ( "" +
                                   newFont.getSize() ).equals( size.getItemAt(
                                       i ) ) )
                               size.setSelectedIndex( i ) ;
                         ital.setSelected(newFont.isItalic());
                         bol.setSelected(newFont.isBold());
                         fontPane.revalidate();
                         oldSelected=jcomb.getSelectedIndex();
                      }
                   } ) ;
                   JButton ok = new JButton( "Okay" ) ;
                   ok.addActionListener( new ActionListener()
                   {
                      public void actionPerformed( ActionEvent ae )
                      {
                         for (int i=0;i<jcomb.getActionListeners().length;i++)
                         jcomb.getActionListeners()[i].actionPerformed(null);
                         for (int i=0;i<jcomb.getItemCount();i++)
                         {
                            bigInterface.getBIGConfigFileParser().set( jcomb.
                                getItemAt( i ) + "Font" ,
                                fonts[ i ].getName() ) ;

                            bigInterface.getBIGConfigFileParser().set( jcomb.
                                getItemAt( i ) + "FontStyle" ,
                                ""+fonts[ i ].getStyle() ) ;

                            bigInterface.getBIGConfigFileParser().set( jcomb.
                                getItemAt( i ) + "FontSize" ,
                                ""+fonts[ i ].getSize() ) ;
                         }
                         f.setVisible( false ) ;
                      }
                   } ) ;

                   JButton cancel = new JButton( "Cancel" ) ;
                   cancel.addActionListener( new ActionListener()
                   {
                      public void actionPerformed( ActionEvent ae )
                      {
                         f.setVisible( false ) ;
                      }
                   } ) ;
                   buttonPane.add(ok);
                   buttonPane.add(cancel);
                   f.getContentPane().add(buttonPane,BorderLayout.SOUTH);
                   f.pack();

                   (new Thread()
                   {
                      public void run()
                      {
                         while (f.isVisible())
                         {
                         int style=0;
                         if (ital.isSelected())
                            style=style|Font.ITALIC;
                         if (bol.isSelected())
                            style=style|Font.BOLD;
                         field.setFont(new Font( ( String ) fontBox.
                             getSelectedItem() , style,
                             Integer.parseInt( ( String ) size.
                                               getSelectedItem() ) ) ) ;

                            field.revalidate();
                           try
                           {
                              this.sleep( 60 ) ;
                           }
                           catch ( InterruptedException ex )
                           {
                           }
                         }
                      }
                   }).start();

                   f.setVisible(true);*/


                }
                public Font getFont(String s)
                {
                  try
                  {
                     String name = bigInterface.getBIGConfigFileParser().
                         stringCheckOut( s + "Font" ) ;
                     int style = bigInterface.getBIGConfigFileParser().
                         intCheckOut( s + "FontStyle" ) ;
                     int size = bigInterface.getBIGConfigFileParser().
                         intCheckOut( s + "FontSize" ) ;
                     return new Font( name , style , size ) ;
                  }
                  catch ( Exception ex )
                  {
                     return null ;
                  }

                }
             }
          );
          setupMenu.add(item);
         item=new JMenuItem("Set Standard Plot Comment");
         item.addActionListener(new ActionListener()
             {
                public void actionPerformed(ActionEvent evt)
                {
                   String comment = null;
                  try
                  {
                     comment = bigInterface.getBIGConfigFileParser().
                         stringCheckOut( "plotComment" ) ;
                  }
                  catch ( Exception ex )
                  {

                     bigInterface.getBIGConfigFileParser().save();
                     File bguicfg=bigInterface.getBIGConfigFileParser().getFile();
                     String content = BIGFileHelper.getFileContent(bguicfg);
                     StringBuffer sb=new StringBuffer(content);
                     sb.append("\n# start auto filled settings\n");
                        sb.append("plotComment = \n");
                        sb.append("plotCommentPercentX = 80\n");
                        sb.append("plotCommentPercentY = 90\n");

                     sb.append("# end auto filled settings");
                     BIGFileHelper.saveToFile( sb.toString() , bguicfg ) ;
                     bigInterface.setBIGConfigFileParser( new
                         BIGConfigFileParser( bguicfg.getAbsolutePath() ) ) ;
                     comment="";
                  }
                  String newComment=JOptionPane.showInputDialog( "Insert the new Standard Plot Comment, use <> to find settings in output-file (eg. use <date>)" ,
                                               comment ) ;
                  if (newComment==null)
                     return;
                  bigInterface.getBIGConfigFileParser().set("plotComment",newComment);
                  int i=-1;
                  while(i==-1)
                  {
                     try
                     {
                        i = Integer.parseInt( JOptionPane.showInputDialog(
                            "Start Position of x-coordinate for comment (Default is 80)." ,
                            "" +
                            bigInterface.getBIGConfigFileParser().
                            intCheckOut( "plotCommentPercentX" ) ) ) ;
                     }
                     catch ( Exception ex1 )
                     {
                     }
                  }
                  bigInterface.getBIGConfigFileParser().set("plotCommentPercentX",""+i);
                  i=-1;
                  while(i==-1)
                  {
                     try
                     {
                        i = Integer.parseInt( JOptionPane.showInputDialog(
                            "Start Position of y-coordinate for comment (Default is 90)." ,
                            "" +
                            bigInterface.getBIGConfigFileParser().
                            intCheckOut( "plotCommentPercentY" ) ) ) ;
                     }
                     catch ( Exception ex1 )
                     {
                     }
                  }
                  bigInterface.getBIGConfigFileParser().set("plotCommentPercentY",""+i);

                }
             }
          );
          setupMenu.add(item);
         item=new JMenuItem("Set Standard Title");
         item.addActionListener(new ActionListener()
             {
                public void actionPerformed(ActionEvent evt)
                {
                   String comment = null;
                  try
                  {
                     comment = bigInterface.getBIGConfigFileParser().
                         stringCheckOut( "standardTitle" ) ;
                  }
                  catch ( Exception ex )
                  {

                     bigInterface.getBIGConfigFileParser().save();
                     File bguicfg=bigInterface.getBIGConfigFileParser().getFile();
                     String content = BIGFileHelper.getFileContent(bguicfg);
                     StringBuffer sb=new StringBuffer(content);
                     sb.append("\n# start auto filled settings\n");
                        sb.append("standardTitle = \n");

                     sb.append("# end auto filled settings");
                     BIGFileHelper.saveToFile( sb.toString() , bguicfg ) ;
                     bigInterface.setBIGConfigFileParser( new
                         BIGConfigFileParser( bguicfg.getAbsolutePath() ) ) ;
                     comment="";
                  }
                  String newComment=JOptionPane.showInputDialog( "Insert the new Standard Title, use <> to find settings in output-file (eg. use <date>)" ,
                                               comment ) ;
                  if (newComment==null)
                     return;
                  bigInterface.getBIGConfigFileParser().set("standardTitle",newComment);

                }
             }
          );
          setupMenu.add(item);
          item=new JMenuItem("Set Default Plot-Insets");
          item.addActionListener(new ActionListener()
              {
                 public void actionPerformed(ActionEvent e)
                 {
                    Insets i = null ;
                    try
                    {

                       i = new Insets(
                            bigInterface.getBIGConfigFileParser().intCheckOut( "plotInsetsTop" ) ,
                            bigInterface.getBIGConfigFileParser().intCheckOut( "plotInsetsLeft" ) ,
                            bigInterface.getBIGConfigFileParser().intCheckOut( "plotInsetsBottom" ) ,
                            bigInterface.getBIGConfigFileParser().intCheckOut( "plotInsetsRight" )
                           ) ;
                       // use standard if exception
                    }
                    catch ( Exception ex)
                    {
                       bigInterface.getBIGConfigFileParser().addEntry("plotInsetsTop","4");
                       bigInterface.getBIGConfigFileParser().addEntry("plotInsetsLeft","8");
                       bigInterface.getBIGConfigFileParser().addEntry("plotInsetsBottom","4");
                       bigInterface.getBIGConfigFileParser().addEntry("plotInsetsRight","32");
                       i = new Insets(4,8,4,32);
                       bigInterface.getBIGConfigFileParser().save();
                    }

                    BIGInsetsPanel inpan=new BIGInsetsPanel(i);
                    int ret=JOptionPane.showConfirmDialog(BIGGUI.this.getContentPane(),inpan,"Select the new standard insets",JOptionPane.OK_CANCEL_OPTION);
                    if (ret==JOptionPane.CANCEL_OPTION)
                       return;
                    else
                    {
                       bigInterface.getBIGConfigFileParser().set("plotInsetsTop",""+inpan.getTheInsets().top);
                       bigInterface.getBIGConfigFileParser().set("plotInsetsLeft",""+inpan.getTheInsets().left);
                       bigInterface.getBIGConfigFileParser().set("plotInsetsBottom",""+inpan.getTheInsets().bottom);
                       bigInterface.getBIGConfigFileParser().set("plotInsetsRight",""+inpan.getTheInsets().right);
                    }
                 }
              });
          setupMenu.add(item);





        JMenuItem environmentEditorItem=new JMenuItem("Edit Environments");
        environmentEditorItem.setToolTipText("Edit Environments");
        environmentEditorItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
               JFrame frame=new JFrame("Environments");
               frame.setIconImage( new ImageIcon( BIGInterface.getInstance().getBenchItPath()
               		+ File.separator + "gui" + File.separator + "img"
               		+ File.separator + "clock.png" ).getImage() );
               frame.setGlassPane(new JPanel());
               frame.setContentPane(new BIGEnvironmentEditor());
               frame.pack();
               frame.setVisible(true);
            }
         } ) ;
         setupMenu.addSeparator() ;
        setupMenu.add(environmentEditorItem);

        menubar.add(setupMenu);
        if (BIGInterface.getSystem() == BIGInterface.UNIX_SYSTEM)
            measureMenu.add(executeSelectedMenuItem);

         JMenuItem stopItem=new JMenuItem("Stop Measurement");
         stopItem.setToolTipText("Stops local running Measurement");
         stopItem.addActionListener(new ActionListener() {
             public void actionPerformed(ActionEvent e) {
                if (executedProcess!=null)
                {
                   executedProcess.destroy() ;
                   executedProcess=null;
                }
                else
                   System.out.println("There is no running process.");
             }
          });
         if (BIGInterface.getSystem() == BIGInterface.UNIX_SYSTEM)
             measureMenu.add(stopItem);

        measureMenu.addSeparator();
        // this menu is not need any longer as we have exec in remote folder now
        //measureMenu.add(executeSelectedRemoteMenuItem);
        // add remote measure menu items to measure menu
        addListItems(remoteLists, measureMenu, 1, false);
        // add DB measure menu items to measure menu
        addListItems(dbLists, measureMenu, 1);
        measureMenu.addSeparator();
        measureMenu.add(updateKernelTreeMenuItem);
        menubar.add(measureMenu);

        evaluateMenu.add(showResultMenuItem);
        // add remote evaluate menu items to evaluate menu
        addListItems(remoteLists, evaluateMenu, 2);
        evaluateMenu.addSeparator();
        // this is not implemented up to now
        //evaluateMenu.add(loadUpMenuItem);
        evaluateMenu.add(dbMenuItem);
        // add DB evaluate menu items to evaluate menu
        addListItems(dbLists, evaluateMenu, 2, false);
        evaluateMenu.addSeparator();
        evaluateMenu.add(updateResultTreeMenuItem);
        menubar.add(evaluateMenu);

        viewMenu.add(toggleViewMenuItem);
        // do not display the view menu because of one item that is also
        // accessible via the toolbar
        //menubar.add(viewMenu);

        helpMenu.add(helpMenuItem);
        helpMenu.addSeparator();
        helpMenu.add(tipMenuItem);
        helpMenu.addSeparator();
        helpMenu.add(aboutMenuItem);
        menubar.add(helpMenu);
        /*
              /////////////////////////////////////////////////
              // this is the old menu structure
              /////////////////////////////////////////////////
              // the menus that will be displayed in the menu bar
              JMenu programMenu = new JMenu( "BenchIT" );
              programMenu.setMnemonic( KeyEvent.VK_B );
              viewMenu = new JMenu( "View" );
              viewMenu.setMnemonic( KeyEvent.VK_V );
              // kernel, result, localdefs, and database menus need to be
              // class global to en-/disable them as needed
              kernelMenu = new JMenu( "Kernels" );
              kernelMenu.setMnemonic( KeyEvent.VK_K );
              resultMenu = new JMenu( "Results" );
              resultMenu.setMnemonic( KeyEvent.VK_R );
              localdefMenu = new JMenu( "Localdefs" );
              localdefMenu.setMnemonic( KeyEvent.VK_O );
              JMenu helpMenu = new JMenu( "Help" );
              helpMenu.setMnemonic( KeyEvent.VK_H );

              programMenu.add( adminMenuItem );
              programMenu.add( dbMenuItem );

              // localMenu will be a sub menu of the "BenchIT" menu containing
              // the menu items "Localdefs", "Kernels", and "Results"
              JMenu localMenu = new JMenu( "Local" );
              localMenu.add( localdefMenuItem );
              localMenu.add( kernelsMenuItem );
              localMenu.add( resultsMenuItem );
              programMenu.add( localMenu );

              programMenu.addSeparator();
              programMenu.add( preferencesMenuItem );

              programMenu.addSeparator();
              programMenu.add( quitMenuItem );
              menubar.add( programMenu );

              viewMenu.add( graphicViewMenuItem );
              viewMenu.add( textViewMenuItem );
              menubar.add( viewMenu );

              localdefMenu.add( loadMenuItem );
              localdefMenu.add( saveMenuItem );
              localdefMenu.add( saveAsMenuItem );
              menubar.add( localdefMenu );

              kernelMenu.add( executeMenuItem );
              kernelMenu.add( executeSelectedMenuItem );
              kernelMenu.add( executeAllMenuItem );
              kernelMenu.add( executeSelectedRemoteMenuItem ) ;
              kernelMenu.addSeparator() ;
              kernelMenu.add( updateKernelTreeMenuItem );
              menubar.add( kernelMenu ) ;

              resultMenu.add( showResultMenuItem );
              // As it's not implemented yet, it's commented out
              //resultMenu.add( loadUpMenuItem );
              resultMenu.add( updateResultTreeMenuItem );
              menubar.add( resultMenu );

              // The contents of the databaseMenu are delivered by an
              // other class and my change by the other class' needs.
              // If you're interested: MyJFrame.getMenu().
              menubar.add( databaseMenu );

              remoteMenu = new BIGRemoteMenu( this );
              kernelMenu.add( remoteMenu );
              menubar.add( remoteMenu );

              helpMenu.add( helpMenuItem );
              helpMenu.add( tipMenuItem );
              helpMenu.add( aboutMenuItem );
              menubar.add( helpMenu );
         */

        // set the constructed menu bar
        setJMenuBar(menubar);

        progressLabel.setText("Initializing GUI: Creating toolbar");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}

        // now create the tool bar
        toolbar = new JToolBar();

        // we need the label font later for the item list of the
        // localdef file view
        labelFont = (new JButton(localdefsItemAction)).getFont();
        labelFont = labelFont.deriveFont(Font.PLAIN);

        JButton button = new JButton(localdefsItemAction);
        button.setMaximumSize(new Dimension(40,40));
        ImageIcon icon = new ImageIcon(bigInterface.getBenchItPath() +
                                       File.separator + "gui" + File.separator +
                                       "img" + File.separator + "localdef.png");
        if (icon != null) button.setIcon(icon);
        else button.setText("Localdefs");
        toolbar.add(button);

        /*      button = new JButton( kernelsItemAction );
              icon = new ImageIcon(bigInterface.getBenchItPath()+File.separator+"gui"+File.separator+ "img" + File.separator + "kernel.png" );
              if ( icon != null ) button.setIcon( icon );
              else button.setText( "Kernels" );
              //toolbar.add( button );

              button = new JButton( resultsItemAction );
              icon = new ImageIcon( bigInterface.getBenchItPath()+File.separator+"gui"+File.separator+"img" + File.separator + "result.png" );
              if ( icon != null ) button.setIcon( icon );
              else button.setText( "Results" );
              //toolbar.add( button );*/

        button = new JButton(databaseItemAction);
        button.setMaximumSize(new Dimension(40,40));
        icon = new ImageIcon(bigInterface.getBenchItPath() + File.separator +
                             "gui" + File.separator + "img" + File.separator +
                             "webdb.png");
        if (icon != null) button.setIcon(icon);
        else button.setText("Web Database");
        toolbar.add(button);

        toolbar.addSeparator();

        /*      button = new JButton( graficalViewItemAction );
              icon = new ImageIcon( bigInterface.getBenchItPath()+File.separator+"gui"+File.separator+"img" + File.separator + "view.png" );
              if ( icon != null ) button.setIcon( icon );
              else button.setText( "Grafical View" );
              //toolbar.add( button );

              button = new JButton( textualViewItemAction );
              icon = new ImageIcon( bigInterface.getBenchItPath()+File.separator+"gui"+File.separator+"img" + File.separator + "text.png" );
              if ( icon != null ) button.setIcon( icon );
              else button.setText( "Textual View" );
              //toolbar.add( button );*/
           choiceList = new JList( this.choiceTypes ) ;
           choiceList.setSelectionMode( ListSelectionModel.SINGLE_SELECTION ) ;
           choiceList.setCellRenderer( new ListCellRenderer()
           {
              JRadioButton[] radioButtons=new JRadioButton[3];
              public Component getListCellRendererComponent(
                  JList list ,
                  Object value ,
                  int index ,
                  boolean isSelected ,
                  boolean cellHasFocus )
              {
                 JRadioButton label = new JRadioButton() ;
                 label.setSelected(isSelected);
                 label.setSize(25,9);
                 label.setMaximumSize(new Dimension(25,9));
                 radioButtons[index]=label;
                 Font f = label.getFont() ;
                 label.setText( value.toString() ) ;
                 if ( isSelected )
                 {
                    Font newF = new Font( f.getFontName() , f.getStyle() , 9 ) ;
                    label.setForeground( Color.BLACK ) ;
                    label.setBackground( Color.WHITE ) ;
                    label.setFont( newF ) ;
                    label.setMaximumSize(label.getPreferredSize());
                 }
                 else
                 {
                    Font newF = new Font( f.getFontName() , f.getStyle() , 9 ) ;
                    label.setFont( newF ) ;
                    label.setMaximumSize(label.getPreferredSize());
                 }

                 return label ;
              }

           } ) ;
           choiceList.setFixedCellWidth(120) ;
           choiceList.setFixedCellHeight(12) ;
           choiceList.setBackground( toolbar.getBackground() ) ;
           choiceList.setBorder( javax.swing.BorderFactory.createEtchedBorder() ) ; //Color.BLACK));
           choiceList.setSelectionForeground( Color.BLACK ) ;
           choiceList.setSelectionBackground( Color.WHITE ) ;
           choiceList.setBackground(Color.LIGHT_GRAY);

           choiceList.setSelectedIndex( this.defaultSelected ) ;
           choiceList.setToolTipText(
                   "Choose the steps carried out on execution");

           toolbar.addSeparator() ;
           toolbar.add( choiceList ) ;
           button = new JButton( executeSelectedItemAction ) ;
           button.setMaximumSize(new Dimension(40,40));
           icon = new ImageIcon( bigInterface.getBenchItPath() + File.separator +
                                 "gui" + File.separator + "img" + File.separator +
                                 "exec.png" ) ;
           if ( icon != null ) button.setIcon( icon ) ;
           else button.setText( "Execute Selected" ) ;
           if ( BIGInterface.getSystem() == BIGInterface.UNIX_SYSTEM )
              toolbar.add( button ) ;
           toolbar.addSeparator() ;

           button = new JButton( toggleViewItemAction ) ;
           button.setMaximumSize( new Dimension( 40 , 40 ) ) ;
           icon = new ImageIcon( bigInterface.getBenchItPath() + File.separator
                                 + "gui" + File.separator + "img" + File.separator +
                                 "toggle.png" ) ;
           if ( icon != null ) button.setIcon( icon ) ;
           else button.setText( "Toggle View - Text/Graphic" ) ;
           toolbar.add( button ) ;

           button = new JButton( showResultItemAction ) ;
           icon = new ImageIcon( bigInterface.getBenchItPath() + File.separator +
                                 "gui" + File.separator + "img" + File.separator +
                                 "show.png" ) ;
        if (icon != null) button.setIcon(icon);
        else button.setText("Show Result");
        //toolbar.add( button );
        //toolbar.addSeparator();


        if (debug > 0) {
            button = new JButton("DebugDB");
            button.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    bigInterface.debugDB();
                }
            });
            toolbar.add(button);
            toolbar.addSeparator();
        }

        boolean enableUpdate=false;
        try
        {
           enableUpdate = bigInterface.getBIGConfigFileParser().boolCheckOut(
               "updateEnabled" ) ;
        }
        catch ( Exception ex2 )
        {
                    System.err.println("option \"updateEnabled\" not found in BGUI.cfg");
                    System.err.println("Setting it to true. (updateEnabled=1)");
                    bigInterface.getBIGConfigFileParser().addEntry("updateEnabled","1");
        }
        if (enableUpdate)
        {
           progressLabel.setText("Looking for update");
           
           icon = new ImageIcon( bigInterface.getBenchItPath() +
                   File.separator +
                   "gui" + File.separator + "img" +
                   File.separator +
                   "update.png" ) ;
           
           boolean test = false;
           
           String upd = null ;
           try
           {
              upd = bigInterface.getBIGConfigFileParser().stringCheckOut(
                  "updateServer" ) ;
           }
           catch ( Exception ex2 )
           {
                    System.err.println(
                        "option \"updateServer\" not found in BGUI.cfg" ) ;
                    bigInterface.getBIGConfigFileParser().addEntry("updateServer",
                        "http://www.benchit.org/~roberts/");
                    try
                    {
                       BIGUpdate.update( bigInterface.getBIGConfigFileParser().
                                         stringCheckOut( "updateServer" ) , false ) ;
                    }
                    catch ( Exception ex1 )
                    {
                       return;
                    }

           }
           toolbar.add(new JPanel());
           
           if (test) {
        	   button = new JButton( new AbstractAction( )
        	           {
        	              public void actionPerformed( ActionEvent ae )
        	              {
        	                    (new Thread()
        	                    { public void run()
        	                       {

        	                 int ret = JOptionPane.showConfirmDialog( BIGGUI.this ,
        	                     "An update for BenchIT-GUI is available. Do you want to download it? (This may take a while)" ,
        	                     "Update available" , JOptionPane.YES_NO_OPTION ) ;
        	                 if ( ret == JOptionPane.NO_OPTION )
        	                    return ;
        	                 try
        	                 {
        	                          BIGUpdate.update( bigInterface.getBIGConfigFileParser().
        	                                            stringCheckOut( "updateServer" ) , false ) ;
        	                 }
        	                 catch ( Exception ex )
        	                 {
        	                    ex.printStackTrace();
        	                    System.err.println(
        	                        "option \"updateServer\" not found in BGUI.cfg" ) ;
        	                    bigInterface.getBIGConfigFileParser().addEntry("updateServer",
        	                        "http://www.benchit.org/~roberts/");
        	                    try
        	                    {
        	                          BIGUpdate.update( bigInterface.
        	                                            getBIGConfigFileParser().
        	                                            stringCheckOut( "updateServer" ) , false ) ;

        	                    }
        	                    catch ( Exception ex1 )
        	                    {
        	                       return;
        	                    }
        	                 }
        	                 JOptionPane.showMessageDialog( BIGGUI.this ,
        	                     "Update was downloaded succesfully. Please restart the GUI now!" ) ;
        	                       }
        	                    }).start();

        	              }
        	           } ) ;
        	           
        	           if ( icon != null )
        	              button.setIcon( icon ) ;
        	           button.setEnabled( system.BIGUpdate.isUpdateAvailable( upd ) ) ;
        	           button.setToolTipText("Update GUI");
        	           toolbar.add(button);

        	           //--------------------------------------------------------------------
        	           
        	           JButton txtUpdBtn = new JButton(
        	        		   new AbstractAction() {
        	        			   public void actionPerformed(ActionEvent evt) {
        	        				   ( new Thread() {
        	        					   public void run() {
        	        						   int ret = JOptionPane.showConfirmDialog( BIGGUI.this ,
        	        				                     "An txt-update for BenchIT-GUI is available. Do you want to download it? (This may take a while)" ,
        	        				                     "Update available" , JOptionPane.YES_NO_OPTION ) ;
        	        						   
        	        						   if ( ret == JOptionPane.NO_OPTION )	return ;
        	        						   
        	        						   try {
        	        							   // System.out.println("Erster Textfile-Update-Versuch");
        	        							   BIGTextfileUpdate.update(
        	        									   bigInterface.getBIGConfigFileParser().
        	        									   stringCheckOut( "updateServer" )
        	        									   , false);
        	        						   }
        	        						   catch(Exception e1) {
        	        							   return;
        	        						   }
        	        						   JOptionPane.showMessageDialog( BIGGUI.this ,
        	        		                     "Update was downloaded succesfully. Please restart the GUI now!" ) ;
        	        					   }
        	        				   } ).start();
        	        			   }
        	        		   });
        	           if ( icon != null )
        	               txtUpdBtn.setIcon( icon ) ;
        	           txtUpdBtn.setToolTipText("Textfile-Update");
        	           txtUpdBtn.setEnabled( BIGTextfileUpdate.isTextfileUpdateAvailable( upd ) );
        	           toolbar.add( txtUpdBtn );
           }          
           
           // stores information about available updates
           // 0					-> no update available
           // everything else	-> update available
           //							general update , update for text files
           final Integer updAvail[] = {	new Integer(0) , new Integer(0) };
           JButton updateBtn = new JButton(
        		   new AbstractAction() {
        			   public void actionPerformed(ActionEvent evt) {
        				   ( new Thread() {
        					   public void run() {
        						   int ret;
        						   if ( updAvail[1].intValue() > 0 ) {
        							   // textfile update vailable
        							   ret = JOptionPane.showConfirmDialog( BIGGUI.this ,
         				                     	"An txt-update for BenchIT-GUI is available. Do you want to download it? (This may take a while)" ,
         				                     	"Update available" , JOptionPane.YES_NO_OPTION ) ;
        							   
        							   if ( ret == JOptionPane.YES_OPTION ){
        								   try {
        									   BIGTextfileUpdate.update(
                 								   bigInterface.getBIGConfigFileParser().
                 								   stringCheckOut( "updateServer" )
                 								   , false);
        								   }
        								   catch(Exception e1) {
        									   e1.printStackTrace();
        					        		   System.err.println(
        					        				   "option \"updateServer\" not found in BGUI.cfg" ) ;
        					        		   bigInterface.getBIGConfigFileParser().addEntry("updateServer",
        					        				   "http://www.benchit.org/downloads/update/");
        					        		   try {
        					        			   BIGUpdate.update( bigInterface.
      					                                            getBIGConfigFileParser().
      					                                            stringCheckOut( "updateServer" ) , false ) ;
        					        		   }
        					        		   catch ( Exception ex1 ) {
        					        			   // TODO: exception handling
        					        		   }
        								   }
        							   }
        						   }
        						   if ( updAvail[0].intValue() > 0 ) {
        							   // general update available
        							   ret = JOptionPane.showConfirmDialog( BIGGUI.this ,
        					                     "An update for BenchIT-GUI is available. Do you want to download it? (This may take a while)" ,
        					                     "Update available" , JOptionPane.YES_NO_OPTION ) ;
        					           if ( ret == JOptionPane.YES_OPTION ) {
        					        	   try {
        					        		   BIGUpdate.update( bigInterface.getBIGConfigFileParser().
        					        				   stringCheckOut( "updateServer" ) , false ) ;
        					        	   }
        					        	   catch ( Exception ex ) {
        					        		   // TODO: exception handling
                 							   // System.err.println("");
      					                 	}
        					           }
        						   }
        						   JOptionPane.showMessageDialog( BIGGUI.this ,
        								   "Update was downloaded succesfully. Please restart the GUI now!" ) ;
        					   }
        				   }).start();
        			   }
        		   });
           updateBtn.setEnabled(false);
           if ( BIGTextfileUpdate.isTextfileUpdateAvailable( upd ) ) {
        	   updAvail[1] = new Integer( 1 );
        	   updateBtn.setEnabled(true);
           }
           if ( BIGUpdate.isUpdateAvailable( upd ) ) {
        	   updAvail[0] = new Integer( 1 );
        	   updateBtn.setEnabled(true);
           }
           icon = new ImageIcon( bigInterface.getBenchItPath() +
                   File.separator +
                   "gui" + File.separator + "img" +
                   File.separator +
                   "update.png" ) ;
           if ( icon != null ) updateBtn.setIcon( icon ) ;
           updateBtn.setToolTipText("Update");
           toolbar.add( updateBtn );
           
           
           
           //--------------------------------------------------------------------
        }

        // localdefCombo
        localDefFilesComboBoxEntries = new String[3];
        localDefFilesComboBoxEntries[0] = "host info";
        localDefFilesComboBoxEntries[1] = "architecture info";
        localDefFilesComboBoxEntries[2] = "display info";
        localdefFilesComboBox = new JComboBox(localDefFilesComboBoxEntries);
        localdefFilesComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                removeCards();
                initCards();
                viewPanel.revalidate();
                textPanel.revalidate();
                mainViewPanel.revalidate();
            }
        });
        localdefFilesComboBox.setToolTipText(
                "Choose the LOCALDEF info you want to work with");
//      toolbar.add( localdefFilesComboBox );

        // viewlevelCombo
        int maximumViewLevel =
                BIGInterface.getInstance().getMaximumViewLevel();
        viewLevelComboBoxEntries = new String[maximumViewLevel + 1];
        for (int i = 0; i <= maximumViewLevel; i++) {
            String levelString = new String("Detail level " + detailString[i]);
            viewLevelComboBoxEntries[i] = levelString;
        }
        viewLevelComboBox = new JComboBox(viewLevelComboBoxEntries);
        final int level = maximumViewLevel;
        viewLevelComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JComboBox cb = (JComboBox) e.getSource();
                String viewLevelName = (String) cb.getSelectedItem();
                for (int i = 0; i <= level; i++) {
                    String levelString = new String("Detail level " +
                            detailString[i]);
                    if (viewLevelName.compareTo(levelString) == 0) {
                        viewLevel = i;
                        // redraws the list of displayed items
                        repaintTabs();
                        // declares the components to be valid again
                        viewPanel.revalidate();
                        textPanel.revalidate();
                        mainViewPanel.revalidate();
                    }
                }
            }
        });
        viewLevelComboBox.setToolTipText(
                "Choose the detail level of the shown information");
        //toolbar.add( viewLevelComboBox );
        //toolbar.addSeparator();

        // execlevelCombo
        //toolbar.add( choiceJCB );

        progressLabel.setText("Initializing GUI: Creating status bar");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}

        // create the status bar
        statusBar = new JToolBar();
        statusBar.setLayout(new BorderLayout());
        statusProgress = new BIGGUIObserverProgress(0, 100);
        statusLabel = new JLabel();
        statusLabel.setFont(new Font("SansSerif", Font.PLAIN, 10));
        statusLabel.setText("BenchIT starting");
        statusProgress.setPreferredSize(new Dimension(
                statusProgress.getMaximum(), 15));
        statusProgress.setMinimumSize(new Dimension(
                statusProgress.getMaximum(), 15));
        statusProgress.setMaximumSize(new Dimension(
                statusProgress.getMaximum(), 15));
        statusProgress.setFont(new Font("SansSerif", Font.PLAIN, 10));
        statusBar.add(statusProgress, BorderLayout.WEST);
        statusBar.addSeparator();
        statusBar.add(statusLabel, BorderLayout.CENTER);
        // setting a label, which shows free ressources
        final JLabel free = new JLabel();
        free.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent evt) {
                if (evt.getClickCount() > 1)
                    System.gc();
            }
        });
        free.setFont(new Font("SansSerif", Font.PLAIN, 10));
        final TimerTask task = new TimerTask() {
            public void run() {
                Thread t = new Thread() {
                    public void run() {
                        free.setText((int) ((Runtime.getRuntime().totalMemory() -
                                             Runtime.getRuntime().freeMemory()) /
                                            1048576) + " MB /" +
                                     (int) (Runtime.getRuntime().totalMemory() /
                                            1048576) + " MB");
                    }
                };
                SwingUtilities.invokeLater(t);

            }
        };
        java.util.Timer t = new java.util.Timer();
        t.scheduleAtFixedRate(task, 0, 10000);
        statusBar.add(free, BorderLayout.EAST);

        bigInterface.setStatusProgress(statusProgress);
        bigInterface.setStatusLabel(statusLabel);

        // false: admin panel is integrated in another GUI
        adminTool = new BIGAdmin( /*false*/);
        statusProgress.setString("");
        statusProgress.setValue(statusProgress.getMinimum());
        statusLabel.setText("done.");
        // Now we create the panels for the actual view area.
        // The viewPanel and textPanel are the contents of a
        // CardLayout and are displayed according to the user's
        // choice to "Grafical View" or "Textual View".
        cardsButtonPanel = new JPanel(new GridBagLayout());
        viewPanel = new JPanel(new GridBagLayout());
        textPanel = new JPanel(new GridBagLayout());
        mainViewPanel = new JPanel(new CardLayout());
        mainViewPanel.add(viewPanel, VIEWPANEL);
        mainViewPanel.add(textPanel, TEXTPANEL);
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(cardsButtonPanel, BorderLayout.NORTH);
        mainPanel.add(mainViewPanel, BorderLayout.CENTER);
        int numberOfMixers=1;
      try
      {
         numberOfMixers = bigInterface.getBIGConfigFileParser().intCheckOut(
             "numberOfMixers" ) ;
      }
      catch ( Exception ex1 )
      {
      }
        resultMixer = new BIGResultMixer[numberOfMixers];
        for (int i=0;i<numberOfMixers;i++)
        {
           resultMixer[i]=new BIGResultMixer( viewPanel , textPanel , "title"+(i+1) ) ;
           resultMixer[i].init(new File(bigInterface.getBenchItPath()+File.separator+
                               "gui"+File.separator+"cfg"+File.separator+"mixer_"+i));
           resultMixer[i].setResultTree(resultTree);
        }
        // listTabs is the area to the left, containing list and trees
        listTabbs = new JTabbedPane(JTabbedPane.TOP);
        // initialize the kernel and result tree
        // only the result tree may be updated during runtime of this
        // program: resultTreeUpdate()
        progressLabel.setText("Initializing GUI: Initializing lists and trees");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}
        initListTabbs();
        // the trees are placed in a JScrollPane and those scroll panes
        // are added to the listTabbs
        kernelTreeScrollPane =
                new JScrollPane(kernelTree);
        icon = new ImageIcon(bigInterface.getBenchItPath() + File.separator +
                             "gui" + File.separator + "img" + File.separator +
                             "kernel16.png");
        listTabbs.addTab(KERNELS_TABB, icon, kernelTreeScrollPane,
                         "Select and execute the kernels for benchmarking");
        JScrollPane resultTreeScrollPane =
                new JScrollPane(resultTree);
        icon = new ImageIcon(bigInterface.getBenchItPath() + File.separator +
                             "gui" + File.separator + "img" + File.separator +
                             "result16.png");
        listTabbs.addTab(RESULTS_TABB, icon, resultTreeScrollPane,
                         "Select and display or up load a result file");
        listTabbs.setPreferredSize(new Dimension(140, 430));

        progressLabel.setText("Initializing GUI: Initializing view panels");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}
        initCards();

        consoleScrollPane = console.getDisplayPanel();
        consoleScrollPane.setPreferredSize(
                new Dimension(dimx - 200, 100));
        Border border =
                BorderFactory.createEmptyBorder();
        TitledBorder titledBorder = BorderFactory.createTitledBorder(
                border, "Output console for: Messages | Warnings | Errors");
        titledBorder.setTitleJustification(TitledBorder.CENTER);
        titledBorder.setTitleFont(
                new Font("SansSerif", Font.PLAIN, 10));
        consoleScrollPane.setBorder(titledBorder);

        rightSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
                                        false, mainPanel, consoleScrollPane);
        // why doesn't the splitter move to it's new location??
        // anyone got an idea?
        if (rightSplit < 0) rightSplit = 550;
        rightSplitPane.setDividerLocation(rightSplit);
        rightSplitPane.setOneTouchExpandable(true);

        mainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                                       false, listTabbs, rightSplitPane);

        if (mainSplit < 0) mainSplit = 170;
        mainSplitPane.setDividerLocation(mainSplit);

        int w = dimx - mainSplitPane.getDividerLocation() - 35;
        int h = rightSplitPane.getDividerLocation() - 38;
        mainViewPanel.setPreferredSize(new Dimension(w, h));

        JPanel panel = new JPanel(new BorderLayout());
        panel.add(toolbar, BorderLayout.PAGE_START);
        toolbar.setFloatable(false);
        panel.add(statusBar, BorderLayout.PAGE_END);
        statusBar.setFloatable(false);
        panel.add(mainSplitPane, BorderLayout.CENTER);

        // We need this when switching states as we have to add
        // the toolbar to the different panels. Look at setGUIState()
        // for understanding the next statement
        normalPanel = (JComponent) mainSplitPane;

        setContentPane(panel);

        // as all components are initialized, add the listeners that can
        // conflict with uninitialized components earlier
        progressLabel.setText("Initializing GUI: Initializing listeners");
        startProgress.setValue(startProgress.getValue() + 1);
        try {
            Thread.sleep(0, 1);
        } catch (InterruptedException ie) {}

        resultTree.setListeners();
        kernelTree.setListeners();
        // JTree MUST be registered manually to display tool tips
//        ToolTipManager.sharedInstance().registerComponent(resultTree);

        listTabbs.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                if ((state != KERNELS) && (state != RESULTS)) {
                    forceListTabbView();
                }
            }
        });
        listTabbs.addFocusListener(new FocusListener() {
            public void focusLost(FocusEvent e) {}

            public void focusGained(FocusEvent e) {
                if (initDone == false) {
                    initDone = true;
                    return;
                }
//                if ((state != KERNELS) && (state != RESULTS)) {
//                    forceListTabbView();
//                }
            }
        });
        listTabbs.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                forceListTabbView();
            }
        });
        /* Deactivate the external console frame as it's now within the GUI. */
        console.setVisible(false);

        //this.setSession(null);
    }

    /**
     * Adds JMenutItems from a list of a certain category to a JMenu.
     * The first item will be a separator.
     * There are only 3 categories: 0 = setup, 1 = measure, 2 = evaluate.
     * The array lists must be of size 3.
     * @param lists The array of Lists.
     * @param menu The JMenu to add the menu items to.
     * @param category The category to choose the items from.
     **/
    private void addListItems(
            java.util.List[] lists, JMenu menu, int category) {
        addListItems(lists, menu, category, true);
    }

    /**
     * Adds JMenutItems from a list of a certain category to a JMenu.
     * The first item will be a separator.
     * There are only 3 categories: 0 = setup, 1 = measure, 2 = evaluate.
     * The array lists must be of size 3.
     * @param lists The array of Lists.
     * @param menu The JMenu to add the menu items to.
     * @param category The category to choose the items from.
     * @param separator If true, the first item is a separator.
     **/
    private void addListItems(
            java.util.List[] lists, JMenu menu, int category, boolean separator) {
        if (lists[category].size() > 0) {
            if (separator) menu.addSeparator();
            for (int i = 0; i < lists[category].size(); i++) {
                Object obj = lists[category].get(i);
                if (obj instanceof JMenuItem)
                    menu.add((JMenuItem) obj);
                else if ((obj instanceof String) &&
                         ((String) obj).compareTo("Separator") == 0)
                    menu.addSeparator();
            }
        }
    }

    /**
     * This method is called when the user chose "load" from the menu
     * or used the key combination CTRL+L. Depending on the current view
     * state of the GUI a proper reaction is choosen.
     **/
    public void loadRequest() {
        if (getGUIState() == LOCALDEFS) {
            editorFileRelation.clear();
            Object[] possibleChoices = (Object[]) getAvailableLOCALDEFS();
            Object selectedValue = JOptionPane.showInputDialog(
                    BIGGUI.this,
                    "Choose the hostname of which you want to load the" +
                    "LOCALDEFS.\nIf those files don't exist " +
                    "new files will be created for this hostname.",
                    "Load present LOCALDEF files",
                    JOptionPane.INFORMATION_MESSAGE, null,
                    possibleChoices, possibleChoices[0]);
            if ((selectedValue != null)
                && !selectedValue.toString().equals("")) {
                bigInterface.setHost(selectedValue.toString());
                loadNewHost();
                if (staticTitle.endsWith(bigInterface.getHost() + ")"))
                    setTitle(staticTitle + ": Localdef settings");
                else
                    setTitle(staticTitle + ": Localdef settings for host "
                             + bigInterface.getHost());
            }
        }
    }

    /**
     * This method is called when the user chose "save" from the menu
     * or used the key combination CTRL+S. Depending on the current view
     * state of the GUI a proper reaction is choosen.
     **/
    public void saveRequest() {
        if (getGUIState() == LOCALDEFS) {
            saveAll();
        } else if ((getGUIState() == KERNELS)
                   && (kernelScriptWindow != null)) {
            this.kernelScriptWindow.saveRequest(false);
        } else if (getGUIState() == ADMIN) {
            this.adminTool.save();
        }
    }

    /**
     * This method is called when the user chose "save as" from the menu.
     * Depending on the current view state of the GUI a proper reaction
     * is choosen.
     **/
    public void saveAsRequest() {
        if (getGUIState() == LOCALDEFS) {
            String host = JOptionPane.showInputDialog(
                    "Type in the host you want to save as.\nThe files " +
                    "for this host will be created or replaced in the " +
                    "LOCALDEFS directory.");
            if ((host != null) && !host.equals("")) {
                saveAs(host);
            }
        } else if ((getGUIState() == RESULTS)
                   && (plotWindow != null)) {
            this.plotWindow.saveAsRequest();
        }
    }

    /** Sends the result file to the benchit database of
     * results.
     * @param resultFile the absolute path name of the result file.
     **/
    public void loadUpResultFile(String resultFile) {
        //
        //
        // needs to be implemented
        // then uncomment code in result tree popop to activate popup entry
        //
        //
    }
    private BIGOutputFile actualFile=null;
    /** Creates a BIGQuickViewWindow and displays the result graph. */
    public void loadResult(final BIGOutputFile file,boolean save) {
       if ((this.actualFile!=null)&&(save))
       {
          final BIGOutputFile tempFile=actualFile;
          (new Thread()
          {
             public void run()
             {
                tempFile.save(new File(tempFile.getFile().getAbsolutePath()+".gui"));
             }
          }).start();
       }
        if (state != RESULTS) setGUIState(RESULTS);
        //final Thread thread = new Thread() {
        //    public void run() {
                quickViewWindow =
                        new BIGQuickViewWindow(thisBIGGUI, file);
                    this.actualFile=file;
                // remove all components from viewPanel and textPanel
                removeCards();
                // set new components to viewPanel and textPanel
                initCards();
                // declare the changed panels to be valid again
                cardsButtonPanel.revalidate();
                viewPanel.revalidate();
                textPanel.revalidate();
                mainViewPanel.revalidate();
                repaint();
                viewPanel.revalidate();
                textPanel.revalidate();
                mainViewPanel.revalidate();
                repaint();
                // for results it should be possible to switch to the text view
                toggleViewItemAction.setEnabled( true );
        //    }
        //};
        //thread.start();
    }

    public Object[] getAvailableLOCALDEFS() {
        Object[] retval = null;
        String dirName = bigInterface.getBenchItPath()
                         + File.separator + "LOCALDEFS" + File.separator;
        File file = null;
        try {
            file = new File(dirName);
        } catch (NullPointerException npe) {
            return retval;
        }
        if (file != null) {
            if (file.exists() && file.isDirectory()) {
                File[] obj = file.listFiles(new FileFilter() {
                    public boolean accept(File pathname) {
                        boolean ret = false;
                        if (pathname != null) {
                            if (pathname.exists() && pathname.isFile()) {
                                String name = pathname.getName();
                                if (!name.endsWith("_input_architecture")
                                    && !name.endsWith("_input_display")) {
                                    ret = true;
                                }
                            }
                        }
                        return ret;
                    }
                });
                if (obj != null) {
                    Arrays.sort(obj);
                    retval = (Object[])new String[obj.length];
                    for (int c = 0; c < obj.length; c++) {
                        retval[c] = (Object) (obj[c].getName());
                    }
                }
            }
        }
        return retval;
    }

    /**
     * Returns the state the GUI is in at the moment of invokation.
     * @return BIGGUI.[ADMIN|DATABASE|LOCALDEFS|KERNELS|RESULTS]
     **/
    public int getGUIState() {
        return state;
    }
    public JPanel getViewPanel()
    {
       return this.viewPanel;
    }
    public JPanel getTextPanel()
    {
       return this.textPanel;
    }
    public void setOnline(boolean on)
    {
       this.resultTree.setOnline(on);
       if (!on)
          this.rightSplitPane.setBottomComponent(this.consoleScrollPane);
       validate();
    }
    public void setOnline(boolean on, JComponent console)
    {
       this.setOnline(on);
       if (!on)
          this.rightSplitPane.setBottomComponent(console);
       validate();
    }


    /** This Method sets the state of the GUI. The only recognized
     * states are:<br>
     * ADMIN, DATABASE, LOCALDEFS, KERNELS, and RESULTS.<br>
     * If another state is requested, this method won't have an
     * effect.
     * @param inState either one of
     * ADMIN, DATABASE, LOCALDEFS, KERNELS, and RESULTS.
     */
    public void setGUIState(int inState) {
        this.rightSplit = this.rightSplitPane.getDividerLocation();
        JPanel panel = null;
        if (state == LOCALDEFS) {
            // force save entries if old state was LOCALDEFS
            saveEntries();
        }
        if (state != inState) { // if the new state is different to the current one
            // set the new one as the current one
            state = inState;
            // enable all state switches
            adminItemAction.setEnabled(true);
            databaseItemAction.setEnabled(true);
            localdefsItemAction.setEnabled(true);
//         kernelsItemAction.setEnabled( true );
//         resultsItemAction.setEnabled( true );
            // disable the toolbar buttons
            //showResultItemAction.setEnabled( false );
            //executeSelectedItemAction.setEnabled( false );
            // disable toolbar combo boxes
//            choiceJCB.setEnabled(false);
            localdefFilesComboBox.setEnabled(false);
            viewLevelComboBox.setEnabled(false);
            // enable tool bar view mode buttons
//         graficalViewItemAction.setEnabled(true);
//         textualViewItemAction.setEnabled(true);
            toggleViewItemAction.setEnabled(true);
            // disable the menus of old menu structure
            //kernelMenu.setEnabled( false );
            //resultMenu.setEnabled( false );
            //localdefMenu.setEnabled( false );
            //databaseMenu.setEnabled( false );
            loadMenuItem.setEnabled(false);
            saveMenuItem.setEnabled(false);
            saveAsMenuItem.setEnabled(false);
            // preset the state of the BIGInterface to normal mode
//         bigInterface.setLoadAdminTool( false );
            // preset the view mode to graphical view
            textViewMode = false;
            ((CardLayout) (mainViewPanel.getLayout())).show(
                    mainViewPanel, VIEWPANEL);
        } else return;
        switch (inState) {
        case ADMIN:

            // got to be set because of the databaseconnection
            //this.rightSplitPane.setBottomComponent(this.consoleScrollPane);
            this.rightSplitPane.setDividerLocation(this.rightSplit);

            // disable adminItemAction as it doesn't make much
            // sense to switch into this mode if we are already in it
            adminItemAction.setEnabled(false);

            // disable tool bar view mode buttons
            //this.viewMenu.setEnabled(false);
            //graficalViewItemAction.setEnabled( false );
            //textualViewItemAction.setEnabled( false );
            toggleViewItemAction.setEnabled(false);
            saveMenuItem.setEnabled(true);
            saveMenuItem.setToolTipText(
                    "Save LOCALDEF variable configuration to config.xml");

            // must be set to force the BIGInterface to execute the right
            // code when loading and saving files
//            bigInterface.setLoadAdminTool( true );

            setTitle(staticTitle + ": AdminTool");

//            adminPanel = adminTool.getDisplayPanel();

            panel = new JPanel(new BorderLayout());
            panel.add(toolbar, BorderLayout.PAGE_START);
            panel.add(statusBar, BorderLayout.PAGE_END);
            panel.add(normalPanel, BorderLayout.CENTER);
            setContentPane(panel);
            if (this.rightSplitPane.getBottomComponent() instanceof conn.MainEditPanel)
            {
               ( ( conn.MainEditPanel )this.rightSplitPane.getBottomComponent() ).
                   setSelectedIndex( 3 ) ;
            }
            validate();
            break;
        case DATABASE:

            // if we are connected
            /*if (this.connectionFrame.getMenu().isEnabled()) {
                this.connectionFrame.setConsole(this.thisBIGGUI);
            }*/

            // disable databaseItemAction as it doesn't make much
            // sense to switch into this mode if we are already in it
            //databaseItemAction.setEnabled( false );
            // disable tool bar view mode buttons
            //this.viewMenu.setEnabled(false);
            //graficalViewItemAction.setEnabled( false );
            //textualViewItemAction.setEnabled( false );
            toggleViewItemAction.setEnabled(false);

            //enable the database menu
            // now this does the MyJFrame itself
            //databaseMenu.setEnabled(true);

            panel = new JPanel(new BorderLayout());
            panel.add(toolbar, BorderLayout.PAGE_START);
            panel.add(statusBar, BorderLayout.PAGE_END);
            panel.add(connectionFrame.getDisplayPanel(),
                      BorderLayout.CENTER);
            setContentPane(panel);
            setTitle("BenchIT - Web Database");
            validate();
            break;
        case LOCALDEFS:

            // got to be set because of the databaseconnection
            //this.rightSplitPane.setBottomComponent(this.consoleScrollPane);
            this.rightSplitPane.setDividerLocation(this.rightSplit);

            // enabling the viewMenu
            //this.viewMenu.setEnabled(true);
            // disable localdefsItemAction as it doesn't make much
            // sense to switch into this mode if we are already in it
            //localdefsItemAction.setEnabled( false );
            // enable toolbar combo boxes
            localdefFilesComboBox.setEnabled(true);
            viewLevelComboBox.setEnabled(true);
            loadMenuItem.setEnabled(true);
            saveMenuItem.setEnabled(true);
            saveAsMenuItem.setEnabled(true);
            loadMenuItem.setToolTipText(
                    "Load the LOCALDEFS of a different host");
            saveMenuItem.setToolTipText("Save the changes on the LOCALDEFS");
            saveAsMenuItem.setToolTipText(
                    "Save a copy of the current LOCALDEFS for a new host");

            // enable the localdef menu
            //localdefMenu.setEnabled( true );

            panel = new JPanel(new BorderLayout());
            panel.add(toolbar, BorderLayout.PAGE_START);
            panel.add(statusBar, BorderLayout.PAGE_END);
            panel.add(normalPanel, BorderLayout.CENTER);
            setContentPane(panel);
            if (staticTitle.endsWith(bigInterface.getHost() + ")"))
                setTitle(staticTitle + ": Localdef settings");
            else
                setTitle(staticTitle + ": Localdef settings for host "
                         + bigInterface.getHost());
            if (this.rightSplitPane.getBottomComponent() instanceof conn.MainEditPanel)
            {
               ( ( conn.MainEditPanel )this.rightSplitPane.getBottomComponent() ).
                   setSelectedIndex( 3 ) ;
            }

            validate();
            break;
        case KERNELS:

            // disabling the viewMenu
            // this.viewMenu.setEnabled(false);

            // set the view mode to textual view
            textViewMode = true;
            ((CardLayout) (mainViewPanel.getLayout())).show(
                    mainViewPanel, TEXTPANEL);

            // disable kernelsItemAction as it doesn't make much
            // sense to switch into this mode if we are already in it
            //kernelsItemAction.setEnabled( false );
            // enable the execute toolbar button
            //executeSelectedItemAction.setEnabled( true );
            // disable the graphical view toolbar button
//            graficalViewItemAction.setEnabled( false );
            toggleViewItemAction.setEnabled(false);
            saveMenuItem.setEnabled(true);
            saveMenuItem.setToolTipText("Save the changes of kernel files");

            // enable toolbar combo box
            choiceList.setEnabled(true);

            // enable the localdef menu
            //kernelMenu.setEnabled( true );
            // bring kernel tabb in front
            listTabbs.setSelectedIndex(
                    listTabbs.indexOfTab(KERNELS_TABB));

            panel = new JPanel(new BorderLayout());
            panel.add(toolbar, BorderLayout.PAGE_START);
            panel.add(statusBar, BorderLayout.PAGE_END);
            panel.add(normalPanel, BorderLayout.CENTER);
            setContentPane(panel);
            String title = staticTitle + ": Execute kernels";
            if (!staticTitle.endsWith(bigInterface.getHost() + ")")) {
                title = title + "; WARNING: current loaded LOCALDEFS belong"
                        + " to a different host: " + bigInterface.getHost();
                console.postMessage("WARNING: Please be aware that the"
                                    +
                        " current loaded LOCALDEFS belong to a different host"
                                    +
                        " than the one this BenchIT GUI is running\non. If you"
                                    +
                        " intend to to a remote execute this might be just"
                                    +
                        " wanted. However, if you're going to run kernels on"
                                    +
                        " the\npresent host, make sure you didn't alter the"
                                    + " wrong LOCALDEFS by accident.",
                                    BIGConsole.WARNING);
            }
            setTitle(title);
            if (this.rightSplitPane.getBottomComponent() instanceof conn.MainEditPanel)
            {
               ( ( conn.MainEditPanel )this.rightSplitPane.getBottomComponent() ).
                   setSelectedIndex( 3 ) ;
            }

            validate();
            this.rightSplitPane.setDividerLocation(this.rightSplit);
            break;
        case RESULTS:

            // got to be set because of the databaseconnection
            //this.rightSplitPane.setBottomComponent(this.consoleScrollPane);
            this.rightSplitPane.setDividerLocation(rightSplit);

            // enabling the viewMenu
            //this.viewMenu.setEnabled(true);
            // enabling switching to localdefs
            //this.localdefsItemAction.setEnabled(true);

            // disable resultsItemAction as it doesn't make much
            // sense to switch into this mode if we are already in it
            //resultsItemAction.setEnabled( false );
            // enable the show result toolbar button
            //showResultItemAction.setEnabled( true );
            saveAsMenuItem.setEnabled(true);
            saveAsMenuItem.setToolTipText(
                    "Save plot to a new .bit file");

            // enable result menu
            //resultMenu.setEnabled( true );
            // bring result tabb in front
            listTabbs.setSelectedIndex(
                    listTabbs.indexOfTab(RESULTS_TABB));

            panel = new JPanel(new BorderLayout());
            panel.add(toolbar, BorderLayout.PAGE_START);
            panel.add(statusBar, BorderLayout.PAGE_END);
            panel.add(normalPanel, BorderLayout.CENTER);
            setContentPane(panel);
            setTitle(staticTitle + ": View results");
            validate();
            break;
        default:
        }
        // remove all components from viewPanel and textPanel
        removeCards();
        // set new components to viewPanel and textPanel
        initCards();
        // declare the changed panels to be valid again
        cardsButtonPanel.revalidate();
        viewPanel.revalidate();
        textPanel.revalidate();
        mainViewPanel.revalidate();
    }

    private void forceListTabbView() {
        String selectedTabb =
                listTabbs.getTitleAt(listTabbs.getSelectedIndex());
        if (selectedTabb.compareTo(KERNELS_TABB) == 0) {
            setGUIState(KERNELS);
        } else if (selectedTabb.compareTo(RESULTS_TABB) == 0) {
            setGUIState(RESULTS);
        }
    }

    public void loadNewHost() {
        Thread loadThread = new Thread() {
            public void run() {
                try {
                    bigInterface.load();
                    removeCards();
                    initCards();
                    repaint();
                    mainViewPanel.revalidate();
                } catch (BIGParserException bpe) {
                    System.err.println("Cannot load file\n" + bpe);
                }
            }
        };
        loadThread.start();
    }

    /**
     * Initializes the kernel and result tree filling both
     * with appropriate information.
     **/
    private void initListTabbs() {
        kernels = new TreeSet();
        DefaultMutableTreeNode rootNode =
                new DefaultMutableTreeNode(kernelRootName);
        DefaultTreeModel model = new DefaultTreeModel(rootNode);
        kernelTree = new BIGKernelTree(model, this);
        kernelTree.setupTree();

        results = new TreeSet();
        rootNode = new DefaultMutableTreeNode(resultRootName);
        model = new DefaultTreeModel(rootNode);
        resultTree = new BIGResultTree(model, this);
        resultTree.setupTree();
    }

    /**
     * Recursive method to add all kernel and directory names
     * to the kernel tree.
     **/
    /*   private void addKernelsToTree(
          DefaultTreeModel model, DefaultMutableTreeNode parentNode,
          Map parent, Iterator it ) {
          while ( it.hasNext() ) {
             DefaultMutableTreeNode childNode = null;
             String key = (String)it.next();
             if ( key.equals( "<kernels>" ) ) {
                // add kernels to the tree
                TreeSet childSet = (TreeSet)(parent.get( key ));
                Iterator it2 = childSet.iterator();
                while ( it2.hasNext() ) {
                   Object nodeObj = it2.next();
                   if ( nodeObj instanceof BIGKernel ) {
                      // it's a kernel
                      BIGKernel kernel = (BIGKernel)nodeObj;
                      kernels.add( kernel.getName() );
                      childNode = new DefaultMutableTreeNode( kernel );
                      model.insertNodeInto(
                         childNode, parentNode, parentNode.getChildCount() );
                   }
                   else {
                      // it's a subdir in the subdir
                      key = nodeObj.toString();
                      // add subdirname and recurse subdirectory
                      childNode = new DefaultMutableTreeNode( key );
                      model.insertNodeInto(
     childNode, parentNode, parentNode.getChildCount() );
                      Map child = (Map)parent.get( key );
     Iterator it3 = (new TreeSet( child.keySet() )).iterator();
                      addKernelsToTree( model, childNode, child, it3 );
                   }
                }
             }
             else {
                // add subdirname and recurse subdirectory
                childNode = new DefaultMutableTreeNode( key );
                model.insertNodeInto(
                      childNode, parentNode, parentNode.getChildCount() );
                Map child = (Map)parent.get( key );
                Iterator it2 = (new TreeSet( child.keySet() )).iterator();
                addKernelsToTree( model, childNode, child, it2 );
             }
          }
       }*/

    /** Initalizes the main view panel according to the current
     * state the GUI is in. This method is called after a new
     * state is set with setGUIState(). */
    private void initCards() {
        if (state == LOCALDEFS) {

            // add combo boxes
            GridBagConstraints gc = new GridBagConstraints();
            gc.fill = GridBagConstraints.BOTH;
            gc.weightx = 1.0;
            cardsButtonPanel.add(localdefFilesComboBox, gc);
            cardsButtonPanel.add(viewLevelComboBox, gc);

            // the CardLayout has to display LOCALDEF information
            String curFile = null;
            String selection = (String) (localdefFilesComboBox.getSelectedItem());
            Object[] allFiles = bigInterface.getAllFilenames().toArray();
            for (int i = 0; i < allFiles.length; i++) {
                if (selection.compareTo(localDefFilesComboBoxEntries[i]) == 0) {
                    curFile = (String) allFiles[i];
                    break;
                }
            }
            if (curFile == null) {
                console.postMessage(
                        "BIGGUI: No match found for LOCALDEF ComboBox selection.",
                        BIGConsole.ERROR);
                return;
            }
            // must be optimized in one pattern
            String viewFile =
                    curFile.replaceAll("<hostname>", bigInterface.getHost());
            viewFile =
                    viewFile.replaceAll("#lt;hostname#gt;",
                                        bigInterface.getHost());
            // <--
            ArrayList entries = bigInterface.getAllEntries(curFile);
            GridBagConstraints c = new GridBagConstraints();
            c.fill = GridBagConstraints.BOTH;
            c.weightx = 1.0;
            c.gridx = 0;
            c.gridy = 0;
            JPanel panel = new JPanel();
            GridBagLayout gbLayout = new GridBagLayout();
            panel.setLayout(gbLayout);

            int pos = 0;
            c.gridx=pos;
            pos++;
            for (int j = 0; j < entries.size(); j++) {
                BIGEntry entry = (BIGEntry) entries.get(j);
                addEntry(entry, pos, gbLayout, c, panel);
                pos++;
            }
            createGlue(pos, gbLayout, c, panel);
            JScrollPane jsp = new JScrollPane(panel);
            jsp.setWheelScrollingEnabled(true);
            jsp.getVerticalScrollBar().setUnitIncrement(15);
            // add scroll pane with graphical representation of the file content
            c.fill = GridBagConstraints.BOTH;
            c.weighty = 1.0;
            viewPanel.add(jsp, c);

            // Inserts Text Editors for the host files
            BIGEditor editor = null;
            for (int i = 0; i < allFiles.length; i++) {
                String tmpFile = (String) allFiles[i];
                //tmpFile=tmpFile.replaceAll("<hostname>",BIGInterface.getInstance().getHost());
                //System.err.println(tmpFile);
                // Check, if an editor for this file already exists.
                // If so, reuse it. If not, create a new one.
                BIGEditor tmpEditor =
                        (BIGEditor) editorFileRelation.get(tmpFile);
                editorFileRelation.remove(tmpEditor);
                if (tmpEditor == null) {
                    tmpEditor = new BIGEditor(
                            bigInterface.getDefFileText(tmpFile));
                    editorFileRelation.put(tmpFile, tmpEditor);
                } else {

                    BIGStrings contains = tmpEditor.getAllLines();
                    tmpEditor = new BIGEditor(
                            contains);
                    editorFileRelation.put(tmpFile, tmpEditor);

                }
                tmpEditor.setTokenMarker(new org.syntax.jedit.tokenmarker.
                                         ShellScriptTokenMarker());
                // extract the Editor for the selected file
                if (tmpFile.compareTo(curFile) == 0)
                    editor = tmpEditor;
            }
            // display the selected Editor's content
            if (editor == null) {
                console.postMessage("BIGGUI: Unable to set Editor "
                                    + "contents.", BIGConsole.ERROR);
                return;
            }
            //JScrollPane editorJSP = new JScrollPane( editor );
            // add scroll pane with text content of the file
            c.fill = GridBagConstraints.BOTH;
            c.weighty = 1.0;
            textPanel.add(editor, c);

        } else if (state == ADMIN) { // the CardLayout has to display admin information
            GridBagConstraints c = new GridBagConstraints();
            c.fill = GridBagConstraints.BOTH;
            c.weightx = 1.0;
            c.weighty = 1.0;
            c.gridx = 0;
            c.gridy = 0;
            if (adminTool == null)return;
            JComponent comp = adminTool.getDisplayPanel();
            //GridBagConstraints gc = new GridBagConstraints();
            viewPanel.add(comp, c);
        } else if (state == KERNELS) { // the CardLayout has to display kernel information
            GridBagConstraints c = new GridBagConstraints();
            c.fill = GridBagConstraints.BOTH;
            c.weightx = 1.0;
            c.weighty = 1.0;
            c.gridx = 0;
            c.gridy = 0;
            if (kernelScriptWindow == null)return;
            //String[] allFiles = kernelScriptWindow.getAllFiles();
            //String selection = kernelScriptWindow.getSelectedTab();
            // Inserts the text panel for the script files
            JComponent[] comps = kernelScriptWindow.getDisplayPanels();
            //JComponent  comp = kernelScriptWindow.getDisplayPanel();
            GridBagConstraints gc = new GridBagConstraints();
            gc.fill = GridBagConstraints.BOTH;
            gc.weightx = 1.0;
            if (comps.length > 1) {
                for (int i = 1; i < comps.length; i++)
                    cardsButtonPanel.add(comps[i], gc);
            }
            //cardsButtonPanel.add(choiceJCB, gc);
            textPanel.add(comps[0], c);
            //textPanel.add( comp, c );
        } else if (state == RESULTS) { // the CardLayout has to display result information
            // first: get the selected result file
            if (resultTree.isSelectionEmpty())return;
            TreePath selectedPath = resultTree.getSelectionPath();
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) selectedPath.
                                          getLastPathComponent();
            if (!(node.getUserObject() instanceof plot.BIGOutputFile)) {
                console.postMessage("No result file selected for "
                                    + "quick view!", BIGConsole.WARNING);
                return;
            }
            GridBagConstraints c = new GridBagConstraints();
            c.fill = GridBagConstraints.BOTH;
            c.anchor = GridBagConstraints.NORTHWEST;
            c.weightx = 1.0;
            c.weighty = 1.0;
            c.gridx = 0;
            c.gridy = 0;
            // check, if a file is selected, but the quickViewWindow is
            // not loaded yet then do it now
            if (quickViewWindow == null) {
                quickViewWindow = new BIGQuickViewWindow(thisBIGGUI, null);
            }
            // second: add the display panel for the selected
            // result file to the viewPanel
            resultViewPanels =
                    quickViewWindow.getDisplayPanels();
            if (resultViewPanels.length != 2) {
                console.postMessage("QuickViewWindow didn't return "
                                    + "2 panels!", BIGConsole.WARNING);
                return;
            }
            viewPanel.add(resultViewPanels[0], c);
            textPanel.add(resultViewPanels[1], c);
            // without the repaint java doesn't paint the panel properly
            // however, with the repaint, you can still see the wrong painting for some millis
            // any suggestions why? you're welcome to mail me: wloch@zhr.tu-dresden.de
            repaint();
        }
        // else
        //{ /* display the selected entry of a JTree */
        //}
    }

    private void removeCards() {
        cardsButtonPanel.removeAll();
        viewPanel.removeAll();
        textPanel.removeAll();
    }

    /*private void view()
        {
       // first: get the selected result file
       if (resultTree.isSelectionEmpty()) {
          console.postMessage("No result file selected for quick view!", BIGConsole.WARNING);
          return;
       }
       TreePath selectedPath = resultTree.getSelectionPath();
       String resultFile = selectedPath.getLastPathComponent().toString();
       if (!results.contains(resultFile)) {
          console.postMessage("No result file selected for quick view!", BIGConsole.WARNING);
          return;
       }
       GridBagConstraints c = new GridBagConstraints();
       c.fill = GridBagConstraints.BOTH;
       c.weightx = 1.0;
       c.gridx = 0;
       c.gridy = 0;
       // second: add the display panel for the selected result file to the viewPanel
       resultViewPanels = quickViewWindow.getDisplayPanels();
       if (resultViewPanels.length != 2) {
          console.postMessage("QuickViewWindow didn't return 2 panels!", BIGConsole.WARNING);
          return;
       }
       removeCards();
       viewPanel.add(resultViewPanels[0], c);
       textPanel.add(resultViewPanels[1], c);
       // without the repaint java doesn't paint the panel properly
       // however, with the repaint, you can still see the wrong painting for some millis
       // any suggestions why? you're welcome to mail me: wloch@zhr.tu-dresden.de
       repaint();
       mainViewPanel.revalidate();
        }*/
    private void execute() {
        if (kernelTree.isSelectionEmpty()) {
            console.postMessage("No kernel selected for execution!",
                                BIGConsole.WARNING);
            return;
        }
        TreePath selectedPath = kernelTree.getSelectionPath();
        //if (debug>0)System.err.println("---------execute(..)--------------");
        //if (debug>0)System.err.println("selectedPath:"+selectedPath.toString());
        String kernelName = selectedPath.getLastPathComponent().toString();
        BIGStrings execThis = new BIGStrings();
        Object nodeObj = ((DefaultMutableTreeNode) selectedPath.
                          getLastPathComponent()).getUserObject();
        if (nodeObj instanceof system.BIGKernel) {
            // a kernel is selected
            execThis.add(((BIGKernel) nodeObj).getAbsolutePath());
        } else {
            // a subdir is selected -> execute all kernel under that subdir
            DefaultTreeModel model = (DefaultTreeModel) kernelTree.getModel();
            DefaultMutableTreeNode rootNode = (DefaultMutableTreeNode) model.
                                              getRoot();
            int end = rootNode.getChildCount();
            for (int i = 0; i < end; i++) {
                DefaultMutableTreeNode childNode = (DefaultMutableTreeNode)
                        rootNode.getChildAt(i);
                if (kernelName.compareTo(childNode.getUserObject().toString()) ==
                    0) {
                    // add all kernels under this subdir to execThis
                    kernelTree.addKernelsToBIGString(execThis, childNode);
                }
            }
        }
        if (execThis.size() > 0) {
            BIGStrings scripts = new BIGStrings();
            switch (choiceList.getSelectedIndex()) {
            // compile only
            case 0:
                for (int i = 0; i < execThis.size(); i++)
                    scripts.add("COMPILE.SH");
                break;
                // run only
            case 1:
                for (int i = 0; i < execThis.size(); i++)
                    scripts.add("RUN.SH");
                break;
                // compile and run
            case 2:
                for (int i = 0; i < execThis.size(); i++)
                    scripts.add("SUBDIREXEC.SH");
                break;
            }
            String msg = "The program is shutting down during the execution "
                         + "of the BenchIT kernels.\n"
                         +
                    "Note that the hostname of the system is used, not that one "
                         + "selected in the GUI.";
            console.postMessage(msg, BIGConsole.WARNING);
            int result = JOptionPane.showConfirmDialog(
                    this, msg, "Warning", JOptionPane.OK_CANCEL_OPTION);
            if (result == JOptionPane.CANCEL_OPTION) {
                return;
            }

            // save localdef files of host for changes to take effect
            if (textViewMode) textViewMode = false;
            final Thread saveThread = saveAll();
            if (textViewMode) textViewMode = true;
            final BIGGUI bgui = this;
            final BIGStrings fscripts = scripts;
            final BIGStrings fexecThis = execThis;

            Thread execThread = new Thread() {
                public void run() {
                    try {
                        saveThread.join();
                    } catch (InterruptedException ie) {}
                    BIGExecute exec = BIGExecute.getInstance();
                    exec.generatePostProcScript(fexecThis, fscripts,
                                                "postproc.sh", kernelTree, true);
                    bigInterface.setConfigFileContents(bgui);
                    if (debug > 0) {
                        for (int i = 10; i > 0; i--) {
                            statusLabel.setText("going down in " + i +
                                                " seconds");
                            try {
                                Thread.sleep(1000);
                            } catch (InterruptedException ie) {}
                        }
                    }
                    saveAndExit();
                }
            };
            execThread.start();
        }
    }

    public void executeSelected() {
        if (kernelTree.isSelectionEmpty()) {
            console.postMessage("No kernel selected for execution!",
                                BIGConsole.WARNING);
            return;
        }
        if (debug > 0) System.err.println(
                "--------------executeSelected(..)-------------");
        int selectionCount = kernelTree.getSelectionCount();
        if (debug > 0) System.err.println("selCount:" + selectionCount);
        TreePath[] selectedPaths = kernelTree.getSelectionPaths();
        BIGStrings execThis = new BIGStrings();
        // traverse selected nodes
        for (int i = 0; i < selectionCount; i++) {
            TreePath selectedPath = selectedPaths[i];
            if (debug > 0) System.err.println("selPath(" + i + "):" +
                                              selectedPath);
            DefaultMutableTreeNode lastNode = (DefaultMutableTreeNode)
                                              selectedPath.getLastPathComponent();
            String kernelName = lastNode.toString();
            if (lastNode.getUserObject() instanceof system.BIGKernel) {
                //leaf
                execThis.add(((BIGKernel) lastNode.getUserObject()).
                             getAbsolutePath());

            } else if (kernelName.compareTo(kernelRootName) == 0) {
                DefaultTreeModel model = (DefaultTreeModel) kernelTree.getModel();
                DefaultMutableTreeNode rootNode = (DefaultMutableTreeNode)
                                                  model.getRoot();
                kernelTree.addKernelsToBIGString(execThis, rootNode);
            } else {
                // a subdir is selected -> execute all kernel under that subdir
                kernelTree.addKernelsToBIGString(execThis, lastNode);
            }
        }
        if (execThis.size() > 0) {
                this.kernelScriptWindow.saveRequest(true);
            BIGStrings scripts = new BIGStrings();
            switch (choiceList.getSelectedIndex()) {
            // compile only
            case 0:
                for (int i = 0; i < execThis.size(); i++)
                    scripts.add("COMPILE.SH");
                break;
                // run only
            case 1:
                for (int i = 0; i < execThis.size(); i++)
                    scripts.add("RUN.SH");
                break;
                // compile and run
            case 2:
                for (int i = 0; i < execThis.size(); i++)
                    scripts.add("SUBDIREXEC.SH");
                break;
            }

            /*  String msg = "The program is shutting down during the execution of the BenchIT kernels.\n"+
                            "Note that the hostname of the system is used, not that one selected in the GUI.";
              console.postMessage(msg, BIGConsole.WARNING);*/
            //not compile
            if (choiceList.getSelectedIndex() != 0) {
                //new
                JComponent message[] = new JComponent[4];
                String[] options = new String[2];
                options[0] = "Okay";
                options[1] = "Cancel";

                message[0] = new JLabel("");
                if (!this.bigInterface.getHost().equals(
                        this.bigInterface.getOriginalHost())) {
                    String msg =
                            "!!! Please note, that not your selected hostname\""
                            + bigInterface.getHost()
                            + "\" is used, but the original hostname\""
                            + this.bigInterface.getOriginalHost() + "\"";
                    console.postMessage(msg, BIGConsole.WARNING);
                    message[0] = new JLabel(msg);
                }
                message[1] = new JLabel("Do you want to shut down the GUI, "
                                        +
                                        "so the measurement isn't influenced?");
                message[2] = new JCheckBox("shut down BIG");
                boolean shutdown=true;
               try
               {
                  shutdown = bigInterface.getInstance().getBIGConfigFileParser().
                      boolCheckOut( "shutDownForExec" ) ;
               }
               catch ( Exception ex )
               {
               }
                boolean askForShutdown=true;
               try
               {
                  askForShutdown = bigInterface.getInstance().
                      getBIGConfigFileParser().boolCheckOut(
                      "askForShutDownForExec" ) ;
               }
               catch ( Exception ex1 )
               {
               }
                ((JCheckBox) message[2]).setSelected(shutdown);
                message[3] = new JCheckBox("ask again");
                ((JCheckBox) message[3]).setSelected(askForShutdown);
                int result=0;
                if (askForShutdown)
                {
                   result = JOptionPane.showOptionDialog(
                       this , message ,
                       "Shut down BIG?" , JOptionPane.DEFAULT_OPTION ,
                       JOptionPane.INFORMATION_MESSAGE , null , options ,
                       options[ 0 ] ) ;
                   //!new
                   // Cancel was pressed
                   if ( result == 1 )return ;
                   shutdown=((JCheckBox) message[2]).isSelected();
                   askForShutdown=((JCheckBox) message[3]).isSelected();
                }
                String doIt="1";
                if (shutdown==false)
                   doIt="0";
                bigInterface.getInstance().
                    getBIGConfigFileParser().set("shutDownForExec",doIt);
              doIt="1";
              if (askForShutdown==false)
                 doIt="0";
              bigInterface.getInstance().
                    getBIGConfigFileParser().set("askForShutDownForExec",doIt);

                // save localdef files of host for changes to take effect
                if (textViewMode) textViewMode = false;
                final Thread saveThread = saveAll();
                if (textViewMode) textViewMode = true;
                final BIGGUI bgui = this;
                final BIGStrings fscripts = scripts;
                final BIGStrings fexecThis = execThis;
                if (((JCheckBox) message[2]).isSelected()) { // if we shut down big
                    Thread execThread = new Thread() {
                        public void run() {
                            try {
                                saveThread.join();
                            } catch (InterruptedException ie) {}
                            BIGExecute exec = BIGExecute.getInstance();
                            exec.generatePostProcScript(fexecThis, fscripts,
                                    "postproc.sh", kernelTree, true);
                            bigInterface.setConfigFileContents(bgui);
                            saveAndExit();
                        }
                    };
                    execThread.start();
                } else { // GUI stays alive during measurement
                    Thread execThread = new Thread() {
                        public void run() {
                            try {
                                saveThread.join();
                            } catch (InterruptedException ie) {}
                            BIGExecute exec = BIGExecute.getInstance();
                            String executeFile =
                                    exec.generatePostProcScript(
                                            fexecThis, fscripts, null,
                                            kernelTree, false);
                            if (executeFile == null) {
                                System.err.println(
                                        "Could not create execution shell-script");
                                return;
                            }
                            bigInterface.setConfigFileContents(bgui);
                            try {
                                Process p = Runtime.getRuntime().exec(
                                        "sh " + executeFile);
                                BIGInterface.getInstance().getConsole().
                                        addStream(p.
                                                  getErrorStream(),
                                                  BIGConsole.ERROR);
                                BIGInterface.getInstance().getConsole().
                                        addStream(p.
                                                  getInputStream(),
                                                  BIGConsole.DEBUG);
                                executedProcess=p;
                                p.waitFor();
                                executedProcess=null;
                            } catch (Exception e) {}
                            kernelTree.reloadKernelsExecutables();
                            resultTree.updateResultTree(null);
                        }
                    };
                    execThread.start();
                }
            } else {
                // for compile only don't shut down
                if (textViewMode) textViewMode = false;
                final Thread saveThread = saveAll();
                if (textViewMode) textViewMode = true;
                final BIGGUI bgui = this;
                final BIGStrings fscripts = scripts;
                final BIGStrings fexecThis = execThis ;
                Thread execThread = new Thread()
                {
                   public void run()
                   {
                      try
                      {
                         saveThread.join() ;
                      }
                      catch ( InterruptedException ie )
                      {}
                      BIGExecute exec = BIGExecute.getInstance() ;
                      String executeFile =
                          exec.generatePostProcScript(
                              fexecThis , fscripts , null , kernelTree , false ) ;
                      bigInterface.setConfigFileContents( bgui ) ;
                      try
                      {
                         Process p = Runtime.getRuntime().exec(
                             "sh " + executeFile ) ;
                         BIGInterface.getInstance().getConsole().addStream( p.
                             getInputStream() , BIGConsole.DEBUG ) ;
                         BIGInterface.getInstance().getConsole().addStream( p.
                             getErrorStream() , BIGConsole.ERROR ) ;
                         p.waitFor() ;

                      }
                      catch ( Exception e )
                      {}
                        kernelTree.reloadKernelsExecutables();
                    }
                };
                execThread.start();

            }
        }
    }

    public void executeSelectedOnOtherSystem() {
        if (kernelTree.isSelectionEmpty()) {
            console.postMessage("No kernel selected for execution!",
                                BIGConsole.WARNING);
            return;
         }


         this.kernelScriptWindow.saveRequest( true ) ;
         saveAll();
        int selectionCount = kernelTree.getSelectionCount();
        TreePath[] selectedPaths = kernelTree.getSelectionPaths();
        final BIGStrings execThis = new BIGStrings();
        for (int i = 0; i < selectionCount; i++) {
            TreePath selectedPath = selectedPaths[i];
            if (debug > 0) System.err.println(selectedPath.getLastPathComponent().
                                              getClass());
            DefaultMutableTreeNode kernelNode = (DefaultMutableTreeNode)
                                                selectedPath.
                                                getLastPathComponent();

            BIGKernel kernel = null;
            String noKernel = null;
            try {
                kernel = kernel = (BIGKernel) kernelNode.getUserObject();
                if (debug > 0) System.err.println("Kernel:" + kernel);
            } catch (ClassCastException ex) {
                noKernel = kernelNode.getUserObject().toString();
            }

            if ((kernel != null)
                /*&&( kernels.contains( kernel.getAbsolutePath() ) )*/) { // leaf
                execThis.add(kernel.getAbsolutePath());
                if (debug > 0) System.err.println("kernel");
            } else if (noKernel.compareTo(kernelRootName) == 0) {
                if (debug > 0) System.err.println("root");
                // clear selection, if any
                kernelTree.clearSelection();
                // select root node
                DefaultTreeModel model = (DefaultTreeModel) kernelTree.getModel();
                DefaultMutableTreeNode rootNode = (DefaultMutableTreeNode)
                                                  model.getRoot();
                TreePath path = new TreePath(model.getPathToRoot(rootNode));
                kernelTree.setSelectionPath(path);
                // selectionCount = 1 makes for loop stop after first run
                selectionCount = 1;
                // do almost the same as if a subdir was selected
                kernelTree.addKernelsToBIGString(execThis, rootNode);
            } else {
                // a subdir is selected -> execute all kernel under that subdir
                if (debug > 0) System.err.println("subdir");
                kernelTree.addKernelsToBIGString(execThis,
                                                 kernelTree.findNode(selectedPath));
            }
        }
        if (execThis.size() > 0) {
            final BIGStrings scripts = new BIGStrings();
            switch (choiceList.getSelectedIndex()) {
            // compile only
            case 0:
                for (int i = 0; i < execThis.size(); i++) scripts.add(
                        "COMPILE.SH");
                break;
                // run only
            case 1:
                for (int i = 0; i < execThis.size(); i++) scripts.add("RUN.SH");
                break;
                // compile and run
            case 2:
                for (int i = 0; i < execThis.size(); i++) scripts.add(
                        "SUBDIREXEC.SH");
                break;

          }
       (new Thread()
       { public void run(){

             remoteMenu.startWorkOnAnotherMachine( execThis , scripts ) ;
          }
       }
       ).start();

        }
    }

    /**
     * Checks all entries wether they should be displayed or not (visible state).
     * Sets an entry visible if the priority level <= current view level, otherwise unvisible.
     */
    private void repaintTabs() {
        Object[] allFiles = bigInterface.getAllFilenames().toArray();

        // this is the upper limit of the for-loop
        // mostly when you change your files, you have fullLoaded
        // (quickLoad disabled or loaded after starting the program)
        int forUpperLimit = allFiles.length;
        // but if theres just <hostname> loaded, we save just this
        /*if (!fullLoaded)
           {
         forUpperLimit=1;
           }*/

        for (int i = 0; i < forUpperLimit; i++) {
            ArrayList entries =
                    bigInterface.getAllEntries(allFiles[i].toString());

            for (int j = 0; j < entries.size(); j++) {
                BIGEntry entry = (BIGEntry) entries.get(j);
                JComponent comp = (JComponent) entry.getComponent();
                JComponent label = (JComponent) entry.getLabel();
                // checks if entry is part of the current selected view level
                if (comp != null && entry.getPriority() <= viewLevel) {
                    comp.setVisible(true);
                    label.setVisible(true);
                } else if (comp != null && entry.getPriority() > viewLevel) {
                    comp.setVisible(false);
                    label.setVisible(false);
                }
                if (debug > 0)
                    System.out.println("BIGGUI: repaintTabs: " + entry.getName() +
                                       " visible (or null?): " +
                                       (comp != null ? "" + comp.isVisible() :
                                        "null"));
            }
        }
    }

    /**
     * Saves the edit values from the components to the values in the BIGEntries
     */
    private void saveEntries() {
        Object[] allFiles = bigInterface.getAllFilenames().toArray();
        // this is the upper limit of the for-loop
        // mostly when you change your files, you have fullLoaded
        // (quickLoad disabled or loaded after starting the program)
        int forUpperLimit = allFiles.length;
        // but if theres just <hostname> loaded, we save just this
        /*if (!fullLoaded)
           {
         forUpperLimit=1;
           }*/
        statusLabel.setText("Saving entries...");
        for (int i = 0; i < forUpperLimit; i++) {
            ArrayList entries = bigInterface.getAllEntries(allFiles[i].toString());

            for (int j = 0; j < entries.size(); j++) {
                BIGEntry entry = (BIGEntry) entries.get(j);
                JComponent comp = (JComponent) entry.getComponent();
                if (comp != null) {
                    int type = entry.getType();
                    switch (type) {
                    case BIGInterface.NONE:
                        break;
                    case BIGInterface.STRING:
                        entry.setValue(((BIGTextField) comp).getText());
                        break;
                     case BIGInterface.INTEGER:
                        entry.setValue(((BIGSpinner) comp).getTextField().
                                       getIntegerValue());
                        break;
                    case BIGInterface.FLOAT:
                        entry.setValue(((BIGSpinner) comp).getTextField().
                                       getValue());
                        break;
                    case BIGInterface.MULTIPLE:
                        entry.setValue(((JComboBox) comp).getSelectedItem().
                                       toString());
                        break;
                    case BIGInterface.LIST:
                        entry.setValue(new BIGStrings(((JList) comp).
                                getSelectedValues()));
                        break;
                    case BIGInterface.BOOLEAN:
                        entry.setValue(new Boolean(((JCheckBox) comp).
                            isSelected() ) ) ;
                        break;
                     case BIGInterface.FREE_TEXT:
                        entry.setValue( ( ( JTextArea ) comp ).getText() ) ;
                        break;
                        /*
                                // not implemented yet, because of less information about this both
                                case BIGInterface.DATETIME: break;
                                case BIGInterface.VECTOR  : break;
                         */
                    default:
                        entry.setValue(null);
                        break;
                    }
                }
            }
        }
        statusLabel.setText("done");
    }

    /*
     * Adds the entry to the panel with specified constraints.
     */
    private void addEntry(
            BIGEntry entry,
            int pos,
            GridBagLayout gbLay,
            GridBagConstraints gbConst,
            JPanel panel) {
        int type = entry.getType();
        String viewname = entry.getViewName().equals("") ? entry.getName() :
                          entry.getViewName();
        Object value = entry.getValue();
        // wheter to use the default value or not
        if (value == null && type != BIGInterface.NONE) {
           // INTEGER null means not set
           if (entry.getType()!=BIGInterface.INTEGER)
           {
              value = entry.getDefaultValue() ;
              viewname += " (!)" ;
              entry.setToolTipText( entry.getToolTipText() + " DEFAULT" ) ;
           }
        }

        gbConst.gridx = 0;
        gbConst.gridy = pos;
        gbConst.gridheight = 1;
        gbConst.gridwidth = 1;
        gbConst.fill = GridBagConstraints.NONE;
        gbConst.anchor = GridBagConstraints.NORTHWEST;
        gbConst.insets = new Insets(1, 3, 1, 3);
        // creates and add a new Label for the entry
        JLabel entryLabel = new JLabel(viewname);
        entryLabel.setFont(labelFont);
        entryLabel.setToolTipText(entry.getToolTipText());
        gbLay.setConstraints(entryLabel, gbConst);

        gbConst.gridx = 1;
        // resize the entry component if it hasn't a BIGSpinner
        if (type != BIGInterface.INTEGER && type != BIGInterface.FLOAT) {
            gbConst.weightx = 90;
            gbConst.fill = GridBagConstraints.HORIZONTAL;
        }
        gbConst.anchor = GridBagConstraints.NORTHWEST;

        // create and add the component for the entry
        JComponent comp = null;
        switch (type) {
        case BIGInterface.STRING:
            comp =
                    new BIGTextField(value, 20, BIGTextField.TEXT);
            break;
        case BIGInterface.INTEGER:
            BIGTextField field =
                new BIGTextField(value, 5, BIGTextField.INTEGER);

            comp = new BIGSpinner(field, 1f);
            Dimension dim = field.getPreferredSize();
            ((BIGSpinner) comp).setPreferredSize(
                    new Dimension(dim.width + 15, dim.height));
            break;
        case BIGInterface.FLOAT:
            field =
                    new BIGTextField(value, 5, BIGTextField.FLOAT);
            comp = new BIGSpinner(field, 1f);
            field.setDecimalCount(3);
            dim = field.getPreferredSize();
            ((BIGSpinner) comp).setPreferredSize(
                    new Dimension(dim.width + 15, dim.height));
            break;
        case BIGInterface.MULTIPLE:
            comp = new JComboBox(entry.getMultipleChoice().toArray());
            ((JComboBox) comp).setSelectedItem(value);
            break;
        case BIGInterface.LIST:
            final BIGStrings values = entry.getMultipleChoice();
            final JList list = new JList(values.toArray());
            final int multiple = entry.getMultipleChoiceCount();

            // viewname must final for out printing
            final String thisname = viewname;
            //System.out.println(multiple);
            list.setSelectionMode(
                    multiple > 1
                    ? ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
                    : ListSelectionModel.SINGLE_SELECTION);
            list.addListSelectionListener(new ListSelectionListener() {
                public void valueChanged(ListSelectionEvent e) {
                    if (list.getSelectedIndices().length > multiple)
                        System.err.println(
                                "You have more than \""
                                + multiple
                                + "\" values selected in \""
                                + thisname
                                + "\".");
                }
            });
            final BIGStrings defaultValue = (BIGStrings) value;
            for (int i = 0; i < defaultValue.size(); i++)
                list.addSelectionInterval(
                        values.find(defaultValue.get(i)),
                        values.find(defaultValue.get(i)));
            list.setVisibleRowCount(4);
            JScrollPane js = new JScrollPane(list);
            comp = js;
            break;
        case BIGInterface.BOOLEAN:
            comp = new JCheckBox();
            ((JCheckBox) comp).setSelected(
                    ((Boolean) value).booleanValue());
            break;
        case BIGInterface.NONE:
            comp = new JLabel();
            break;
        case BIGInterface.FREE_TEXT:
           if (value.toString().endsWith("\n"))
              comp = new JTextArea(value.toString());
           else
              comp = new JTextArea(value.toString()+"\n");
           ((JTextArea)comp).setBorder(BorderFactory.createLineBorder(Color.BLACK));
            break;

            /*
                 // not implemented yet, because of less information about this both
                 case BIGInterface.DATETIME: break;
                 case BIGInterface.VECTOR  : break;
             */
        }

        if (comp != null) {
            comp.setToolTipText(entry.getToolTipText());
            gbLay.setConstraints(comp, gbConst);
            // adding label
            panel.add(entryLabel);
            // adding compontent
            panel.add(comp);
            entry.setComponent(comp);
            entry.setLabel(entryLabel);
            // checks if entry is part of the current selected view level
            if (entry.getPriority() > viewLevel) {
                comp.setVisible(false);
                entryLabel.setVisible(false);
            }
        } else {
            System.err.println(
                    "BIGGUI: No type is matching for this entry: "
                    + viewname
                    + ".");
        }

        if (debug > 0)
            System.out.println(
                    "BIGGUI: Entry \""
                    + entry.getName()
                    + "\" added to pos: "
                    + entry.getPos());
    }

    /**
     * Adds a glue to the panel, so that the elements are always on top.
     */
    private void createGlue(
            int pos,
            GridBagLayout gbLay,
            GridBagConstraints gbConst,
            JPanel panel) {
        gbConst.gridx = 0;
        gbConst.gridy = pos;
        gbConst.gridheight = 1;
        // align over to cells
        gbConst.gridwidth = 2;
        // resize to max
        gbConst.weightx = 100;
        gbConst.weighty = 100;
        // in both directions
        gbConst.fill = GridBagConstraints.BOTH;
        // always add on NORTH
        gbConst.anchor = GridBagConstraints.NORTH;
        gbConst.insets = new Insets(0, 0, 0, 0);
        // create and add empty panel
        JPanel glue = new JPanel();
        gbLay.setConstraints(glue, gbConst);
        panel.add(glue);
    }
    public void reloadOutput()
    {


       this.removeCards();
       // set new components to viewPanel and textPanel
       this.initCards();
       // declare the changed panels to be valid again
       viewPanel.revalidate();
       textPanel.revalidate();
       mainViewPanel.revalidate();
       repaint();
       viewPanel.revalidate();
       textPanel.revalidate();
       mainViewPanel.revalidate();
       repaint();

    }
    /**
     * Sets the value of the component by the given type of entry
     *
     * @param entry the entry
     * @param comp the Component which were set to the value of the entry
     */
    /*	private void updateValue(BIGEntry entry, Component comp) {
            Object value = entry.getValue() == null ? entry.getDefaultValue() : entry.getValue();
      final String name = entry.getName();
      if (comp == null)
       return;

      try {
       switch (entry.getType()) {
        case BIGInterface.STRING :
          ((BIGTextField) comp).setValue(value.toString());
         break;
        case BIGInterface.INTEGER :
        case BIGInterface.FLOAT :
          ((BIGSpinner) comp).setValue(value.toString());
         break;
        case BIGInterface.MULTIPLE :
          ((JComboBox) comp).setSelectedItem(value);
         break;
        case BIGInterface.LIST :
         final BIGStrings values = entry.getMultipleChoice();
         final JList list = new JList(values.toArray());
         final int multiple = entry.getMultipleChoiceCount();
         System.out.println(multiple);
         list.setSelectionMode(
          multiple > 1
           ? ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
           : ListSelectionModel.SINGLE_SELECTION);
         list.addListSelectionListener(new ListSelectionListener() {
          public void valueChanged(ListSelectionEvent e) {
           if (list.getSelectedIndices().length > multiple)
            System.err.println(
             "You have more than \""
              + multiple
              + "\" values selected in \""
              + name
              + "\".");
          }
         });
         final BIGStrings defaultValue = (BIGStrings) value;
         for (int i = 0; i < defaultValue.size(); i++)
          list.addSelectionInterval(
           values.find(defaultValue.get(i)),
           values.find(defaultValue.get(i)));
         list.setVisibleRowCount(4);
         JScrollPane js = new JScrollPane(list);
         comp = js;
         break;
        case BIGInterface.BOOLEAN :
         ((JCheckBox) comp).setSelected(
          ((Boolean) value).booleanValue());
         break;
         /*
          // not implemented yet, because of less information about this both
          case BIGInterface.DATETIME: break;
          case BIGInterface.VECTOR  : break;
      */
     /*			}
        //        comp.repaint();
       } catch (ClassCastException cce) {
        console.postMessage(
         "unable to cast " + name + " with " + entry.getType(),
         BIGConsole.ERROR);
       }

      }*/

     /**
      * Saves all files to db
      * @returns the Thread that carries out the action so you can
      * wait for it
      */
     private Thread saveAll() {
         Thread thread = new Thread() {
             public void run() {
                 // System.err.println("save");
                 BIGDefFileParser defParser = new BIGDefFileParser();
                 if (statusProgress != null)
                     defParser.getObservable().addObserver(statusProgress);
                 // CardLayout shows VIEWPANEL, save all entries in db first
                 if (!textViewMode) saveEntries();

                 Object[] allFiles = bigInterface.getAllFilenames().toArray();
                 // save every rendered file
                 // this is the upper limit of the for-loop
                 // mostly when you change your files, you have fullLoaded
                 // (quickLoad disabled or loaded after starting the program)
                 int forUpperLimit = allFiles.length;
                 // but if there's just <hostname> loaded, we save just this
                 /*if ( !fullLoaded ) {
                    forUpperLimit = 1;
                              }*/
                 for (int i = 0; i < forUpperLimit; i++) {
                     String curFile = allFiles[i].toString();
                     if (statusLabel != null)
                         statusLabel.setText("Saving " +
                                             curFile.replaceAll("<hostname>",
                                 bigInterface.getHost()));
                     BIGEditor edit = (BIGEditor) editorFileRelation.get(
                             curFile);
                     // if clickIt is shown, render the file first
                     if (!textViewMode && (edit != null)) {
                         ArrayList entries = bigInterface.getAllEntries(curFile);

                         BIGStrings tmp = new BIGStrings(entries.size());

                         for (int c = 0; c < entries.size(); c++)
                             tmp.add(((BIGEntry) entries.get(c)).getName());

                         // render the text
                         try {
                             BIGStrings newText = defParser.render(
                                     edit.getAllLines(), curFile, tmp);
                             edit.setText(newText.toString());
                         } catch (Exception e) {}
                     }
                     // save the text
                     if (edit != null) {
                         try {
                             bigInterface.setDefFileText(edit.getAllLines(),
                                     curFile);
                         } catch (Exception e) {}
                     }
                     statusLabel.setText("done");
                 }
             } // end of run()
         };
         thread.start();
         return thread;
     }

    /**
     * Saves the edited parameter in the files specified by the given host.
     *
     * @param host  the new host
     */
    private void saveAs(String host) {
        final String fhost = host;
        Thread thread = new Thread() {
            public void run() {
                if (fhost == null || fhost.equals(""))return;
                try {
                    BIGDefFileParser defParser = new BIGDefFileParser();
                    if (statusProgress != null)
                        defParser.getObservable().addObserver(statusProgress);
                    // is clickIt is shown, save all entries in db first
                    if (!textViewMode) saveEntries();

                    Object[] allFiles = bigInterface.getAllFilenames().toArray();
                    // save every rendered file
                    // this is the upper limit of the for-loop
                    // mostly when you change your files, you have fullLoaded
                    // (quickLoad disabled or loaded after starting the program)
                    int forUpperLimit = allFiles.length;
                    // but if theres just <hostname> loaded, we save just this
                    /*if (!fullLoaded)
                                    {
                       forUpperLimit = 1;
                                    }*/

                    for (int i = 0; i < forUpperLimit; i++) {
                        String curFile = allFiles[i].toString();
                        if (statusLabel != null)
                            statusLabel.setText("Saving " +
                                                curFile.replaceAll("<hostname>",
                                    fhost));
                        BIGEditor edit = (BIGEditor) editorFileRelation.get(
                                curFile);
                        // if clickIt is shown, render the file first
                        if (!textViewMode) {
                            ArrayList entries = bigInterface.getAllEntries(
                                    curFile);

                            BIGStrings tmp = new BIGStrings(entries.size());
                            for (int c = 0; c < entries.size(); c++)
                                tmp.add(((BIGEntry) entries.get(c)).getName());

                            // render the text
                            BIGStrings newText = defParser.render(edit.
                                    getAllLines(), curFile, tmp);
                            edit.setText(newText.toString());
                        }
                    }

                    // sets the new host

                    bigInterface.setHost(fhost);
                    for (int i = 0; i < forUpperLimit; i++) {
                        String curFile = allFiles[i].toString();
                        // save the text
                        BIGEditor edit = (BIGEditor) editorFileRelation.get(
                                curFile);
                        bigInterface.setDefFileText(edit.getAllLines(), curFile);
                    }

                    if (statusLabel != null)
                        statusLabel.setText("Saving files as \"" + fhost
                                            + "\" is finished.");

                    // init the new host and fill the tabs
                    bigInterface.load();
                    removeCards();
                    initCards();
                    if (staticTitle.endsWith(bigInterface.getHost() + ")"))
                        setTitle(staticTitle + ": Localdef settings");
                    else
                        setTitle(staticTitle + ": Localdef settings for host "
                                 + bigInterface.getHost());
                } catch (Exception e) {
                    e.printStackTrace();
                    console.postMessage("Error during saving as \""
                                        + fhost + "\" in the files. " + e,
                                        BIGConsole.ERROR, true);
                }
            }
        };
        thread.start();
    }

    public void setKernelScriptWindow(BIGKernelScriptWindow ksw) {
        final BIGKernelScriptWindow fksw = ksw;
        Thread t = new Thread() {
            public void run() {
                // create the Class that provides the display
                // panel for the COMPILE.SH and RUN.SH files
                // of that kernel's directory
                kernelScriptWindow = fksw;
                // remove all components from viewPanel and textPanel
                removeCards();
                // set new components to viewPanel and textPanel
                initCards();
                // declare the changed panels to be valid again
                BIGGUI.this.viewPanel.revalidate();
                BIGGUI.this.textPanel.revalidate();
                BIGGUI.this.mainViewPanel.revalidate();
            }
        };
        SwingUtilities.invokeLater(t);
    }

    public BIGKernelScriptWindow getKernelScriptWindow() {
        return kernelScriptWindow;
    }


    public void saveAndExit() {
    	boolean save = false;
    	try {
			save = bigInterface.getInstance().getBIGConfigFileParser().boolCheckOut("loadAndSaveSettings");
		} catch (Exception e) {}
    	if ((this.actualFile!=null)&& save)
        {
           final BIGOutputFile tempFile=actualFile;
           (new Thread()
           {
              public void run()
              {
                 tempFile.save(new File(tempFile.getFile().getAbsolutePath()+".gui"));
              }
           }).start();
        }
       if (this.resultMixer!=null)
       for (int i=0;i<this.resultMixer.length;i++)
          this.resultMixer[i].save(new File(bigInterface.getBenchItPath()+File.separator+
                                   "gui"+File.separator+"cfg"+File.separator+"mixer_"+i));
        // first we set this windows changes to the configfileParser
        int i = bigInterface.setConfigFileContents(this);
        // then we clean the memory
        System.gc();
        //and then we exit
        System.exit(0);
    }

    public void setValues(int inx, int iny, int inxSize, int inySize, int rs,
                          int ms,
                          int detailLevel) {
        rightSplit = rs;
        rightSplitPane.setDividerLocation(rightSplit);
        mainSplit = ms;
        mainSplitPane.setDividerLocation(mainSplit);
        setBounds(inx, iny, inxSize, inySize);
        if (detailLevel != viewLevel) {
            viewLevel = detailLevel;
            // redraws the list of displayed items
            repaintTabs();
            // declares the components to be valid again
            viewPanel.revalidate();
            textPanel.revalidate();
            mainViewPanel.revalidate();
            viewLevelComboBox.setSelectedIndex(viewLevel);
        }
    }

    public int getRightSplit() {
        return rightSplitPane.getDividerLocation();
    }

    public int getMainSplit() {
        return mainSplitPane.getDividerLocation();
    }

    public int getDetailLevel() {
        return viewLevel;
    }

    private static BIGQuickViewWindow getQuickViewWindow() {
        return quickViewWindow;
    }

    public TreeSet getKernels() {
        if (debug > 0) System.err.println(
                "------------- getKernels () --------------");
        if (debug > 0) System.err.println(this.kernels);

        //   (new Exception()).printStackTrace();
        return kernels;
    }

    public JLabel getStatusLabel() {
        return statusLabel;
    }

    public TreeSet getResults() {
        return results;
    }
    public BIGResultMixer[] getResultMixer() {
        return this.resultMixer;
    }

    public BIGGUIObserverProgress getStatusProgress() {
        return statusProgress;
    }

    public BIGResultTree getResultTree() {
        return this.resultTree;
    }

    public BIGKernelTree getKernelTree() {
        return this.kernelTree;
    }
    public JPanel getConsoleScrollPane()
    {
       return this.consoleScrollPane;
    }
    public JSplitPane getRightSplitPane()
    {
       return this.rightSplitPane;
    }

    /**
     * returns this.bigInterface
     */
    public BIGInterface getBIGInterface() {
        return this.bigInterface;
    }

    /**
     * Displays the text view panel.
     **/
    public void setTextView() {
        if (!textViewMode) {
            CardLayout cl = (CardLayout) (mainViewPanel.getLayout());
            cl.show(mainViewPanel, TEXTPANEL);
            textViewMode = true;
        }
    }

    /**
     * Displays the graph view panel.
     **/
    public void setGraphView() {
        if (textViewMode) {
            CardLayout cl = (CardLayout) (mainViewPanel.getLayout());
            cl.show(mainViewPanel, this.VIEWPANEL);
            textViewMode = false;
        }
    }


    /**
     * Sets the result view to the result mixer.
     **/
    public void setResultMixer(int whichResultMixer) {
       this.setResultMixer(this.resultMixer[whichResultMixer]);
    }
    /**
     * Sets the result view to the result mixer.
     **/
    public void setResultMixer(BIGResultMixer brm) {
        viewPanel.removeAll();
        viewPanel.revalidate();
        viewPanel.setLayout(new GridLayout(1, 1));
        viewPanel.add(brm.getPlot());
        viewPanel.revalidate();
        viewPanel.repaint();
        // switch to the graphic view
        if ( textViewMode )
        {
        	toggleViewItemAction.actionPerformed( null );
        }
        // for ResultMixers it shouldn't be possible to switch to the text view
        toggleViewItemAction.setEnabled( false );
    }

    private void setSessionDialog()
    {
       final BIGWizard wiz = new BIGWizard( "Select the actual Session" ) ;
       final JPanel pan = new JPanel() ;
       pan.setLayout(new FlowLayout(FlowLayout.CENTER));
       final JRadioButton local = new JRadioButton(  ) ;
       final JRadioButton remote = new JRadioButton(  ) ;
       final JList remoteList = new JList( remoteMenu.getRemoteDefs() ) ;
       remoteList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION );

       if ( remoteMenu.getRemoteDefs().length > 0 )
          remoteList.setSelectedIndex( 0 ) ;
       local.setAction( new AbstractAction()
       {
          public void actionPerformed( ActionEvent evt )
          {
             local.setSelected(true);
             remote.setSelected( false ) ;
             remoteList.setEnabled( false ) ;
          }
       } ) ;
       local.setText("local");
       remote.setAction( new AbstractAction()
       {
          public void actionPerformed( ActionEvent evt )
          {
             remote.setSelected(true);
             local.setSelected( false ) ;
             remoteList.setEnabled( true ) ;
          }
       } ) ;
       remote.setText("remote");
       pan.add( local ) ;
       if ( remoteMenu.getRemoteDefs().length > 0 )
       {
          pan.add( remote ) ;
          pan.add( remoteList ) ;
       }
       if (getSession()==null)
       {
          local.setSelected( true ) ;
          remoteList.setEnabled(false);
       }
       else
       {
          remote.setSelected(true);
          remoteList.setSelectedValue(getSession(),true);
       }

       wiz.setFinishAction(new AbstractAction()
           {
              public void actionPerformed(ActionEvent evt)
              {
                 if (local.isSelected())
                 {
                    setSession( null ) ;
                 }
                 else
                 {
                    setSession( ( system.RemoteDefinition ) remoteList.
                                 getSelectedValue() ) ;
                 }
                 wiz.setVisible(false);
              }
           });
       wiz.addPanel( pan , "Select the Session" ) ;
       wiz.start();
    }
    public void setSession(final system.RemoteDefinition def)
    {
       (new Thread()
       {
          public void run()
          {

             session=def;
             // localhost
             if (def==null)
             {
                staticTitle=( "Session at local host (hostname=" +
                               BIGInterface.getInstance().getOriginalHost() +
                               "), LOCALDEFS opened for \"" +
                               BIGInterface.getInstance().getHost() + "\"" ) ;
                setGUIState(LOCALDEFS);
             }
             else
             {
                if ((def.getHostname()==null)||(def.getHostname().equals("null")))
                {
                   def.setHostname(remoteMenu.getHostname(def));
                   try
                   {
                      remoteMenu.saveRemoteDefs() ;
                   } catch (Exception e)
                   {
                      e.printStackTrace();
                   }
                }
                BIGInterface.getInstance().setHost( def.getHostname() ) ;
                staticTitle=( "Session at remote host " + def.getIP() +
                               " (hostname=" + def.getHostname() + ")" ) ;
                setGUIState(LOCALDEFS);
             }

          }
       }).start();
    }
    public system.RemoteDefinition getSession()
    {
       return this.session;
    }
    public void print()
    {
       switch (this.state)
       {
          case LOCALDEFS:
             System.out.println("Not yet implemented for LOCALDEFS");
             break;
          case KERNELS:
             this.kernelScriptWindow.getActualEditor().print() ;
             break ;
          case RESULTS:
             if (textViewMode)
             {
                for (int i=0;i<textPanel.getComponentCount();i++)
                {
                   if (textPanel.getComponent( i ) instanceof BIGEditor)
                   {
                      ((BIGEditor)textPanel.getComponent( i )).print();
                   }
                }
             }else
             {

                PrinterJob printJob = PrinterJob.getPrinterJob() ;
                printJob.setPrintable( this.plotWindow.getChartPanel() ) ;
                if ( printJob.printDialog() )
                {
                   try
                   {
                      printJob.print() ;
                   }
                   catch ( PrinterException ex2 )
                   {
                      ex2.printStackTrace() ;
                   }
                }

             }
             break;

       }
    }



    /**
     * This class builds a frame which analyses and plots the output
     * files.
     */
    private class BIGQuickViewWindow {
        // needed to determine if JFreeChart shall be used
        private boolean plotExists = false;
        // the textual and graphical display panels
        private JComponent[] displayPanels;

        // private JList resultList;
        // private JProgressBar progressBar;
        // private Thread runner;
        /** The referrence to the Observer class for the progress bar. */
        private BIGObservable progress;
        BIGGUI referenceToBIGGUI;
        plot.BIGOutputFile file;
        public BIGQuickViewWindow(BIGGUI referenceToBIGGUI,
                                  plot.BIGOutputFile file) {
            progress = new BIGObservable();
            this.referenceToBIGGUI = referenceToBIGGUI;
            // this remembers whether the jarfiles exist or not
            try {
                // try to instantiate a class from each jar file
                // jcommon-0.8.0.jar:
                //new RefineryUtilities();
                // jfreechart-0.9.8.jar:
                //new org.jfree.chart.ChartUtilities();
                // if success, then the jar files exist and we can
                // plot the results with java
                plotExists = true;
            }
            // if not success, then the jar files do not exist and
            // we use QUICKVIEW.SH
            catch (NoClassDefFoundError ncdfe) {
                if (debug > 0)
                    System.out.println("BIGGUI: " + ncdfe.getMessage());
                System.out.println("BIGGUI: java plot not possible - "
                                   + "using QUICKVIEW.SH");
                plotExists = false;
            }
            this.file = file;
            // initialize the display panels
            initComponents();
        }

        /**
         * init all comps.
         */
        private void initComponents() {
           //System.err.println("init components "+this.file);
            // get the selected result file from the tree
            if (resultTree.isSelectionEmpty()) {
                console.postMessage("No result file selected for "
                                    + "quick view!", BIGConsole.WARNING);
                return;
            }
            plot.BIGOutputFile resultFileTemp = null;
            if (this.file != null) {
                resultFileTemp = file;
            } else {
                TreePath path = resultTree.getSelectionPath();
                DefaultMutableTreeNode node =
                        (DefaultMutableTreeNode) path.getLastPathComponent();
                resultFileTemp = (plot.BIGOutputFile) node.
                                 getUserObject();
            }
            final plot.BIGOutputFile resultFile = resultFileTemp;
            resultFile.setProgress(BIGGUI.this.statusProgress,BIGGUI.this.statusLabel);
            // no result file is selected post message
           plotWindow=new BIGPlot(resultFile);
            displayPanels=(plotWindow).getDisplayPanels();
            if (displayPanels == null) {
                console.postMessage("Please select a "
                                    + "result file.", BIGConsole.WARNING);
                // inititalize the display panel field size
                displayPanels = new JComponent[2];
                displayPanels[0] = new JPanel();
                displayPanels[1] = new JPanel();
            }
        }

        public JComponent[] getDisplayPanels() {
            return displayPanels;
        }

        /** This Method is needed for adding this class Observable
         * to an Observer of a progress bar. */
        public Observable getObservable() {
            return progress;
        }

        /** The internel class for the progress updates. */
        private class BIGObservable extends Observable {
            private int oldValue = -1;
            public void setProgress(int value) {
                if (oldValue != value) {
                    oldValue = value;
                    setChanged();
                    notifyObservers(new Integer(value));
                    try {
                        Thread.sleep(0, 1);
                    } catch (InterruptedException ie) {}
                }
            }
        }
    } // end of BIGQuickViewWindow


    class AdminItemAction extends AbstractAction {
        public AdminItemAction(String text, String desc,
                               Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public AdminItemAction(String desc,
                               KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public AdminItemAction(String text, String desc,
                               ImageIcon icon, KeyStroke accelerator,
                               boolean enabled) {
            super(text, icon);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, icon);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            setGUIState(ADMIN);
        }
    }


    class DatabaseItemAction extends AbstractAction {
        public DatabaseItemAction(String text, String desc,
                                  Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public DatabaseItemAction(String desc,
                                  KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public DatabaseItemAction(String text, String desc,
                                  ImageIcon icon, KeyStroke accelerator,
                                  boolean enabled) {
            super(text, icon);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, icon);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            setGUIState(DATABASE);
        }
    }


    class LocaldefsItemAction extends AbstractAction {
        public LocaldefsItemAction(String text, String desc,
                                   Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public LocaldefsItemAction(String desc,
                                   KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public LocaldefsItemAction(String text, String desc,
                                   ImageIcon icon, KeyStroke accelerator,
                                   boolean enabled) {
            super(text, icon);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, icon);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            setGUIState(LOCALDEFS);
        }
    }


    class KernelsItemAction extends AbstractAction {
        public KernelsItemAction(String text, String desc,
                                 Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public KernelsItemAction(String desc,
                                 KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public KernelsItemAction(String text, String desc,
                                 ImageIcon icon, KeyStroke accelerator,
                                 boolean enabled) {
            super(text, icon);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, icon);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            setGUIState(KERNELS);
        }
    }


    class ResultsItemAction extends AbstractAction {
        public ResultsItemAction(String text, String desc,
                                 Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public ResultsItemAction(String desc,
                                 KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public ResultsItemAction(String text, String desc,
                                 ImageIcon icon, KeyStroke accelerator,
                                 boolean enabled) {
            super(text, icon);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, icon);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            setGUIState(RESULTS);
        }
    }


    /*   class GraficalViewItemAction extends AbstractAction {
          public GraficalViewItemAction( String text, String desc,
             Integer mnemonic ) {
             super( text );
             putValue( SHORT_DESCRIPTION, desc );
             putValue( MNEMONIC_KEY, mnemonic );
          }
          public GraficalViewItemAction( String desc,
             KeyStroke accelerator, boolean enabled ) {
             super();
             putValue( SHORT_DESCRIPTION, desc );
             putValue( ACCELERATOR_KEY, accelerator );
             setEnabled( enabled );
          }
          public GraficalViewItemAction( String text, String desc,
             ImageIcon icon, KeyStroke accelerator, boolean enabled ) {
             super( text, icon );
             putValue( SHORT_DESCRIPTION, desc );
             putValue( ACCELERATOR_KEY, accelerator );
             putValue( SMALL_ICON, icon );
             setEnabled( enabled );
          }
          public void actionPerformed( ActionEvent e ) {
             CardLayout cl = (CardLayout)(mainViewPanel.getLayout());
             textViewMode = false;
            if ( BIGGUI.this.getGUIState() == BIGGUI.LOCALDEFS ) {
                 localdefFilesComboBox.setEnabled(true);
                 viewLevelComboBox.setEnabled(true);
                 cardsButtonPanel.revalidate();
             }
             cl.show( mainViewPanel, VIEWPANEL );
          }
       }
       class TextualViewItemAction extends AbstractAction {
          public TextualViewItemAction( String text, String desc,
             Integer mnemonic ) {
             super( text );
             putValue( SHORT_DESCRIPTION, desc );
             putValue( MNEMONIC_KEY, mnemonic );
          }
          public TextualViewItemAction( String desc,
             KeyStroke accelerator, boolean enabled ) {
             super();
             putValue( SHORT_DESCRIPTION, desc );
             putValue( ACCELERATOR_KEY, accelerator );
             setEnabled( enabled );
          }
          public TextualViewItemAction( String text, String desc,
             ImageIcon icon, KeyStroke accelerator, boolean enabled ) {
             super( text, icon );
             putValue( SHORT_DESCRIPTION, desc );
             putValue( ACCELERATOR_KEY, accelerator );
             putValue( SMALL_ICON, icon );
             setEnabled( enabled );
          }
          public void actionPerformed( ActionEvent e ) {
             CardLayout cl = (CardLayout)(mainViewPanel.getLayout());
             textViewMode = true;
             if ( BIGGUI.this.getGUIState() == BIGGUI.LOCALDEFS ) {
                 localdefFilesComboBox.setEnabled(true);
                 viewLevelComboBox.setEnabled(false);
                 cardsButtonPanel.revalidate();
             }
             cl.show( mainViewPanel, TEXTPANEL );
          }
       }*/
    class ToggleViewItemAction extends AbstractAction {
        public ToggleViewItemAction(String text, String desc,
                                    Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public ToggleViewItemAction(String desc,
                                    KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public ToggleViewItemAction(String text, String desc,
                                    ImageIcon iconA,
                                    KeyStroke accelerator, boolean enabled) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, iconA);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            CardLayout cl = (CardLayout) (mainViewPanel.getLayout());
            textViewMode = !textViewMode;
            if (BIGGUI.this.getGUIState() == BIGGUI.LOCALDEFS) {
                localdefFilesComboBox.setEnabled(true);
                viewLevelComboBox.setEnabled(!textViewMode);
                cardsButtonPanel.revalidate();
            }
            if (textViewMode)
                cl.show(mainViewPanel, TEXTPANEL);
            else
                cl.show(mainViewPanel, VIEWPANEL);
        }
    }


    class ExecuteSelectedItemAction extends AbstractAction {
        public ExecuteSelectedItemAction(String text, String desc,
                                         Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public ExecuteSelectedItemAction(String desc,
                                         KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public ExecuteSelectedItemAction(String text, String desc,
                                         ImageIcon icon, KeyStroke accelerator,
                                         boolean enabled) {
            super(text, icon);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, icon);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            executeSelected();
        }
    }


    class ShowResultItemAction extends AbstractAction {
        public ShowResultItemAction(String text, String desc,
                                    Integer mnemonic) {
            super(text);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(MNEMONIC_KEY, mnemonic);
        }

        public ShowResultItemAction(String desc,
                                    KeyStroke accelerator, boolean enabled) {
            super();
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            setEnabled(enabled);
        }

        public ShowResultItemAction(String text, String desc,
                                    ImageIcon icon, KeyStroke accelerator,
                                    boolean enabled) {
            super(text, icon);
            putValue(SHORT_DESCRIPTION, desc);
            putValue(ACCELERATOR_KEY, accelerator);
            putValue(SMALL_ICON, icon);
            setEnabled(enabled);
        }

        public void actionPerformed(ActionEvent e) {
            final Thread thread = new Thread() {
                public void run() {
                    quickViewWindow = new BIGQuickViewWindow(thisBIGGUI, null);
                }
            };
            thread.start();
            Thread thread2 = new Thread() {
                public void run() {
                    try {
                        thread.join();
                    } catch (InterruptedException ie) {}
                    if (statusLabel != null)
                        statusLabel.setText("Repainting plot panel...");
                    // remove all components from viewPanel and textPanel
                    removeCards();
                    // set new components to viewPanel and textPanel
                    initCards();
                    // declare the changed panels to be valid again
                    viewPanel.revalidate();
                    textPanel.revalidate();
                    mainViewPanel.revalidate();
                    repaint();
                    viewPanel.revalidate();
                    textPanel.revalidate();
                    mainViewPanel.revalidate();
                    repaint();
                    if (statusLabel != null)
                        statusLabel.setText("done");
                }
            };
            thread2.start();
        }
    }
}
/******************************************************************************
 *  Log-History
 *
 *  $Log: BIGGUI.java,v $
 *  Revision 1.43  2007/07/10 10:41:30  tschuet
 *  actualize the gui-icon
 *
 *  Revision 1.42  2007/07/03 11:28:26  tschuet
 *  insert a new benchit icon into the frames
 *
 *  Revision 1.41  2007/04/17 10:17:52  tschuet
 *  there was a spelling mistake: string 'preferrences' in the setup menu has been corrected to 'preferences'
 *
 *  Revision 1.40  2007/03/27 18:45:03  tschuet
 *  deactivate the "toggle dispay" button while using a result mixer (it is no longer possible the switch to text view in a result mixer)
 *
 *  Revision 1.39  2007/02/27 12:37:49  tschuet
 *  different bugfixes (mixer, remote folder -> see meeting at february, 27th)
 *
 *  Revision 1.38  2007/01/15 06:47:20  rschoene
 *  empty Integer variables for LOCALDEFS are possible, compile/run selection changed
 *
 *  Revision 1.37  2006/12/18 08:09:34  rschoene
 *  new FontDialog
 *
 *  Revision 1.36  2006/12/14 08:02:32  rschoene
 *  added BIGInterface.FREE_TEXT
 *
 *  Revision 1.35  2006/12/06 06:54:19  rschoene
 *  added output for init-progressbar "Looking for update"
 *
 *  Revision 1.34  2006/11/06 13:18:44  rschoene
 *  added Screenshot
 *
 *  Revision 1.33  2006/10/04 11:46:03  rschoene
 *  changed setOnline
 *
 *  Revision 1.32  2006/08/16 13:21:38  rschoene
 *  global Insets
 *
 *  Revision 1.31  2006/07/26 13:02:21  rschoene
 *  saves LOCALDEFS when running a kernel remote
 *
 *  Revision 1.30  2006/07/05 09:28:22  rschoene
 *  debugged reset all plot-information
 *
 *  Revision 1.29  2006/07/03 14:19:48  rschoene
 *  new update-function
 *
 *  Revision 1.28  2006/06/28 10:37:53  rschoene
 *  some changes for online-mode
 *
 *  Revision 1.27  2006/06/26 10:47:34  rschoene
 *  integrated online- in result-mode
 *
 *  Revision 1.26  2006/06/25 12:38:54  rschoene
 *  added update support
 *
 *  Revision 1.25  2006/06/23 10:06:43  rschoene
 *  set resulttree for mixers
 *
 *  Revision 1.24  2006/06/02 16:10:02  rschoene
 *  I hope it runs better now
 *
 *  Revision 1.23  2006/05/27 10:00:27  rschoene
 *  changed behaviour of Remote-measurement, removed bugs and debug printing
 *
 *  Revision 1.22  2006/05/26 12:06:18  rschoene
 *  removed a bug (font)
 *
 *  Revision 1.21  2006/05/12 09:59:25  rschoene
 *  set title and legend fonts
 *
 *  Revision 1.20  2006/05/12 08:47:19  rschoene
 *  comment in plot
 *
 *  Revision 1.19  2006/05/11 18:26:58  rschoene
 *  font settings
 *
 *  Revision 1.18  2006/05/11 10:30:45  rschoene
 *  changes for reset plot-info, save height/width for save/change default colors
 *
 *  Revision 1.17  2006/04/04 10:10:55  rschoene
 *  remoteexec now as thred
 *
 *  Revision 1.16  2006/03/09 14:04:24  rschoene
 *  added printing
 *
 *  Revision 1.15  2006/03/07 12:49:20  rschoene
 *  handle ask for shutdown in BGUI.cfg
 *
 *  Revision 1.14  2006/02/07 16:36:55  rschoene
 *  all new plotting
 *
 *  Revision 1.13  2006/01/31 21:12:20  rschoene
 *  prepared reset all plots, removed an ineccessary call
 *
 *  Revision 1.12  2006/01/10 10:52:37  rschoene
 *  moved a debug-message
 *
 *  Revision 1.11  2006/01/10 10:43:30  rschoene
 *  plotting colors where öhm ... you now, but fixed, also removed debugMessage
 *
 *  Revision 1.10  2006/01/09 16:33:52  rschoene
 *  most functionality of BIGQuickViewWindow moved to BIGPlot
 *
 *  Revision 1.9  2005/12/15 11:11:32  rschoene
 *  now saves changes before run
 *
 *  Revision 1.8  2005/12/14 16:00:50  rschoene
 *  added BIGEnvironmentEditor
 *
 *  Revision 1.7  2005/12/07 10:25:24  rschoene
 *  fun with parameter -host (hostname can be null and thats really s**t)
 *
 *  Revision 1.6  2005/11/23 14:59:56  rschoene
 *  F6 didnt work
 *
 *  Revision 1.5  2005/11/17 09:38:43  rschoene
 *  rsome session-experiences
 *
 *  Revision 1.4  2005/11/11 13:44:20  rschoene
 *  now handles (compile/run) in a list
 *
 *  Revision 1.3  2005/11/02 14:44:27  rschoene
 *  removed debug output
 *
 *  Revision 1.2  2005/11/02 14:15:14  rschoene
 *  removed bugs and added multiple mixer support
 *
 *  Revision 1.1  2005/10/20 13:13:56  rschoene
 *  src3 add
 *
 *  Revision 1.38  2005/06/16 10:44:01  rschoene
 *  execute only shown on unix-systems
 *
 *  Revision 1.37  2005/06/16 09:24:51  rschoene
 *  changed creation of conn.MyJFrame
 *
 *  Revision 1.35  2005/05/26 09:04:08  rschoene
 *  dividerLocation bug again >:(
 *
 *  Revision 1.34  2005/05/26 08:42:03  rschoene
 *  save commit for editors
 *
 *  Revision 1.31  2005/05/11 10:01:54  rschoene
 *  deleted old structures (less memory using...)
 *
 *  Revision 1.30  2005/05/11 09:32:50  rschoene
 *  removed jumping-right-divider-bug 2 (ADMIN-Mode)
 *
 *  Revision 1.29  2005/05/11 09:27:45  rschoene
 *  right divider doesnt jump when clicking first on results
 *
 *  Revision 1.28  2005/04/26 12:13:28  rschoene
 *  removed bug that didnt load the editors, when loading a new host
 *
 *  Revision 1.27  2005/04/22 12:41:09  rschoene
 *  some changes, like less mem-using for changed files and removing of not needed stuff
 *
 *  Revision 1.26  2005/04/14 16:11:57  wloch
 *  implemented sorting of the kernel tree
 *
 *  Revision 1.25  2005/04/04 10:34:28  rschoene
 *  removed GC-Problem with BIGPlots on more then 2 yaxis
 *
 *  Revision 1.24  2005/03/22 14:16:23  rschoene
 *  removed debug message
 *
 *  Revision 1.23  2005/03/15 10:41:42  wloch
 *  added detail level restroration after restart
 *
 *  Revision 1.22  2005/02/23 13:14:22  wloch
 *  assigned new accelerator keys to the menu
 *
 *  Revision 1.21  2005/02/22 17:01:53  wloch
 *  added subnode to BRM and instant loading
 *
 *  Revision 1.20  2005/02/22 11:41:00  wloch
 *  corrected remote execute menu entry in menu and popup
 *
 *  Revision 1.19  2005/02/21 19:13:16  wloch
 *  major changes to menu structure and popup menus
 *
 *  Revision 1.18  2005/02/18 09:45:19  wloch
 *  implemented a better popup menu API
 *
 *
 ******************************************************************************/
