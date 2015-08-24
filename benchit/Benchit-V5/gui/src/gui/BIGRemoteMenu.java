package gui ;

import system.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import java.io.* ;
import java.util.* ;
import ch.ethz.ssh2.* ;
//import com.ice.tar.* ;
import system.RemoteDefinition ;
import org.apache.tools.tar.*;

/**
 * <p>Ueberschrift: BenchIT BIGRemoteMenu</p>
 * <p>Beschreibung: Menu for Remote Folder support (execute on other system via ssh)</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR TU Dresden</p>
 * @author Robert Schoene
 * @version 1.0
 */

public class BIGRemoteMenu
    extends JMenu
{
   static final String knownHostPath = System.getProperty( "user.home" ) +
       File.separator + ".ssh" +
       File.separator + "known_hosts" ;
   static final String idDSAPath = System.getProperty( "user.home" ) +
       File.separator + ".ssh" + File.separator +
       "id_dsa" ;
   static final String idRSAPath = System.getProperty( "user.home" ) +
       File.separator + ".ssh" + File.separator +
       "id_rsa" ;

   KnownHosts database = new KnownHosts() ;
   JFrame loginFrame = null ;

   private static boolean debug = false ;
   public RemoteDefinition[] remoteDefs = null ;
   private BIGGUI bgui ;

   // MenuItems
   JMenuItem execRemote ;
   JMenuItem removeOutput ;
   JMenuItem removeFolder ;
   JMenuItem getOutput ;
   JMenuItem checkForOutput ;
   JMenuItem remoteFolder ;
   JMenuItem copyExec ;
   JMenuItem justExec ;

   /**
    * Creates a menu for a menu bar containing all JMenuItems for operations
    * on remote machines. This constructor does the same as
    * BIGRemoteMenu( gui, true );
    * @param gui The BIGGUI.
    **/
   public BIGRemoteMenu( BIGGUI gui )
   {
      this( gui , true ) ;
   }

   /**
    * Creates all JMenuItems for operations on remote machines. Puts all those
    * menu items together to one menu, if oldStructure is true.<br>
    * If oldStructure is false, the JMenuItems will be created, but not added
    * to this BIGRemoteMenu. Instead, the menu(s) must be generated using the
    * returned Lists from getRemoteMenu().
    * @param gui The BIGGUI.
    * @param oldStructure true, if all menu items should be added to this menu.
    **/
   public BIGRemoteMenu( BIGGUI gui , boolean oldStructure )
   {
      createMenus( gui ) ;
      if ( oldStructure )
      {
         this.add( remoteFolder ) ;
         this.add( checkForOutput ) ;
         this.add( getOutput ) ;
         this.add( execRemote ) ;
         this.add( removeFolder ) ;
      }
      try
      {
         this.database.addHostkeys( new File( this.knownHostPath ) ) ;
      }
      catch ( IOException ex )
      {
         System.err.println( "Couldn't find " + this.knownHostPath ) ;
      }
   }

   /**
    * Creates the JMenuItems for this BIGRemoteMenu.
    * @param gui The BIGGUI.
    **/
   private void createMenus( BIGGUI gui )
   {
      this.bgui = gui ;
      if ( BIGInterface.getInstance().getDebug( "BIGRemoteMenu" ) > 0 ) debug = true ;
      // if(BIGInterface.getSystem()==BIGInterface.WINDOWS_SYSTEM) return;
      try
      {
         String name =
             BIGInterface.getInstance().getBenchItPath() +
             File.separator +
             "gui" +
             File.separator + "cfg" + File.separator +
             BIGInterface.getInstance().
             getBIGConfigFileParser().stringCheckOut( "remoteXMLFile" ) ;
         File xmlFile = new File( name ) ;

         this.remoteDefs = RemoteDefinition.getRemoteDefsFromXML( xmlFile ) ;
      }
      catch ( Exception e )
      {
         e.printStackTrace() ;
         System.out.println( "Couldn't load \"remoteXMLFile\" from BGUI.cfg" ) ;
         this.remoteDefs = new RemoteDefinition[0 ] ;
      }
      /*        try {
                  this.sshCommand =
                          BIGInterface.getInstance().
       getBIGConfigFileParser().stringCheckOut("sshCommand");
              } catch (Exception e) {
                  System.out.println(
       "Couldn't load \"sshCommand\" from BGUI.cfg, using \"ssh\"");
              }
              try {
                  this.scpCommand =
                          BIGInterface.getInstance().
       getBIGConfigFileParser().stringCheckOut("scpCommand");
              } catch (Exception e) {
                  System.out.println(
       "Couldn't load \"scpCommand\" from BGUI.cfg, using \"scp\" instead");
              }
              String additional = "";
              try {
                  additional =
                          BIGInterface.getInstance().
                          getBIGConfigFileParser().stringCheckOut(
                          "additionalSCPFlags");
                  if (!additional.equals("")) {
                      additional = " " + additional;
                  }
              } catch (Exception e) {
                  System.out.println(
       "Couldn't load \"additionalSCPFlags\" from BGUI.cfg, using none");
              }
              this.scpCommand = this.scpCommand + additional;
              try {
                  this.tarCommand =
                          BIGInterface.getInstance().
       getBIGConfigFileParser().stringCheckOut("tarCommand");
              } catch (Exception e) {
                  System.out.println(
       "Couldn't load \"tarCommand\" from BGUI.cfg, using \"tar\" instead");
              }*/
      this.setText( "Remote Mode" ) ;

      remoteFolder = new JMenuItem( "Create a remote folder" ) ;
      remoteFolder.setToolTipText( "Create a remote folder at a computer" ) ;
      remoteFolder.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {
                  createRemoteFolderDialog() ;
               }
            }
            ).start() ;
         }
      } ) ;

      checkForOutput = new JMenuItem( "Check for output" ) ;
      checkForOutput.setToolTipText( "Check a remote folder for output data" ) ;
      checkForOutput.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {

                  if ( remoteDefs.length == 0 )
                     showEmptyDialog() ;
                  else
                     checkForOutput() ;
               }
            }
            ).start() ;

         }
      } ) ;

      getOutput = new JMenuItem( "Get output data" ) ;
      getOutput.setToolTipText( "Get the output files from a remote folder" ) ;
      getOutput.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {

                  if ( remoteDefs.length == 0 )
                     showEmptyDialog() ;
                  else
                     getOutput() ;
               }
            }
            ).start() ;

         }
      } ) ;

      removeFolder = new JMenuItem( "Remove remote folder" ) ;
      removeFolder.setToolTipText( "Removes a remote folder" ) ;
      removeFolder.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {
                  if ( remoteDefs.length == 0 )
                     showEmptyDialog() ;
                  else
                     removeRemoteFolder() ;
               }
            }
            ).start() ;
         }
      } ) ;

      removeOutput = new JMenuItem( "Remove remote output" ) ;
      removeOutput.setToolTipText( "Removes output data in remote folder" ) ;
      removeOutput.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {
                  if ( remoteDefs.length == 0 )
                     showEmptyDialog() ;
                  else
                     removeRemoteOutput() ;
               }
            }
            ).start() ;
         }
      } ) ;
      execRemote = new JMenuItem( "Execute in remote folder" ) ;
      execRemote.setToolTipText(
          "Executes selected kernels in remote folder" ) ;
      execRemote.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {
                  if ( remoteDefs.length == 0 )
                     showEmptyDialog() ;
                  else
                     bgui.executeSelectedOnOtherSystem() ;
               }
            }
            ).start() ;

         }
      } ) ;
      copyExec = new JMenuItem( "Copy executables to remote folder" ) ;
      copyExec.setToolTipText(
          "Copy selected available executables to remote folder" ) ;
      copyExec.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {
                  if ( remoteDefs.length == 0 )
                     showEmptyDialog() ;
                  else
                  {
                     RemoteDefinition[] remDefs = getDefsDialog(
                         "to copy the executables to" ) ;
                     for ( int i = 0 ; i < remDefs.length ; i++ )
                     {
                        copyExecutables( remDefs[ i ] ) ;
                     }
                  }
               }
            }
            ).start() ;

         }
      } ) ;
      justExec = new JMenuItem( "Run remote executables" ) ;
      justExec.setToolTipText(
          "Run selected available executables in remote folder" ) ;
      justExec.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            ( new Thread()
            {
               public void run()
               {
                  if ( remoteDefs.length == 0 )
                     showEmptyDialog() ;
                  else
                  {
                     RemoteDefinition[] remDefs = getDefsDialog(
                         "to start the executables at" ) ;
                     for ( int i = 0 ; i < remDefs.length ; i++ )
                     {
                        if ( onlyRunCompilations( remDefs[ i ] , "" ) == 0 )
                           startRemoteProc( remDefs[ i ] ) ;
                     }
                  }
               }
            }
            ).start() ;

         }
      } ) ;
   }

   /**
    * Returns this BIGRemoteMenu's JMenuItems in ordered Lists according
    * to their category.<br>
    * The returned array is of size 3 where the three indices represent the
    * following categories: [0] setup, [1] measure, [2] evaluate.<br>
    * Besides JMenuItems the Lists may also contain String objects with the
    * value "Separator". Any menu generating method should add a separator
    * to the menu at that place.
    * @return an array of 3 Lists of JMenuItems.
    **/
   public java.util.List[] getRemoteMenus()
   {
      java.util.List[] retval = new java.util.List[3 ] ;
      retval[ 0 ] = new ArrayList() ;
      retval[ 1 ] = new ArrayList() ;
      retval[ 2 ] = new ArrayList() ;
      // add menu items for category: setup
      retval[ 0 ].add( remoteFolder ) ;
      retval[ 0 ].add( removeFolder ) ;
      // add menu items for category: measure
      retval[ 1 ].add( execRemote ) ;
      retval[ 1 ].add( copyExec ) ;
      retval[ 1 ].add( justExec ) ;
      // add menu items for category: evaluate
      retval[ 2 ].add( checkForOutput ) ;
      retval[ 2 ].add( getOutput ) ;
      retval[ 2 ].add( removeOutput ) ;
      return retval ;
   }

   /**
    * shows a Dialog, which can create a Remote Folder
    */
   private void createRemoteFolderDialog()
   {
      final BIGWizard wiz = new BIGWizard( "Create new Session" , false ) ;
      JPanel jp = new JPanel( new GridLayout( 12 , 1 ) ) ;
      Vector insets=new Vector();
      for (int i=0;i<remoteDefs.length;i++)
      {
          boolean found=false;
          for (int j=0;j<insets.size();j++)
              if (remoteDefs[i].getIP().equals(insets.get(j)))
                  found=true;
          if (!found)
              insets.add(remoteDefs[i].getIP());
      }
      final JComboBox ipField = new JComboBox( insets ) ;
      ipField.setMaximumRowCount(20);
      ipField.setEditable(true);
      final JTextField folderField = new JTextField(20) ;
      folderField.setEditable(true);
      folderField.setToolTipText("insert the foldername here, special characters are prohibited.");
         folderField.addKeyListener( new KeyAdapter()
         {
            public void keyTyped( KeyEvent ke )
            {
               boolean acceptable = true ;
               if ( !Character.isLetterOrDigit( ke.getKeyChar() ) )
                  acceptable = false ;

               if ( ke.getKeyChar() == '\n' )
               {
                  acceptable = true ;
                  wiz.next( null ) ;
               }
               if ( ke.getKeyChar() == '\b' )
                  acceptable = true ;
               if ( ke.getKeyChar() == '/' )
                  acceptable = true ;
               if ( ke.getKeyChar() > 122 )
                  acceptable = false ;
               if ( ke.getKeyChar() == '-' )
                  acceptable = true ;
               if ( ke.getKeyChar() == '_' )
                  acceptable = true ;
               if ( !acceptable )
               {
                  ke.consume() ;
                  final JWindow w = new JWindow() ;
                  JLabel l = new JLabel(
                      "Unacceptable input. Not a letter, neither a number." ) ;
                  l.setBorder( new javax.swing.border.EtchedBorder() ) ;
                  l.setForeground( Color.RED ) ;
                  l.setBackground( Color.WHITE ) ;
                  w.getContentPane().add( l ) ;
                  Point p = ( ( JTextField ) ke.getSource() ).
                      getLocationOnScreen() ;
                  p.move( ( int ) p.getX() , ( int ) p.getY() +
                          ( ( JTextField ) ke.getSource() ).getHeight() ) ;
                  w.setLocation( p ) ;
                  w.pack() ;
                  w.setVisible( true ) ;
                  ( new Thread()
                  {
                     public void run()
                     {
                        try
                        {
                           this.sleep( 2000 ) ;
                        }
                        catch ( InterruptedException ex )
                        {
                        }
                        w.setVisible( false ) ;
                     }
                  } ).start() ;
               }
            }
         } ) ;
         folderField.addFocusListener( new FocusAdapter()
         {
            public void focusLost( FocusEvent evt )
            {
               String text = folderField.getText() ;
               StringBuffer sb = new StringBuffer() ;
               for ( int i = 0 ; i < text.length() ; i++ )
               {
                  char c = text.charAt( i ) ;
                  boolean acceptable = false ;
                  if ( Character.isLetterOrDigit( c ) )
                  {
                     acceptable = true ;
                  }
                  if ( c > 122 )
                     acceptable = false ;

                  if ( c == '/' )
                     acceptable = true ;
                  if ( c == '-' )
                     acceptable = true ;
                  if ( c == '_' )
                     acceptable = true ;
                  if ( acceptable )
                     sb.append( text.charAt( i ) ) ;
               }
               if ( !sb.toString().equals( text ) )
               {

                  final JWindow w = new JWindow() ;
                  JLabel l = new JLabel( "Removed unacceptable characters." ) ;
                  l.setBorder( new javax.swing.border.EtchedBorder() ) ;
                  l.setForeground( Color.RED ) ;
                  l.setBackground( Color.WHITE ) ;
                  w.getContentPane().add( l ) ;
                  Point p = ( ( JTextField ) folderField ).
                      getLocationOnScreen() ;
                  p.move( ( int ) p.getX() , ( int ) p.getY() +
                          folderField.getHeight() ) ;
                  w.setLocation( p ) ;
                  w.pack() ;
                  w.setVisible( true ) ;
                  ( new Thread()
                  {
                     public void run()
                     {
                        try
                        {
                           this.sleep( 2000 ) ;
                        }
                        catch ( InterruptedException ex )
                        {
                        }
                        w.setVisible( false ) ;
                     }
                  } ).start() ;
                  folderField.setText( sb.toString() ) ;
                  folderField.validate() ;
               }
            }
         } ) ;
         final JCheckBox useSCP = new JCheckBox( "use SCP instead of SSH/tar" ) ;
      useSCP.setSelected( false ) ;
      final JCheckBox useLOCALDEF=new JCheckBox("Define specific LOCALDEF for folder");
      useLOCALDEF.setSelected( false ) ;
      final JTextField LOCALDEFtf=new JTextField();
      LOCALDEFtf.setEnabled(false);
      final JLabel LOCALDEFlabel=new JLabel( "Specific LOCALDEF for this remote folder" );
      LOCALDEFlabel.setEnabled(false);
      useLOCALDEF.addActionListener(new ActionListener()
          {
             public void actionPerformed(ActionEvent ae)
             {
                LOCALDEFtf.setEnabled(useLOCALDEF.isSelected());
                LOCALDEFlabel.setEnabled(useLOCALDEF.isSelected());
             }
          });
      if ( BIGInterface.getSystem() == BIGInterface.WINDOWS_SYSTEM )
      {
         useSCP.setEnabled( false ) ;
         useSCP.setText( useSCP.getText() + "(UNIX-Option only)" ) ;
     }
     insets=new Vector();
      for (int i=0;i<remoteDefs.length;i++)
      {
          boolean found=false;
          for (int j=0;j<insets.size();j++)
              if (remoteDefs[i].getUsername().equals(insets.get(j)))
                  found=true;
          if (!found)
              insets.add(remoteDefs[i].getUsername());
      }

      final JComboBox userField = new JComboBox(insets) ;
      userField.setMaximumRowCount(20);
      userField.setEditable(true);
      jp.add( new JLabel( "IP or DNS-name of the remote computer" ) ) ;
      jp.add( ipField ) ;
      jp.add( new JLabel( "Username on the remote-computer" ) ) ;
      jp.add( userField ) ;
      jp.add( new JLabel( "Foldername within the home-directory" ) ) ;
      jp.add( folderField ) ;
      jp.add(new JLabel("Shell command for remote computer"));
      final JTextField cmd=new JTextField("bash --login");
      jp.add(cmd);
      jp.add(useLOCALDEF);
      jp.add( LOCALDEFlabel ) ;
      jp.add( LOCALDEFtf ) ;
      jp.add( useSCP ) ;

      wiz.addPanel( jp , "Set the information for the new Session" ) ;

      BIGConsole bcc = null ;
      JPanel pan = null ;
      Action act = null ;
      final Vector saveDef=new Vector();
      try
      {
         final BIGConsole bcc2 = new BIGConsole( 200 , 200 , null ) ;
         bcc = bcc2 ;
         pan = bcc.getDisplayPanel() ;
         pan.setBorder( BorderFactory.createTitledBorder(
             "Output and Messages for copying" ) ) ;
         act = new AbstractAction()
         {
            public void actionPerformed( ActionEvent evt )
            {
                RemoteDefinition def = new RemoteDefinition(
                        (String)ipField.getSelectedItem(),
                        (String)userField.getSelectedItem(),
                        folderField.getText(),
                        false, useSCP.isSelected() ,
                        null,
                        useLOCALDEF.isSelected(),LOCALDEFtf.getText(),
                        cmd.getText() ) ;
               if (saveDef.size()==0)
                  saveDef.add(def);
               else
                  def=(RemoteDefinition)saveDef.get(0);

               final File temp = new File( BIGInterface.getInstance().
                                           getBenchItPath() +
                                           File.separator + "gui" +
                                           File.separator +
                                           "temp" + File.separator +
                                           def.getFoldername() ) ;
               String benchPath = BIGInterface.getInstance().
                   getBenchItPath() ;
               // tools and files
               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "benchit" ) , temp , true ) ;
               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "benchit.c" ) , temp , true ) ;
               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "interface.h" ) , temp , true ) ;
               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "tools" ) , temp , true ) ;
               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "LOCALDEFS" + File.separator +
                   "PROTOTYPE_input_display" ) , temp , true ) ;

               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "LOCALDEFS" + File.separator +
                   "PROTOTYPE_input_architecture" ) , temp , true ) ;

               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "jbi" ) , temp , true ) ;
               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "RUN.SH" ) ,
                                                  new File( temp.
                   getAbsolutePath() ) , true ) ;
               system.BIGFileHelper.copyToFolder( new File( benchPath +
                   File.separator + "COMPILE.SH" ) ,
                                                  new File( temp.
                   getAbsolutePath() ) , true ) ;
               //--------------------------------------------------------------
               // remove unwanted files and folders
               String unwanted = "" ;
               try
               {
                  unwanted = BIGInterface.getInstance().getBIGConfigFileParser().
                      stringCheckOut( "dontCopyList" ) ;
               }
               catch ( Exception ex )
               {
                  BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
                      "dontCopyList",
                      "tools/kernel-gen*");
               }
               StringTokenizer tokenizer = new StringTokenizer(unwanted,",");
               // store all files in directory to Vector
               File [] actualFiles=temp.listFiles();
               Vector all_Files=new Vector();
               for ( int i = 0 ; i < actualFiles.length ; i++ )
                  all_Files.add( actualFiles[ i ] ) ;
               for (int i=0;i<all_Files.size();i++)
               {
                  File actual=(File) (all_Files.get(i));
                  if (actual.isDirectory())
                  {
                     actualFiles=actual.listFiles();
                     for ( int k = 0 ; k< actualFiles.length ;k++ )
                        all_Files.add( actualFiles[ k ] ) ;
                  }
               }
               while (tokenizer.hasMoreTokens())
               {
                  // actualFile can contain * and ?
                  String actualFilterFileName=tokenizer.nextToken().trim();
                  for (int i=0;i<all_Files.size();i++)
                  {
                     if (!((File)all_Files.get(i)).exists())
                     {
                        all_Files.remove(i);
                        i--;
                     }
                     else
                     {
                       if ( isRegEx(
                                      actualFilterFileName,
                                      ( ( File ) all_Files.get( i ) ).getAbsolutePath().
                                      substring( temp.getAbsolutePath().length() +
                                                 1 )  ) )
                        {
                           BIGFileHelper.remove( ( ( File ) all_Files.get( i ) ) ) ;
                           all_Files.remove( i ) ;
                           i-- ;
                        }
                     }
                  }

               }
               //--------------------------------------------------------------
               if ( useSCP.isSelected() )
               {
                        String command = "scp -r " +
                            temp.getAbsolutePath() +
                            " " + def.getUsername() + "@" + def.getIP() + ":~/" ;
                        Process p = null ;
                        bcc2.postMessage(
                            "Please enter your password at the console, you started the BenchIT-GUI with." ,
                            BIGConsole.DEBUG ) ;

                        try
                        {
                           p = Runtime.getRuntime().exec( command ) ;
                           bcc2.
                               addStream( p.getInputStream() ,
                                          BIGConsole.DEBUG ) ;
                           bcc2.
                               addStream( p.getErrorStream() ,
                                          BIGConsole.ERROR ) ;

                        }
                        catch ( IOException ex )
                        {
                           bcc2.postMessage(
                               "Couldn't copy folder to remote machine." ,
                               BIGConsole.ERROR ) ;
                        }
                        try
                        {
                           p.waitFor() ;
                        }
                        catch ( InterruptedException ex1 )
                        {
                           bcc2.postMessage( "Didn't wait for scp to end!" ,
                                             BIGConsole.ERROR ) ;
                        }

                        bcc2.postMessage(
                            "Succesfully copied main data to new remote-folder" ,
                            BIGConsole.DEBUG ) ;

                        addRemoteDefinition( def ) ;
                        try
                        {
                           saveRemoteDefs() ;
                        }
                        catch ( Exception ex )
                        {
                           System.err.println(
                               "Couldn't load \"RemoteXMLFile\".Can't save new entry" ) ;
                        }


               }
               else
               {
                  Session sess = openSession( def ) ;
                  if ( sess == null )
                  {
                     bcc2.postMessage(
                         "Error(0) while copying basic folder to remote system" ,
                         BIGConsole.ERROR ) ;
                     return ;

                  }
                  TarOutputStream tarOut = null;
                  try
                  {
                     tarOut = new TarOutputStream( sess.getStdin() ) ;
                  }
                  catch ( IOException ex )
                  {
                  }
                  try
                  {
                     bcc2.
                         addStream( sess.getStdout() ,
                                    BIGConsole.DEBUG ) ;
                     bcc2.
                         addStream( sess.getStderr() ,
                                    BIGConsole.ERROR ) ;

                  }
                  catch ( IOException ex2 )
                  {
                     bcc2.postMessage(
                         "Error(3) while copying basic folder to remote system" ,
                         BIGConsole.ERROR ) ;
                     closeSession( sess ) ;
                     sess.getExitSignal() ;
                     return ;
                  }

                  tarOut.setLongFileMode(tarOut.LONGFILE_GNU);
                  File[] allFiles=null;
                  allFiles=system.BIGFileHelper.getAllSubFiles(temp,allFiles,0);
                  String path = "" ;
                  if ( def.getFoldername().startsWith( "/" ) )
                  {
                     path = "-C /" ;
                  }
                  try
                  {
                     sess.execCommand( "tar xBf - " + path ) ;
                     if ( debug ) System.err.println( "tar xBf - " + path ) ;

                  }
                  catch ( IOException ex4 )
                  {
                     bcc2.postMessage(
                         "Error(1) while copying basic folder to remote system" ,
                         BIGConsole.ERROR ) ;
                     return ;

                  }
                  try
                  {
                  byte buffer[] = new byte[8192];
                  // Open archive file
                  boolean isWin=false;
                  if (BIGInterface.getSystem()==BIGInterface.WINDOWS_SYSTEM)
                      isWin=true;
                  if ( allFiles != null )
                  {
                     for ( int i = 0 ; i < allFiles.length ; i++ )
                     {
                        File file =  allFiles[ i ] ;
                        if ( file == null || !file.exists( ))
                           continue ;


                        // Add archive entry
                        TarEntry tarAdd = new TarEntry( file ) ;
                        tarAdd.setModTime( file.lastModified() ) ;
                        tarAdd.setName( file.getAbsolutePath().substring(temp.getAbsolutePath().lastIndexOf("temp")+5) ) ;
                        if (isWin)
                            tarAdd.setName(tarAdd.getName().replaceAll("\\\\","/"));
                        tarOut.putNextEntry( tarAdd ) ;
                        // Write file to archive
                        if (file.isFile())
                        {
                           FileInputStream in = new FileInputStream( file ) ;
                           while ( true )
                           {
                              int nRead = in.read( buffer , 0 , buffer.length ) ;
                              if ( nRead <= 0 )
                                 break ;
                              tarOut.write( buffer , 0 , nRead ) ;
                           }
                           in.close() ;
                        }
                        tarOut.flush();
                        tarOut.closeEntry() ;
                     }
                     tarOut.close() ;
                  }
                  }
                  catch ( IOException ex3 )
                  {
                     ex3.printStackTrace();
                     bcc2.postMessage(
                         "Error(2) while copying basic folder to remote system" ,
                         BIGConsole.ERROR ) ;
                     return ;

                  }
                  bcc2.postMessage(
                      "Succesfully copied main data to new remote-folder" ,
                      BIGConsole.DEBUG ) ;
                  addRemoteDefinition( def ) ;
                  try
                  {
                     saveRemoteDefs() ;
                  }
                  catch ( Exception ex )
                  {
                     System.err.println(
                         "Couldn't load \"RemoteXMLFile\".Can't save new entry" ) ;
                  }
               }

            }
         } ;
      }
      catch ( IOException ex )
      {
         JLabel error = new JLabel( "Couldn't open BIGConsole" ) ;
         pan = new JPanel() ;
         pan.add( error ) ;
      }
      if ( act != null )
         wiz.addPanel( pan , act ) ;
      else
         wiz.addPanel( pan ) ;
      JPanel pan2 = null ;
      Action act2 = null ;
      try
      {
         act2 = null ;
         final BIGConsole bcc3 = new BIGConsole( 200 , 200 , null ) ;
         pan2 = bcc3.getDisplayPanel() ;

         act2 = new AbstractAction()
         {
            public void actionPerformed( ActionEvent evt )
            {
               RemoteDefinition def = new RemoteDefinition(
                   ( String ) ipField.getSelectedItem() ,
                   ( String ) userField.getSelectedItem() ,
                   ( String ) folderField.getText() ,
                   false , useSCP.isSelected() ,
                   null,
                   useLOCALDEF.isSelected() , LOCALDEFtf.getText() ,
                   cmd.getText() ) ;
               if (saveDef.size()==0)
                  saveDef.add(def);
               else
                  def=(RemoteDefinition)saveDef.get(0);

               if (( useLOCALDEF.isSelected() ) &&
                   (new File(BIGInterface.getInstance().getBenchItPath() +
                                  File.separator + "LOCALDEFS" + File.separator+
                                  LOCALDEFtf.getText())).exists())
               {
                  bcc3.postMessage( "LOCALDEFS for " + LOCALDEFtf.getText() +
                                    " already exist, skip creating." ) ;
                  return ;
               }
               String name = getHostname( def ) ;
               // if we have the LOCALDEFS dont create em
               if ( ( !useLOCALDEF.isSelected() ) &&
                    ( ( new File( BIGInterface.getInstance().getBenchItPath() +
                                  File.separator + "LOCALDEFS" + File.separator +
                                  name ) ).exists() ))
               {
                  bcc3.postMessage("LOCALDEFS for "+name+" already exist, skip creating.");
                  return;
               }
               if (saveDef.size()==0)
                  saveDef.add(def);
               else
                  def=(RemoteDefinition)saveDef.get(0);

               Session s = openSession( def ) ;
               try
               {
                  InputStream[] is = new InputStream[2 ] ;
                  is[ 0 ] = s.getStdout() ;
                  is[ 1 ] = s.getStderr() ;
                  bcc3.addStream( is[ 0 ] , BIGConsole.DEBUG ) ;
                  bcc3.addStream( is[ 1 ] , BIGConsole.WARNING ) ;
                  String otherLocaldef = def.getLOCALDEF() ;
                  String cmd =
                      // go in remote dir
                      "cd " + def.getFoldername() +
                      " && echo \"start creating LOCALDEFs\" " +
                      // add executable to tools
                      " && chmod -R +x tools &&" +
                      // make LOCALDEF-dir
                      " mkdir LOCALDEFS &&" +
                      // copy PROTOTYPEs in
                      " mv PROTOTYPE* LOCALDEFS/ &&" +
                      // dont ask for stuff
                      " export BENCHIT_INTERACTIVE=0 &&" +
                      " echo \"y\" | sh tools/FIRSTTIME &&";
                  // add localdefname if necessary
                  if ( otherLocaldef!=null )
                  {
                     cmd = cmd + " mv LOCALDEFS/"+name+" LOCALDEFS/" + otherLocaldef+" && ";
                     cmd = cmd + " mv LOCALDEFS/"+name+"_input_architecture LOCALDEFS/" + otherLocaldef+"_input_architecture && ";
                     cmd = cmd + " mv LOCALDEFS/"+name+"_input_display LOCALDEFS/" + otherLocaldef+"_input_display && ";
                  }
                  // !must pipe an y in, because it may ask for input
                  cmd = cmd + " echo \"finished creating LOCALDEFs\" " ;

                  s.execCommand( cmd) ;
                  //System.err.println(cmd);
                  /*System.err.println("cd " + def.getFoldername() +
                                 " && echo \"start creating LOCALDEFs\" &&"+
                                 " chmod -R +x tools &&"+
                                 " mkdir LOCALDEFS &&"+
                                 " mv PROTOTYPE* LOCALDEFS/ &&"+
                                 " export BENCHIT_INTERACTIVE=0 &&"+
                                 " echo \"y\" | sh tools/FIRSTTIME "+
                                 " && echo \"finished creating LOCALDEFs\"");*/
                  s.waitUntilDataAvailable(1000);
               }
               catch ( IOException ex )
               {
                  System.err.println( "COuldnt start FIRSTTIME" ) ;
               }
            }
         } ;
      }
      catch ( IOException ex1 )
      {
         System.err.println("Error with console");
      }

         if ( act2 != null )
            wiz.addPanel( pan2 , act2 ) ;
         else
         wiz.addPanel( pan2 ) ;

      try
      {
         act2 = null ;
         final BIGConsole bcc3 = new BIGConsole( 200 , 200 , null ) ;
         pan2 = bcc3.getDisplayPanel() ;

      act2 = new AbstractAction()
      {
         public void actionPerformed( ActionEvent evt )
         {
            RemoteDefinition def = new RemoteDefinition(
                ( String ) ipField.getSelectedItem() ,
                ( String ) userField.getSelectedItem() ,
                ( String ) folderField.getText() ,
                false , useSCP.isSelected() ,
                null ,
                useLOCALDEF.isSelected() , LOCALDEFtf.getText() ,
                cmd.getText() ) ;
            if ( saveDef.size() == 0 )
               saveDef.add( def ) ;
            else
               def = ( RemoteDefinition ) saveDef.get( 0 ) ;
            // if we have the LOCALDEFS dont create em
            if ( ( useLOCALDEF.isSelected() ) &&
                 ( new File( BIGInterface.getInstance().getBenchItPath() +
                             File.separator + "LOCALDEFS" + File.separator +
                             LOCALDEFtf.getText() ) ).exists() )
            {
               bcc3.postMessage( "LOCALDEFS for " + LOCALDEFtf.getText() +
                                 " already exist, skip getting." ) ;
               bcc3.postMessage(
                   "Press Exit to finish creating a remotefolder.\n" +
                   "You may load the LOCALDEFS of the remote-system\n" +
                   LOCALDEFtf.getText() + " by Loading LOCALDEFs." ) ;

               return ;
            }
            String name = getHostname( def ) ;
            if ( ( !useLOCALDEF.isSelected() ) &&
                 ( ( new File( BIGInterface.getInstance().getBenchItPath() +
                               File.separator + "LOCALDEFS" + File.separator +
                               name ) ).exists() ) )
            {
               bcc3.postMessage( "LOCALDEFS for " + name +
                                 " already exist, skip getting." ) ;
               bcc3.postMessage(
                   "Press Exit to finish creating a remotefolder.\n" +
                   "You may load the LOCALDEFS of the remote-system\n" +
                   name + " by Loading LOCALDEFs." ) ;

               return ;
            }

            getLOCALDEFSFromRemote( def ) ;
            bcc3.postMessage( "Press Exit to finish creating a remotefolder.\n" +
                              "You may load the LOCALDEFS of the remote-system\n" +
                              name + " by Loading LOCALDEFs." ) ;
         }
         } ;
      }
      catch ( IOException ex1 )
      {
         System.err.println("Error with console");
      }

         if ( act2 != null )
            wiz.addPanel( pan2 , act2 ) ;
         else
         wiz.addPanel( pan2 ) ;
      wiz.start() ;
   }

   private JPanel getDefsPanel()
   {

      JPanel pan = new JPanel( new GridLayout( this.remoteDefs.length , 1 ) ) ;
      for ( int i = 0 ; i < this.remoteDefs.length ; i++ )
         pan.add( new JCheckBox( this.remoteDefs[ i ].toString() ,
                                 this.remoteDefs[ i ].isSelected() ) ) ;
      return pan ;
   }

   private JCheckBox[] getDefsAction( JPanel pan )
   {
      JCheckBox[] checkBoxes = new JCheckBox[this.remoteDefs.length ] ;
      int position = 0 ;
      for ( int i = 0 ; i < pan.getComponentCount() ; i++ )
      {
         if ( pan.getComponent( i ) instanceof JCheckBox )
         {
            checkBoxes[ position ] = ( JCheckBox ) pan.getComponent( i ) ;
            position++ ;
         }
      }
      return checkBoxes ;
   }

   private void put( SCPClient scp , File f , String directory )
   {
      if ( f.isFile() )
      {
         try
         {
            scp.put( f.getAbsolutePath() , directory ) ;
         }
         catch ( IOException ex1 )
         {
         }
      }
      else
      {
         File[] files = f.listFiles( new FileFilter()
         {
            public boolean accept( File f )
            {
               if ( f.isFile() )
               {
                  return true ;
               }
               return false ;
            }
         } ) ;
         if ( files != null )
         {
            String[] filesStr = new String[files.length ] ;
            for ( int i = 0 ; i < files.length ; i++ )
            {
               filesStr[ i ] = files[ i ].getAbsolutePath() ;
            }

            try
            {
               scp.put( filesStr , directory + "/" + f.getName() ) ;
            }
            catch ( IOException ex )
            {
            }
         }

         File[] dirs = f.listFiles( new FileFilter()
         {
            public boolean accept( File f )
            {
               if ( !f.isFile() )
               {
                  return true ;
               }
               return false ;
            }
         } ) ;

         if ( dirs != null )
         {

            for ( int i = 0 ; i < dirs.length ; i++ )
            {
               this.put( scp , dirs[ i ] , directory + "/" + f.getName() ) ;
            }

         }
      }
   }
   //stores the position of the vertical scrollbar
   int defsDialogPosition=0;
   private RemoteDefinition[] getDefsDialog( final String reason )
   {
      JCheckBox[] checkBoxes = new JCheckBox[this.remoteDefs.length ] ;
      for ( int i = 0 ; i < checkBoxes.length ; i++ )
      {
         checkBoxes[ i ] = new JCheckBox( this.remoteDefs[ i ].toString() ,
                                          this.remoteDefs[ i ].isSelected() ) ;
      }
      JPanel pan=new JPanel(new GridLayout(checkBoxes.length+1,1));
      pan.add(new JLabel("Select the Remote Folder for " + reason ));
      for (int i=0;i<checkBoxes.length;i++)
      {
         pan.add( checkBoxes[ i ] ) ;
      }
      final JScrollPane sp=new JScrollPane(pan);
      sp.getVerticalScrollBar().setValue(defsDialogPosition);
      sp.setPreferredSize(new Dimension(500,500));
      sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
      sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
      int value = JOptionPane.showConfirmDialog( bgui ,sp ,
                                                 "select remote folders for " +
                                                 reason ,
                                                 JOptionPane.OK_CANCEL_OPTION ,
                                                 JOptionPane.QUESTION_MESSAGE ) ;
      defsDialogPosition=sp.getVerticalScrollBar().getValue();
      if ( ( value == JOptionPane.CANCEL_OPTION ) ||
           ( value == JOptionPane.CLOSED_OPTION ) )
         return new RemoteDefinition[0 ] ;
      else
      {
         int numberOfRemFolders = 0 ;
         for ( int i = 0 ; i < checkBoxes.length ; i++ )
         {
            remoteDefs[ i ].setSelected( checkBoxes[ i ].isSelected() ) ;
            if ( checkBoxes[ i ].isSelected() )
            {
               numberOfRemFolders++ ;
            }
         }
         RemoteDefinition[] retRemDef = new RemoteDefinition[
             numberOfRemFolders ] ;
         numberOfRemFolders = 0 ;
         for ( int i = 0 ; i < checkBoxes.length ; i++ )
         {
            remoteDefs[i].setSelected(checkBoxes[i].isSelected());
            if ( checkBoxes[ i ].isSelected() )
            {
               retRemDef[ numberOfRemFolders ] = remoteDefs[ i ] ;
               numberOfRemFolders++ ;
            }
         }
         return retRemDef ;
      }
   }

   /**
    * opens a Dialog where to check for output-data
    */
   private void checkForOutput()
   {

      RemoteDefinition[] remDefs = this.getDefsDialog( "to check for output" ) ;
      for ( int j = 0 ; j < remDefs.length ; j++ )
      {
         RemoteDefinition defs = remDefs[ j ] ;
         checkForOutputs( defs ) ;
      }
   }

   /**
    * shows a dialog where to get output data from
    */
   private void getOutput()
   {
      ( new Thread()
      {
         public void run()
         {
            RemoteDefinition[] remDefs = getDefsDialog(
                "to get the output from" ) ;
            for ( int j = 0 ; j < remDefs.length ; j++ )
            {
               RemoteDefinition defs = remDefs[ j ] ;
               getOutputFromRemote( defs ) ;
               BIGRemoteMenu.this.bgui.getResultTree().updateResultTree( null ) ;
            }
         }
      } ).start() ;
   }

   /**
    * remove a remote folder
    */
   private void removeRemoteFolder()
   {
      final JDialog jd = new JDialog() ;
      JPanel jp = new JPanel( new GridLayout( 3 , 1 ) ) ;
      final JComboBox defBox = new JComboBox( this.remoteDefs ) ;
      jp.add( new JLabel( "Select the Remote Folder to remove" ) ) ;
      jp.add( defBox ) ;
      JPanel but = new JPanel() ;
      JButton cancel = new JButton( "Cancel" ) ;
      cancel.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            jd.setVisible( false ) ;
         }
      } ) ;
      JButton okay = new JButton( "Okay" ) ;
      okay.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            jd.setVisible( false ) ;
            if ( removeRemoteFolder( defBox.getSelectedIndex() ) == 0 )
               System.out.println( "Removing succesfull." ) ;
            else
               System.out.println( "Removing failed." ) ;

         }
      } ) ;

      but.add( cancel , BorderLayout.WEST ) ;
      but.add( okay , BorderLayout.EAST ) ;
      jd.setDefaultCloseOperation( jd.DISPOSE_ON_CLOSE ) ;
      jp.add( but ) ;
      jd.setContentPane( jp ) ;
      jd.setSize( jd.getPreferredSize() ) ;
      jd.setVisible( true ) ;
   }

   /**
    * remove output from a remote folder
    */
   private void removeRemoteOutput()
   {
      final JDialog jd = new JDialog() ;
      JPanel jp = new JPanel( new GridLayout( 3 , 1 ) ) ;
      final JComboBox defBox = new JComboBox( this.remoteDefs ) ;
      jp.add( new JLabel( "Select the Remote Folder to remove output from" ) ) ;
      jp.add( defBox ) ;
      JPanel but = new JPanel() ;
      JButton cancel = new JButton( "Cancel" ) ;
      cancel.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            jd.setVisible( false ) ;
         }
      } ) ;
      JButton okay = new JButton( "Okay" ) ;
      okay.addActionListener( new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            jd.setVisible( false ) ;
            removeRemoteOutput( defBox.getSelectedIndex() ) ;
         }
      } ) ;

      but.add( cancel , BorderLayout.WEST ) ;
      but.add( okay , BorderLayout.EAST ) ;
      jd.setDefaultCloseOperation( jd.DISPOSE_ON_CLOSE ) ;
      jp.add( but ) ;
      jd.setContentPane( jp ) ;
      jd.pack() ;
      jd.validate() ;
      jd.setVisible( true ) ;
   }

   /**
    * starts the scp command to copy KernelDirectories and LOCALDEFS
    * @param def RemoteDefinition destination
    * @param tempDir File source tempDir
    * @return int returnstatus of scp
    */
   private int scpCopyKernelsAndLOCDEFs( RemoteDefinition def , File tempDir )
   {
      if ( def.getUseSCP() )
      {
         this.showTypeDialog( def ) ;
         String command = "scp -r " +
             tempDir.getAbsolutePath() +
             " " + def.getUsername() + "@" + def.getIP() + ":~/" ;
         Process p = null ;
         BIGConsole bcc2 = null ;
         try
         {
            bcc2 = new BIGConsole( 0 , 0 , null ) ;
            p = Runtime.getRuntime().exec( command ) ;
            bcc2.
                addStream( p.getInputStream() ,
                           BIGConsole.DEBUG ) ;
            bcc2.
                addStream( p.getErrorStream() ,
                           BIGConsole.ERROR ) ;

         }
         catch ( IOException ex )
         {
            System.err.println( "Couldn't copy folder to remote machine." ) ;
            return -1 ;
         }
         try
         {
            return p.waitFor() ;
         }
         catch ( InterruptedException ex1 )
         {
            System.err.println( "Didn't wait for scp to end!" ) ;
            return -1 ;
         }

      }
      else
      {
         String path = "" ;
         if ( def.getFoldername().startsWith( "/" ) )
         {
            path = "-C /" ;
         }
         Session sess = openSession( def ) ;
         try
         {
            InputStream is[] = new InputStream[2 ] ;
            is[ 0 ] = sess.getStdout() ;
            is[ 1 ] = sess.getStderr() ;
            BIGConsole c = new BIGConsole( 300 , 600 , is ) ;
            //c.show() ;
         }
         catch ( IOException ex2 )
         {
            ex2.printStackTrace();
         }
         if ( sess == null )
         {
            System.err.println(
                "Error(0) while copying basic folder to remote system"
                 ) ;
            return  -1;

         }
         TarOutputStream tarOut = null;
         try
         {
            tarOut = new TarOutputStream( sess.getStdin() ) ;
         }
         catch ( IOException ex )
         {
         }
         tarOut.setLongFileMode(tarOut.LONGFILE_GNU);
         File[] allFiles=null;
         allFiles=system.BIGFileHelper.getAllSubFiles(tempDir,allFiles,0);
         try
         {
            sess.execCommand( "tar xBf - " + path ) ;
            if ( debug ) System.err.println( "tar xBf - " + path ) ;
         }
         catch ( IOException ex4 )
         {
            System.err.println(
                "Error(1) while copying basic folder to remote system"  ) ;
            return -1;

         }
         try
         {
         byte buffer[] = new byte[8192];
         // Open archive file

         if ( allFiles != null )
         {
             boolean isWin=false;
             if (BIGInterface.getSystem()==BIGInterface.WINDOWS_SYSTEM)
                 isWin=true;
            for ( int i = 0 ; i < allFiles.length ; i++ )
            {
               File file =  allFiles[ i ] ;
               if ( file == null || !file.exists( ))
                  continue ;


              // Add archive entry
              TarEntry tarAdd = new TarEntry(file);
              tarAdd.setModTime(file.lastModified());
              tarAdd.setName(file.getAbsolutePath().substring(tempDir.
                      getAbsolutePath().lastIndexOf(File.separator + "temp" +
                      File.separator +
                      def.getFoldername()) + 6));
              if (isWin)
                  tarAdd.setName(tarAdd.getName().replaceAll("\\\\", "/"));
               tarOut.putNextEntry( tarAdd ) ;
               // Write file to archive
               if (file.isFile())
               {
                  FileInputStream in = new FileInputStream( file ) ;
                  while ( true )
                  {
                     int nRead = in.read( buffer , 0 , buffer.length ) ;
                     if ( nRead <= 0 )
                        break ;
                     tarOut.write( buffer , 0 , nRead ) ;
                  }
                  in.close() ;
               }
               tarOut.flush();
               tarOut.closeEntry() ;
            }
            tarOut.close() ;
         }
         }
         catch ( IOException ex3 )
         {
            ex3.printStackTrace();
            System.err.println(
                "Error(2) while copying basic folder to remote system"  ) ;
            return -1;

         }
            closeSession( sess ) ;
            sess.getExitSignal() ;
            return 0 ;


         /*TarArchive arch = null ;
         try
         {
            sess.execCommand( "tar xf - " + path ) ;
            arch = new TarArchive( sess.getStdin() , tempDir ) ;
         }
         catch ( Exception ex4 )
         {
            System.err.println(
                "Error(1) while copying something to remote directory" ) ;

         }
         try
         {
            arch.writeEntry( new TarEntry( tempDir ) , true ) ;
         }
         catch ( Exception ex3 )
         {
            System.err.println(
                "Error(2) while copying something to remote directory" ) ;
         }
         try
         {
            BIGInterface.getInstance().getConsole().addStream( sess.
                getStdout() ,
                BIGConsole.DEBUG ) ;
         }
         catch ( Exception ex2 )
         {
            System.err.println(
                "Error(3) while copying something to remote directory" ) ;
            return -1 ;
         }
         try
         {
            arch.closeArchive() ;
         }
         catch ( IOException ex )
         {
         }*/
      }
   }

   /**
    * starts ssh command, that checks for output
    * @param def RemoteDefinition destination
    * @return int returnstatus of ssh
    */
   private int checkForOutputs( RemoteDefinition def )
   {
      Session sess = null ;
      sess = openSession( def ) ;
      if ( sess == null )return 0 ;
      BIGConsole bcc = null ;
      try
      {
         InputStream[] in = new InputStream[2 ] ;
         in[ 0 ] = sess.getStdout() ;
         in[ 1 ] = sess.getStderr() ;
         bcc = new BIGConsole( 400 , 400 , in ) ;
         bcc.setTitle( "Results on " + def.getIP() + "(" + def.getFoldername() +
                       ")" ) ;
         bcc.setVisible( true ) ;
         sess.execCommand( "echo \"Found results\" && find " +
                           def.getFoldername() +
                           " -name *.bit && echo \"FINISHED\"" ) ;
         if ( debug ) System.err.println( "find " + def.getFoldername() +
                                          " -name *.bit" ) ;

      }
      catch ( IOException ex2 )
      {
         System.err.println(
             "Error(2) while checking for output" ) ;
         return -1 ;

      }

      while ( bcc.getText().indexOf( "\nFINISHED" ) == -1 ) ;
      if (bcc.getText().equals("Found results\nFINISHED"))
         System.out.println("No results were found.");
      closeSession( sess ) ;
      sess.getExitSignal() ;
      return 0 ;
   }

   /**
    * starts scp-command to get outputdata from a RemoteDefinition
    * @param def RemoteDefinition to get from this
    * @return int returnstatus from scp
    */
   private int getOutputFromRemote( RemoteDefinition def )
   {
      if ( def.getUseSCP() )
      {

         this.showTypeDialog( def ) ;
         String command = "scp -r " + def.getUsername() + "@" + def.getIP() +
             ":~/" + def.getFoldername() + "/" + "output " +
             " " + BIGInterface.getInstance().getBenchItPath() ;
         Process p = null ;
         BIGConsole bcc2 = null ;
         try
         {
            bcc2 = new BIGConsole( 0 , 0 , null ) ;
            p = Runtime.getRuntime().exec( command ) ;
            bcc2.
                addStream( p.getInputStream() ,
                           BIGConsole.DEBUG ) ;
            bcc2.
                addStream( p.getErrorStream() ,
                           BIGConsole.ERROR ) ;

         }
         catch ( IOException ex )
         {
            System.err.println( "Couldn't copy folder to remote machine." ) ;
            return -1 ;
         }
         try
         {
            return p.waitFor() ;
         }
         catch ( InterruptedException ex1 )
         {
            System.err.println( "Didn't wait for scp to end!" ) ;
            return -1 ;
         }

      }
      else
      {
         try
         {
          JLabel status=BIGInterface.getInstance().getStatusLabel();
            String path = "" ;
            if ( def.getFoldername().startsWith( "/" ) )
               path = "-C / " ;
            File tempFile=new File("temp.tar");
            Session sess = this.openSession( def ) ;
            if (sess==null)
               return -1;
            status.setText("open Session");
            sess.execCommand(  "tar cBf - " + path + def.getFoldername() +"/output") ;
            status.setText("get output");
            OutputStream out = new FileOutputStream( tempFile) ;

            InputStream is = sess.getStdout() ;

            byte[] buff = new byte[8192 ] ;

            while ( true )
            {
               if (is.available()<0)
                  break;
               int len = is.read( buff ) ;
               if ( len < 0 )
               {
                  break ;
               }
               out.write( buff , 0 , len ) ;
            }

            out.flush() ;
            out.close() ;
            String tempContent=BIGFileHelper.getFileContent(tempFile);
            if (tempContent.indexOf(def.getFoldername())<0)
            {
               JOptionPane.showMessageDialog(null,"The folder seems to contain no output-data");
               status.setText("done");
               return -1;
            }
            tempContent=tempContent.substring(tempContent.indexOf(def.getFoldername()));
            BIGFileHelper.saveToFile( tempContent , tempFile ) ;

            File tempDir = ( new
                             File( BIGInterface.getInstance().getBenchItPath() +
                                   File.separator + "gui" + File.separator + "temp" ) ) ;
            TarInputStream tarIn=new TarInputStream(new FileInputStream(tempFile));
            TarEntry actualEntry=tarIn.getNextEntry();
            while(actualEntry!=null)
            {
               if (actualEntry.isDirectory())
               {
                  File newDirectory=new File(tempDir.getAbsolutePath()+File.separator+
                      actualEntry.getName());
                  newDirectory.mkdirs();
               }
               else
               {
                  byte[] buffer = new byte[8192];
                  File newFile = new File( tempDir.getAbsolutePath() +
                                           File.separator +
                                           actualEntry.getName() ) ;
                  FileOutputStream outNew=new FileOutputStream(newFile);
                  tarIn.copyEntryContents(outNew);
                  /*FileInputStream in = new FileInputStream( actualEntry.getFile() ) ;
                     while ( true )
                     {
                        int nRead = in.read( buffer , 0 , buffer.length ) ;
                        if ( nRead <= 0 )
                           break ;
                        outNew.write( buffer , 0 , nRead ) ;
                     }
                     in.close() ;*/
                     outNew.close();
               }
               actualEntry=tarIn.getNextEntry();
            }
            /*TarArchive arch = new TarArchive( new FileInputStream( tempFile ) ) ;
            File tempDir = ( new
                             File( BIGInterface.getInstance().getBenchItPath() +
                                   File.separator + "gui" + File.separator + "temp" ) ) ;

            try
            {
               arch.extractContents( tempDir ) ;
            }
            catch ( IOException ex6 )
            {
               ex6.printStackTrace() ;
               System.err.println( "Couldn't extract temporary tar-file" ) ;
               return -1 ;
            }*/
            this.closeSession( sess ) ;

            BIGFileHelper.copyToFolder( new File( tempDir.getAbsolutePath() +
                                                  File.separator +
                                                  def.getFoldername() +
                                                  File.separator + "output" ) ,
                                        new
                                        File( BIGInterface.getInstance().
                                              getBenchItPath() ) , false ) ;
            status.setText( "removing temporary files" ) ;
            BIGFileHelper.remove(tempDir);
            //BIGFileHelper.remove(tempFile);
            status.setText( "done" ) ;

            return 0 ;
         }
         catch ( IOException ex2 )
         {
            ex2.printStackTrace();
            return -1 ;
         }

      }

   }

   /**
    * starts scp-command to get outputdata from a RemoteDefinition
    * @param def RemoteDefinition to get from this
    * @return int returnstatus from scp
    */
   private int getLOCALDEFSFromRemote( RemoteDefinition def )
   {
      if ( def.getUseSCP() )
      {

         this.showTypeDialog( def ) ;
         String command = "scp -r " + def.getUsername() + "@" + def.getIP() +
             ":~/" + def.getFoldername() + "/" + "LOCALDEFS " +
             " " + BIGInterface.getInstance().getBenchItPath() ;
         Process p = null ;
         BIGConsole bcc2 = null ;
         try
         {
            bcc2 = new BIGConsole( 0 , 0 , null ) ;
            p = Runtime.getRuntime().exec( command ) ;
            bcc2.
                addStream( p.getInputStream() ,
                           BIGConsole.DEBUG ) ;
            bcc2.
                addStream( p.getErrorStream() ,
                           BIGConsole.ERROR ) ;

         }
         catch ( IOException ex )
         {
            System.err.println( "Couldn't copy LOCALDEFS from remote machine." ) ;
            return -1 ;
         }
         try
         {
            return p.waitFor() ;
         }
         catch ( InterruptedException ex1 )
         {
            System.err.println( "Didn't wait for scp to end!" ) ;
            return -1 ;
         }

      }
      else
      {
         try
         {
          JLabel status=BIGInterface.getInstance().getStatusLabel();
            String path = "" ;
            if ( def.getFoldername().startsWith( "/" ) )
               path = "-C / " ;
            File tempFile=new File("temp.tar");
            Session sess = this.openSession( def ) ;
            if (sess==null)
               return -1;
            status.setText("open Session");
            sess.execCommand(  "tar cBf - " + path + def.getFoldername() +"/LOCALDEFS") ;
            status.setText("get LOCALDEFS");
            OutputStream out = new FileOutputStream( tempFile) ;

            InputStream is = sess.getStdout() ;

            byte[] buff = new byte[8192 ] ;

            while ( true )
            {
               if (is.available()<0)
                  break;
               int len = is.read( buff ) ;
               if ( len < 0 )
               {
                  break ;
               }
               out.write( buff , 0 , len ) ;
            }

            out.flush() ;
            out.close() ;
            String tempContent=BIGFileHelper.getFileContent(tempFile);
            if (tempContent.indexOf(def.getFoldername())<0)
            {
               JOptionPane.showMessageDialog(null,"The folder seems to contain no LOCALDEFS");
               status.setText("done");
               return -1;
            }
            tempContent=tempContent.substring(tempContent.indexOf(def.getFoldername()));
            BIGFileHelper.saveToFile( tempContent , tempFile ) ;

            File tempDir = ( new
                             File( BIGInterface.getInstance().getBenchItPath() +
                                   File.separator + "gui" + File.separator + "temp" ) ) ;
            TarInputStream tarIn=new TarInputStream(new FileInputStream(tempFile));
            TarEntry actualEntry=tarIn.getNextEntry();
            while(actualEntry!=null)
            {
               if (actualEntry.isDirectory())
               {
                  File newDirectory=new File(tempDir.getAbsolutePath()+File.separator+
                      actualEntry.getName());
                  newDirectory.mkdirs();
               }
               else
               {
                  byte[] buffer = new byte[8192];
                  File newFile = new File( tempDir.getAbsolutePath() +
                                           File.separator +
                                           actualEntry.getName() ) ;
                  FileOutputStream outNew = new FileOutputStream( newFile ) ;
                  tarIn.copyEntryContents( outNew ) ;
                  outNew.close() ;
               }
               actualEntry = tarIn.getNextEntry() ;
               }
               this.closeSession( sess )  ;
               BIGFileHelper.copyToFolder( new File( tempDir.getAbsolutePath() +
                                                  File.separator +
                                                  def.getFoldername() +
                                                  File.separator + "LOCALDEFS" ) ,
                                        new
                                        File( BIGInterface.getInstance().
                                              getBenchItPath() ) , false ) ;
            status.setText( "removing temporary files" ) ;
            BIGFileHelper.remove(tempDir);
            BIGFileHelper.remove(tempFile);
            status.setText( "done" ) ;

            return 0 ;
         }
         catch ( IOException ex2 )
         {
            ex2.printStackTrace();
            return -1 ;
         }

      }

   }
   /**
    * starts ssh - command to remove a remote-folders output data from a remote computer
    * @param remDefPos int position in this.remoteDefs
    * @return int returnstatus of the ssh command
    */
   private int removeRemoteOutput( int remDefPos )
   {
      Session sess = this.openSession( remoteDefs[ remDefPos ] ) ;
      if (sess==null)
      {
         System.err.println( "Couldn't remove remote output." ) ;
         return -1 ;
      }

      String home = "~" ;
      if ( this.remoteDefs[ remDefPos ].getFoldername().startsWith( "/" ) )
         home = "" ;
      try
      {
         sess.execCommand( "rm -r " + home + "/" +
                           remoteDefs[ remDefPos ].getFoldername() +
                           "/output/*" ) ;
      }
      catch ( IOException ex2 )
      {
         System.err.println( "Couldn't remove remote folder." ) ;
         return -1 ;
      }
      return 0 ;

   }

   /**
    * starts ssh - command to remove a remote-folder from a remote computer and from
    * this.remoteDefs
    * @param remDefPos int position in this.remoteDefs
    * @return int returnstatus of the ssh command
    */
   private int removeRemoteFolder( int remDefPos )
   {
      Session sess = this.openSession( remoteDefs[ remDefPos ] ) ;
      if (sess==null)
      {
         System.err.println( "Couldn't remove remote folder." ) ;
         return -1 ;
      }

      String home = "~" ;
      if ( this.remoteDefs[ remDefPos ].getFoldername().startsWith( "/" ) )
         home = "" ;

      try
      {
         sess.execCommand( "rm -r " + home + "/" +
                           remoteDefs[ remDefPos ].getFoldername() ) ;
      }
      catch ( IOException ex2 )
      {
         System.err.println( "Couldn't remove remote folder." ) ;
         return -1 ;
      }
      this.removeRemoteDefinition( remDefPos ) ;
      try
      {
         this.saveRemoteDefs() ;
      }
      catch ( Exception ex )
      {
         ex.printStackTrace() ;
         System.out.println(
             "Couldn't load \"RemoteXMLFile\".Can't remove entry" ) ;
      }

      return 0 ;
   }

   public void saveRemoteDefs()
       throws Exception
   {

      synchronized ( BIGRemoteMenu.this )
      {
         RemoteDefinition.saveRemoteDefsToXML( remoteDefs ,
                                               new File(
             BIGInterface.getInstance().
             getBenchItPath() +
             File.separator +
             "gui" +
             File.separator + "cfg" + File.separator +
             BIGInterface.getInstance().
             getBIGConfigFileParser().stringCheckOut(
                 "remoteXMLFile" ) )
             ) ;
      }

   }

   public int removeRemoteDefinition( RemoteDefinition def )
   {
      for ( int i = 0 ; i < this.remoteDefs.length ; i++ )
      {
         if ( this.remoteDefs[ i ] == def )
         {
            return this.removeRemoteFolder( i ) ;
         }
      }
      return -1 ;
   }

   /**
    * @return RemoteDefinition[] this.remoteDefs
    */
   public RemoteDefinition[] getRemoteDefs()
   {
      return remoteDefs ;
   }

   private void showTypeDialog( RemoteDefinition def )
   {
      if ( BIGInterface.getSystem() == BIGInterface.UNIX_SYSTEM )
      {
         boolean b = true ;
         try
         {
            b = BIGInterface.getInstance().getBIGConfigFileParser().
                boolCheckOut( "showRemoteTypeDialog" ) ;
         }
         catch ( Exception ignored )
         {}
         if ( b )
            JOptionPane.showMessageDialog( this.bgui ,
                                           "Please type the password for " +
                                           def +
                                           " at the console you started BenchIT at." ) ;
      }
   }

   private void showEmptyDialog()
   {
      JOptionPane.showMessageDialog( this.bgui ,
                                     "There are no remotefolders defined, add some." ) ;
   }

   /**
    * opens a dialog, that asks on which remote folder the kernels shall be executed
    * @param kernels BIGStrings the kernels (matmul_double)
    * @param scripts BIGStrings the scripts (COMPILE.SH,RUN.SH,)
    */
   public void startWorkOnAnotherMachine( final BIGStrings kernels ,
                                          final BIGStrings scripts )
   {
      RemoteDefinition[] remDefs = this.getDefsDialog(
          "to start selected kernels at" ) ;
      for ( int j = 0 ; j < remDefs.length ; j++ )
      {
         RemoteDefinition defs = remDefs[ j ] ;
         File tempDir = new File( BIGInterface.getInstance().
                                  getBenchItPath() +
                                  File.separator +
                                  "gui" + File.separator +
                                  "temp" +
                                  File.separator +
                                  defs.getFoldername() ) ;
         tempDir.mkdirs() ;
         File kernelDir = new File( tempDir.getAbsolutePath() +
                                    File.separator + "kernel" ) ;

         kernelDir.mkdirs() ;
         File locDefDir = new File( tempDir.getAbsolutePath() +
                                    File.separator + "LOCALDEFS" ) ;
         locDefDir.mkdirs() ;
         // copy kernels to tempdir
         while ( true )
         {
            if ( ( kernelDir.exists() ) )break ;
         }
         while ( true )
         {
            if ( ( locDefDir.exists() ) )break ;
         }
         //System.err.println("Kernels:"+kernels);
         for ( int i = 0 ; i < kernels.size() ; i++ )
         {
            //BIGKernel k=new BIGKernel(new File());
            String kernelName = kernels.get( i ) ;
            // java.util.Map parent = kernelList ;
            // java.util.Iterator it = parent.keySet().iterator() ;
            // recursivly search for kernel

            BIGKernel k = BIGRemoteMenu.this.bgui.getKernelTree().findKernel(
                kernelName ) ;
            if ( k == null )
            {
               System.err.println(
                   "An error occured while searching kernel " +
                   kernelName ) ;
               return ;
            }
            //BIGKernel k = BIGExecute.findKernel( kernelName ,
            //    parent , it ) ;
            File f3 = new File( kernelDir.getAbsolutePath() +
                                File.separator +
                                k.getRelativePath() ).getParentFile() ;
            f3.mkdirs() ;
            BIGFileHelper.copyToFolder( new File( k.
                                                  getAbsolutePath() ) ,
                                        /*new File(*/
                                        f3
                                        /*)*/, true ) ;
         }
         // copy tools-folder
         BIGFileHelper.copyToFolder(new File(BIGInterface.getInstance().
                                  getBenchItPath() +
                                  File.separator +"tools"),tempDir,true);
         // and benchit.c
         BIGFileHelper.copyToFolder(new File(BIGInterface.getInstance().
                                  getBenchItPath() +
                                  File.separator +"benchit.c"),tempDir,true);
         // and interface.h
         BIGFileHelper.copyToFolder(new File(BIGInterface.getInstance().
        		 				  getBenchItPath() +
        		 				  File.separator +"interface.h"),tempDir,true);

         // generate shellscript

         if ( generatePostProcScript( kernels , scripts , defs ) == -1 )
         {
            return ;
         }
         // if not LOCALDEF-check copy all
         BIGFileHelper.copyToFolder( new File( BIGInterface.
                                               getInstance().
                                               getBenchItPath()
                                               + File.separator + "LOCALDEFS" ) ,
                                     tempDir , true ) ;
         // remove unwanted files and folders
         String unwanted = "" ;
         try
         {
            unwanted = BIGInterface.getInstance().getBIGConfigFileParser().
                stringCheckOut( "dontCopyList" ) ;
         }
         catch ( Exception ex )
         {
            BIGInterface.getInstance().getBIGConfigFileParser().addEntry(
                "dontCopyList",
                "tools/kernel-gen*");
         }
         StringTokenizer tokenizer=new StringTokenizer(unwanted,",");
         // store all files in directory to Vector
         File [] actualFiles=tempDir.listFiles();
         Vector allFiles=new Vector();
         for ( int i = 0 ; i < actualFiles.length ; i++ )
            allFiles.add( actualFiles[ i ] ) ;
         for (int i=0;i<allFiles.size();i++)
         {
            File actual=(File) (allFiles.get(i));
            if (actual.isDirectory())
            {
               actualFiles=actual.listFiles();
               for ( int k = 0 ; k< actualFiles.length ;k++ )
                  allFiles.add( actualFiles[ k ] ) ;
            }
         }
         while (tokenizer.hasMoreTokens())
         {
            // actualFile can contain * and ?
            String actualFilterFileName=tokenizer.nextToken().trim();
            for (int i=0;i<allFiles.size();i++)
            {
               if (!((File)allFiles.get(i)).exists())
               {
                  allFiles.remove(i);
                  i--;
               }
               else
               {
                 if ( isRegEx(
                                actualFilterFileName,
                                ( ( File ) allFiles.get( i ) ).getAbsolutePath().
                                substring( tempDir.getAbsolutePath().length() +
                                           1 )  ) )
                  {
                     BIGFileHelper.remove( ( ( File ) allFiles.get( i ) ) ) ;
                     allFiles.remove( i ) ;
                     i-- ;
                  }
               }
            }

         }
         // }
         scpCopyKernelsAndLOCDEFs( defs , tempDir ) ;
         BIGFileHelper.remove( tempDir ) ;
         startRemoteProc( defs ) ;
      }

   }

   private int startRemoteProc( RemoteDefinition def )
   {
      String shellCmd=def.getShellCommand();
      final Session sess = openSession( def ) ;
      if (sess==null)
      {
         System.err.println( "Couldn't start remote-proc." ) ;
         return -1 ;
      }

      InputStream[] ins = new InputStream[2 ] ;
      try
      {
         ins[ 0 ] = sess.getStderr() ;
         ins[ 1 ] = sess.getStdout() ;
         BIGConsole bcc = new BIGConsole( 400 , 400 , ins )
         {
            public void dispose()
            {
               closeSession( sess ) ;
            }
         } ;
         bcc.setTitle( "Kernel(s) running on " + def.getIP() + "(" +
                       def.getFoldername() + ")" ) ;
         bcc.setVisible( true ) ;
         sess.execCommand( "nohup "+shellCmd+" " +
                           def.getFoldername() +
                           "/remoteproc.sh | tee " + def.getFoldername() +
                           "/output.txt" ) ;

      }
      catch ( IOException ex1 )
      {
         ex1.printStackTrace() ;
         System.err.println( "Couldn't start remote procedure." ) ;
         return -1 ;
      }
      return 0 ;
   }

   private int generatePostProcScript( BIGStrings kernels , BIGStrings scripts ,
                                       RemoteDefinition def )
   {

      String shellCmd=def.getShellCommand();
      String comment = BIGInterface.getInstance().getEntry( "<hostname>" ,
          "BENCHIT_FILENAME_COMMENT" ).getValue().toString() ;
      //-----
      String hostname = this.getHostname( def );
      if (hostname == null)
      {
    	  System.err.println("Couldn't get hostname");
    	  return -1;
      }
      def.setHostname( hostname );
      //-----
      //def.setHostname( this.getHostname( def ) ) ;
      if ( !def.getLOCALDEF().equals( BIGInterface.getInstance().getHost() ) )
      {
         comment = BIGFileHelper.getFileContent( new File( BIGInterface.
             getInstance().getBenchItPath() + File.separator + "LOCALDEFS" +
             File.separator + def.getLOCALDEF() ) ) ;
         if ( comment != null )
            comment = plot.BIGOutputParser.getValue( "BENCHIT_FILENAME_COMMENT" ,
                comment ) ;
      }
      if ((comment==null)||(comment.equals("")))
         comment="0";
      String noParam = "" ;

      BIGStrings script = new BIGStrings() ;
      if ( kernels.size() != scripts.size() )
      {
         System.err.println(
             "Number of kernels unequal to number of scripts." ) ;
         while ( kernels.size() > scripts.size() )
         {
            scripts.add( BIGExecute.DEFAULT_SCRIPT ) ;
         }
      }
      String directory = "${PWD}/" + def.getFoldername() ;
      if ( def.getFoldername().startsWith( "/" ) )
         directory = def.getFoldername() ;
      // header of file
      script.add( "#!/bin/sh" ) ;
      script.add( "echo \"##### starting postproceeding / selected kernels at "+def.getHostname()+" with comment "+comment+"\"" ) ;
      // header of file
      script.add( "PWD=`pwd`" ) ;
      script.add(
          "echo \"##### permissions in bin- and tools-folder will be set to +rwx! ##### \" " ) ;
      script.add( "chmod -R +rwx " + directory + "/bin" ) ;
      script.add( "chmod -R +rwx " + directory + "/tools" ) ;
      script.add( "chmod -R +rwx " + directory +
                  "/COMPILE.SH" ) ;
      script.add( "chmod -R +rwx " + directory + "/RUN.SH" ) ;
      // kernel starts
      for ( int pos = 0 ; pos < kernels.size() ; pos++ )
      {
         String kernelName = kernels.get( pos ) ;
         String scriptName = scripts.get( pos ) ;
// recursivly search for kernel
         BIGKernel k = bgui.getKernelTree().findKernel( kernelName ) ;
         if ( k == null )
         {
            System.err.println( "couldn't find kernel "
                                + kernelName ) ;
            continue ;
         }
         // maybe BENCHIT_FILENAME_COMMENT is defined in PARAMETERS
         File files[] = k.getFiles() ;
         for ( int l = 0 ; l < files.length ; l++ )
         {
            if ( files[ l ].getName().equals( "PARAMETERS" ) )
            {
               String paramComment = plot.BIGOutputParser.getValue(
                   "BENCHIT_FILENAME_COMMENT" ,
                   BIGFileHelper.getFileContent( files[ l ] ) ) ;
               if ( ( paramComment != null ) && ( !paramComment.equals( "" ) ) )
               {
                  comment = paramComment ;
               }
            }
         }

         String cmd = scriptName ;
         {
            script.add(
                "echo \"##### starting \\\"" + k.getName() +
                "\\\"\"" ) ;
            String targetRun=" --target="+def.getLOCALDEF();
            String targetCompile =" --target="+def.getLOCALDEF()+" ";

            try
            {
               if ( BIGInterface.getInstance().getBIGConfigFileParser().
                    boolCheckOut( "settedTargetActiveCompileRemote" ) )
               {
                  targetCompile = " --target=" +
                      BIGInterface.getInstance().
                      getBIGConfigFileParser().
                      stringCheckOut(
                          "settedTargetCompileRemote" ) ;
               }
               if ( BIGInterface.getInstance().getBIGConfigFileParser().
                    boolCheckOut( "settedTargetActiveRunRemote" ) )
               {
                  targetRun = " --target=" +
                      BIGInterface.getInstance().
                      getBIGConfigFileParser().
                      stringCheckOut( "settedTargetRunRemote" ) ;
               }
               if ( BIGInterface.getInstance().getBIGConfigFileParser().
                    boolCheckOut( "runRemoteWithoutParameter" ) )
               {
                  noParam = " --no-parameter-file" ;
               }

            }
            catch ( Exception ex )
            {
               System.err.println(
                   "At least one variable for compiling /" +
                   " running on remote systems was not set" +
                   " within BGUI.cfg. Using defaults." ) ;
            }

            if ( scriptName.equals( BIGExecute.BOTH ) )
            {
               script.add( shellCmd+" " + directory +
                           "/COMPILE.SH" +
                           targetCompile + " " +
                           k.getNameAfterSorting( 0 )
                   ) ;
               String run = shellCmd+" " + directory +
                   "/RUN.SH" ;
               run = run + targetRun + noParam + " " +
                   k.getNameAfterSorting( 0 ) ;
               run = run + "." +
                   comment ;
               run = run + " --output-dir=" + directory + "/output" ;

               script.add( run
                   /*+" >>out.txt 2>>err.txt"*/
                   ) ;
            }
            else
            {
               if ( scriptName.equals( BIGExecute.COMPILE ) )
               {
                  script.add( shellCmd+" " + directory +
                              "/COMPILE.SH" + targetCompile +
                              " " +
                              k.getNameAfterSorting( 0 ) ) ;
                  /*  script.add( "bash ${PWD}/" + def.getFoldername() +
                                "/kernel/" +
                                k.getRelativePath() + "/" +
                                scriptName+comment+targetCompile
                        /*+" >>out.txt 2>>err.txt"*/
                   //  ) ;
               }
               else
               {
                  return this.onlyRunCompilations( def ,
                      k.getNameAfterSorting( 0 ) ) ;
               }
            }
            script.add(
                "echo \"##### finished \\\"" + kernelName +
                "\\\"\"" ) ;
         }
      }
      // write file
      try
      {
         script.saveToFile( BIGInterface.getInstance().getBenchItPath() +
                            File.separator + "gui" +
                            File.separator + "temp" + File.separator +
                            def.getFoldername() +
                            File.separator + "remoteproc.sh" ) ;
      }
      catch ( Exception e )
      {
         System.err.println(
             "BIGExecute: Error during writing of remoteproc.sh\n" + e ) ;
         return -1 ;
      }
      return 0 ;
   }

   private void addRemoteDefinition( RemoteDefinition def )
   {
      RemoteDefinition[] defsNew = new RemoteDefinition[this.remoteDefs.
          length +
          1 ] ;
      for ( int i = 0 ; i < remoteDefs.length ; i++ )
      {
         defsNew[ i ] = remoteDefs[ i ] ;
      }
      defsNew[ remoteDefs.length ] = def ;
      this.remoteDefs = defsNew ;
   }

   private void removeRemoteDefinition( int position )
   {
      RemoteDefinition[] defsNew = new RemoteDefinition[this.remoteDefs.
          length -
          1 ] ;
      for ( int i = 0 ; i < position ; i++ )
      {
         defsNew[ i ] = remoteDefs[ i ] ;
      }
      for ( int i = position ; i < defsNew.length ; i++ )
      {
         defsNew[ i ] = remoteDefs[ i + 1 ] ;
      }
      this.remoteDefs = defsNew ;
   }

   public int onlyRunCompilations( RemoteDefinition def , String match )
   {
      if ( debug ) System.err.println( "--------------onlyRun..()" ) ;
      if ( debug ) System.err.println( "Def:" + def ) ;
      //Session sess=null;
      String output = new String() ;
      BIGConsole bcc = null ;
      Session sess = null ;
      try
      {
         sess = this.openSession( def ) ;
         if ( sess == null )
         {
            System.err.println( "Couldn't run remote executables." ) ;
            return -1 ;
         }

         InputStream[] is = new InputStream[2 ] ;
         is[ 1 ] = sess.getStderr() ;
         is[ 0 ] = sess.getStdout() ;

         bcc = new BIGConsole( 1 , 1 , is ) ;
         //  "find " + def.getFoldername() +
         //                         "/bin/ -name *.*.*"
         // sess.getStdin().write(("ls " + def.getFoldername() + "/bin/*\n").getBytes());
          sess.execCommand( "ls " + def.getFoldername() +
                            "/bin/* && echo \"FINISHED\"" ) ;
      }
      catch ( IOException ex3 )
      {
         ex3.printStackTrace() ;
         System.err.println( "Couldn't check for executables" ) ;
         return -1 ;
      }
      while ( bcc.getText().indexOf( "\nFINISHED" ) == -1 ) ;
      closeSession( sess ) ;
      output = bcc.getText() ;

      StringTokenizer st = null ;
      st = new StringTokenizer( output , "\n" ) ;
      Vector kernels = new Vector() ;
      // int nextToWrite = 0 ;
      String directory = def.getFoldername() + "/bin/" ;
      // first the executables (c,f)
      String temp ;
      //  System.err.println(st.countTokens());
      while ( st.hasMoreElements() )
      {
         temp = ( String ) st.nextElement() ;
         if ( debug ) System.err.println( "Element:" + temp ) ;
         if ( temp.startsWith( directory ) )
         {
            if ( debug ) System.err.println( "starts with dir" ) ;
            kernels.add( temp ) ;
            if ( ( ( String ) kernels.lastElement() ).endsWith( ":" ) &&
                 ( temp.indexOf( match ) != -1 ) )
            {
               if ( debug ) System.err.println( "is dir" ) ;
               kernels.add( temp.substring( 0 ,
                                            temp.length() - 1 ) + "/RUN.SH" ) ;
               // nextToWrite++ ;
               // now the directories(java)
               while ( st.hasMoreElements() )
               {
                  temp = ( String ) st.nextElement() ;
                  if ( temp.startsWith( directory ) && temp.endsWith( ":" ) &&
                       ( temp.indexOf( match ) != -1 ) )
                  {
                     kernels.add( temp.substring( 0 ,
                                                  temp.length() - 1 ) +
                                  "/RUN.SH" ) ;

                  }

               }

            }
            //  nextToWrite++;

         }
         else
         {
            if ( ( temp.indexOf( "RUN.SH" ) != -1 ) &&
                 ( temp.indexOf( match ) != -1 ) )
            {
               kernels.add( temp.substring( 0 , temp.lastIndexOf( "/" ) + 1 ) +
                            "RUN.SH " +
                            temp.substring( temp.lastIndexOf( "/" ) + 1 ,
                                            temp.length() ) ) ;
               //  nextToWrite++ ;
            }
         }
      }
      if ( kernels.size() < 1 )
      {
         System.out.println(
             "There are no compiled kernels within the binary folder" ) ;
         return -1 ;
      }
      //while ((numberOfKernels<kernels.length)&&(kernels[numberOfKernels]!=null))
      //   numberOfKernels++;
      String[] allKernels = new String[kernels.size() ] ;
      for ( int i = 0 ; i < allKernels.length ; i++ )
         allKernels[ i ] = ( String ) kernels.get( i ) ;

      JList selectionList = new JList( allKernels ) ;
      //  selectionList.setVisibleRowCount(10);
      selectionList.setBorder( BorderFactory.createTitledBorder(
          "available compilations" ) ) ;
      JTextField jt = new JTextField() ;
      jt.setBorder( BorderFactory.createTitledBorder(
          "regular expression (includes *'s and ?'s)" ) ) ;
      Object[] o = new Object[3 ] ;
      o[ 0 ] = "Found kernels:" ;
      o[ 1 ] = selectionList ;
      o[ 2 ] = jt ;
      int value = JOptionPane.showConfirmDialog( null , o ,
                                                 "select remote compilations" ,
                                                 JOptionPane.OK_CANCEL_OPTION ,
                                                 JOptionPane.QUESTION_MESSAGE ) ;
      if ( value == JOptionPane.CANCEL_OPTION )
         return -1 ;
      else
      {
         String add = "" ;
         try
         {
            add=" --target="+def.getLOCALDEF();
            if ( BIGInterface.getInstance().getBIGConfigFileParser().
                 boolCheckOut( "settedTargetActiveRunRemote" ) )
            {
               add = " --target=" +
                   BIGInterface.getInstance().getBIGConfigFileParser().
                   stringCheckOut( "settedTargetRunRemote" ) ;
            }
            if ( BIGInterface.getInstance().getBIGConfigFileParser().
                 boolCheckOut( "runRemoteWithoutParameter" ) )
            {
               add = add + " --no-parameter-file" ;
            }

         }
         catch ( Exception e )
         {
            System.err.println(
                "At least one variable in BGUI.cfg not set. Using defaults." ) ;
         }
         String[] returnStrings ;
         // regular expression
         if ( ( jt.getText() != null ) && ( !jt.getText().equals( "" ) ) )
         {
            Vector vOfEexecs = new Vector() ;
            // getting matches in JList for regex
            for ( int i = 0 ; i < allKernels.length ; i++ )
            {
               if ( isRegEx( jt.getText() , allKernels[ i ] ) )
               {
                  vOfEexecs.add( allKernels[ i ] ) ;
               }
            }
            // turn Objects into Strings
            o = vOfEexecs.toArray() ;
            returnStrings = new String[o.length ] ;
            if ( returnStrings.length == 0 )
               return -1 ;
            for ( int i = 0 ; i < o.length ; i++ )
            {
               returnStrings[ i ] = ( String ) o[ i ] ;
               String kernelDir = returnStrings[ i ].substring( 0 ) ;
               while ( kernelDir.indexOf( "." ) > -1 )
                  kernelDir = kernelDir.substring( 0 ,
                      kernelDir.indexOf( "." ) ) +
                      "/" +
                      kernelDir.substring( kernelDir.indexOf( "." ) +
                                           1 ,
                                           kernelDir.length() ) ;
               kernelDir = kernelDir.substring( 0 ,
                                                kernelDir.lastIndexOf( "/" ) ) ;
               while ( kernelDir.charAt( 0 ) == '/' )
                  kernelDir = kernelDir.substring( 1 ) ;
               returnStrings[ i ] = returnStrings[ i ].substring( def.
                   getFoldername().length() + 5 ) ;
               returnStrings[ i ] = def.getFoldername() + "/RUN.SH" +
                   add + " " + returnStrings[ i ] +
                   " --output-dir=${PWD}/" +
                   def.getFoldername() + "/output" ;

            }
         }
         else
         // selected in list
         {
            // turn Objects into Strings
            returnStrings = new String[selectionList.
                getSelectedValues().length ] ;
            for ( int i = 0 ; i < returnStrings.length ; i++ )
            {
               returnStrings[ i ] = ( String ) ( selectionList.
                                                 getSelectedValues()[ i ] ) ;
               returnStrings[ i ] = returnStrings[ i ].substring( def.
                   getFoldername().length() + 5 ) ;

               returnStrings[ i ] = def.getFoldername() + "/RUN.SH" +
                   add + " " + returnStrings[ i ] +
                   " --output-dir=${PWD}/" +
                   def.getFoldername() + "/output" ;
            }
            if ( returnStrings.length == 0 )return -1 ;
         }
         File tempDir = new File( BIGInterface.getInstance().
                                  getBenchItPath() +
                                  File.separator +
                                  "gui" + File.separator +
                                  "temp" +
                                  File.separator +
                                  def.getFoldername() ) ;
         tempDir.mkdirs() ;
         StringBuffer fileContent = new StringBuffer() ;
         fileContent.append( "#!/bin/sh\n" ) ;
         fileContent.append( "PWD=`pwd`\n" ) ;
         fileContent.append( "echo \"##### permissions in bin- and tools-folder will be set to +rwx! ##### \" \n" ) ;
         fileContent.append( "chmod -R +rwx ${PWD}/" + def.getFoldername() +
                             "/bin\n" ) ;
         fileContent.append( "chmod -R +rwx ${PWD}/" + def.getFoldername() +
                             "/tools\n" ) ;
         fileContent.append( "chmod -R +rwx ${PWD}/" + def.getFoldername() +
                             "/COMPILE.SH\n" ) ;
         fileContent.append( "chmod -R +rwx ${PWD}/" + def.getFoldername() +
                             "/RUN.SH\n" ) ;
         fileContent.append( "echo \"##### starting selected kernels\"\n" ) ;

         for ( int i = 0 ; i < returnStrings.length ; i++ )
         {
            fileContent.append( returnStrings[ i ] + "\n" ) ;
         }
         fileContent.append( "echo \"##### finished selected kernels\"\n" ) ;

         // generate shellscript
         system.BIGFileHelper.saveToFile( fileContent.toString() ,
                                          new
                                          File( ( BIGInterface.getInstance().
                                                  getBenchItPath() +
                                                  File.separator + "gui" +
                                                  File.separator + "temp" +
                                                  File.separator +
                                                  def.getFoldername() +
                                                  File.separator +
                                                  "remoteproc.sh" )
                                          ) ) ;
         BIGFileHelper.copyToFolder( new File( BIGInterface.getInstance().
                                               getBenchItPath() +
                                               File.separator + "LOCALDEFS" )
                                     , new File( BIGInterface.getInstance().
                                                 getBenchItPath() +
                                                 File.separator + "gui" +
                                                 File.separator + "temp" ) , false

             ) ;
         int retInt = scpCopyKernelsAndLOCDEFs( def , tempDir ) ;
         if ( retInt != 0 )return retInt ;
         //BIGFileHelper.remove( tempDir ) ;
         return 0 ; //startRemoteProc( def ) ;
      }
   }

   /**
    * opens a dialog to select some executables which will be copied
    * to a remote folder
    * @param def RemoteDefinition remotefolder to copy to
    * @return int errorReturn by ssh/scp commands
    */
   public int copyExecutables( RemoteDefinition def )
   {
      File f = new File( BIGInterface.getInstance().getBenchItPath() +
                         File.separator + "bin" + File.separator ) ;
      File[] files = f.listFiles( new FileFilter()
      {
         public boolean accept( File fileInBinPath )
         {
            String temp = fileInBinPath.getName() ;
            int numberOfDots = 0 ;
            while ( temp.indexOf( "." ) > -1 )
            {
               numberOfDots++ ;
               temp = temp.substring( temp.indexOf( "." ) + 1 , temp.length() ) ;
            }
            if ( numberOfDots > 5 )
               return true ;

            // if ( fileInBinPath.isDirectory() )return true ;
            return false ;
         }
      } ) ;
      if ( files.length == 0 )
      {
         System.out.println( "No executables were found" ) ;
         return -1 ;
      }
      JList selectionList = new JList( files ) ;
      // just show the filename not the path
      selectionList.setCellRenderer( new DefaultListCellRenderer()
      {
         public Component getListCellRendererComponent(
             JList list ,
             Object value ,
             int index ,
             boolean isSelected ,
             boolean cellHasFocus )
         {
            Component c = super.getListCellRendererComponent( list ,
                value ,
                index ,
                isSelected ,
                cellHasFocus
                ) ;
            this.setText( ( ( File ) value ).getName() ) ;
            return c ;
         }
      }
      ) ;
      selectionList.setVisibleRowCount( 10 ) ;
      selectionList.setBorder( BorderFactory.createTitledBorder(
          "available compilations" ) ) ;
      JTextField jt = new JTextField() ;
      jt.setBorder( BorderFactory.createTitledBorder(
          "regular expression (includes *'s and ?'s)" ) ) ;
      Object[] o = new Object[3 ] ;
      o[ 0 ] = "Found executables:" ;
      o[ 1 ] = selectionList ;
      o[ 2 ] = jt ;
      int value = JOptionPane.showConfirmDialog( null , o ,
                                                 "select executables to copy" ,
                                                 JOptionPane.OK_CANCEL_OPTION ,
                                                 JOptionPane.QUESTION_MESSAGE ) ;
      if ( value == JOptionPane.CANCEL_OPTION )
         return -1 ;
      else
      {

         File[] returnStrings ;
         // regular expression
         if ( ( jt.getText() != null ) && ( !jt.getText().equals( "" ) ) )
         {
            Vector vOfEexecs = new Vector() ;
            // getting matches in JList for regex
            for ( int i = 0 ; i < files.length ; i++ )
            {
               if ( isRegEx( jt.getText() , files[ i ].getName() ) )
               {
                  vOfEexecs.add( files[ i ] ) ;
               }
            }
            // turn Objects into Strings
            o = vOfEexecs.toArray() ;
            returnStrings = new File[o.length ] ;
            for ( int i = 0 ; i < o.length ; i++ )
            {
               returnStrings[ i ] = ( File ) o[ i ] ;
            }
         }
         else
         // selected in list
         {
            // turn Objects into Strings
            returnStrings = new File[selectionList.
                getSelectedValues().length ] ;
            for ( int i = 0 ; i < returnStrings.length ; i++ )
               returnStrings[ i ] = ( File ) ( selectionList.
                                               getSelectedValues()[ i ] ) ;
            if ( returnStrings.length == 0 )return -1 ;
         }
         File tempDir = new File( BIGInterface.getInstance().
                                  getBenchItPath() +
                                  File.separator +
                                  "gui" + File.separator +
                                  "temp" +
                                  File.separator +
                                  def.getFoldername() ) ;
         File binDir = new File( tempDir.getPath() + File.separator + "bin" ) ;
         binDir.mkdirs() ;
         for ( int i = 0 ; i < returnStrings.length ; i++ )
            BIGFileHelper.copyToFolder( returnStrings[ i ] , binDir , true ) ;
         return scpCopyKernelsAndLOCDEFs( def , tempDir ) ;
      }
   }

   /**
    * This thread consumes output from the remote server and displays it in
    * the terminal window.
    *
    */
   class RemoteConsumer
       extends Thread
   {
      private boolean run = false ;
      StringBuffer sb = new StringBuffer() ;
      InputStream in = null ;
      RemoteConsumer( InputStream in )
      {
         this.in = in ;
      }

      private void addText( byte[] data , int len )
      {
         for ( int i = 0 ; i < len ; i++ )
         {
            char c = ( char ) ( data[ i ] & 0xff ) ;
            if ( data[ i ] != 0 )
            {
               sb.append( c ) ;
            }
         }
      }

      public void run()
      {
         run = true ;
         byte[] buff = new byte[8192 ] ;

         try
         {
            while ( run )
            {
               int len = in.read( buff ) ;
               if ( len == -1 )
                  return ;
               addText( buff , len ) ;
            }
         }
         catch ( Exception e )
         {
            e.printStackTrace() ;
         }
      }

      public String getText()
      {
         return sb.toString() ;
      }

      public void stopIt()
      {
         run = false ;
      }
   }

   private Session openSession( RemoteDefinition def )
   {
      boolean emptyXSA=false;
      try
      {
         emptyXSA = system.BIGInterface.getInstance().getBIGConfigFileParser().
             boolCheckOut( "emptyXSA" ) ;
      }
      catch ( Exception ex4 )
      {
         System.out.println("The flag \"emptyXSA\" wasn't found in file gui/cfg/BGUI.cfg\n"+
         "If you have exchanged your keys with an empty passphrase,\n"+
         "Close the GUI and add the setting \"emptyXSA=1\" to the file.\n"+
         "When the GUI is restarted, the dialog asking for a password (and this text)"+
         " won't appear again.");
      }

      Connection conn = new Connection( def.getIP() ) ;
      Session sess = null ;
      String password = def.getPassword() ;
      try
      {
         boolean isAuthenticated = false ;
         if ( ( new File( this.idDSAPath ) ).exists() )
            isAuthenticated = conn.authenticateWithPublicKey( def.
                getUsername() ,
                new File( this.idDSAPath ) , "password" ) ;
         if ( !isAuthenticated )
         {
            if ( ( new File( this.idRSAPath ) ).exists() )
               isAuthenticated = conn.authenticateWithPublicKey( def.
                   getUsername() ,
                   new File( this.idRSAPath ) , "password" ) ;
         }

         if ( isAuthenticated )
         {
            return conn.openSession() ;
         }
      }
      catch ( Exception ex )
      {
      }

      try
      {
         /*
          *
          * CONNECT AND VERIFY SERVER HOST KEY (with callback)
          *
          */

         String[] hostkeyAlgos = database.
             getPreferredServerHostkeyAlgorithmOrder(
                 def.getIP() ) ;

         if ( hostkeyAlgos != null )
            conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

         conn.connect( new SimpleVerifier(database) ) ;

         /*
          *
          * AUTHENTICATION PHASE
          *
          */

         boolean enableKeyboardInteractive = true ;
         boolean enableDSA = true ;
         boolean enableRSA = true ;

         String lastError = null ;

         while ( true )
         {
            if ( ( enableDSA || enableRSA ) &&
                 conn.isAuthMethodAvailable( def.getUsername() , "publickey" ) )
            {
               if ( enableDSA )
               {
                  File key = new File( idDSAPath ) ;

                  if ( key.exists() )
                  {
                     if ( password == null )
                     {

                        boolean res = false;
                       if (emptyXSA)
                       {
                          res = conn.authenticateWithPublicKey(
                              def.getUsername() , key , "" ) ;

                          if ( res == true )
                          {
                             if ( debug ) System.err.println( "DSA_1" ) ;
                             break ;
                          }
                          else
                          {

                             conn.close();
                             conn = new Connection( def.getIP() ) ;

                             if ( hostkeyAlgos != null )
                                conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

                             conn.connect( new SimpleVerifier( database ) ) ;


                          }
                       }
                       else
                       {
                          EnterSomethingDialog esd = new EnterSomethingDialog(
                              loginFrame , "DSA Authentication for " +
                              def.getUsername() + "@" + def.getIP() ,
                              new String[]
                              {lastError ,
                              "Enter DSA private key password:"} , true ) ;
                          esd.setVisible( true ) ;
                          res = conn.authenticateWithPublicKey(
                              def.getUsername() , key , esd.answer ) ;

                          if ( res == true )
                          {
                             password = esd.answer ;
                             if ( debug ) System.err.println( "DSA" ) ;
                             break ;
                          }
                           conn.close();
                           conn=new Connection(def.getIP());

                             if ( hostkeyAlgos != null )
                                conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

                             conn.connect( new SimpleVerifier( database ) ) ;


                          lastError = "DSA authentication failed." ;
                       }
                     }
                     else
                     {

                        boolean res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , password ) ;

                        if ( res == true )
                        {
                           if ( debug ) System.err.println( "DSA" ) ;
                           break ;
                        }
                        lastError = "DSA authentication failed." ;

                     }
                  }

                  enableDSA = false ; // do not try again
               }

               if ( enableRSA )
               {
                  File key = new File( idRSAPath ) ;

                  if ( key.exists() )
                  {
                     if ( password == null )
                     {
                        boolean res = false;
                       if (emptyXSA)
                       {
                          res = conn.authenticateWithPublicKey(
                              def.getUsername() , key , "" ) ;

                          if ( res == true )
                          {
                             if ( debug ) System.err.println( "RSA_1" ) ;
                             break ;
                          }
                          else
                          {

                             conn.close();
                             conn=new Connection(def.getIP());

                             if ( hostkeyAlgos != null )
                                conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

                             conn.connect( new SimpleVerifier( database ) ) ;


                          }
                       }
                       else
                       {
                          EnterSomethingDialog esd = new
                              EnterSomethingDialog(
                                  loginFrame , "RSA Authentication for " +
                                  def.getUsername() + "@" + def.getIP() ,
                                  new String[]
                                  {lastError ,
                                  "Enter RSA private key password:"} , true ) ;
                          esd.setVisible( true ) ;
                          try
                          {
                             res = conn.authenticateWithPublicKey(
                                 def.getUsername() , key , esd.answer ) ;
                          }
                          catch ( IOException ex5 )
                          {
                             System.out.println(
                                 "RSA auth. failed. Try other..." ) ;
                             enableRSA = false ;
                             conn.close() ;
                             conn = new Connection( def.getIP() ) ;
                             if ( hostkeyAlgos != null )
                                conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

                             conn.connect( new SimpleVerifier( database ) ) ;

                             continue ;
                          }

                          if ( res == true )
                          {
                             password = esd.answer ;
                             if ( debug ) System.err.println( "RSA" ) ;
                             break ;
                          }
                            conn.close();
                            conn=new Connection(def.getIP());

                            if ( hostkeyAlgos != null )
                               conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

                            conn.connect( new SimpleVerifier( database ) ) ;


                          lastError = "RSA authentication failed." ;
                       }
                     }
                     else
                     {

                        boolean res = false;
                     try
                     {
                        res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , password ) ;
                     }
                     catch ( IOException ex6 )
                     {
                           System.out.println("RSA auth. failed. Try other...");
                           enableRSA=false;
                           conn.close();
                           conn=new Connection(def.getIP());
                           if ( hostkeyAlgos != null )
                              conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

                           conn.connect( new SimpleVerifier(database) ) ;

                           continue;

                     }

                        if ( res == true )
                        {
                           if ( debug ) System.err.println( "RSA" ) ;
                           break ;
                        }
                        lastError = "RSA authentication failed." ;

                     }
                  }
                  enableRSA = false ; // do not try again
               }

               continue ;
            }

            if ( enableKeyboardInteractive &&
                 conn.isAuthMethodAvailable( def.getUsername() ,
                                             "keyboard-interactive" ) )
            {
               InteractiveLogic il = new InteractiveLogic( lastError , def ) ;

               boolean res = false ;
               try
               {
                  res = conn.authenticateWithKeyboardInteractive(
                      def.getUsername() , il ) ;
               }
               catch ( IOException ex1 )
               {
                   if (debug)ex1.printStackTrace();
                   JOptionPane.showMessageDialog(null,"Keyboard Authetnification failed.");
                   break;
               }

               if ( res == true )
               {
                  if ( debug ) System.err.println( "Keyboard" ) ;
                  break ;
               }
               else
               {
                  password = null ;
               }

               if ( il.getPromptCount() == 0 )
               {
                  // aha. the server announced that it supports "keyboard-interactive", but when
                  // we asked for it, it just denied the request without sending us any prompt.
                  // That happens with some server versions/configurations.
                  // We just disable the "keyboard-interactive" method and notify the user.

                  lastError = "Keyboard-interactive does not work." ;

                  enableKeyboardInteractive = false ; // do not try again
               }
               else
               {
                  lastError = "Keyboard-interactive auth failed." ; // try again, if possible
                  enableKeyboardInteractive = false ;
               }

               continue ;
            }

            if ( conn.isAuthMethodAvailable( def.getUsername() , "password" ) )
            {
               if ( password == null )
               {
                  final EnterSomethingDialog esd = new
                      EnterSomethingDialog(
                          loginFrame ,
                          "Password Authentication for " +
                          def.getUsername() + "@" + def.getIP() ,
                          new String[]
                          {lastError ,
                          "Enter password for " + def.getUsername() +
                          "@" + def.getIP()} , true ) ;
                  esd.setVisible( true ) ;

                  if ( esd.answer == null )
                     throw new IOException( "Login aborted by user" ) ;
                  password = esd.answer ;
                  boolean res = false ;
                  try
                  {
                     res = conn.authenticateWithPassword( def.
                         getUsername() ,
                         esd.answer ) ;
                  }
                  catch ( IOException ex2 )
                  {
                     lastError = "Password authentication failed." ;

                  }

                  if ( res == true )
                  {
                     if ( debug ) System.err.println( "PWD" ) ;
                     break ;
                  }
                  else
                     password = null ;
               }
               else
               {

                  boolean res = false ;
                  try
                  {
                     res = conn.authenticateWithPassword( def.
                         getUsername() ,
                         password ) ;
                  }
                  catch ( IOException ex3 )
                  {

                     lastError = "Password authentication failed." ; // try again, if possible
                  }

                  if ( res == true )
                  {
                     if ( debug ) System.err.println( "PWD" ) ;
                     break ;
                  }

               }

               lastError = "Password authentication failed." ; // try again, if possible

               continue ;
            }

            throw new IOException(
                "No supported authentication methods available." ) ;
         }
         sess = conn.openSession() ;
         this.conns.put( sess , conn ) ;

         if ( password != null )
            def.setPassword( password ) ;

      }
      catch ( Exception exc )
      {
         exc.printStackTrace() ;
         System.err.println( "Couldn't connect to " + def.getIP() ) ;
      }
      return sess ;
   }

   private HashMap conns = new HashMap() ;

   private void closeSession( Object o )
   {
      if ( o instanceof Session )
         ( ( Session ) o ).close() ;
      if ( ( ( Connection ) conns.get( o ) ) != null )
      {
         ( ( Connection ) conns.get( o ) ).close() ;
         conns.remove( o ) ;
      }
   }

   private SCPClient openSCPClient( RemoteDefinition def )
   {
      Connection conn = new Connection( def.getIP() ) ;
      SCPClient sess = null ;
      String password = def.getPassword() ;
      try
      {
         boolean isAuthenticated = false ;
         if ( ( new File( this.idDSAPath ) ).exists() )
            isAuthenticated = conn.authenticateWithPublicKey( def.
                getUsername() ,
                new File( this.idDSAPath ) , "password" ) ;
         if ( !isAuthenticated )
         {
            if ( ( new File( this.idRSAPath ) ).exists() )
               isAuthenticated = conn.authenticateWithPublicKey( def.
                   getUsername() ,
                   new File( this.idRSAPath ) , "password" ) ;
         }

         if ( isAuthenticated )
         {
            return conn.createSCPClient() ;
         }
      }
      catch ( Exception ex )
      {
      }

      try
      {
         /*
          *
          * CONNECT AND VERIFY SERVER HOST KEY (with callback)
          *
          */

         String[] hostkeyAlgos = database.
             getPreferredServerHostkeyAlgorithmOrder(
                 def.getIP() ) ;

         if ( hostkeyAlgos != null )
            conn.setServerHostKeyAlgorithms( hostkeyAlgos ) ;

         conn.connect( new SimpleVerifier(database) ) ;

         /*
          *
          * AUTHENTICATION PHASE
          *
          */

         boolean enableKeyboardInteractive = true ;
         boolean enableDSA = true ;
         boolean enableRSA = true ;

         String lastError = null ;

         while ( true )
         {
            if ( ( enableDSA || enableRSA ) &&
                 conn.isAuthMethodAvailable( def.getUsername() , "publickey" ) )
            {
               if ( enableDSA )
               {
                  File key = new File( idDSAPath ) ;

                  if ( key.exists() )
                  {
                     if ( password == null )
                     {

                        boolean res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , "" ) ;

                        if ( res == true )
                        {
                           if ( debug ) System.err.println( "DSA_1" ) ;
                           break ;
                        }
                        EnterSomethingDialog esd = new EnterSomethingDialog(
                            loginFrame , "DSA Authentication for " +
                            def.getUsername() + "@" + def.getIP() ,
                            new String[]
                            {lastError ,
                            "Enter DSA private key password:"} , true ) ;
                        esd.setVisible( true ) ;
                        res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , esd.answer ) ;

                        if ( res == true )
                        {
                           password = esd.answer ;
                           if ( debug ) System.err.println( "DSA" ) ;
                           break ;
                        }
                        lastError = "DSA authentication failed." ;
                     }
                     else
                     {

                        boolean res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , password ) ;

                        if ( res == true )
                        {
                           if ( debug ) System.err.println( "DSA" ) ;
                           break ;
                        }
                        lastError = "DSA authentication failed." ;

                     }
                  }

                  enableDSA = false ; // do not try again
               }

               if ( enableRSA )
               {
                  File key = new File( idRSAPath ) ;

                  if ( key.exists() )
                  {
                     if ( password == null )
                     {
                        boolean res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , "" ) ;
                        if ( res == true )
                        {
                           if ( debug ) System.err.println( "RSA_1" ) ;
                           break ;
                        }
                        EnterSomethingDialog esd = new
                            EnterSomethingDialog(
                                loginFrame , "RSA Authentication for " +
                                def.getUsername() + "@" + def.getIP() ,
                                new String[]
                                {lastError ,
                                "Enter RSA private key password:"} , true ) ;
                        esd.setVisible( true ) ;

                        res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , esd.answer ) ;

                        if ( res == true )
                        {
                           password = esd.answer ;
                           if ( debug ) System.err.println( "RSA" ) ;
                           break ;
                        }
                        lastError = "RSA authentication failed." ;
                     }
                     else
                     {

                        boolean res = conn.authenticateWithPublicKey(
                            def.getUsername() , key , password ) ;

                        if ( res == true )
                        {
                           if ( debug ) System.err.println( "RSA" ) ;
                           break ;
                        }
                        lastError = "RSA authentication failed." ;

                     }
                  }
                  enableRSA = false ; // do not try again
               }

               continue ;
            }

            if ( enableKeyboardInteractive &&
                 conn.isAuthMethodAvailable( def.getUsername() ,
                                             "keyboard-interactive" ) )
            {
               InteractiveLogic il = new InteractiveLogic( lastError , def ) ;

               boolean res = false ;
               try
               {
                  res = conn.authenticateWithKeyboardInteractive(
                      def.getUsername() , il ) ;
               }
               catch ( IOException ex1 )
               {
               }

               if ( res == true )
               {
                  if ( debug ) System.err.println( "Keyboard" ) ;
                  break ;
               }
               else
               {
                  password = null ;
               }

               if ( il.getPromptCount() == 0 )
               {
                  // aha. the server announced that it supports "keyboard-interactive", but when
                  // we asked for it, it just denied the request without sending us any prompt.
                  // That happens with some server versions/configurations.
                  // We just disable the "keyboard-interactive" method and notify the user.

                  lastError = "Keyboard-interactive does not work." ;

                  enableKeyboardInteractive = false ; // do not try again
               }
               else
               {
                  lastError = "Keyboard-interactive auth failed." ; // try again, if possible
               }

               continue ;
            }

            if ( conn.isAuthMethodAvailable( def.getUsername() , "password" ) )
            {
               if ( password == null )
               {
                  final EnterSomethingDialog esd = new
                      EnterSomethingDialog(
                          loginFrame ,
                          "Password Authentication for " +
                          def.getUsername() + "@" + def.getIP() ,
                          new String[]
                          {lastError ,
                          "Enter password for " + def.getUsername() +
                          "@" + def.getIP()} , true ) ;
                  esd.setVisible( true ) ;

                  if ( esd.answer == null )
                     throw new IOException( "Login aborted by user" ) ;
                  password = esd.answer ;
                  boolean res = false ;
                  try
                  {
                     res = conn.authenticateWithPassword( def.
                         getUsername() ,
                         esd.answer ) ;
                  }
                  catch ( IOException ex2 )
                  {
                     lastError = "Password authentication failed." ;

                  }

                  if ( res == true )
                  {
                     if ( debug ) System.err.println( "PWD" ) ;
                     break ;
                  }
                  else
                     password = null ;
               }
               else
               {

                  boolean res = false ;
                  try
                  {
                     res = conn.authenticateWithPassword( def.
                         getUsername() ,
                         password ) ;
                  }
                  catch ( IOException ex3 )
                  {

                     lastError = "Password authentication failed." ; // try again, if possible
                  }

                  if ( res == true )
                  {
                     if ( debug ) System.err.println( "PWD" ) ;
                     break ;
                  }

               }

               lastError = "Password authentication failed." ; // try again, if possible

               continue ;
            }

            throw new IOException(
                "No supported authentication methods available." ) ;
         }
         sess = conn.createSCPClient() ;
         this.conns.put( sess , conn ) ;
      }
      catch ( Exception exc )
      {
         exc.printStackTrace() ;
         System.err.println( "Couldn't connect to " + def.getIP() ) ;
      }
      if ( password != null )
         def.setPassword( password ) ;
      return sess ;
   }

   /**
    * checks for sth. like regular expressions but only supports *'s and ?'s
    * @param regEx String the pseudo-regular-expression
    * @param check String the sentence to check
    * @return boolean is check expressed by regEx?
    */
   public static boolean isRegEx( String regEx , String check )
   {
      if ( regEx.length() > check.length() )
         return false ;
      if ( ( regEx.length() == 0 ) && ( check.length() == 0 ) )
         return true ;
      if ( regEx.length() == 0 )return false ;
      if ( check.length() == 0 )return false ;
      int i = 0 ;
      {
         // ?s
         if ( regEx.charAt( i ) == '?' )
         {
            return isRegEx( regEx.substring( 1 , regEx.length() ) ,
                            check.substring( 1 , check.length() ) ) ;
         }
         else
         // *s
         if ( regEx.charAt( i ) == '*' )
         {
            // if the last char is a *
            if ( i == regEx.length() - 1 )
            {
               return true ;
            }
            boolean retBool = false ;
            for ( int j = 0 ; j < check.length() ; j++ )
            {
               retBool = ( retBool ||
                           isRegEx( regEx.substring( 1 , regEx.length() ) ,
                                    check.substring( j , check.length() ) ) ) ;
            }
            return retBool ;
         }
         else
         // other characters
         {
            if ( regEx.charAt( i ) == check.charAt( i ) )
            {
               return isRegEx( regEx.substring( 1 , regEx.length() ) ,
                               check.substring( 1 , check.length() ) ) ;
            }
            else return false ;
         }
      }
   }

   /**
    * get the hostname of a remotedefinition
    * @param def RemoteDefinition
    * @return String the result of the commandline call hostname
    */
   public String getHostname( RemoteDefinition def )
   {
	   // had been searched for. so no need to look twice
	   if (def.getHostname()!=null)
         return def.getHostname();
	   // if the remote definition has a pinned LOCALDEF
	   if ( ( def.getHostname() == null ) && ( def.getLOCALDEF() != null ) )
	   {
		   System.out.println( "Don't look for hostname, use " + def.getLOCALDEF() +
                             " as LOCALDEF" ) ;
		   return def.getLOCALDEF() ;
	   }
       String retString=null;
       
       StreamGobbler[] in = new StreamGobbler[2 ] ;
	   Session session = openSession( def ) ;
	   
	   if ( session != null )
	   {
		   String ret="";
		   try
		   {
			   in[ 0 ] =new ch.ethz.ssh2.StreamGobbler(session.getStderr());
			   in[ 1 ] =new ch.ethz.ssh2.StreamGobbler(session.getStdout());
	           session.execCommand( "hostname && echo \"FINISHED\"\n");
	           System.out.println("Opening ssh-session to "+def+" to get the hostname.");
		   	}
		   	catch ( IOException ex )
	        {
		   		ex.printStackTrace() ;
		   		return null ;
	        }
	        try
	        {
	            byte[] buffer = new byte[8192 ] ;
	            int nums = in[ 1 ].read( buffer ) ;
	            while ( nums > -1 )
	            {
	               ret = ret + new String( buffer , 0 , nums ) ;
	               nums = in[ 1 ].read( buffer ) ;
	               if (ret.endsWith("FINISHED"))
	                  break;
	            }
	        }
	        catch ( IOException ex1 )
	        {
	            ex1.printStackTrace();
	        }
	        closeSession( session ) ;
	        try
	        {
	            retString = ret.substring(0,ret.indexOf("\nFINISHED"));
	        }catch(Exception e)
	        {
	            System.out.println("Getting hostname failed.");
	        }
	   }
	   else
	   {
		   System.err.println( "Couldn't open Session" ) ;
	   }
       if ( (retString != null) && (retString.lastIndexOf("\n")>0) )
       {
    	   retString = retString.substring( retString.lastIndexOf( "\n" ) + 1 ) ;
       }
       return retString ;
  }
   
   /**
    * get the hostname of a remotedefinition
    * @param def RemoteDefinition
    * @return String the result of the commandline call hostname
    */
   public String getHostnameOldVersion( RemoteDefinition def )
   {
	   // had been searched for. so no need to look twice
	   if (def.getHostname()!=null)
         return def.getHostname();
	   // if the remote definition has a pinned LOCALDEF
	   if ( ( def.getHostname() == null ) && ( def.getLOCALDEF() != null ) )
	   {
		   System.out.println( "Don't look for hostname, use " + def.getLOCALDEF() +
                             " as LOCALDEF" ) ;
		   return def.getLOCALDEF() ;
	   }
       boolean done = false;
       String retString=null;
       while (!done)
       {
    	   StreamGobbler[] in = new StreamGobbler[2 ] ;
    	   Session s = openSession( def ) ;
    	   if ( s != null )
    	   {
    		   BIGConsole bcc = null ;
    		   String ret="";
    		   try
    		   {
    			   in[ 0 ] =new ch.ethz.ssh2.StreamGobbler(s.getStderr());
    			   in[ 1 ] =new ch.ethz.ssh2.StreamGobbler(s.getStdout());
		           /* in[ 0 ] = s.getStderr() ;
		            in[ 1 ] = s.getStdout() ;
		            bcc = new BIGConsole( 100 , 100 , in ) ;*/
		           s.execCommand( "hostname && echo \"FINISHED\"\n");
		           System.out.println("Opening ssh-session to "+def+" to get the hostname.");
    		   	}
    		   	catch ( IOException ex )
		        {
    		   		ex.printStackTrace() ;
    		   		return null ;
		        }
		        try
		        {
		            byte[] buffer = new byte[8192 ] ;
		            int nums = in[ 1 ].read( buffer ) ;
		            while ( nums > -1 )
		            {
		               ret = ret + new String( buffer , 0 , nums ) ;
		               nums = in[ 1 ].read( buffer ) ;
		               if (ret.endsWith("FINISHED"))
		                  break;
		            }
		        }
		        catch ( IOException ex1 )
		        {
		            ex1.printStackTrace();
		        }
         //s.execCommand( "hostname && echo \"FINISHED\"" ) ;
/*         while ( bcc.getText().indexOf( "FINISHED" ) < 0 )
         {
         }
            System.err.println(bcc.getText());*/
		        closeSession( s ) ;
		        try
		        {
		            retString = ret.substring(0,ret.indexOf("\nFINISHED"));
		            done=true;
		        }catch(Exception e)
		        {
		            System.out.println("Getting hostname failed. Try again...");
		        }
    	   }
    	   else
    	   {
    		   System.err.println( "Couldn't open Session" ) ;
    	   }
       }
       if (retString.lastIndexOf("\n")>0)
       {
    	   retString = retString.substring( retString.lastIndexOf( "\n" ) + 1 ) ;
       }
       return retString ;
  }
   
   /**
    * This ServerHostKeyVerifier asks the user on how to proceed if a key cannot be found
    * in the in-memory database.
    *
    */
   class AdvancedVerifier
       implements ServerHostKeyVerifier
   {
      public boolean verifyServerHostKey( String hostname , int port ,
                                          String serverHostKeyAlgorithm ,
                                          byte[] serverHostKey )
          throws
          Exception
      {
         final String host = hostname ;
         final String algo = serverHostKeyAlgorithm ;

         String message ;

         /* Check database */

         int result = database.verifyHostkey( hostname ,
                                              serverHostKeyAlgorithm ,
                                              serverHostKey ) ;

         switch ( result )
         {
            case KnownHosts.HOSTKEY_IS_OK:
               return true ;

            case KnownHosts.HOSTKEY_IS_NEW:
               message = "Do you want to accept the hostkey (type " + algo +
                   ") from " + host + " ?\n" ;
               break ;

            case KnownHosts.HOSTKEY_HAS_CHANGED:
               message = "WARNING! Hostkey for " + host +
                   " has changed!\nAccept anyway?\n" ;
               break ;

            default:
               throw new IllegalStateException() ;
         }

         /* Include the fingerprints in the message */

         String hexFingerprint = KnownHosts.createHexFingerprint(
             serverHostKeyAlgorithm , serverHostKey ) ;
         String bubblebabbleFingerprint = KnownHosts.
             createBubblebabbleFingerprint(
                 serverHostKeyAlgorithm ,
                 serverHostKey ) ;

         message += "Hex Fingerprint: " + hexFingerprint +
             "\nBubblebabble Fingerprint: " + bubblebabbleFingerprint ;

         /* Now ask the user */
         //int choice = JOptionPane.showConfirmDialog( loginFrame , message ) ;

        // if ( choice == JOptionPane.YES_OPTION )
         {
            /* Be really paranoid. We use a hashed hostname entry */

            String hashedHostname = KnownHosts.createHashedHostname(
                hostname ) ;

            /* Add the hostkey to the in-memory database */

            database.addHostkey( new String[]
                                 {hashedHostname} ,
                                 serverHostKeyAlgorithm , serverHostKey ) ;

            /* Also try to add the key to a known_host file */

            /*try
            {
               KnownHosts.addHostkeyToFile( new File( knownHostPath ) ,
                                            new String[]
               {
                  hashedHostname
               } ,
                   serverHostKeyAlgorithm , serverHostKey ) ;
            }
            catch ( IOException ignore )
            {
            }*/

            return true ;
         }

        /* if ( choice == JOptionPane.CANCEL_OPTION )
         {
            throw new Exception(
                "The user aborted the server hostkey verification." ) ;
         }

         return false ;*/
      }
   }

   /**
    * The logic that one has to implement if "keyboard-interactive" autentication shall be
    * supported.
    *
    */
   class InteractiveLogic
       implements InteractiveCallback
   {
      int promptCount = 0 ;
      String lastError ;
      RemoteDefinition def = null ;
      public InteractiveLogic( String lastError , RemoteDefinition def )
      {
         this.lastError = lastError ;
         this.def = def ;
      }

      /* the callback may be invoked several times, depending on how many questions-sets the server sends */

      public String[] replyToChallenge( String name , String instruction ,
                                        int numPrompts , String[] prompt ,
                                        boolean[] echo )
          throws IOException
      {
         String[] result = new String[numPrompts ] ;

         for ( int i = 0 ; i < numPrompts ; i++ )
         {
            /* Often, servers just send empty strings for "name" and "instruction" */

            String[] content = new String[]
                {
                lastError , name , instruction ,
                prompt[ i ]} ;

            if ( lastError != null )
            {
               /* show lastError only once */
               lastError = null ;
            }
            if ( def.getPassword() == null )
            {
               EnterSomethingDialog esd = new EnterSomethingDialog(
                   loginFrame ,
                   "Keyboard Interactive Authentication for " +
                   def.getUsername() + "@" + def.getIP() ,
                   content , !echo[ i ] ) ;

               esd.setVisible( true ) ;
               if ( esd.answer == null )
                  throw new IOException( "Login aborted by user" ) ;

               result[ i ] = esd.answer ;
               def.setPassword( esd.answer ) ;
            }
            else
            {
               result[ i ] = def.getPassword() ;
            }
            promptCount++ ;
         }

         return result ;
      }

      /* We maintain a prompt counter - this enables the detection of situations where the ssh
       * server is signaling "authentication failed" even though it did not send a single prompt.
       */

      public int getPromptCount()
      {
         return promptCount ;
      }
   }

   /**
    * This dialog displays a number of text lines and a text field.
    * The text field can either be plain text or a password field.
    */
   class EnterSomethingDialog
       extends JDialog
   {
      private static final long serialVersionUID = 1L ;

      JTextField answerField ;
      JPasswordField passwordField ;

      final boolean isPassword ;

      String answer ;

      public EnterSomethingDialog( JFrame parent , String title ,
                                   String content ,
                                   boolean isPassword )
      {
         this( parent , title , new String[]
               {content} , isPassword ) ;
      }

      public EnterSomethingDialog( JFrame parent , String title ,
                                   String[] content , boolean isPassword )
      {
         super( parent , title , true ) ;

         this.isPassword = isPassword ;

         JPanel pan = new JPanel() ;
         pan.setLayout( new BoxLayout( pan , BoxLayout.Y_AXIS ) ) ;

         for ( int i = 0 ; i < content.length ; i++ )
         {
            if ( ( content[ i ] == null ) || ( content[ i ] == "" ) )
               continue ;
            JLabel contentLabel = new JLabel( content[ i ] ) ;
            pan.add( contentLabel ) ;

         }

         answerField = new JTextField( 20 ) ;
         passwordField = new JPasswordField( 20 ) ;

         if ( isPassword )
            pan.add( passwordField ) ;
         else
            pan.add( answerField ) ;

         KeyAdapter kl = new KeyAdapter()
         {
            public void keyTyped( KeyEvent e )
            {
               if ( e.getKeyChar() == '\n' )
                  finish() ;
            }
         } ;

         answerField.addKeyListener( kl ) ;
         passwordField.addKeyListener( kl ) ;

         getContentPane().add( BorderLayout.CENTER , pan ) ;

         setResizable( false ) ;
         pack() ;
         setLocationRelativeTo( null ) ;
      }

      private void finish()
      {
         if ( isPassword )
            answer = new String( passwordField.getPassword() ) ;
         else
            answer = answerField.getText() ;

         dispose() ;
      }
   }

}

/**
 *
 * <p>Überschrift: </p>
 *
 * <p>Beschreibung: </p>
 *
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organisation: </p>
 *
 * @author unbekannt
 * @version 1.0
 */
class SimpleVerifier
    implements ServerHostKeyVerifier
{
   KnownHosts database ;

   /*
    * This class is being used by to verify keys from e.g. "~/.ssh/known_hosts"
    */

   public SimpleVerifier( KnownHosts database )
   {
      if ( database == null )
         throw new IllegalArgumentException() ;

      this.database = database ;
   }

   public boolean verifyServerHostKey( String hostname , int port ,
                                       String serverHostKeyAlgorithm ,
                                       byte[] serverHostKey )
       throws Exception
   {
      int result = database.verifyHostkey( hostname , serverHostKeyAlgorithm ,
                                           serverHostKey ) ;

      switch ( result )
      {
         case KnownHosts.HOSTKEY_IS_OK:

            return true ; // We are happy

         case KnownHosts.HOSTKEY_IS_NEW:

            // Unknown host? Blindly accept the key and put it into the cache.
            // Well, you definitely can do better (e.g., ask the user).

            // The following call will ONLY put the key into the memory cache!
            // To save it in a known hosts file, call also "KnownHosts.addHostkeyToFile(...)"
            database.addHostkey( new String[]
                                 {hostname} , serverHostKeyAlgorithm ,
                                 serverHostKey ) ;

            return true ;

         case KnownHosts.HOSTKEY_HAS_CHANGED:

            // Close the connection if the hostkey has changed.
            // Better: ask user and add new key to database.
            return false ;

         default:
            throw new IllegalStateException() ;
      }
   }
}

/*****************************************************************************
  Log-History

 *****************************************************************************/
