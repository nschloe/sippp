package system;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.StringTokenizer;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

/**
 * <p>Überschrift: BenchIT</p>
 *
 * <p>Beschreibung: BIGUpdate is used to replace complete
 * files. Nevertheless it is necessary to edit lines in text files.
 * That's the job of BIGTextfileUpdate. BIGTextfileUpdate reads
 * a file on the BenchIT server like jobs.txt where affected file,
 * affected string,replace string and so on are specified. After that
 * the specified tasks are executed.
 * </p>
 *
 * <p>Copyright: Copyright (c) 2007</p>
 *
 * <p>Organisation: ZIH TU Dresden</p>
 *
 * @author Ronny Tschueter
 * @version 1.0
 */

public class BIGTextfileUpdate {
	/**
	 * variable to synchronize searching for updates,
	 * the variable is used by the two threads t1 and t2
	 */
	private static Integer synchVar = new Integer( 0 );
	
	/**
	 * This method tests if updates for some text files are available. To do so a connection
	 * to the specified update server is established and the file 'jobs.txt' is opened to look
	 * for entries with a newer version id than the own one. If such etries are found then this
	 * method retutns true else false.
	 * 
	 * @param serverAsString - location of the update server as string
	 * @return boolean - true if updates are available, otherwise false
	 */
	public static boolean isTextfileUpdateAvailable( String serverAsString )
	{
	      final URL txt;
	      URL server;
	      // default timeout for URLConnection
	      final int timeout = 2000;
	      // the two threads to avoid too long waiting on a server response
	      Thread t1, t2;
	      
	      // if (debug) System.out.println("betrete isTextfileUpadteAvailable()");
	      try
	      {
	         server = new URL( serverAsString ) ;
	      }
	      catch ( MalformedURLException ex )
	      {
	         System.err.println("It was tried to create a URL from "+serverAsString+".");
	         System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
	         return false;
	      }	      
	      try
	      {
	         txt = new URL( server.toExternalForm() + "jobs.txt" ) ;
	      }
	      catch ( MalformedURLException ex )
	      {
	         System.err.println("It was tried to create a URL from " + server.toExternalForm() + "jobs.txt.");
	         System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
	         return false;
	      }
	      
	      // this thread looks for an update, but it is possible that the http-request take a long time
	      // e.g if no connection can be established
	      t1 = new Thread()
	    	      {
	    	         public void run()
	    	         {
	    	        	// an URLConnection to the update server
	    	        	URLConnection urlConn;
	    	        	try {
	    	        		// txt.openConnection() can take a long time if an error occures
	    	        		urlConn = txt.openConnection();
							urlConn.getContentLength();
						}
	    	        	catch (IOException e) {}
	    	        	
	    	     		// at this point we are successfully connected to the server 
	    	        	synchronized(synchVar)
	    	        	{ 
	    	     			if ( synchVar.intValue() != 2 )
	    	     			{
	    	     				// if there was no timeout (synchVar != 2) and
	    	     				// a correct connection is established we set
	    	     				// synchVar to 1
	    	     				synchVar = new Integer( 1 );
	    	     			}
	    	        	};
	    	         }
	    	      };
	      t1.start();
	      
	      // to avoid that the GUI has to wait very long on a request, we start a "timeout-thread"
	      t2 = new Thread()
	      {
		         public void run()
		         {
		        	try {
		        		// the thread sleeps for (timeout) milliseconds 
						Thread.sleep( timeout );
					} catch (InterruptedException ie) {}
					
		        	synchronized(synchVar)
		        	{
		        		if ( synchVar.intValue() != 1 )
		        		{
		        			// after the thread has slept for (timeout) milliseconds
		        			// and if there is no correct connection (synchVar != 1)
		        			// we set synchVar to 2
		        			synchVar = new Integer( 2 );
		        		}
		        	};
		         }
		      };
		  t2.start();
	      
		  // sleep while threads t1 and t2 coordinate the update-search 
		  for(int numberOfSleeps = 0; numberOfSleeps < 2*timeout; numberOfSleeps++)
		  {

			  synchronized( synchVar )
			  {
				  if (synchVar.intValue() != 0)
					  break;
			  }
			  try {
				  Thread.sleep( timeout/1000 );
			  }
			  catch (InterruptedException ie) {}
		  }
		  synchronized( synchVar )
		  {
			  
			  // if the timeout is reached or no connection could be established
			  // return false (no update available)
			  if (synchVar.intValue() != 1)
			  {
				  if (debug) System.out.println("timeout");
				  return false;
			  }
			  
		  }
	      
	      double jobVersion=0.0;
	      
	      try
	      {
	         byte[] content = new byte[100 ] ;
	         int length = -1 ;
	         try
	         {
	            length = txt.openStream().read( content ) ;
	         }
	         catch ( IOException ex3 )
	         {
	            System.err.println(
	                "File " + server.getPath() + "jobs.txt not found. Update not available." ) ;
	            return false;
	         }
	         String txtContent = new String( content , 0 , length ) ;
	         if ( txtContent.indexOf( "\n" ) > -1 )
	            txtContent = txtContent.substring( 0 , txtContent.indexOf( "\n" ) ) ;
	         jobVersion = Double.parseDouble( txtContent ) ;
	      }
	      catch ( NumberFormatException ex1 )
	      {
	         System.err.println("The jobs.txt file at "+server+" has an invalid format.");
	         return false;
	      }
	      if (BIGInterface.getJobVersion() < jobVersion)
	         return true;
	      else
	         return false;
	}
	
	/**
	 * This method performs the update of text files.
	 *
	 * @param server		string representation of the server-url
	 * @param consoleMsg	if true write status to console 
	*/
	public static void update( String server, boolean consoleMsg )
	{
		double ownVersion = BIGInterface.getJobVersion();
		String newJobVersion=""+ownVersion;
		URL txt;
	    
		if (debug) System.out.println("starting update");
		
		try
	    {
	    	new URL( server ) ;
	    }
	    catch ( MalformedURLException ex )
	    {
	    	System.err.println("It was tried to create a URL from "+server+".");
	    	System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
	    }
	    
	    try
	    {
	    	new URL( server+"BenchIT.jar" ) ;
	    }
	    catch ( MalformedURLException ex )
	    {
	    	System.err.println("It was tried to create a URL from "+server+"BenchIT.jar.");
	    	System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
	    	return;
	    }
	    
	    try
	    {
	    	txt = new URL( server + "jobs.txt" ) ;
	    }
	    catch ( MalformedURLException ex )
	    {
	    	System.err.println("It was tried to create a URL from " + server + "jobs.txt.");
	    	System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
	    	return;
	    }
	    
	    try
	    {
	    	txt.openStream() ;
	    }
	    catch ( IOException ex2 )
	    {
	    	System.err.println("File jobs.txt not found");
	    	return;
	    }
	    
	    if (consoleMsg) System.out.println("Getting new version.");
	
	    byte[] buffer = new byte[32768];
	    int length = buffer.length;
	    int totalBytes = 0;
	    
	    
	    // if (debug || consoleMsg) System.out.println("build frame");
	    
	    final JFrame jf = new JFrame("Updating textfiles");
	    JPanel panel = new JPanel();
	    JProgressBar progress = new JProgressBar(0,100);
	    // jf.setSize(500,50);
	    jf.setContentPane(panel);
	    final Thread txtUpdThread = Thread.currentThread();
	    JButton cancelBtn = new JButton("Cancel");
	    cancelBtn.addActionListener( new ActionListener() {
	    	public void actionPerformed(ActionEvent evt) {
	    		try {
	    			txtUpdThread.interrupt();
	    		}
	    		catch(SecurityException se) {}
	    		jf.setVisible( false );
	    	}
	    });
	    panel.add(new JLabel("Checking Update"), BorderLayout.NORTH);
	    panel.add(progress, BorderLayout.CENTER);
	    panel.add( cancelBtn, BorderLayout.SOUTH);
	    
	    jf.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	    jf.pack();
	    jf.validate();
	    jf.setVisible(true);
	    
	    try
	    {
	    	// Stream to get the jobs-file
	    	InputStream in = txt.openStream() ;
	    	// buffer to store content of the jobs-file
	    	StringBuffer sb = new StringBuffer();
	    	if (debug) System.out.println("let's start with work");
	    	while ( ( totalBytes < txt.openConnection().getContentLength() ) &&
	                 ( sb.indexOf( "\n" + ownVersion + "\n" ) < 0 ) )
	        {
	    		length = in.read( buffer ) ;
	            totalBytes += length;
	            if (consoleMsg) System.out.print(".");
	            if (length > -1) sb.append(new String(buffer,0,length));
	        }
	    	in.close();
	    	
	        if (debug) System.out.println("extract tasks");
	    	String tasks = sb.toString();
	    	if (debug) System.out.println("A list of all tasks:\n" + tasks);
	        
	        // the jobs-file begins with the latest version-id
	        // so you can take this id as the new jobVersion of the gui
	        newJobVersion = tasks.substring(0, tasks.indexOf("\n"));
	        if (debug) System.out.println("newVersion: " + newJobVersion);
	        
	        // select the interesting part:
	        // -> that means the part of changes between the latest version on the server
	        // and the (obsolete) ownVersion
	        if (debug) System.out.println("index of own version: " + tasks.indexOf( "\n" + ownVersion + "\n" ) );
	        if (tasks.indexOf( "\n" + ownVersion + "\n" ) >= 0)
	        {
	        	tasks = tasks.substring(0, tasks.indexOf( "\n" + ownVersion + "\n") );
	        }
	        
	        // break the single (long) string into tokens divided by newline 
	        StringTokenizer tokenizer = new StringTokenizer( tasks, "\n" );
	        BIGJobList jobList = new BIGJobList();
	        if (debug) System.out.println("now jobList.fillList() is started");
	        jobList.fillList( tokenizer );
	         
	        progress.setStringPainted(true);
	        jf.setContentPane(progress);
	        jf.validate();
	        
	        /*File updateFolder=new File( BIGInterface.getInstance().getBenchItPath() +
	                          File.separator + "gui" + File.separator +
	                          "update");
	        
	        updateFolder.mkdirs();
	        while (!updateFolder.exists());*/
	        
	        Iterator iter = jobList.iterator();
	        
	        while(iter.hasNext())
	        {
	        	BIGJobListElement ele = (BIGJobListElement) iter.next();
	        	if (debug) System.err.println("Perform update for: " + ele.versionID);
	        	performUpdate( ele );
	        }
	        
	        // TODO: write new version only if update was executed successfully
	        BIGFileHelper.saveToFile( 	newJobVersion ,
 					new File( BIGInterface.getInstance().
                                getBenchItPath() +
                                File.separator + "gui" +
                                File.separator + "cfg" +
                                File.separator + "jobVersion.txt" ) ) ;
	    }
		catch ( IOException ex1 )
		{
			ex1.printStackTrace();
		}
		
		jf.setVisible(false);
		if (consoleMsg)	System.out.println("Done.");
	}
	
	/**
	 * This method perform all task specified in the BIGJobListElement element.
	 * 
	 * @param element - determines all task that has to be executed (which file, 
	 * 					affected string, replacement string)
	 */
	private static void performUpdate(BIGJobListElement element) {
		String path;
		BIGConfigFileParser cfgParser;
		
		if ( element.getAffectedFile().equals("BGUI.cfg")) {
			cfgParser = BIGInterface.getInstance().getBIGConfigFileParser();
			if (debug) System.out.println("performUpdate: BGUI.cfg" );
		}
		else {
			path = BIGInterface.getInstance().
            		getBenchItPath() +
            		File.separator + "gui" +
            		File.separator + "cfg" +
            		File.separator + element.getAffectedFile();
			cfgParser = new BIGConfigFileParser( path );
			if (debug) System.out.println("performUpdate: " + path.toString() );
		}
		if (debug) System.out.println("performUpdate: " + element.getAffectedString() +
								" is set to " + element.getReplaceWith() );
		cfgParser.set( element.getAffectedString(), element.getReplaceWith() );
		cfgParser.save();
	}
	
	private static boolean debug = false;
	
	public static String AFFECTEDFILE	= "affected file";
	public static String AFFECTEDSTRING	= "affected string";
	public static String REPLACEWITH	= "replace with";
}
