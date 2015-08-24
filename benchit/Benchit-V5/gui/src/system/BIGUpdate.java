package system ;

/**
 * <p>Überschrift: BenchIT</p>
 *
 * <p>Beschreibung: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organisation: ZHR TU Dresden</p>
 *
 * @author Robert Schoene
 * @version 1.0
 */
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.StringTokenizer;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
public class BIGUpdate
{
	// variable to synchronize searching for updates,
	// the variable is used by the two threads t1 and t2
	private static Integer synchVar = new Integer( 0 );
	
   public static void main( String[] args )
   {
      if (args.length<1)
      {
         System.out.println("Start the update manually with:");
         System.out.println("java -classpath BenchIT.jar system.BIGUpdate <server>");
         return;
      }
      if (isUpdateAvailable(args[0]))
         update(args[0],true);
   }

   /**
    * isUpdateAvailable
    *
    * @param server String: e.g. http:/www.benchit.org/gui-update/
    * in the folder should be 2 files. BenchIT.jar and version.txt
    * BenchIT.jar is the executable jar for the GUI and versions.txt should
    * contain s.th like 1.2 or 0.99
    * @return updateAvailable: yes, elsewise false
    */
   public static boolean isUpdateAvailable( String server )
   {
      final URL u,txt;
      // default timeout for URLConnection
      final int timeout = 2000;
      // the two threads to avoid too long waiting on a server response
      Thread t1, t2;
      try
      {
         u = new URL( server ) ;
      }
      catch ( MalformedURLException ex )
      {
         System.err.println("It was tried to create a URL from "+server+".");
         System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
         return false;
      }
      try
      {
         txt = new URL( server+"version.txt" ) ;
      }
      catch ( MalformedURLException ex )
      {
         System.err.println("It was tried to create a URL from "+server+"version.txt.");
         System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
         return false;
      }
      
      // this thread looks for an update, but it is possible that the hhtp-request take a long time
      // e.g if no connection can be established
      t1 = new Thread()
    	      {
    	         public void run()
    	         {
    	        	// System.out.println("Marke: t1-1");
    	        	// an URLConnection to the update server
    	        	URLConnection urlConn;
    	        	try {
    	        		// System.out.println("Marke: t1-2");
    	        		// txt.openConnection() can take a long time if an error occures
    	        		urlConn = txt.openConnection();
						urlConn.getContentLength();
						// System.out.println("Marke: t1-3");
					}
    	        	catch (IOException e) {}
    	        	// System.out.println("Marke: t1-4");
    	     		// at this point we are successfully connected to the server 
    	        	synchronized(synchVar)
    	        	{
    	     			// System.out.println("Marke: t1-5"); 
    	     			if ( synchVar.intValue() != 2 )
    	     			{
    	     				// if there was no timeout (synchVar != 2) and
    	     				// a correct connection is established we set
    	     				// synchVar to 1
    	     				synchVar = new Integer( 1 );
    	     				// System.out.println("Marke: t1-6");
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
	        	// System.out.println("Marke: t2-1");
	        	try {
	        		// the thread sleeps for (timeout) milliseconds 
					Thread.sleep( timeout );
				} catch (InterruptedException ie) {}
				// System.out.println("Marke: t2-2");
	        	synchronized(synchVar)
	        	{
	        		if ( synchVar.intValue() != 1 )
	        		{
	        			// after the thread has slept for (timeout) milliseconds
	        			// and if there is no correct connection (synchVar != 1)
	        			// we set synchVar to 2
	        			synchVar = new Integer( 2 );
	        			// System.out.println("Marke: t2-3");
	        		}
	        	};
	         }
	      };
	  t2.start();
      
	  
	  // System.out.println("Marke: busy-waiting");
	  // sleep while threads t1 and t2 coordinate the update-search 
	  for(int numberOfSleeps=0;numberOfSleeps<2*timeout;numberOfSleeps++)
	  {

		  synchronized( synchVar )
		  {
			  if (synchVar.intValue() != 0)
				  break;
		  }
		  // System.out.println("Marke: busy-waiting");
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
			  return false;
		  }
		  
	  }
	  
      /*try {
		urlConn = txt.openConnection();
		urlConn.setReadTimeout(5000);
		
		urlConn.getContentLength();
      }
      catch (IOException ioe) {
    	  System.err.println("File version.txt not found");
          return false;
      }*/
      
      /*try
      {
         txt.openConnection().getContentLength() ;
      }
      catch ( IOException ex2 )
      {
         System.err.println("File version.txt not found");
         return false;
      }*/
      
      double version=0.0;
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
                "File "+server+"version.txt not found. Update not available." ) ;
            return false;
         }
         String txtContent = new String( content , 0 , length ) ;
         if ( txtContent.indexOf( "\n" ) > -1 )
            txtContent = txtContent.substring( 0 , txtContent.indexOf( "\n" ) ) ;
         version = Double.parseDouble( txtContent ) ;
      }
      catch ( NumberFormatException ex1 )
      {
         System.err.println("The version.txt file at "+server+" has an invalid format.");
         return false;
      }
      if (BIGInterface.getInstance().getVersion()<version)
         return true;
      else
         return false;
   }

   /**
    * update
    *
    * @param server see isUpd..
    */
   public static void update( String server, boolean consoleMsg )
   {
      double ownVersion=BIGInterface.getInstance().getVersion();
      String newFileVersion=""+ownVersion;
      URL u,jar,txt;
      try
      {
         u = new URL( server ) ;
      }
      catch ( MalformedURLException ex )
      {
         System.err.println("It was tried to create a URL from "+server+".");
         System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
      }
      try
      {
         jar = new URL( server+"BenchIT.jar" ) ;
      }
      catch ( MalformedURLException ex )
      {
         System.err.println("It was tried to create a URL from "+server+"BenchIT.jar.");
         System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
         return;
      }

      try
      {
         txt = new URL( server+"version.txt" ) ;
      }
      catch ( MalformedURLException ex )
      {
         System.err.println("It was tried to create a URL from "+server+"version.txt.");
         System.err.println("This URL is malformed. Try s.th. like http://www.benchit.org/gui-update");
         return;
      }
      try
      {
         txt.openStream() ;
      }
      catch ( IOException ex2 )
      {
         System.err.println("File version.txt not found");
         return;
      }

      if (consoleMsg)
         System.out.println("Getting new version.");

      byte[] buffer =new byte[32768];
      int length=buffer.length;
      int totalBytes=0;
      //-------------------------------------------------------
      final JFrame jf=new JFrame("Updating");
      //-------------------------------------------------------
      JProgressBar progress=new JProgressBar(0,100);
      jf.setSize(500,50);
      jf.setContentPane(new JLabel("Checking Update"));
      //-------------------------------------------------------
      final Thread updateThread = Thread.currentThread();
      JButton cancel = new JButton("Cancel");
      cancel.addActionListener( new ActionListener() {
    	  public void actionPerformed(ActionEvent evt) {
    		  try {
    			  updateThread.interrupt();
    		  }
    		  catch(SecurityException se) {}
    		  jf.setVisible( false );
    	  }
      });
      //-------------------------------------------------------
      jf.validate();
      jf.setVisible(true);
      try
      {
         InputStream in = txt.openStream() ;
         StringBuffer sb=new StringBuffer();
         while ( ( totalBytes < txt.openConnection().getContentLength() ) &&
                 ( sb.indexOf( "\n" + ownVersion + "\n" ) < 0 ) )
         {
            length = in.read( buffer ) ;
            totalBytes+=length;
            if (consoleMsg)
               System.out.print(".");
            if (length>-1)
               sb.append(new String(buffer,0,length));
         }
         in.close();
         String files=sb.toString();
         newFileVersion=files.substring(0,files.indexOf("\n"));
         if (files.indexOf( "\n" + ownVersion + "\n" ) >= 0)
         {
            files=files.substring(0,files.indexOf( "\n" + ownVersion + "\n"));
         }
         StringTokenizer st=new StringTokenizer(files,"\n");
         HashSet hs=new HashSet();
         while (st.hasMoreTokens())
         {
            String file=st.nextToken();
            try
            {
               Double.parseDouble( file ) ;
            }
            catch ( NumberFormatException ex4 )
            {
               // its not a version hint, but a file ;)

               if ( !hs.contains( file ) )
                  hs.add( file ) ;
            }
         }
         progress.setStringPainted(true);
         jf.setContentPane(progress);
         jf.validate();
         Iterator it=hs.iterator();
         File f;
         File origF;
         File updateFolder=new File( BIGInterface.getInstance().getBenchItPath() +
                          File.separator + "gui" + File.separator +
                          "update");
         updateFolder.mkdirs();
         while (!updateFolder.exists());

         while(it.hasNext())
         {
            // build url
            String entry = ( String ) it.next() ;
            // until hash is ok
            do
            {
            jf.getContentPane().validate();
            progress.setValue(0);
            String fileUrl = server + entry ;
            progress.setString(fileUrl);
            f = new File( BIGInterface.getInstance().getBenchItPath() +
                          File.separator + "gui" + File.separator +
                          "update" + File.separator + entry ) ;
            origF = new File( BIGInterface.getInstance().getBenchItPath() +
                              File.separator + "gui" + File.separator +
                              entry ) ;

            if ( BIGInterface.getSystem() == BIGInterface.WINDOWS_SYSTEM )
            {
               // cant use replaceAll, because it doesn't work
               // never trust a framework :(
               StringBuffer newName=new StringBuffer();
               String oldName=BIGInterface.getInstance().getBenchItPath() +
                             File.separator + "gui" + File.separator +
                             "update" + File.separator +
                             entry;
               for (int i=0;i<oldName.length();i++)
               {
                  if (oldName.charAt(i)=='/')
                     newName.append('\\');
                  else
                     newName.append(oldName.charAt(i));
               }
               f=new File(newName.toString());
               newName=new StringBuffer();
               oldName=BIGInterface.getInstance().getBenchItPath() +
                             File.separator + "gui" + File.separator +
                             entry;
               for (int i=0;i<oldName.length();i++)
               {
                  if (oldName.charAt(i)=='/')
                     newName.append('\\');
                  else
                     newName.append(oldName.charAt(i));
               }
               origF=new File(newName.toString());
/*
               f = new File( BIGInterface.getInstance().getBenchItPath() +
                             File.separator + "gui" + File.separator +
                             "update" + File.separator +
                             entry.replaceAll( "/" , "\\" ) ) ;
               origF=new File( BIGInterface.getInstance().getBenchItPath() +
                             File.separator + "gui" + File.separator +
                             entry.replaceAll( "/" , "\\" ) ) ;*/

            }
            u = new URL( fileUrl ) ;
            URLConnection conn=u.openConnection();
            int fileLength=conn.getContentLength();
            f.getParentFile().mkdirs();
            while(!f.getParentFile().exists());
            FileOutputStream out=new FileOutputStream(f);
            progress.setMaximum(fileLength);
            totalBytes=0;
            in=u.openStream();
            while ( ( totalBytes < fileLength ) && in.available()>-1 )
            {
               length = in.read( buffer ) ;
               totalBytes += length ;
               progress.setValue(totalBytes);
               if (length>-1)
               out.write(buffer,0,length);
               if ( consoleMsg )
                  System.out.print( "." ) ;
               if ( Thread.currentThread().isInterrupted() ) {
            	   BIGFileHelper.remove(updateFolder);
            	   System.out.println("Update was interrupted");
            	   return;
               }
            }
            in.close() ;
            out.close();
            f.hashCode();
            u = new URL( fileUrl+".hash" ) ;
            conn=u.openConnection();
            fileLength=conn.getContentLength();
            in=u.openStream();
            sb=new StringBuffer();
            while ( ( totalBytes < fileLength ) && in.available()>-1 )
            {
               length = in.read( buffer ) ;
               totalBytes += length ;
               progress.setValue(totalBytes);
               if (length>-1) {
                  sb.append(new String(buffer,0,length));
               }
               else {
            	   break;
               }
               if ( consoleMsg )
                  System.out.print( "." ) ;
               if ( Thread.currentThread().isInterrupted() ) {
            	   BIGFileHelper.remove(updateFolder);
            	   System.out.println("Update was interrupted");
            	   return;
               }
            }
         } while((""+f.hashCode()).equals(sb.toString()));
         BIGFileHelper.copyToFolder(f,origF.getParentFile(),true);
         f.delete();

            BIGFileHelper.saveToFile( newFileVersion ,
                                      new File( BIGInterface.getInstance().
                                                getBenchItPath() +
                                                File.separator + "gui" +
                                                File.separator + "cfg" +
                                                File.separator + "version.txt" ) ) ;
         }
         BIGFileHelper.remove(updateFolder);
      }
      catch ( IOException ex1 )
      {
         ex1.printStackTrace();
      }
      jf.setVisible(false);
      if (consoleMsg)
         System.out.println("Done.");
   }

}
