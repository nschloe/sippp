package system ;

import javax.swing.* ;
import java.awt.event.* ;
import java.io.* ;

public class BIGExecuteConsole
    extends JFrame
{
   private Process process ;
   OutputStreamWriter outWrite ;
   InputStreamReader erRead ;
   InputStreamReader ipRead ;
   JTextArea jtf ;
   private Thread getProcessesOutput ;
   private String command;
   public BIGExecuteConsole thisEXC;
   public BIGExecuteConsole(String command)
   {
      this.thisEXC=this;
      this.command=command;
   }

   public int run(final long waitInMs)
   {
      try
      {
         process = Runtime.getRuntime().exec( this.command ) ;
      }
      catch ( IOException ex )
      {
         System.err.println( ex ) ;
         System.err.println( "Couldn't start "+command ) ;
         return -1 ;
      }
      jtf = new JTextArea() ;
      this.setSize( 800 , 600 ) ;
      this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
      this.addWindowListener(new WindowAdapter()
                             {
                                public void windowClosing(WindowEvent evt)
                                {
                                   String msg = "Are you sure? This would close the running application";
                                   int result = javax.swing.JOptionPane.showConfirmDialog( thisEXC , msg , "Warning" ,
                                       javax.swing.JOptionPane.OK_CANCEL_OPTION ) ;
                                   if ( result == JOptionPane.CANCEL_OPTION )
                                   {
                                      return ;
                                   }
                                   process.destroy();
                                   setVisible(false);
                                }
                             });
      InputStream errorStr = process.getErrorStream() ;
      OutputStream outputStr = process.getOutputStream() ;
      InputStream inputStr = process.getInputStream() ;
      ipRead = new InputStreamReader( inputStr ) ;
      erRead = new InputStreamReader( errorStr ) ;
      outWrite = new OutputStreamWriter( outputStr ) ;
      getProcessesOutput = new Thread()
      {
         public void run()
         {
            while ( true )
            {

               if ( ( process != null ) && ( ipRead != null ) &&
                    ( erRead != null ) )
               {
                  try
                  {
                     if ( erRead.ready() )
                     {
                        char character ;
                        while ( erRead.ready() )
                        {
                           character = ( char ) erRead.read() ;
                           //System.err.print( character ) ;
                           jtf.setText( jtf.getText() + character ) ;

                        }
                     }
                     if ( ipRead.ready() )
                     {
                        char character ;
                        while ( ipRead.ready() )
                        {
                           character = ( char ) ipRead.read() ;
                           //System.err.print( character ) ;
                           jtf.setText( jtf.getText() + character ) ;
                        }
                     }
                  }
                  catch ( IOException ex )
                  {
                     System.err.println( ex ) ;
                     System.err.println(
                         "Couldn't read output from Process." ) ;
                  }
                  jtf.repaint() ;
               }
               try
               {
                  jtf.setCaretPosition( jtf.getText().length() ) ;
               }
               catch ( IllegalArgumentException ex1 )
               {
               }

               try
               {
                  this.sleep( waitInMs ) ;
               }
               catch ( InterruptedException ex1 )
               {
                  System.err.println( "Thread's sleep was interrupted" ) ;
               }
            }
         }
      } ;
      getProcessesOutput.start() ;
      jtf.setEditable(false);

      jtf.addKeyListener( new KeyListener()
      {
         private StringBuffer toWriteString = new StringBuffer() ;
         public void keyPressed( KeyEvent ke )
         {

         }

         public void keyReleased( KeyEvent ke )
         {

         }

         public void keyTyped( KeyEvent ke )
         {

            char[] typedChar = new char[ 1 ] ;
            typedChar[ 0 ] = ke.getKeyChar() ;
            if ((typedChar[0]>19)&&(typedChar[0]<127))
            {
               toWriteString.append( typedChar[ 0 ] ) ;
               jtf.setText(jtf.getText()+typedChar[0]);
               return;
            }

            // backspace
            if (typedChar[0]==8)
            {
               if (toWriteString.length()>0)
               {
                  toWriteString.deleteCharAt( toWriteString.length() - 1 ) ;
                  jtf.setText( jtf.getText().substring( 0 ,
                      jtf.getText().length() - 1 ) ) ;
               }
               return;
            }
            if ( ( typedChar[ 0 ] == '\n' ) )
            {
               toWriteString.append(typedChar[0]);
               // wenn es nix zu schreiben gibt
               if ( ( toWriteString.length() < 2 ) )
               {
                  // getting the last line of input command
                  System.err.println(jtf.getText());
                  StringBuffer lastLineOfJTF = new StringBuffer( jtf.getText().
                                                                 substring( 0 ,jtf.getText().lastIndexOf("\n") )) ;
                  System.err.println(lastLineOfJTF);
                  while (lastLineOfJTF.indexOf("\n")!=-1)
                  {
                     lastLineOfJTF.delete(0,lastLineOfJTF.indexOf("\n")+1);
                  }
                  if (( lastLineOfJTF.indexOf( "e.g." ) != -1 )||(lastLineOfJTF.indexOf("You got to fill this field!")!=-1))
                  {
                     toWriteString=new StringBuffer();
                     jtf.setText( jtf.getText() +
                                  "You got to fill this field!\n" ) ;
                     jtf.setCaretPosition( jtf.getText().length() ) ;
                     return ;
                  }
               }
               jtf.setText( "" ) ;
               try
               {
                  if (toWriteString.toString().endsWith("\\\n"))
                     toWriteString.append("\n");
                  outWrite.write( toWriteString.toString().toCharArray() ) ;
                  outWrite.flush() ;
                  this.toWriteString = new StringBuffer() ;
               }
               catch ( IOException ex1 )
               {
                  System.err.println(
                      "Couldn't flush the settings to OutputStream." ) ;
               }
            }

         }

      }
      ) ;
      this.setContentPane( jtf ) ;
      this.setVisible( true ) ;
      try
      {
         process.waitFor() ;
      }
      catch ( InterruptedException ex1 )
      {
         System.err.println( "Interrupted while waiting for Process" ) ;
      }
      this.setVisible(false);
      return process.exitValue() ;
   }
   public static void main(String[] args)
   {
      try
      {
         BIGExecuteConsole bec = new BIGExecuteConsole( args[ 0 ] ) ;
         System.exit(bec.run(100));
      } catch (Exception e)
      {
         System.err.println(e);
      }
   }


}
