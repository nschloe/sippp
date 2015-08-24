package gui ;

import java.awt.BorderLayout;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileFilter;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import system.BIGFileHelper;
import system.BIGInterface;
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
public class BIGEnvironmentEditor
    extends JTabbedPane
{
   private final static File ENVIRONMENTFOLDER = new File( BIGInterface.
       getInstance().getBenchItPath() + File.separator + "tools" +
       File.separator + "environments" ) ;
   private File[] environments;
   private BIGEditor[] editors;
   public BIGEnvironmentEditor()
   {
      environments=ENVIRONMENTFOLDER.listFiles(new FileFilter()
          {
             public boolean accept(File f)
             {
                if (f.isFile())
                   return true;
                return false;
             }
          });
      editors=new BIGEditor[environments.length];
      for (int i=0;i<environments.length;i++)
      {
         final int finI=i;
         JPanel pan= new JPanel(new BorderLayout());
         editors[ i ] = new BIGEditor( environments[ i ] ) ;
         JButton save = new JButton( "Save" ) ;
         save.addActionListener( new ActionListener()
         {
            public void actionPerformed( ActionEvent evt )
            {
               if ( BIGEnvironmentEditor.this.editors[ finI ].textChanged() )
                  BIGFileHelper.saveToFile(
                      BIGEnvironmentEditor.this.editors[ finI ].getText() ,
                      BIGEnvironmentEditor.this.environments[ finI ] ) ;
               BIGEnvironmentEditor.this.editors[ finI ].textSaved() ;
            }
         } ) ;
         pan.add( editors[ i ] , BorderLayout.CENTER ) ;
         pan.add( save , BorderLayout.SOUTH ) ;
         this.addTab(environments[ i ].getName() , pan ) ;
      }
   }
   /**
    * override this to be sure the right background is around the tabs
    * @param g Graphics
    */
   public void paint(Graphics g)
   {
      g.setColor(new JPanel().getBackground());
      g.fillRect(0,0,this.getSize().width,this.getSize().height);
      super.paint(g);
   }
}
