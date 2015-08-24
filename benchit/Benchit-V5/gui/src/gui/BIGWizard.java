package gui ;

import java.awt.*;
import java.awt.event.*;
import javax.swing.* ;

import system.BIGInterface;

import java.util.*;
import java.io.File;

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
public class BIGWizard
    extends JFrame
{
   private Vector panels = new Vector();
   private Vector actions = new Vector();

   // used for an icon or what...
   JPanel leftPanel=new JPanel();
   CardLayout rightPaneLayout = new CardLayout();
   JPanel rightPanel=new JPanel(rightPaneLayout);
   JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
   int actualPanel=0;
   private Action finishAction=new DefaultCloseAction();
   boolean backActivated=true;
   JButton next,last,finish;

   public BIGWizard()
   {
      next= new JButton("Next >>");
      last= new JButton("<< Back");
      finish= new JButton("Exit");
      next.addActionListener(new ActionListener()
          {
             public void actionPerformed(ActionEvent evt)
             {
                next(evt);
             }
          });
      last.addActionListener(new ActionListener()
          {
             public void actionPerformed(ActionEvent evt)
             {
                back(evt);
             }
          } ) ;
          last.setEnabled(false);
          finish.addActionListener(new ActionListener()
              {
                 public void actionPerformed(ActionEvent evt)
                 {
                    //System.err.println("finish");
                    //System.err.println(finishAction);
                    finishAction.actionPerformed(evt);
                 }
              } ) ;

      buttonPanel.add( last ) ;
      buttonPanel.add( next ) ;
      buttonPanel.add( finish ) ;

      JLabel lab = new JLabel( new ImageIcon( system.BIGInterface.getInstance().
                                              getBenchItPath() +
                                              File.separator + "gui" +
                                              File.separator + "img" +
                                              File.separator + "splash.jpg" ) ) ;
      lab.setBorder(BorderFactory.createEtchedBorder());
      leftPanel.add(lab,BorderLayout.CENTER);
      // putting all together
      JPanel contentPane= new JPanel();
      contentPane.setLayout(new BorderLayout());
      contentPane.add(leftPanel,BorderLayout.WEST);
      contentPane.add(rightPanel,BorderLayout.CENTER);
      contentPane.add(buttonPanel,BorderLayout.SOUTH);
      this.setContentPane(contentPane);
      this.setIconImage( new ImageIcon( BIGInterface.getInstance().getBenchItPath()
      		+ File.separator + "gui" + File.separator + "img"
      		+ File.separator + "clock.png" ).getImage() );
   }

   public BIGWizard(String title)
   {
      this();
      this.setTitle(title);
      this.setIconImage( new ImageIcon( BIGInterface.getInstance().getBenchItPath()
      		+ File.separator + "gui" + File.separator + "img"
      		+ File.separator + "clock.png" ).getImage() );
   }
   public BIGWizard(String title,boolean backActivated)
   {
      this();
      this.setTitle(title);
      this.backActivated=backActivated;
      this.setIconImage( new ImageIcon( BIGInterface.getInstance().getBenchItPath()
      		+ File.separator + "gui" + File.separator + "img"
      		+ File.separator + "clock.png" ).getImage() );
   }

   public JPanel[] getPanels()
   {
      JPanel[] pan=new JPanel[this.panels.size()];
      for (int i=0;i<pan.length;i++)
         pan[i]=(JPanel)panels.get(i);
      return pan;
   }
   public JPanel getPanel(int position)
   {
      return (JPanel)this.panels.get(position);
   }
   public void next(ActionEvent evt)
   {

       //System.err.println("next");
       rightPaneLayout.next(rightPanel);
       actualPanel++;
       if (actualPanel==panels.size()-1)
          next.setEnabled(false);
       else
          next.setEnabled(true);
       if ( backActivated )
          if ( actualPanel < 1 )
             last.setEnabled( false ) ;
          else
             last.setEnabled( true ) ;

       rightPanel.revalidate();
       buttonPanel.revalidate();
       ((Action)actions.get(actualPanel)).actionPerformed(evt);

    }
    public void back(ActionEvent evt)
    {

        //System.err.println("last");
        rightPaneLayout.next(rightPanel);
        actualPanel--;
        if (actualPanel==panels.size()-1)
           next.setEnabled(false);
        else
           next.setEnabled(true);

        if ( backActivated )
           if ( actualPanel < 1 )
              last.setEnabled( false ) ;
           else
              last.setEnabled( true ) ;
        rightPanel.revalidate();
        buttonPanel.revalidate();
        ((Action)actions.get(actualPanel)).actionPerformed(evt);

    }

   public void addPanel(JPanel pan)
   {
      this.panels.add(pan);
      this.actions.add(new DefaultAction());
      this.rightPanel.add(pan.toString(),pan);
   }
   public void addPanel(JPanel pan, String title)
   {
      pan.setBorder(BorderFactory.createTitledBorder(title));
      this.addPanel(pan);
   }

   public void addPanel(JPanel pan,int position)
   {
      this.panels.add(position,pan);
      this.actions.add(new DefaultAction());
      for (int i=position+1;i<panels.size();i++)
      {
         rightPanel.remove((JPanel)panels.get(i));
         rightPanel.add((JPanel)panels.get(i));
      }
   }
   public void addPanel(JPanel pan,Action act)
   {
      this.panels.add(pan);
      this.actions.add(act);
      this.rightPanel.add(pan.toString(),pan);
   }
   public void addPanel(JPanel pan, String title,Action act)
   {
      pan.setBorder(BorderFactory.createTitledBorder(title));
      this.addPanel(pan,act);
   }

   public void addPanel(JPanel pan,int position,Action act)
   {
      this.panels.add(position,pan);
      this.actions.add(position,act);
      for (int i=position+1;i<panels.size();i++)
      {
         rightPanel.remove((JPanel)panels.get(i));
         rightPanel.add((JPanel)panels.get(i));
      }
   }

   public void removePanel(JPanel pan)
   {
      int index=this.panels.indexOf(pan);
      this.panels.remove(index);
      this.actions.remove(index);
      this.rightPanel.remove(pan);
   }
   public void setFinishAction(Action action)
   {
      this.finishAction=action;
   }
   public void start()
   {
      if (this.panels.size()==0)
         return;
      if (this.panels.size()==1)
         this.next.setEnabled(false);
      this.pack();
      this.rightPanel.revalidate();
            int w = this.getGraphicsConfiguration().getDevice().getDisplayMode().getWidth();
            int h = this.getGraphicsConfiguration().getDevice().getDisplayMode().getHeight();
            this.setBounds(
               ( w - getWidth() ) / 2 , ( h - getHeight() ) / 2,
               this.getWidth() , this.getHeight() ) ;

      this.setVisible(true);
   }
   class DefaultAction extends AbstractAction
   {
      public void actionPerformed(ActionEvent evt)
      {

         //System.err.println("default");
      }
   };
   class DefaultCloseAction extends AbstractAction
   {
      public void actionPerformed(ActionEvent evt)
      {
         //System.err.println("defaultClose");
         setVisible(false);
      }
   };

}
