package gui ;

import java.awt.GridLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

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

public class BIGInsetsPanel extends JPanel
   {
      java.awt.Insets i=null;
      JSpinner spinLeft;
      JSpinner spinTop;
      JSpinner spinBottom;
      JSpinner spinRight;
      public BIGInsetsPanel(java.awt.Insets i)
      {
         this.i=i;
         init();
      }
      private void init()
      {
         this.setLayout(new GridLayout(1,8));
         this.add(new JLabel("Top:"));
         spinTop=new JSpinner(new SpinnerNumberModel(i.top,0,500,1));
         this.add(spinTop);
         this.add(new JLabel("Left:"));
         spinLeft=new JSpinner(new SpinnerNumberModel(i.left,0,500,1));
         this.add(spinLeft);
         this.add(new JLabel("Bottom:"));
         spinBottom=new JSpinner(new SpinnerNumberModel(i.bottom,0,500,1));
         this.add(spinBottom);
         this.add(new JLabel("Right:"));
         spinRight=new JSpinner(new SpinnerNumberModel(i.right,0,500,1));
         this.add(spinRight);

      }
      public void setTheInsets(java.awt.Insets i)
      {
         this.spinTop.setValue(new Integer(i.top));
         this.spinLeft.setValue(new Integer(i.left));
         this.spinBottom.setValue(new Integer(i.bottom));
         this.spinRight.setValue(new Integer(i.right));
         this.revalidate();
      }
      public java.awt.Insets getTheInsets()
      {
         i.top = Integer.parseInt( ( ( JSpinner.NumberEditor ) spinTop.getEditor() ).
                                   getTextField().getText() ) ;
         i.left = Integer.parseInt( ( ( JSpinner.NumberEditor ) spinLeft.getEditor() ).
                                    getTextField().getText() ) ;
         i.right = Integer.parseInt( ( ( JSpinner.NumberEditor ) spinRight.
                                       getEditor() ).getTextField().getText() ) ;
         i.bottom = Integer.parseInt( ( ( JSpinner.NumberEditor ) spinBottom.
                                        getEditor() ).getTextField().getText() ) ;
         return i;
      }

}
