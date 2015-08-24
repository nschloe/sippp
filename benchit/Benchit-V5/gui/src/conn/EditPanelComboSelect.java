package conn;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

/**
 *
 * <p>Überschrift: The EditPanelComoSelect</p>
 * <p>Beschreibung: The Setup-tab from the MainEditPanel</p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Organisation: ZHR</p>
 * @author rschoene
 * @version 1.0
 */

public class EditPanelComboSelect extends  JScrollPane //implements Scrollable
{
   /** radio button for sel b arch*/
   private JRadioButton architectureRadio = new JRadioButton(
       "Select graph by architecture" ) ;
   /** radio button for sel b kernel*/
   private JRadioButton kernelRadio = new JRadioButton(
       "Select graph by kernel" , true ) ;
   /** radio button for sel free*/
   private JRadioButton freeRadio = new JRadioButton( "Select graph free" ) ;
   /** radio buttons for select free. contains which information can be selected free*/
   private JRadioButton[] freeRadioButtons ;

   /**
    * Constructor
    * @param ed the MainEditPanel of the program
    */
   public EditPanelComboSelect( MainEditPanel ed )
   {
      // for better mouse-scrolling
      this.getVerticalScrollBar().setUnitIncrement( 15 ) ;
      // make a list of radiobuttons for free selection
      freeRadioButtons = new JRadioButton[ed.identifiers.length ] ;
      // buttongroups for if one is selected unselect the others
      ButtonGroup group = new ButtonGroup() ;
      // defining the group
      for ( int i = 0 ; i < ed.identifiers.length ; i++ )
      {
         freeRadioButtons[ i ] = new JRadioButton( ed.identifiers[ i ] ) ;
         group.add( freeRadioButtons[ i ] ) ;
         // set selected. the selected is the last difference between...
         // nah just look to newAbstratEditPanel
         if ( ed.identifiers[ i ].equals( "Filename" ) )
         {
            freeRadioButtons[ i ].setSelected( true ) ;
         }
      }
      // no group for the top-level radiobuttons
      JPanel panel = new JPanel() ;
      // but disable everything else, when acrhitecture is selected
      architectureRadio.addActionListener(
          new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            if ( architectureRadio.isSelected() )
            {
               kernelRadio.setSelected( false ) ;
               freeRadio.setSelected( false ) ;
               for ( int i = 0 ; i < freeRadioButtons.length ; i++ )
               {
                  freeRadioButtons[ i ].setEnabled( false ) ;
               }
            }
         }
      }
      ) ;
      // the same to kernel
      kernelRadio.addActionListener(
          new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            if ( kernelRadio.isSelected() )
            {
               architectureRadio.setSelected( false ) ;
               freeRadio.setSelected( false ) ;
               for ( int i = 0 ; i < freeRadioButtons.length ; i++ )
               {
                  freeRadioButtons[ i ].setEnabled( false ) ;
               }
            }
         }
      }
      ) ;
      // and when setting free, enabling the belonging sub-radiobuttons
      freeRadio.addActionListener(
          new ActionListener()
      {
         public void actionPerformed( ActionEvent evt )
         {
            if ( freeRadio.isSelected() )
            {
               architectureRadio.setSelected( false ) ;
               kernelRadio.setSelected( false ) ;
               for ( int i = 0 ; i < freeRadioButtons.length ; i++ )
               {
                  freeRadioButtons[ i ].setEnabled( true ) ;
               }
            }
         }
      }
      ) ;
      // layout-stuff
      GridBagLayout gridbag = new GridBagLayout() ;
      panel.setLayout( gridbag ) ;
      GridBagConstraints gc = new GridBagConstraints() ;
      gc.anchor = GridBagConstraints.WEST ;
      gc.gridx = 0 ;
      gc.gridwidth = 2 ;
      gc.gridy = 0 ;
      panel.add( architectureRadio , gc ) ;
      gc.gridy = 1 ;
      panel.add( kernelRadio , gc ) ;
      gc.gridy = 2 ;
      panel.add( freeRadio , gc ) ;
      gc.gridx = 1 ;
      gc.gridwidth = 1 ;
      for ( int i = 0 ; i < ed.identifiers.length ; i++ )
      {
         gc.gridy = 3 + i ;
         freeRadioButtons[ i ].setEnabled( false ) ;
         panel.add( freeRadioButtons[ i ] , gc ) ;
      }
      double[] temp =
          {
          .2 , .8} ;
      // +1: because an array starts with 0
      double[] temp2 = new double[2 + ed.identifiers.length ] ;
      for ( int j = 0 ; j < temp2.length ; j++ )
      {
         temp2[ j ] = 1.0 / temp2.length ;
      }
      gc.gridy = 2 + ed.identifiers.length ;
      gridbag.columnWeights = temp ;
      gridbag.rowWeights = temp2 ;
      this.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS ) ;
      // better scrolling
      this.getVerticalScrollBar().setUnitIncrement( 15 ) ;
      this.setHorizontalScrollBarPolicy( JScrollPane.
                                         HORIZONTAL_SCROLLBAR_ALWAYS ) ;
      this.setViewportView( panel ) ;

   }

   /**
    * @return 	0 if free is selected
    * 		1 if kernel is selected
    *		2 if architecture is selected
    */
   public int getSelectedConfiguration()
   {
      // free
      if ( this.freeRadio.isSelected() )
      {
         return 0 ;
      }
      // kernel
      if ( this.kernelRadio.isSelected() )
      {
         return 1 ;
      }
      // architecture
      return 2 ;
   }

   /**
    * returns the identifiers of the setted freeRadioButtons[]
    * @return identifiers of the setted radioButtons
    */
   public String[] getSelectedRadioButton()
   {
      // get how many are selected to build the array
      int count = 0 ;
      for ( int i = 0 ; i < freeRadioButtons.length ; i++ )
      {
         if ( freeRadioButtons[ i ].isSelected() )
         {
            count++ ;
         }
      }
      // build the array
      String[] active = new String[count ] ;
      // write array-content
      count = 0 ;
      for ( int i = 0 ; i < freeRadioButtons.length ; i++ )
      {
         if ( freeRadioButtons[ i ].isSelected() )
         {
            active[ count ] = freeRadioButtons[ i ].getText() ;
            count++ ;
         }
      }
      return active ;
   }
   /**
    * returns the identifiers of the setted freeRadioButtons[]
    * @return identifiers of the setted radioButtons
    */
   public String[] getUnselectedRadioButton()
   {
      // get how many are selected to build the array
      int count = 0 ;
      for ( int i = 0 ; i < freeRadioButtons.length ; i++ )
      {
         if ( !freeRadioButtons[ i ].isSelected() )
         {
            count++ ;
         }
      }
      // build the array
      String[] active = new String[count ] ;
      // write array-content
      count = 0 ;
      for ( int i = 0 ; i < freeRadioButtons.length ; i++ )
      {
         if ( !freeRadioButtons[ i ].isSelected() )
         {
            active[ count ] = freeRadioButtons[ i ].getText() ;
            count++ ;
         }
      }
      return active ;
   }

}
