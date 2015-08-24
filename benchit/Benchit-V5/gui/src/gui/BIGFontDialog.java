/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 *
 *
 * Last change by: $Author: tschuet $
 * $Revision: 1.6 $
 * $Date: 2007/07/10 10:41:30 $
 *******************************************************************/

package gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import plot.BIGPlotable;
import system.BIGInterface;
import system.BIGUtility;

/**
 * A JFrame to make all font-settings.
 * <br>
 * Here you can set the fontname, -size and -style for every item in BIGPlotable.fontNames[]
 *
 * @author Ronny Tschueter
 *
 */

public class BIGFontDialog extends JFrame
{
	private BIGPlotable plotable;
	private Font fonts[];
	private JTabbedPane displayTabPane;
	private ActionListener al;
	private BIGInterface bigInterface = BIGInterface.getInstance();
	int count = BIGPlotable.fontNames.length;
	private int row = 0;

	/**
	 * This constructor is used if you select the item "Set Plot Fonts" in the menu "Setup".
	 * All settings will be written into the BGUI.cfg-file.
	 *
	 * @param newFonts an array of used fonts
	 */
	public BIGFontDialog(Font[] newFonts)
	{
		plotable = null;
		displayTabPane = null;
		al = null;
		fonts = newFonts;
		showFontDialog();
	}

	/**
	 * This constructor is used in the config-dialog of a plot.
	 * All settings will be written into newPlotable.
	 *
	 * @param newPlotable BIGPlotable the plotable which will be used
	 * @param newDisplayTabPane JTabbedPane the pane where the plot is displayed
	 * @param newAl ActionListener an ActionLitener that actualize the plot (e.g. ActionListener of the setButton)
	 */
	public BIGFontDialog(BIGPlotable newPlotable, JTabbedPane newDisplayTabPane, ActionListener newAl)
    {
		plotable = newPlotable;
		fonts = plotable.getAllFonts();
		displayTabPane = newDisplayTabPane;
		al = newAl;
		showFontDialog();
    }

	/**
	 * builds the font-dialog and handles all actions
	 *
	 */
	private void showFontDialog()
	{
		// show plot if displayTabPane is not null
		if (displayTabPane != null)
		{
	        displayTabPane.setSelectedIndex( 0 ) ;
	        displayTabPane.revalidate() ;
		}
        //build new frame
        this.setTitle("Choose the Fonts for Axis");
        this.setIconImage( new ImageIcon( BIGInterface.getInstance().getBenchItPath()
        		+ File.separator + "gui" + File.separator + "img"
        		+ File.separator + "clock.png" ).getImage() );
        final GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints() ;
        Insets insetsForGridbag = new Insets( 5 , 5 , 5 , 5 );
        c.insets = insetsForGridbag;
        final JPanel fontPane = new JPanel( gridbag );
        fontPane.setPreferredSize( new Dimension( 700, 300 ));
        JPanel buttonPane=new JPanel(new FlowLayout());

        // labels for every font name
        final JLabel jlComment = new JLabel( "plotComment" );
        final JLabel jlTitle = new JLabel( "title" );
        final JLabel jlLegend = new JLabel( "legend" );
        final JLabel jlXAxis = new JLabel( "XAxis" );
        final JLabel jlXAxisTicks = new JLabel( "XAxisTick" );
        final JLabel jlYAxis = new JLabel( "YAxis" );
        final JLabel jlYAxisTicks = new JLabel( "YAxisTick" );
        // an array to store the labels
        final JLabel labels[] = new JLabel[ count ]; //NULLPOINTEREXCEPTION
        labels[0] = jlComment;
        labels[1] = jlTitle;
        labels[2] = jlLegend;
        labels[3] = jlXAxis;
        labels[4] = jlXAxisTicks;
        labels[5] = jlYAxis;
        labels[6] = jlYAxisTicks;

        // create different combo boxes with all available fonts
        String[] availableFonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
        final JComboBox fontBoxComment = new JComboBox( availableFonts );
        final JComboBox fontBoxTitle = new JComboBox( availableFonts );
        final JComboBox fontBoxLegend = new JComboBox( availableFonts );
        final JComboBox fontBoxXAxis = new JComboBox( availableFonts );
        final JComboBox fontBoxXAxisTicks = new JComboBox( availableFonts );
        final JComboBox fontBoxYAxis = new JComboBox( availableFonts );
        final JComboBox fontBoxYAxisTicks = new JComboBox( availableFonts );
        // an array to store the combo boxes
        final JComboBox fontBoxes[] = new JComboBox[ count ];
        fontBoxes[0] = fontBoxComment;
        fontBoxes[1] = fontBoxTitle;
        fontBoxes[2] = fontBoxLegend;
        fontBoxes[3] = fontBoxXAxis;
        fontBoxes[4] = fontBoxXAxisTicks;
        fontBoxes[5] = fontBoxYAxis;
        fontBoxes[6] = fontBoxYAxisTicks;

        // sizes
        String [] siz={"8","9","10","12","14","16","18","20","24","28","32","40"};
        // select size in this combobox
        final JComboBox sizeComment = new JComboBox(siz);
        final JComboBox sizeTitle = new JComboBox(siz);
        final JComboBox sizeLegend = new JComboBox(siz);
        final JComboBox sizeXAxis = new JComboBox(siz);
        final JComboBox sizeXAxisTicks = new JComboBox(siz);
        final JComboBox sizeYAxis = new JComboBox(siz);
        final JComboBox sizeYAxisTicks = new JComboBox(siz);
        // an array to stroe the combo boxes
        final JComboBox[] sizeBoxes = new JComboBox[ count ];
        sizeBoxes[0] = sizeComment;
        sizeBoxes[1] = sizeTitle;
        sizeBoxes[2] = sizeLegend;
        sizeBoxes[3] = sizeXAxis;
        sizeBoxes[4] = sizeXAxisTicks;
        sizeBoxes[5] = sizeYAxis;
        sizeBoxes[6] = sizeYAxisTicks;

        // checkboxes for italic/bold style
        final JCheckBox italComment = new JCheckBox("italic",fonts[0].isItalic());
        final JCheckBox bolComment = new JCheckBox("bold",fonts[0].isBold());
        final JCheckBox italTitle = new JCheckBox("italic",fonts[0].isItalic());
        final JCheckBox bolTitle = new JCheckBox("bold",fonts[0].isBold());
        final JCheckBox italLegend = new JCheckBox("italic",fonts[0].isItalic());
        final JCheckBox bolLegend = new JCheckBox("bold",fonts[0].isBold());
        final JCheckBox italXAxis = new JCheckBox("italic",fonts[0].isItalic());
        final JCheckBox bolXAxis = new JCheckBox("bold",fonts[0].isBold());
        final JCheckBox italXAxisTicks = new JCheckBox("italic",fonts[0].isItalic());
        final JCheckBox bolXAxisTicks = new JCheckBox("bold",fonts[0].isBold());
        final JCheckBox italYAxis = new JCheckBox("italic",fonts[0].isItalic());
        final JCheckBox bolYAxis = new JCheckBox("bold",fonts[0].isBold());
        final JCheckBox italYAxisTicks = new JCheckBox("italic",fonts[0].isItalic());
        final JCheckBox bolYAxisTicks = new JCheckBox("bold",fonts[0].isBold());
        // arrays to store the check boxes
        final JCheckBox[] italBoxes = new JCheckBox[count];
        italBoxes[0] = italComment;
        italBoxes[1] = italTitle;
        italBoxes[2] = italLegend;
        italBoxes[3] = italXAxis;
        italBoxes[4] = italXAxisTicks;
        italBoxes[5] = italYAxis;
        italBoxes[6] = italYAxisTicks;
        final JCheckBox[] bolBoxes = new JCheckBox[count];
        bolBoxes[0] = bolComment;
        bolBoxes[1] = bolTitle;
        bolBoxes[2] = bolLegend;
        bolBoxes[3] = bolXAxis;
        bolBoxes[4] = bolXAxisTicks;
        bolBoxes[5] = bolYAxis;
        bolBoxes[6] = bolYAxisTicks;

        // testfield
        final JLabel fieldComment = new JLabel("Test 1 2 3 2^20") ;
        final JLabel fieldTitle = new JLabel("Test 1 2 3 2^20") ;
        final JLabel fieldLegend = new JLabel("Test 1 2 3 2^20") ;
        final JLabel fieldXAxis = new JLabel("Test 1 2 3 2^20") ;
        final JLabel fieldXAxisTicks = new JLabel("Test 1 2 3 2^20") ;
        final JLabel fieldYAxis = new JLabel("Test 1 2 3 2^20") ;
        final JLabel fieldYAxisTicks = new JLabel("Test 1 2 3 2^20") ;
        // an array to store the testfields
        final JLabel testfields[] = new JLabel[count];
        testfields[0] = fieldComment;
        testfields[1] = fieldTitle;
        testfields[2] = fieldLegend;
        testfields[3] = fieldXAxis;
        testfields[4] = fieldXAxisTicks;
        testfields[5] = fieldYAxis;
        testfields[6] = fieldYAxisTicks;

        // load all settings
        loadSettings( fontBoxes, sizeBoxes, italBoxes, bolBoxes );
        // revalidate all testfields
        for (int i = 0; i < count; i++)
        {
        	revalidateTestfield( fontBoxes[ i ], sizeBoxes[ i ], italBoxes[ i ], bolBoxes[ i ], testfields[ i ] );
        }
        /*
        revalidateTestfield( fontBoxComment, sizeComment, italComment, bolComment, fieldComment );
        revalidateTestfield( fontBoxTitle, sizeTitle, italTitle, bolTitle, fieldTitle );
        revalidateTestfield( fontBoxLegend, sizeLegend, italLegend, bolLegend, fieldLegend );
        revalidateTestfield( fontBoxXAxis, sizeXAxis, italXAxis, bolXAxis, fieldXAxis );
        revalidateTestfield( fontBoxXAxisTicks, sizeXAxisTicks, italXAxisTicks, bolXAxisTicks, fieldXAxisTicks );
        revalidateTestfield( fontBoxYAxis, sizeYAxis, italYAxis, bolYAxis, fieldYAxis );
        revalidateTestfield( fontBoxYAxisTicks, sizeYAxisTicks, italYAxisTicks, bolYAxisTicks, fieldYAxisTicks );
        */

        // add all actioin listener
        // action listener for comment
        fontBoxComment.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( ( (JComboBox) aevt.getSource() ), sizeComment, italComment, bolComment, fieldComment );
        			}
        		});
        sizeComment.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxComment, ( (JComboBox) aevt.getSource() ), italComment, bolComment, fieldComment );
        			}
        		});
        italComment.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxComment, sizeComment, ( (JCheckBox) aevt.getSource() ), bolComment, fieldComment );
        			}
        		});
        bolComment.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxComment, sizeComment, italComment, ( (JCheckBox) aevt.getSource() ), fieldComment );
        			}
        		});
        // action listener for title
        fontBoxTitle.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( ( (JComboBox) aevt.getSource() ), sizeTitle, italTitle, bolTitle, fieldTitle );
        			}
        		});
        sizeTitle.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxTitle, ( (JComboBox) aevt.getSource() ), italTitle, bolTitle, fieldTitle );
        			}
        		});
        italTitle.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxTitle, sizeTitle, ( (JCheckBox) aevt.getSource() ), bolTitle, fieldTitle );
        			}
        		});
        bolTitle.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxTitle, sizeTitle, italTitle, ( (JCheckBox) aevt.getSource() ), fieldTitle );
        			}
        		});
        // action listener for legend
        fontBoxLegend.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( ( (JComboBox) aevt.getSource() ), sizeLegend, italLegend, bolLegend, fieldLegend );
        			}
        		});
        sizeLegend.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxLegend, ( (JComboBox) aevt.getSource() ), italLegend, bolLegend, fieldLegend );
        			}
        		});
        italLegend.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxLegend, sizeLegend, ( (JCheckBox) aevt.getSource() ), bolLegend, fieldLegend );
        			}
        		});
        bolLegend.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxLegend, sizeLegend, italLegend, ( (JCheckBox) aevt.getSource() ), fieldLegend );
        			}
        		});
        // action listener for xAxis
        fontBoxXAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( ( (JComboBox) aevt.getSource() ), sizeXAxis, italXAxis, bolXAxis, fieldXAxis );
        			}
        		});
        sizeXAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxXAxis, ( (JComboBox) aevt.getSource() ), italXAxis, bolXAxis, fieldXAxis );
        			}
        		});
        italXAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxXAxis, sizeXAxis, ( (JCheckBox) aevt.getSource() ), bolXAxis, fieldXAxis );
        			}
        		});
        bolXAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxXAxis, sizeXAxis, italXAxis, ( (JCheckBox) aevt.getSource() ), fieldXAxis );
        			}
        		});
        // action listener for xAxisTicks
        fontBoxXAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( ( (JComboBox) aevt.getSource() ), sizeXAxisTicks, italXAxisTicks, bolXAxisTicks, fieldXAxisTicks );
        			}
        		});
        sizeXAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxXAxisTicks, ( (JComboBox) aevt.getSource() ), italXAxisTicks, bolXAxisTicks, fieldXAxisTicks );
        			}
        		});
        italXAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxXAxisTicks, sizeXAxisTicks, ( (JCheckBox) aevt.getSource() ), bolXAxisTicks, fieldXAxisTicks );
        			}
        		});
        bolXAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxXAxisTicks, sizeXAxisTicks, italXAxisTicks, ( (JCheckBox) aevt.getSource() ), fieldXAxisTicks );
        			}
        		});
        // action listener for yAxis
        fontBoxYAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( ( (JComboBox) aevt.getSource() ), sizeYAxis, italYAxis, bolYAxis, fieldYAxis );
        			}
        		});
        sizeYAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxYAxis, ( (JComboBox) aevt.getSource() ), italYAxis, bolYAxis, fieldYAxis );
        			}
        		});
        italYAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxYAxis, sizeYAxis, ( (JCheckBox) aevt.getSource() ), bolYAxis, fieldYAxis );
        			}
        		});
        bolYAxis.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxYAxis, sizeYAxis, italYAxis, ( (JCheckBox) aevt.getSource() ), fieldYAxis );
        			}
        		});
        // action listener for yAxisTicks
        fontBoxYAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( ( (JComboBox) aevt.getSource() ), sizeYAxisTicks, italYAxisTicks, bolYAxisTicks, fieldYAxisTicks );
        			}
        		});
        sizeYAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxYAxisTicks, ( (JComboBox) aevt.getSource() ), italYAxisTicks, bolYAxisTicks, fieldYAxisTicks );
        			}
        		});
        italYAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxYAxisTicks, sizeYAxisTicks, ( (JCheckBox) aevt.getSource() ), bolYAxisTicks, fieldYAxisTicks );
        			}
        		});
        bolYAxisTicks.addActionListener(new ActionListener()
        		{
        			public void actionPerformed( ActionEvent aevt )
        			{
        				revalidateTestfield( fontBoxYAxisTicks, sizeYAxisTicks, italYAxisTicks, ( (JCheckBox) aevt.getSource() ), fieldYAxisTicks );
        			}
        		});

        BIGUtility.setConstraints( c , 0 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , jlComment , gridbag , c ) ;
        BIGUtility.setConstraints( c , 1 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fontBoxComment , gridbag , c ) ;
        BIGUtility.setConstraints( c , 2 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , sizeComment , gridbag , c ) ;
        BIGUtility.setConstraints( c , 3 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , italComment , gridbag , c ) ;
        BIGUtility.setConstraints( c , 4 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , bolComment , gridbag , c ) ;
        BIGUtility.setConstraints( c , 5 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fieldComment , gridbag , c ) ;
        row++;

        BIGUtility.setConstraints( c , 0 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , jlTitle , gridbag , c ) ;
        BIGUtility.setConstraints( c , 1 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fontBoxTitle , gridbag , c ) ;
        BIGUtility.setConstraints( c , 2 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , sizeTitle , gridbag , c ) ;
        BIGUtility.setConstraints( c , 3 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , italTitle , gridbag , c ) ;
        BIGUtility.setConstraints( c , 4 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , bolTitle , gridbag , c ) ;
        BIGUtility.setConstraints( c , 5 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fieldTitle , gridbag , c ) ;
        row++;

        BIGUtility.setConstraints( c , 0 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , jlLegend , gridbag , c ) ;
        BIGUtility.setConstraints( c , 1 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fontBoxLegend , gridbag , c ) ;
        BIGUtility.setConstraints( c , 2 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , sizeLegend , gridbag , c ) ;
        BIGUtility.setConstraints( c , 3 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , italLegend , gridbag , c ) ;
        BIGUtility.setConstraints( c , 4 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , bolLegend , gridbag , c ) ;
        BIGUtility.setConstraints( c , 5 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fieldLegend , gridbag , c ) ;
        row++;

        BIGUtility.setConstraints( c , 0 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , jlXAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 1 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fontBoxXAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 2 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , sizeXAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 3 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , italXAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 4 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , bolXAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 5 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fieldXAxis , gridbag , c ) ;
        row++;

        BIGUtility.setConstraints( c , 0 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , jlXAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 1 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fontBoxXAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 2 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , sizeXAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 3 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , italXAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 4 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , bolXAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 5 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fieldXAxisTicks , gridbag , c ) ;
        row++;

        BIGUtility.setConstraints( c , 0 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , jlYAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 1 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fontBoxYAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 2 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , sizeYAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 3 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , italYAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 4 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , bolYAxis , gridbag , c ) ;
        BIGUtility.setConstraints( c , 5 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fieldYAxis , gridbag , c ) ;
        row++;

        BIGUtility.setConstraints( c , 0 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , jlYAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 1 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fontBoxYAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 2 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , sizeYAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 3 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , italYAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 4 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , bolYAxisTicks , gridbag , c ) ;
        BIGUtility.setConstraints( c , 5 , row , 1 , 1 ) ;
        BIGUtility.addComponent( fontPane , fieldYAxisTicks , gridbag , c ) ;
        row++;
        // adding use always standard fonts
        boolean useStandard=false;
      try
      {
         useStandard=BIGInterface.getInstance().getBIGConfigFileParser().boolCheckOut(
             "useStandardFonts" ) ;
      }
      catch ( Exception ex )
      {
         BIGInterface.getInstance().getBIGConfigFileParser().addEntry("useStandardFonts","0");
      }
        final JCheckBox cbStandard=new JCheckBox("Always use standard fonts",useStandard);
        cbStandard.addActionListener(new ActionListener()
            {
               public void actionPerformed(ActionEvent ae)
               {
                  String bool="0";
                  if (cbStandard.isSelected())
                     bool="1";
                  BIGInterface.getInstance().getBIGConfigFileParser().set("useStandardFonts",bool);
               }
            });
            c.anchor=GridBagConstraints.CENTER;
        BIGUtility.setConstraints( c , 0 , row , 6 , 1 ) ;
        BIGUtility.addComponent( fontPane , cbStandard , gridbag , c ) ;
        row++;


        this.getContentPane().add(fontPane,BorderLayout.NORTH);
        JButton ok = new JButton( "Okay" ) ;
        // when okay is pressed
        ok.addActionListener( new ActionListener()
        {
           public void actionPerformed( ActionEvent ae )
           {
        	  if (plotable != null)
        	  {
        		  // if there is a plotable -> use it
            	  for (int i = 0; i < count; i++)
            	  {
            		  plotable.setFont(BIGPlotable.fontNames[ i ], testfields[i].getFont() );
            	  }
        		  /*
        		  // set the font for Comment ( BIGPlotable.fontNames[ 0 ] )
            	  plotable.setFont(BIGPlotable.fontNames[ 0 ], fieldComment.getFont() );
            	  // set the font for Title ( BIGPlotable.fontNames[ 1 ] )
            	  plotable.setFont(BIGPlotable.fontNames[ 1 ], fieldTitle.getFont() );
            	  // set the font for Legend ( BIGPlotable.fontNames[ 2 ] )
            	  plotable.setFont(BIGPlotable.fontNames[ 2 ], fieldLegend.getFont() );
            	  // set the font for XAxis ( BIGPlotable.fontNames[ 3 ] )
            	  plotable.setFont(BIGPlotable.fontNames[ 3 ], fieldXAxis.getFont() );
            	  // set the font for XAxisTick ( BIGPlotable.fontNames[ 4 ] )
            	  plotable.setFont(BIGPlotable.fontNames[ 4 ], fieldXAxisTicks.getFont() );
            	  // set the font for YAxis ( BIGPlotable.fontNames[ 5 ] )
            	  plotable.setFont(BIGPlotable.fontNames[ 5 ], fieldYAxis.getFont() );
            	  // set the font for YAxisTick ( BIGPlotable.fontNames[ 6 ] )
            	  plotable.setFont(BIGPlotable.fontNames[ 6 ], fieldYAxisTicks.getFont() );
            	  */
        	  }
        	  else
        	  {
        		  // if no plotable is definded, then use the BIGConfigFileParser
        		  for (int i = 0; i < BIGPlotable.fontNames.length; i++)
                  {
                     bigInterface.getBIGConfigFileParser().set( BIGPlotable.fontNames[ i ] + "Font" ,
                         testfields[ i ].getFont().getName() ) ;

                     bigInterface.getBIGConfigFileParser().set( BIGPlotable.fontNames[ i ] + "FontStyle" ,
                         ""+testfields[ i ].getFont().getStyle() ) ;

                     bigInterface.getBIGConfigFileParser().set( BIGPlotable.fontNames[ i ] + "FontSize" ,
                         ""+testfields[ i ].getFont().getSize() ) ;
                  }
                  //-----
        		  // and save actual changes to the BGUI.cfg-file
                  BIGInterface.getInstance().getBIGConfigFileParser().save();
                  //-----

        	  }

        	  if (al != null)
        	  {
        		  al.actionPerformed( null );
        	  }
        	  // finally close the frame
        	  BIGFontDialog.this.setVisible( false );
           }
        } );

        // if closed do nothing but close
        JButton cancel = new JButton( "Close" ) ;
        cancel.addActionListener( new ActionListener()
        {
           public void actionPerformed( ActionEvent ae )
           {
              BIGFontDialog.this.setVisible( false ) ;
           }
        } ) ;

        // add buttons
        buttonPane.add(ok);
        buttonPane.add(cancel);
        this.getContentPane().add(buttonPane,BorderLayout.SOUTH);
        this.pack();
    }

    /**
     * load previous settings for every item in BIGPlotable.fontNames[]
     * @param fontBoxes JComboBox[] the combo-boxes to select the font-name
     * @param sizeBoxes JComboBox[] the combo-boxes to select the font-size
     * @param italBoxes JCheckBox[] the check-boxes to define an italic-style
     * @param bolBoxes JCheckBox[] the check-boxes to define a bold-style
     */
	private void loadSettings(JComboBox[] fontBoxes, JComboBox[] sizeBoxes, JCheckBox[] italBoxes, JCheckBox[] bolBoxes)
	{
		for (int i = 0; i < count; i++)
		{
	    	// set the selected font family
		    fontBoxes[ i ].setSelectedItem( fonts[ i ].getFamily() ); //.getFontName() ) ;
			// get selected sizes
		    for ( int j = 0 ; j < sizeBoxes[i].getItemCount() ; j++ )
		       if ( ( "" +
		              fonts[ i ].getSize() ).equals( sizeBoxes[i].getItemAt( j ) ) )
		          sizeBoxes[i].setSelectedIndex( j ) ;
		    // get selected stayle (bold, italic)
		    italBoxes[i].setSelected(fonts[ i ].isItalic());
		    bolBoxes[i].setSelected(fonts[ i ].isBold());
		}
	}

    /**
     * revalidate text in the JLabel to test actual settings
     *
     * @param fontBox JComboBox defines the font-name
     * @param size JComboBox defines the font-size
     * @param ital JCheckBox defines if the style of this font is italic
     * @param bol JCheckBox defines if the style of this font is bold
     * @param field JLabel contains the text to test actual settings
     */
    private void revalidateTestfield(JComboBox fontBox, JComboBox size, JCheckBox ital, JCheckBox bol, JLabel field)
	{
    	int style = 0 ;
	    if ( ital.isSelected() )
	       style = style | Font.ITALIC ;
	    if ( bol.isSelected() )
	       style = style | Font.BOLD ;
	    // set the font for the testfield
	    new Font( 	( String ) fontBox.getSelectedItem() ,
					style ,
					Integer.parseInt( ( String )size.getSelectedItem() ) );
	    field.setFont( 	new Font( 	( String ) fontBox.getSelectedItem() ,
	    				style ,
						Integer.parseInt( ( String )size.getSelectedItem() ) ) ) ;
	    // repaint testfield
	    field.revalidate() ;
	 }
}

/********************************************************************
 * Log-History
 *
 * $Log: BIGFontDialog.java,v $
 * Revision 1.6  2007/07/10 10:41:30  tschuet
 * actualize the gui-icon
 *
 * Revision 1.5  2007/07/03 11:28:26  tschuet
 * insert a new benchit icon into the frames
 *
 * Revision 1.4  2007/05/08 09:39:56  tschuet
 * reorganize imports
 *
 * Revision 1.3  2007/02/27 12:37:50  tschuet
 * different bugfixes (mixer, remote folder -> see meeting at february, 27th)
 *
 * Revision 1.2  2006/12/18 08:42:36  rschoene
 * use standard font can be set
 *
 * Revision 1.1  2006/12/18 08:09:34  rschoene
 * new FontDialog
 *
 *
 *******************************************************************/
