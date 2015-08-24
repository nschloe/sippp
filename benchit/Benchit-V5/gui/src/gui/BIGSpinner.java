/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGSpinner.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.1 $
*  $Date: 2005/10/20 13:13:57 $
*
******************************************************************************/
package gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.plaf.basic.*;

/**
 * This Component creates a spinner for a specified TextField, which increase or decrease
 * the value of the TextField (Integer, Float, Double, ...) by clicking the ArrowUp or ArrowDown
 * button.<br>
 * You can set, how big one strut is.<br><br>
 * I.e. the value of the TextField is 5.3 the strut is 0.4, so the next value after
 * pressing the up button is 5.7, 6.1, ...
 *
 * @author Carsten Luxig, c.luxig@lmcsoft.com
 * @see BIGTextField
 */

public class BIGSpinner extends JPanel {
	/**
	* Creates a spinner for the <code>BIGTextField</code> and the size of one strut.
	* @param jtf     the <code>BIGTextField</code>
	* @param strut   the size for 1 strut
	*/
	public BIGSpinner(BIGTextField jtf, float strut) {
		int type = jtf.getType();
		createSpinner(jtf, strut, type);
	}

	/**
	* This method creates the Spinner.
	*/
	private void createSpinner(
		BIGTextField field,
		float strutValue,
		int type) {
		final int fieldType = type;
		jtf = field;
		final float strut = strutValue;
		// Button to increase the value of the TextField
		incButton = new BasicArrowButton(SwingConstants.NORTH);
		incButton.addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				mousePressed = true;
				Thread runner = new Thread() {
					public void run() {
						int counter = 0;
						while (mousePressed && isEnabled()) {
							if (fieldType == BIGTextField.INTEGER) {
								int value = jtf.getValue().intValue();
								value += new Float(strut).intValue();
								if (jtf.getMaximum() != null) {
									int maximum = jtf.getMaximum().intValue();
									if (value > maximum)
										jtf.setText(Integer.toString(maximum));
									else
										jtf.setText(Integer.toString(value));
								} else
									jtf.setText(Integer.toString(value));
							} else if (fieldType == BIGTextField.FLOAT) {
								float value = jtf.getValue().floatValue();
								value += strut;
								if (jtf.getMaximum() != null) {
									float maximum =
										jtf.getMaximum().floatValue();
									if (value > maximum)
										jtf.setText(Float.toString(maximum));
									else
										jtf.setText(Float.toString(value));
								} else
									jtf.setText(Float.toString(value));
							}
							try {
								if (counter < 5)
									sleep(200);
								else if (counter < 30)
									sleep(100);
								else if (counter < 60)
									sleep(50);
								else if (counter >= 60)
									sleep(20);
							} catch (InterruptedException ie) {
							}
							counter++;
						}
					}
				};
				runner.start();
			}
			public void mouseReleased(MouseEvent e) {
				mousePressed = false;
			}
		});

		// Button to decrease the value of the TextField
		decButton = new BasicArrowButton(SwingConstants.SOUTH);
		decButton.addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				mousePressed = true;
				Thread runner = new Thread() {
					public void run() {
						int counter = 0;
						while (mousePressed && isEnabled()) {
							if (fieldType == BIGTextField.INTEGER) {
								int value = jtf.getValue().intValue();
								value -= new Float(strut).intValue();
								if (jtf.getMinimum() != null) {
									int minimum = jtf.getMinimum().intValue();
									if (value < minimum)
										jtf.setText(Integer.toString(minimum));
									else
										jtf.setText(Integer.toString(value));
								} else
									jtf.setText(Integer.toString(value));
							} else if (fieldType == BIGTextField.FLOAT) {
								float value = jtf.getValue().floatValue();
								value -= strut;
								if (jtf.getMinimum() != null) {
									float minimum =
										jtf.getMaximum().floatValue();
									if (value < minimum)
										jtf.setText(Float.toString(minimum));
									else
										jtf.setText(Float.toString(value));
								} else
									jtf.setText(Float.toString(value));
							}
							try {
								if (counter < 5)
									sleep(200);
								else if (counter < 30)
									sleep(100);
								else if (counter < 60)
									sleep(50);
								else if (counter >= 60)
									sleep(20);
							} catch (InterruptedException ie) {
							}
							counter++;
						}
					}
				};
				runner.start();
			}
			public void mouseReleased(MouseEvent e) {
				mousePressed = false;
			}
		});
		setLayout(new BorderLayout(3, 0));
		add("West", jtf);
		JPanel p1 = new JPanel(new GridLayout(2, 1, 0, 2));
		p1.add(incButton);
		p1.add(decButton);
		add(p1);
	}
	// interface

	/**
	* Setting the size means to set preferred size of the buttons.
	* @param d   the dimension of the <b>whole</b> <code>BIGTextField</code> inclusive the <code>BIGSpinner</code>
	*/
	public void setSize(Dimension d) {
        setPreferredSize(d);
		Dimension buttonDim =
			new Dimension(incButton.getWidth(), d.height / 2 - 1);
		Insets in = new Insets(0, 0, 0, 0);
		incButton.setMargin(in);
		incButton.setPreferredSize(buttonDim);
		decButton.setMargin(in);
		decButton.setPreferredSize(buttonDim);
	}

	/**
	* Sets whether the Spinner is enabled or not.
	* @param enable   if disabled you cant modify this <code>BIGTextField</code>.   *
	*/
	public void setEnabled(boolean enable) {
		incButton.setEnabled(enable);
		decButton.setEnabled(enable);
		jtf.setEnabled(enable);
	}

	/**
	* Check whether the <code>BIGSpinner</code> is enabled or not.
	* @return enable state.
	*/
	public boolean isEnabled() {
		return jtf.isEnabled() & incButton.isEnabled() & decButton.isEnabled();
	}

	/**
	* You can add an <code>ActionListener</code> which occurs for <b>both</b>
	* buttons (inc. and dec. button).
	* @param l   the <code>ActionListener</code>
	*/
	public void addActionListener(ActionListener l) {
		incButton.addActionListener(l);
		decButton.addActionListener(l);
	}

	/**
	* Sets the value to the corresponding <code>BIGTextField</code>
	* @param value   the new value for the text field
	*/
	public void setValue(String value) {
		jtf.setValue(value);
	}

	/**
	 * Returns the text from the corresponding <code>BIGTextField</code>
	 * @return the text from the corresponding <code>BIGTextField</code>
	 */
	public String getText() {
		return jtf.getText();
	}

	/**
	 * Returns the corresponding <code>BIGTextField</code>
	 * @return the corresponding <code>BIGTextField</code>
	 */
	public BIGTextField getTextField() {
		return jtf;
	}

    public void setTextField(BIGTextField field) {
        jtf = field;
	}

	// variables
	private BasicArrowButton incButton, decButton;
	private BIGTextField jtf;
	private boolean mousePressed = true;
}
/*****************************************************************************
Log-History

*****************************************************************************/
