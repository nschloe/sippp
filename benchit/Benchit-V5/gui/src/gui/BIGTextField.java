/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGTextField.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.3 $
*  $Date: 2007/01/15 06:49:26 $
*
******************************************************************************/
package gui;

import javax.swing.*;
import java.awt.event.*;
import java.util.*;

/**
 * This creates a TextField by a specified type. So only type-specified values where accepted,
 * otherwise an error occurs. You can combine this <code>BIGTextField</code> with a <code>BIGSpinner</code>
 * so you can spinn your values (useful for number formats).
 * <br><br>
 * It is possible to set the minimum and maximum value for the TextField.
 *
 * @author Carsten Luxig, c.luxig@lmcsoft.com
 * @see BIGSpinner
 */
public class BIGTextField
	extends JTextField
	implements ActionListener, FocusListener {
	/** The <code>BIGTextField</code> only accept integer-values for this TextField. */
	public static final int INTEGER = 0;
	/** The <code>BIGTextField</code> only accept float-values for this TextField. */
	public static final int FLOAT = 1;
	/** The <code>BIGTextField</code> accept any values for this TextField. */
	public static final int TEXT = 2;
	/** The <code>BIGTextField</code> only accept double-values for this TextField. */
	public static final int DOUBLE = 3;
	/** The <code>BIGTextField</code> only accept currency-values for this TextField. */
	public static final int CURRENCY = 4;
	/** The <code>BIGTextField</code> only accept percent-values for this TextField. */
	public static final int PERCENT = 5;
	/** The <code>BIGTextField</code> only accept date-values for this TextField. */
	public static final int DATE = 6;

	/**
	 * Create a TextField with a value and a specified type for this value.
	 * @param value   the value of the TextField
	 * @param type    the type of the <code>BIGTextField</code>
	 */
	public BIGTextField(Object value, int type) {
		super();
		oldValue = "";
		fieldType = type;
		decimal = fieldType == CURRENCY ? 2 : -1;
		setValue(value);
		addActionListener(this);
		addFocusListener(this);
	}

	/**
	 * Create a TextField with a value, the number of columns and a specified type for this value.
	 * @param value   the value of the TextField
	 * @param col    the number of columns of the TextField
	 * @param type    the type of the <code>BIGTextField</code>
	 */
	public BIGTextField(Object value, int col, int type) {
		super(col);
		oldValue = "";
		fieldType = type;
		decimal = fieldType == CURRENCY ? 2 : -1;
		setValue(value);
		addActionListener(this);
		addFocusListener(this);
	}

	/**
	 * Create a TextField with a value, the number of columns and a specified type for this value.
	 * Although you can decide if the <code>BIGTextField</code> must have a value or not.
	 * @param value   the value of the TextField
	 * @param col    the number of columns of the TextField
	 * @param type    the type of the <code>BIGTextField</code>
	 * @param valueNecessary    is a value always necessary or not (true or false).
	 */
	public BIGTextField(
		Object value,
		int col,
		int type,
		boolean valueNecessary) {
		super(col);
		oldValue = "";
		fieldType = type;
		decimal = fieldType == CURRENCY ? 2 : -1;
		setValue(value);
		addActionListener(this);
		addFocusListener(this);
		valueIsNecessary(valueNecessary);
	}
	/**
	 * Create a TextField the number of columns and a specified type for this value.
	 * @param col    the number of columns of the TextField
	 * @param type    the type of the <code>BIGTextField</code>
	 */
	public BIGTextField(int col, int type) {
		super(col);
		oldValue = "";
		fieldType = type;
		decimal = fieldType == CURRENCY ? 2 : -1;
		addActionListener(this);
		addFocusListener(this);
	}

	/**
	 * Create a TextField the number of columns.
	 * The default type is FLOAT.
	 * @param col    the number of columns of the TextField
	 */
	public BIGTextField(int col) {
		super(col);
		oldValue = "";
		fieldType = FLOAT;
		addActionListener(this);
		addFocusListener(this);
	}

	/**
	 * Create a TextField.
	 * The default type is FLOAT.
	 */
	public BIGTextField() {
		super();
		oldValue = "";
		fieldType = FLOAT;
		addActionListener(this);
		addFocusListener(this);
	}

	/** Only intern use. */
	public void actionPerformed(ActionEvent e) {
		checkFormat(getText());
	}
	/** Only intern use. */
	public void focusGained(FocusEvent e) {
		oldValue = getText();
	}
	/** Only intern use. */
	public void focusLost(FocusEvent e) {
		if (!errorMessage)
			checkFormat(getText());
	}

	/*
	 * Checks if the value of the TextField has a correct format.
	 */
	private void checkFormat(Object objToCheck) {
           String toCheck="";
           if (objToCheck!=null)
            toCheck=objToCheck.toString();
		text = toCheck;
		if (!text.equals("")) {
			try {
				if (fieldType == INTEGER) {
                                   if (objToCheck!=null)
                                   {
                                      int i = Integer.parseInt( text ) ;
                                      if ( minValue != null )
                                      {
                                         int min = minValue.intValue() ;
                                         if ( i < min )
                                            printError( MINIMUM_ERROR ) ;
                                      }
                                      if ( maxValue != null )
                                      {
                                         int max = maxValue.intValue() ;
                                         if ( i > max )
                                            printError( MAXIMUM_ERROR ) ;
                                      }
                                      setText( "" + i ) ;
                                   } else
                                   {
                                      setText("");
                                   }
				} else if (fieldType == FLOAT) {
					text = text.replace(',', '.');
					int index = text.indexOf(".");
					if (index != -1) {
						String suffix =
							(index == 0 ? "0" : text.substring(0, index));
						String prefix = text.substring(index + 1);
						text = suffix + "." + prefix;
					}
					float f = Float.parseFloat(text);
					if (minValue != null) {
						float min = minValue.floatValue();
						if (f < min)
							printError(MINIMUM_ERROR);
					}
					if (maxValue != null) {
						float max = maxValue.floatValue();
						if (f > max)
							printError(MAXIMUM_ERROR);
					}
					setText(new Float(f).toString());
				} else if (fieldType == DOUBLE) {
					text = text.replace(',', '.');
					int index = text.indexOf(".");
					if (index != -1) {
						String suffix =
							(index == 0 ? "0" : text.substring(0, index));
						String prefix = text.substring(index + 1);
						text = suffix + "." + prefix;
					}
					double f = Double.parseDouble(text);
					if (minValue != null) {
						double min = minValue.doubleValue();
						if (f < min)
							printError(MINIMUM_ERROR);
					}
					if (maxValue != null) {
						double max = maxValue.doubleValue();
						if (f > max)
							printError(MAXIMUM_ERROR);
					}
					setText(new Double(f).toString());
				} else if (fieldType == CURRENCY || fieldType == PERCENT) {
					text = text.replace(',', '.');
					int index = text.indexOf(".");
					if (index != -1) {
						String suffix =
							(index == 0 ? "0" : text.substring(0, index));
						String prefix = text.substring(index + 1);
						text = suffix + "." + prefix;
					}
					String str = buildCurrency(text, getDecimalCount());
					double f = Double.parseDouble(str);
					if (minValue != null) {
						double min = minValue.doubleValue();
						if (f < min)
							printError(MINIMUM_ERROR);
					}
					if (maxValue != null) {
						double max = maxValue.doubleValue();
						if (f > max)
							printError(MAXIMUM_ERROR);
					}
					setText(str);
				} else if (fieldType == TEXT) {
					setText(text);
				} else if (fieldType == DATE) {
					Calendar cal = buildDate(text);
					if (cal != null && minDate != null && cal.before(minDate))
						printError(MINIMUM_ERROR);
					else if (cal != null)
						setDate(cal);
					else
						printError(DATE_ERROR);
				}
				//        setText(text);
				oldValue = getText();
				for (int count = 0; count < connectionList.size(); count++) {
					((JTextField) connectionList.get(count)).setText(oldValue);
				}
			} catch (NumberFormatException nfe) {
				printError(NUMBER_FORMAT_ERROR);
			}
		} else if (valueNecessary)
			printError(VALUE_IS_NESSESSARY_ERROR);
	}

	/**
	 * Prints the error by displaying an <code>JOptionPane</code>.
	 */
	private void printError(int errorType) {
		errorMessage = true;
		if (errorType == NUMBER_FORMAT_ERROR) {
            JOptionPane.showMessageDialog(null, "The value has an invalidate format.", "Error", JOptionPane.ERROR_MESSAGE);
        }
        else if (errorType == VALUE_IS_NESSESSARY_ERROR) {
            JOptionPane.showMessageDialog(null,"You have to insert a value.","Error",JOptionPane.ERROR_MESSAGE);
        }
        else if (getType() == INTEGER) {
			if (errorType == MINIMUM_ERROR) {
				if (getMaximum() != null)
                    JOptionPane.showMessageDialog(null,"The value is not in the range "
                            + getMinimum().intValue() + " till "
                            + getMaximum().intValue() + ".", "Error", JOptionPane.ERROR_MESSAGE);
				else
                    JOptionPane.showMessageDialog(null,"The value must be greater than "+ getMinimum().intValue(), "Error",JOptionPane.ERROR_MESSAGE);
            }
            else if (errorType == MAXIMUM_ERROR) {
				if (getMinimum() != null)
                    JOptionPane.showMessageDialog(null,"The value is not in the range "
                            + getMinimum().intValue() + " till "
                            + getMaximum().intValue() + ".", "Error", JOptionPane.ERROR_MESSAGE);
				else
                    JOptionPane.showMessageDialog(null,"The value must be smaller than "+ getMaximum().intValue(), "Error",JOptionPane.ERROR_MESSAGE);
			}
        }
        else if (getType() == FLOAT) {
			if (errorType == MINIMUM_ERROR) {
				if (getMaximum() != null)
					JOptionPane.showMessageDialog(
                        null,"The value is not in the range "+ getMinimum()+ " till "
                            + getMaximum()+ ".",  "Error",JOptionPane.ERROR_MESSAGE);
				else
                    JOptionPane.showMessageDialog(null,"The value must be greater than " + getMinimum(), "Error",JOptionPane.ERROR_MESSAGE);
            }
            else if (errorType == MAXIMUM_ERROR) {
				if (getMinimum() != null)
					JOptionPane.showMessageDialog(
                        null,"The value is not in the range "+ getMinimum()+ " till "
                            + getMaximum()+ ".",  "Error",JOptionPane.ERROR_MESSAGE);
				else
                    JOptionPane.showMessageDialog(null,"The value must be smaller than " + getMaximum(), "Error",JOptionPane.ERROR_MESSAGE);
			}
        }
        else if ( getType() == DOUBLE || getType() == CURRENCY || getType() == PERCENT) {
			if (errorType == MINIMUM_ERROR) {
				if (getMaximum() != null)
                    JOptionPane.showMessageDialog(null,"The value is not in the range "
                            + getMinimum().doubleValue() + " till "
                            + getMaximum().doubleValue() + ".", "Error", JOptionPane.ERROR_MESSAGE);
				else
                    JOptionPane.showMessageDialog(null,"The value must be greater than " + getMinimum().doubleValue(), "Error",JOptionPane.ERROR_MESSAGE);
            }
            else if (errorType == MAXIMUM_ERROR) {
				if (getMinimum() != null)
                    JOptionPane.showMessageDialog(null,"The value is not in the range "
                            + getMinimum().doubleValue() + " till "
                            + getMaximum().doubleValue() + ".", "Error", JOptionPane.ERROR_MESSAGE);
				else
                    JOptionPane.showMessageDialog(null,"The value must be greater than " + getMaximum().doubleValue(), "Error",JOptionPane.ERROR_MESSAGE);
			}
        }
        else if (getType() == DATE) {
			if (errorType == DATE_ERROR)
                JOptionPane.showMessageDialog(null, "The value is not a validate date.","Error",JOptionPane.ERROR_MESSAGE);
			else if (errorType == MINIMUM_ERROR)
                JOptionPane.showMessageDialog(null,"The date must be after today", "Error",JOptionPane.ERROR_MESSAGE);
		}
		errorMessage = false;
		setText(oldValue);
		requestFocus();
	}

	/**
	 * Rounds the double by the number of decimals.
	 */
	private Double round(Double d, int decimal) {
		String help = d.toString();
		if (decimal <= -1)
			return d;
		int index = help.indexOf(".");
		int suffixNumber = Integer.parseInt(help.substring(0, index));
		if (help.length() - index <= decimal + 1)
			return d;
		else {
			String prefix = help.substring(index + 1, index + decimal + 2);
			String suffix = "";
			int[] values = new int[decimal + 1];
			for (int i = 0; i < prefix.length(); i++)
				if (i <= decimal)
					values[i] = Integer.parseInt(prefix.substring(i, i + 1));
			boolean roundNext;
			int i = decimal;
			if (values[i] >= 5)
				roundNext = true;
			else
				roundNext = false;
			while (roundNext) {
				if (i <= 0) {
					suffixNumber++;
					roundNext = false;
					break;
				} else
					values[i - 1]++;
				if (values[i - 1] == 10)
					values[i - 1] = 0;
				else
					roundNext = false;
				i--;
			}
			prefix = "";
			for (i = 0; i < decimal; i++)
				prefix += values[i];
			suffix += suffixNumber;
			help = suffix + "." + prefix;
			return new Double(help);
		}
	}

	/**
	 * Creates a correct currency String.
	 */
	private String buildCurrency(String str, int decimal) {
		String help = str.replace(',', '.');
		int index = help.indexOf(".");
		if (index == -1)
			return help + ".00";
		int suffixNumber = Integer.parseInt(help.substring(0, index));
		if (help.length() - index > decimal + 1) {
			Double d = round(new Double(help), decimal);
			help = d.toString();
		}
		if (help.length() - index == 1)
			return help + "00";
		if (help.length() - index == 2)
			return help + "0";
		if (help.length() - index >= decimal)
			return help;
		return null;
	}

	/**
	 * Creates a correct date value.
	 */
	private Calendar buildDate(String text) {
		try {
			int date, month, year;
			String[] split = split(text, ".");
			if (split.length < 3)
				split = split(text, ",");
			if (split.length < 3)
				split = split(text, "/");
			if (split.length < 3)
				split = split(text, "\\");
			if (split.length < 3)
				split = split(text, ":");
			if (split.length == 3) {
				date = Integer.parseInt(split[0]);
				month = Integer.parseInt(split[1]) - 1;
				year = buildYear(split[2]);
				if (date > 0 && month >= 0 && year >= 0)
					return new GregorianCalendar(year, month, date);
			}
			if (text.length() == 6 || text.length() == 8) {
				date = Integer.parseInt(removeZero(text.substring(0, 2)));
				month = Integer.parseInt(removeZero(text.substring(2, 4))) - 1;
				year = buildYear(text.substring(4, text.length()));
				if (date > 0 && month >= 0 && year >= 0)
					return new GregorianCalendar(year, month, date);
			}
			return null;
		} catch (NumberFormatException nfe) {
			return null;
		}
	}

	/**
	 * Removes the nulls.
	 */
	private String removeZero(String number) {
		try {
			while (number.substring(0, 1).equals("0"))
				number = number.substring(1, number.length());
			return number;
		} catch (StringIndexOutOfBoundsException sioobe) {
			return "x";
		}
	}

	/**
	 * Creates the year by the max. length.
	 */
	private int buildYear(String number) {
		try {
			int year = 0;
			if (number.length() == 1) {
				year = Integer.parseInt(number);
				return 2000 + year;
			}
			if (number.length() <= 3) {
				year = Integer.parseInt(removeZero(number));
				if (year < 80)
					return 2000 + year;
				else
					return 1900 + year;
			}
			if (number.length() == 4) {
				year = Integer.parseInt(number);
				return year;
			}
		} catch (NumberFormatException nfe) {
		}
		return 0;
	}

	/** A given text where splited by the given letters.
	 * @param splitText The text which should be splited
	 * @param splitString The letters, the splitText will be searched.
	 * @return The splited text as an array of Strings.
	 */
	private String[] split(String splitText, String splitString) {
		if (splitText != null && splitText.length() > 0) {
			int indexOf = splitText.indexOf(splitString);
			int marker = 0;
			Vector vec = new Vector();
			if (indexOf == -1)
				vec.add(splitText);
			else {
				String next = "";
				while (indexOf != -1) {
					next = splitText.substring(marker, indexOf);
					vec.add(next);
					marker = indexOf + splitString.length();
					indexOf = splitText.indexOf(splitString, marker);
				}
				next = splitText.substring(marker, splitText.length());
				vec.add(next);
			}
			String[] searches = new String[vec.size()];
			for (int i = 0; i < vec.size(); i++)
				searches[i] = vec.elementAt(i).toString().trim().toLowerCase();
			return searches;
		} else
			return null;
	}

	// interface

	/** Returns the type (FLOAT, INTEGER, ...) of this TextField. */
	public int getType() {
		return fieldType;
	}

	/**
	 * Sets the type of this TextField.
	 * @param type FLOAT - only float values were accepted,
	 *             INTEGER - only integer values were accepted,
	 *             TEXT - all values were accepted.
	 *             ... see top.
	 */
	public void setType(int type) {
		fieldType = type;
	}

	/**
	 * Sets the maximal possible value of this TextField.
	 * @param max     the highest acceptable value.
	 */
	public void setMaximum(Float max) {
		maxValue = max;
	}

	/**
	 * Returns the maximal possible value of this TextField.
	 * @return the maximum value.
	 */
	public Float getMaximum() {
		return maxValue;
	}

	/**
	 * Sets the minimal possible value of this TextField.
	 * @param min     the lowest acceptable value.
	 */
	public void setMinimum(Float min) {
		minValue = min;
	}

	/**
	 * Sets the minimal possible value of this TextField.
	 * @param cal     the lowest acceptable value in type DATE.
	 */
	public void setMinimum(Calendar cal) {
		minDate = cal;
	}

	/**
	 * Returns the minimal possible value of this TextField.
	 * @return the minimum value.
	 */
	public Float getMinimum() {
		return minValue;
	}

	/**
	 * Returns the actually value of this TextField as a Float-Object. */
	public Float getValue() {
		if (!getText().equals(""))
			return Float.valueOf(getText());
		else
			return new Float(0.0);
	}

	/** sets a new Value to this TextField depending on the FieldTyp (CURRENCY, FLOAT,...). */
	public void setValue(Object value) {
		checkFormat(value);
		//    if ( getType() == CURRENCY ) setText(buildCurrency(value));
	}

	/** Sets a new Date value, if the FieldType is DATE.
	 */
	public void setDate(Calendar date) {
		if (getType() == DATE && date != null) {
			int day = date.get(Calendar.DATE);
			int month = date.get(Calendar.MONTH) + 1;
			int year = date.get(Calendar.YEAR);
			String text =
				(day < 9 ? "0" : "")
					+ day
					+ "."
					+ (month < 9 ? "0" : "")
					+ month
					+ "."
					+ year;
			setText(text);
		}
	}

	/** return the actually value of this TextField as a Double-Object. */
	public Double getDoubleValue() {
		try {
			if (getText() != "")
				return Double.valueOf(getText());
			else
				return null;
		} catch (NumberFormatException nfe) {
			return null;
		}
	}

	/**
	 * Returns the actually value of this TextField as a currency-String. */
	public String getCurrencyValue() {
		Double d = getDoubleValue();
		if (getType() == CURRENCY && d != null) {
			return buildCurrency("" + d.doubleValue(), 2);
		}
		return null;
	}

	/** Returns the actually value of this TextField in a percent value as Double.
	 *  i.e.: 16% --> 0.16
	 */
	public Double getPercentValue() {
		if (getType() != PERCENT)
			return null;
		try {
			if (getText() != "") {
				double d = Double.valueOf(getText()).doubleValue();
				return new Double(d / 100d);
			} else
				return null;
		} catch (NumberFormatException nfe) {
			return null;
		}
	}

	/** return the actually value of this TextField as a Integer-Object. */
	public Integer getIntegerValue() {
		if (getType() != INTEGER)
			return null;
		try {
			if (getText() != "") {
				Integer i = Integer.valueOf(getText());
				return i;
			} else
				return null;
		} catch (NumberFormatException nfe) {
			return null;
		}
             }


	/**
	 * Returns the actually value of this TextField as a Calendar-Object. */
	public Calendar getDate() {
		if (getType() != DATE)
			return null;
		if (!getText().equals("")) {
			String[] split = split(getText(), ".");
			int date = Integer.parseInt(split[0]);
			int month = Integer.parseInt(split[1]) - 1;
			int year = Integer.parseInt(split[2]);
			GregorianCalendar cal =
				new GregorianCalendar(year, month, date, 0, 0, 0);
			return cal;
		} else
			return null;
	}

	/** sets the number of decimals after the comma,
	 * (if FieldType == CURRENCY --> decimalCount := 2,
	 *  so the value will be round to 2 decimals after comma.
	 *  i.e. the value is 157.999 the rounded value is: 158.<b>00</b>).
	 * @param dec   number of decimals
	 */
	public void setDecimalCount(int dec) {
		decimal = dec;
	}

	/** Returns the number of decimals after the comma,
	 *  (i.e. if fieldType == CURRENCY --> getDecimalCount = 2,
	 *   158.<b>00</b>).
	 * @return The number of decimals, -1 if it wasn't set
	 */
	public int getDecimalCount() {
		return decimal;
	}

	/** set to true, when its nevessary to put in a value in this TextField. */
	public void valueIsNecessary(boolean isNecessary) {
		valueNecessary = isNecessary;
	}

	/** return true, if its necessary to put in a value in this textfield. */
	public boolean isValueNecessary() {
		return valueNecessary;
	}

	/**
	 * Connect a <code>JTextField</code> with another <code>JTextField</code>.
	 * So all connected <code>JTextField</code>s where changed, when the value of
	 * one changed.
	 * @param field The <code>JTextField</code>, which were add to the Connection.
	 */
	public void addConnection(BIGTextField field) {
		connectionList.add(field);
	}

	/**
	 * Removes a connected <code>JTextField</code>.
	 * @param field The <code>JTextField</code>, which were removed from the Connection.
	 */
	public void removeConnection(BIGTextField field) {
		connectionList.remove(field);
	}

	/**
	 * Removes all connected <code>JTextField</code>s.
	 */
	public void removeAllConections() {
		connectionList.clear();
	}

	public String toString() {
		return getText();
	}

	// variables
	private static final int MINIMUM_ERROR = 1,
		MAXIMUM_ERROR = 2,
		NUMBER_FORMAT_ERROR = 3,
		VALUE_IS_NESSESSARY_ERROR = 4,
		DATE_ERROR = 5;
	private boolean errorMessage = false, valueNecessary = false;
	private JDialog messageDialog;
	private String text;
	private int fieldType, decimal = -1;
	private Float maxValue, minValue;
	private Calendar minDate;
	private String oldValue;
	private ArrayList connectionList = new ArrayList();
}
/*****************************************************************************
Log-History

*****************************************************************************/
