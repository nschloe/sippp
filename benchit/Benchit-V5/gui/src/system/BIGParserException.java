/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGParserException.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.1 $
*  $Date: 2005/10/20 13:13:58 $
*
******************************************************************************/
package system;

/**
 * Thrown on an error of one of the two parsers. See message for more details.
 * For Example the <code>BIGDefFileParser</code>.
 * 
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @see BIGDefFileParser
 **/
public class BIGParserException extends Exception {

	/**
	 * Constructs an <code>BIGParserException</code> with no detail message.
	 **/
	public BIGParserException() {
		super();
	}

	/**
	 * Constructs an <code>BIGParserException</code> with the specified detail message.
	 * 
	 * @param   message     the detail message
	 **/
	public BIGParserException(String message) {
		super(message);
	}
}
/*****************************************************************************
Log-History

*****************************************************************************/
