/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGDoubleIdentifierException.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.1 $
*  $Date: 2005/10/20 13:13:58 $
*
******************************************************************************/
package system;

/**
 * Thrown when an identifer like a filename or a parametername is tried
 * to add twicely.
 * For Example on <code>BIGInterface.AddFile()</code> or <code>BIGInterface.AddEntry()</code>.
 * 
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @see BIGInterface
 **/
public class BIGDoubleIdentifierException extends RuntimeException {

	/**
	 * Constructs an <code>BIGDoubleIdentifierException</code> with no detail message.
	 **/
	public BIGDoubleIdentifierException() {
		super();
	}

	/**
	 * Constructs an <code>BIGDoubleIdentifierException</code> with the specified detail message.
	 * 
	 * @param   message     the detail message
	 **/
	public BIGDoubleIdentifierException(String message) {
		super(message);
	}

}
/*****************************************************************************
Log-History

*****************************************************************************/
