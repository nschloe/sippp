/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGAccessViolationException.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.1 $
*  $Date: 2005/10/20 13:13:58 $
*
******************************************************************************/
package system;

/**
 * Thrown on an unauthorized call of a method.
 * For Example when an <code>BIGEntry.SetType()</code> is called twice.
 * 
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @see BIGEntry
 **/
public class BIGAccessViolationException extends RuntimeException {

	/**
	 * Constructs an <code>BIGAccessViolationException</code> with no detail message.
	 **/
	public BIGAccessViolationException() {
		super();
	}

	/**
	 * Constructs an <code>BIGAccessViolationException</code> with the specified detail message.
	 * 
	 * @param   message     the detail message
	 **/
	public BIGAccessViolationException(String message) {
		super(message);
	}
}
/*****************************************************************************
Log-History

*****************************************************************************/
