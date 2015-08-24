package system;

/**
 * This class represents an elemntary taskof the textfile update.
 * The update works like following: <br>
 * The update process handles text files that are constructed like
 * 	<location_identifier> = <correspondig_value>
 * The process parses the specified text file ( = affectedFile)
 * and looks for the location_identifier ( = affectedString) where
 * changes should be introduced. If the location_identifier was found
 * then the actual corresponding_value will be replaced by a
 * new value ( = replaceWith).
 * 
 * @author Ronny Tschueter
 *
 */
public class BIGJobListElement {
	/**
	 * The constructor for BIGJobListElement.
	 * 
	 * @param versionID - 	the version id to which this
	 * 						job element is related 
	 */
	public BIGJobListElement(double versionID) {
		super();
		this.versionID = versionID;
	}
	
	/**
	 * Returns the name of the file that this job will edit.
	 * 
	 * @return String - name of affected file
	 */
	public String getAffectedFile() {
		return affectedFile;
	}
	
	/**
	 * Sets the file that should be edited by this task
	 * 
	 * @param affectedFile - affected file
	 */
	public void setAffectedFile(String affectedFile) {
		this.affectedFile = affectedFile;
	}
	
	/**
	 * Returns the string sequence this task is looking for.
	 * The string sequence identifies the correct location for
	 * changing the coresponding value. 
	 * 
	 * @return String - affected string
	 */
	public String getAffectedString() {
		return affectedString;
	}
	
	/**
	 * Sets the string sequence this task is looking for.
	 * The string sequence identifies the correct location for
	 * changing the coresponding value.
	 * 
	 * @param affectedString - location for changes 
	 */
	public void setAffectedString(String affectedString) {
		this.affectedString = affectedString;
	}
	
	/**
	 * Returns the string that replace the value of the location
	 * indentified by affectedString. So the string represents the
	 * new value written by the update process.
	 * 
	 * @return String - the new value
	 */
	public String getReplaceWith() {
		return replaceWith;
	}
	
	/**
	 * Sets the string that replace the value of the location
	 * indentified by affectedString.
	 * 
	 * @param replaceWith - the new value
	 */
	public void setReplaceWith(String replaceWith) {
		this.replaceWith = replaceWith;
	}
	
	/** the file that is affected by the update */
	private String affectedFile;
	/** names the location, the corresponding value will be edited */
	private String affectedString;
	/** holds the new value that the update should write */
	private String replaceWith;
	/** version number */
	double versionID;
}
