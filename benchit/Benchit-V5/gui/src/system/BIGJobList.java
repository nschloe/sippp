package system;

import java.util.ArrayList;
import java.util.Collections;
import java.util.StringTokenizer;

/**
 * A textfile update can consists of different tasks. This class
 * implements a data structure to hold all tasks of an update.
 * 
 * @author Ronny Tschueter
 *
 */
public class BIGJobList extends ArrayList {
	public BIGJobList() {
		super();
	}
	
	/**
	 * This method gets a tokenizer that contains the elemantary parts (affectedFile,
	 * affectedString, replaceWith) of the jobs.txt-file on the update server. It creates
	 * a BIGJobListElement for each task and set appropriate values. 
	 * 
	 * @param tokenizer - contains elementary parts of jobs.txt-file 
	 * @return boolean - true if list is filled successfully, otherwise false 
	 */
	public boolean fillList(StringTokenizer tokenizer) {
		double version;
		BIGJobListElement element = null;
		
		// printStringTokenizer( tokenizer );
		
        if ( tokenizer.hasMoreTokens() ) {
        	String str = tokenizer.nextToken();
        	if (debug) System.out.println( "First if statement: " + str );
            try
            {
            	// if str represents a version-id
            	// if not an exception will be thrown
            	version = Double.parseDouble( str ) ;
            	element = new BIGJobListElement( version );
            }
            catch ( NumberFormatException nfe )
            {
            	System.err.println("The version.txt file has an invalid format.");
            	return false;
            }
        }
        
        while ( tokenizer.hasMoreTokens() ) {
        	String str = tokenizer.nextToken();
        	if (debug) System.out.println( "while statement: " + str );
        	
        	if (str.startsWith( BIGTextfileUpdate.AFFECTEDFILE ) ) {
        		if (debug) System.out.println("AFFECTEDFILE");
        		str = str.substring( str.indexOf("=") + 1, str.length() ).trim();
        		if (debug) System.out.println("trimmed string: " + str);
        		element.setAffectedFile( str );
        		if (debug) System.out.println("AFFECTED FILE IS SETTED");
        	}
        	else if (str.startsWith( BIGTextfileUpdate.AFFECTEDSTRING ) ) {
        		if (debug) System.out.println("AFFECTEDSTRING");
        		str = str.substring( str.indexOf("=") + 1, str.length() ).trim();
        		element.setAffectedString( str );
        	}
        	else if (str.startsWith( BIGTextfileUpdate.REPLACEWITH ) ) {
        		if (debug) System.out.println("REPLACEWITH");
        		str = str.substring( str.indexOf("=") + 1, str.length() ).trim();
        		element.setReplaceWith( str );
        		this.add( element );
        	}
        	else {
        		// it must be a version entry
        		try
                {
                	// if str represents a version-id
                	// if not an exception will be thrown
                	// this.add( element );
                	version = Double.parseDouble( str ) ;
                	if (debug) System.out.println("VERSION");
                	element = new BIGJobListElement( version );
                }
        		catch ( NumberFormatException nfe) {
        			if (debug) System.out.println("The version.txt file has an invalid format.");
        			return false;
        		}
        	}
        }
        
        if (debug) System.out.println("END while statement");
        
        // this.add( element );
        
        // now the array list is filled, but in the wrong order
        // actual order:
        // element0		element1	element2	...
        // id = 1.2		id = 1.1	id = 1.0
        // at first the latest element (with the highest id) will be executed
        // but that means it is possible that older changes overwrite newer changes
        // that's why we turn the order
        Collections.reverse( this );
        
        /*System.err.println("reverse");
        Iterator iter = this.iterator();
        while(iter.hasNext()) {
        	System.err.println( ( (BIGJobListElement) iter.next() ).versionID );
        }*/
        
        
		return true;
	}
	
	/*private void printStringTokenizer(StringTokenizer st) {
		if (debug) System.out.println("BEGIN StringTokenizer");
		while (st.hasMoreTokens()) {
			if (debug) System.out.println( st.nextToken() );
		}
		if (debug) System.out.println("END StringTokenizer");
	}*/
	
	boolean debug = false;
}
