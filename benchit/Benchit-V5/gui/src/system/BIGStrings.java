/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  BIGStrings.java
*
*  Author: SWTP Nagel 1
*  Last change by: $Author: rschoene $
*  $Revision: 1.2 $
*  $Date: 2006/05/30 11:46:21 $
*
******************************************************************************/
package system;

import java.util.*;
import java.io.*;

/**
 * Its a List of <code>String</code>. Handled simular to an <code>ArrayList</code>,
 * but it isn't extended. Its a class for its own, build like the Strings class in
 * Delphi.
 * <P>
 * <code>Strings</code> is taken from <code>fXlib</code>, soon available under:<BR>
 * <code><a href="http://www.fx-world.de">http://www.fx-world.de</a></code>
 *
 * @author <a href="mailto:fx@fx-world.de">Pascal Weyprecht</a>
 * @version 1.1
 * @see String
 * @see ArrayList
 **/
public class BIGStrings {

	private ArrayList list;

	/**
	 * Constructs an empty list with an initial capacity of ten.
	 *
	 **/
	public BIGStrings() {
		list = new ArrayList();
	}

	/**
	 * Constructs an empty list with the specified initial capacity.
	 *
	 **/
	public BIGStrings(int initialCapacity) {
		list = new ArrayList(initialCapacity);
	}

	/**
	 * Constructs a list filled by the values from the Array by using the method toString.
	 *
	 **/
	public BIGStrings(Object[] fill) {
		list = new ArrayList(fill.length);
		for (int i = 0; i < fill.length; i++)
			list.add(fill[i].toString());
	}

	/**
	 * Constructs a list filled by the values from the ArrayList by using the method toString.
	 *
	 **/
	public BIGStrings(ArrayList fill) {
		list = new ArrayList(fill.size());
		for (int i = 0; i < fill.size(); i++)
			list.add(fill.get(i).toString());
	}

	/**
	 * Inserts the specified <code>String</code> at the specified position in
	 * this list. Shifts the <code>Strings</code> currently at that position (if
	 * any) and any subsequent elements to the right (adds one to their indices).
	 *
	 * @param   index           index at which the specified <code>String</code>
	 *                          is to be inserted
	 * @param   element         <code>String</code> to be inserted
	 **/
	public void add(int index, String element) {
		while (index > list.size()) {
			list.add("");
		}
		list.add(index, element);
	}

	/**
	 * Appends the specified <code>String</code> to the end of this list.
	 *
	 * @param   element         <code>String</code> to be inserted
	 **/
	public boolean add(String element) {
		return list.add(element);
	}

	/**
	 * Returns the <code>String</code> at the specified position in this list.
	 *
	 * @param   index           index of <code>String</code> to return
	 * @return                  the element at the specified position in this list
	 **/
	public String get(int index) {
		return (String) list.get(index);
	}

	/**
	 * Removes all of the <code>Strings</code> from this list. The list will be
	 * empty after this call returns.
	 *
	 **/
	public void clear() {
		list.clear();
	}

	/**
	 * Compares the specified object with this list for equality. Returns true
	 * if and only if the specified object is also a list, both lists have the
	 * same size, and all corresponding pairs of strings in the two lists are equal.
	 *
	 * @param   o               the object to be compared for equality with this list
	 * @return                  <code>true</code> if the specified object is equal
	 *                          to this list
	 **/
	public boolean equals(Object o) {
		if (list.size() != ((List) o).size()) {
			return false;
		}
		for (int i = 0; i < list.size(); i++) {
			if (((String) list.get(i)).equals(((List) o).get(i)) == false) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Removes the <code>String</code> at the specified position in this list. Shifts
	 * any subsequent strings to the left (subtracts one from their indices).
	 *
	 * @param   index           the index of the <code>String</code> to removed
	 * @return                  the <code>String</code> that was removed from the list.
	 **/
	public String remove(int index) {
		return (String) list.remove(index);
	}

	/**
	 * Removes the first occurrence in this list of the specified <code>String</code>
	 * (optional operation). If this list does not contain the <code>String</code>, it
	 * is unchanged.
	 *
	 * @param   element         <code>String</code> to be removed from this list, if present
	 * @return                  <code>true</code> if this list contained the specified
	 *                          <code>String</code>
	 **/
	public boolean remove(String element) {
		return list.remove(element);
	}

	/**
	 * Replaces the <code>String</code> at the specified position in this list with the
	 * specified <code>String</code>. If the list is to short it is filled with empty lines
	 * (NOT with <code>null</code> elements).
	 *
	 * @param   index           index of <code>String</code> to replace
	 * @param   element         <code>String</code> to be stored at the specified position
	 * @return                  the <code>String</code> previously at the specified position
	 **/
	public String set(int index, String element) {
		while (index >= list.size()) {
			list.add("");
		}
		return (String) list.set(index, element);
	}

	/**
	 * Returns the number of <code>String</code> in this list.
	 *
	 * @return                  the number of <code>Strings</code> in this list
	 **/
	public int size() {
		return list.size();
	}

	/**
	 * Returns a comma text representation of all strings in the <code>Strings</code>.
	 * It's in a form like:
	 * <BR>"hello","this","is a strings","object"
	 *
	 * @return                  the commaText representation of <code>Strings</code>
	 **/
	public String getCommaText() {
		String result = "";
		for (int i = 0; i < list.size(); i++) {
			String tempString = new String((String) list.get(i));
			tempString = tempString.replaceAll("\"", "\"\"");
			result = result + "\"" + tempString + "\"";
			if (i + 1 < list.size()) {
				result = result + ",";
			}
		}
		return result;
	}

	/**
	 * Sets the whole <code>Strings</code> object based upon a commaText.
	 * It has to be in a form like:
	 * <BR>"hello","this","is a strings","object"
	 * <BR>otherwise there will be a out of bounce exception.
	 *
	 * @param   str             the commaText representation of <code>Strings</code>
	 **/
	public void setCommaText(String str) {
		boolean finished = false;
		int posA = -1;
		int posB = -1;
		list.clear();
		if (str == null)
			finished = true;
		while (!finished) {
			posA = str.indexOf("\"", posB + 1);
			//System.out.print(posA+":");
			if (posA >= 0) {
				posB = str.indexOf("\"", posA + 1);
				//System.out.print(posB+"\n");
				while ((posB + 2 <= str.length())
					&& ("\"\"".equals(str.substring(posB, posB + 2)))) {
					//System.out.print("["+str.substring(posB,posB+2)+"]");
					posB = str.indexOf("\"", posB + 2);
					//System.out.print(posA+":"+posB+"\n");
				}
				list.add(str.substring(posA + 1, posB));
			} else {
				finished = true;
			}
		}
	}

	/**
	 * Reads a text out of the file into the list. Before reading the list is cleared.
	 * Every line in the file gets its own <code>String</code> in the list.
	 *
	 * @param   filename        name of the file where to read from
	 **/
	public void readFromFile(String filename)
		throws FileNotFoundException, IOException {
		RandomAccessFile file = new RandomAccessFile(filename, "r");
		String line = file.readLine();

		list.clear();
		while (line != null) {
			list.add(line);
			line = file.readLine();
		}
		file.close();
	}

	/**
	 * Saves the text into the specified file.
	 * Every <code>String</code> in the list gets its own line in the file.
	 *
	 * @param   filename        name of the file where to save to
	 **/
	public void saveToFile(String filename)
        {
           BIGFileHelper.saveToFile(this.toString(),new File(filename));
	}

	/**
	 * Returns true if this list contains the specified <code>String</code>.
	 *
	 * @param   element     element whose presence in this list is to be tested
	 * @return              <code>true</code> if the specified element is present;
	 *                      <code>false</code> otherwise
	 **/
	public boolean contains(String element) {
		return list.contains(element);
	}

	/**
	 * Returns an iterator over the <code>String</code> in this
	 * list in proper sequence.
	 *
	 * @return              an iterator over the <code>String</code>
	 *                      in this list in proper sequence.
	 **/
	public Iterator iterator() {
		return list.iterator();
	}

	/**
	 * Concatenates all strings in the list to one string. After every element
	 * there is a new line added.
	 *
	 * @return                  Concatenation of all <code>String</code> in the list
	 **/
	public String toString() {
		String result = "";
		Iterator it = list.iterator();
		while (it.hasNext()) {
			result = result.concat((String) it.next() + "\n");
		}
/*      for ( int i = 0; i < list.size(); i++ )
      {
         result = result.concat( (String)list.get( i ) + "\n" );
      }*/
		return result;
	}

	/**
	 * Converts the <code>Strings</code> to an <code>ArrayList</code> which
	 * contains the <code>String</code>. It is a totally copy, it has nothing
	 * to do with the intern <code>ArrayList</code>, also the <code>String</code>
	 * are copied.
	 *
	 * @return                  an <code>ArrayList</code> with <code>String</code>
	 **/
	public ArrayList toArrayList() {
		ArrayList result = new ArrayList();
		Iterator it = list.iterator();
		while (it.hasNext()) {
			result.add(new String((String) it.next()));
		}
		return result;
	}

	/**
	 * Converts the <code>Strings</code> to an array which
	 * contains the <code>String</code>s. It is a totally copy, it has nothing
	 * to do with the intern <code>ArrayList</code>, also the <code>String</code>s
	 * are copied.
	 *
	 * @return                  an array with <code>String</code>
	 **/
	public String[] toArray() {
		String[] result = new String[list.size()];
		for (int l = 0; l < list.size(); l++) {
			result[l] = new String((String) list.get(l));
		}
		return result;
	}

	/**
	 * Returns the line number where the <code>String</code> could be found
	 * on the first time. Not the whole <code>String</code> in the line must match
	 * also if the toFind is a part of the line the line number is returned.
	 *
	 * @param   toFind          the <code>String</code> what should be found
	 * @return                  line number where the <code>String</code> was found,
	 *                          or -1 when the <code>String</code> wasn't found
	 **/
	public int find(String toFind) {
		int lineNr = 0;
		while (lineNr < list.size()) {
			if (((String) list.get(lineNr)).indexOf(toFind) >= 0) {
				return lineNr;
			} else {
				lineNr++;
			}
		}
		return -1;
	}

	/**
	 * Returns the line number where the <code>String</code> could be found
	 * on the first time, starting at line <code>fromIndex</code>. Not the
	 * whole <code>String</code> in the line must match also if the toFind
	 * is a part of the line the line number is returned.
	 *
	 * @param   toFind          the <code>String</code> what should be found
	 * @param   fromIndex       the line where to start the search
	 * @return                  line number where the <code>String</code> was found,
	 *                          or -1 when the <code>String</code> wasn't found
	 **/
	public int find(String toFind, int fromIndex) {
		int lineNr = fromIndex;
		while (lineNr < list.size()) {
			if (((String) list.get(lineNr)).indexOf(toFind) >= 0) {
				return lineNr;
			} else {
				lineNr++;
			}
		}
		return -1;
	}

	/**
	 * Sorts the lines alphabetically.
	 *
	 **/
	public void sort() {
		Collections.sort(list);
	}

	/**
	 * concatenate a BIGStrings object to this and return it
	 * @param bs a BIRStrings object to concat
	 *
	 */
	//added by pisi 15.05.2003
	public BIGStrings concat(BIGStrings bs) {
		BIGStrings out = new BIGStrings();
		Iterator it = this.list.iterator();
		while (it.hasNext()) {
			out.add((String) it.next());
		}
		it = bs.iterator();
		while (it.hasNext()) {
			out.add((String) it.next());
		}
		return out;
	}
}
/*****************************************************************************
Log-History

*****************************************************************************/
