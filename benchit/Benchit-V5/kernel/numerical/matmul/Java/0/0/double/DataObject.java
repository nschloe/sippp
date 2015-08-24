/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, Java
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.3 $
 * $Date: 2006/05/09 10:26:53 $
 *******************************************************************/

/** This class handles all the data that will be processed by the
  * measured algorithm.
  * @author Robert Wloch, wloch@zhr.tu-dresden.de
  */
public class DataObject {
	public int maxn = -1;
	// data to be processed
	public double[] a = null;
	public double[] b = null;
	public double[] c = null;
	public DataObject() {
	}
}


/********************************************************************
 * Log-History
 *
 * $Log: DataObject.java,v $
 * Revision 1.3  2006/05/09 10:26:53  hackenb
 * *** empty log message ***
 *
 * 
 *******************************************************************/
