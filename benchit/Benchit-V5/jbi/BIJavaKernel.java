/******************************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  Interface to the BenchIT-project (java version)                            *
 *
 *  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
 *  Last change by: $Author: mickler $
 *  $Revision: 1.3 $
 *  $Date: 2005/11/12 10:06:18 $
 *
 ******************************************************************************/
public interface BIJavaKernel {
    /* provides an empty info-struct. has to be filled by the kernel */
    public int bi_getinfo( InfoObject info );

    /* initializes the kernel with the maximum problem-size. returns a pointer to the
       allocated memory. this pointer is given with each other kernel call */
    public Object bi_init( int problemsizemax );

    /* calls the kernel with one problem-size. Expects the results in the *result. */
    public int bi_entry( Object dObject, int problemsize, double[] results );
}

/******************************************************************************
 *  Log-History
 *
 *  $Log: BIJavaKernel.java,v $
 *  Revision 1.3  2005/11/12 10:06:18  mickler
 *  # Added CVS footer
 *
 *
 */
