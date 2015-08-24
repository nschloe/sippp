/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, Java
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.4 $
 * $Date: 2006/05/09 10:26:53 $
 *******************************************************************/

/** This class definies the measured algorithm.
  * @author Robert Wloch, wloch@zhr.tu-dresden.de
  */
public class MatMul {
   private DataObject data;
   public MatMul( DataObject data ) {
      this.data = data;
   }
   public void testCall() {
      int i, j, k;
      int s = data.maxn;
   }
   void init() {
      int i, j;
      int s = data.maxn;
      for( i = 0; i < s; i++) {
         for( j = 0; j < s; j++) {
            int index = i * s + j;
            data.c[index] = 0.0;
            data.a[index] = (double)index;
            data.b[index] = (double)j;
         }
      }
   }
   void matmul_ijk() {
      int i, j, k;
      int s = data.maxn;
      for( i = 0; i < s; i++) {
         for( j = 0; j < s; j++) {
            for( k = 0; k < s; k++) {
               int index = j * s;
               data.c[index + i] += data.a[k * s + i] * data.b[index + k];
            }
         }
      }
   }
   void matmul_ikj() {
      int i, j, k;
      int s = data.maxn;
      for( i = 0; i < s; i++) {
         for( k = 0; k < s; k++) {
            for( j = 0; j < s; j++) {
               int index = j * s;
               data.c[index + i] += data.a[k * s + i] * data.b[index + k];
            }
         }
      }
   }
   void matmul_kij() {
      int i, j, k;
      int s = data.maxn;
      for( k = 0; k < s; k++) {
         for( i = 0; i < s; i++) {
            for( j = 0; j < s; j++) {
               int index = j * s;
               data.c[index + i] += data.a[k * s + i] * data.b[index + k];
            }
         }
      }
   }
   void matmul_kji() {
      int i, j, k;
      int s = data.maxn;
      for( k = 0; k < s; k++) {
         for( j = 0; j < s; j++) {
            for( i = 0; i < s; i++) {
               int index = j * s;
               data.c[index + i] += data.a[k * s + i] * data.b[index + k];
            }
         }
      }
   }
   void matmul_jik() {
      int i, j, k;
      int s = data.maxn;
      for( j = 0; j < s; j++) {
         for( i = 0; i < s; i++) {
            for( k = 0; k < s; k++) {
               int index = j * s;
               data.c[index + i] += data.a[k * s + i] * data.b[index + k];
            }
         }
      }
   }
   void matmul_jki() {
      int i, j, k;
      int s = data.maxn;
      for( j = 0; j < s; j++) {
         for( k = 0; k < s; k++) {
            for( i = 0; i < s; i++) {
               int index = j * s;
               data.c[index + i] += data.a[k * s + i] * data.b[index + k];
            }
         }
      }
   }
}

/********************************************************************
 * Log-History
 *
 * $Log: MatMul.java,v $
 * Revision 1.4  2006/05/09 10:26:53  hackenb
 * *** empty log message ***
 *
 * Revision 1.3  2005/09/11 22:55:29  mickler
 * - Fixed: initialize "c"-matrix with 0.0 instead 1.0
 *
 * Revision 1.2  2005/07/19 12:10:36  wloch
 * added cvs footers
 * 
 *******************************************************************/
