/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  java pcl kernel skeleton
*  this file: the interface between WORK and JBI using the PCL
*
*  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*  Last change by: $Author: molka $
*  $Revision: 1.7 $
*  $Date: 2007/02/01 15:38:01 $
*
******************************************************************************/
public class SkeletonPCLKernelMain implements BIJavaKernel {
   /* if it is static it has to be calculated just one time */
   private static double timeroverhead = 0.0;
   /**
    * Here we define a minimum time that our kernel needs
    * we do this to avoid a divide by zero.
    **/
   private static double MINTIME = 5.0e-6;
   /**
    * These variables will help us to keep the overview over the arrays
    * we access for our functions/data.
    **/
   /**
    * Number of different ways an algorithm will be measured.
    * Example: loop orders: ijk, ikj, jki, jik, kij, kji -> n_of_works=6 with
    * each different loop order in an own function.
    **/
   private int n_of_works;
   /**
    * Number of fixed functions we have per measurement.
    * Example: execution time and MFLOPS are measured for each loop order
    * -> n_of_sure_funcs_per_work=2
    **/
   private int n_of_sure_funcs_per_work;
   private int num_funcs = 0;
   private int MAXIMUM = 500;
   private int STEPSIZE = 1;
   private BIEnvHash rwe = null;

   private long descr = 0;             // descriptor
   private PCL pcl;                    // reference to PCL
   private int mode;                   // count mode
   private int wish_nevents;           // # of events in wish list
   private int[] wish_list;            // wish lsit of events; array required
   private int doable_nevents;         // # of events in wish list
   private int[] doable_list;          // doable list of events; array required
   private long[] i_result;            // int result for each event
   private double[] fp_result;         // fp result for each event
   /**
    * The constructor.
    **/
   public SkeletonPCLKernelMain() {
      pcl = new PCL();
      mode = pcl.PCL_MODE_USER_SYSTEM;
      wish_nevents = 3;
      wish_list = new int[wish_nevents];
      wish_list[0] = pcl.PCL_FP_INSTR;
      wish_list[1] = pcl.PCL_INSTR;
      wish_list[2] = pcl.PCL_L1DCACHE_MISSRATE;
      doable_nevents = 0;
      doable_list = null;
      i_result = null;
      fp_result = null;

      rwe = BIEnvHash.getInstance();
   }
   /**
    * The implementation of the bi_getinfo from the BenchIT interface.
    * Here the info is filled with informations about the kernel.
    * @param info An Object to hold the info about the kernel.
    **/
   public int bi_getinfo( InfoObject info ) {
      evaluate_environment();
// PCL code ######################################
      if ( pcl.PCLinit( descr ) != pcl.PCL_SUCCESS ) {
         System.err.println( "unable to initialize PCL" );
         return 1;
      }
      doable_list = pcl.PCLcheck( descr, wish_list, wish_nevents, mode );
      if ( doable_list == null ) {
         System.err.println( "None of your PCL Events can be counted on this machine!" );
         return 1;
      }
      if ( doable_list != null ) {
         if ( ( doable_nevents == 0 ) && ( doable_list.length > 0 ) )
            doable_nevents = doable_list.length;
         if ( doable_nevents < 1 ) {
            System.err.println( "PCL returned an empty doable_list! " + doable_list
               + " #" + doable_nevents );
            return 1;
         }
      }
      i_result = new long[doable_nevents];
      fp_result = new double[doable_nevents];
      if ( ( i_result == null ) || ( fp_result == null ) ) {
         System.err.println( "No more memory for result arrays" );
         return 1;
      }
      if ( pcl.PCLquery( descr, doable_list, doable_nevents, mode ) != pcl.PCL_SUCCESS ) {
         System.err.println( "PCL event(s) not measurable" );
         return 1;
      }
// ende PCL code ####################################
      info.codesequence = "work_[1|2]()";
      info.xaxistext = "Problem Size";
      info.log_xaxis = 0;
      info.base_xaxis = 0.0;
      info.maxproblemsize = MAXIMUM;
      info.kernel_execs_mpi1 = 0;
      info.kernel_execs_mpi2 = 0;
      info.kernel_execs_pvm = 0;
      info.kernel_execs_omp = 0;
      info.kernel_execs_pthreads = 0;
      info.kernel_execs_javathreads = 0;
      /* B ########################################################*/
      n_of_works = 2; /* number versions of this algorithm (ijk, ikj, kij, ... = 6 */
      n_of_sure_funcs_per_work = 2; /* time measurement and FLOPS (calculated) */
      /*########################################################*/
      num_funcs = n_of_works * ( n_of_sure_funcs_per_work + doable_nevents );
      info.numfunctions = num_funcs;

      /* allocating memory for y axis texts and properties */
      info.yaxistexts = new String[info.numfunctions];
      info.outlier_direction_upwards = new int[info.numfunctions];
      info.legendtexts = new String[info.numfunctions];
      info.log_yaxis = new int[info.numfunctions];
      info.base_yaxis = new double[info.numfunctions];
      /* setting up y axis texts and properties */
      for ( int j = 0; j < n_of_works; j++ ) {
         /* B ########################################################*/
         int index1 = 0 * n_of_works + j;
         int index2 = 1 * n_of_works + j;
         //int index3 = 2 * n_of_works + j;
         // 1st function
         info.yaxistexts[index1] = "Calculation Time in s";
         info.outlier_direction_upwards[index1] = 1;
         info.log_yaxis[index1] = 0;
         info.base_yaxis[index1] = 0.0;
         // 2nd function
         info.yaxistexts[index2] = "FLOPS";
         info.outlier_direction_upwards[index2] = 0;
         info.log_yaxis[index2] = 0;
         info.base_yaxis[index2] = 0.0;
         /*########################################################*/
         // 3rd function
         //info.yaxistexts[index3] = "";
         //info.outlier_direction_upwards[index3] = 0;
         //info.log_yaxis[index3] = 0;
         //info.base_yaxis[index3] = 0.0;
         for ( int i = 0; i < doable_nevents; i++ ) {
            String str;
            int index = n_of_works * ( n_of_sure_funcs_per_work + i ) + j;
            if ( doable_list[i] < pcl.PCL_MFLOPS )
               str = pcl.PCLeventname( doable_list[i] ) + " per second";
            else
               str = pcl.PCLeventname( doable_list[i] );
            info.yaxistexts[index] = str;
            info.outlier_direction_upwards[index] = 1;
            info.log_yaxis[index] = 0;
            info.base_yaxis[index] = 0;
         }
         switch ( j ) {
            /* B ########################################################*/
            case 1: // 2nd version legend text; maybe (ikj)
               info.legendtexts[index1] = "Calculation Time in s (2)";
               info.legendtexts[index2] = "FLOPS (2)";
               for ( int i = 0; i < doable_nevents; i++ )
               {
                  String str;
                  int index = n_of_works * ( n_of_sure_funcs_per_work + i ) + j;
                  if ( doable_list[i] < pcl.PCL_MFLOPS )
                     str = pcl.PCLeventname( doable_list[i] ) + " per second (2)";
                  else
                     str = pcl.PCLeventname( doable_list[i] ) + " (2)";
                  info.legendtexts[index] = str;
               }
               break;
            case 0: // 1st version legend text; maybe (ijk)
            default:
               info.legendtexts[index1] = "Calculation Time in s (1)";
               info.legendtexts[index2] = "FLOPS (1)";
               for ( int i = 0; i < doable_nevents; i++ )
               {
                  String str;
                  int index = n_of_works * ( n_of_sure_funcs_per_work + i ) + j;
                  if ( doable_list[i] < pcl.PCL_MFLOPS )
                     str = pcl.PCLeventname( doable_list[i] ) + " per second (1)";
                  else
                     str = pcl.PCLeventname( doable_list[i] ) + " (1)";
                  info.legendtexts[index] = str;
               }
            /*########################################################*/
         }
      }
      return 0;
   }
   /**
    * Implementation of the bi_init of the BenchIT interface.
    * Making usage always of the same memory is faster.
    * HAVE A LOOK INTO THE HOWTO !
    **/
   public Object bi_init( int problemsizemax ) {
      SkeletonPCLDataObject dataObject = new SkeletonPCLDataObject();
      /* initialize your own arrays in here */
/*
      dataObject.maxn = problemsizemax;
      dataObject.a = new double[problemsizemax * problemsizemax];
*/
      return (Object)dataObject;
   }
   /**
    * The central function within each kernel. This function
    * is called for each measurment step seperately.
    * @param  dObject      an Object to the attributes initialized in bi_init,
    *                      it is the Object bi_init returns
    * @param  problemsize  the actual problemsize
    * @param  results      an array of doubles, the
    *                      size of the array depends on the number
    *                      of functions, there are #functions+1
    *                      doubles
    * @return 0 if the measurment was sucessfull, something
    *         else in the case of an error
    **/
   public int bi_entry( Object dObject, int problemsize, double[] results ) {
      /**
       * In java results is the total result array, not just a subarray
       * as in c. The offset points to the start part of the current entry.
       **/
      //int offset = ( num_funcs + 1 ) * ( problemsize - 1 );
      /* if it is static it has to be calculated just one time */
      double timeroverhead = 0.0;
      /* ts, te: the start and end time of the measurement */
      /* timeinsecs: the time for a single measurement in seconds */
      double ts = 0, te = 0;
      double timeinsecs = 0.0;
      /* flops stores the calculated FLOPS */
      double flops = 0.0;
      /* Cast Object reference */
      SkeletonPCLDataObject dataObject = (SkeletonPCLDataObject)dObject;

      /* calculate overhead if it is not done yet */
      if ( timeroverhead == 0.0 ) {
         timeroverhead = gettimeroverhead();
         /* maybe we have a VERY fast timer ;-)
          * the /10000 is because we measure the timeroverhead with 10000 calls 
          * to the timer. if this can't be measured with the timers resolution
          * 1 call has to be 10000 times faster than the resolution of the timer
          */
         if ( timeroverhead == 0.0 ) timeroverhead = MINTIME/10000;
      }
      /* check wether the pointer to store the results in is valid or not */
      if ( results == null ) return 1;
      /* B ########################################################*/
      /* maybe some more init stuff in here */
      SkeletonPCLWork work = new SkeletonPCLWork( dataObject );
      /*########################################################*/

      for ( int j = 0; j < n_of_works; j++ )
      {
         /* B ########################################################*/
         int index1 = 0 * n_of_works + j;
         int index2 = 1 * n_of_works + j;
         /* reset of reused values */
         ts = 0;
         te = 0;
         timeinsecs = 0.0;
         for ( int i = 0; i < doable_nevents; i++ ) {
            i_result[i] = 0;
            fp_result[i] = 0.0;
         }
         /* choose version of algorithm */
         switch ( j ) {
            case 1: // 2nd version legend text; maybe (ikj)
               if ( pcl.PCLstart( descr, doable_list, doable_nevents, mode )
                  != pcl.PCL_SUCCESS ) {
                  System.err.println( "Problem with starting PCL event(s)" );
                  return 1;
               }
               /* take start time, do measurment, and take end time */
               ts = JBI.bi_gettime();
               work.work_2();
               te = JBI.bi_gettime();
               if ( pcl.PCLstop( descr, i_result, fp_result, doable_nevents )
                  != pcl.PCL_SUCCESS ) {
                  System.err.println( "Problem with reading PCL event(s)" );
                  return 1;
               }
               break;
            case 0: // 1st version legend text; maybe (ijk)
            default:
               if ( pcl.PCLstart( descr, doable_list, doable_nevents, mode )
                  != pcl.PCL_SUCCESS ) {
                  System.err.println( "Problem with starting PCL event(s)" );
                  return 1;
               }
               /* take start time, do measurment, and take end time */
               ts = JBI.bi_gettime();
               work.work_1();
               te = JBI.bi_gettime();
               if ( pcl.PCLstop( descr, i_result, fp_result, doable_nevents )
                  != pcl.PCL_SUCCESS ) {
                  System.err.println( "Problem with reading PCL event(s)" );
                  return 1;
               }
         }
         /* calculate the used time and FLOPS */
         timeinsecs = te - ts;
         timeinsecs -= timeroverhead;
         // this flops value is a made up! this calulations should be replaced
         // by something right for the choosen algorithm
         flops = (double)problemsize;
         /* check for divide by zero, if timeinsecs is zero
         * timeinsecs=0 means that our operation does not need any time! */
         if ( timeinsecs < MINTIME ) timeinsecs = MINTIME;
         /* store the results in results[1], results[2], ...
         * [1] for the first function, [2] for the second function
         * and so on ...
         * the index 0 always keeps the value for the x axis
         */
         /* B ########################################################*/
         // the xaxis value needs to be stored only once!
         if ( j == 0 ) results[0] = (double)problemsize;
         results[index1 + 1] = timeinsecs;
         results[index2 + 1] = flops;
         for ( int i = 0; i < doable_nevents; i++ )
         {
            int index = n_of_works * ( n_of_sure_funcs_per_work + i ) + j;
            if ( doable_list[i] < pcl.PCL_MFLOPS )
               results[index + 1] = (double)i_result[i] / ( 1.0 * timeinsecs );
            else
               results[index + 1] = fp_result[i];
            if ( doable_list[i] == pcl.PCL_INSTR ) {
               results[index + 1] /= ( 1.0 * timeinsecs );
            }
         }
         /*########################################################*/
      }
      return 0;
   }
   /**
    * Tries to measure the timer overhead for a single call to bi_gettime().
    * @return the calculated overhead in seconds
    */
   private double gettimeroverhead() {
      int s;
      double ts, te, t;
      ts = JBI.bi_gettime();
      for ( s = 0; s < 10000; s++ ) {
         t = JBI.bi_gettime();
      }
      te = JBI.bi_gettime();
      return ( te - ts ) / 10000.0;
   }
   /**
    * Reads the environment variables used by this kernel.
    **/
   private void evaluate_environment() {
      String smax = null, sstep = null;
      int max = -1, maxDef = 500, step = -1, stepDef = 1;
      smax = rwe.bi_getEnv( "BENCHIT_SKELETONS_SKELETON_JAVA_0_PCL_0_INCREMENT" );
      sstep = rwe.bi_getEnv( "BENCHIT_SKELETONS_SKELETON_JAVA_0_PCL_0_STEPS" );
      if ( ( smax == null ) || ( sstep == null ) ) {
         max = maxDef;
         step = stepDef;
      }
      try {
         max = (new Integer( smax )).intValue();
         step = (new Integer( sstep )).intValue();
      }
      catch ( NumberFormatException nfe ) {
         max = maxDef;
         step = stepDef;
      }
      MAXIMUM = max;
      STEPSIZE = step;
   }
}
/******************************************************************************
*  Log-History
*
*  $Log: SkeletonPCLKernelMain.java,v $
*  Revision 1.7  2007/02/01 15:38:01  molka
*  changed Java-Kernels to use C-Timer (JBI.bi_gettime()) instead of System.currentTimeMillis()
*
*  Revision 1.6  2005/11/12 10:05:36  mickler
*  - Fixed wrong subtraction (ts - te) in gettimeroverhead()
*
*  Revision 1.5  2005/11/04 16:24:22  mickler
*  # Changed kernels to set base_[xy]axis=0 if not using logarithmic scale
*  - Fixed some kernels with log_[xy]axis=0 and base_[xy]axis=10 to base=0
*
*  Revision 1.4  2005/09/11 23:03:38  mickler
*  - Fixed: Changed MINTIME to timer granularity
*  - Fixed: Corrected names of env variables
*
*  Revision 1.3  2005/07/29 11:53:52  wloch
*  removed setting of kernelname in bi getinfo and equalized parameter variables
*
*  Revision 1.2  2005/07/20 12:42:56  wloch
*  kernellanguage and libraries are now taken from kernel name
*
*  Revision 1.1.1.1  2005/07/18 13:03:21  wloch
*  the final restructured benchit source tree
*
*  Revision 1.3  2005/06/21 12:27:25  wloch
*  adapted to new interface in JBI and benchit c
*
*  Revision 1.2  2005/04/19 15:57:39  wloch
*  ans neue interface angepasst
*
*  Revision 1.1  2005/02/14 17:31:34  wloch
*  skeleton for java kernel using the PCL
*
*
*
******************************************************************************/
