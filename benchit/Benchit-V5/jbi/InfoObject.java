/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  Part of the BenchIT-project (java version)
*
*  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*  Last change by: $Author: rschoene $
*  $Revision: 1.4 $
*  $Date: 2006/05/09 11:16:46 $
*
******************************************************************************/

public class InfoObject{
   public String codesequence;
   public String xaxistext;
   public String[] yaxistexts = null;
   public String[] legendtexts = null;
   public int maxproblemsize;
   public int numfunctions;
   public int numlibraries;
   public int[] outlier_direction_upwards=null;
   public int kernel_execs_mpi1;
   public int kernel_execs_mpi2;
   public int kernel_execs_pvm;
   public int kernel_execs_omp;
   public int kernel_execs_pthreads;
   public int kernel_execs_javathreads;
   public int log_xaxis;
   public int[] log_yaxis=null;
   public int base_xaxis;
   public int[] base_yaxis=null;
   public InfoObject(){
      codesequence = "";
      xaxistext = "";
      maxproblemsize = 0;
      numfunctions = 0;
      kernel_execs_mpi1 = 0;
      kernel_execs_mpi2 = 0;
      kernel_execs_pvm = 0;
      kernel_execs_omp = 0;
      kernel_execs_pthreads = 0;
      kernel_execs_javathreads = 0;
      log_xaxis = 0;
      base_xaxis = 0;
   }
}

/******************************************************************************
 *  Log-History
 *
 *  $Log: InfoObject.java,v $
 *  Revision 1.4  2006/05/09 11:16:46  rschoene
 *  axisbase is now int
 *
 *  Revision 1.3  2005/07/29 11:52:24  wloch
 *  kernel name is no long set in bi getinfo
 *
 *  Revision 1.2  2005/07/20 12:42:21  wloch
 *  kernellanguage and libraries are now taken from kernel name
 *
 *
 */
