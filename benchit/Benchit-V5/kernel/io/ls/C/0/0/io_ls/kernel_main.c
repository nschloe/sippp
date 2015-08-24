/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2006/11/01 20:22:51 $
 *******************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"

#include "io_ls.h"


/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t * pmydata)
{
   int errors = 0;
   char * p = 0;
   char * q = 0;
   
   p = bi_getenv( "BENCHIT_KERNEL_IO_TREE_ROOT", 0 );
   if ( p == NULL ) errors++;
   else 
   {
    q = bi_getenv( "BENCHIT_KERNEL_IO_TREE_DEPTH", 0 );
    if ( q == NULL ) errors++;
    else pmydata->myTree= init_tree_descr (p,atoi( q ));
   } 
   p = bi_getenv( "BENCHIT_KERNEL_IO_TREE_FPD", 0 );
   if ( p == NULL ) errors++;
   else pmydata->filesPerDir = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_IO_DIRS_TO_READ", 0 );
   if ( p == NULL ) errors++;
   else pmydata->dirsToRead = atoi( p );
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      exit( 1 );
   }
   pmydata->steps = atoi( q );
/*   if (( pmydata->max - pmydata->min + 1 ) % pmydata->increment != 0) pmydata->steps++;
*/
}

/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * pinfo )
{
   mydata_t * penv;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   penv = (mydata_t *) malloc( sizeof( mydata_t ) );

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup( "ls in directory tree of given depth#"
             "E.g. tree with depth 3 in /bt#"
             "ls /bt/d1/d0/d0; ls /bt/d1/d0/d1; ls /bt/d1/d1/d0 ...#" );
   pinfo->kerneldescription = bi_strdup( "Reading directory entries in binary tree of directories" );
   pinfo->xaxistext = bi_strdup( "Depth of tree" );
   pinfo->maxproblemsize = penv->steps;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 0;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;
   pinfo->numfunctions = 2;

   /* allocating memory for y axis texts and properties */
   pinfo->yaxistexts = malloc( pinfo->numfunctions * sizeof( char* ) );
   if ( pinfo->yaxistexts == NULL )
   {
     fprintf( stderr, "Allocation of yaxistexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   pinfo->outlier_direction_upwards = malloc( pinfo->numfunctions * sizeof( int ) );
   if ( pinfo->outlier_direction_upwards == NULL )
   {
     fprintf( stderr, "Allocation of outlier direction failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   pinfo->legendtexts = malloc( pinfo->numfunctions * sizeof( char* ) );
   if ( pinfo->legendtexts == NULL )
   {
     fprintf( stderr, "Allocation of legendtexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   pinfo->base_yaxis = malloc( pinfo->numfunctions * sizeof( double ) );
   if ( pinfo->base_yaxis == NULL )
   {
     fprintf( stderr, "Allocation of base yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }

   /* setting up y axis texts and properties */
      pinfo->yaxistexts[0] = bi_strdup( "Dirs / s" );
      pinfo->outlier_direction_upwards[0] = 0;
      pinfo->base_yaxis[0] = 10; //logarythmic axis 10^x
      pinfo->legendtexts[0] = bi_strdup( "Directories read in order" );
 
      pinfo->yaxistexts[1] = bi_strdup( "Dirs / s" );
      pinfo->outlier_direction_upwards[1] = 0;
      pinfo->base_yaxis[1] = 10; //logarythmic axis 10^x
      pinfo->legendtexts[1] = bi_strdup( "Directories read randomly" );

   /* free all used space */
   if (penv) free( penv );
}



/** Implementation of the bi_init of the BenchIT interface.
 *  Here you have the chance to allocate the memory you need.
 *  It is also possible to allocate the memory at the beginning
 *  of every single measurment and to free the memory thereafter.
 *  But making usage always of the same memory is faster.
 *  HAVE A LOOK INTO THE HOWTO !
 */
void* bi_init( int problemsizemax )
{
   mydata_t * pmydata;
   char     cmd[STRING_SIZE];
   int      status;

   pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( pmydata == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   evaluate_environment(pmydata);

   sprintf (cmd, "${BENCHITROOT}/bin/CHECK_DIR_TREE.SH %s %i %i", pmydata->myTree->root, 
       pmydata->myTree->depth, pmydata->filesPerDir);
    status= system (cmd);
    if (status == 127) {
   fprintf(stderr, "\nCannot create tree in directory %s\n", pmydata->myTree->root); exit(127); }
  
   /* if script not found or tree does not exist or has wrong size -> create new one */
   if (status != 0) {
      sprintf (cmd, "rm -rf %s/d[01] %s/*state", pmydata->myTree->root, pmydata->myTree->root);
      system (cmd);
      sprintf (cmd, "mkdir -p %s", pmydata->myTree->root); system (cmd);
      create_dirs (pmydata->myTree);
      create_files_regularly (pmydata->myTree, pmydata->filesPerDir);
   }

   sprintf (cmd, "${BENCHITROOT}/bin/CHECK_OTHER_FILES.SH %s %i %i", pmydata->myTree->root, 
       pmydata->myTree->depth, pmydata->filesPerDir);
   status= system (cmd);

    /* if there are files in other than leaf directories remove them */
   if (status == 0 || status == 3) 
      remove_files_except_leafs (pmydata->myTree);

   return (void *)pmydata;
}



/** The central function within each kernel. This function
 *  is called for each measurment step seperately.
 *  @param  mdpv         a pointer to the structure created in bi_init,
 *                       it is the pointer the bi_init returns
 *  @param  problemsize  the actual problemsize
 *  @param  results      a pointer to a field of doubles, the
 *                       size of the field depends on the number
 *                       of functions, there are #functions+1
 *                       doubles
 *  @return 0 if the measurment was sucessfull, something
 *          else in the case of an error
 */
int bi_entry( void * mdpv, int iproblemsize, double * dresults )
{
  /* dstart, dend: the start and end time of the measurement */
  /* dtime: the time for a single measurement in seconds */
  double dstart = 0.0, dend = 0.0, dtime = 0.0;
  /* flops stores the calculated FLOPS */
  double dres = 0.0;
  /* ii is used for loop iterations */
  myinttype ii = 0;
  /* cast void* pointer */
  mydata_t * pmydata = (mydata_t *) mdpv;


  /* check wether the pointer to store the results in is valid or not */
  if ( dresults == NULL ) return 1;

  /* get the actual time
   * do the measurement / your algorythm
   * get the actual time
   */
  dstart = bi_gettime(); 
  dres = intern_read_dirs (pmydata->myTree, iproblemsize, pmydata->dirsToRead, 0, 0); 
  dend = bi_gettime();

//  fprintf( stderr, "Problemsize=%d, Value=%f\n", imyproblemsize, dres);

  /* calculate the used time and FLOPS */
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
      
  /* If the operation was too fast to be measured by the timer function,
   * mark the result as invalid 
   */
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;

  /* store the results in results[1], results[2], ...
  * [1] for the first function, [2] for the second function
  * and so on ...
  * the index 0 always keeps the value for the x axis
  */
  dresults[0] = (double)iproblemsize;
  dresults[1] = dtime;

  /* now do the random test */
  dstart = bi_gettime(); 
  dres = intern_read_dirs (pmydata->myTree, iproblemsize, pmydata->dirsToRead, 1, 0); 
  dend = bi_gettime();
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;
  dresults[2] = dtime;

  return 0;
}

/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   mydata_t * pmydata = (mydata_t*)mdpv;
   if ( pmydata ) free( pmydata );
   return;
}


/********************************************************************
 * Log-History
 * 
 * $Log: kernel_main.c,v $
 * Revision 1.1  2006/11/01 20:22:51  william
 * first checkin after complete codereview and simple tests
 *
 * Revision 1.1.1.1  2006/04/18 10:03:49  william
 * import version 0.1
 *
 * Revision 1.5  2006/01/09 16:24:20  william
 * updated the cvs-header
 *
 * Revision 1.4  2006/01/09 15:57:01  william
 * cvs-keyword-problems
 *
 * Revision 1.3  2005/12/15 15:44:17  hackenb
 * modified/unified header and footer
 *
 * Revision 1.2  2005/12/14 23:33:32  william
 * changed the algorythm of the skeleton -> fibonacci numbers
 *
 * Revision 1.1  2005/12/14 22:37:12  william
 * A simple Version of the skeleton for easy first time development
 *
 *******************************************************************/ 
