/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: pairwise Send/Recv between two MPI-Prozesses>
 *         this file holds all the functions needed by the 
 *         benchit-interface
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.3 $
 * $Date: 2006/01/12 11:44:02 $
 *******************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <mpi.h>
#include "pingpong.h"


/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t * pmydata)
{
   int errors = 0;
   char * p = 0;
   p = bi_getenv( "BENCHIT_KERNEL_REPETITIONS", 0 );
   if ( p == NULL ) errors++;
   else pmydata->repeat = atoi( p );
   /* latency means minimal data */
   pmydata->msgsize = 1;

   MPI_Comm_rank(MPI_COMM_WORLD, &(pmydata->commrank));
   MPI_Comm_size(MPI_COMM_WORLD, &(pmydata->commsize));
 
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      exit( 1 );
   }
   pmydata->pairs = (myinttype) (pmydata->commsize * (pmydata->commsize - 1));
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
   pinfo->codesequence = bi_strdup( "for all sender-receiver-pairs do MPI_send-MPI_recv" );
   pinfo->kerneldescription = bi_strdup( "kernel performs a pingpong with MPI for all possible pairs of MPI processes" );
   pinfo->xaxistext = bi_strdup( "MPI-proccess pair" );
   pinfo->maxproblemsize = penv->pairs;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 1;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;
   pinfo->numfunctions = 1;

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
      pinfo->yaxistexts[0] = bi_strdup( "time in s" );
      pinfo->outlier_direction_upwards[0] = 0;
      pinfo->base_yaxis[0] = 0;
      pinfo->legendtexts[0] = bi_strdup( "time in s" );
 
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
   myinttype ii=0;

   pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( pmydata == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   evaluate_environment(pmydata);

   /* allocate and fill msg-buffer for mpi-communication */
   pmydata->buffer = (myinttype *) malloc( sizeof(myinttype) * pmydata->msgsize );
   if ( pmydata->buffer == 0 )
   {
      fprintf( stderr, "Allocation of structure pmydata->buffer failed\n" ); fflush( stderr );
      exit( 127 );
   }
   for (ii=0; ii < pmydata->msgsize; ii++)
   {
      pmydata->buffer[ii] = (myinttype)random();
   }
   
   IDL(3, printf("\nrank=%d buffercontent:%d\n",pmydata->commrank, pmydata->commsize));
   for (ii=0; ii < pmydata->msgsize; ii++)
   {
   IDL(3, printf("%d ",pmydata->buffer[ii]));
   }
   IDL(3, printf("\n"));
  
   IDL(3, printf("\nrank=%d size=%d\n",pmydata->commrank, pmydata->commsize));
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
  myinttype isender = 0, ireceiver = 0, imyproblemsize = (myinttype) iproblemsize;
  /* cast void* pointer */
  mydata_t * pmydata = (mydata_t *) mdpv;

  IDL(3, printf("\nrank=%d entered bi_entry\n",pmydata->commrank));

  /* calculate real problemsize */
  //fprintf( stderr, "\nrank=%d problemsize=%d\n",pmydata->commrank ,imyproblemsize);fflush( stderr );
  // rank from 0-inf
  // problemsize from 1-inf
  /* example: 
   * 4 mpi-proccesses means rank = 0,1,2,3 size=4 problemsize=4*3=12
   * pair1 to pair12: (sender,receiver) 
   * sender=rank0     (0,1) (0,2) (0,3)     0,0 - prohibited
   * sender=rank1     (1,0) (1,2) (1,3)     1,1 - prohibited 
   * sender=rank2     (2,0) (2,1) (2,3)     2,2 - prohibited 
   * sender=rank3     (3,0) (3,1) (3,2)     3,3 - prohibited
   */
  isender = (myinttype) (imyproblemsize / pmydata->commsize);
  ireceiver = imyproblemsize % pmydata->commsize;
  if (ireceiver == isender) ireceiver++;

  /* check wether the pointer to store the results in is valid or not */
  if ( pmydata->commrank == 0 )
  {
    if ( dresults == NULL )
    {
      fprintf( stderr, "\nrank=%d resultpointer not allocated - panic\n",pmydata->commrank);fflush( stderr );
      return 1;
    }
  }

  /* get the actual time
   * do the measurement / your algorythm
   * get the actual time
   */
  MPI_Barrier( MPI_COMM_WORLD );
  dstart = bi_gettime(); 
  pingpong(&isender, &ireceiver, pmydata);
  MPI_Barrier( MPI_COMM_WORLD );
  dend = bi_gettime();

  IDL(3, printf("rank=%d Problemsize=%d, Value=%f\n",pmydata->commrank, imyproblemsize, dres));

  if ( pmydata->commrank == 0 )
  {
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
    dresults[0] = (double)imyproblemsize;
    dresults[1] = (double) (dtime / 2*pmydata->repeat);
  }

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
 * Revision 1.3  2006/01/12 11:44:02  william
 * changed result in a way that it reflects only one communication step (pingpongtime / 2) and not both
 *
 * Revision 1.2  2006/01/12 11:34:06  william
 * cvs-keyword-problems with binary/ASCII flags
 *
 * Revision 1.1  2006/01/12 11:30:57  william
 * checked in new kernel-tree "communication"
 *
 * Revision 1.7  2006/01/09 16:24:21  william
 * updated the cvs-header
 *
 * Revision 1.6  2006/01/09 15:54:09  william
 * cvs-keyword-problems
 *
 * Revision 1.5  2006/01/09 11:31:00  william
 * filled in infos for the cvs-header and deleted falsly inserted files
 *
 * Revision 1.4  2006/01/08 00:29:16  william
 * forgot to divide the time needed for the ping pong by the number of repetitions
 *
 * Revision 1.3  2006/01/07 23:09:33  william
 * seems that my strings included some prohibited chars??
 *
 * Revision 1.2  2006/01/07 22:56:09  william
 * removed unused variable
 *
 * Revision 1.1  2006/01/07 22:31:17  william
 * pairwise pingpong all-to-all with minimal message-size => latency-benchmark
 *
 *
 *******************************************************************/ 
