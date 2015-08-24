/*******************************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Heat conduction (CG-Solver, vectorized)
 *         Based on the CGV-Exmple in the Parallel Programming Workshop
 *         http://www.hlrs.de/organization/par/par_prog_ws/
 *         Section [42] "Laplace-Example with MPI and PETSc", and
 *         http://www.hlrs.de/organization/par/par_prog_ws/practical/README.html
 *         CG-Solver, vectorized - CGV.tar.gz
 *         by Dr. Rolf Rabenseifner (HLRS)
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.1 $
 * $Date: 2007/01/22 14:54:19 $
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <mpi.h>
#include "interface.h"
#include "cgv.h"


/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t * pmydata)
{
   int errors = 0;
   char * p = 0;

   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX", 0 );
   if ( p == NULL ) errors++;
   else pmydata->max = atoi( p );

   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN", 0 );
   if ( p == NULL ) errors++;
   else pmydata->min = atoi( p );
   
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT", 0 );
   if ( p == NULL ) errors++;
   else pmydata->increment = atoi( p );
 
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      exit( 1 );
   }
   pmydata->steps = (myinttype) ( pmydata->max - pmydata->min + 1 ) / pmydata->increment;
   if (( pmydata->max - pmydata->min + 1 ) % pmydata->increment != 0) pmydata->steps++;

   //printf("\nim evaluate: rank: %d, pmydata->min: %d, pmydata->increment: %d, pmydata->max: %d\n", pmydata->commrank, pmydata->min, pmydata->increment, pmydata->max);
   //flush(stdout);
}

/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * pinfo )
{
   mydata_t * pmydata;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   pmydata = (mydata_t *) malloc( sizeof( mydata_t ) );

   MPI_Comm_rank(MPI_COMM_WORLD, &(pmydata->commrank));
   MPI_Comm_size(MPI_COMM_WORLD, &(pmydata->commsize));

   /* get environment variables for the kernel */
   evaluate_environment(pmydata);

   IDL(3, printf("\nim bi_getinfo: rank=%d size=%d\n",pmydata->commrank, pmydata->commsize));

   pinfo->numfunctions = 1;

   /* allocating memory for y axis texts and properties */
   pinfo->yaxistexts = malloc(pinfo->numfunctions * sizeof(char*));
   pinfo->outlier_direction_upwards = malloc(pinfo->numfunctions * sizeof(int));
   pinfo->legendtexts = malloc(pinfo->numfunctions * sizeof(char*));
   pinfo->base_yaxis = malloc(pinfo->numfunctions * sizeof(double));

   /* setting up y axis texts and properties */
   pinfo->yaxistexts[0] = bi_strdup("FLOPS");
   pinfo->outlier_direction_upwards[0] = 0;
   pinfo->base_yaxis[0] = 0;
   pinfo->legendtexts[0] = bi_strdup("FLOPS");

   pinfo->base_xaxis = 0.0;
   
   pinfo->codesequence = bi_strdup("");
   pinfo->kerneldescription = bi_strdup("CGV kernel");
   pinfo->xaxistext = bi_strdup("Problem Size");
   pinfo->maxproblemsize = pmydata->steps;
   pinfo->num_processes = pmydata->commsize;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 1;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;

   /* free all used space */
   if (pmydata) free(pmydata);
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

   pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( pmydata == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   
   MPI_Comm_rank(MPI_COMM_WORLD, &(pmydata->commrank));
   MPI_Comm_size(MPI_COMM_WORLD, &(pmydata->commsize));

   evaluate_environment(pmydata);
  
   IDL(3, printf("\nim bi_init: rank=%d size=%d\n",pmydata->commrank, pmydata->commsize));
   IDL(3, printf("im bi_init: max=%d, min=%d, increment=%d, steps=%d\n",pmydata->max, pmydata->min, pmydata->increment, pmydata->steps));
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
  double dstart = 0.0, dend = 0.0, dtime = 0.0, bi_flops;
  /* flops stores the calculated FLOPS */
  double dres = 0.0;
  /* ii is used for loop iterations */
  myinttype ii = 0, imyproblemsize = (myinttype) iproblemsize;
  /* cast void* pointer */
  mydata_t * pmydata = (mydata_t *) mdpv;

  IDL(3, printf("\nrank=%d entered bi_entry\n",pmydata->commrank));
  /* calculate real problemsize */
  imyproblemsize = pmydata->min + ( (imyproblemsize - 1)  * pmydata->increment );

  numprocs = pmydata->commsize;
  my_rank = pmydata->commrank;

/*
  Arguments:
  <number of rows in physical area of the Laplace equation>
  <number of columns in physical area of the Laplace equation>
  <maximal number of iterations>
  <epsilon>
  <print and debug level 0..5>
*/

  IDL(3, printf("\nim bi_entry, vor CGV-start: rank=%d size=%d\n",pmydata->commrank, pmydata->commsize));

  bi_flops = cgv_all((int)imyproblemsize, (int)imyproblemsize, 500, 1e-6, 0);

  IDL(3, printf("rank=%d Problemsize=%d, Value=%f\n",pmydata->commrank, imyproblemsize, dres));

  if ( pmydata->commrank == 0 )
  {
    /* store the value for the x axis in dresults[0] */
    dresults[0] = (double)imyproblemsize;
    dresults[1] = bi_flops;
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
 * Revision 1.1  2007/01/22 14:54:19  hackenb
 * + initial commit :-)
 *
 *
 *
 *******************************************************************/
