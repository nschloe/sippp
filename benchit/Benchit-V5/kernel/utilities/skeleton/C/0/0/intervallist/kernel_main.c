/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton with list PARAMETER
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2007/06/19 12:57:44 $
 *******************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"

#include "simple.h"


/* Reads the environment variables used by this kernel. 
 * see interface.h for bi_getenv("name", exit_on_error)
 */
void evaluate_environment(mydata_t * pmydata)
{
   char * p = 0;
   unsigned int errors = 0, inumentries=0, ii;
   
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN", 1 );
   pmydata->min = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX", 1 );
   pmydata->max = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT", 1 );
   pmydata->increment = atoi( p );
   pmydata->steps = (myinttype) ( pmydata->max - pmydata->min + 1 ) / pmydata->increment;
   if (( pmydata->max - pmydata->min + 1 ) % pmydata->increment != 0) pmydata->steps++;

  /* think positive */
  pmydata->empty_list = 'f';
   
  p = bi_getenv( "BENCHIT_KERNEL_PROBLEMLIST", 0 );
  if ( p == NULL )
  {
    errors++;
    fprintf( stderr, "BENCHIT_KERNEL_PROBLEMLIST not set\n" ); fflush( stderr );
    pmydata->empty_list = 't';
  }
  else
  {
    if ( 0 == strlen(p) )
    {
      fprintf( stderr, "empty BENCHIT_KERNEL_PROBLEMLIST\n" ); fflush( stderr );
      pmydata->empty_list = 't';
    }
    else
    {
      /* for length of list  */
      inumentries=1; /* first commata means 1 entry already found */
      /* find out how many values are given in the list */
      while (p) 
      {
        p = strstr( p,",");
        if (p) 
        {
          p++;
          inumentries++;
        }
      }
      pmydata->problemlistsize=inumentries-1;
      /* allocate aray according to number of entries */
      pmydata->problemlist = (unsigned int *)malloc( sizeof(unsigned int) * inumentries);
      p = bi_getenv( "BENCHIT_KERNEL_PROBLEMLIST", 0 );
      pmydata->problemlist[0] = strtol(p, (char **)NULL, 10); /*entry bevore first commata*/
      for (ii=1;ii<inumentries; ii++) 
      {
        p = strstr( p,",")+1; /* pointer to next number in string */
        pmydata->problemlist[ii] = strtol(p, (char **)NULL, 10);
      }
    }
  }

  if ( errors > 0 )
  {
  fprintf( stderr, "There's at least one environment variable not set!\n" );
  exit( 1 );
  }

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
   pinfo->codesequence = bi_strdup( "start kernel; do nothing; " );
   pinfo->kerneldescription = bi_strdup( "simple skeleton for c kernels" );
   pinfo->xaxistext = bi_strdup( "Problem Size" );
   pinfo->maxproblemsize = penv->problemlistsize;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 0;
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
      pinfo->base_yaxis[0] = 10; //logarythmic axis 10^x
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

   pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( pmydata == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   evaluate_environment(pmydata);
//  fprintf( stderr, "max=%d, min=%d, increment=%d, steps=%d\n",pmydata->max, pmydata->min, pmydata->increment, pmydata->steps);
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
  myinttype ii = 0, imyproblemsize = (myinttype) iproblemsize;
  /* cast void* pointer */
  mydata_t * pmydata = (mydata_t *) mdpv;

  /* calculate real problemsize */
  //imyproblemsize = pmydata->min + ( (imyproblemsize - 1)  * pmydata->increment );
  imyproblemsize= pmydata->problemlist[iproblemsize];
  
  /* check wether the pointer to store the results in is valid or not */
  if ( dresults == NULL ) return 1;

  /* get the actual time
   * do the measurement / your algorythm
   * get the actual time
   */
  dstart = bi_gettime(); 
  dres = simple(&imyproblemsize); 
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
  dresults[0] = (double)imyproblemsize;
  dresults[1] = dtime;

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
 * Revision 1.1  2007/06/19 12:57:44  william
 * Skeleton takes values as komma separated list and executes them in the provided order . . .
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
