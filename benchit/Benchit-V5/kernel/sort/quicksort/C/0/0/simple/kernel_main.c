/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.2 $
 * $Date: 2006/07/03 22:54:02 $
 *******************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"

#include "simple.h"


/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t * pmydata)
{
   int errors = 0;
   char * p = 0;
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN", 0 );
   if ( p == NULL ) errors++;
   else pmydata->min = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX", 0 );
   if ( p == NULL ) errors++;
   else pmydata->max = atoi( p );
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
}

/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * pinfo )
{
   mydata_t * penv;
   myinttype ii=0;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   penv = (mydata_t *) malloc( sizeof( mydata_t ) );

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup( "partition, qsort(left), qsort(right)" );
   pinfo->kerneldescription = bi_strdup( "Quicksort" );
   pinfo->xaxistext = bi_strdup( "number of elements" );
   pinfo->maxproblemsize = penv->steps;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 0;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;
   pinfo->numfunctions = 6;

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
   for (ii=0; ii<pinfo->numfunctions;ii++)
   {
      pinfo->yaxistexts[0] = bi_strdup( "time in s" );
      pinfo->outlier_direction_upwards[0] = 0;
      pinfo->base_yaxis[0] = 10; //logarythmic axis 10^x
   } 
   pinfo->legendtexts[0] = bi_strdup( "integer (clib)" );
   pinfo->legendtexts[1] = bi_strdup( "float   (clib)" );
   pinfo->legendtexts[2] = bi_strdup( "double  (clib)" );
   pinfo->legendtexts[3] = bi_strdup( "integer (self)" );
   pinfo->legendtexts[4] = bi_strdup( "float   (self)" );
   pinfo->legendtexts[5] = bi_strdup( "double  (self)" );
/*   
   pinfo->legendtexts[0] = bi_strdup( "integer (stack)" );
   pinfo->legendtexts[1] = bi_strdup( "float   (stack)" );
   pinfo->legendtexts[2] = bi_strdup( "double  (stack)" );
*/
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

   /* get PARAMETERS */
   evaluate_environment(pmydata);

   pmydata->intarray = (myinttype *) malloc(sizeof(myinttype) * pmydata->max);
   pmydata->floatarray = (float *) malloc(sizeof(float) * pmydata->max);
   pmydata->doublearray =  (double *) malloc(sizeof(double) * pmydata->max);
   if ( pmydata->intarray == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   if ( pmydata->floatarray == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   if ( pmydata->doublearray == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   
   /*initialization for random sequence;*/
   //srandom( ( unsigned ) time( NULL ) );
   srandomdev();

   /* fill the lists with random values */
   for ( ii = 0; ii < pmydata->max; ii++ )
   {
      pmydata->intarray[ii] = (myinttype)random();
   }

   for ( ii = 0; ii < pmydata->max; ii++ )
   {  //( (random() + random() / random()) * pow( random(), 3 ) )
      pmydata->floatarray[ii] = (float) (pow( random(), 2 ) / random() );
   }

   for ( ii = 0; ii < pmydata->max; ii++ )
   {
      pmydata->doublearray[ii] = (double) (pow( random(), 2 ) / random() );
   }
   
/*
      printf("\nInteger-Numbers (%d)\n",pmydata->max);
      for (ii=0; ii<pmydata->max; ii += 5)
        {
		   printf("%d:\t %d\t %d\t %d\t %d\t %d\n",ii,pmydata->intarray[ii],pmydata->intarray[ii+1],pmydata->intarray[ii+2],pmydata->intarray[ii+3],pmydata->intarray[ii+4]);
		}

      printf("\nFloat-Numbers (%d)\n",pmydata->max);
      for (ii=0; ii<pmydata->max; ii += 5)
        {
		   printf("%d:\t %.1f\t\t %.1f\t\t %.1f\t\t %.1f\t\t %.1f\n",ii,pmydata->floatarray[ii],pmydata->floatarray[ii+1],pmydata->floatarray[ii+2],pmydata->floatarray[ii+3],pmydata->floatarray[ii+4]);
		}

      printf("\nDouble-Numbers (%d)\n",pmydata->max);
      for (ii=0; ii<pmydata->max; ii += 5)
        {
		   printf("%d:\t %.2f\t\t %.2f\t\t %.2f\t\t %.2f\t\t %.2f\n",ii,pmydata->doublearray[ii],pmydata->doublearray[ii+1],pmydata->doublearray[ii+2],pmydata->doublearray[ii+3],pmydata->doublearray[ii+4]);
		}

*/

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
  /* lists */
  myinttype * intarray;
  float * floatarray;
  double * doublearray;

  /* calculate real problemsize */
  imyproblemsize = pmydata->min + ( (imyproblemsize - 1)  * pmydata->increment );

  /* check wether the pointer to store the results in is valid or not */
  if ( dresults == NULL ) return 1;

  /* create temp-array's which will be filled and then sorted */
  intarray = (myinttype *) malloc(sizeof(myinttype) * imyproblemsize);
  floatarray = (float *) malloc(sizeof(float) * imyproblemsize);
  doublearray =  (double *) malloc(sizeof(double) * imyproblemsize);

  for (ii=0; ii<imyproblemsize; ii++)
  {
    intarray[ii] = pmydata->intarray[ii];
	floatarray[ii] = pmydata->floatarray[ii];
	doublearray[ii] = pmydata->doublearray[ii];
  }
  /* check wether values are unsorted*/
  ii = 0;
  ii += verify_int(intarray, imyproblemsize);
  ii += verify_float(floatarray, imyproblemsize);
  ii += verify_double(doublearray, imyproblemsize);
//  if (ii != 3) fprintf(stderr, "\n -- values are unsorted (clib-qsort) --\n");
  
  dresults[0] = (double)imyproblemsize;

  dstart = bi_gettime(); 
  qsort(intarray, imyproblemsize, sizeof(myinttype), quicksort_clib_myinttype);
  dend = bi_gettime();
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;
  dresults[1] = dtime;

  dstart = bi_gettime(); 
  qsort(floatarray, imyproblemsize, sizeof(float), quicksort_clib_flt);
  dend = bi_gettime();
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;
  dresults[2] = dtime;

  dstart = bi_gettime(); 
  qsort(doublearray, imyproblemsize, sizeof(double), quicksort_clib_dbl);
  dend = bi_gettime();
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;
  dresults[3] = dtime;
  
  /* check values */
  ii = 0;
  ii += verify_int(intarray, imyproblemsize);
  ii += verify_float(floatarray, imyproblemsize);
  ii += verify_double(doublearray, imyproblemsize);
//  if (ii = 3) fprintf(stderr, "\n -- values are sorted (clib-qsort) --\n");
  if (ii != 3) fprintf(stderr, "\nverification of sorted lists (clib-qsort) failed!!!\n");

  /* re-init unsorted values */ 
  for (ii=0; ii<imyproblemsize; ii++)
  {
    intarray[ii] = pmydata->intarray[ii];
	floatarray[ii] = pmydata->floatarray[ii];
	doublearray[ii] = pmydata->doublearray[ii];
  }
  /* check wether values are unsorted*/
  ii = 0;
  ii += verify_int(intarray, imyproblemsize);
  ii += verify_float(floatarray, imyproblemsize);
  ii += verify_double(doublearray, imyproblemsize);
//  if (ii != 3) fprintf(stderr, "\n -- values are unsorted (self-qsort) --\n");

  dstart = bi_gettime(); 
//  quicksort_int(intarray, 0, imyproblemsize);
  quicksort_wikipedia_int(intarray, 0, imyproblemsize);
  dend = bi_gettime();
//printf("\nIntarray finished:\n");
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;
  dresults[4] = dtime;

  dstart = bi_gettime(); 
//  quicksort_float(floatarray, 0, imyproblemsize);
  quicksort_wikipedia_flt(floatarray, 0, imyproblemsize);
  dend = bi_gettime();
//printf("\nFloatarray finished:\n");
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;
  dresults[5] = dtime;

  dstart = bi_gettime(); 
//  quicksort_double(doublearray, 0, imyproblemsize);
  quicksort_wikipedia_dbl(doublearray, 0, imyproblemsize);
  dend = bi_gettime();
//printf("\nDoublearray finished:\n");
  dtime = dend - dstart;
  dtime -= dTimerOverhead;
  if( dtime < dTimerGranularity ) dtime = INVALID_MEASUREMENT;
  dresults[6] = dtime;

//for (ii=0; ii<imyproblemsize; ii++) printf("%d %d\n",intarray[ii],pmydata->intarray[ii]);

  /* check values */
  if (!(verify_int(intarray, imyproblemsize))) 
  {
	  printf("\nverification of sorted lists (selfmade integer quicksort) failed!!!\n");
	  printf("\nIntarray:\n");
      for (ii=0; ii<imyproblemsize; ii += 5)
        {
		   printf("%d %d %d %d %d\n",intarray[ii],intarray[ii+1],intarray[ii+2],intarray[ii+3],intarray[ii+4]);
//		   printf("%d %d %d %d %d\n\n",pmydata->intarray[ii],pmydata->intarray[ii+1],pmydata->intarray[ii+2],pmydata->intarray[ii+3],pmydata->intarray[ii+4]);
		}
  }
  if (!(verify_float(floatarray, imyproblemsize))) 
  {
	  printf("\nverification of sorted lists (selfmade float quicksort) failed!!!\n");
	  printf("\nFloatarray:\n");
      for (ii=0; ii<imyproblemsize; ii += 5)
        {
		   printf("%f %f %f %f %f\n",floatarray[ii],floatarray[ii+1],floatarray[ii+2],floatarray[ii+3],floatarray[ii+4]);
		}
  }
  if (!(verify_double(doublearray, imyproblemsize))) 
  {
	  printf("\nverification of sorted lists (selfmade double quicksort) failed!!!\n");
	  printf("\ndoublearray:\n");
      for (ii=0; ii<imyproblemsize; ii++)
        {
		   printf("%f\t\t%f\n",doublearray[ii],pmydata->doublearray[ii]);
		}
  }
  	
  if(intarray) free(intarray);
  if(doublearray) free(doublearray);
  if(doublearray) free(floatarray);
  return 0;
}

/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   mydata_t * pmydata = (mydata_t*)mdpv;
   if ( pmydata->intarray ) free( pmydata->intarray );
   if ( pmydata->floatarray ) free( pmydata->floatarray );
   if ( pmydata->doublearray ) free( pmydata->doublearray );
   if ( pmydata ) free( pmydata );
   return;
}


/********************************************************************
 * Log-History
 * 
 * $Log: kernel_main.c,v $
 * Revision 1.2  2006/07/03 22:54:02  william
 * first fix for the memoryleak
 *
 * Revision 1.1  2006/07/03 22:34:28  william
 * quicksort up and running
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
