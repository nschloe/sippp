/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  c kernel skeleton for PAPI use
*  This file: interface between the kernel and BenchIT.
*
*  Author: Robert Schoene (robert.schoene@zih.tu-dresden.de)
*  Last change by: $Author: mickler $
*  $Revision: 1.4 $
*  $Date: 2005/12/13 00:40:33 $
*
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"

#include <papi.h>
#include <papiStdEventDefs.h>
/*  Header for local functions
 */
#include "work.h"

/** These variables will help us to keep the overview over the arrays
  * we access for our functions/data.
  */
/* Number of different ways an algorithm will be measured.
   Example: loop orders: ijk, ikj, jki, jik, kij, kji -> n_of_works=6 with
   each different loop order in an own function. */
int n_of_works;
/* Number of fixed functions we have per measurement.
   Example: execution time and MFLOPS are measured for each loop order
   -> n_of_sure_funcs_per_work=2 */
int n_of_sure_funcs_per_work;

int MIN, MAX, INCREMENT;

// this will handle the PAPI-Events

int papi_eventset = PAPI_NULL;
int papi_returnvalue = PAPI_OK;

// example event 1 fp ops
int event1 = PAPI_FP_OPS;
// example event 2 total L2 cache misses
int event2 = PAPI_L2_TCM;


/*  Header for local functions
 */
void evaluate_environment( void );
void handlePapiError( int );

/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * infostruct )
{
   int i = 0, j = 0; /* loop var for n_of_works */
   (void) memset ( infostruct, 0, sizeof( bi_info ) );
   /* get environment variables for the kernel */
   evaluate_environment();
   infostruct->codesequence = bi_strdup( "work_[1|2]()" );
   infostruct->xaxistext = bi_strdup( "Problem Size" );
   infostruct->maxproblemsize = (MAX-MIN+1)/INCREMENT;
   if( (MAX-MIN+1) % INCREMENT != 0 )
     infostruct->maxproblemsize++;
   infostruct->num_processes = 1;
   infostruct->num_threads_per_process = 0;

   infostruct->kernel_execs_mpi1 = 0;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   /* B ########################################################*/
   n_of_works = 2; /* number versions of this algorithm (ijk, ikj, kij, ... = 6 */
   n_of_sure_funcs_per_work = 3; /* time measurement FP_OPS, INT_OPS */
   /*########################################################*/
   infostruct->numfunctions = n_of_works * n_of_sure_funcs_per_work;

   /* allocating memory for y axis texts and properties */
   infostruct->yaxistexts = malloc( infostruct->numfunctions * sizeof( char* ) );
   if ( infostruct->yaxistexts == 0 )
   {
     fprintf( stderr, "Allocation of yaxistexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->outlier_direction_upwards = malloc( infostruct->numfunctions * sizeof( int ) );
   if ( infostruct->outlier_direction_upwards == 0 )
   {
     fprintf( stderr, "Allocation of outlier direction failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->legendtexts = malloc( infostruct->numfunctions * sizeof( char* ) );
   if ( infostruct->legendtexts == 0 )
   {
     fprintf( stderr, "Allocation of legendtexts failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->base_yaxis = malloc( infostruct->numfunctions * sizeof( double ) );
   if ( infostruct->base_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of base yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   /* setting up y axis texts and properties */
   for ( j = 0; j < n_of_works; j++ )
   {
      /* B ########################################################*/
      int index1 = 0 * n_of_works + j;
      int index2 = 1 * n_of_works + j;
      int index3 = 2 * n_of_works + j;
      // 1st function
      infostruct->yaxistexts[index1] = bi_strdup( "Calculation Time in s" );
      infostruct->outlier_direction_upwards[index1] = 1;
      infostruct->base_yaxis[index1] = 0;
      // 2nd function
      infostruct->yaxistexts[index2] = bi_strdup( "Floating Point Operations" );
      infostruct->outlier_direction_upwards[index2] = 0;
      infostruct->base_yaxis[index2] = 0;
      /*########################################################*/
      // 3rd function
      infostruct->yaxistexts[index3] = bi_strdup( "L2 Cache Misses" );
      infostruct->outlier_direction_upwards[index3] = 0;
      infostruct->base_yaxis[index3] = 0;
      switch ( j )
      {
         /* B ########################################################*/
         case 1: // 2nd version legend text; maybe (ikj)
            infostruct->legendtexts[index1] =
               bi_strdup( "Calculation Time in s (2)" ); // "... (ikj)"
            infostruct->legendtexts[index2] =
               bi_strdup( "Floating Point Operations (2)" ); // "... (ikj)"
            infostruct->legendtexts[index3] =
               bi_strdup( "L2 Cache Misses (2)" ); // "... (ijk)"
            break;
         case 0: // 1st version legend text; maybe (ijk)
         default:
            infostruct->legendtexts[index1] =
               bi_strdup( "Calculation Time in s (1)" ); // "... (ijk)"
            infostruct->legendtexts[index2] =
               bi_strdup( "Floating Point Operations (1)" ); // "... (ijk)"
            infostruct->legendtexts[index3] =
               bi_strdup( "L2 Cache Misses (1)" ); // "... (ijk)"
         /*########################################################*/
      }
   }
   if ( DEBUGLEVEL > 3 )
   {
      /* the next for loop: */
      /* this is for your information only and can be ereased if the kernel works fine */
      for ( i = 0; i < infostruct->numfunctions; i++ )
      {
         printf( "yaxis[%2d]=%s\t\t\tlegend[%2d]=%s\n",
            i, infostruct->yaxistexts[i], i, infostruct->legendtexts[i] );
      }
   }
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
   mydata_t* mdp;
   const PAPI_hw_info_t *hwinfo = NULL;
   mdp = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( mdp == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
/*   if ( problemsizemax > STEPS )
   {
      fprintf( stderr, "Illegal maximum problem size\n" ); fflush( stderr );
      exit( 127 );
   }*/
   /* B ########################################################*/
   /* malloc our own arrays in here */
   /*########################################################*/
   
   IDL(3,printf("\nPAPI init..."));
   if((papi_returnvalue = PAPI_library_init(PAPI_VER_CURRENT)) != PAPI_VER_CURRENT )
   {
      IDL(3,printf("error\n"));
      exit(1);
   }
   IDL(3,printf("done\n"));
   hwinfo = PAPI_get_hardware_info();
   if (hwinfo != NULL)
   {
   IDL(3,printf("\n-------------- PAPI Infos ------------\n"));
   IDL(3,printf("Nodes in the system    :%i\n",hwinfo->nnodes));
   IDL(3,printf("CPUs per node          :%i\n",hwinfo->ncpu));
   IDL(3,printf("Total CPUs             :%i\n",hwinfo->totalcpus));
   IDL(3,printf("Vendor ID Number of CPU:%i\n",hwinfo->vendor));
   IDL(3,printf("Vendor ID String of CPU:%i\n",hwinfo->vendor_string));
   IDL(3,printf("Model Number of CPU    :%i\n",hwinfo->model));
   IDL(3,printf("Model String of CPU    :%i\n",hwinfo->model_string));
   IDL(3,printf("Revision Number of CPU :%i\n",hwinfo->vendor_string));
   IDL(3,printf("(estimated) MHz of CPU :%f\n",hwinfo->mhz));
   IDL(3,printf("\n--------------------------------------\n",hwinfo->nnodes));
   }
   IDL(2,printf("This system has %d available counters.\n",PAPI_num_counters()));
   IDL(3,printf("\nPAPI create eventset..."));
   papi_returnvalue = PAPI_create_eventset(&papi_eventset);
   if (papi_returnvalue!=PAPI_OK)
   {
     IDL(3,printf("error\n"));
     handlePapiError(papi_returnvalue);
   }
   IDL(3,printf("done\n"));
   IDL(3,printf("\nPAPI add eventset... (1)"));
   papi_returnvalue = PAPI_add_event(papi_eventset,event1);
   if (papi_returnvalue!=PAPI_OK)
   {
     IDL(3,printf("error\n"));
     handlePapiError(papi_returnvalue);
   }
   IDL(3,printf("done\n"));
   IDL(3,printf("\nPAPI add eventset... (2)"));
   papi_returnvalue = PAPI_add_event(papi_eventset,event2);
   if (papi_returnvalue!=PAPI_OK)
   {
     IDL(3,printf("error\n"));
     handlePapiError(papi_returnvalue);
   }
   IDL(3,printf("done\n"));
   return (void*)mdp;
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
int bi_entry( void* mdpv, int problemsize, double* results )
{
  /* ts, te: the start and end time of the measurement */
  /* timeinsecs: the time for a single measurement in seconds */
  long long ts = 0, te = 0;
  long long  eventValues[2]={0,0};
  double timeinsecs = 0.0;
  /* flops stores the calculated FLOPS */
  // double flops = 0.0;
  /* j is used for loop iterations */
  int j = 0;
  /* cast void* pointer */
  mydata_t* mdp = (mydata_t*)mdpv;

  /* calculate real problemsize */
  problemsize = MIN + ( problemsize - 1 ) * INCREMENT;

  /* check wether the pointer to store the results in is valid or not */
  if ( results == NULL ) return 1;

  /* B ########################################################*/
  /* maybe some init stuff in here */
  mdp->dummy = 0;
  /*########################################################*/

  for ( j = 0; j < n_of_works; j++ )
  {
    /* B ########################################################*/
    int index1 = 0 * n_of_works + j;
    int index2 = 1 * n_of_works + j;
    int index3 = 2 * n_of_works + j;
    /* reset of reused values */
    ts = 0.0;
    te = 0.0;
    /* choose version of algorithm */
    switch ( j ) {
      case 1: // 2nd version legend text; maybe (ikj)
        /* take start time, do measurment, and take end time */
        ts = PAPI_get_real_usec();
          papi_returnvalue = PAPI_start(papi_eventset);
          if (papi_returnvalue!=PAPI_OK)
          {
            handlePapiError(papi_returnvalue);
          }
            work_2();
          papi_returnvalue = PAPI_stop(papi_eventset,eventValues);
          if (papi_returnvalue!=PAPI_OK)
          {
            handlePapiError(papi_returnvalue);
          }
        te = PAPI_get_real_usec();
        break;
      case 0: // 1st version legend text; maybe (ijk)
      default:
        /* take start time, do measurment, and take end time */
        ts = PAPI_get_real_usec();
          papi_returnvalue = PAPI_start(papi_eventset);
          if (papi_returnvalue!=PAPI_OK)
          {
            handlePapiError(papi_returnvalue);
          }
            work_1();
          papi_returnvalue = PAPI_stop(papi_eventset,eventValues);
          if (papi_returnvalue!=PAPI_OK)
          {
            handlePapiError(papi_returnvalue);
          }
        te = PAPI_get_real_usec();
    }
    /* calculate the used time (measured microseconds) */
    timeinsecs = (te - ts)/1000000.0;
    // this flops value is a made up! this calulations should be replaced
    // by something right for the choosen algorithm
    // flops = (double)problemsize;
    /* If the operation was too fast to be measured by the timer function,
     * mark the result as invalid */
    if ( timeinsecs < 1e-6 ) timeinsecs = INVALID_MEASUREMENT;
    /* store the results in results[1], results[2], ...
    * [1] for the first function, [2] for the second function
    * and so on ...
    * the index 0 always keeps the value for the x axis
    */
    /* B ########################################################*/
    // the xaxis value needs to be stored only once!
    if ( j == 0 ) results[0] = (double)problemsize;
    results[index1 + 1] = timeinsecs;
    results[index2 + 1] = eventValues[0];
    results[index3 + 1] = eventValues[0];
    /*########################################################*/
  }

  return 0;
}

/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   papi_returnvalue = PAPI_cleanup_eventset(papi_eventset);
   if (papi_returnvalue!=PAPI_OK)
   {
     handlePapiError(papi_returnvalue);
   }
   /* Free all memory and data structures, EventSet must be empty. */
   papi_returnvalue = PAPI_destroy_eventset(&papi_eventset);
   if (papi_returnvalue!=PAPI_OK)
   {
     handlePapiError(papi_returnvalue);
   }
   mydata_t* mdp = (mydata_t*)mdpv;
   /* B ########################################################*/
   /* may be freeing our arrays here */
   /*########################################################*/
   if ( mdp ) free( mdp );
   return;
}
/********************************************************************/
/*************** End of interface implementations *******************/
/********************************************************************/

/** Tries to measure the timer overhead for a single call to PAPI_get_real_usec().
 *  @return the calculated overhead in seconds
 */
double gettimeroverhead()
{
  long long start, stop;
  int s;

  start = PAPI_get_real_usec();
  for ( s = 0; s < 10000; s++ )
  {
    PAPI_get_real_usec();
  }
  stop = PAPI_get_real_usec();
  // E10 : 4 because of loop 6 because of usec instead of sec
  return ( stop - start ) / 1E10;
}

/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
   int errors = 0;
   char * p = 0;
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN", 0 );
   if ( p == 0 ) errors++;
   else MIN = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX", 0 );
   if ( p == 0 ) errors++;
   else MAX = atoi( p );
   p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT", 0 );
   if ( p == 0 ) errors++;
   else INCREMENT = atoi( p );
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      fprintf( stderr, "This kernel needs the following environment variables:\n" );
      fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_MIN\n" );
      fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_MAX\n" );
      fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT\n" );
      fprintf( stderr, "\nThis kernel will iterate from BENCHIT_KERNEL_PROBLEMSIZE_MIN\n\
to BENCHIT_KERNEL_PROBLEMSIZE_MAX, incrementing by\n\
BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT with each step.\n" );
      exit( 1 );
   }
}
void handlePapiError(int papiReturn)
{
    if (papiReturn==PAPI_OK)
      return;
    fprintf(stderr, "PAPI error %d: %s\n",papiReturn,PAPI_strerror(papiReturn));
    exit(1);
}

/*****************************************************************************

LOG-History

$Log: kernel_main.c,v $
Revision 1.4  2005/12/13 00:40:33  mickler
+ Removed references to bi_info::log_[xy]axis
+ Changed for new unified min, max, increment format

Revision 1.3  2005/11/22 01:26:53  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring

Revision 1.2  2005/11/09 12:07:00  rschoene
commenting ... bah

Revision 1.1  2005/11/09 09:07:37  rschoene
initial PAPI skeleton

*****************************************************************************/

