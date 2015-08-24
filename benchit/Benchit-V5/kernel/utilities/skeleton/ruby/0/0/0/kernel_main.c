/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: <description>
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: mueller $
 * $Revision: 1.2 $
 * $Date: 2006/05/23 08:10:27 $
 *******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"
#include "kernel_main.h"
#include "tools/benchscript.h"


/* FIRST: implement the BenchIT Interface functions */

/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * pinfo )
{
   int i, j, graph = 0;
   char* legend_text;
   mydata_t * penv;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   penv = (mydata_t *) malloc( sizeof( mydata_t ) );

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup( "start kernel; do nothing; " );
   pinfo->kerneldescription = bi_strdup( "simple skeleton for ruby kernels" );
   pinfo->xaxistext = bi_strdup( "Problem Size" );
   pinfo->base_xaxis = 2;
   pinfo->maxproblemsize = penv->steps;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 0;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;
   pinfo->numfunctions = penv->num_processes * penv->num_threads;

   /* allocating memory for y axis texts and properties */
   pinfo->yaxistexts = malloc( pinfo->numfunctions * sizeof( char* ) );
   if ( pinfo->yaxistexts == NULL )
   {
     fprintf( stderr, "Allocation of yaxistexts failed.\n" ); 
     fflush( stderr );
     exit( 127 );
   }
   pinfo->outlier_direction_upwards = malloc( pinfo->numfunctions * 
     sizeof( int ) );
   if ( pinfo->outlier_direction_upwards == NULL )
   {
     fprintf( stderr, "Allocation of outlier direction failed.\n" ); 
     fflush( stderr );
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
   for( i=1; i <= penv->num_processes; i++ )
   {
     for( j=1; j <= penv->num_threads; j++ )
     {
       pinfo->yaxistexts[graph] = bi_strdup( "time in s" );
    	 pinfo->outlier_direction_upwards[graph] = 0;
   	 	 pinfo->base_yaxis[graph] = penv->logbase;
   	 	 
   	 	 /* build legend text */
		   legend_text = bi_strdup( "read speed" );
		   if( penv->num_processes > 1 )
		   {
		     legend_text = bi_strcat( legend_text, " processes: " );
		     legend_text = bi_strcat( legend_text, int2char( i ) );   
		   }
		   if( penv->num_threads > 1 )
		   {
		     legend_text = bi_strcat( legend_text, " threads: " );
		     legend_text = bi_strcat( legend_text, int2char( j ) );
		   }
		   
   		 pinfo->legendtexts[graph] = bi_strdup( legend_text );
   		 graph++;
   	 }
   }

   /* free all used space */
   if (penv) free( penv );
}

/* SECOND implement local functions */

/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t * pmydata)
{
  int errors = 0;
  char * p = 0;

  /* read the kernel specific values */ 
  p = bi_getenv( "BENCHIT_KERNEL_LOGBASE", 0 );
  if ( p == NULL ) errors++;
  else pmydata->logbase = atoi( p );
   
  p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN_POWER", 0 );
  if ( p == NULL ) errors++;
  else pmydata->min = atoi( p );

  p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX_POWER", 0 );
  if ( p == NULL ) errors++;
  else pmydata->max = atoi( p );

	p = bi_getenv( "BENCHIT_KERNEL_MAX_PROCESSES", 1 );
  if ( p == NULL ) errors++;
  else pmydata->num_processes = atoi( p );
  
  p = bi_getenv( "BENCHIT_KERNEL_MAX_THREADS", 0 );
  if ( p == NULL ) errors++;
  else pmydata->num_threads = atoi( p );

  /* read the script specifics */
  p = bi_getenv( "BENCHIT_RUBY_INTERPRETER", 0 );
  if ( p == NULL ) errors++;
  else pmydata->interpreter = bi_strdup( p );

  p = bi_getenv( "KERNELDIR", 0 );
  if ( p == NULL ) errors++;
  else pmydata->kerneldir = bi_strdup( p );

  p = bi_getenv( "BENCHIT_KERNEL_MIN_RUNTIME", 0 );
  if ( p == NULL ) errors++;
  else pmydata->min_runtime = atoi( p );

  if ( errors > 0 )
  {
		fprintf( stderr, "There's at least one environment variable not set!\n" );
  	fflush( stderr );
		exit( 1 );
  }

	/* calculate the number of steps to run	  */
  pmydata->steps = (int) ( pmydata->max - pmydata->min + 1 );
}

char* bi_build_work_script( char* script_file, char *meas_mode, void* pdata )
{
  char *work_script;

  mydata_t * pmydata = (mydata_t *) pdata;

  work_script = bi_strcat( pmydata->interpreter, " " );
  work_script = bi_strcat( work_script, pmydata->kerneldir );
  work_script = bi_strcat( work_script, "/" );
  work_script = bi_strcat( work_script, script_file );
  work_script = bi_strcat( work_script, " --BIMode " );
  work_script = bi_strcat( work_script, meas_mode );

	return work_script;
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
  char *work_script;
	mydata_t * pmydata;

  pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
  if ( pmydata == 0 )
  {
  	fprintf( stderr, "Allocation of structure mydata_t failed\n" ); 
  	fflush( stderr );
  	exit( 127 );
  }
  evaluate_environment(pmydata);

	/* initialize the database for measurement */
	work_script = bi_build_work_script( "work_script.rb", "init", pmydata );
 	work_script = bi_strcat( work_script, " --BIProblemSize " );
  work_script = bi_strcat( work_script, int2char( pow( pmydata->logbase, 
    pmydata->max ) ) );

	/* call the interpreter without multiprocessing or -threading */
	bi_script( (void*) work_script, 1, 1 );
 
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
int bi_entry( void* mdpv, int iproblemsize, double* dresults )
{
  /* dstart, dend: the start and end time of the measurement */
  /* dtime: the time for a single measurement in seconds */
  double start = 0.0, end = 0.0, time = 0.0;
  
  /* i: is used for loop iterations */
  int normalize_count = 0, proc_count, thread_count, graph_count = 1;
  int myproblemsize = (int) iproblemsize;

	/* work_script:  is used to store th script na/me and params */
  char *work_script;

  /* cast void* pointer */
  mydata_t * pmydata = (mydata_t *) mdpv;

  /* calculate real problemsize */
  myproblemsize = pow( pmydata->logbase, ( myproblemsize - 1 + pmydata->min ) );

  /* check wether the pointer to store the results in is valid or not */
  if ( dresults == NULL ) return 1;

	work_script = bi_build_work_script( "work_script.rb", "read", pmydata );
  work_script = bi_strcat( work_script, " --BIProblemSize " );
  work_script = bi_strcat( work_script, int2char( myproblemsize ) );

  /* start */
  for( proc_count = 1; proc_count <= pmydata->num_processes; proc_count++ )
  {
    for( thread_count = 1; thread_count <= pmydata->num_threads; 
      thread_count++ )
    {
	    //reset the normalize counter
    	normalize_count = 0;
	
			do
  		{
   		  /* get the actual time */
	  		start = bi_gettime(); 
	  		/* do measurement */
	  		bi_script( (void*) work_script, proc_count, thread_count );
				/* get actual time */
 				end = bi_gettime();

  			/* cumulate the used time */
	  		time += ( end - start );
	  		time -= dTimerOverhead;
	  		
	  		normalize_count++;
  		}
  		while( time <= (float) pmydata->min_runtime ); 
  
  		/* calculate the average time used for a run */
  		time = time / normalize_count;
      
  		/* if the operation was too fast to be measured mark as invalid */
  		if( time < dTimerGranularity ) time = INVALID_MEASUREMENT;

  		/* store the result */
  		dresults[graph_count] = time;
  		
  		graph_count++;
    }
  }

	/* store the problemsize */
  dresults[0] = (double) myproblemsize;

  return 0;
}

/** 
  Clean up the memory
  currently not used
*/
void bi_cleanup( void* dummy )
{ 
   return;
}
/********************************************************************/
/*************** End of interface implementations *******************/
/********************************************************************/


/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: <description>
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: mueller $
 * $Revision: 1.2 $
 * $Date: 2006/05/23 08:10:27 $
 *******************************************************************/
