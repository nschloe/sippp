/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.3 $
 * $Date: 2007/07/11 10:36:30 $
 *******************************************************************/
 
#include "simple.h"

mydata_t * pmydata;
unsigned long long ullcount;

/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
   int errors = 0;
   char * p = 0;
   
   ullcount = (unsigned long long) 0;
   p = bi_getenv( "BENCHIT_KERNEL_LIST", 0 );
   if ( p == NULL ) 
   {
      fprintf( stderr, "couldn't find BENCHIT_KERNEL_LIST");
      errors++;
   }
   else
   {
	  pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
	  if ( pmydata == 0 )
	  {
	     fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
	     exit( 127 );
	  }
	  //      fprintf( stderr, "\nBENCHIT_KERNEL_LIST = %s, length=%d\n", p, strlen(p));
	  pmydata->plistenv = (char *) malloc(sizeof(char) * strlen(p) +1 );
	  if ( pmydata->plistenv == 0 )
	  {
	     fprintf( stderr, "Allocation of structure pmydata->plistenv failed\n" ); fflush( stderr );
	     exit( 127 );
	  }
	  strncpy(pmydata->plistenv, p, strlen(p)+1);
//	  fprintf( stderr, "\nBENCHIT_KERNEL_LIST = %s, length=%d\n", pmydata->plistenv, strlen(pmydata->plistenv));
   }   
   
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      exit( 1 );
   }
   
   pmydata->plist=bi_parselist( &ullcount, pmydata->plistenv );
}

void bi_getinfo( bi_info * pinfo )
{
/*
	mydata_t * penv;
   unsigned long long ii=0;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   penv = (mydata_t *) malloc( sizeof( mydata_t ) );
*/
   /* get environment variables for the kernel */
   evaluate_environment();
   pinfo->codesequence = bi_strdup( "go through array and change neighbours if necessary" );
   pinfo->kerneldescription = bi_strdup( "Bubblesort (version)" );
   pinfo->xaxistext = bi_strdup( "number of elements" );
   //pinfo->maxproblemsize = penv->steps;
   pinfo->maxproblemsize = ullcount;
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
      pinfo->legendtexts[0] = bi_strdup( "(version) elements" );
 
   /* free all used space */
/*   if (penv) free( penv ); */
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

   unsigned long long ii;
   bi_list_t * p;
	
   /*initialization for random sequence;*/
   srandom( ( unsigned ) time( NULL ) );
/*
   pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( pmydata == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   evaluate_environment(pmydata);

   pmydata->plist = bi_parselist( &ii, pmydata->plistenv );
*/   
/*
   fprintf( stderr, "\nnumber of elements = %llu\n", ullcount);

   ii = ullcount;
   p = pmydata->plist;
   while ( ii > 0)
   {
     fprintf( stderr, "\n%f",p->dnumber);
	 p=p->pnext;
	 ii--;
   }
*/

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
  double dstart = 0.0, dend = 0.0, dtime = 0.0;
  double dres = 0.0;
  float * psortarray = NULL;
  unsigned long long imyproblemsize, ii;
  bi_list_t * p;
/*  
  fprintf( stderr, "\nentering bi_entry");
*/
  ii=iproblemsize-1;
  p = pmydata->plist;
  while ( ii > 0)
  {
/*
     fprintf( stderr, "\n%f",p->dnumber);
*/	 p=p->pnext;
	 ii--;
  }
  imyproblemsize = (unsigned long long) p->dnumber;
/*  
  fprintf( stderr, "\nimyproblemsize=%llu",imyproblemsize);fflush(stderr);
*/  
  psortarray = (float *)(malloc( sizeof(double) * imyproblemsize) );
  
  for ( ii = 0; ii < imyproblemsize; ii++ )
  {
       psortarray[ii] = ( float ) ( random() + random() /
               random() * pow( random(), 3 ) );
/*
       fprintf( stderr, "\npsortarray[%llu]=%f",ii,psortarray[ii]);fflush(stderr);
*/
  }



  /* check wether the pointer to store the results in is valid or not */
  if ( dresults == NULL ) return 1;

  /* get the actual time
   * do the measurement / your algorythm
   * get the actual time
   */
  dstart = bi_gettime(); 
  dres = 0;
  fprintf( stderr, "\nbubblesortf(psortarray,imyproblemsize=%llu)",imyproblemsize);fflush(stderr);
  bubblesortf( psortarray, imyproblemsize );
  dend = bi_gettime();
/*
  fprintf( stderr, "Problemsize=%llu, Value=%f\n", imyproblemsize, dres);
*/
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

  if ( !( verifyf(psortarray,imyproblemsize)) ) printf( "Verification intsort failed! \n" );

  free( psortarray );
  
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
 *******************************************************************/ 
