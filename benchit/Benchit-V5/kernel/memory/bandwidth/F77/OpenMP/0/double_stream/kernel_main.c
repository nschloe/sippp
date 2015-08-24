/******************************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: measure Bandwidth inspired by STREAM benchmark (FORTRAN OMP-version)
 *
 * according to the rules, reffer this Benchmark as:
 * "BenchIT kernel based on a variant of the STREAM benchmark code"
 * when publishing results
 *
 * This file connects the work-part (work.f) to the interface (benchit.c)
 * via their headers
 * 
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.4 $
 * $Date: 2007/02/13 13:21:33 $
 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <omp.h>
#include "interface.h"
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

long minlength, maxlength, accessstride, numjumps,internal_repeats,offset;
double dMemFactor;
long nMeasurements;

/*  Header for local functions
 */
void evaluate_environment( void );
void checkSTREAMresults(double* a,double* b, double *c,int length, int NTIMES);

/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * infostruct )
{
  char buf[200];
   int i = 0, j = 0; /* loop var for n_of_works */
   (void) memset ( infostruct, 0, sizeof( bi_info ) );
   /* get environment variables for the kernel */
   evaluate_environment();
   
   sprintf(buf,"STREAM inspired benchmark (F77+OpenMP)#"
               "OFFSET=%i#"
               "NTIMES=%i#"
               "THREADS=%i",offset,internal_repeats,omp_get_max_threads());
   infostruct->codesequence = bi_strdup( buf );
   infostruct->xaxistext = bi_strdup( "N (Length of vectors)" );
   infostruct->maxproblemsize = nMeasurements;
   infostruct->num_processes = 1;
   infostruct->num_threads_per_process = omp_get_max_threads();
   infostruct->kernel_execs_mpi1 = 0;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 1;
   infostruct->kernel_execs_pthreads = 0;
   /* B ########################################################*/
   n_of_works = 4; /* number versions of this algorithm (ijk, ikj, kij, ... = 6 */
   n_of_sure_funcs_per_work = 2; /* time measurement and FLOPS (calculated) */
   /*########################################################*/
   infostruct->numfunctions = n_of_works * n_of_sure_funcs_per_work;
      infostruct->base_xaxis = 10;

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
      //int index3 = 2 * n_of_works + j;
      // 1st function
      infostruct->yaxistexts[index1] = bi_strdup( "Time in Seconds" );
      infostruct->outlier_direction_upwards[index1] = 1;
      infostruct->base_yaxis[index1] = 10;
      // 2nd function
      infostruct->yaxistexts[index2] = bi_strdup( "Bandwidth in MByte/s" );
      infostruct->outlier_direction_upwards[index2] = 0;
      infostruct->base_yaxis[index2] = 10;
      /*########################################################*/
      // 3rd function
      //infostruct->yaxistexts[index3] = bi_strdup( "" );
      //infostruct->outlier_direction_upwards[index3] = 0;
      //infostruct->base_yaxis[index3] = 0;
      switch ( j )
      {
         /* B ########################################################*/
         case 3: // 2nd version legend text; maybe (ikj)
            infostruct->legendtexts[index1] =
               bi_strdup( "Time Triad" ); // "... (ijk)"
            infostruct->legendtexts[index2] =
               bi_strdup( "Bandwidth Triad" ); // "... (ijk)"
            break;
         case 2: // 2nd version legend text; maybe (ikj)
            infostruct->legendtexts[index1] =
               bi_strdup( "Time Add" ); // "... (ijk)"
            infostruct->legendtexts[index2] =
               bi_strdup( "Bandwidth Add" ); // "... (ijk)"
            break;
         case 1: // 2nd version legend text; maybe (ikj)
            infostruct->legendtexts[index1] =
               bi_strdup( "Time Scale" ); // "... (ijk)"
            infostruct->legendtexts[index2] =
               bi_strdup( "Bandwidth Scale" ); // "... (ijk)"
            break;
         case 0: // 1st version legend text; maybe (ijk)
         default:
            infostruct->legendtexts[index1] =
               bi_strdup( "Time Copy" ); // "... (ijk)"
            infostruct->legendtexts[index2] =
               bi_strdup( "Bandwidth Copy" ); // "... (ijk)"
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
   }/*
   /* B ########################################################*/
   /* malloc our own arrays in here */
   //fprintf( stderr, "maxlength=%i\n",maxlength ); fflush( stderr );
   mdp->a=(double*)malloc(maxlength*sizeof(double)+offset);
   if (mdp->a==0)
   {
      fprintf( stderr, "Allocation of structure a failed\n" ); fflush( stderr );
      exit( 127 );
   }
   mdp->b=(double*)malloc(maxlength*sizeof(double)+offset);
   if (mdp->b==0)
   {
      fprintf( stderr, "Allocation of structure b failed\n" ); fflush( stderr );
      exit( 127 );
   }
   mdp->c=(double*)malloc(maxlength*sizeof(double)+offset);
   if (mdp->c==0)
   {
      fprintf( stderr, "Allocation of structure c failed\n" ); fflush( stderr );
      exit( 127 );
   }
   /*########################################################*/
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
  double start[4][internal_repeats];
  double stop[4][internal_repeats];
  double best[4];
  /* flops stores the calculated FLOPS */
  double flops = 0.0,timeinsecs=0.0;
  int k=0;
  /* j is used for loop iterations */
  int j = 0;
  /* cast void* pointer */
  mydata_t* mdp = (mydata_t*)mdpv;
	double* a=mdp->a;
	double* b=mdp->b;
	double* c=mdp->c;
	double scalar=3.0;
	double modifier=0.0;
	int length=0;
  length=(long)(((double)minlength)*pow(dMemFactor, (problemsize-1)));
  //bi_cleanup(mdpv);
  //mdpv=bi_init(length+offset);
  
  /* check wether the pointer to store the results in is valid or not */
  if ( results == NULL ) return 1;
  for (j=0; j<length; j++) {
		a[j] = 1.0;
		b[j] = 2.0;
		c[j] = 0.0;
	}
	for (k=0;k<internal_repeats;k++)
	{
        start[0][k]=bi_gettime();
        	copy_(c,a,&length);
        stop[0][k]=bi_gettime();
        
        start[1][k]=bi_gettime();
        scale_(b,c,&scalar,&length);
        stop[1][k]=bi_gettime();
        
        start[2][k]=bi_gettime();
        add_(c,a,b,&length);
        stop[2][k]=bi_gettime();
        
        start[3][k]=bi_gettime();
        triad_(a,b,c,&scalar,&length);
        stop[3][k]=bi_gettime();
  }
  best[0]=stop[0][0]-start[0][0];
  best[1]=stop[1][0]-start[1][0];
  best[2]=stop[2][0]-start[2][0];
  best[3]=stop[3][0]-start[3][0];
  for (j=0;j<4;j++)
  for (k=1;k<internal_repeats;k++)
  {
  	if (best[j]>(stop[j][k]-start[j][k]))
  		best[j]=stop[j][k]-start[j][k];
  }
  for ( j = 0; j < n_of_works; j++ )
  {
    /* B ########################################################*/
    int index1 = 0 * n_of_works + j;
    int index2 = 1 * n_of_works + j;
    if (j>1)
    	modifier=3.0;
    else
    	modifier=2.0;
    /* calculate the used time and FLOPS */
    timeinsecs = best[j];
    timeinsecs -= dTimerOverhead;
    if( timeinsecs < 5*dTimerGranularity ) timeinsecs = INVALID_MEASUREMENT;
    // this flops value is a made up! this calulations should be replaced
    // by something right for the choosen algorithm
    flops = (1.0E-6*length*modifier*sizeof(double))/timeinsecs;
    if( timeinsecs < 5*dTimerGranularity ) flops = INVALID_MEASUREMENT;
    /* If the operation was too fast to be measured by the timer function,
     * mark the result as invalid */
    /* store the results in results[1], results[2], ...
    * [1] for the first function, [2] for the second function
    * and so on ...
    * the index 0 always keeps the value for the x axis
    */
    /* B ########################################################*/
    // the xaxis value needs to be stored only once!
    if ( j == 0 ) results[0] = length;
    results[index1 + 1] = timeinsecs;
    results[index2 + 1] = flops;
    /*########################################################*/
  }
  IDL(3,checkSTREAMresults(a,b,c,length,internal_repeats));
  return 0;
}

void checkSTREAMresults (double* a,double* b,double* c,int length, int NTIMES)
{
	double aj,bj,cj,scalar;
	double asum,bsum,csum;
	double epsilon;
	int	j,k;

    /* reproduce initialization */
	aj = 1.0;
	bj = 2.0;
	cj = 0.0;
    /* a[] is NOT modified during timing check */
	//aj = 2.0E0 * aj;
    /* now execute timing loop */
	scalar = 3.0;
	for (k=0; k<NTIMES; k++)
        {
            cj = aj;
            bj = scalar*cj;
            cj = aj+bj;
            aj = bj+scalar*cj;
        }
	aj = aj * (double) (length);
	bj = bj * (double) (length);
	cj = cj * (double) (length);

	asum = 0.0;
	bsum = 0.0;
	csum = 0.0;
	for (j=0; j<length; j++) {
		asum += a[j];
		bsum += b[j];
		csum += c[j];
	}
#ifdef VERBOSE
	printf ("Results Comparison: \n");
	printf ("        Expected  : %f %f %f \n",aj,bj,cj);
	printf ("        Observed  : %f %f %f \n",asum,bsum,csum);
#endif

#define abs(a) ((a) >= 0 ? (a) : -(a))
	epsilon = 1.e-8;

	if (abs(aj-asum)/asum > epsilon) {
		printf ("Failed Validation on array a[]\n");
		printf ("        Expected  : %f \n",aj);
		printf ("        Observed  : %f \n",asum);
	}
	else if (abs(bj-bsum)/bsum > epsilon) {
		printf ("Failed Validation on array b[]\n");
		printf ("        Expected  : %f \n",bj);
		printf ("        Observed  : %f \n",bsum);
	}
	else if (abs(cj-csum)/csum > epsilon) {
		printf ("Failed Validation on array c[]\n");
		printf ("        Expected  : %f \n",cj);
		printf ("        Observed  : %f \n",csum);
	}
	else {
		printf ("Solution Validates\n");
	}
}
/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   mydata_t* mdp = (mydata_t*)mdpv;
   /* B ########################################################*/
   /* may be freeing our arrays here */
  	free(mdp->a);
  	free(mdp->b);
  	free(mdp->c);
   /*########################################################*/
   if ( mdp ) free( mdp );
   return;
}
/********************************************************************/
/*************** End of interface implementations *******************/
/********************************************************************/

/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
   int errors = 0;
   char * p = 0;
  char *envir;
  IDL(3,printf("Init global variables ... "));
  envir=bi_getenv("BENCHIT_KERNEL_MIN_ACCESS_LENGTH",1);
  if (envir==0)
  {
  	printf("BENCHIT_KERNEL_MIN_ACCESS_LENGTH not found! Exiting");
  	exit(127);
  }
  minlength=atoi(envir) ;
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_MAX_ACCESS_LENGTH",1);
  if (envir==0)
  {
  	printf("BENCHIT_KERNEL_MAX_ACCESS_LENGTH not found! Exiting");
  	exit(127);
  }
  maxlength=atoi(envir);
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_ACCESS_STEPS",1);
  if (envir==0)
  {
  	printf("BENCHIT_KERNEL_ACCESS_STEPS not found! Exiting");
  	exit(127);
  }
  nMeasurements =atoi(envir);
  dMemFactor =((double)maxlength)/((double)minlength);
  dMemFactor = pow(dMemFactor, 1.0/((double)nMeasurements-1));
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_INTERNAL_NUM_MEASUREMENT",1);
  if (envir==NULL)
  {
  	printf("BENCHIT_KERNEL_INTERNAL_NUM_MEASUREMENT not found! Exiting");
  	exit(127);
  }
  internal_repeats=atoi(envir);
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_OFFSET",1);
  if (envir==NULL)
  {
  	printf("BENCHIT_KERNEL_OFFSET not found! Exiting");
  	exit(127);
  }
  offset=atoi(envir);
  IDL(3,printf("done\n"));
}


/********************************************************************
 * Log-History
 * 
 * $Log: kernel_main.c,v $
 * Revision 1.4  2007/02/13 13:21:33  rschoene
 * changed y axis names
 *
 * Revision 1.3  2007/02/13 12:56:37  rschoene
 * changed infostruct->omp and num_threads_per_process
 *
 * Revision 1.1  2007/02/13 12:48:00  rschoene
 * STREAM-Benchmark F77 + OpenMP
 *
 * 
 *******************************************************************/
