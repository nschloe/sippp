/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
* Contact: benchit@zih.tu-dresden.de
*
*  c DGEMV kernel
*  This file: interface between the kernel and BenchIT.
*
*  Author: Robert Schoene (robert.schoene@tu-dresden.de)
*  Last change by: $Author: rschoene $
*  $Revision: 1.4 $
*  $Date: 2007/07/06 05:43:14 $
*
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"
/*  Header for local functions
 */
#include "work.h"


/** These variables will help us to keep the overview over the arrays
  * we access for our functions/data.
  */
/* Number of different ways an algorithm will be measured.
   normal, blas, sse
  */
int n_of_works;
/* Number of fixed functions we have per measurement.
   execution time and MFLOPS are measured for each loop order
   -> n_of_sure_funcs_per_work=2 */
int n_of_sure_funcs_per_work;


myinttype MIN;
myinttype MAX;
myinttype INCREMENT;

/*  Header for local functions
 */
void evaluate_environment( void );

void initData(mydata_t* mds,int n)
{
   int i,j;
   for (i=0;i<n;i++)
   {
      mds->a[i]=1.1;
      mds->y[i]=1.1;
      for (j=0;j<n;j++)
         mds->x[i*n+j]=0.01;
   }
}


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
  infostruct->codesequence = bi_strdup( "for (i=0;i<sizeVector;i++)#  for (j=0;j<sizeAusgabe;j++)#    y[j]=alpha*a[i]*x[i*sizeAusgabe+j]+y[j];");
   infostruct->xaxistext = bi_strdup( "Matrix Size" );
  infostruct->kerneldescription = bi_strdup( "Matrix Vector Multiply (C)" );
   infostruct->maxproblemsize = (int)((MAX-MIN)/INCREMENT);
   infostruct->kernel_execs_mpi1 = 0;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   /* B ########################################################*/
   n_of_works = 2; /* number versions of this algorithm */
   n_of_sure_funcs_per_work = 1; /* MFLOPS (calculated) */
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
      // 1st function
      infostruct->yaxistexts[index1] = bi_strdup( "FLOPS" );
      infostruct->outlier_direction_upwards[index1] = 0;
      infostruct->base_yaxis[index1] = 0;
      switch ( j )
      {
         /* B ########################################################*/
     case 0:
        infostruct->legendtexts[ index1 ] =
          bi_strdup( "FLOPS (ij)" );
        break;
      case 1:
        infostruct->legendtexts[ index1 ] =
          bi_strdup( "FLOPS (ji)" );
        break;
      default:
        fprintf( stderr, "Should not reach default section of case.\n" );
        fflush( stderr );
        exit( 127 );
         /*case 0:
         default:
            infostruct->legendtexts[index1] =
               bi_strdup( "Normal - Calculation Time in s" );
            infostruct->legendtexts[index2] =
               bi_strdup( "Normal - FLOPS" );
            infostruct->legendtexts[index3] =
               bi_strdup( "Normal - SSE Instructions" );*/
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
   if ( problemsizemax > MAX )
   {
      fprintf( stderr, "Illegal maximum problem size\n" ); fflush( stderr );
      exit( 127 );
   }


  /* calculate real maximum problem size */
  problemsizemax=MAX;
  mdp->x=malloc( (problemsizemax*problemsizemax) * sizeof(double));
  IDL( 3, printf("Alloc 1 done\n"));
  mdp->y=malloc( (problemsizemax) * sizeof(double));
  IDL( 3, printf("Alloc 2 done\n"));
  mdp->a=malloc( (problemsizemax) * sizeof(double));
  IDL( 3, printf("Alloc 3 done\n"));
  
  
  if( (mdp->a==0) || (mdp->x==0) || (mdp->y==0))
    {
      printf("malloc (%ld bytes) failed in bi_init()\n",
	     (long) ((2.0+problemsizemax)*problemsizemax * sizeof(double)));
      bi_cleanup( mdp);
      exit(127);
    }

  IDL( 2, printf("leave bi_init\n"));
  
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
  double start=0.0;
  double stop=0.0;
  // used for precision
  long numberOfRuns=1,i=0;
  int j=0;
   mydata_t* mdp = (mydata_t*)mdpv;
  
  /* calculate real problemsize */
  problemsize = MIN + ( problemsize - 1 ) * INCREMENT;
  

   /* check wether the pointer to store the results in is valid or not */
   if ( results == NULL ) return 1;

   /* B ########################################################*/
   /* maybe some init stuff in here */
   initData(mdpv,problemsize);
   /*########################################################*/

   for ( j = 0; j < n_of_works; j++ )
   {
      int index1 = 0 * n_of_works + j;
    // reset variables
    numberOfRuns=1;
    start=0.0;
    stop=0.0;
      /* B ########################################################*/
      /* choose version of algorithm */
      switch ( j ) {
         case 1:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                   {
                     ji_(problemsize,problemsize, 1.1,0.9,mdp->a, mdp->x, mdp->y);
                   }
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.01 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
                 break;
         case 0:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                     ij_(problemsize,problemsize, 1.1,0.9,mdp->a, mdp->x, mdp->y);
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.01 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
                 break;
      }
      /* store the results in results[1], results[2], ...
      * [1] for the first function, [2] for the second function
      * and so on ...
      * the index 0 always keeps the value for the x axis
      */
      /* B ########################################################*/
      // the xaxis value needs to be stored only once!
      if ( j == 0 ) results[0] = (double)problemsize;
      results[index1 + 1]= (2.0*problemsize+2.0*problemsize*problemsize)/stop;
      /*########################################################*/
   }

   return 0;
}

/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   mydata_t* mdp = (mydata_t*)mdpv;
   /* B ########################################################*/
   /* may be freeing our arrays here */
   if (mdp->a) free(mdp->a);
   if (mdp->x) free(mdp->x);
   if (mdp->y) free(mdp->y);
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
  MIN = 1;
  MAX = 500;
  INCREMENT = 1;
  p = bi_getenv( "BENCHIT_DGEMV_C_DOUBLE_MIN", 0 );
  if ( p == 0 ) errors++;
  else MIN = atoi( p );
  p = bi_getenv( "BENCHIT_DGEMV_C_DOUBLE_MAX", 0 );
  if ( p == 0 ) errors++;
  else MAX = atoi( p );
  p = bi_getenv( "BENCHIT_DGEMV_C_DOUBLE_INCREMENT", 0 );
  if ( p == 0 ) errors++;
  else INCREMENT = atoi( p );
  if ( errors > 0 )
  {
    fprintf( stderr, "There's at least one environment variable not set!\n" );
    fprintf( stderr, "This kernel needs the following environment variables:\n" );
    fprintf( stderr, "BENCHIT_DGEMV_C_DOUBLE_MIN\n" );
    fprintf( stderr, "BENCHIT_DGEMV_C_DOUBLE_MAX\n" );
    fprintf( stderr, "BENCHIT_DGEMV_C_DOUBLE_INCREMENT\n" );
    fprintf( stderr, "\tThis kernel will iterate from MIN to MAX in steps with size INCREMENT.\n" );
    exit( 1 );
  }
}

/*****************************************************************************

LOG-History

$Log: kernel_main.c,v $
Revision 1.4  2007/07/06 05:43:14  rschoene
moved variable declarations

Revision 1.3  2006/06/28 07:52:51  rschoene
uses dTimerOverhead

Revision 1.2  2006/04/10 08:23:06  rschoene
changed header/footer

Revision 1.1  2006/04/05 07:24:49  rschoene
gemv, double precision

(based on Robert Wlochs c kernel skeleton)
*****************************************************************************/
