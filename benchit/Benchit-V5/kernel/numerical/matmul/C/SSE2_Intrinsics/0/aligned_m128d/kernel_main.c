/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: SSE2 Matrix Multiply (C), aligned data
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.6 $
 * $Date: 2006/06/28 07:52:27 $
 *******************************************************************/

// for gcc _mm_malloc
#include <errno.h>
#include <stddef.h> /* ptrdiff_t */
#include <stdint.h> /* uintptr_t */

#include <xmmintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"
/*  Header for local functions
 */
#include "work.h"


// definitions for GNU Comipler which doesnt support e.g. _mm_free

#if (defined (__GNUC__))
    #define _mm_malloc(X,Y) _aligned_malloc(X,Y)
    #define _mm_free(X) _aligned_free(X)
#endif
/**
* functions for gcc who has no _mm_malloc
*/
#define NOT_POWER_OF_TWO(n) (((n) & ((n) - 1)))
#define UI(p) ((uintptr_t) p)
static void *ptr_align(void *p0, size_t alignment, size_t offset)
{
     return (void *) (((UI(p0) + (alignment + sizeof(void*)) + offset)
		       & (~UI(alignment - 1)))
		       - offset);
}


void *_aligned_offset_malloc(size_t size, size_t alignment, size_t offset)
{
    void *p0, *p;
    if (NOT_POWER_OF_TWO(alignment)) {
        errno = EINVAL;
	return((void*) 0);
    }
    if (size == 0)
	return((void*) 0);
    if (alignment < sizeof(void *))
	alignment = sizeof(void *);
/* including the extra sizeof(void*) is overkill on a 32-bit
    machine, since malloc is already 8-byte aligned, as long
    as we enforce alignment >= 8 ...but oh well */
    p0 = malloc(size + (alignment + sizeof(void*)));
    if (!p0)
	return((void*) 0);
    p = ptr_align(p0, alignment, offset);
    *(((void **) p) - 1) = p0;
    return p;
}

void *_aligned_malloc(size_t size, size_t alignment)
{
    return _aligned_offset_malloc(size, alignment, 0);
}

void _aligned_free(void *memblock)
{
    if (memblock)
	free(*(((void **) memblock) - 1));
}


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
      for (j=0;j<n;j++)
      {
         mds->a[i*n+j]=30;
     		 mds->b[i*n+j]=0.01;
         mds->c[i*n+j]=0.0;
      }
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
  infostruct->codesequence = bi_strdup(
  					"for (i=0;i<s;i++)#"
					"{#"
					"  for (j=0;j<s-s%2;j=j+2)#"
					"  {#"
					"    xmm_c=_mm_load_pd(&c[i*s+j]);#"
					"    for (k=0;k<s;k++)#"
					"    {#"
					"      xmm_a=_mm_load1_pd(&a[i*s+k]);#"
					"      xmm_b=_mm_load_pd(&b[k*s+j]);#"
					"      xmm_temp=_mm_mul_pd(xmm_a,xmm_b);#"
					"      xmm_c=_mm_add_pd(xmm_c,xmm_temp);#"
					"    }#"
					"    mm_store_pd(&c[i*s+j],xmm_c);#"
					"  }#"
					"  for (j=s-s%2;j<s;j++)#"
					"    for (k=0;k<s;k++)#"
					"      c[i*s+j]=c[i*s+j]+a[i*s+k]*b[k*s+j];"
					"}#"
					 );
   infostruct->xaxistext = bi_strdup( "Matrix Size" );
   infostruct->kerneldescription = bi_strdup( "Matrix Multiply, SSE2, aligned (C)" );
   infostruct->maxproblemsize = (MAX-MIN+1)/INCREMENT;
   if((MAX-MIN+1) % INCREMENT != 0) infostruct->maxproblemsize++;
   infostruct->kernel_execs_mpi1 = 0;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   /* B ########################################################*/
   n_of_works = 6; /* number versions of this algorithm (norm,blas,sse_,sse2_(algn)= 4 */
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
          bi_strdup( "FLOPS (ijk)" );
        break;
      case 1:
        infostruct->legendtexts[ index1 ] =
          bi_strdup( "FLOPS (ikj)" );
        break;
      case 2:
        infostruct->legendtexts[ index1 ] =
          bi_strdup( "FLOPS (jik)" );
        break;
      case 3:
        infostruct->legendtexts[ index1 ] =
          bi_strdup( "FLOPS (jki)" );
        break;
      case 4:
        infostruct->legendtexts[ index1 ] =
          bi_strdup( "FLOPS (kij)" );
        break;
      case 5:
        infostruct->legendtexts[ index1 ] =
          bi_strdup( "FLOPS (kji)" );
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

  /* calculate real maximum problem size
     problemsizemax might be smaller then BENCHIT_KERNEL_PROBLEMSIZE_MAX
     if BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT is greater then one */

  problemsizemax = MIN + problemsizemax * INCREMENT;

  mdp->a=(double*)_mm_malloc( (problemsizemax*problemsizemax) * sizeof(double),16);
  IDL( 3, printf("Alloc 1 done\n"));
  mdp->b=(double*)_mm_malloc( (problemsizemax*problemsizemax) * sizeof(double),16);
  IDL( 3, printf("Alloc 2 done\n"));
  mdp->c=(double*)_mm_malloc( (problemsizemax*problemsizemax) * sizeof(double),16);
  IDL( 3, printf("Alloc 3 done\n"));
  
  if( (mdp->a==0) || (mdp->b==0) || (mdp->c==0))
    {
      printf("malloc (%ld bytes) failed in bi_init()\n",
	     (long) ((3.0*problemsizemax)*problemsizemax * sizeof(double)));
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
  
  /* calculate real problemsize */
  problemsize = MIN + ( problemsize - 1 ) * INCREMENT;
  
   mydata_t* mdp = (mydata_t*)mdpv;

   /* check wether the pointer to store the results in is valid or not */
   if ( results == NULL ) return 1;

   /* B ########################################################*/
   /* maybe some init stuff in here */
   initData(mdpv,problemsize);
   /*########################################################*/

   for ( j = 0; j < n_of_works; j++ )
   {
    // reset variables
    numberOfRuns=1;
    start=0.0;
    stop=0.0;
      /* B ########################################################*/
      int index1 = 0 * n_of_works + j;
      /* choose version of algorithm */
      switch ( j ) {
         case 5:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                   {
                     multassealignkji_(mdp->a,mdp->b,mdp->c,&problemsize);
                   }
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.001 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
                 break;
         case 4:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                     multassealignkij_(mdp->a,mdp->b,mdp->c,&problemsize);
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.001 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
                 break;
         case 3:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                     multassealignjki_(mdp->a,mdp->b,mdp->c,&problemsize);
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.001 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
                 break;
         case 2:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                     multassealignjik_(mdp->a,mdp->b,mdp->c,&problemsize);
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.001 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
                 break;
         case 1:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                     multassealignikj_(mdp->a,mdp->b,mdp->c,&problemsize);
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.001 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
                 break;
         default:;
         case 0:
                 do
                 {
                   initData(mdpv,problemsize);
                   start=bi_gettime();
                   for (i=0;i<numberOfRuns;i++)
                     multassealignijk_(mdp->a,mdp->b,mdp->c,&problemsize);
                   stop=bi_gettime();
                   stop=stop-start-dTimerOverhead;
                   numberOfRuns=numberOfRuns*8;
                 } while ( stop<0.001 );
                 numberOfRuns=(long)(numberOfRuns/8);
                 stop=stop/((1.0)*(numberOfRuns));
      }
      /* store the results in results[1], results[2], ...
      * [1] for the first function, [2] for the second function
      * and so on ...
      * the index 0 always keeps the value for the x axis
      */
      /* B ########################################################*/
      // the xaxis value needs to be stored only once!
      if ( j == 0 ) results[0] = (double)problemsize;
      results[index1 + 1]= (2.0*problemsize*problemsize*problemsize)/stop;
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
   if (mdp->c) _mm_free(mdp->c);
   if (mdp->b) _mm_free(mdp->b);
   if (mdp->a) _mm_free(mdp->a);
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
  MAX = 600;
  INCREMENT = 1;
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
    fprintf( stderr, "There's at least one PROBLEMSIZE variable not set!\n" );
    exit( 1 );
  }
}


/*****************************************************************************
LOG-History

$Log: kernel_main.c,v $
Revision 1.6  2006/06/28 07:52:27  rschoene
uses dTimerOverhead

Revision 1.5  2005/12/15 09:28:28  hackenb
new variable names
modified/unified header and footer
MIN/MAX/INCREMENT iterating strategy

Revision 1.4  2005/11/23 10:54:24  mickler
- Removed incomplete changes from previous revision

Revision 1.2  2005/11/09 12:00:15  rschoene
commenting ... bah

Revision 1.1  2005/11/09 09:11:21  rschoene
initial SSE2 matmul kernel (double precision, aligned data)
*****************************************************************************/
