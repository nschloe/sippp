/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: blocked Matrix Multiplication (C)
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.2 $
 * $Date: 2007/07/06 05:43:45 $
 *******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "interface.h"
#include "matmul.h"

#define FPT double

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

int MIN, MAX, INCREMENT, MATRIX_SIZE;

/*  Header for local functions */
void evaluate_environment( void );


typedef struct floating_data_struct
{
  FPT *feld1, *feld2, *feld3;
}
fds;

void ( *entry1 ) ( double *a, double *b, double *c, int *size, int *blocksize );
extern void multaijk_( double *a, double *b, double *c, int *size, int *blocksize );
extern void multaikj_( double *a, double *b, double *c, int *size, int *blocksize );
extern void multajik_( double *a, double *b, double *c, int *size, int *blocksize );
extern void multajki_( double *a, double *b, double *c, int *size, int *blocksize );
extern void multakij_( double *a, double *b, double *c, int *size, int *blocksize );
extern void multakji_( double *a, double *b, double *c, int *size, int *blocksize );
extern double getlanguage_( void );

void init_( fds *myfds, int *size );
int getnumberofversions_( void );
void useversion_( int *version );
void entry_( void *ptr, int *size, int *blocksize );
double count_( int *version, int *size );
int coreerror( char *string );
double getseqentryoverhead( void *mem );

void init_( fds *myfds, int *size )
{
  register int x, y;
  long index;

  IDL( 3, printf( "field size: %ld bytes", ( long ) * size * ( *size ) * sizeof( FPT ) ) );
  for ( x = 0; x < *size; x++ )
    for ( y = 0; y < *size; y++ )
    {
      index = x * ( *size ) + y;
      IDL( 5, printf( "%ld\n", index ) );
      /* Feld voller Zahlen zwisch 0 und 100 */
      myfds->feld1[ index ] = 30;
      /* Ein Feld voller Zahlen kleiner 0 */
      myfds->feld2[ index ] = 0.01;
      myfds->feld3[ index ] = 0.0;
    }
  IDL( 3, printf( "init fertig\n" ) );
}

int getnumberofversions_()
{
  return n_of_works;
}

void useversion_( int *version )
{
  switch ( *version )
  {
    case 0:
      entry1 = multaijk_;
      break;
    case 1:
      entry1 = multaikj_;
      break;
    case 2:
      entry1 = multajik_;
      break;
    case 3:
      entry1 = multajki_;
      break;
    case 4:
      entry1 = multakij_;
      break;
    case 5:
      entry1 = multakji_;
      break;
  }
}

void entry_( void *ptr, int *size, int* blocksize )
{
  fds * myfds = ptr;
  double *f1 = myfds->feld1, *f2 = myfds->feld2, *f3 = myfds->feld3;
  if ( *size == 0 )
    return ;
  else
    entry1( f1, f2, f3, size, blocksize );
}

double count_( int *version, int *size )
{
  double ulSize = 1.0 * *size;
  switch ( *version )
  {
    default:
      return 2.0 * ( ulSize ) * ( ulSize ) * ( ulSize );
  }
}

void bi_getinfo( bi_info* infostruct )
{
  int i, j;
	char buff[512];
  memset ( infostruct, 0, sizeof( bi_info ) );
  /* get environment variables for the kernel */
  evaluate_environment();
  infostruct->codesequence = bi_strdup( "for( i=0; i<s; i++)#"
                                        "  for( j=0; j<s; j++)#"
                                        "    for( k=0; k<s; k++)#"
                                        "    {#"
                                        "      c[j*s+i]+=a[k*s+i]*b[j*s+k];#"
                                        "    }" );
  infostruct->xaxistext = bi_strdup( "Blocksize" );
  infostruct->kerneldescription = bi_strdup( "blocked Matrix Multiply (C)" );
	sprintf(buff,"blocked Matrix Multiply (C), Matrix Size=%i", MATRIX_SIZE);
  infostruct->kerneldescription = bi_strdup( buff );
  infostruct->maxproblemsize = (MAX-MIN+1)/INCREMENT;
  if((MAX-MIN+1) % INCREMENT != 0) infostruct->maxproblemsize++;
  infostruct->numfunctions = 6;
  infostruct->kernel_execs_mpi1 = 0;
  infostruct->kernel_execs_mpi2 = 0;
  infostruct->kernel_execs_pvm = 0;
  infostruct->kernel_execs_omp = 0;
  infostruct->kernel_execs_pthreads = 0;
  /* B ########################################################*/
  n_of_works = 6;
  n_of_sure_funcs_per_work = 1;
  /*########################################################*/
  infostruct->numfunctions = n_of_works * n_of_sure_funcs_per_work;

  /* allocating memory for y axis texts and properties */
  infostruct->yaxistexts = malloc( infostruct->numfunctions * sizeof( char* ) );
  if ( infostruct->yaxistexts == 0 )
  {
    fprintf( stderr, "Allocation of yaxistexts failed.\n" );
    fflush( stderr );
    exit( 127 );
  }
  infostruct->outlier_direction_upwards = malloc( infostruct->numfunctions * sizeof( int ) );
  if ( infostruct->outlier_direction_upwards == 0 )
  {
    fprintf( stderr, "Allocation of outlier direction failed.\n" );
    fflush( stderr );
    exit( 127 );
  }
  infostruct->legendtexts = malloc( infostruct->numfunctions * sizeof( char* ) );
  if ( infostruct->legendtexts == 0 )
  {
    fprintf( stderr, "Allocation of legendtexts failed.\n" );
    fflush( stderr );
    exit( 127 );
  }
  infostruct->base_yaxis = malloc( infostruct->numfunctions * sizeof( double ) );
  if ( infostruct->base_yaxis == 0 )
  {
    fprintf( stderr, "Allocation of base yaxis failed.\n" );
    fflush( stderr );
    exit( 127 );
  }
  /* setting up y axis texts and properties */
  for ( j = 0; j < n_of_works; j++ )
  {
    /* B ########################################################*/
    int index1 = 0 * n_of_works + j;
    /* 1st function */
    infostruct->yaxistexts[ index1 ] = bi_strdup( "FLOPS" );
    infostruct->outlier_direction_upwards[ index1 ] = 0;
    infostruct->base_yaxis[ index1 ] = 0;
    /*########################################################*/
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
              i, infostruct->yaxistexts[ i ], i, infostruct->legendtexts[ i ] );
    }
  }

}

/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
  int errors = 0;
  char * p = 0;
  IDL( 3, printf("env \n" ) );
  p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MIN", 0 );
  if ( p == 0 ) errors++;
  else MIN = atoi( p );
  p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_MAX", 0 );
  if ( p == 0 ) errors++;
  else MAX = atoi( p );
  p = bi_getenv( "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT", 0 );
  if ( p == 0 ) errors++;
  else INCREMENT = atoi( p );
  p = bi_getenv( "BENCHIT_KERNEL_MATRIX_SIZE", 0 );
  if ( p == 0 ) errors++;
  else MATRIX_SIZE = atoi( p );
  if ( errors > 0 )
  {
    fprintf( stderr, "There's at least one environment variable not set!\n" );
    fprintf( stderr, "This kernel needs the following environment variables:\n" );
    fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_MIN\n" );
    fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_MAX\n" );
    fprintf( stderr, "BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT\n" );
    fprintf( stderr, "BENCHIT_KERNEL_MATRIX_SIZE\n" );
    fprintf( stderr, "\nThis kernel will iterate from BENCHIT_KERNEL_PROBLEMSIZE_MIN\n\
to BENCHIT_KERNEL_PROBLEMSIZE_MAX, incrementing by\n\
BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT with each step.\n" );
    exit( 1 );
  }
}

double getseqentryoverhead( void *mem )
{
  double start, stop, diff;
  int nu = 0, s;
  IDL( 3, printf("oh \n" ) );

  init_( mem, &nu );
  start = bi_gettime();
  for ( s = 0; s < 1000; s++ )
  {
    entry_( mem, &nu, &nu );
  }
  stop = bi_gettime();
  diff = stop - start - dTimerOverhead;
  if( diff < dTimerGranularity )
    diff = 0.0;
  return diff / 1000;
}

int bi_entry( void* mdpv, int problemsize, double* results )
{
  static double calloverhead = 0;
  int v = 1;
  double time = 0;
  unsigned long count = 0;
  double start, stop;

  /* calculate real problemsize */
  problemsize = MIN + ( problemsize - 1 ) * INCREMENT;

  /* check wether the pointer to store the results in is valid or not */
  if ( results == NULL )
    return 1;

  results[ 0 ] = problemsize;
	//count =1;
	//IDL( -1, printf("entry %i\n",problemsize ) );
  
  count = (((2.0*MATRIX_SIZE)*MATRIX_SIZE)*MATRIX_SIZE);
  for ( v = 0; v < n_of_works; v++ )
  {
    useversion_( &v );
    calloverhead = getseqentryoverhead( mdpv );
    init_( mdpv, &MATRIX_SIZE );
    start = bi_gettime();
    entry_( mdpv,&MATRIX_SIZE, &problemsize );
    stop = bi_gettime();
    time = stop - start;
    //printf("%e\n",time);
    time -= dTimerOverhead;
    time -= calloverhead;
    /* If the measured time is smaller than the resolution of our timer,
     * mark the result as invalid
     */
    if ( time < dTimerGranularity )
      results[ v + 1 ] = INVALID_MEASUREMENT;
    else
      results[ v + 1 ] = ( ( double ) count ) / time;
  }
  return 0;
}

int coreerror( char *string )
{
  printf( "Core Error: %s\n", string );
  return 1;
}

void *bi_init( int problemsizemax )
{

  fds * myfds;
  IDL( 3, printf("init %i\n",problemsizemax ) );

  IDL( 2, printf( "enter bi_init\n" ) );
  myfds = malloc( sizeof( fds ) );
  IDL( 3, printf( "allocating structure myfds\n" ) );
  if ( myfds == NULL )
  {
    printf( "allocation of structure myfds failed\n" );
    exit( 127 );
  }

  /* calculate real maximum problem size
     problemsizemax might be smaller than BENCHIT_KERNEL_PROBLEMSIZE_MAX
     if BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT is greater than one */

  problemsizemax = MATRIX_SIZE;

  myfds->feld1 = malloc( ( problemsizemax ) * ( problemsizemax ) * sizeof( FPT ) );
  IDL( 3, printf( "Alloc 1 done\n" ) );
  myfds->feld2 = malloc( ( problemsizemax ) * ( problemsizemax ) * sizeof( FPT ) );
  IDL( 3, printf( "Alloc 2 done\n" ) );
  myfds->feld3 = malloc( ( problemsizemax ) * ( problemsizemax ) * sizeof( FPT ) );
  IDL( 3, printf( "Alloc 3 done\n" ) );

  if ( ( myfds->feld1 == NULL ) || ( myfds->feld2 == NULL ) ||
       ( myfds->feld3 == NULL ) )
  {
    printf( "malloc (%ld bytes) failed in bi_init()\n",
            ( long ) ( 3.0 * problemsizemax * problemsizemax * sizeof( FPT ) ) );
    bi_cleanup( myfds );
    exit( 127 );
  }

  IDL( 2, printf( "leave bi_init\n" ) );
  return ( myfds );
}

void bi_cleanup( void* mdpv )
{
  fds * data = mdpv;
  IDL( 3, printf( "cleaning..." ) )
  if ( data != NULL )
  {
    IDL( 3, printf( "1" ) )
    if ( data->feld1 != NULL )
    {
      free( data->feld1 );
      data->feld1 = NULL;
    }
    IDL( 3, printf( "2" ) )
    if ( data->feld2 != NULL )
    {
      free( data->feld2 );
      data->feld2 = NULL;
    }
    IDL( 3, printf( "3" ) )
    if ( data->feld3 != NULL )
    {
      free( data->feld3 );
      data->feld3 = NULL;
    }
    IDL( 3, printf( "4\n" ) )
    free( data );
  }
}


/********************************************************************
 * Log-History
 * 
 * $Log: matmul_sub.c,v $
 * Revision 1.2  2007/07/06 05:43:45  rschoene
 * moved some debug prints
 *
 * Revision 1.1  2006/11/20 15:17:10  rschoene
 * a blocked Matrix Multiplication in C with datatype doule
 *
 * 
 *******************************************************************/
