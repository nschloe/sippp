/***********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply (F77)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.9 $
 * $Date: 2006/05/09 09:33:29 $
***********************************************************************/

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

int MIN, MAX, INCREMENT;

/*  Header for local functions */
void evaluate_environment( void );

typedef struct floating_data_struct
{
  FPT *feld1, *feld2, *feld3;
}
fds;

void ( *entry1 ) ( double *a, double *b, double *c, int *size );
extern double getlanguage_( void );

void init_( fds *myfds, int *size );
int getnumberofversions_( void );
void useversion_( int *version );
void entry_( void *ptr, int *size );
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

void entry_( void *ptr, int *size )
{
  fds * myfds = ptr;
  double *f1 = myfds->feld1, *f2 = myfds->feld2, *f3 = myfds->feld3;
  if ( *size == 0 )
    return ;
  else
    entry1( f1, f2, f3, size );
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

  memset ( infostruct, 0, sizeof( bi_info ) );
  /* get environment variables for the kernel */
  evaluate_environment();
  infostruct->codesequence = bi_strdup( "for( i=0; i<s; i++)#"
                                        "  for( j=0; j<s; j++)#"
                                        "    for( k=0; k<s; k++)#"
                                        "    {#"
                                        "      c[j*s+i]+=a[k*s+i]*b[j*s+k];#"
                                        "    }" );
  infostruct->xaxistext = bi_strdup( "Matrix Size" );
  infostruct->kerneldescription = bi_strdup( "Matrix Multiply (F77)" );
  infostruct->maxproblemsize = (MAX-MIN+1)/INCREMENT;
  if((MAX-MIN+1) % INCREMENT != 0) infostruct->maxproblemsize++;
  infostruct->num_processes = 1;
  infostruct->num_threads_per_process = 0;
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

double getseqentryoverhead( void *mem )
{
  double start, stop, diff;
  int nu = 0, s;

  init_( mem, &nu );
  start = bi_gettime();
  for ( s = 0; s < 1000; s++ )
  {
    entry_( mem, &nu );
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

  count = count_( &v, &problemsize );
  for ( v = 0; v < n_of_works; v++ )
  {
    useversion_( &v );
    calloverhead = getseqentryoverhead( mdpv );
    init_( mdpv, &problemsize );
    start = bi_gettime();
    entry_( mdpv, &problemsize );
    stop = bi_gettime();
    time = stop - start;
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

  problemsizemax = MIN + problemsizemax * INCREMENT;

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

/*****************************************************************************
 
LOG-History
 
$Log: matmul_sub.c,v $
Revision 1.9  2006/05/09 09:33:29  rschoene
removed bug: count_ delivered an unsigned long, which isn't long enough,
changed to double

Revision 1.8  2006/01/10 17:56:26  william
changed variables to new names

Revision 1.7  2006/01/03 15:30:16  mickler
# Changed for MIN, MAX, INCREMENT

Revision 1.6  2005/11/29 20:08:23  mickler
# Removed unused variable

Revision 1.5  2005/11/24 03:36:29  mickler
- Fixed // comment
- Fixed missing type for variable calloverhead

Revision 1.4  2005/11/22 01:26:53  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring

Revision 1.3  2005/11/04 16:24:22  mickler
# Changed kernels to set base_[xy]axis=0 if not using logarithmic scale
- Fixed some kernels with log_[xy]axis=0 and base_[xy]axis=10 to base=0

Revision 1.2  2005/08/24 11:19:27  rschoene
set outlier-direction to 0 !!!!!

Revision 1.1  2005/08/20 15:48:22  rschoene
Matrix Multiply for F77 checkin
(merged from src3-matmulc and src2-matmul-f77)

Revision 1.4  2005/08/18 11:57:50  rschoene
added * in Line 152 : unsigned long ulSize = (NEW)*(/NEW)size;

Revision 1.3  2005/08/10 14:36:47  rschoene
new calculation for number of fl-p-operations (removed int-overflow-bug)

Revision 1.2  2005/07/29 11:53:51  wloch
removed setting of kernelname in bi getinfo and equalized parameter variables

Revision 1.1  2005/07/20 21:38:26  mickler
# Added /numerical/matmul/C/0/0/double/ kernel

Revision 1.3  2005/03/10 22:36:35  william
another set of changes to all names of all kernels
 
Revision 1.2  2005/03/08 16:40:46  william
changed kernelname according to new namingconvention
 
Revision 1.1.1.1  2004/12/14 21:22:57  william
Release 3.0 - created new cvs-tree src2
 
Revision 2.1  2004/05/26 10:35:43  juckel
fixed 0 0 bug in resultfile\nadapted matmul_f and matmul_c to new interface
 
Revision 2.0  2003/12/09 11:18:56  juckel
build of version 2.0
 
Revision 1.9  2003/10/16 08:32:05  kluge
added support for benchit shell, measurment range now editable via COMPILE script
 
Revision 1.8  2003/10/13 11:13:00  kluge
removed bug; wrong calculation of real problemsize, forgot the -1
 
Revision 1.7  2003/10/13 08:10:59  kluge
some changes for easier measuring
 
Revision 1.6  2003/07/18 11:41:04  kluge
neues Verzeichnis membits_c
 
Revision 1.5  2003/05/09 06:18:41  kluge
no message
 
Revision 1.4  2003/05/06 11:18:53  kluge
update f�r interface.h und f�r sch�nheit
 
Revision 1.3  2003/01/28 11:41:13  kluge
new header
 
*****************************************************************************/
