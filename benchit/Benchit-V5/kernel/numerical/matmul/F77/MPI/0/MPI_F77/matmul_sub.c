#include "interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include "matmul.h"
#include "mpi_functions.h"
#include "matrix_functions.h"

/** I have defined a debug level to get some output in the case of errors.
 * to use this try do compile with -DDEBUGLEVEL=[2..6]
 */
#if(!defined(DEBUGLEVEL))
#define DEBUGLEVEL (0)
#endif

#define FPT double
#define MINTIME 1.0e-22


int MAXPROBLEMSIZE;


/* Reads the environment variables used by this kernel. */
void evaluate_environment()
{
   int errors = 0;
   char * p = 0;
   MAXPROBLEMSIZE = 0;
     
   p = bi_getenv( "BENCHIT_MATMUL_F77_DOUBLE_MPI_MPIF_MAXPROBLEMSIZE", 0 );
   if ( p == 0 ) errors++;
   else MAXPROBLEMSIZE = atoi( p );
   
   if ( errors > 0 )
   {
      fprintf( stderr, "There's at least one environment variable not set!\n" );
      fprintf( stderr, "This kernel needs the following environment variables:\n" );
      fprintf( stderr, "BENCHIT_MATMUL_F77_DOUBLE_MPI_VAR1_MAXPROBLEMSIZE\n" );
      exit( 1 );
   }
}




/** The matrix multiply uses three matrices, two as input and one for the
 * result. I use an individual memory area with every matrix. Because bi_init()
 * can only return one pointer the three pointers are put into a structure.
 * bi_init() returns a pointer to such a structure.
 */
typedef struct floating_data_struct
{
  FPT *a, *b, *c, *temp_b, *temp_c;
  myinttype sizeall, sizeone;
}
fds;

int rank, size;

void entry_(void *ptr, myinttype * size);
double gettimeroverhead (void);
double getseqentryoverhead (void *mem);
int result_check(double *mat, int lines, int cols);

/* how many childs we have in a binary tree ? 
 * implemented in c because fortran 77 is not able
 * to handle recursive calls
 */
myinttype childs_ ( myinttype *id, myinttype *size)
{
  myinttype child0=2*(*id)+1, child1=2*(*id)+2, we_are=1;
  if (child0<*size) 
    we_are+=childs_( &child0, size);
  if (child1<*size)
    we_are+=childs_( &child1, size);
  return(we_are);
}

/*********************************************
 * FIRST PART OF THIS FILE: MY OWN FUNCTIONS *
 *********************************************/

void
entry_ (void *ptr, myinttype * size)
{
  /* cast the pointer */
  fds *myfds = ptr;
  /* unpack the pointers */
  double *a = myfds->a,
    *b = myfds->b, 
    *c = myfds->c,
    *temp_b = myfds->temp_b, 
    *temp_c = myfds->temp_c;
  myinttype sizeall = myfds->sizeall, 
    sizeone = myfds->sizeone;
  if (*size == 0)
    return;
  else
    {
      mpimatrixbroadcast_( a, &sizeall, &sizeall);
      mpimatrixscatter_( b, temp_b, &sizeall, &sizeone);
      matmul_( a, temp_b, temp_c, &sizeall, &sizeall, 
	       &sizeall, &sizeone);
      mpimatrixgather_( c, temp_c, &sizeall, &sizeone);
    }
}

/** Calculate the overhead for bi_gettime(). This is necessary
 * because this time has to be subtracted from the execution
 * time of the kernel.
 */
double
gettimeroverhead ()
{
  double start, stop;
  int s;
    
  /* call bi_gettime 1000 times and the divide the passed
   * time by 1000 */
  start = bi_gettime ();
  for (s = 0; s < 1000; s++)
    {
      (void) bi_gettime ();
    }
  stop = bi_gettime ();
  return (start - stop) / 1000;
}

/** Calculate the overhead for a call to the real kernel. This is
 * necessary because this time has also to be subtracted from
 * the execution time of the kernel.
 */
double
getseqentryoverhead (void *mem)
{
  double start, stop;
  myinttype nu = 0, s;
    
  start = bi_gettime ();
  for (s = 0; s < 1000; s++)
    {
      entry_ (mem, &nu);
    }
  stop = bi_gettime ();
  return (stop - start) / 1000;
}

int
result_check(double *mat, int lines, int cols)
{
  int line, col, ziel=lines;

  for (line = 0; line < lines; line++)
    for (col = 0; col < cols; col++)
      if(mat[cols * line + col] != lines)
	{
	  printf("(%d) internal kernel error\n", rank);
	  printf("     matrix multiply produces incorrect results");
	  printf("     error on line %d column %d, expected %d, got %f\n",
		 line, col, ziel,mat[cols * line + col] );
	  /*	  print_mat(mat,lines,cols); */
	  return 1;
	}
  fflush (stdout);  
  return 0;
}

/***************************************************
 * SECOND PART: IMPLEMENTING THE BenchIt INTERFACE *
 ***************************************************/

/** The implementation of the interface function bi_entry
 */
int
bi_entry (void *mcb, int problemsize, double *results)
{
  /* static variables, the values are calculated just once */
  static double timeroverhead = 0.0, calloverhead = 0.0;
  double time = 0;
  double count = 0.0, nul=0.0;
  double start, stop;
  myinttype prob = (myinttype) problemsize;
  fds *myfds=(fds*) mcb;
    
  /* calculate overhead if it is not done yet */
  if (timeroverhead == 0.0)
    {
      timeroverhead = gettimeroverhead ();
      /* maybe we have a VERY fast timer ;-) */
      if (timeroverhead == 0.0)
	timeroverhead = MINTIME;
    }
  if (calloverhead == 0.0)
    {
      calloverhead = getseqentryoverhead (mcb);
      /* maybe we have a VERY fast timer ;-) */
      if (calloverhead == 0.0)
	calloverhead = MINTIME;
    }
  /* init fields in structure for entry_() */
  myfds->sizeall=problemsize*size;
  myfds->sizeone=problemsize;
  /* calculate the number of operations we do with the given matrix size */
  count = 2.0 * (double) myfds->sizeall * 
                (double) myfds->sizeall * 
                (double) myfds->sizeall;
  /* fill temp matrices with zero's */
  matrixfill_(myfds->temp_b, &myfds->sizeall, &myfds->sizeone, &nul);
  matrixfill_(myfds->temp_c, &myfds->sizeall, &myfds->sizeone, &nul);
  /* clear destination matrix */
  if(rank==0)
    {
      matrixfill_( myfds->c, &myfds->sizeall, &myfds->sizeall, &nul);
    }
  /* get time */
  start = bi_gettime ();
  /* call kernel */
  entry_ (mcb, &prob);
  /* get time */
  stop = bi_gettime ();
  /* check result */
  if(rank==0)
    if(result_check(myfds->c,myfds->sizeall,myfds->sizeall)!=0)
      {
	printf("(%d) parallel matrix multiply produced incorrect result\n", rank);
	return 1;
      }
  /* real time = timer - overhead */
  time = stop - start;
  time -= timeroverhead;
  time -= calloverhead;
  /* check for divide by zero, if time is zero
   * time=0 means that our operation does not need any time !
   */
  if (time < MINTIME)
    time = MINTIME;
  /* put the FLOPS rate into result structure */
  /* The first field in the return vector ist reserved for the
   * prblem size. Be sure to test for a NULL pointer */
  if (results != NULL)
    {
      results[0] = size*problemsize;
      results[1] = (double) count / (double) time ;
    }
  return 0;
}

/** Init the kernel for the maximum problem size
 * All the memory we use within all measurements
 * is allocated here
 */
void *
bi_init (int problemsizemax)
{
  /* the structure to hold my pointers */
  fds *myfds;
  myinttype sizemax;
  double one=1.0, zero=0.0;
        
  MPI_Comm_size( MPI_COMM_WORLD, &size);
  MPI_Comm_rank( MPI_COMM_WORLD, &rank);
  /* check if the argument is valid */
  mpivariables_();
  sizemax=MAXPROBLEMSIZE*size;
  if (problemsizemax > MAXPROBLEMSIZE)
    {
      printf ("(%d) Illegal maximum problem size\n", rank);
      exit (127);
    }  
  IDL (3, printf ("(%d) Enter init\n", rank));
  /* allocate a new structure for my pointers */
  myfds = malloc (sizeof (fds));
  if( myfds==NULL )
    {
      printf("(%d) could not allocate myfds\n", rank);
      exit(127);
    }
  myfds->a=NULL;
  myfds->b=NULL;
  myfds->c=NULL;
  myfds->temp_b=NULL;
  myfds->temp_c=NULL;
  /* allocate the memory for all matrices */
  myfds->a=(double*)malloc (sizemax*sizemax*sizeof (FPT));
  IDL (3, printf ("(%d) Alloc a done\n", rank));
  myfds->temp_b=(double*)malloc(sizemax*MAXPROBLEMSIZE*sizeof (FPT));
  IDL (3, printf ("(%d) Alloc temp_b done\n", rank));
  myfds->temp_c=(double*)malloc(sizemax*MAXPROBLEMSIZE*sizeof(FPT));
  IDL (3, printf ("(%d) Alloc temp_c done\n", rank)); 
  if ((myfds->a == NULL) 
      || (myfds->temp_b == NULL)
      || (myfds->temp_c == NULL))
    {
      printf ("(%d) one malloc failed in bi_init()\n", rank);
      printf ("a=%p; temp_b=%p; temp_c=%p\n", myfds->a, myfds->b,  myfds->c);
      printf ("(%d) could not get %f MB of Memory\n", rank, 
                 (double)(3.0*sizemax*sizemax*sizeof (FPT))/
                 (double)(1024*1024));
      bi_cleanup (myfds);
      exit (127);
    }
  if(rank==0) 
    {
      myfds->b=(double*)malloc (sizemax*sizemax*sizeof (FPT));
      IDL (3, printf ("Alloc b done\n"));
      myfds->c=(double*)malloc (sizemax*sizemax*sizeof (FPT));
      IDL (3, printf ("Alloc c done\n"));
      if ((myfds->b == NULL) || (myfds->c == NULL))
	{
	  printf ("(%d) one malloc failed in bi_init()\n", rank);
	  bi_cleanup (myfds);
	  exit (127);
	}
    }
  else
    {
      myfds->b=NULL;
      myfds->c=NULL;
    }
  /* init the memory: matrices a nd b are filled with 1.0
   */
  IDL (3, printf ("Fill matrices a and b\n"));
  matrixfill_( myfds->a, &sizemax, &sizemax, &one);
  if(rank==0)
    {
      matrixfill_( myfds->b, &sizemax, &sizemax, &one);
      matrixfill_( myfds->c, &sizemax, &sizemax, &zero);
    }
  return myfds;
}

/** The interface function to clean up the things
 * we got from the operating system
 */
void
bi_cleanup (void *mcb)
{
  /* cast the void pointer */
  fds *data = mcb;
    
  /* free all the memory, always test for NULL pointer */
  IDL (3, printf ("cleaning...")); 
  if (data != NULL)
    {
      IDL (3, printf ("a"));
      if (data->a != NULL)
	{
	  free (data->a);
	  data->a = NULL;
	}
      IDL (3, printf ("b"));
      if (data->b != NULL)
	{
	  free (data->b);
	  data->b = NULL;
	}
      IDL (3, printf ("c"));
      if (data->c != NULL)
	{
	  free (data->c);
	  data->c = NULL;
	}
      IDL (3, printf ("temp_b"));
      if (data->temp_b != NULL)
	{
	  free (data->temp_b);
	  data->temp_b = NULL;
	}
      IDL (3, printf ("temp_c"));
      if (data->temp_c != NULL)
	{
	  free (data->temp_c);
	  data->temp_c = NULL;
	}
      IDL (3, printf ("fds\n"));
      free (data);
    }
}



void bi_getinfo( bi_info * infostruct )
   {
   int  ii;
   char buffer[200];

   (void) memset ( infostruct, 0, sizeof( bi_info ) );

   /* get environment variables for the kernel */
   evaluate_environment();
   infostruct->codesequence = bi_strdup ( "broadcast A#scatter B#multiply parts#gather C" );
   infostruct->maxproblemsize = MAXPROBLEMSIZE;
   infostruct->xaxistext = bi_strdup( "matrix size" );
   infostruct->kernel_execs_mpi1 = 1;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   infostruct->numfunctions = 1;

   /* allocating memory for y axis texts and properties */
   infostruct->yaxistexts = malloc( infostruct->numfunctions * sizeof ( char * ) );
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
   infostruct->log_yaxis = malloc( infostruct->numfunctions * sizeof( int ) );
   if ( infostruct->log_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of log yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }
   infostruct->base_yaxis = malloc( infostruct->numfunctions * sizeof( double ) );
   if ( infostruct->base_yaxis == 0 )
   {
     fprintf( stderr, "Allocation of base yaxis failed.\n" ); fflush( stderr );
     exit( 127 );
   }

   /* setting up y axis texts and properties */
   for ( ii = 0; ii < infostruct->numfunctions; ii++ )
   {
      infostruct->yaxistexts[ii] = bi_strdup( "FLOPS" );
      infostruct->outlier_direction_upwards[ii] = 1;
      infostruct->log_yaxis[ii] = 0;
      infostruct->base_yaxis[ii] = 0;
/*      sprintf(buffer, "unrolled %d", ii+1 );
*/    sprintf(buffer, " ");
      infostruct->legendtexts[ii] = bi_strdup( buffer );
   }

   if ( DEBUGLEVEL > 3 )
   {
      for ( ii = 0; ii < infostruct->numfunctions; ii++ )
      {
         printf( "yaxis[%2d]=%s\t\t\tlegend[%2d]=%s\n",
            ii, infostruct->yaxistexts[ii], ii, infostruct->legendtexts[ii] );
      }
   }
}

/*****************************************************************************

LOG-History

$Log: matmul_sub.c,v $
Revision 1.1  2005/12/06 15:48:22  william
created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names


*****************************************************************************/
