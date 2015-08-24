/*********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Description: The implementation of the BenchScript Interface
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: molka $
 * $Revision: 1.2 $
 * $Date: 2007/01/12 10:44:35 $
 *********************************************************************/

#include "benchscript.h"
#include "interface.h"

#include <stddef.h>
#include <sys/wait.h>
#include <pthread.h>
#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

double logx( double base, double value )
{
  return log( value ) / log( base );
}

char* int2char( int number )
{
  char buffer[100], *ret_val;
  size_t size;

  size = sprintf( buffer, "%d", number ) * sizeof(char);

  ret_val = (char*) malloc ( size +1 );

  strcpy( ret_val, buffer );
  
  return ret_val;
}

char* bi_strcat( const char *str1, const char *str2 )
{
  const char *pi1, *pi2;
  char *so, *po;
  size_t size;

  pi1 = str1;
  while (*pi1 != '\0')
  {
    pi1++;
  }

  pi2 = str2;
  while (*pi2 != '\0')
  {
    pi2++;
  }

  size = ( ( ( pi1 + 1 - str1 ) + ( pi2 + 1 - str2 ) ) * sizeof(char) );

  so = (char*) malloc ( size );

  pi1 = str1;
  pi2 = str2;
  
  po = so;
  while (*pi1 != '\0')
  {
    *po++ = *pi1++;
  }
  while (*pi2 != '\0')
  {
    *po++ = *pi2++;
  }

  *po = *pi1;

  return (so);
}

void bi_script( char* script, int num_processes, int num_threads )
{
  if( num_processes <= 1 )
  {
    if( num_threads <= 1 )
    {
/*      printf( "************** single run **********************\n" ); */
      bi_script_run( (void*) script );
/*      printf( "************** single don **********************\n" ); */
    }
    else
    {
/*      printf( "************** threaded run ********************\n" ); */
      bi_script_create_threads( script, num_threads );
/*      printf( "************** threaded don ********************\n" ); */
    }
  }
  else
  {
/*      printf( "************** mproc run ***********************\n" ); */
    bi_script_create_processes( script, num_processes, num_threads );
/*      printf( "************** mproc don ***********************\n" ); */
  }
}

void bi_script_create_processes( char* script, int num_processes, int num_threads )
{
  int i,j;
  int first_pid, last_pid;
  int * child_pids;
  
  /* recognize the parent process pid */
  child_pids = (int*) malloc( num_processes* sizeof(int) );
  
  first_pid = getpid();

  num_processes = 1;
  
  for( i=0; i < num_processes; i++ )
  {
    if( getpid() == first_pid )
    {
      last_pid = fork();
      if( last_pid == 0 )
      {
        /* check if single or multi threaded */
        if( num_threads <= 1 )
        {
/*          printf( "process pid: %d\n", getpid() ); */
          bi_script_run( (void*) script );
        }
        else
        {
/*          printf( "process pid: %d\n", getpid() ); */
          bi_script_create_threads( script, num_threads );
        }
        
        exit(0); 
      }
      else if( last_pid < 0 )
      {
        /* the process could not be forked */
        exit(1);
      }
      else
      {
        /* collect the child pids */
        child_pids[i] = last_pid;
      }
    }
  }
 
  int status;
  
  /* wait until all childs have exited */
  for( j=0; j < num_processes; j++ )
  {
    waitpid( child_pids[j], &status, 0 );
  }
}

void bi_script_create_threads( char* script, int num_threads )
{
  int i,j;
  pthread_t thread_ids[ num_threads ];

/*    printf( "process pid in threadcaller: %d\n", getpid() ); */
  /* start the threads */
  for( i=0; i < num_threads; i++ )
  { 
    pthread_create( &thread_ids[i], NULL, &bi_script_run, script );
/*    printf( "created thread %d\n", thread_ids[i]); */
  }

  /* collect the threads */
  for( j=0; j < num_threads; j++ )
  {
/*    printf( "waiting for thread %d\n", thread_ids[j]); */
    pthread_join( thread_ids[j], NULL);
  }
}

void* bi_script_run( void* script )
{
  return (void*) system( (char*) script );
}

/********************************************************************
 * Log-History
 * 
 * $Log: benchscript.c,v $
 * Revision 1.2  2007/01/12 10:44:35  molka
 * replaced //-comments and tabs
 *
 * Revision 1.1  2006/05/23 08:13:00  mueller
 * - changed location of benchscript files to tools/
 *
 * Revision 1.1  2006/05/22 10:05:45  mueller
 * - adding benchscript
 *
 * 
 ********************************************************************/
