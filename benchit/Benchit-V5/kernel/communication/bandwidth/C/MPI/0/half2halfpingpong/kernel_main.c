/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: pairwise Send/Recv between two MPI-Prozesses>
 *         this file holds all the functions needed by the 
 *         benchit-interface
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.3 $
 * $Date: 2006/01/30 13:54:58 $
 *******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <mpi.h>
#include "pingpong.h"
#include <errno.h>


int steps_done=0;

/* Reads the environment variables used by this kernel. */
void evaluate_environment(mydata_t * pmydata)
{
  unsigned int errors = 0, inumentries=0, ii;
  char * p = 0;
  char * s = 0;
    
  p = bi_getenv( "BENCHIT_KERNEL_REPETITIONS", 0 );
  if ( p == NULL ) errors++;
  else pmydata->repeat = strtol(p, (char **)NULL, 10);
  
  p = bi_getenv( "BENCHIT_KERNEL_SHOW_PAIR_BANDWITH", 0 );
  if ( p == NULL ) errors++;
  else pmydata->pair_bandwith = strtol(p, (char **)NULL, 10);
  
  p = bi_getenv( "BENCHIT_KERNEL_SHOW_TOTAL_BANDWITH", 0 );
  if ( p == NULL ) errors++;
  else pmydata->total_bandwith =strtol(p, (char **)NULL, 10);

  if ( pmydata->pair_bandwith==0 && pmydata->total_bandwith==0 )
  {
    fprintf( stderr, "nothing to be shown\n" );
    fprintf( stderr, "neither BENCHIT_KERNEL_SHOW_PAIR_BANDWITH nor BENCHIT_KERNEL_SHOW_TOTAL_BANDWITH set\n" );
    exit( 1 );
  }
  
  p = bi_getenv( "BENCHIT_KERNEL_MIN_MSG_SIZE", 0 );
  if ( p == NULL ) errors++;
  else pmydata->min_msg_size = strtol(p, (char **)NULL, 10);
  
  p = bi_getenv( "BENCHIT_KERNEL_MAX_MSG_SIZE", 0 );
  if ( p == NULL ) errors++;
  else pmydata->max_msg_size = strtol(p, (char **)NULL, 10);
  
  p = bi_getenv( "BENCHIT_KERNEL_MSG_SIZE_INCREMENT", 0 );
  if ( p == NULL ) errors++;
  else pmydata->msg_size_increment = strtol(p, (char **)NULL, 10);
  
  MPI_Comm_rank(MPI_COMM_WORLD, &(pmydata->commrank));
  MPI_Comm_size(MPI_COMM_WORLD, &(pmydata->commsize));
  
  /* think positive */
  pmydata->empty_list = 'f';

  p = bi_getenv( "BENCHIT_KERNEL_SENDERLIST", 0 );
  s = bi_getenv( "BENCHIT_KERNEL_RECEIVERLIST", 0 );

  if ( (p == NULL) || (s == NULL) )
  {
    errors++;
    fprintf( stderr, "BENCHIT_KERNEL_SENDERLIST or BENCHIT_KERNEL_RECEIVERLIST not set\n" ); fflush( stderr );
    pmydata->empty_list = 't';
  }
  else
  {
    if ( 0 == strlen( p ) || 0 == strlen( s ) ||  strlen( s ) != strlen( p ) )
    {
      fprintf( stderr, "empty BENCHIT_KERNEL_SENDERLIST or BENCHIT_KERNEL_RECEIVERLIST or length-missmatch\n" ); fflush( stderr );
      pmydata->empty_list = 't';
    }
    else
    {

      /* for senderlist */
      inumentries=1; /* first commata means 1 entry already found */
      /* find out how many values are given in the list */
      while (p) 
      {
        p = strstr( p,",");
        if (p) 
        {
          p++;
          inumentries++;
        }
      }
      if ( inumentries != ( pmydata->commsize / 2 ) ) /* wrong list */
      {
        fprintf( stderr, "Listentries dont match MPI_Comm_size - using own sender-receiverlist\n" );
        pmydata->empty_list = 't';
      }      
      /* allocate aray according to number of entries */
      pmydata->senderlist = (unsigned int *)malloc( sizeof(unsigned int) * inumentries);
      p = bi_getenv( "BENCHIT_KERNEL_SENDERLIST", 0 );
      pmydata->senderlist[0] = strtol(p, (char **)NULL, 10); /*entry bevore first commata*/
      for (ii=1;ii<inumentries; ii++) 
      {
        p = strstr( p,",")+1; /* pointer to next number in string */
        pmydata->senderlist[ii] = strtol(p, (char **)NULL, 10);
      }

      /* for receiverlist */
      inumentries=1; /* first commata means 1 entry already found */
      /* find out how many values are given in the list */
      while (s) 
      {
        s = strstr( s,",");
        if (s) 
        {
          s++;
          inumentries++;
        }
      }
      if ( inumentries != ( pmydata->commsize / 2 ) ) /* wrong list */
      {
        fprintf( stderr, "Listentries dont match MPI_Comm_size - using own sender-receiverlist\n" );
        pmydata->empty_list = 't';
      }      
      /* allocate aray according to number of entries */
      pmydata->receiverlist = (unsigned int *)malloc( sizeof(unsigned int) * inumentries);
      s = bi_getenv( "BENCHIT_KERNEL_RECEIVERLIST", 0 );
      pmydata->receiverlist[0] = strtol(s, (char **)NULL, 10); /*entry bevore first commata*/
      for (ii=1;ii<inumentries; ii++) 
      {
        s = strstr( s,",")+1; /* pointer to next number in string */
        pmydata->receiverlist[ii] = strtol(s, (char **)NULL, 10);
      }

    }
  }

/*
    for (ii=0;ii<inumentries; ii++) 
    {
      printf("senderlist[%d] = %d\n", ii,  pmydata->senderlist[ii]);
    }
*/          
  
  if ( errors > 0 )
  {
  fprintf( stderr, "There's at least one environment variable not set!\n" );
  exit( 1 );
  }

   pmydata->steps = (unsigned int) ( pmydata->max_msg_size - pmydata->min_msg_size + 1 ) / pmydata->msg_size_increment;
   if (( pmydata->max_msg_size - pmydata->min_msg_size + 1 ) % pmydata->msg_size_increment  != 0) pmydata->steps++;

  fprintf( stderr, "\nsteps=%d\n",pmydata->steps );
}




/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * pinfo )
{
   mydata_t * penv;
   unsigned int ii=0;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   penv = (mydata_t *) malloc( sizeof( mydata_t ) );

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup( "each process sender xor receiver;  do MPI_send-MPI_recv simultaneous" );
   pinfo->kerneldescription = bi_strdup( "kernel performs a MPI_send-MPI_recv with all MPI processes simultaneously" );
   pinfo->xaxistext = bi_strdup( "Messagesize in bytes" );
   pinfo->maxproblemsize = penv->steps-1;
   pinfo->num_processes = 1;
   pinfo->num_threads_per_process = 0;
   pinfo->kernel_execs_mpi1 = 1;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 0;

   if (penv->pair_bandwith==1) ii++;
   if (penv->total_bandwith==1) ii++;
   pinfo->numfunctions = ii;

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

   if (penv->pair_bandwith==1) ii++;
   if (penv->total_bandwith==1) ii++;


   /* setting up y axis texts and properties */
   if ( penv->pair_bandwith==1 && penv->total_bandwith==0 )
   {
     pinfo->yaxistexts[0] = bi_strdup( "bandwith in bytes/s" );
     pinfo->outlier_direction_upwards[0] = 0;
     pinfo->base_yaxis[0] = 0;
     pinfo->legendtexts[0] = bi_strdup( "bandwith of one communication-pair in bytes/s" );
   }
 
   if ( penv->pair_bandwith==0 && penv->total_bandwith==1 )
   {
     pinfo->yaxistexts[0] = bi_strdup( "bandwith in bytes/s" );
     pinfo->outlier_direction_upwards[0] = 0;
     pinfo->base_yaxis[0] = 0;
     pinfo->legendtexts[0] = bi_strdup( "total bandwith of all communication-pairs in bytes/s" );
   }

   if ( penv->pair_bandwith==1 && penv->total_bandwith==1 )
   {
     pinfo->yaxistexts[0] = bi_strdup( "bandwith in bytes/s" );
     pinfo->outlier_direction_upwards[0] = 0;
     pinfo->base_yaxis[0] = 0;
     pinfo->legendtexts[0] = bi_strdup( "bandwith of one communication-pair in bytes/s" );
     pinfo->yaxistexts[1] = bi_strdup( "bandwith in bytes/s" );
     pinfo->outlier_direction_upwards[1] = 0;
     pinfo->base_yaxis[1] = 0;
     pinfo->legendtexts[1] = bi_strdup( "total bandwith of all communication-pairs in bytes/s" );
   }

   /* free all used space */
   if (penv) free( penv );
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
   mydata_t * pmydata;
   unsigned int ii=0;

   pmydata = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( pmydata == 0 )
   {
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); fflush( stderr );
      exit( 127 );
   }
   evaluate_environment(pmydata);

   /* allocate and fill msg-msg_string for mpi-communication */
/*
 *    pmydata->msg_string = (unsigned int *) malloc( sizeof(unsigned int) * pmydata->max_msg_size );
 */
   pmydata->msg_string = (unsigned int *) malloc(pmydata->max_msg_size );
   if ( pmydata->msg_string == 0 )
   {
      fprintf( stderr, "Allocation of structure pmydata->msg_string failed\n" ); fflush( stderr );
      exit( 127 );
   }
   for (ii=0; ii < pmydata->max_msg_size/sizeof(unsigned int); ii++)
   {
/*
 *       pmydata->msg_string[ii] = 1;
 */
    pmydata->msg_string[ii] = (unsigned int)random();
   }
   
   IDL(3, printf("\nrank=%d msg_stringcontent:%d\n",pmydata->commrank, pmydata->commsize));
   for (ii=0; ii < pmydata->max_msg_size; ii++)
   {
   IDL(3, printf("%d ",pmydata->msg_string[ii]));
   }
   IDL(3, printf("\n"));
  
   IDL(3, printf("\nrank=%d size=%d\n",pmydata->commrank, pmydata->commsize));
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
  /* dstart, dend: the start and end time of the measurement */
  /* dtime: the time for a single measurement in seconds */
  double dstart = 0.0, dend = 0.0, dtime = 0.0;
  /* flops stores the calculated FLOPS */
  double dres = 0.0;
  /* ii is used for loop iterations */
  unsigned int isender = 0, ireceiver = 0, imyproblemsize = (unsigned int) iproblemsize;
  unsigned long int imsgsize = 0;
  /* cast void* pointer */
  mydata_t * pmydata = (mydata_t *) mdpv;

  IDL(3, printf("\nrank=%d entered bi_entry\n",pmydata->commrank));


steps_done++;
  
  /* there was no sender/receiverlist supplied */
  if ( pmydata->empty_list == 't' )
  {
 /*
    if ( (pmydata->commrank == 0 ) && (iproblemsize == 1))
    {
      fprintf( stderr, "\nLists empty - setting sender / receiver automatically\n");fflush( stderr );
    }
*/
    /* lower half is sender, upper half is receiver */
    if (pmydata->commrank < (pmydata->commsize / 2))
    {
      isender=pmydata->commrank;
      ireceiver=pmydata->commrank + (pmydata->commsize / 2);
    }
    else
    {
      isender=pmydata->commrank - (pmydata->commsize / 2);
      ireceiver=pmydata->commrank;
    }
    if ( isender > (pmydata->commsize - 1) || ireceiver > (pmydata->commsize - 1) )
    {
      fprintf( stderr, "\nrank=%d sender=%d receiver=%d\n", pmydata->commrank, isender, ireceiver);fflush( stderr );
      isender = 0;
      ireceiver = 1;
    }
  }
  else /* we have those two lists */
  {
    if (pmydata->commrank < (pmydata->commsize / 2))
    {
      isender=pmydata->senderlist[pmydata->commrank];
      ireceiver=pmydata->receiverlist[pmydata->commrank];
    }
    else
    {
      isender=pmydata->senderlist[pmydata->commrank - (pmydata->commsize / 2)];
      ireceiver=pmydata->receiverlist[pmydata->commrank - (pmydata->commsize / 2)];
    }
  }
/*
    fprintf( stderr, "\nisender=%d, ireceiver=%d\n",isender, ireceiver);fflush( stderr );
*/

  imsgsize = pmydata->min_msg_size + (imyproblemsize * pmydata->msg_size_increment);

  /* check wether the pointer to store the results in is valid or not */
  if ( pmydata->commrank == 0 )
  {
    if ( dresults == NULL )
    {
      fprintf( stderr, "\nrank=%d resultpointer not allocated - panic\n",pmydata->commrank);fflush( stderr );
      return 1;
    }
  }

  /* get the actual time
   * do the measurement / your algorythm
   * get the actual time
   */
  MPI_Barrier( MPI_COMM_WORLD );
  dstart = bi_gettime(); 
  pingpong(&isender, &ireceiver, pmydata, &imsgsize);
  MPI_Barrier( MPI_COMM_WORLD );
  dend = bi_gettime();

  IDL(3, printf("rank=%d Problemsize=%d, Value=%f\n",pmydata->commrank, imyproblemsize, dres));

  if ( pmydata->commrank == 0 )
  {
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
    dresults[0] = (double)imsgsize;

    /* setting up y axis texts and properties */
    if ( pmydata->pair_bandwith==1 && pmydata->total_bandwith==0 )
    {
      dresults[1] = (double) (imsgsize * pmydata->repeat * 2 / dtime);
    }   
    if ( pmydata->pair_bandwith==0 && pmydata->total_bandwith==1 )
    {
      dresults[1] = (double) (imsgsize * pmydata->repeat * pmydata->commsize / dtime);
    }
  
    if ( pmydata->pair_bandwith==1 && pmydata->total_bandwith==1 )
    {
      dresults[1] = (double) (imsgsize * pmydata->repeat * 2 / dtime);
      dresults[2] = (double) (imsgsize * pmydata->repeat * pmydata->commsize / dtime);
    }
  }

  return 0;
}

/** Clean up the memory 
 */
void bi_cleanup( void* mdpv )
{
   mydata_t * pmydata = (mydata_t*)mdpv;

  fprintf( stderr, "\nsteps done = %d\n",steps_done );

   if ( pmydata ) free( pmydata );
   return;
}

/********************************************************************
 * Log-History
 * 
 * $Log: kernel_main.c,v $
 * Revision 1.3  2006/01/30 13:54:58  william
 * this is a big commit because the cvs-server was down for 2 weeks
 *
 * Revision 1.2  2006/01/13 10:24:58  william
 * first test successfully completed - minor fixes
 *
 * Revision 1.1  2006/01/12 11:30:58  william
 * checked in new kernel-tree "communication"
 *
 * Revision 1.7  2006/01/09 16:24:21  william
 * updated the cvs-header
 *
 * Revision 1.6  2006/01/09 15:54:09  william
 * cvs-keyword-problems
 *
 * Revision 1.5  2006/01/09 11:31:00  william
 * filled in infos for the cvs-header and deleted falsly inserted files
 *
 * Revision 1.4  2006/01/08 00:29:16  william
 * forgot to divide the time needed for the ping pong by the number of repetitions
 *
 * Revision 1.3  2006/01/07 23:09:33  william
 * seems that my strings included some prohibited chars??
 *
 * Revision 1.2  2006/01/07 22:56:09  william
 * removed unused variable
 *
 * Revision 1.1  2006/01/07 22:31:17  william
 * pairwise pingpong all-to-all with minimal message-size => latency-benchmark
 *
 *
 *******************************************************************/ 
