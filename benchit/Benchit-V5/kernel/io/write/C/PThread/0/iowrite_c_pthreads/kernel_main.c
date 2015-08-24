/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: simple Variant of the c-Skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.1 $
 * $Date: 2006/12/14 12:54:15 $
 *******************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"

#include "iowrite.h"
#include "tpool.h"


#define QUEUE_LENGTH_MULTIPLY 1
#define BUFFERSIZE 1048576
#define FILES_PER_DIR 10


#define DEBUG_NOTHING -1
#define DEBUG_ERROR 0
#define DEBUG_WARNING 1
#define DEBUG_INFO 2

#ifndef DEBUG
#  define DEBUG 0
#endif


/****f* functions/helpers::dprintf
 * SYNOPSIS
 * int dprintf( const int level, const char * comment, ... )
 * DESCRIPTION
 * this function is used for debugging purpose and uses the
 * DEBUGLEVEL-environmentvariable of BenchIT.
 * There are 3 different levels:
 * DEBUG_NOTHING -1
 * DEBUG_ERROR 0
 * DEBUG_WARNING 1
 * DEBUG_INFO 2
 ***/
int dprintf( const int level, const char * comment, ... )
{
	  int result = 0;
	  
	#if defined( DEBUG )
	  va_list args;
	  va_start( args, comment);
	  
	  if ( DEBUG >= level)
	  	{
		 result = vfprintf( stderr, comment, args );
		}
	  
	#else
	  /*
	   * this is done to avoid compiler-warnings about unused parameters
	   */
	  void * dummy = 0;
	  dummy = (void *) ( &level );
	  dummy = (void *) ( comment );
	  
	#endif
	  fflush(stderr);
	  fflush(stdout);
	  return result;
}





/* Reads the environment variables used by this kernel. */
void evaluate_environment(environment_variables_t * pmydata)
{
	pmydata->FILESIZE = (double)atof( (const char *)getenv("BENCHIT_KERNEL_FILESIZE") );
	pmydata->DISKPATH = (char *) malloc( sizeof( char ) * 128 );
	strncpy(pmydata->DISKPATH, ( const char *) getenv("BENCHIT_KERNEL_DISKPATH"), 128);
	pmydata->DISKSPACE = (double)atof( (const char *)getenv("BENCHIT_KERNEL_DISKSPACE") );
	pmydata->NUMCHANNELS = (int)atoi( (const char *)getenv("BENCHIT_KERNEL_NUMCHANNELS") );
	pmydata->CHANNELFACTOR = (int)atoi( (const char *)getenv("BENCHIT_KERNEL_CHANNELFACTOR") );
//	pmydata->RAMSIZE = (double)atof( (const char *)getenv("BENCHIT_KERNEL_RAMSIZE") );	
}




/****f* functions/helpers::writefile
 * SYNOPSIS
 * void writefile(backpack_t * bp, unsigned long count, double * start_time, double * end_time)
 * DESCRIPTION
 * this function writes 1 file of the given size and measures the 
 * time needed to do so
 ***/
void writefile(backpack_t * bp, unsigned long count, double * start_time, 
    double * end_time)
{
	unsigned long i = 0, j = 0, nmemb = 0, nloops = 0;
	char * file;
	FILE * fp;

	file = (char *) malloc( (strlen(bp->filepath) + 63) * sizeof(char));
	if (file==NULL) { printf("writefile: Cant get path for writing!\n"); 
	fflush(stdout); exit (1); }

	j = ( count / FILES_PER_DIR ) * FILES_PER_DIR ;

	snprintf( file, (strlen(bp->filepath) + 63),"%snumber_%d/output%d", 
	   bp->filepath, (unsigned int) j, (unsigned int) count );  

	fp=fopen(file, "w");

	if(fp==NULL)
	  {
	    printf("can't create file %s\n", file);
	    exit(1);
	  }

	/*begin of time measurement*/
	nmemb = BUFFERSIZE / sizeof(char);
	nloops =  (unsigned int)( bp->env_var->FILESIZE / BUFFERSIZE );
	*start_time = bi_gettime();
	/*buffer from RAM to HD*/
	for ( i = 0; i < nloops; i++)
	{
	 fwrite(bp->filebuffer, sizeof(char), nmemb, fp);
	}
	*end_time = bi_gettime();
	fclose(fp);

	dprintf( 1, "writefile: finished writing file number %d \n", (int)count);
	free(file);
}



/****f* functions/helpers::thread_writefile
 * SYNOPSIS
 * void thread_writefile(thread_arg_wrapper_t *taw)
 * DESCRIPTION
 * this is a wrapper for the threadpool
 * the threadpool only takes functions with one argument
 * so all the arguments are "wrapped" in to one record
 ***/
void thread_writefile(thread_arg_wrapper_t *taw)
{
	  writefile( (backpack_t *)( taw->bp ), taw->i,  taw->start_time , 
	      taw->end_time );
}



/**  The implementation of the bi_getinfo from the BenchIT interface.
 *   Here the infostruct is filled with informations about the
 *   kernel.
 *   @param infostruct  a pointer to a structure filled with zero's
 */
void bi_getinfo( bi_info * pinfo )
{
   environment_variables_t * penv;
   
   (void) memset ( pinfo, 0, sizeof( bi_info ) );
   penv = (environment_variables_t *) malloc( sizeof( environment_variables_t ) );

   /* get environment variables for the kernel */
   evaluate_environment(penv);
   pinfo->codesequence = bi_strdup( "mkdir binary-directory-tree; write leaves (=files)" );
   pinfo->kerneldescription = bi_strdup( "PThread-kernel writing files into a flat directory" );
   pinfo->xaxistext = bi_strdup( "Data written in bytes" );
   pinfo->maxproblemsize = (int)(penv->DISKSPACE / penv->FILESIZE);
   pinfo->num_processes = penv->NUMCHANNELS;
   pinfo->num_threads_per_process = penv->CHANNELFACTOR;
   pinfo->kernel_execs_mpi1 = 0;
   pinfo->kernel_execs_mpi2 = 0;
   pinfo->kernel_execs_pvm = 0;
   pinfo->kernel_execs_omp = 0;
   pinfo->kernel_execs_pthreads = 1;
   pinfo->numfunctions = 2;

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
      pinfo->yaxistexts[0] = bi_strdup( "byte / s" );
      pinfo->outlier_direction_upwards[0] = 0;
      pinfo->base_yaxis[0] = 10; //logarythmic axis 10^x
      pinfo->legendtexts[0] = bi_strdup( "average I/O performance" );
 
      pinfo->yaxistexts[1] = bi_strdup( "byte / s" );
      pinfo->outlier_direction_upwards[1] = 0;
      pinfo->base_yaxis[1] = 10; //logarythmic axis 10^x
      pinfo->legendtexts[1] = bi_strdup( "maximum I/O performance" );
 
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
	 unsigned long l = 0, nr_of_files = 0;
	 int i = 0, r = 0;
	 backpack_t * bp;
	 char * file;
	 
	 /* the next line is just to avoid complier-warnings */
	 nr_of_files = problemsizemax;
	 
	 bp = (backpack_t *) malloc( sizeof( backpack_t ) );
	 if (bp == NULL) 
	    { printf ("bi_init: malloc for backpack-variable failed\n");
	    fflush( stdout ); exit (1); }

	 bp->env_var = (environment_variables_t *) 
	    malloc ( sizeof( environment_variables_t ) );
	 if (bp->env_var == NULL) 
	    { printf("bi_init: malloc for backpack-variable->env_var failed\n");
	    fflush( stdout ); exit (1); }

	 evaluate_environment(bp->env_var);
	 dprintf(2, "\nFilesize = %f\n",bp->env_var->FILESIZE);
         dprintf(2, "NumChannels = %i\n",bp->env_var->NUMCHANNELS);
         dprintf(2, "ChannelFactor = %i\n",bp->env_var->CHANNELFACTOR);
         dprintf(2, "DiskPath = %s\n",bp->env_var->DISKPATH);
         dprintf(2, "DiskSpace = %f\n",bp->env_var->DISKSPACE);
//         dprintf(2, "RAMSize = %f\n",bp->env_var->RAMSIZE);
         dprintf(2, "TimeLimit = %i\n",bp->env_var->TIMELIMIT);

	 nr_of_files = (unsigned long)( (bp->env_var->DISKSPACE) / 
	    (bp->env_var->FILESIZE) );
	    
	 if (nr_of_files != problemsizemax) printf("bi_init: strange nr of files . . .\n");

	 bp->filepath = (char *) malloc( sizeof( char ) * 255 );
	 if (bp->filepath == NULL) 
	    { printf("bi_init: malloc for backpack-variable->filepath \
	    failed\n"); fflush( stdout ); exit (1); }

   	 dprintf(2, "DISKPATH letztes Zeichen = %s \n", 
	    bp->env_var->DISKPATH + strlen (bp->env_var->DISKPATH)-1 );

	 i = sprintf( bp->filepath, "%sio_write_tmp_%d/", bp->env_var->DISKPATH,
	    (int)bi_gettime() );
	 if (i <= 0) { printf ("bi_init: sprintf to bp->filepath failed\n"); 
	     exit (1); } 
	 dprintf( 2 , "filepath = %s\n", bp->filepath );
	 if ( mkdir(bp->filepath, S_IRWXU) ) { printf ("bi_init: mkdir of \
	    bp->filepath = %s failed\n", bp->filepath ); exit (1); }

	 /* now the directories will be made */

	 file = (char *) malloc( sizeof(char) * 255 );
	 if (file==NULL) { printf("bi_entry: Cant get path for writing!'\n"); 
	 fflush(stdout); exit (1); }

	 for ( i = 0; i <= nr_of_files; i = i + FILES_PER_DIR )
	 {
          strncpy(file, bp->filepath, 255); 
	  sprintf(file, "%snumber_%d/", bp->filepath, (unsigned int)i);
	  mkdir(file, S_IRWXU);
	  dprintf( 2, "made directory: %s\n", file);
	 }

	 /*
	  * allocate 1 Mb for the buffer 
	  */
	 l = BUFFERSIZE; /* 1048576 = 1Mb in byte */
	 bp->filebuffer = (char *)calloc( l+1, sizeof(char) );
	 dprintf( 2, "FILESIZE=%f  sizeof(char)=%d \n", bp->env_var->FILESIZE, 
	    sizeof(char) );
	 
	 for (i = 0; i < l; i++)
	 	{
		 r = 1 + (int) (13.0*rand() /( RAND_MAX+1.0));
		 switch (r)
		 	{
		 	 case 1  : bp->filebuffer[i] = 'T';
			 case 2  : bp->filebuffer[i] = 'h';
			 case 3  : bp->filebuffer[i] = 'o';
			 case 4  : bp->filebuffer[i] = 'm';
			 case 5  : bp->filebuffer[i] = 'a';
			 case 6  : bp->filebuffer[i] = 's';
			 case 7  : bp->filebuffer[i] = 'W';
			 case 8  : bp->filebuffer[i] = 'i';
			 case 9  : bp->filebuffer[i] = 'l';
			 case 10 : bp->filebuffer[i] = 'l';
			 case 11 : bp->filebuffer[i] = 'i';
			 case 12 : bp->filebuffer[i] = 'a';
			 case 13 : bp->filebuffer[i] = 'm';
			 default : bp->filebuffer[i] = 'e';
			} 
		}
	 dprintf( 2, "BufferSize = %d\n", (unsigned long)
	    strlen( bp->filebuffer )); 

	 free(file);
 	 return ( (void *) bp );	 
}




/****f* functions/bi_entry
 * DESCRIPTION
 * This is the "real" measurement.
 * The treadpool is intialized.
 * The number of files is calculated based upon the vaulues
 * set in the io_write_pthreads/PARAMETERS file.
 * After that writefile() is called n-times as a job of the 
 * workerthreads of the threadpool.
 * A rather complex loop calculates the io-performance which
 * are to be shown in the performance-graph afterwards.
 * Finally the written files are deleted.
 ***/	
int bi_entry( void * bpp, int problemsize, double * results )
	{
	 backpack_t * bp;
 	 tpool_t tpool;
	 thread_arg_wrapper_t *taw;
	 unsigned long nr_of_files = 0, i = 0, j = 0, k = 0, l = 0;
	 double * start_time, * end_time, * activity, 
	    max_val = 0.0, min = 0.0, max = 0.0; 
	 int num_threads = 0, queue_length = 0;
	 char * file1;

	 bp = (backpack_t *)bpp;
/*	
	 nr_of_files = (unsigned long)( (bp->env_var->DISKSPACE) / 
	    (bp->env_var->FILESIZE) );
*/
	 nr_of_files = problemsize;
	  
	 dprintf( 2, "Number of files that will be written:%d\n", nr_of_files );
	 
	 /*
	  * now the threadpool will be initialised
	  */
	 num_threads =(int) ( (bp->env_var->NUMCHANNELS) * 
	    (bp->env_var->CHANNELFACTOR) );
	 dprintf( 2, "number of threads = %d\n", num_threads );
	 queue_length = (int) ( num_threads * QUEUE_LENGTH_MULTIPLY );
	 dprintf( 2, "queuelength = %d\n", queue_length );
	 tpool_init( &tpool, num_threads, queue_length,0 );
	 
	 start_time = (double *) calloc( (unsigned long) nr_of_files, 
	    sizeof( double ) );
	 if (start_time == NULL) { printf ("bi_entry: malloc for start_time \
	    failed\n"); fflush(stdout); exit (1); }
	 
	 end_time = (double *) calloc( (unsigned long) nr_of_files, 
	    sizeof( double ) );
	 if (end_time == NULL) { printf ("bi_entry: malloc for taw failed\n"); 
	 fflush(stdout); exit (1); }
	 
	 taw = (thread_arg_wrapper_t *) calloc( (unsigned long) nr_of_files, 
	 sizeof( thread_arg_wrapper_t ) );
	 if (taw == NULL) { printf ("bi_entry: malloc for taw failed\n"); 
	 fflush(stdout); exit (1); }

	 for( i = 0; i < nr_of_files ; i++ )
	   {
	     taw[i].i = i;
	     taw[i].start_time = &start_time[i]; 
	     taw[i].end_time = &end_time[i];
/* 
 * this has to be an deep copy in order to provide each thread with an own 
 * copy of the buffer (parallelism may otherwise cause problems??? on some
 * very exotic machines - at least that is what they told me)
 */ 
	     taw[i].bp = (backpack_t *) malloc( sizeof( backpack_t ) );
	     if (taw[i].bp == NULL) { printf ("bi_entry: malloc for taw[%d].\
	     backpack-variable failed\n", (int)i ); fflush(stdout); exit (1);}
  	     
	     taw[i].bp->env_var = (environment_variables_t *) malloc 
	        ( sizeof( environment_variables_t ) );
	     if (taw[i].bp->env_var == NULL) { printf ("bi_entry: malloc for \
	        taw[%d].backpack-variable->env_var failed\n", (int)i );
		fflush(stdout); exit (1); }
	     evaluate_environment(taw[i].bp->env_var);

	     taw[i].bp->filepath = (char *) malloc ( sizeof( char ) * 255 );
	     if (taw[i].bp->filepath == NULL) { printf ("bi_entry: malloc for \
	        taw[%d].backpack-variable->filepath failed\n", (int)i );
		fflush(stdout); exit (1); }
	     strncpy(taw[i].bp->filepath, bp->filepath, 255); 
	     dprintf(2, "filepath des %iten files: %s\n", i, 
	        taw[i].bp->filepath);

	     taw[i].bp->filebuffer = (char *) malloc ( (unsigned long) 
	        BUFFERSIZE / sizeof( char ) );
	     if (taw[i].bp->filebuffer == NULL) { printf ("bi_entry: malloc for\
	        taw[%d].backpack-variable->filebuffer failed\n", (int)i );
		fflush(stdout); exit (1); }
	     strncpy(taw[i].bp->filebuffer, bp->filebuffer, BUFFERSIZE); 
	   }

	 /*writing loop*/
	 for( i = 0; i < nr_of_files; i++ )	
		{
                  tpool_add_work( tpool, thread_writefile, &taw[i] ); 
		}

	tpool_destroy(tpool, 1);

	/*deleting written files */

	file1 = (char *) malloc( 255 * sizeof(char));
	
	for( i = 0; i < nr_of_files; i++ )
	{
	j = ( ( (unsigned int)( i / FILES_PER_DIR ) ) * FILES_PER_DIR ); 
	snprintf( file1, 255, "rm %snumber_%d/output%d", taw[i].bp->filepath, 
	   (int) j, (int) i ); 
	dprintf( 2, "%s\n", file1 );
	system(file1);
	}
	free(file1);

	i = 0;

dprintf( 2, "before mysterious numbercrunching\n");

	results[0]=problemsize*bp->env_var->FILESIZE;				

	min=start_time[0];
	max=end_time[0];

        /* search for min and max */
	for( i = 0; i < nr_of_files ; i++ )
	{
	min = start_time[i] < min ? start_time[i] : min;
	max = end_time[i] > max ? end_time[i] : max;
	}

        /* average */	
	results[1]= (double) nr_of_files * ( bp->env_var->FILESIZE / 
	    (max - min) );	

	 activity = (double *) calloc( (unsigned long) ( nr_of_files * 2 + 1), 
	    sizeof( double ) );
	 if (activity == NULL) { printf ("bi_entry: malloc for activity  \
	    failed\n"); fflush(stdout); exit (1); }

	i = 0;
	j = 0;
	k = 0;
	l = 0;
	min = 0.0;
	max = 0.0;

	activity[0] = (bp->env_var->FILESIZE) / (end_time[0]-start_time[0]);
	
	while( l < (int)( 2 * nr_of_files ) )
		{
		  l++;
		  /* suche frühesten startwert */
		  for( i = 0; i < (int)( nr_of_files - 1 ); i++ ) 
			  {
			    if( start_time[i] < min )
				    {
				      min=start_time[i];
				      j=i;
				    }	
			  }
		  /* suche frühesten endwert */
		  for( i = 0; i < (int)( nr_of_files - 1 ); i++) 
			  {
			    if( end_time[i] < max)
				    {
				      max = end_time[i];
				      k=i;
				    }
			  }
		  /* start vor ende??
		   * filewriterate dazuaddieren */
		  if( min < max ) 
		      {
			activity[l] = activity[l-1] + (bp->env_var->FILESIZE) / 
			   (end_time[j] - start_time[j] );
		      }
		  /* filewriterate subtrahieren */
		  else 
		      {
			activity[l] = activity[l-1] - (bp->env_var->FILESIZE) / 
			   (end_time[k] - start_time[k]);
		      }
		      
		  for( i = 0; i < (int)(nr_of_files-1); i++ ) 
		      {
			min = start_time[i] > min ? start_time[i] : min;
		      }
		  for( i = 0; i < (int)(nr_of_files-1); i++ ) 
		      {
			max = end_time[i] > max ? end_time[i] : max;
		      }
		  start_time[j] = min;
		  end_time[k] = max;
		}

	/* what is the maximum-value in the activity-graph */
	max_val = activity[0];
	for ( i = 0; i < (int)( nr_of_files * 2 ); i++ )
	{
	  max_val = activity[i] > max_val ? activity[i] : max_val;
	}

	/*max-io-rate*/
	results[2]=max_val;

dprintf( 2, "after mysterious numbercrunching\n");


	if(activity == NULL){printf("activity NOT freed properly\n");}
dprintf( 2, "freeing activity\n");
  	free(activity);
  	if(activity == NULL){printf("activity freed properly\n");}

dprintf( 2, "freeing start/end-time\n");
  	if(start_time == NULL){printf("start_time NOT freed properly\n");}
  	if(end_time == NULL){printf("end_time NOT freed properly\n");}
	free(start_time);
	free(end_time);
  	if(start_time == NULL){printf("start_time freed properly\n");}
  	if(end_time == NULL){printf("end_time freed properly\n");}

dprintf( 2, "freeing taw\n");
	for( i = 0; i < nr_of_files; i++ )
	{
	  free(taw[i].bp->env_var->DISKPATH);
	  free(taw[i].bp->env_var);
	  free(taw[i].bp->filebuffer);
	  free(taw[i].bp->filepath);
/*	  free(taw[i].bp);*/
	}

	free(taw);
	if(taw == NULL){printf("taw freed properly\n");}
	
dprintf( 2, "freed all structures properly\n");
	return 0;
}


/****f* functions/bi_cleanup
 * DESCRIPTION
 * undo everything the kernel did before
 ***/	
void bi_cleanup(void *bp)
	{
	
	char * command = (char *) malloc( 255 * sizeof(char));

	snprintf( command, 255, "rm -r %s", ((backpack_t *)bp)->filepath);
	system(command);
	free(command);
	free( (void *)((backpack_t *)bp)->env_var->DISKPATH);
	free( (void *)( ((backpack_t *)bp)->env_var) );
	free( (void *)( ((backpack_t *)bp)->filebuffer) );
	free( (void *)( ((backpack_t *)bp)->filepath) );
	free( (void *)( (backpack_t *) bp ) );
	}

/********************************************************************
 * Log-History
 * 
 * $Log: kernel_main.c,v $
 * Revision 1.1  2006/12/14 12:54:15  william
 * changed the algorithm a bit for mor informative resultfilesmade chenges to reflect new format of COMPILE.SH and a like
 *
 * Revision 1.2  2006/05/21 20:58:00  william
 * added complete cleanup of generated files
 *
 * Revision 1.1  2006/04/27 12:01:16  william
 * altered, cleaned up and successfully tested the kernel
 *
 *
 *******************************************************************/ 
