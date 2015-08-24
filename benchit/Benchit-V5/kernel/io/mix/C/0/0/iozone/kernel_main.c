/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: c kernel skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.2 $
 * $Date: 2007/04/27 07:39:46 $
 *******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "interface.h"
#include "kernel_main.h"
#include <unistd.h>

#define MAX_NR_FCT 22
#define MAX_LINE_LENGTH 255

/* from wor.h */
mydata_t* mdp;

void assert_mdp(){
   if ( mdp == NULL ) mdp = (mydata_t*)malloc( sizeof( mydata_t ) );
   if ( mdp == NULL ){
      fprintf( stderr, "Allocation of structure mydata_t failed\n" ); 
      fflush( stderr );
      exit(ENOMEM);
   }
}


/* Reads the environment variables used by this kernel. */
void evaluate_environment(){
    int inumentries = 0;
    int ii = 0;
    char * p;
    char unit;
    assert_mdp();
    
    mdp->filesize_min_unit = (char *)malloc(1);
    mdp->filesize_max_unit = (char *)malloc(1);
    mdp->filesize_inc_unit = (char *)malloc(1);
    
    mdp->filename = bi_getenv( "BENCHIT_KERNEL_FILENAME", 1 );
    mdp->filesize_min = bi_getenv( "BENCHIT_KERNEL_FILESIZE_MIN", 1 );
    mdp->filesize_max = bi_getenv( "BENCHIT_KERNEL_FILESIZE_MAX", 1 );
    mdp->filesize_inc = bi_getenv( "BENCHIT_KERNEL_FILESIZE_INCREMENT", 1 );
    mdp->min = (unsigned int)strtol(mdp->filesize_min, &(mdp->filesize_min_unit), 10);
    mdp->max = (unsigned int)strtol(mdp->filesize_max, &(mdp->filesize_max_unit), 10);
    mdp->inc = (unsigned int)strtol(mdp->filesize_inc, &(mdp->filesize_inc_unit), 10);
    mdp->recordsize = bi_getenv( "BENCHIT_KERNEL_RECORDSIZE", 1 );
    mdp->testlist = bi_getenv( "BENCHIT_KERNEL_TESTLIST", 1 );
    mdp->cachelinesize = bi_getenv( "BENCHIT_KERNEL_CACHELINE_SIZE", 1 );
    mdp->cachesize = bi_getenv( "BENCHIT_KERNEL_CACHE_SIZE", 1 );
    mdp->options = bi_getenv( "BENCHIT_KERNEL_OPTIONS", 1 );

//    printf("min=%lu max=%lu inc=%lu\n", mdp->min, mdp->max, mdp->inc);
    
    unit=*(mdp->filesize_min_unit);
    switch (unit){
        case 'k':
             ii=1;
             break;
        case 'm':
             ii=1024;
             break;
        case 'g':
             ii=1048576;
             break;
        default:
             exit(BENVUNKNOWN);
             break;
    }
    mdp->min = mdp->min * ii;
    
    unit=*(mdp->filesize_max_unit);
    switch (unit){
        case 'k':
             ii=1;
             break;
        case 'm':
             ii=1024;
             break;
        case 'g':
             ii=1048576;
             break;
        default:
             exit(BENVUNKNOWN);
             break;
    }
    mdp->max = mdp->max * ii;
    
    unit=*(mdp->filesize_inc_unit);
    switch (unit){
        case 'k':
             ii=1;
             break;
        case 'm':
             ii=1024;
             break;
        case 'g':
             ii=1048576;
             break;
        default:
             exit(BENVUNKNOWN);
             break;
    }
    mdp->inc = mdp->inc * ii;
//    printf("min=%lu max=%lu inc=%lu\n", mdp->min, mdp->max, mdp->inc);
    
    if ( 0 == strlen( mdp->testlist )){
        fprintf( stderr, "no tests to commit\n" ); 
        fflush( stderr );
        exit(BENVEMPTY);
    }
    
    /* find out how many values are given in the list */
    p = mdp->testlist;
    inumentries=1; /* first commata means 1 entry already found */
    while (p){
        p = strstr( p,",");
        if (p){
            p++;
            inumentries++;
        }
    }
    mdp->nr_tests = inumentries;
    /* allocate aray according to number of entries */
    mdp->testarray = (unsigned int *)malloc( sizeof(unsigned int) * inumentries);
    p = mdp->testlist;
    mdp->testarray[0] = strtol(p, (char **)NULL, 10);
    for (ii=1;ii<inumentries; ii++){
        p = strstr( p,",")+1; /* pointer to next number in string */
        mdp->testarray[ii] = strtol(p, (char **)NULL, 10);
    }
    
}



void bi_getinfo( bi_info * infostruct )
{
   int i = 0, j = 0, iswitch=0;
   (void) memset ( infostruct, 0, sizeof( bi_info ) );
   /* get environment variables for the kernel */
   printf("\nHello You there\n");
   evaluate_environment();
   infostruct->codesequence = bi_strdup( "iozone" );
   infostruct->xaxistext = bi_strdup( "size of file" );
   infostruct->maxproblemsize = (mdp->max - mdp->min+1)/mdp->inc;
   if( (mdp->max - mdp->min+1) % mdp->inc != 0 )
     infostruct->maxproblemsize++;
   infostruct->num_processes = 1;
   infostruct->num_threads_per_process = 0;
//   infostruct->base_xaxis = 0;

   infostruct->kernel_execs_mpi1 = 0;
   infostruct->kernel_execs_mpi2 = 0;
   infostruct->kernel_execs_pvm = 0;
   infostruct->kernel_execs_omp = 0;
   infostruct->kernel_execs_pthreads = 0;
   infostruct->numfunctions = MAX_NR_FCT;

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

   j=0;
   for ( i = 0; i < mdp->nr_tests; i++ ){
        iswitch = (int)mdp->testarray[i];
        switch (iswitch){
            case 0:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "write" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "rewrite" );
                j++;
                break;
            
            case 1:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "read" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "reread" );
                j++;
                break;
             
            case 2:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "random read" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "random write" );
                j++;
                break;
           
            case 3:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "read backwarts" );
                j++;
                break;
            
            case 4:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "rewrite record" );
                j++;
                break;
            
            case 5:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "stride read" );
                j++;
                break;
            
            case 6:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "fwrite" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "frewrite" );
                j++;
                break;
            
            case 7:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "fread" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "freread" );
                j++;
                break;
            
            case 8:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "random mix" );
                j++;
                break;
            
            case 9:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "pwrite" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "prewrite" );
                j++;
                break;
            
            case 10:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "pread" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "preread" );
                j++;
                break;
            
            case 11:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "pwritev" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "prewritev" );
                j++;
                break;
            
            case 12:
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "preadv" );
                j++;
                infostruct->yaxistexts[j] = bi_strdup( "Bytes/s" );
                infostruct->outlier_direction_upwards[j] = 0;
                infostruct->base_yaxis[j] = 0;
                infostruct->legendtexts[j] = bi_strdup( "prereadv" );
                j++;
                break;
            default:
                exit(BENVUNKNOWN);
                break;
        }
   }
   infostruct->numfunctions = j;
   mdp->numfunctions = j;

   if ( DEBUGLEVEL > 3 ){
      for ( i = 0; i < infostruct->numfunctions; i++ ){
         printf( "yaxis[%2d]=%s\t\t\tlegend[%2d]=%s\n",
            i, infostruct->yaxistexts[i], i, infostruct->legendtexts[i] );
      }
   }
}



void* bi_init( int problemsizemax ){
		char filenamestring[STRINGSIZE];

    assert_mdp();
    return (void*)mdp;
    
    /* clean up resultoutputfile */
    system("rm -f iozone.out");
    
    /* clean up outputfile */
    sprintf(filenamestring,"rm -f %s", mdp->filename);
    system(filenamestring);
}



int bi_entry( void* mdpv, int problemsize, double* results ){
    char command[STRINGSIZE];
    char inputline[MAX_LINE_LENGTH+1];
    int ii=0, ret=0, j=1;
    unsigned long filesize;
    char filesizestring[STRINGSIZE];
    char * p;
    FILE * ptr;
    
    
    if ( results == NULL ) exit(ENOMEM);

    /* calculate real problemsize in Kbyte*/
    filesize = mdp->min + ( problemsize - 1 ) * mdp->inc;
    sprintf(filesizestring, "%lu", filesize);

    for (ii=0; ii<mdp->nr_tests; ii++){
            
            /* assemble iozone-call */
        snprintf(command, STRINGSIZE, "./bin/iozone -s %luk -r %s -S %s -L %s %s -i %d -w -f %s -b iozone.xls ", 
        filesize, 
        mdp->recordsize,
        mdp->cachesize,
        mdp->cachelinesize,
        mdp->options,
        mdp->testarray[ii],
        mdp->filename);
        printf("command: %s\n", command);

        /* really bad hack ahead */
        if ((ptr = popen(command, "r")) != NULL)
            while (fgets(inputline, MAX_LINE_LENGTH, ptr) != NULL){
                if ((inputline[0] == '"') && 
                    (inputline[1] == filesizestring[0])){
                        p = inputline;
                        p++; /* skip first */
                        p = strstr( p,"\""); /* find second " */ 
                        p++;
                        results[j] = (double)strtol(p, (char **)NULL, 10) * 1024;
                        printf("result - %d - %f \n", j, results[j]); 
                        j++;
                }
            }
        (void) pclose(ptr);
        popen("sync", "r");
        //sleep((unsigned int)5);
    }

    system("rm -f iozone.tmp");
    system("rm -f iozone.xls");
    
    results[0] = (double)filesize * 1024;
    return 0;
}

/** Clean up the memory
 */
void bi_cleanup( void* mdpv )
{
   mydata_t* mdp = (mydata_t*)mdpv;
   if ( mdp ) free( mdp );
   return;
}


/********************************************************************
 * Log-History
 * $Log: kernel_main.c,v $
 * Revision 1.2  2007/04/27 07:39:46  william
 * additional functionality
 *
 * Revision 1.1  2007/04/25 07:11:33  william
 * removed the iozone-sources
 *
 *******************************************************************/
