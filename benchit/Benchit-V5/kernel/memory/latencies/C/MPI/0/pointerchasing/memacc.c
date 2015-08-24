/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Memory Access Time (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.4 $
 * $Date: 2006/01/03 18:10:14 $
 *******************************************************************/

#include "interface.h"
#include "memacc.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <mpi.h>

#ifndef BENCHIT_KERNEL_MIN_ACCESS_LENGTH
#define BENCHIT_KERNEL_MIN_ACCESS_LENGTH (2048)
#endif

#ifndef BENCHIT_KERNEL_MAX_ACCESS_LENGTH
#define BENCHIT_KERNEL_MAX_ACCESS_LENGTH (1024*1024)
#endif

#ifndef BENCHIT_KERNEL_ACCESS_STRIDE
#define BENCHIT_KERNEL_ACCESS_STRIDE (2048)
#endif

#ifndef BENCHIT_KERNEL_NUMBER_OF_JUMPS
#define BENCHIT_KERNEL_NUMBER_OF_JUMPS (4000000)
#endif


unsigned int random_number( unsigned int max);
void make_linked_memory( void *mem, int count);
void init_global_vars(void);

long minlength, maxlength, accessstride, numjumps;
int rank,size;

void bi_getinfo(bi_info* infostruct){
  int i;
  char buf[200];

  init_global_vars();
  /*infostruct->kernelstring=bi_strdup("Random Memory Access");*/
  infostruct->kerneldescription = bi_strdup( "Memory Access Time (C)" );
  infostruct->codesequence=bi_strdup("for i=1,N#  var=memory[random(0..size)]#");
  infostruct->xaxistext=bi_strdup("Accessed Memory in Byte");
  /*infostruct->kernellanguage=bi_strdup("C");*/
  infostruct->numfunctions=size;
  infostruct->maxproblemsize=((maxlength-minlength+1)/accessstride);
  infostruct->outlier_direction_upwards=malloc(infostruct->numfunctions*sizeof(int));
  infostruct->outlier_direction_upwards[0]=1;
  infostruct->base_xaxis=2.0;
  infostruct->base_yaxis=malloc(infostruct->numfunctions*sizeof(double));
  infostruct->base_yaxis[0]=0.0;
  infostruct->legendtexts=malloc(infostruct->numfunctions*sizeof(char*));
  if (infostruct->legendtexts==0){
    printf("No more core\n");
    exit(127);
  }
  for (i=0;i<infostruct->numfunctions;i++){
    sprintf(buf,"Average Access Time (%d active, %d idle)",i+1,size-i-1);
    infostruct->legendtexts[i]=bi_strdup(buf);
  }
  infostruct->yaxistexts=malloc(infostruct->numfunctions*sizeof(char*));
  if (infostruct->legendtexts==0){
    printf("No more core\n");
    exit(127);
  }
  for (i=0;i<infostruct->numfunctions;i++)
    infostruct->yaxistexts[i]=bi_strdup("s");
#ifdef USE_MPI
  infostruct->kernel_execs_mpi1=1;
#endif
}

void init_global_vars() {
    
  char *envir;
    
#ifdef USE_MPI
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  MPI_Comm_size (MPI_COMM_WORLD, &size);
#else
  rank=0;
  size=1;
#endif
  IDL(3,printf("Init global variables ... "));
  envir=bi_getenv("BENCHIT_KERNEL_MIN_ACCESS_LENGTH",1);
  minlength=(envir != 0) ? 1024*atoi(envir) : BENCHIT_KERNEL_MIN_ACCESS_LENGTH;
  if(minlength==0) {
    minlength=BENCHIT_KERNEL_MIN_ACCESS_LENGTH;
  }
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_MAX_ACCESS_LENGTH",1);
  maxlength=(envir != 0) ? 1024*atoi(envir) : BENCHIT_KERNEL_MAX_ACCESS_LENGTH;
  if(maxlength==0) {
    maxlength=BENCHIT_KERNEL_MIN_ACCESS_LENGTH;
  }
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_ACCESS_STRIDE",1);
  accessstride=(envir != 0) ? 1024*atoi(envir) : BENCHIT_KERNEL_ACCESS_STRIDE;
  if(accessstride==0) {
    accessstride=BENCHIT_KERNEL_ACCESS_STRIDE;
  }
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_NUMBER_OF_JUMPS",1);
  numjumps=(envir != 0) ? 1024*atoi(envir) : BENCHIT_KERNEL_NUMBER_OF_JUMPS;
  if(numjumps==0) {
    numjumps=BENCHIT_KERNEL_NUMBER_OF_JUMPS;
  }
  IDL(3,printf("done\n"));
}

BI_GET_CALL_OVERHEAD_FUNC((),jump_around(NULL,0,0));

/** generates a random number between 0 and (max-1)
 *  @param  max maximum random number
 *  @return a random number between 0 and (max-1)
 */
unsigned int random_number( unsigned int max)
{
  return (unsigned int) (((double)max)*rand()/(RAND_MAX+1.0));
}

/** creates a memory are that is randomly linked 
 *  @param mem     the memory area to be used
 *  @param length  the number of bytes that should be used
 */
void make_linked_memory( void *mem, int length) {

  /* some pointers to generate the list */
  void **ptr, **first;
  /** how many ptr we create within the memory */
  int num_ptr=length/sizeof(void *);
  /** the list for all memory locations that are linked */
  int *ptr_numbers;
  /** for the loops */
  int loop_ptrs;
  /** actual random number */
  int act_num;

  /* allocate memory for ptr numbers */
  ptr_numbers=(int *) malloc(num_ptr*sizeof(int));
  if( num_ptr>0 && ptr_numbers==NULL)
    {
      printf("no more core in make_linked_mem()\n");
      bi_cleanup(mem);
      exit(1);
    }
  /* initialize ptr numbers, the 0 is used as the first
   * number
   */
  for( loop_ptrs=1; loop_ptrs<num_ptr; loop_ptrs++)
    ptr_numbers[loop_ptrs-1]=loop_ptrs;

  /* init first ptr with first memory location */
  ptr=(void **)mem;
  first=ptr;
   
  num_ptr--;

  while( num_ptr>1 ) {
    /* get a random position within the
       remaining list */
    act_num=random_number(num_ptr);
    /* create a link from the last ptr 
       to this ptr */
    *ptr=(void *) (first+ptr_numbers[act_num]);
    /* move pointer to new memory location */
    ptr=first+ptr_numbers[act_num];
    /* remove used ptr number from list of
       pointer numbers, just copies the last 
       number to the actual position */
    ptr_numbers[act_num]=ptr_numbers[num_ptr-1];
    num_ptr--;
  }

  /* the last number is linked to the first */
  *ptr=(void *) first;

  /* free the ptr list */
  free(ptr_numbers);
  IDL(4,printf("\n"));
}

void *bi_init(int problemsizemax){
  void *mem;

  IDL(3, printf("Enter init ... "));
  mem=malloc(minlength+(problemsizemax+1)*accessstride);
  if (mem==NULL){
    printf("No more core, need %.3f MByte\n", 
	   ((double)minlength*(problemsizemax+1)*accessstride)/(1024*1024));
    exit(127);
  }
  IDL(3, printf("allocated %.3f MByte\n",
		((double)minlength+(problemsizemax+1)*accessstride)/(1024*1024)));
  return (mem);
}

int bi_entry(void *mcb,int problemsize,double *results) {

  static double calloh=0;
  double start, stop;
  int numproc;

  problemsize=minlength+(problemsize-1)*accessstride; 
  if(calloh==0) {
    calloh=bi_get_call_overhead();
    if(calloh==0)
      calloh=dTimerGranularity;
  }

  if (rank==0) results[0]=(double) problemsize;

  for(numproc=1;numproc<=size;numproc++){    

    if(rank<numproc)
    {
      IDL( 2, printf("Making structure\n"));
      make_linked_memory( mcb, problemsize);
      IDL( 2, printf("Enter measurement\n"));
      jump_around( mcb, problemsize,numjumps); 
      start=bi_gettime();
      jump_around( mcb, problemsize,numjumps);
      stop=bi_gettime();
      IDL( 2, printf("Done\n"));
    }

    if (rank==0){
      /* we have always numjumps memory accesses */
      results[numproc]=(double)((stop-start-calloh-dTimerOverhead)/((double)numjumps));
    }
  }
  return (0);
}

void bi_cleanup(void *mcb){
  free(mcb);
  return;
}


/********************************************************************
 * Log-History
 * 
 * $Log: memacc.c,v $
 * Revision 1.4  2006/01/03 18:10:14  hackenb
 * minor changes
 *
 * Revision 1.3  2006/01/03 16:42:01  hackenb
 * modified/unified header and footer
 * new interface
 * now using bi_gettime()
 *
 * Revision 1.2  2005/11/22 01:26:51  mickler
 * + Using BI_GET_CALL_OVERHEAD_FUNC macro now
 * + Using bi_timer() function for time measuring
 * 
 * Revision 1.1  2005/08/24 12:22:51  juckel
 * first checkin
 * 
 * Revision 1.2  2005/08/16 16:20:02  juckel
 * some bugfixing
 * 
 * Revision 1.1  2005/08/16 14:20:53  juckel
 * - included kernel from old benchit into new structure
 * - changed MPI-functionality
 *   old: only root process runs memory test -> generates 1 data set
 *   new: for i=1 to numproc do ruu mesurement -> generates numproc data sets
 * 
 * Revision 2.2  2004/11/03 21:40:14  kluge
 * forgot the /2
 * 
 * Revision 2.1  2004/09/23 15:58:29  juckel
 * ...
 * 
 * Revision 2.0  2003/12/09 11:19:00  juckel
 * build of version 2.0
 * 
 * Revision 1.2  2003/12/04 09:56:09  kluge
 * memacces has now a README and is ready to be used
 * 
 * Revision 1.1  2003/12/01 20:52:03  kluge
 * memaccess_c initial checkin
 * 
 *******************************************************************/
