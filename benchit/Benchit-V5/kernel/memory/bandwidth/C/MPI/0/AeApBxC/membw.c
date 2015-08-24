/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Memory Bandwidth (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.3 $
 * $Date: 2006/01/03 18:10:33 $
 *******************************************************************/

#include "interface.h"
#include "membw.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <mpi.h>

#ifndef BENCHIT_KERNEL_MIN_ACCESS_SIZE
#define BENCHIT_KERNEL_MIN_ACCESS_SIZE (2048)
#endif

#ifndef BENCHIT_KERNEL_MAX_ACCESS_SIZE
#define BENCHIT_KERNEL_MAX_ACCESS_SIZE (1024*1024)
#endif

#ifndef BENCHIT_KERNEL_ACCESS_STRIDE
#define BENCHIT_KERNEL_ACCESS_STRIDE (2048)
#endif


void init_global_vars(void);

long minlength, maxlength, accessstride;
int rank,size;

void bi_getinfo(bi_info* infostruct){
  int i;
  char buf[200];

  init_global_vars();
  /*infostruct->kernelstring=bi_strdup("Random Memory Access");*/
  infostruct->kerneldescription = bi_strdup( "Memory Bandwidth (C)" );
  infostruct->codesequence=bi_strdup("for i=1,N#  a[i]+=b[i]*c[i]#");
  infostruct->xaxistext=bi_strdup("Accessed Memory in Byte");
  /*infostruct->kernellanguage=bi_strdup("C");*/
  infostruct->numfunctions=size;
  infostruct->maxproblemsize=((maxlength-minlength+1)/accessstride);
  infostruct->outlier_direction_upwards=malloc(infostruct->numfunctions*sizeof(int));
  for (i=0;i<infostruct->numfunctions;i++)
    infostruct->outlier_direction_upwards[i]=0;
  infostruct->base_xaxis=2.0;
  infostruct->base_yaxis=malloc(infostruct->numfunctions*sizeof(double));
  infostruct->base_yaxis[0]=0.0;
  infostruct->legendtexts=malloc(infostruct->numfunctions*sizeof(char*));
  if (infostruct->legendtexts==0){
    printf("No more core\n");
    exit(127);
  }
  for (i=0;i<infostruct->numfunctions;i++){
    sprintf(buf,"Bandwidth (%d active, %d idle)",i+1,size-i-1);
    infostruct->legendtexts[i]=bi_strdup(buf);
  }
  infostruct->yaxistexts=malloc(infostruct->numfunctions*sizeof(char*));
  if (infostruct->legendtexts==0){
    printf("No more core\n");
    exit(127);
  }
  for (i=0;i<infostruct->numfunctions;i++)
    infostruct->yaxistexts[i]=bi_strdup("Byte/s");
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
  envir=bi_getenv("BENCHIT_KERNEL_MIN_ACCESS_SIZE",1);
  minlength=(envir != 0) ? 1024*atoi(envir) : BENCHIT_KERNEL_MIN_ACCESS_SIZE;
  if(minlength==0) {
    minlength=BENCHIT_KERNEL_MIN_ACCESS_SIZE;
  }
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_MAX_ACCESS_SIZE",1);
  maxlength=(envir != 0) ? 1024*atoi(envir) : BENCHIT_KERNEL_MAX_ACCESS_SIZE;
  if(maxlength==0) {
    maxlength=BENCHIT_KERNEL_MIN_ACCESS_SIZE;
  }
  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_ACCESS_STRIDE",1);
  accessstride=(envir != 0) ? 1024*atoi(envir) : BENCHIT_KERNEL_ACCESS_STRIDE;
  if(accessstride==0) {
    accessstride=BENCHIT_KERNEL_ACCESS_STRIDE;
  }
  IDL(3,printf("done\n"));
}

BI_GET_CALL_OVERHEAD_FUNC((),mem_read( NULL,NULL, NULL, 0))

void *bi_init(int problemsizemax){
  vec_struct *mem;
  long i;
  long maxi;

  IDL(3, printf("Enter init ... "));
  maxi=(minlength+(problemsizemax+1)*accessstride)/sizeof(double);
  mem=malloc(sizeof(vec_struct));
  if (mem==NULL){
    printf("No more core, need %.3f MByte\n", 
	   ((double)minlength*(problemsizemax+1)*accessstride)/(1024*1024));
    exit(127);
  }
  mem->a=malloc(maxi/3*sizeof(double));
  mem->b=malloc(maxi/3*sizeof(double));
  mem->c=malloc(maxi/3*sizeof(double));
  if (mem->a==NULL){
    printf("No more core, need %.3f MByte\n",
           ((double)minlength*(problemsizemax+1)*accessstride)/(1024*1024));
    exit(127);
  }
  if (mem->b==NULL){
    printf("No more core, need %.3f MByte\n",
           ((double)minlength*(problemsizemax+1)*accessstride)/(1024*1024));
    exit(127);
  }
  if (mem->c==NULL){
    printf("No more core, need %.3f MByte\n",
           ((double)minlength*(problemsizemax+1)*accessstride)/(1024*1024));
    exit(127);
  }
  IDL(3, printf("allocated %.3f MByte\n",
		((double)minlength+(problemsizemax+1)*accessstride)/(1024*1024)));
  for (i=0;i<maxi/3;i++) {
    mem->a[i]=(double)(i);
    mem->b[i]=(double)(i);
    mem->c[i]=(double)(i);
  }
  return (void *)(mem);
}

int bi_entry(void *mcb,int problemsize,double *results) {

  static double calloh=0;
  double start, stop;
  int numproc,i,repeats,vectorsize;
  double *a=((vec_struct *)mcb)->a;
  double *b=((vec_struct *)mcb)->b;
  double *c=((vec_struct *)mcb)->c;

  problemsize=(minlength+(problemsize-1)*accessstride); 
  vectorsize=problemsize/sizeof(double)/3;
  repeats=(maxlength/problemsize);
  if(calloh==0) {
    calloh=bi_get_call_overhead();
  }
  
  if (rank==0) results[0]=(double) problemsize;

  for(numproc=1;numproc<=size;numproc++){    

    IDL( 2, printf("Enter measurement\n")); 
    mem_read( a, b, c, vectorsize);
#ifdef USE_MPI
    MPI_Barrier(MPI_COMM_WORLD);
#endif
    if(rank<numproc){
      start=bi_gettime();
      for (i=0;i<repeats;i++) mem_read( a, b, c, vectorsize);
    }
#ifdef USE_MPI
    MPI_Barrier(MPI_COMM_WORLD);
#endif
    stop=bi_gettime();
    IDL( 2, printf("Done\n"));

    if (rank==0){
      results[numproc]=(double)((double)(problemsize+problemsize/3)*numproc/((stop-start-calloh)/repeats-dTimerOverhead));
    }
  }
  return (0);
}

void bi_cleanup(void *mcb){
  double *a=((vec_struct *)mcb)->a;
  double *b=((vec_struct *)mcb)->b;
  double *c=((vec_struct *)mcb)->c;
  free(a);
  free(b);
  free(c);
  free(mcb);
  return;
}


/********************************************************************
 * Log-History
 * 
 * $Log: membw.c,v $
 * Revision 1.3  2006/01/03 18:10:33  hackenb
 * modified/unified header and footer
 * new interface
 * now using bi_gettime()
 *
 * Revision 1.2  2005/11/22 01:26:50  mickler
 * + Using BI_GET_CALL_OVERHEAD_FUNC macro now
 * + Using bi_timer() function for time measuring
 * 
 * Revision 1.1  2005/08/18 14:26:21  juckel
 * *** empty log message ***
 * 
 * Revision 1.4  2005/08/18 11:42:46  juckel
 * included barrier after measurement and accumulated bandwidth over processors
 * 
 * Revision 1.3  2005/08/17 14:11:26  juckel
 * none
 * 
 * Revision 1.2  2005/08/17 12:01:04  juckel
 * finetuning
 * 
 * Revision 1.1  2005/08/17 10:23:52  juckel
 * added kernel to cvs
 * 
 *******************************************************************/
