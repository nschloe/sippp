/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Memory Access Time (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.5 $
 * $Date: 2006/01/03 16:40:26 $
 *******************************************************************/

#include "interface.h"
#include "pointerchasing.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#ifndef BENCHIT_KERNEL_MIN_ACCESS_LENGTH
#define BENCHIT_KERNEL_MIN_ACCESS_LENGTH (2048)
#endif

#ifndef BENCHIT_KERNEL_MAX_ACCESS_LENGTH
#define BENCHIT_KERNEL_MAX_ACCESS_LENGTH (1024*1024)
#endif

#ifndef BENCHIT_KERNEL_ACCESS_STEPS
#define BENCHIT_KERNEL_ACCESS_STEPS (100)
#endif

#ifndef BENCHIT_KERNEL_NUMBER_OF_JUMPS
#define BENCHIT_KERNEL_NUMBER_OF_JUMPS (4000000)
#endif


unsigned int random_number( unsigned long max);
void make_linked_memory( void *mem, long count);
void init_global_vars(void);

long minlength, maxlength, accessstride, numjumps;
double dMemFactor;
long nMeasurements;

int NUM_COUNTERS;

void bi_getinfo(bi_info* infostruct){
  int i;
  char buf[200], *s;
 

  init_global_vars();
  /*infostruct->kernelstring=bi_strdup("Random Memory Access");*/
  infostruct->kerneldescription = bi_strdup( "Memory Access Time (C)" );
  infostruct->codesequence=bi_strdup("for i=1,N#  var=memory[random(0..size)]#");
  infostruct->xaxistext=bi_strdup("Accessed Memory in Byte");
  
  infostruct->numfunctions= 1;
  infostruct->maxproblemsize=nMeasurements;
  infostruct->outlier_direction_upwards=malloc(infostruct->numfunctions*sizeof(int));
  
  for (i=0; i< infostruct->numfunctions; i++)
  		infostruct->outlier_direction_upwards[i]=1;
		
  infostruct->base_xaxis=2.0;

  infostruct->base_yaxis=malloc(infostruct->numfunctions*sizeof(double));
  infostruct->base_yaxis[0]=0.0;
  infostruct->legendtexts=malloc(infostruct->numfunctions*sizeof(char*));
  
  infostruct->legendtexts[0]=bi_strdup("Average Access Time");
  infostruct->yaxistexts=malloc(infostruct->numfunctions*sizeof(char*));
  infostruct->yaxistexts[0]=bi_strdup("s");
}

void init_global_vars() {
    
  char *envir;
    

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
  envir=bi_getenv("BENCHIT_KERNEL_ACCESS_STEPS",1);
  nMeasurements = (envir != 0) ? atoi(envir) : BENCHIT_KERNEL_ACCESS_STEPS;
  dMemFactor =((double)maxlength)/((double)minlength);
  dMemFactor = pow(dMemFactor, 1.0/((double)nMeasurements-1));

  envir=0;
  envir=bi_getenv("BENCHIT_KERNEL_NUMBER_OF_JUMPS",1);
  numjumps=(envir != 0) ? atoi(envir) : BENCHIT_KERNEL_NUMBER_OF_JUMPS;
  if(numjumps==0) {
    numjumps=BENCHIT_KERNEL_NUMBER_OF_JUMPS;
  }
  IDL(3,printf("done\n"));
}

BI_GET_CALL_OVERHEAD_FUNC((),jump_around(NULL,0))

void *bi_init(int problemsizemax){
  void *mem;

  IDL(3, printf("Enter init ... "));
  mem=malloc(maxlength);
  if (mem==NULL){
    printf("No more core, need %.3f MByte\n", 
	   (double)maxlength);
    exit(127);
  }
  IDL(3, printf("allocated %.3f MByte\n",
		(double)maxlength));
  return (mem);
}

void bi_cleanup(void *mcb){
  free(mcb);
  return;
}


/********************************************************************
 * Log-History
 * 
 * $Log: pointerchasing_init.c,v $
 * Revision 1.5  2006/01/03 16:40:26  hackenb
 * new interface
 *
 * Revision 1.4  2006/01/03 15:34:35  hackenb
 * modified/unified header and footer
 * new interface
 *
 * 
 * Revision 1.3  2005/11/22 01:26:51  mickler
 * + Using BI_GET_CALL_OVERHEAD_FUNC macro now
 * + Using bi_timer() function for time measuring
 * 
 * Revision 1.2  2005/09/06 08:50:07  mark
 * bugfixes
 * 
 * Revision 1.1  2005/09/02 11:44:11  mark
 * *** empty log message ***
 * 
 * Revision 1.3  2005/08/24 12:23:20  juckel
 * added NUMBER_OF_JUMPS to PARAMETERS
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
