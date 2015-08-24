/***********************************************************************
 *
 * B e n c h I T - Performance Measurement for Scientific Applications
 *
 * implemented Benchit Interface for Memory Access Time (C)
 * Author: Michael Kluge (kluge@zhr.tu-dresden.de)
 *
 * $Revision: 1.3 $
 * $Date: 2005/11/22 01:26:51 $
 * $State: Exp $
 *
 ***********************************************************************/

#include "interface.h"
#include "pointerchasing.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <perfmon/pfmlib.h>

#include <perfmon/perfmon.h>
#include <perfmon/pfmlib_itanium2.h>


#define MAX_EVT_NAME_LEN        256
#define NUM_PMCS PFMLIB_MAX_PMCS
#define NUM_PMDS PFMLIB_MAX_PMDS

#ifndef MIN_ACCESS_LENGTH
#define MIN_ACCESS_LENGTH (2048)
#endif

#ifndef MAX_ACCESS_LENGTH
#define MAX_ACCESS_LENGTH (1024*1024)
#endif

#ifndef ACCESS_STEPS
#define ACCESS_STEPS (100)
#endif

#ifndef NUMBER_OF_JUMPS
#define NUMBER_OF_JUMPS (4000000)
#endif


unsigned int random_number( unsigned long max);
void make_linked_memory( void *mem, long count);
void init_global_vars(void);

long minlength, maxlength, accessstride, numjumps;
double dMemFactor;
long nMeasurements;

int NUM_COUNTERS;
char sCounters[10][100];


void bi_getinfo(bi_info* infostruct){
  int i, l;
  char buf[200], *s;
  int events[10];

  init_global_vars();

	
  /*infostruct->kernelstring=bi_strdup("Random Memory Access");*/
  infostruct->codesequence=bi_strdup("for i=1,N#  var=memory[random(0..size)]#");
  infostruct->xaxistext=bi_strdup("Accessed Memory in Byte");
  
  infostruct->numfunctions= 1+ NUM_COUNTERS;
  infostruct->maxproblemsize=nMeasurements;
  infostruct->outlier_direction_upwards=malloc(infostruct->numfunctions*sizeof(int));
  
  for (i=0; i< infostruct->numfunctions; i++)
  		infostruct->outlier_direction_upwards[i]=1;
		
  infostruct->log_xaxis=1;
  infostruct->base_xaxis=2.0;
  infostruct->log_yaxis=malloc(infostruct->numfunctions*sizeof(int));
  infostruct->log_yaxis[0]=0;
  infostruct->base_yaxis=malloc(infostruct->numfunctions*sizeof(double));
  infostruct->base_yaxis[0]=0.0;
  infostruct->legendtexts=malloc(infostruct->numfunctions*sizeof(char*));
  
	if (pfm_initialize() != PFMLIB_SUCCESS) {
		fprintf(stderr, "cannot initialize libpfm\n");
		exit(0);
	}

 	for (i=0; i< NUM_COUNTERS; i++) {
  
  
  infostruct->yaxistexts=malloc(infostruct->numfunctions*sizeof(char*));
  if (infostruct->legendtexts==0){
    printf("No more core\n");
    exit(127);
  }
  infostruct->yaxistexts[0]=bi_strdup("s");
  infostruct->yaxistexts[1]=bi_strdup("");

}

void init_global_vars() {
    
	char *envir, *p, *q;
	int i;

	IDL(3,printf("Init global variables ... "));
	envir=bi_getenv("MIN_ACCESS_LENGTH",1);
	minlength=(envir != NULL) ? 1024*atoi(envir) : MIN_ACCESS_LENGTH;
	if(minlength==0) {
		minlength=MIN_ACCESS_LENGTH;
	}
	envir=NULL;
	envir=bi_getenv("MAX_ACCESS_LENGTH",1);
	maxlength=(envir != NULL) ? 1024*atoi(envir) : MAX_ACCESS_LENGTH;
	if(maxlength==0) {
		maxlength=MIN_ACCESS_LENGTH;
	}
	envir=NULL;
	envir=bi_getenv("ACCESS_INCREMENT",0); /*in kB */
	if (envir != NULL) {
		i = atoi(envir);
		nMeasurements = (maxlength-minlength)/(i*1024);
		dMemFactor = -i;	/* if <0 : linear measurement */
	}
	else {
		envir=bi_getenv("ACCESS_STEPS",1);
		if (envir != NULL) {
			nMeasurements = (envir != 0) ? atoi(envir) : ACCESS_STEPS;
			dMemFactor =((double)maxlength)/((double)minlength);
			dMemFactor = pow(dMemFactor, 1.0/((double)nMeasurements-1));
		}
	}		
		
	envir=NULL;
	envir=bi_getenv("NUMBER_OF_JUMPS",1);
	numjumps=(envir != NULL) ? atoi(envir) : NUMBER_OF_JUMPS;
	if(numjumps==0) {
		numjumps=NUMBER_OF_JUMPS;
	}
	envir = bi_getenv("PAPI_COUNTERS",1);
	p = envir;
	NUM_COUNTERS = 0;
	while (p) {
		p = strchr(p, ',');
		if (p) p++; 
		NUM_COUNTERS++;
	}
	/*sCounters = malloc(sizeof(char*) * NUM_COUNTERS);*/
	i = 0;
	p = envir;
	for (i=0; i<NUM_COUNTERS; i++) {	
		q = strchr(p, ',');
		if (q) {		
			strncpy(sCounters[i], p, (int)(q-p));
			sCounters[i][q-p+1]=0;
			p = ++q;
		}	
		else
			strcpy(sCounters[i], p);	
	}
	IDL(3,printf("done\n"));
}

BI_GET_CALL_OVERHEAD_FUNC((),jump_around(NULL,0));


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

/*****************************************************************************

LOG-History

$Log: pointerchasing_init.c,v $
Revision 1.3  2005/11/22 01:26:51  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring

Revision 1.2  2005/09/16 06:08:14  mark
*** empty log message ***

Revision 1.2  2005/09/13 12:49:26  mark
PAPI counter in PARAMETERS file
near stepsize

Revision 1.1  2005/09/02 12:14:47  mark
*** empty log message ***

Revision 1.1  2005/09/02 11:44:11  mark
*** empty log message ***

Revision 1.3  2005/08/24 12:23:20  juckel
added NUMBER_OF_JUMPS to PARAMETERS

Revision 1.2  2005/08/16 16:20:02  juckel
some bugfixing

Revision 1.1  2005/08/16 14:20:53  juckel
- included kernel from old benchit into new structure
- changed MPI-functionality
  old: only root process runs memory test -> generates 1 data set
  new: for i=1 to numproc do ruu mesurement -> generates numproc data sets

Revision 2.2  2004/11/03 21:40:14  kluge
forgot the /2

Revision 2.1  2004/09/23 15:58:29  juckel
...

Revision 2.0  2003/12/09 11:19:00  juckel
build of version 2.0

Revision 1.2  2003/12/04 09:56:09  kluge
memacces has now a README and is ready to be used

Revision 1.1  2003/12/01 20:52:03  kluge
memaccess_c initial checkin


*****************************************************************************/
