/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, BLAS, ESSL (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.1 $
 * $Date: 2006/10/11 11:41:38 $
 *******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "interface.h"
#include "dgemm.h"

void bi_getinfo(bi_info* infostruct) {
	bi_dgemm_start = atol(bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_MIN",1));
	bi_dgemm_stop  = atol(bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_MAX",1));
	bi_dgemm_increment = atol(bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT",1));
	/* TODO: FEHLER abfragen */
	
	infostruct->codesequence=bi_strdup("DGEMM");
	infostruct->xaxistext=bi_strdup("Matrix Size");
	infostruct->maxproblemsize = (bi_dgemm_stop-bi_dgemm_start+1)/bi_dgemm_increment;
	if((bi_dgemm_stop-bi_dgemm_start+1) % bi_dgemm_increment != 0) infostruct->maxproblemsize++;
	infostruct->numfunctions=1;
  
	infostruct->yaxistexts = (char **) malloc (sizeof (char *) *infostruct->numfunctions);
	infostruct->yaxistexts[0] = bi_strdup ("FLOPS");
  
	infostruct->outlier_direction_upwards = malloc(sizeof(int) * infostruct->numfunctions);
	infostruct->outlier_direction_upwards[0]=0;

	infostruct->legendtexts=(char**) malloc( sizeof(char*)*infostruct->numfunctions );
	infostruct->legendtexts[0]=bi_strdup("FLOPS");
  
	infostruct->base_yaxis = malloc(sizeof(double)*infostruct->numfunctions);
	infostruct->base_yaxis[0] = 0;

	infostruct->kerneldescription = bi_strdup( "Matrix Multiply, BLAS, ESSL (C)" );
}

void* bi_init(int problemsizemax) {

	fds *myfds;
	long lMaxSize;

	/* calculate real maximum problem size
	   problemsizemax might be smaller then BENCHIT_KERNEL_PROBLEMSIZE_MAX
	   if BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT is greater then one */

	problemsizemax = bi_dgemm_start + (problemsizemax-1) * bi_dgemm_increment;;
  
	lMaxSize = problemsizemax*problemsizemax*sizeof(double);

	IDL( 3, printf("Enter init\n"));
	myfds=malloc(sizeof(fds));
	if(myfds==NULL) {
		printf("Allocation of structure myfds failed\n");
		exit(127);
	}

	myfds->feld1=malloc(lMaxSize);
	myfds->feld2=malloc(lMaxSize);
	myfds->feld3=malloc(lMaxSize);

	if( (myfds->feld1==NULL) || (myfds->feld2==NULL) || (myfds->feld3==NULL) ) {
		printf("\nmalloc (%ld bytes) failed in bi_init()\n",(long) (3.0*lMaxSize)); 
		bi_cleanup( myfds);
		exit(127);
		}
	IDL( 3, printf("Alloc done %ld Bytes\n", 3*lMaxSize))
	return (myfds);
}

extern void bi_cleanup(void *mcb) {
	fds *data=mcb;
	IDL( 3, printf("cleaning..."));
	if(data!=NULL) {
		IDL( 3, printf("1"));
		if(data->feld1!=NULL) {
		free(data->feld1);
		data->feld1=NULL;
	}
	IDL( 3, printf("2"));
	if(data->feld2!=NULL) {
		free(data->feld2);
		data->feld2=NULL;
	}
	IDL( 3, printf("3"));
	if(data->feld3!=NULL) {
		free(data->feld3);
		data->feld3=NULL;
	}
	IDL( 3, printf("4\n"));
	free(data);
	}
}


/********************************************************************
 * Log-History
 *
 * $Log: dgemm_init.c,v $
 * Revision 1.1  2006/10/11 11:41:38  hackenb
 * initial commit
 *
 *
 * 
 *******************************************************************/
