/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, BLAS, MKL (C) - OpenMP version
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.1 $
 * $Date: 2006/07/19 12:10:58 $
 *******************************************************************/

#include <mkl_cblas.h>
#include <stdio.h>
#include <dgemm.h>
#include <interface.h>
#include <omp.h>

void init_data(fds *myfds, int size) {
	long x, index, max;
  #pragma omp parallel for schedule(static,1) private(x,index,max) shared(myfds,size)
	for(x = 0; x < size; x++) {
		index = x * size;
		max = index + size;
		for(index; index < max; index++) {
			myfds->feld1[index] = 30.0;
			myfds->feld2[index] = 0.01;
			myfds->feld3[index] = 0.0;
		}
	}
	IDL(5, printf("init_data done\n"));
}


int bi_entry(void *mcb, int problemsize,double *results){
	double one=1.0;
	double time=0, start, stop;
	double nOperations=0.0;
	long lCurrentSize;
	unsigned long size;
	char N='N';
	double *f1, *f2, *f3;
	int ii, jj;
	double dummy = 0.0;

	if(results == NULL)
		return -1;

	size = MIN + (problemsize-1) * INCREMENT;
	results[0] = size;
	nOperations = (1.0*size)*(1.0*size)*(2.0*size-1.0);
	
	lCurrentSize = size*size*sizeof(double);

	((fds*)mcb)->feld1=malloc(lCurrentSize);
	((fds*)mcb)->feld2=malloc(lCurrentSize);
	((fds*)mcb)->feld3=malloc(lCurrentSize);

	f1=((fds*)mcb)->feld1; f2=((fds*)mcb)->feld2; f3=((fds*)mcb)->feld3;

	if( (f1==NULL) || (f2==NULL) || (f3==NULL) ) {
		printf("\nmalloc (%ld bytes) failed in bi_entry()\n",(long) (3.0*lCurrentSize)); 
		bi_cleanup(mcb);
		exit(127);
		}

	init_data(mcb, size);

	/* ************************** */
	start=bi_gettime();
	cblas_dgemm(CblasRowMajor,CblasNoTrans,CblasNoTrans, size, size, size, 1.0, f1, size, f2, size, one, f3, size);
	stop=bi_gettime();
	/* ************************** */

	time=stop-start - dTimerOverhead;
	if (time < 3*dTimerGranularity)   {
		results[1]=INVALID_MEASUREMENT;
	}
	else {
		results[1]=nOperations/time;
	}

	if(mcb!=NULL) {
		if(f1!=NULL) {
			free(f1);
			f1=NULL;
		}
		if(f2!=NULL) {
			free(f2);
			f2=NULL;
		}
		if(f3!=NULL) {
			free(f3);
			f3=NULL;
		}
	}

	return 0;
}


/********************************************************************
 * Log-History
 *
 * $Log: dgemm_entry.c,v $
 * Revision 1.1  2006/07/19 12:10:58  hackenb
 * + initial commit
 *
 * 
 *******************************************************************/
