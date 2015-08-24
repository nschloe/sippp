/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, BLAS, MKL (C) - OpenMP version
 *         this kernel has been optimized to run with high
 *         performance on SGI Altix 3700 with more than 32 CPUs
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
	unsigned long size, optsize, diff;
	char N='N';
	double *f1, *f2, *f3;
	int ii, jj, kk, ompnumthreads;
	fds* pmydata;

	pmydata = (fds*)mcb;
        ompnumthreads = omp_get_max_threads();

	if(results == NULL)
		return -1;
	
	size = MIN + (problemsize-1) * INCREMENT;

	lCurrentSize = size*size*sizeof(double);

	pmydata->feld1=malloc(lCurrentSize);
	pmydata->feld2=malloc(lCurrentSize);
	pmydata->feld3=malloc(lCurrentSize);

	f1=pmydata->feld1; f2=pmydata->feld2; f3=pmydata->feld3;

	if( (f1==NULL) || (f2==NULL) || (f3==NULL) ) {
		printf("\nmalloc (%ld bytes) failed in bi_entry()\n",(long) (3.0*lCurrentSize)); 
		bi_cleanup(mcb);
		exit(127);
		}

	start=bi_gettime();
	init_data(mcb, size);
	stop=bi_gettime();
	time=stop-start - dTimerOverhead;

	/* ************************** */
	start=bi_gettime();
	diff = size % ompnumthreads;
	optsize = size - diff;

	cblas_dgemm(CblasRowMajor,CblasNoTrans,CblasNoTrans, optsize, optsize, size, 1.0, pmydata->feld1, size, pmydata->feld2, size, one, pmydata->feld3, size);

	if (diff > 0)
	{
		/*right part*/
		cblas_dgemm(CblasRowMajor,CblasNoTrans,CblasNoTrans, optsize, diff, size, 1.0, pmydata->feld1, size, &(pmydata->feld2[optsize]), size, one, &(pmydata->feld3[optsize]), size);
		/*bottom part*/
		cblas_dgemm(CblasRowMajor,CblasNoTrans,CblasNoTrans, diff, optsize, size, 1.0, &(pmydata->feld1[size*optsize]), size, pmydata->feld2, size, one, &(pmydata->feld3[size*optsize]), size);
		/*bottom right part*/
		cblas_dgemm(CblasRowMajor,CblasNoTrans,CblasNoTrans, diff, diff, size, 1.0, &(pmydata->feld1[size*optsize]), size, &(pmydata->feld2[optsize]), size, one, &(pmydata->feld3[size*optsize+optsize]), size);
	}

	stop=bi_gettime();

	results[0] = size;
	nOperations = (1.0*size)*(1.0*size)*(2.0*size-1.0);
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
