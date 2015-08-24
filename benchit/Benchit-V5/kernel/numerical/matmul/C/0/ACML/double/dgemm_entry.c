/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply, BLAS, ACML (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.5 $
 * $Date: 2005/12/15 09:28:28 $
 *******************************************************************/

#include <acml.h>
#include <stdio.h>
#include "dgemm.h"
#include "interface.h"

void init_data( fds *myfds, int size) {
	register int x, y;
	long index;
	for( x=0; x<size; x++)
		for( y=0; y<size; y++){
			index=x*size+y;
			myfds->feld1[index]=30;
			myfds->feld2[index]=0.01;
			myfds->feld3[index]=0.0;
		}
	IDL( 5, printf("init_data done\n"));
}


int bi_entry(void *mcb, int problemsize,double *results){
	double one=1.0;
	double time=0, start, stop;
	double nOperations, size;
	char N='N';
	double *f1= ((fds*)mcb)->feld1, *f2=((fds*)mcb)->feld2, *f3=((fds*)mcb)->feld3;
	
	if(results == NULL)
		return -1;
	
	size = bi_dgemm_start + (problemsize-1) * bi_dgemm_increment;
	results[0] = size;
	nOperations = (1.0*size)*(1.0*size)*(2.0*size-1.0);
	
	/* init matrices -> cache-friendly */
	init_data(mcb, size);
	
	/* ************************** */
	start=bi_timer();
	dgemm(N, N, size, size, size, 1.0, f1, size, f2, size, one, f3, size);
	stop=bi_timer();
	/* ************************** */
	
	time=stop-start - dTimerOverhead;
	if (time < 3*dTimerGranularity)   {
		results[1]=INVALID_MEASUREMENT;
	}
	else
		results[1]=nOperations/time;

	return 0;
}


/********************************************************************
 * Log-History
 *
 * $Log: dgemm_entry.c,v $
 * Revision 1.5  2005/12/15 09:28:28  hackenb
 * new variable names
 * modified/unified header and footer
 * MIN/MAX/INCREMENT iterating strategy
 *
 * 
 *******************************************************************/
