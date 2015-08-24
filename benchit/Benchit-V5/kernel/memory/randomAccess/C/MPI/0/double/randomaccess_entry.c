/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: memory bandwith w. random access
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.4 $
 * $Date: 2006/10/10 14:58:25 $
 *******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include "interface.h"

extern long N_ACCESSES;
extern long BENCHIT_KERNEL_RANDOMACCESS_MEMSIZE;
extern double* a;
extern double* b;
extern long* j;

double r=1.0;

extern int mpiRank, mpiSize;
	
double simple_rand();

extern double* dRand;
	
void mix_index(double randomness) {
	double r;
	long i;
	
	for(i = 0 ; i < N_ACCESSES; i++) {
		if (simple_rand() >= randomness)
			j[i]=i;
		else {
			r = simple_rand() * 2.0 * (double)N_ACCESSES;
			j[i] = (long) r;
		}
	}
}
int bi_entry(void *mcb, int problemsize,double *results){
	int i;
	double start, stop;
	double r = 2.1;
	double randomness; 
	initArays();
	
	randomness = dRand[problemsize-1];
	mix_index(randomness);
	
	MPI_Barrier(MPI_COMM_WORLD);
	if(mpiRank == 0) {
		start = bi_timer();
	}
	MPI_Barrier(MPI_COMM_WORLD);
		
	for(i=0;i<N_ACCESSES;i++)
		r = r + a[j[i]];
		
	MPI_Barrier(MPI_COMM_WORLD);	
	if (mpiRank==0) {
		stop=bi_timer();
	}
	
	if (r < N_ACCESSES/3)
		printf("<");	
	if (results) {
		results[0] = randomness;
		results[1] = ((double)(mpiSize*N_ACCESSES*(1*sizeof(double)+sizeof(long))))/(stop-start);
	}
	return 0;
}

/*
double streaming(){
	int i;
	double start=0.0, stop=1.0E+9;
	double r;
		
	r = 1.1;
	initArays();
	
	MPI_Barrier(MPI_COMM_WORLD);

	if(mpiRank == 0)
		start = MPI_Wtime();
	
	MPI_Barrier(MPI_COMM_WORLD);
			
	for(i=0;i<N_ACCESSES;i++)
		r = r + b[i];
		
	MPI_Barrier(MPI_COMM_WORLD);
	
	if (mpiRank==0)
		stop=MPI_Wtime();
	
	if (r < N_ACCESSES/3)
		printf("<");
		
	return stop-start;
}
*/


/********************************************************************
 * Log-History
 * 
 * $Log: randomaccess_entry.c,v $
 * Revision 1.4  2006/10/10 14:58:25  rschoene
 * changed defines to environment-variables to use bi_getenv, also new: number
 * of measurements
 *
 * Revision 1.3  2006/01/03 18:25:08  hackenb
 * modified/unified header and footer
 * new interface
 * now using bi_gettime()
 *
 * 
 *******************************************************************/
