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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <interface.h>
#include <omp.h>
#include <dgemm.h>

void bi_getinfo(bi_info* infostruct) {
	MIN = atol(bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_MIN",1));
	MAX = atol(bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_MAX",1));
	INCREMENT = atol(bi_getenv("BENCHIT_KERNEL_PROBLEMSIZE_INCREMENT",1));
	/* TODO: FEHLER abfragen */

	infostruct->kerneldescription = bi_strdup( "Matrix Multiply, BLAS, MKL (C) - OpenMP version for SGI Altix" );	
	infostruct->codesequence=bi_strdup("DGEMM");
	infostruct->xaxistext=bi_strdup("Matrix Size");
	infostruct->maxproblemsize = (MAX-MIN+1)/INCREMENT;
	if( (MAX-MIN+1) % INCREMENT != 0 ) infostruct->maxproblemsize++;
  
	infostruct->yaxistexts = (char **) malloc (sizeof (char *) *infostruct->numfunctions);
	infostruct->yaxistexts[0] = bi_strdup ("FLOPS");
  
	infostruct->outlier_direction_upwards = malloc(sizeof(int) * infostruct->numfunctions);
	infostruct->outlier_direction_upwards[0]=0;

	infostruct->legendtexts=(char**) malloc( sizeof(char*)*infostruct->numfunctions );
	infostruct->legendtexts[0]=bi_strdup("FLOPS");
  
	infostruct->base_yaxis = malloc(sizeof(double)*infostruct->numfunctions);
	infostruct->base_yaxis[0] = 0;

	infostruct->num_processes = 1;
	infostruct->num_threads_per_process = atol(bi_getenv("BENCHIT_NUM_CPUS",1));
	infostruct->kernel_execs_mpi1 = 0;
	infostruct->kernel_execs_mpi2 = 0;
	infostruct->kernel_execs_pvm = 0;
	infostruct->kernel_execs_omp = 1;
	infostruct->kernel_execs_pthreads = 0;
	infostruct->numfunctions = 1;
}

void* bi_init(int problemsizemax) {

	fds *myfds;
	long lMaxSize;

	IDL( 3, printf("Enter init\n"));
	myfds=malloc(sizeof(fds));
	if(myfds==NULL) {
		printf("Allocation of structure myfds failed\n");
		exit(127);
	}

	return (myfds);
}

extern void bi_cleanup(void *mcb) {
	fds *data=mcb;
	free(data);
}


/********************************************************************
 * Log-History
 *
 * $Log: dgemm_init.c,v $
 * Revision 1.1  2006/07/19 12:10:58  hackenb
 * + initial commit
 *
 * 
 *******************************************************************/
