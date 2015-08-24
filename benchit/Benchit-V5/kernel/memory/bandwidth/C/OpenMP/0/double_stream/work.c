/**************************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: measure Bandwidth inspired by STREAM benchmark (C OMP-version)
 *
 * according to the rules, reffer this Benchmark as:
 * "BenchIT kernel based on a variant of the STREAM benchmark code"
 * when publishing results
 *
 * This file contains the work, that is done: copy,scale,add and triad
 * 
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.2 $
 * $Date: 2007/02/13 13:04:18 $
 ************************************************************************/
#include "work.h" 
 
void copy_( double *a, double *b, int size)
{
	register int i=0;
	#pragma omp parallel for
	for (i=0;i<size;i++)
	{
		a[i]=b[i];
	}
}
void scale_( double *a, double *b, double scalar, int size)
{
	register int i=0;
	#pragma omp parallel for
	for (i=0;i<size;i++)
	{
		a[i]=scalar*b[i];
	}
}
void add_( double *a, double *b, double *c, int size)
{
	register int i=0;
	#pragma omp parallel for
	for (i=0;i<size;i++)
	{
		a[i]=b[i]+c[i];
	}
}
void triad_( double *a, double *b, double *c, double scalar, int size)
{
	register int i=0;
	#pragma omp parallel for
	for (i=0;i<size;i++)
	{
		a[i]=b[i]+scalar*c[i];
	}
}
 
 /********************************************************************
 * Log-History
 * 
 * $Log: work.c,v $
 * Revision 1.2  2007/02/13 13:04:18  rschoene
 * changed parameter b in copy to double*, added #include "work.h"
 *
 * Revision 1.1  2007/02/13 12:46:12  rschoene
 * STREAM Benchmar C + OpenMP
 *
 * 
 *******************************************************************/
