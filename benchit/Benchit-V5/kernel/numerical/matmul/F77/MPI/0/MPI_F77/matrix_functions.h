#include "matmul.h"

#ifndef BENCHIT_HAVE_MATRIX_FUNCTIONS_H
#define BENCHIT_HAVE_MATRIX_FUNCTIONS_H

void matrixinit_( double *a, double *b, myinttype *sizeall, 
		  myinttype *sizeone);
void matrixprint_( double *a, myinttype *rows, myinttype *cols);
void matrixfill_(double *a, myinttype *rows, myinttype *cols, double *value);
void matmul_(double *a, double *b, double *c, myinttype *rowsa, 
	     myinttype *colsa, myinttype *rowsb, myinttype *colsb);

#endif /* BENCHIT_HAVE_MATRIX_FUNCTIONS_H */

/*****************************************************************************

LOG-History

$Log: matrix_functions.h,v $
Revision 1.1  2005/12/06 15:48:23  william
created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names


******************************************************************************/
