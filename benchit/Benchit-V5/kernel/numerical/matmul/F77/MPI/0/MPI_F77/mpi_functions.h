#include "matmul.h"

#ifndef BENCHIT_MPI_FUNCTIONS_F77_H
#define BENCHIT_MPI_FUNCTIONS_F77_H

/* some Fortran compilers export symbols in s special way:
 * all letter are big letters
 */
#if (defined (_CRAY)    || \
     defined (_SR8000)  || \
     defined (_SX)      || \
     defined (_USE_OLD_STYLE_CRAY_TYPE))
#define mpivariables_  	        MPIVARIABLES
#define mpibinarybroadcast_	MPIBINARYBROADCAST
#define mpimatrixgather_	MPIMATRIXGATHER
#define mpimatrixscatter_	MPIMATRIXSCATTER
#define mpishiftmatrix_         MPISHIFTMATRIX
#define mpimatrixbroadcast_     MPIMATRIXBROADCAST
#endif

void mpivariables_(void);
void mpibinarybroadcast_( char *p, myinttype *bytes);
void mpimatrixbroadcast_( double *n, myinttype *rows, myinttype *cols);
void mpimatrixscatter_( double *a, double *b, myinttype *sizeall, 
			myinttype *sizeone);
void mpimatrixgather_( double *a, double *b, myinttype *sizeall, 
		       myinttype *sizeone);
void mpishiftmatrix_( double *n, myinttype *rows, myinttype *cols);
myinttype childs_( myinttype *id, myinttype *size);

#endif /* BENCHIT_MPI_FUNCTIONS_F77_H */

/*****************************************************************************

LOG-History

$Log: mpi_functions.h,v $
Revision 1.1  2005/12/06 15:48:23  william
created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names


******************************************************************************/

