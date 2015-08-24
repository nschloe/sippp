#ifndef BENCHIT_MATMUL_H
#define BENCHIT_MATMUL_H

/* some Fortran compilers export symbols in s special way:
 * all letter are big letters
 */
/*     defined (_SX)      || \ */
#if (defined (_CRAY)    || \
     defined (_SR8000)  || \
     defined (_USE_OLD_STYLE_CRAY_TYPE))
#define matmul_	MATMUL
#endif

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif

#endif /* BENCHIT_MATMUL_H */

/*****************************************************************************

LOG-History

$Log: matmul.h,v $
Revision 1.1  2005/12/06 15:48:22  william
created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names


*****************************************************************************/
