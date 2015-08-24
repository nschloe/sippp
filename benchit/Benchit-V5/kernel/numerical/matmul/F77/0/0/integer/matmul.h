/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply in F77 with integers
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.1 $
 * $Date: 2006/04/24 11:16:11 $
 *******************************************************************/

#ifndef BENCHIT_MATMUL_H
#define BENCHIT_MATMUL_H

/* some Fortran compilers export symbols in s special way:
 * all letter are big letters
 */
/*     defined (_SX)      || \   (the SX does this not any longer)*/
#if (defined (_CRAY)    || \
     defined (_SR8000)  || \
     defined (_USE_OLD_STYLE_CRAY_TYPE))
#define multaijk_	MULTAIJK
#define multaikj_	MULTAIKJ
#define multajik_	MULTAJIK
#define multajki_	MULTAJKI
#define multakij_	MULTAKIJ
#define multakji_	MULTAKJI
#endif

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif

extern void multaijk_( myinttype *a, myinttype *b, myinttype *c, myinttype *size);
extern void multaikj_( myinttype *a, myinttype *b, myinttype *c, myinttype *size);
extern void multajik_( myinttype *a, myinttype *b, myinttype *c, myinttype *size);
extern void multajki_( myinttype *a, myinttype *b, myinttype *c, myinttype *size);
extern void multakij_( myinttype *a, myinttype *b, myinttype *c, myinttype *size);
extern void multakji_( myinttype *a, myinttype *b, myinttype *c, myinttype *size);

#endif /* BENCHIT_MATMUL_H */

/********************************************************************
 * Log-History
 * 
 * $Log: matmul.h,v $
 * Revision 1.1  2006/04/24 11:16:11  rschoene
 * initial commit for matmul-f77-int (mostly taken from matmul-f77-double)
 *
 * 
 *******************************************************************/
