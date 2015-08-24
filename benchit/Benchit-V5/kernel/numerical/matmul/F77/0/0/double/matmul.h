/***********************************************************************
*
* �B e n c h I T - Performance Measurement for Scientific Applications
*
* �Header file for Matrix Multiply (Fortran 77)
* �Author: Michael Kluge (kluge@zhr.tu-dresden.de)
*
* �$Revision: 1.1 $
* �$Date: 2005/08/20 15:48:22 $
* �$State: Exp $
*
***********************************************************************/

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

extern void multaijk_( double *a, double *b, double *c, myinttype *size);
extern void multaikj_( double *a, double *b, double *c, myinttype *size);
extern void multajik_( double *a, double *b, double *c, myinttype *size);
extern void multajki_( double *a, double *b, double *c, myinttype *size);
extern void multakij_( double *a, double *b, double *c, myinttype *size);
extern void multakji_( double *a, double *b, double *c, myinttype *size);

#endif /* BENCHIT_MATMUL_H */

/*****************************************************************************

LOG-History

$Log: matmul.h,v $
Revision 1.1  2005/08/20 15:48:22  rschoene
Matrix Multiply for F77 checkin
(merged from src3-matmulc and src2-matmul-f77)

Revision 1.1.1.1  2004/12/14 21:22:57  william
Release 3.0 - created new cvs-tree src2

Revision 2.0  2003/12/09 11:18:57  juckel
build of version 2.0

Revision 1.7  2003/11/11 12:53:51  kluge
update of comments

Revision 1.6  2003/10/16 08:31:54  kluge
added support for benchit shell, measurment range now editable via COMPILE script

Revision 1.5  2003/09/23 07:42:24  kluge
moved old line for sx

Revision 1.4  2003/07/18 11:41:05  kluge
neues Verzeichnis membits_c

Revision 1.3  2003/06/16 09:40:59  kluge
added ./ and changed Flops to Ops

Revision 1.2  2003/01/28 11:44:42  kluge
new header



*****************************************************************************/
