/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply (C)
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.3 $
 * $Date: 2005/12/15 09:28:29 $
 *******************************************************************/

#include "stdio.h"
#include "stdlib.h"
#include <string.h>
#include "matmul.h"
#include "interface.h"

void multaijk_( double *a, double *b, double *c, int *size );
void multaikj_( double *a, double *b, double *c, int *size );
void multajik_( double *a, double *b, double *c, int *size );
void multajki_( double *a, double *b, double *c, int *size );
void multakji_( double *a, double *b, double *c, int *size );
void multakij_( double *a, double *b, double *c, int *size );

double getlanguage_( void );

void multaijk_( double *a, double *b, double *c, int *size )
{
  int i, j, k;
  int s = *size;
  for ( i = 0; i < s; i++ )
    for ( j = 0; j < s; j++ )
      for ( k = 0; k < s; k++ )
      {
        c[ i * s + j ] = c[ i * s + j ] + a[ i * s + k ] * b[ k * s + j ];
      }
}

void multaikj_( double *a, double *b, double *c, int *size )
{
  int i, j, k;
  int s = *size;
  for ( i = 0; i < s; i++ )
    for ( k = 0; k < s; k++ )
      for ( j = 0; j < s; j++ )
      {
        c[ i * s + j ] = c[ i * s + j ] + a[ i * s + k ] * b[ k * s + j ];
      }
}

void multajik_( double *a, double *b, double *c, int *size )
{
  int i, j, k;
  int s = *size;
  for ( j = 0; j < s; j++ )
    for ( i = 0; i < s; i++ )
      for ( k = 0; k < s; k++ )
      {
        c[ i * s + j ] = c[ i * s + j ] + a[ i * s + k ] * b[ k * s + j ];
      }
}

void multajki_( double *a, double *b, double *c, int *size )
{
  int i, j, k;
  int s = *size;
  for ( j = 0; j < s; j++ )
    for ( k = 0; k < s; k++ )
      for ( i = 0; i < s; i++ )
      {
        c[ i * s + j ] = c[ i * s + j ] + a[ i * s + k ] * b[ k * s + j ];
      }
}

void multakij_( double *a, double *b, double *c, int *size )
{
  int i, j, k;
  int s = *size;
  for ( k = 0; k < s; k++ )
    for ( i = 0; i < s; i++ )
      for ( j = 0; j < s; j++ )
      {
        c[ i * s + j ] = c[ i * s + j ] + a[ i * s + k ] * b[ k * s + j ];
      }
}

void multakji_( double *a, double *b, double *c, int *size )
{
  int i, j, k;
  int s = *size;
  for ( k = 0; k < s; k++ )
    for ( j = 0; j < s; j++ )
      for ( i = 0; i < s; i++ )
      {
        c[ i * s + j ] = c[ i * s + j ] + a[ i * s + k ] * b[ k * s + j ];
      }
}

double getlanguage_()
{
  return 1.0;
}


/*****************************************************************************
LOG-History
 
$Log: matmul_c_core.c,v $
Revision 1.3  2005/12/15 09:28:29  hackenb
new variable names
modified/unified header and footer
MIN/MAX/INCREMENT iterating strategy

Revision 1.2  2005/11/22 01:26:52  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring

Revision 1.1  2005/07/20 21:38:26  mickler
# Added /numerical/matmul/C/0/0/double/ kernel

Revision 1.1.1.1  2004/12/14 21:22:57  william
Release 3.0 - created new cvs-tree src2
 
Revision 2.0  2003/12/09 11:18:56  juckel
build of version 2.0
 
Revision 1.4  2003/05/23 10:10:07  kluge
another index update
 
Revision 1.3  2003/05/23 09:15:09  kluge
bug removed: wrong index while accessing matrix
 
Revision 1.2  2003/01/28 11:41:13  kluge
new header
 
*****************************************************************************/
