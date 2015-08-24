/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: Matrix Multiply (C) integer
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: rschoene $
 * $Revision: 1.1 $
 * $Date: 2006/04/24 12:21:46 $
 *******************************************************************/

#include "stdio.h"
#include "stdlib.h"
#include <string.h>
#include "matmul.h"
#include "interface.h"

void multaijk_( int *a, int *b, int *c, int *size );
void multaikj_( int *a, int *b, int *c, int *size );
void multajik_( int *a, int *b, int *c, int *size );
void multajki_( int *a, int *b, int *c, int *size );
void multakji_( int *a, int *b, int *c, int *size );
void multakij_( int *a, int *b, int *c, int *size );

double getlanguage_( void );

void multaijk_( int *a, int *b, int *c, int *size )
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

void multaikj_( int *a, int *b, int *c, int *size )
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

void multajik_( int *a, int *b, int *c, int *size )
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

void multajki_( int *a, int *b, int *c, int *size )
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

void multakij_( int *a, int *b, int *c, int *size )
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

void multakji_( int *a, int *b, int *c, int *size )
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


/********************************************************************
 * Log-History
 * 
 * $Log: matmul_c_core.c,v $
 * Revision 1.1  2006/04/24 12:21:46  rschoene
 * initial commit matmul-c-int, based on matmul-c-double
 *
 * 
 *******************************************************************/
