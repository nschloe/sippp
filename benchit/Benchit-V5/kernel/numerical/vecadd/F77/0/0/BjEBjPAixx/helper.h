/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.4 $
 *  $Date: 2006/01/30 13:54:58 $
 *******************************************************************/
#ifndef __helper_h
#define __helper_h

#include "work.h"

/*prototypes for helper.c*/
void deallocate( fds * pmem );
void allocateANDtouch( fds * pmem, int *pisize );
void entry_( void *ptr, int * pisize, int iunrolled );
double getseqentryoverhead( void *pmem );

extern void vecadd_ ( int *in, int *im, int *iunrolled, double *pda,
                      double *pdb, double *sum );

#endif
