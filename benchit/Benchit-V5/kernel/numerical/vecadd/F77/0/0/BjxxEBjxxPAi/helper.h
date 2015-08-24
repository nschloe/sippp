/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.3 $
 *  $Date: 2005/12/19 12:46:24 $
 *******************************************************************/
#ifndef __helper_h
#define __helper_h

#include "work.h"

/*prototypes for helper.c*/
void deallocate( fds * pmem );
void allocateANDtouch( fds * pmem, int *pisize );
void entry_( void *ptr, int *pisize, int iunrolled );
double getseqentryoverhead( void *pmem );

extern void vecadd_ ( int *in, int *im, int *iunrolled, double *pda,
                      double *pdb );
#endif
