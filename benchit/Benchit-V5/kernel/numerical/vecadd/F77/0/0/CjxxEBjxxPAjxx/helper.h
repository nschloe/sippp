/********************************************************************
 *  BenchIT - Performance Measurement for Scientific Applications
 *
 *  <Description>
 *
 *  Author: Thomas William (benchit@zih.tu-dresden.de)
 *  Last change by: $Author: william $
 *  $Revision: 1.3 $
 *  $Date: 2006/01/03 14:00:06 $
 *******************************************************************/
#ifndef __helper_h
#define __helper_h

#include "work.h"

/*prototypes for helper.c*/
double gettimeroverhead( void );
void deallocate( mydata_t * pmem );
void allocateANDtouch( mydata_t* pmem, int *pisize );
void entry_( void *ptr, int *pisize, int iunrolled );
double getseqentryoverhead( void *pmem );

extern void vecadd_ ( int *in, int *im, int *iunrolled, double *pda, double *pdb, double *pdc );
#endif
