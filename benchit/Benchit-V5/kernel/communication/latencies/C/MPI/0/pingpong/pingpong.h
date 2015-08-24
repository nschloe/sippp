/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: pairwise Send/Recv between two MPI-Prozesses
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.2 $
 * $Date: 2006/01/12 11:34:05 $
 *******************************************************************/

#ifndef __pingpong_h
#define __pingpong_h

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif

#include "interface.h"
#include "mpi.h"

/** The data structure that holds all the data.
 *  Please use this construct instead of global variables.
 *  Global Variables seem to have large access times (?).
 */
typedef struct mydata
{
   myinttype commsize;
   myinttype commrank;
   myinttype msgsize;
   myinttype repeat;
   myinttype pairs;
   myinttype * buffer;
} mydata_t;

extern void pingpong( int *from, int *to, void *mdpv );

#endif


/********************************************************************
 * Log-History
 * 
 * $Log: pingpong.h,v $
 * Revision 1.2  2006/01/12 11:34:05  william
 * cvs-keyword-problems with binary/ASCII flags
 *
 * Revision 1.1  2006/01/12 11:30:57  william
 * checked in new kernel-tree "communication"
 *
 * Revision 1.4  2006/01/09 16:24:21  william
 * updated the cvs-header
 *
 * Revision 1.3  2006/01/09 15:54:09  william
 * cvs-keyword-problems
 *
 * Revision 1.2  2006/01/09 11:31:00  william
 * filled in infos for the cvs-header and deleted falsly inserted files
 *
 * Revision 1.1  2006/01/07 22:31:17  william
 * pairwise pingpong all-to-all with minimal message-size => latency-benchmark
 *
 * 
 *******************************************************************/
