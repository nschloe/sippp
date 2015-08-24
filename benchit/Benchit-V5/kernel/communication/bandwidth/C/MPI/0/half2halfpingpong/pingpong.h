/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: pairwise Send/Recv between two MPI-Prozesses
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.2 $
 * $Date: 2006/01/13 10:24:59 $ 
 *******************************************************************/

#ifndef __pingpong_h
#define __pingpong_h

#include "interface.h"
#include "mpi.h"

/** The data structure that holds all the data.
 *  Please use this construct instead of global variables.
 *  Global Variables seem to have large access times (?).
 BENCHIT_KERNEL_REPETITIONS=1
 BENCHIT_KERNEL_SHOW_PAIR_BANDWITH="1"
 BENCHIT_KERNEL_SHOW_TOTAL_BANDWITH=1
 BENCHIT_KERNEL_MIN_MSG_SIZE=$((1024*1024*1))
 BENCHIT_KERNEL_MAX_MSG_SIZE=$((1024*1024*1024))
 BENCHIT_KERNEL_MSG_SIZE_INCREMENT=$((1024*1024*1))
 BENCHIT_KERNEL_SENDERLIST=""
 BENCHIT_KERNEL_RECEIVERLIST=""
 */
typedef struct mydata
{
   int commsize;
   int commrank;
   unsigned int repeat;
   unsigned int steps;
   unsigned long int pair_bandwith;
   unsigned long int total_bandwith;
   unsigned long int min_msg_size;
   unsigned long int max_msg_size;
   unsigned long int msg_size_increment;
   unsigned char empty_list;
   unsigned int * senderlist;
   unsigned int * receiverlist;
   unsigned int * msg_string;
} mydata_t;

extern void pingpong( unsigned int *from, unsigned int *to, void *mdpv, unsigned long int * msgsize );

#endif


/********************************************************************
 * Log-History
 * 
 * $Log: pingpong.h,v $
 * Revision 1.2  2006/01/13 10:24:59  william
 * first test successfully completed - minor fixes
 *
 * Revision 1.1  2006/01/12 11:30:58  william
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
