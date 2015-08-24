/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: pairwise Send/Recv between two MPI-Prozesses>
 *         the function to be measured>
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.2 $ 
 * $Date: 2006/01/13 10:24:58 $
 *******************************************************************/

#include <stdio.h>

#include "pingpong.h"

void pingpong(unsigned int *from, unsigned int *to, void *mdpv, unsigned long int * msgsize)
{
  /* cast void* pointer */
  mydata_t * pmydata = (mydata_t *) mdpv;
  MPI_Status status;
  unsigned int loop;
  
  IDL(3, printf("rank %d, inside pingpong\n", pmydata->commrank ));
  
  if( *msgsize == 0 ) 
      return;
  
  for( loop = 0; loop < pmydata->repeat; loop++)
  {
      if(pmydata->commrank == *from)
      {
        MPI_Send( pmydata->msg_string, *msgsize, MPI_BYTE, *to, 1, MPI_COMM_WORLD );
        MPI_Recv( pmydata->msg_string  , *msgsize, MPI_BYTE, *to, 1, MPI_COMM_WORLD, &status);
      }
      else if(pmydata->commrank == *to)
      {
        MPI_Recv( pmydata->msg_string, *msgsize, MPI_BYTE, *from, 1, MPI_COMM_WORLD, &status);
        MPI_Send( pmydata->msg_string, *msgsize, MPI_BYTE, *from, 1, MPI_COMM_WORLD);
      }
  }
}


/********************************************************************
 * Log-History
 * 
 * $Log: pingpong.c,v $
 * Revision 1.2  2006/01/13 10:24:58  william
 * first test successfully completed - minor fixes
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
