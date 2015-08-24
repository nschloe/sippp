/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <the headerfile>
*
*  Author: Thomas William (<william@zhr.tu-dresden.de>)
*
*  $Revision: 1.1 $
*  $Author: william $
*  $Date: 2006/12/14 12:54:15 $
*  $State: Exp $
*
******************************************************************************/

#ifndef __iowrite_h
#define __iowrite_h
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <unistd.h>


/*
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
*/

/****c* io_write_pthreads/types
 * SYNOPSIS
 * this kernel uses a set of selfdefined types.
 * follow the links for further information
 * on each of them
 ***/
 
 
/****f* types/environment_variables_t
 * SYNOPSIS 
 * this type holds all environment variables
 * SOURCE
 */
typedef struct 
	{
	  double FILESIZE;
	  int NUMCHANNELS;
	  int CHANNELFACTOR;
	  char * DISKPATH;
	  double DISKSPACE;
//	  double RAMSIZE;
	  int TIMELIMIT;
	} environment_variables_t;

/*******/	

/****f* types/backpack_t
 * SYNOPSIS 
 * holds all values needed throughout the measurement
 * this includes:
 *   - the environment variables
 *   - ...
 * this is the type of the bi_init-return-value
 * SOURCE
 */
typedef struct
	{
	  environment_variables_t * env_var;
	  char * filepath;
	  char * filebuffer;
	} backpack_t;
/*******/	

	
/****f* types/thread_arg_wrapper_t
 * SYNOPSIS 
 * this is the wrapper-type for the pthreadpool 
 * SOURCE
 */
typedef struct 
	{
	backpack_t * bp;
	unsigned long i;
	double * start_time;
	double * end_time;
	} thread_arg_wrapper_t;
/*******/	

/***********************************************************
* This is the complete log for this file:
*
* $Log: iowrite.h,v $
* Revision 1.1  2006/12/14 12:54:15  william
* changed the algorithm a bit for mor informative resultfilesmade chenges to reflect new format of COMPILE.SH and a like
*
* Revision 1.1.1.1  2006/04/18 10:03:50  william
* import version 0.1
*
* Revision 1.1.1.1  2004/12/14 21:22:56  william
* Release 3.0 - created new cvs-tree src2
*
*
***********************************************************/
