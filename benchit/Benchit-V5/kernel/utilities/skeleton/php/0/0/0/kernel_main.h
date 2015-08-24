/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: <description>
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: mueller $
 * $Revision: 1.1 $
 * $Date: 2006/05/22 10:12:43 $
 *******************************************************************/
 
#ifndef __kernelmain_h
#define __kernelmain_h

/** The data structure that holds all the data.
 *  Please use this construct instead of global variables.
 *  Global Variables seem to have large access times (?).
 */
typedef struct mydata
{
  /* measurement bounds */
  int min;
  int max;
  int logbase;
  int steps;
   
  /* gloabl enviroment */ 
  int num_processes;
  int num_threads; 
   
  /* script runtime specifics */
  char* interpreter;
  char* kerneldir;
  int min_runtime;
   
  /* database specific data */
  char* dbserver;
  char* dbname;
  char* dbuser;
  char* dbpass;
} mydata_t;

void evaluate_environment( mydata_t * pmydata );

#endif

/********************************************************************
 * Log-History
 * 
 * $Log: kernel_main.h,v $
 * Revision 1.1  2006/05/22 10:12:43  mueller
 * - adding php skeleton
 *
 * 
 *******************************************************************/
