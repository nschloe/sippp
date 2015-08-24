/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  c pcl kernel skeleton
*  this file: the algorithm header file
*
*  Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*  Last change by: $Author: mickler $
*  $Revision: 1.2 $
*  $Date: 2005/11/22 01:26:54 $
*
******************************************************************************/

#ifndef __work_h
#define __work_h

#ifdef BENCHIT_USE_PCL
#include <pcl.h>
#endif

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif

/** The data structure that holds all the data.
 *  Please use this construct instead of global variables.
 *  Global Variables seem to have large access times (?).
 */
typedef struct mydata
{
   myinttype dummy;
#ifdef BENCHIT_USE_PCL
   int *doable_list;
   int doable_nevents;
   unsigned int mode;
   PCL_DESCR_TYPE descr;
   PCL_CNT_TYPE * i_result;
   PCL_FP_CNT_TYPE * fp_result;
#endif
/*
   <variabletype1> <variablename1>;
   <variabletype2> <variablename2>;
*/
} mydata_t;

extern void work_1( void );
extern void work_2( void );

#endif

/*****************************************************************************

LOG-History

$Log: work.h,v $
Revision 1.2  2005/11/22 01:26:54  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring


*****************************************************************************/

