/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: c kernel skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.4 $
 * $Date: 2006/01/09 16:24:21 $
 *******************************************************************/

#ifndef __work_h
#define __work_h

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
/*
   <variabletype1> <variablename1>;
   <variabletype2> <variablename2>;
*/
} mydata_t;

extern void work_1( void );
extern void work_2( void );

#endif


/********************************************************************
 * Log-History
 *
 * $Log: work.h,v $
 * Revision 1.4  2006/01/09 16:24:21  william
 * updated the cvs-header
 *
 * Revision 1.3  2005/12/15 15:44:18  hackenb
 * modified/unified header and footer
 *
 * Revision 1.2  2005/11/22 01:26:53  mickler
 * + Using BI_GET_CALL_OVERHEAD_FUNC macro now
 * + Using bi_timer() function for time measuring
 * 
 *******************************************************************/
