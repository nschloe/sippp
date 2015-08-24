/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  c PAPI kernel skeleton 
*  This file: The header
*
*  Author: Robert Schoene (robert.schoene@zih.tu-dresden.de)
*  Last change by: $Author: mickler $
*  $Revision: 1.3 $
*  $Date: 2005/11/22 01:26:54 $
*
******************************************************************************/

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


/*****************************************************************************

LOG-History

$Log: work.h,v $
Revision 1.3  2005/11/22 01:26:54  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring

Revision 1.2  2005/11/09 12:07:00  rschoene
commenting ... bah

Revision 1.1  2005/11/09 09:07:37  rschoene
initial PAPI skeleton

*****************************************************************************/
