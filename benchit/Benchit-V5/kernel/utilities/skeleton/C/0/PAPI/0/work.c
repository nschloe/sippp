/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  c PAPI kernel skeleton 
*  This file: The algorithm
*
*  Author: Robert Schoene (robert.schoene@zih.tu-dresden.de)
*  Last change by: $Author: mickler $
*  $Revision: 1.3 $
*  $Date: 2005/11/22 01:26:54 $
*
******************************************************************************/

#include "work.h"
#include "interface.h"

void work_1( void )
{
}

void work_2( void )
{
}

/*****************************************************************************

LOG-History

$Log: work.c,v $
Revision 1.3  2005/11/22 01:26:54  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring

Revision 1.2  2005/11/09 12:07:00  rschoene
commenting ... bah

Revision 1.1  2005/11/09 09:07:37  rschoene
initial PAPI skeleton

*****************************************************************************/
