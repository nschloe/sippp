/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
*
* Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*
* $Revision: 1.5 $
* $Date: 2007/01/19 15:05:11 $
* $State: Exp $
*
***********************************************************************/
#ifndef BIENVHASH_H
#define BIENVHASH_H

#include "stringlib.h"
/** @file bienvhash.h
* @Brief For more information check bienvhash(template)c.
*/
/** Brief For more information check bienvhash(template)c.
*/

/** Dumps the table to stdout. */
extern void bi_dumpTable(void);
/** Dumps the table to our result file. */
extern void bi_dumpTableToFile( FILE ** );
/** Fills the table with predefined content. */
extern void bi_fillTable(void);
/** Retrieves a value from the table. If the given key
    does not exist a null pointer is returned. */
extern char *bi_get ( const char *, int * );
/** Creates the table and initializes the fileds. */
extern void bi_initTable(void);
/** Puts a Key-Value pair into the table. If the key
    already exists, the value will be overwritten.
    Returns 0, if the key is new, 1 if a value was
    overwritten. */
extern int bi_put( const char *, const char * );
/** Returns the number of entries stored in the table. */
extern int bi_size(void);
/** Adds variables from a PARAMETER file to the table. */
extern int bi_readParameterFile( const char * );

#endif
/*****************************************************************************
Log-History

$Log: bienvhash.h,v $
Revision 1.5  2007/01/19 15:05:11  molka
removed some icc-warnings

Revision 1.4  2006/09/29 14:51:14  rschoene
commented

Revision 1.3  2005/07/21 13:32:04  wloch
implemented hashtable dump into bit file

Revision 1.2  2005/07/19 12:17:59  wloch
added cvs footer

Revision 1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree

*****************************************************************************/
