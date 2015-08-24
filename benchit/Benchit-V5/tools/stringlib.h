/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
*
* Author: Robert Wloch (wloch@zhr.tu-dresden.de)
*
* $Revision: 1.2 $
* $Date: 2006/09/29 14:51:14 $
* $State: Exp $
*
***********************************************************************/

/** @file stringlib.h
* @Brief For advanced string work, see c-file for more information.
*/
#ifndef STRINGLIB_H
#define STRINGLIB_H


#define STR_LEN 65536
/**
* Brief Commented functions under stringlibc.
*/
extern int compare( const char *, const char * );
extern int comparem( const char *, char * );
extern int comparec( char *, char * );
extern int escapeChar( const char *, char *, char );
extern int indexOf( const char *, char, int );
extern int lastIndexOf( const char *, char, int );
extern int length( const char * );
extern int lengthc( char * );
extern int substring( const char*, char*, int, int );
extern int trim( const char *, char * );
extern int trimChar( const char *, char *, char );

extern int min( int, int );

#endif
/*****************************************************************************
Log-History

$Log: stringlib.h,v $
Revision 1.2  2006/09/29 14:51:14  rschoene
commented

Revision 1.1.1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree

*****************************************************************************/
