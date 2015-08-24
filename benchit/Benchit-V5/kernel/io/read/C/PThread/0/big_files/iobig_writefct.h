/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <the header-file for the writefunction>
*
*  Author: 	Sebastian Koelling (<koelling@zhr.tu-dresden.de>)
*
*  $Revision: 1.1 $
*  $Author: william $
*  $Date: 2007/04/26 02:33:04 $
*  $State: Exp $
*
******************************************************************************/

#ifndef iobig_writefct_h
#define iobig_writefct_h
char *inttobinstring(long number);
void CreateFile(char *buffer, char* filename);
int CreateTree(long maxsub, char *path, long maxdeep, long deep, long *actual, char *buffer, long maxfiles);
void ioCreate(char *buffer, long number);
#endif

/***********************************************************
* This is the complete log for this file:
*
* $Log: iobig_writefct.h,v $
* Revision 1.1  2007/04/26 02:33:04  william
* fixed errors due to old layout of Kernel (no more RUN.SH)
*
* Revision 1.1.1.1  2006/04/18 10:03:50  william
* import version 0.1
*
* Revision 1.1.1.1  2004/12/14 21:22:56  william
* Release 3.0 - created new cvs-tree src2
*
*
***********************************************************/
