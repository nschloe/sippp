/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <the header-file bigread-structs>
*
*  Author: 	Sebastian Koelling (<koelling@zhr.tu-dresden.de>)
*		Thomas William (<william@zhr.tu-dresden.de>)
*
*  $Revision: 1.1 $
*  $Author: william $
*  $Date: 2007/04/26 02:33:04 $
*  $State: Exp $
*
******************************************************************************/

#ifndef iobigread_h
#define iobigread_h

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif

typedef struct io_data_struct
        {
	double FILESIZE;
	double DISKSPACE;
	double RAMSIZE;

	int POTFILPERDIR;
	int FILESPERDIR;
	int FILESPERTHREAD;
	int MAXREPEAT;
	int REPEATSTOP;
	int NUMCHANNELS;
	int CHANNELFACTOR;
	int TIMELIMIT;

	char * DISKPATH; 
	char * TMPHEADER;

        char *path;
	char *startpath;
	long maxdeep;
        } iods;

/*
    wrapperstruct for
    readfiles(problemsize, global->maxdeep, btime+i, etime+i);	    
*/
typedef struct 
	{
	iods *global;
	long problemsize;
	double *btime;
	double *etime;
	} thread_arg_wrapper_t;

extern void thread_readfile(thread_arg_wrapper_t *taw);

#endif /* #ifndef iobigread_h */
/***********************************************************
* This is the complete log for this file:
*
* $Log: iobigread.h,v $
* Revision 1.1  2007/04/26 02:33:04  william
* fixed errors due to old layout of Kernel (no more RUN.SH)
*
* Revision 1.2  2006/04/20 23:23:23  william
* let the bughuntseason begin
*
* Revision 1.1.1.1  2006/04/18 10:03:50  william
* import version 0.1
*
* Revision 1.1.1.1  2004/12/14 21:22:56  william
* Release 3.0 - created new cvs-tree src2
*
*
***********************************************************/
