/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: c kernel skeleton
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: william $
 * $Revision: 1.2 $
 * $Date: 2007/04/27 07:39:46 $
 *******************************************************************/

#ifndef __work_h
#define __work_h

#define STRINGSIZE 512

typedef struct mydata{
		char * filename;
    char * filesize_min;
    char * filesize_min_unit;
    char * filesize_max;
    char * filesize_max_unit;
    char * filesize_inc;
    char * filesize_inc_unit;
    char * recordsize;
    char * testlist;
    char * cachelinesize;
    char * cachesize;
    char * options;
//    char ** testnamearray;
    unsigned int * testarray;
    unsigned int nr_tests; /* number of iozone tests */
    unsigned int numfunctions; /* many tests include 2 functions */
    unsigned long min;
    unsigned long max;
    unsigned long inc;
} mydata_t;

extern void work( char * );
#endif


/********************************************************************
 * Log-History
 *
 * $Log: kernel_main.h,v $
 * Revision 1.2  2007/04/27 07:39:46  william
 * additional functionality
 *
 * Revision 1.1  2007/04/25 07:11:33  william
 * removed the iozone-sources
 *
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
