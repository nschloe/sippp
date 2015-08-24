/*********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Description: The BenchScript Interface
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: mueller $
 * $Revision: 1.1 $
 * $Date: 2006/05/23 08:13:00 $
 *********************************************************************/

#ifndef BENCHSCRIPT_H
#define BENCHSCRIPT_H

extern double logx( double base, double value );
extern char* int2char( int number );
extern char* bi_strcat( const char *str1, const char *str2 );

/* NEW */

extern void bi_script( char* script, int num_processes, int num_threads );
extern void bi_script_create_processes( char* script, int num_processes, int num_threads );
extern void bi_script_create_threads( char* script, int num_threads );
extern void* bi_script_run( void* script );

#endif

/********************************************************************
 * Log-History
 * 
 * $Log: benchscript.h,v $
 * Revision 1.1  2006/05/23 08:13:00  mueller
 * - changed location of benchscript files to tools/
 *
 * Revision 1.2  2006/05/22 10:06:19  mueller
 * - fixed header
 *
 * 
 ********************************************************************/
