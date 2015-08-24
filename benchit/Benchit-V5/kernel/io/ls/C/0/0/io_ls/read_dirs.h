/***********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  Header file for read_dirs.c
 *  Author: Peter Gottschling (gottschling@zhr.tu-dresden.de)
 *
 *  $Revision: 1.1 $
 *  $Date: 2006/11/01 20:22:51 $
 *  $State: Exp $
 *
 ***********************************************************************/

#ifndef BENCHIT_READ_DIRS_H
#define BENCHIT_READ_DIRS_H

#include "create_dirs.h"


/** Reads silently the content of a directory and returns the number of files (-1 if failed) */
int read_dir(const char* dir); 

/** Reads silently the content of directory tree at depth <myDepth> (numberDirs times, if necessary restarts) 
 *  and returns the number of files (-1 if failed) */
int read_dirs_regularly(const dir_tree_descr_t tree, int myDepth, int numberDirs);

/** Reads silently the content of directory tree at depth <myDepth> in random order (numberDirs times)
 *  and returns the number of files (-1 if failed) */
int read_dirs_randomly(const dir_tree_descr_t tree, int myDepth, int numberDirs);

/** Overhead of read_dirs_regularly */
int overhead_read_dirs_regularly(const dir_tree_descr_t tree, int myDepth, int numberDirs);

/** Overhead of read_dirs_randomly */
int overhead_read_dirs_randomly(const dir_tree_descr_t tree, int myDepth, int numberDirs);

/*  Internal function used by the 4 above and by io_ls because of easier handling */
int intern_read_dirs (const dir_tree_descr_t tree, int myDepth, int numberDirs, int randomly, int overhead);

#endif /* BENCHIT_READ_DIRS_H */

/*****************************************************************************

LOG-History

$Log: read_dirs.h,v $
Revision 1.1  2006/11/01 20:22:51  william
first checkin after complete codereview and simple tests

Revision 1.1.1.1  2006/04/18 10:03:49  william
import version 0.1

Revision 1.2  2004/07/02 14:34:56  pgottsch
A few cleanings

Revision 1.1  2004/07/02 14:24:36  pgottsch
First version (is running)


*****************************************************************************/
