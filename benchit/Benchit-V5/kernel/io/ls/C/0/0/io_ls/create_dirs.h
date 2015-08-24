/***********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  Header file for create_dirs.c
 *  Author: Peter Gottschling (gottschling@zhr.tu-dresden.de)
 *
 *  $Revision: 1.1 $
 *  $Date: 2006/11/01 20:22:51 $
 *  $State: Exp $
 *
 ***********************************************************************/

#ifndef BENCHIT_CREATE_DIRS_H
#define BENCHIT_CREATE_DIRS_H

#include "io_ls_types.h"

/** Copies at most <n> byte of <src> into <dest> and makes sure that
 *  <dest> is 0-finished.  Returns number of copied bytes.  The closing
 *  0 char is only counted if n is reached (since no further copy in dest
 *  is possible).
 */
int myStrncpy (char *dest, const char *src, int n);

/** Converts binary number with <digits> significant digits into name. 
 *  If x is the most significant bit (corr. to 2^(digits-1)), y the second,
 *  ... z the least sign. then name will be:
 *  <prefix> x <infix> y <infix> ... <infix> z,
 *  e.g. binary2name(7, 5, n, 30, "_pr_", "_in_", "_su_") yields
 *  _pr_0_in_0_in_1_in_0_in_1_su_
 */
void binary2name(int number, int digits, char *name, int nameLength, 
		 const char *prefix, const char *infix, const char *suffix);

/** Creates directories in <path> as a tree of <depth>, 
 *  e.g. create_dirs("a", 2) yields
 *  a/d0/d0 a/d0/d1 a/d1/d0 a/d1/d1 
 */
void create_dirs(const dir_tree_descr_t tree);

/** Writes in each leaf directory from create_dirs <filesPerDir> files
 *  named reg0, reg1, ...
 */ 
void create_files_regularly(const dir_tree_descr_t tree, int filesPerDir);

/** Writes in each directory from create_dirs <filesPerDir> files
 *  named reg0, reg1, ...
 *  if directory has depth d: fromDepth <= d <= toDepth
 */ 
void create_files_depth_interval(const dir_tree_descr_t tree, int filesPerDir, 
				 int fromDepth, int toDepth);

/** Same as before with d: 1 <= d <= tree->depth-1 */
void create_files_except_leafs (const dir_tree_descr_t tree, int filesPerDir);
	
/** Removes all files except in the leaf directories */
void remove_files_except_leafs (const dir_tree_descr_t tree);

/** Writes in the directories from create_dirs in average <filesPerDir> files
 *  named rand0, rand1, ...
 */
void create_files_randomly(const dir_tree_descr_t tree, int filesPerDir);

/** Clean directories in tree at depths from <fromDepth> to <toDepth> */
void clean_directories (const dir_tree_descr_t tree, int fromDepth, int toDepth);


#endif /* BENCHIT_CREATE_DIRS_H */





/*****************************************************************************

LOG-History

$Log: create_dirs.h,v $
Revision 1.1  2006/11/01 20:22:51  william
first checkin after complete codereview and simple tests

Revision 1.1.1.1  2006/04/18 10:03:49  william
import version 0.1

Revision 1.4  2004/09/29 07:29:08  pgottsch
Script call changed

Revision 1.3  2004/07/08 16:55:08  pgottsch
Files not only written in the leaf dir + Deletion of files at any depth of the tree

Revision 1.2  2004/07/02 14:34:56  pgottsch
A few cleanings

Revision 1.1  2004/07/02 14:24:36  pgottsch
First version (is running)


*****************************************************************************/
