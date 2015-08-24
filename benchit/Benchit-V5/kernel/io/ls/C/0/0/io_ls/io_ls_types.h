/***********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  Header file for my_types.c
 *  Author: Peter Gottschling (gottschling@zhr.tu-dresden.de)
 *
 *  $Revision: 1.1 $
 *  $Date: 2006/11/01 20:22:51 $
 *  $State: Exp $
 *
 ***********************************************************************/

#ifndef BENCHIT_IO_LS_TYPES_H
#define BENCHIT_IO_LS_TYPES_H

#define STRING_SIZE 1024
#define PATH_NAME_SIZE STRING_SIZE

/* internal */
struct str_dir_tree_descr_t {
    char *root;
    int  depth;
};

/** Data structure to describe directory tree, contains root and depth of tree */
typedef struct str_dir_tree_descr_t* dir_tree_descr_t;


/** Initialize: allocate and copy root */
dir_tree_descr_t init_tree_descr (const char* root, int depth);

/** Set memory free */
void free_tree_descr (dir_tree_descr_t d); 

#endif /* BENCHIT_IO_LS_TYPES_H */





/*****************************************************************************

LOG-History

$Log: io_ls_types.h,v $
Revision 1.1  2006/11/01 20:22:51  william
first checkin after complete codereview and simple tests

Revision 1.1.1.1  2006/04/18 10:03:49  william
import version 0.1

Revision 1.3  2004/07/08 10:24:58  pgottsch
Minor corrections

Revision 1.2  2004/07/02 14:34:56  pgottsch
A few cleanings

Revision 1.1  2004/07/02 14:24:36  pgottsch
First version (is running)


*****************************************************************************/
