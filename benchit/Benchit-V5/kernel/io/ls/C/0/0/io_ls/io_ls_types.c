/***********************************************************************
 *
 *  B e n c h I T - Performance Measurement for Scientific Applications
 *
 *  implemented Kernel io_ls (C)
 *  Author: Peter Gottschling (gottschling@zhr.tu-dresden.de)
 *
 *  $Revision: 1.1 $
 *  $Date: 2006/11/01 20:22:51 $
 *  $State: Exp $
 *
 ***********************************************************************/


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "io_ls_types.h"

dir_tree_descr_t init_tree_descr (const char* root, int depth) {
    dir_tree_descr_t tmp;
    if (root == NULL) {
	fprintf (stderr, "init_tree_descr: root pointer is NULL (undefined environment .. ?)\n");
	exit (127); }
    tmp= (dir_tree_descr_t) malloc (sizeof (struct str_dir_tree_descr_t));
    tmp->root= (char *) malloc (strlen (root) + 1);
    strcpy (tmp->root, root); tmp->depth= depth;
    return tmp;
}

void free_tree_descr (dir_tree_descr_t d) {
    free (d->root); free (d);
}



/*****************************************************************************

LOG-History

$Log: io_ls_types.c,v $
Revision 1.1  2006/11/01 20:22:51  william
first checkin after complete codereview and simple tests

Revision 1.1.1.1  2006/04/18 10:03:49  william
import version 0.1

Revision 1.2  2004/07/02 14:34:56  pgottsch
A few cleanings

Revision 1.1  2004/07/02 14:24:36  pgottsch
First version (is running)



*****************************************************************************/
