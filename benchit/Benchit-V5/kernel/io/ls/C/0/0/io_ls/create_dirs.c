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


#include <dirent.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "create_dirs.h"

int myStrncpy (char *dest, const char *src, int n) {
    int sl;
    sl= strlen(src);

    if (sl >= n) {
	fprintf (stderr, "In myStrcpy: src is to long -> will be cutted\n");
	strncpy (dest, src, n-1);
	dest[n-1]= 0; /* make sure that finished with 0 */
	return n; }
    strncpy (dest, src, n);
    return sl;
}

/* internal function of binary2name */
static void binary2name_cpy (char **pos, const char *text, int *restLength) {
    int copied;
    copied= myStrncpy(*pos, text, *restLength);
    *pos+= copied; 
    *restLength-= copied;
    if (*restLength == 0) {
	fprintf (stderr, "In binary2name: name string too short\nExit!"); exit (127);}
}

void binary2name(int number, int digits, char *name, int nameLength, 
		 const char *prefix, const char *infix, const char *suffix) {
    int      restLength, bitmask;
    char     *pos, *lastPos, digitString[2];

    /* restLength= nameLength-1; for final 0 */
    restLength= nameLength;
    lastPos= name + (nameLength-1);
    pos= name;
    digitString[1]= 0;

    binary2name_cpy (&pos, prefix, &restLength);
    for (bitmask= 1 << (digits-1); bitmask > 1; bitmask>>= 1) {
	digitString[0]= number & bitmask ? '1' : '0';
	binary2name_cpy (&pos, digitString, &restLength);
	binary2name_cpy (&pos, infix, &restLength);
    }

    digitString[0]= number & bitmask ? '1' : '0';
    binary2name_cpy (&pos, digitString, &restLength);
    binary2name_cpy (&pos, suffix, &restLength);
}
    
void create_dirs(const dir_tree_descr_t tree) {
    int i, j, number;
    char name[PATH_NAME_SIZE], pathPrefix[PATH_NAME_SIZE], cwd[PATH_NAME_SIZE];
    FILE *fp;

    getcwd(cwd, PATH_NAME_SIZE);
    chdir (tree->root); fp= fopen("dir_state","w");
    fprintf(fp, "Writing of directories not finished.");
    fclose (fp);

    strcpy (pathPrefix, tree->root); strcat (pathPrefix, "/d");
    for (i= 1; i <= tree->depth; i++) {
	number= 1 << i;
	for (j= 0; j < number; j++) {
	    binary2name (j, i, name, PATH_NAME_SIZE, pathPrefix, "/d", "");
	    /* printf ("%s\n", name); */
	    if (mkdir(name, S_IRWXU)) {
		fprintf(stderr, "\nCannot create subdirectory %s\n", name); exit(127); }
	} }

    fp= fopen("dir_state","w");
    fprintf(fp, "Directories successfully written.\nDepth is %i.\n", tree->depth);
    fclose (fp);
    /* to not confuse any other function reset working directory */
    chdir(cwd);
}

void create_files_regularly(const dir_tree_descr_t tree, int filesPerDir) {
    char pathPrefix[PATH_NAME_SIZE], dirName[PATH_NAME_SIZE], fileName[PATH_NAME_SIZE], 
         cwd[PATH_NAME_SIZE];
    int  i, j, number;
    FILE *fp;

    getcwd(cwd, PATH_NAME_SIZE);
    chdir (tree->root); fp= fopen("reg_files_state","w");
    fprintf(fp, "Writing of files regularly not finished.");
    fclose (fp);

    strcpy (pathPrefix, tree->root); strcat (pathPrefix, "/d");
    number= 1 << tree->depth;
    for (i= 0; i < number; i++) {
	binary2name (i, tree->depth, dirName, PATH_NAME_SIZE, pathPrefix, "/d", "");
	chdir (dirName);
	for (j= 0; j < filesPerDir; j++) {
	    sprintf (fileName, "reg%i", j);
	    fp= fopen(fileName,"w");
	    fprintf (fp, "Senseless text of file %s in directory %s\n", fileName, dirName); 
	    fclose (fp);}
    }
    chdir (tree->root); fp= fopen("reg_files_state","w");
    fprintf(fp, "Files successfully written.\nRegularly\nDepth is %i.\n%i files per dir.\n", tree->depth, filesPerDir);
    fclose (fp);
    /* to not confuse any other function reset working directory */
    chdir(cwd);
}

void create_files_depth_interval(const dir_tree_descr_t tree, int filesPerDir, 
				 int fromDepth, int toDepth) {
    char pathPrefix[PATH_NAME_SIZE], dirName[PATH_NAME_SIZE], fileName[PATH_NAME_SIZE], 
         cwd[PATH_NAME_SIZE];
    int  i, j, d, number;
    FILE *fp;

    getcwd(cwd, PATH_NAME_SIZE);
    chdir (tree->root); fp= fopen("reg_files_interval_state","w");
    fprintf(fp, "Writing of files regularly not finished.");
    fclose (fp);

    snprintf (pathPrefix, PATH_NAME_SIZE, "%s/d", tree->root); 
    if (toDepth > tree->depth) {
	fprintf (stderr, "create_files_regularly called with depth %i. It is reduced to depth of tree (%i)\n.", 
		 toDepth, tree->depth);
	toDepth= tree->depth; }

    for (d= fromDepth; d <= toDepth; d++) {
	number= 1 << d;
	for (i= 0; i < number; i++) {
	    binary2name (i, d, dirName, PATH_NAME_SIZE, pathPrefix, "/d", "");
	    chdir (dirName);
	    for (j= 0; j < filesPerDir; j++) {
		sprintf (fileName, "reg%i", j);
		fp= fopen(fileName,"w");
		fprintf (fp, "Senseless text of file %s in directory %s\n", fileName, dirName); 
		fclose (fp);} } }

    chdir (tree->root); fp= fopen("reg_files_interval_state","w");
    fprintf(fp, "Files successfully written.\nRegularly\nDepth is %i-%i.\n%i files per dir.\n", 
	    fromDepth, toDepth, filesPerDir);
    fclose (fp);
    /* to not confuse any other function reset working directory */
    chdir(cwd);
}

void create_files_except_leafs (const dir_tree_descr_t tree, int filesPerDir) {
    create_files_depth_interval(tree, filesPerDir, 1, tree->depth-1);
}
	
void remove_files_except_leafs (const dir_tree_descr_t tree) {
    char pathPrefix[PATH_NAME_SIZE], dirName[PATH_NAME_SIZE], cwd[PATH_NAME_SIZE];
    int  i, d, number;

    getcwd(cwd, PATH_NAME_SIZE);
    for (d= 1; d <= tree->depth-1; d++) {
	number= 1 << d;
	for (i= 0; i < number; i++) {
	    binary2name (i, d, dirName, PATH_NAME_SIZE, pathPrefix, "/d", "");
	    chdir (dirName);
	    system ("rm -f reg*"); } }

    chdir (tree->root); 
    system ("rm -f reg_files_interval_state");

    /* to not confuse any other function reset working directory */
    chdir(cwd);
}

void create_files_randomly(const dir_tree_descr_t tree, int filesPerDir) {
    char pathPrefix[PATH_NAME_SIZE], dirName[PATH_NAME_SIZE], fileName[PATH_NAME_SIZE], 
         cwd[PATH_NAME_SIZE];
    int  i, number, totalFiles;
    FILE *fp;

    getcwd(cwd, PATH_NAME_SIZE);
    chdir (tree->root); fp= fopen("rand_files_state","w");
    fprintf(fp, "Writing of files randomly not finished.");
    fclose (fp);

    strcpy (pathPrefix, tree->root); strcat (pathPrefix, "/d");
    number= 1 << tree->depth;
    for (i= 0, totalFiles= number*filesPerDir; i < totalFiles; i++) {
	binary2name (rand() % number, tree->depth, dirName, PATH_NAME_SIZE, pathPrefix, "/d", "");
	chdir (dirName);
	sprintf (fileName, "rand%i", i);
	fp= fopen(fileName,"w");
	fprintf (fp, "Senseless text of file %s in directory %s\n", fileName, dirName); 
	/* printf ("Writing file %s in directory %s\n", fileName, dirName); */
	fclose (fp); 
    }
    chdir (tree->root); fp= fopen("rand_files_state","w");
    fprintf(fp, "Files successfully written.\nRandomly\nDepth is %i.\n%i files overall.\n", tree->depth, totalFiles);
    fclose (fp);
    /* to not confuse any other function reset working directory */
    chdir(cwd);
}


void clean_directories (const dir_tree_descr_t tree, int fromDepth, int toDepth) {
    char pathPrefix[PATH_NAME_SIZE], dirName[PATH_NAME_SIZE];
    int             i, d, number;
    DIR             *dp;
    struct dirent   *dirp;
    struct stat     sbuf;

    snprintf (pathPrefix, PATH_NAME_SIZE, "%s/d", tree->root); 
    if (toDepth > tree->depth) {
	fprintf (stderr, "create_files_regularly called with depth %i. It is reduced to depth of tree (%i)\n.", 
		 toDepth, tree->depth);
	toDepth= tree->depth; }

    for (d= fromDepth; d <= toDepth; d++) {
	number= 1 << d;
	for (i= 0; i < number; i++) {
	    binary2name (i, d, dirName, PATH_NAME_SIZE, pathPrefix, "/d", "");
	    dp = opendir(dirName);                 /* no error checking */
	    while ((dirp = readdir(dp)) != 0) {
		lstat(dirp->d_name, &sbuf);        /* no error checking */
		if (S_ISREG(sbuf.st_mode))
		    remove (dirp->d_name); }
	    closedir(dp); } }
}


/*****************************************************************************

LOG-History

$Log: create_dirs.c,v $
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

Revision 1.1  2004/07/02 14:24:35  pgottsch
First version (is running)

*****************************************************************************/
