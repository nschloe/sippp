/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
* 
* Author: Michael Kluge (kluge@zhr.tu-dresden.de)
*
* $Revision: 1.6 $
* $Date: 2007/01/19 15:05:11 $
* $State: Exp $
*
***********************************************************************/

/** @file fileversion.c
* @Brief gets the fileversion and writes them to an environment variable.
*/

#include <sys/stat.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <dirent.h>
#include <time.h>

/** replace all existing characters O with character N */
#define REPLACE(STR,O,N) for( index=0; index<strlen(STR); index++) \
                            if( STR[index]==O ) STR[index]=N;
                            
/** str.toUpperCase() ;) to say it in JAVA */
#define UPPERCASE(STR) for( index=0; index<strlen(STR); index++) \
                            STR[index]= (char) toupper(STR[index]);
/**@brief look for BENCHIT_KERNEL_FILE_VERSION_<filename> in files and set
* an ENVIRONMENT VARIABLE for it
*/
int main (void)
{
    /** folder structure */
    DIR *dp;
    struct dirent *ep;
    struct stat statbuf;
    char *buff, *found;
    /** buffer for file content */
    int buffsize=100*1024;
    char temp[100];
    char temp2[100];
    int index;
    FILE *f;
    /* allocate memory for filecontent  */
    buff=(char *) malloc(buffsize);
    if(buff==NULL)
    {
        printf("Unable to allocate %dKB\n", buffsize/1024);
        exit(1);
    }

    /** open actual folder */
    dp = opendir ("./");
    if (dp != NULL)
    {
        while( (ep=readdir(dp))!=NULL )
        {
            /* skip some special candidates */
            /** everything, which starts with . */
            if( ep->d_name[0]=='.' )
                continue;
            /** CVS isnt what we want */
            if( strcmp(ep->d_name,"CVS")==0 )
                continue;
            /** neither are .o files */
            index=strlen(ep->d_name);
            if( ep->d_name[index-2]=='.' && ep->d_name[index-1]=='o' )
                continue;
            /** stat for filename (get information) */
            if( stat(ep->d_name,&statbuf)!=0 )
            {
                /** error! stat not succesful */
                REPLACE( ep->d_name, '.', '_');
                UPPERCASE( ep->d_name);
                printf("BENCHIT_KERNEL_FILE_VERSION_%s='%s (%s)'\n",
                    ep->d_name, "NO REVISION, UNABLE TO STAT", "COULD NOT STAT");
                continue;
            }
            /* stat succesful */
            else
            {
                /** write changes since last time to temp2 */
                strncpy( temp2, ctime(&statbuf.st_mtime), 100);
                for( index=0; index<100; index++)
                    if( temp2[index]<' ' )
                        temp2[index]=0;
                /** maybe enlarge buffer for filecontent */
                if( statbuf.st_size>buffsize )
                {
                    buff=(char *) realloc(buff,statbuf.st_size);
                    if(buff==NULL)
                    {
                        printf("Unable to allocate %dKB\n", buffsize/1024);
                        exit(1);
                    }
                    buffsize=statbuf.st_size;
                }
                /** open file */
                if( (f=fopen(ep->d_name,"r"))==NULL )
                {
                    REPLACE( ep->d_name, '.', '_');
                    UPPERCASE( ep->d_name);
                    printf("BENCHIT_KERNEL_FILE_VERSION_%s='%s (%s)'\n",
                        ep->d_name, "NO REVISION, UNABLE TO OPEN", temp2);
                    continue;
                }
                memset( buff, 0, buffsize);
                /** read complete file */
                if( fread( buff, statbuf.st_size, 1, f)!= 1)
                {
                    REPLACE( ep->d_name, '.', '_');
                    UPPERCASE( ep->d_name);
                    printf("BENCHIT_KERNEL_FILE_VERSION_%s='%s (%s)'\n",
                        ep->d_name, "NO REVISION, UNABLE TO READ", temp2);
                    continue;
                }
                fclose(f);
                /** check for $Revision: */
                found=strstr( buff, "$Revision:");
                if( found==NULL )
                {
                    REPLACE( ep->d_name, '.', '_');
                    UPPERCASE( ep->d_name);
                    printf("BENCHIT_KERNEL_FILE_VERSION_%s='%s (%s)'\n",
                        ep->d_name, "NO REVISION", temp2);
                    continue;
                }
                /** go to the beginning of '$Revision:' */
                found+=10;
                while( *found==' ' )
                    found++;
                index=0;
                memset( temp, 0, 100);
                /** read revision number */
                while( *found!='$' && *found>' ' && index<99 )
                    temp[index++]=*(found++);
                /** okay, write result */
                REPLACE( ep->d_name, '.', '_');
                UPPERCASE( ep->d_name);
                printf("BENCHIT_KERNEL_FILE_VERSION_%s='%s (%s)'\n",
                        ep->d_name, temp, temp2 );

            }
        }
        (void) closedir (dp);
    }
    else
    perror ("Couldn't open the directory");
    free(buff);
    printf("\n");
    return 0;
}
/*****************************************************************************
Log-History

$Log: fileversion.c,v $
Revision 1.6  2007/01/19 15:05:11  molka
removed some icc-warnings

Revision 1.5  2007/01/12 10:44:35  molka
replaced //-comments and tabs

Revision 1.4  2006/09/29 14:50:55  rschoene
commented

Revision 1.3  2005/08/04 11:17:47  kluge
documentation added

Revision 1.2  2005/07/19 12:17:59  wloch
added cvs footer

Revision 1.1.1.1  2005/07/18 13:03:19  wloch
the final restructured benchit source tree

Revision 1.3  2005/07/04 12:10:49  wloch
removed export calls

*****************************************************************************/
