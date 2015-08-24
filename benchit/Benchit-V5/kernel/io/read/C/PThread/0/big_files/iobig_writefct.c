/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <the writefunction>
*
*  Author: 	Sebastian Koelling (<koelling@zhr.tu-dresden.de>)
*
*  $Revision: 1.1 $
*  $Author: william $
*  $Date: 2007/04/26 02:33:04 $
*  $State: Exp $
*
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "interface.h"
#include "iobig_writefct.h"
#include "iobigread.h"

/*this function changes number into the belonging (binary) string*/
char *inttobinstring(long number)
    	{                                                                               
	int i, j;                                                                       
	char *zahl;                                                                     
double FILESIZE=0.0, DISKSPACE=0.0, RAMSIZE=0.0 ;

int POTFILPERDIR=0, FILESPERDIR=0, FILESPERTHREAD=0, MAXREPEAT=0,
    REPEATSTOP=0, NUMCHANNELS=0, CHANNELFACTOR=0, TIMELIMIT=0 ;

char * DISKPATH=NULL; 
char * TMPHEADER=NULL;


iods * pmydata;

pmydata = (iods *) malloc(sizeof(iods));

evaluate_environment(pmydata);

FILESIZE = pmydata->FILESIZE;
POTFILPERDIR = pmydata->POTFILPERDIR ;
FILESPERDIR =  pmydata->FILESPERDIR ;
FILESPERTHREAD = pmydata->FILESPERTHREAD;
MAXREPEAT = pmydata->MAXREPEAT;
REPEATSTOP = pmydata->REPEATSTOP;
DISKPATH = (char *) malloc( sizeof( char ) * 128 );
DISKPATH = pmydata->DISKPATH ;
TMPHEADER = (char *) malloc( sizeof( char ) * 128 );
TMPHEADER = pmydata->TMPHEADER;
DISKSPACE = pmydata->DISKSPACE;
NUMCHANNELS = pmydata->NUMCHANNELS;
CHANNELFACTOR = pmydata->CHANNELFACTOR;
RAMSIZE = pmydata->RAMSIZE;
TIMELIMIT = pmydata->TIMELIMIT;



    	zahl=malloc((POTFILPERDIR+1)*sizeof(char));
	if(zahl==NULL) {printf("\nCant get memory to transform digits into string!\n"); exit(127); }
    	memset(zahl, '0', (POTFILPERDIR+1));

	/*end of string*/
    	zahl[POTFILPERDIR]='\0';
    	i=(POTFILPERDIR-1);

	/*j passes potencies of 2 until its enough for the files in one 
	  directory*/
    	for(j=1; j<FILESPERDIR; j<<=1)
		{
		/* "zahl" is pictured from behind with the potencies of 
		  2 from "number" */
		if((number & j)!=0) zahl[i]='1';
    		i--;
    		}
    	return zahl;
    	}


/*this function creates the file "filename" with the content of "buffer"*/
void CreateFile(char *buffer, char* filename)
	{
	FILE *fp;
	unsigned long n;
double FILESIZE=0.0, DISKSPACE=0.0, RAMSIZE=0.0 ;

int POTFILPERDIR=0, FILESPERDIR=0, FILESPERTHREAD=0, MAXREPEAT=0,
    REPEATSTOP=0, NUMCHANNELS=0, CHANNELFACTOR=0, TIMELIMIT=0 ;

char * DISKPATH=NULL; 
char * TMPHEADER=NULL;


iods * pmydata;

pmydata = (iods *) malloc(sizeof(iods));

evaluate_environment(pmydata);

FILESIZE = pmydata->FILESIZE;
POTFILPERDIR = pmydata->POTFILPERDIR ;
FILESPERDIR =  pmydata->FILESPERDIR ;
FILESPERTHREAD = pmydata->FILESPERTHREAD;
MAXREPEAT = pmydata->MAXREPEAT;
REPEATSTOP = pmydata->REPEATSTOP;
DISKPATH = (char *) malloc( sizeof( char ) * 128 );
DISKPATH = pmydata->DISKPATH ;
TMPHEADER = (char *) malloc( sizeof( char ) * 128 );
TMPHEADER = pmydata->TMPHEADER;
DISKSPACE = pmydata->DISKSPACE;
NUMCHANNELS = pmydata->NUMCHANNELS;
CHANNELFACTOR = pmydata->CHANNELFACTOR;
RAMSIZE = pmydata->RAMSIZE;
TIMELIMIT = pmydata->TIMELIMIT;



	/*open file*/    
	fp=fopen(filename,"w");
	if(fp==NULL) { printf("Cant write file %s\n!", filename); exit(127); }
	n=fwrite(buffer, sizeof(char), (size_t)(FILESIZE/sizeof(char)), fp);
	if(n!=FILESIZE) printf("Warning a file wasnt created correctly. The return value of fwrite was %ld instead of regular filesize!.\n", n);

	/*close file*/ 
	fclose(fp);
    }


/*this function creates a binary tree with the depth of "maxdeep" and "maxsub" 
  ends which contains "maxfiles" files at each end*/
int CreateTree(long maxsub, char *path, long maxdeep, long deep, long *actual, char *buffer, long maxfiles) 
    	{                                                                               
    	char *help, *digit, *filename;                                                  
    	long i, j, changes;
double FILESIZE=0.0, DISKSPACE=0.0, RAMSIZE=0.0 ;

int POTFILPERDIR=0, FILESPERDIR=0, FILESPERTHREAD=0, MAXREPEAT=0,
    REPEATSTOP=0, NUMCHANNELS=0, CHANNELFACTOR=0, TIMELIMIT=0 ;

char * DISKPATH=NULL; 
char * TMPHEADER=NULL;


iods * pmydata;

pmydata = (iods *) malloc(sizeof(iods));

evaluate_environment(pmydata);

FILESIZE = pmydata->FILESIZE;
POTFILPERDIR = pmydata->POTFILPERDIR ;
FILESPERDIR =  pmydata->FILESPERDIR ;
FILESPERTHREAD = pmydata->FILESPERTHREAD;
MAXREPEAT = pmydata->MAXREPEAT;
REPEATSTOP = pmydata->REPEATSTOP;
DISKPATH = (char *) malloc( sizeof( char ) * 128 );
DISKPATH = pmydata->DISKPATH ;
TMPHEADER = (char *) malloc( sizeof( char ) * 128 );
TMPHEADER = pmydata->TMPHEADER;
DISKSPACE = pmydata->DISKSPACE;
NUMCHANNELS = pmydata->NUMCHANNELS;
CHANNELFACTOR = pmydata->CHANNELFACTOR;
RAMSIZE = pmydata->RAMSIZE;
TIMELIMIT = pmydata->TIMELIMIT;



	/*if no tree(=no subdirectories) have to be created -> just write 
	  files*/
    	if (maxfiles<FILESPERDIR)
        	{
        	filename=malloc(134*sizeof(char));
		/*abort when all files are created*/
        	/* for (i=0; i<maxfiles+1; i++) WHY maxfiles+1 ??? */
/* CHANGED */
        	for (i=0; i<=maxfiles; i++)
        		{
			/*change number "i" to (binary) string "digit"*/ 
            		digit=inttobinstring(i);
            		sprintf(filename, "%s", digit);
			/*create file*/
			changes=random()%((long)(FILESIZE/(100*sizeof(char))));
                        for(j=0;j<changes;j++)
			        { /* FILESIZE can be larger than buffer !!!!!!!! --> SEGV */
				     buffer[(long)random() % (long)FILESIZE]=(char)(random()%250+1);
				}
            		CreateFile(buffer, filename);
            		}
            	free(filename);
        	return 0;
        	}

	/*if maximum depth is reached -> dont go deeper (=dont do anything)*/
    	if (deep>=maxdeep) return 0;
	/*if number of necessary ends is reached -> dont go on*/
    	if ((*actual)>maxsub) return 0;
                                                                                
   	help=malloc(128*sizeof(char)); 
	if(help==NULL) { printf("\nCant get memory to create tree subdirectories!\n"); exit(127); }
	/*creation of a new "zero-branch"*/
    	sprintf(help, "%s0/", path);
    	if(mkdir(help, S_IRWXU)) { printf("\nCant create subdirectory %s\n", help); exit(127); }
	/*depth of tree increases*/
    	deep++;
	/*if maximum depth is reached -> write the files there*/
    	if (deep==maxdeep)
		{
        	filename=malloc(134*sizeof(char));
		if(filename==NULL) { printf("\nCant get memory to create filename!\n"); exit(127); }
		/*stop when directory has given number of files or when all files are written*/
		for (i=0; i<=FILESPERDIR && ((*actual)*FILESPERDIR+i)<maxfiles+1; i++)
            		{
			/*change number to (binary) string*/
            		digit=inttobinstring(i);
			/* "filename" contains filename and path based to the base directory */
            		sprintf(filename, "%s%s", help, digit);
            		CreateFile(buffer, filename);
			free(digit);
            		}                                                               
        	free(filename);
		/*one end of tree ready*/
        	(*actual)++;
        	}
	/*call function again -> create next layer*/
    	CreateTree(maxsub, help, maxdeep, deep, actual, buffer, maxfiles);

	/*if the number of needed ends of the tree are created -> dont go on*/
    	if ((*actual)>maxsub) { free(help); return 0; }
    
	/*creation of a new "one-branch"*/
	sprintf(help, "%s1/", path);
	if(mkdir(help, S_IRWXU)) { printf("\nCant create subdirectory %s\n", help); exit(127); }
	/*if maximum depth is reached -> write the files there*/
	if (deep==maxdeep)
        	{
        	filename=malloc(134*sizeof(char));
		if(filename==NULL) { printf("\nCant get memory to create filename!\n"); exit(127); }
		/*stop when directory has given number of files or when all files are written*/
        	for (i=0; i<FILESPERDIR && ((*actual)*FILESPERDIR+i)<maxfiles+1; i++)
            		{
			/*change number to (binary) string*/
            		digit=inttobinstring(i);
			/* "filename" contains filename and path based to the base directory */
            		sprintf(filename, "%s%s", help, digit);
            		CreateFile(buffer, filename);
			free(digit);
			}
        	free(filename);
		/*one end of tree ready*/
        	(*actual)++;
        	}
	/*call function again -> create next layer*/
	CreateTree(maxsub, help, maxdeep, deep, actual, buffer, maxfiles);

	free(help);
	return 0;
	}


/*buffer -> content of file that should be written, number -> number of files that should be written*/
void ioCreate(char *buffer, long number)
	{
	long i, j;
double FILESIZE=0.0, DISKSPACE=0.0, RAMSIZE=0.0 ;

int POTFILPERDIR=0, FILESPERDIR=0, FILESPERTHREAD=0, MAXREPEAT=0,
    REPEATSTOP=0, NUMCHANNELS=0, CHANNELFACTOR=0, TIMELIMIT=0 ;

char * DISKPATH=NULL; 
char * TMPHEADER=NULL;


iods * pmydata;

pmydata = (iods *) malloc(sizeof(iods));

evaluate_environment(pmydata);

FILESIZE = pmydata->FILESIZE;
POTFILPERDIR = pmydata->POTFILPERDIR ;
FILESPERDIR =  pmydata->FILESPERDIR ;
FILESPERTHREAD = pmydata->FILESPERTHREAD;
MAXREPEAT = pmydata->MAXREPEAT;
REPEATSTOP = pmydata->REPEATSTOP;
DISKPATH = (char *) malloc( sizeof( char ) * 128 );
DISKPATH = pmydata->DISKPATH ;
TMPHEADER = (char *) malloc( sizeof( char ) * 128 );
TMPHEADER = pmydata->TMPHEADER;
DISKSPACE = pmydata->DISKSPACE;
NUMCHANNELS = pmydata->NUMCHANNELS;
CHANNELFACTOR = pmydata->CHANNELFACTOR;
RAMSIZE = pmydata->RAMSIZE;
TIMELIMIT = pmydata->TIMELIMIT;



	i=0;
	/*from count -> calculation of the "depth" of the tree*/
    	j=(number>>POTFILPERDIR==0) ? 0 : (long)(log((double)(number>>POTFILPERDIR))/log(2))+1;
	/*call: number>>POTFILPERDIR -> structure of the tree that has to be 
	                                created
                0 -> structure that is already created

                "" -> momentarily path

                j -> maximum depth of the tree

                0 -> momentarily depth

                &i -> number of created ends (here:i=0)

                buffer -> content of the file(s) that should be written

                number -> number of files that should be created*/
    	CreateTree(number>>POTFILPERDIR, (char*)"", j, 0, &i, buffer, number);
    	}

/***********************************************************
* This is the complete log for this file:
*
* $Log: iobig_writefct.c,v $
* Revision 1.1  2007/04/26 02:33:04  william
* fixed errors due to old layout of Kernel (no more RUN.SH)
*
* Revision 1.3  2006/04/20 23:23:23  william
* let the bughuntseason begin
*
* Revision 1.2  2006/04/20 12:59:36  william
* moved iobigread.c to old_iobigread.c (new version namen kernel_main.c)
* created new file eval.c - check in is comming next
* eval.c includes everything concerning environment-stuff
*
* Revision 1.1.1.1  2006/04/18 10:03:50  william
* import version 0.1
*
* Revision 1.1.1.1  2004/12/14 21:22:56  william
* Release 3.0 - created new cvs-tree src2
*
*
***********************************************************/
