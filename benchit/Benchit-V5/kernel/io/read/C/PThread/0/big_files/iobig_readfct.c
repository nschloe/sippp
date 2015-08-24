/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <the readfunction>
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
#include "iobig_readfct.h"
#include "iobigread.h"

/*this function takes over a number and the length of the path 
  and gives back a string with the path that belongs to the number*/
char *makenumtopath(long num, long digits, char* filename)
	{
	long pos, i, j;
	/*this variable checks the delivered number and checks it digit 
	  by digit for its binary value*/
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

	pos=1;
	/*(char)array is filled from behind!*/
	filename[127]=0;
	/*i=length of the string that is returned*/
	i=2*digits-POTFILPERDIR;

	for(j=0; j<i; j++)
	        {
		/*changing the binary number into string digit by digit*/
	        filename[126-j]=(pos & num) ? '1' : '0';
		/*=> next position*/
	        pos<<=1;
		/*if it is not the filename anymore => then it is a path*/
	        if (j>=(POTFILPERDIR-1)) { j++; filename[126-j]='/'; }
        	}

	/*begin of the path: "./.../..."*/
	filename[126-j]='.';

	/*return of the path as a string*/
	return (filename+126-j);
	}



void readfiles(long problemsize, long maxdeep, double *btime, double *etime)
        {
        char *filename, *filenamebuffer, *buffer;
        FILE *fp;
        long i, filenumber;
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


	/*buffer for read filecontent*/
        buffer=malloc(FILESIZE);
	if(buffer==NULL) { printf("\nCant get memory to read testfile!\n"); exit(127); }

	/*buffer for all filenames that are read at one call*/	
	filenamebuffer=malloc(128*FILESPERTHREAD*sizeof(char));
	if(filenamebuffer==NULL) { printf("\nCant get memory to recover Filename!\n"); exit(127); }

	/*reading of the files and time measurement*/
	*btime=bi_gettime();
	/*FILESPERTHREAD files are read at one call*/
        for(i=0;i<FILESPERTHREAD;i++)
    	    	{
		/*selection of one file (depending on simulated diskspace)*/
	    	filenumber=rand()%problemsize;
		/*converting the (file)number into the appendant position in 
		the tree*/
	    	filename=makenumtopath(filenumber, maxdeep+POTFILPERDIR, (filenamebuffer+128*(FILESPERTHREAD-i-1)));
		/*open file*/
           	fp=fopen(filename, "r");
	    	if (fp==NULL) {printf("File %s not found!\n", filename); exit(127);}
		/*read file*/
    	    	n=fread(buffer, sizeof(char), (size_t)(FILESIZE/sizeof(char)), fp);
		if(n!=FILESIZE) printf("Error: A fread didnt return regular filesize but %ld. This will affect results!\n", n);
		/*close file*/
            	fclose(fp);
    	    	}
    	*etime=bi_gettime();
        free(buffer);
	free(filenamebuffer);
/*        free(filename);*/
        }

/***********************************************************
* This is the complete log for this file:
*
* $Log: iobig_readfct.c,v $
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
