/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*
*  <Description>
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

#include "iobig_writefct.h"
#include "interface.h"
#include "iobigread.h"
#include "eval.h"

int main(int argc, char** argv)
	{
	const char *path;
	char *start, *destination, *buffer;
	FILE *fp;
	long i;
	double ds, fs;
double FILESIZE=0.0, DISKSPACE=0.0, RAMSIZE=0.0 ;

int POTFILPERDIR=0, FILESPERDIR=0, FILESPERTHREAD=0, MAXREPEAT=0,
    REPEATSTOP=0, NUMCHANNELS=0, CHANNELFACTOR=0, TIMELIMIT=0;

char * DISKPATH=NULL; 
char * TMPHEADER=NULL;

iods * pmydata;



printf("\ncreate\n"); fflush(NULL);

pmydata = (iods *) malloc(sizeof(iods));

evaluate_environment(pmydata);

printf("\nevaluate\n"); fflush(NULL);

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
	/*ds => number of files that have to be created*/
	
	/* ds=DISKSPACE/FILESIZE; */
	ds=DISKSPACE;
	fs=FILESIZE;
	ds=ds/fs;

printf("PATH=%s",DISKPATH);

	start=malloc(4096*sizeof(char));
	if(start==NULL) { printf("\nCant get memory to save execution path!\n"); return 1; }
	getcwd(start, 4096);

	path=DISKPATH;
	if(path==NULL) { printf("\nCant get path for writing!\n"); return 1; }

	/*iobig.tmp is created to save datas for the read and the remove program*/	
	destination=malloc((strlen(path)+32)*sizeof(char));
	if(destination==NULL) { printf("\nCant get memory for creating tmp-file path!\n"); return 1; }
	sprintf(destination, "%siobig.tmp", path);

	/*if the file that should be created already exists => try another name*/
	while(fopen(destination, "r")!=NULL)
		{
		i++;
		sprintf(destination, "%siobig%ld.tmp", path, i);
		}
	
	printf("Creating temporary file %s\n", destination);
	fp=fopen(destination, "w");
	if(fp==NULL) { printf("\nCant create tmp-file! Maybe no write properties in provided path.\n"); return 1; }
	fprintf(fp, TMPHEADER); fprintf(fp, "\n");

	/*write indentify-string, created directory, filenumber and filesize to tmp-file*/
	sprintf(destination, "%stmp%ld/", path, (long)(time(NULL)));
	fprintf(fp, "%s", destination); fprintf(fp, "\n");
	fprintf(fp, "%f %f", ds, fs);
	fclose(fp);
	if(mkdir(destination, S_IRWXU)) { printf("\nCant create path: %s\n!", destination); return 1; }

	/*create the file that should be written*/
	buffer=malloc(FILESIZE);
        if(buffer==NULL) { printf("\nCant get memory for filecontent!\n"); return 1; }

	for(i=0;i<((long)(FILESIZE/sizeof(char)));i++) buffer[i]=(char)(rand()%255);

	if(chdir(destination)) { printf("\nCant change to directory %s\n", path); return 1; }

	/*create binary-tree with the files*/
	printf("Creating %ld files for the Benchmark\n", (long)(ds));
        ioCreate(buffer, (long)(ds-1));
	
	if(chdir(start)) printf("\nCant change to directory %s\n.", start);
	
	free(destination);
	free(start);
	return 0;
	}

/***********************************************************
* This is the complete log for this file:
*
* $Log: iocreate.c,v $
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
* Revision 2.0  2003/12/09 11:18:44  juckel
* build of version 2.0
*
* Revision 1.7  2003/09/10 07:08:39  william
* simple readding done
*
* Revision 1.5  2003/05/23 20:01:46  william
* made too many changes to name them all - but here are few samples:
* -corrected ARCHDEFS
* -rearanged all io-shellscripts
* -killed a few bucks
* -thought out a few new values
* ...
*
* Revision 1.4  2003/05/21 17:53:35  william
* some changes to fullfill the "basic-units" recommendation
*
* Revision 1.3  2003/04/25 08:17:13  william
* hopefully solved the "a char is'nt always a char" problem cause
* there are systems where a char is not 1 but 2 bytes long and
* therefor you'll need 2xHDspace for the kernels
* -should be solved by now :]
*
* Revision 1.2  2003/04/02 15:31:44  william
* added the Log-writing
*
***********************************************************/
