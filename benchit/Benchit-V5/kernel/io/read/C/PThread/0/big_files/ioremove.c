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

#include "interface.h"
#include "iobigread.h"
#include "eval.h"


int main(int argc, char** argv)
	{
	const char *path, *tmpheader;
	char *destination, *fileheader, *remove, *systemstring;
	long i;
	double ds, fs, dsz, fsz;
	FILE *fp;
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


	/*getting information to find and identify file*/
	i=0;
	dsz=DISKSPACE;
	fsz=FILESIZE;
	dsz=dsz/fsz;
	tmpheader=TMPHEADER;
	if(tmpheader==NULL) {printf ("\n Cant get header of tmp-file\n"); return 1;}
	path=DISKPATH;
        if(path==NULL) { printf("\nCant get path for writing!\n"); return 1; }
	
	/*destination -> tmp file*/
	destination=malloc((strlen(path)+32)*sizeof(char));
        if(destination==NULL) { printf("\nCant get memory for reading tmp-file!\n"); return 1; }
	sprintf(destination, "%siobig.tmp", path);

	remove=malloc(4096*sizeof(char));
	if(remove==NULL) { printf("\nCant get memory for reading path!\n"); return 1; }

	fileheader=malloc(1024*sizeof(char));

	/*searching a tmp file with fitting content*/
	for(;;)
		{
		fp=fopen(destination, "r");
		if(fp==NULL) 
			{ 
			printf("file with number %ld not found \n", i); 
			i++; 
			if(i>1024) { printf("\nNo fitting tmp-file found! Sorry, dont know what to clean up.\n"); return 1; }
			sprintf(destination, "%siobig%ld.tmp", path, i);
			fp=fopen(destination, "r");
			continue;
			}
		i++;
		fscanf(fp, "%[^\n]", fileheader); fgetc(fp);
		fscanf(fp, "%[^\n]", remove); fgetc(fp);
		fscanf(fp, "%lf %lf", &ds, &fs); 
		/*printf("%s\n%s\n%lf %lf\n", fileheader, remove, ds, fs);*/
		fclose(fp);
		if((long)ds==(long)dsz && fs==fsz && !(strcmp(tmpheader, fileheader))) break;
		sprintf(destination, "%siobig%ld.tmp", path, i);
		} 
		
	printf("Found fitting tmp-file.\n");

	/*remove files and tree*/
	systemstring=malloc(5012*sizeof(char));
	if(systemstring==NULL) printf("\nOut of memory. Cant clean up files.\n");
	sprintf(systemstring, "rm -r %s*", remove);
	printf("performing %s\n", systemstring);
	system(systemstring);

	/*remove directory*/
	sprintf(systemstring,"rmdir %s", remove);
	printf("performing %s\n", systemstring);
	system(systemstring);

	/*remove tmp file*/
	sprintf(systemstring,"rm %s", destination);
	printf("performing %s\n", systemstring);
	system(systemstring);

	free(systemstring);
	free(remove);
	free(destination);
	free(fileheader);
	return 0;
	}

/***********************************************************
* This is the complete log for this file:
*
* $Log: ioremove.c,v $
* Revision 1.1  2007/04/26 02:33:04  william
* fixed errors due to old layout of Kernel (no more RUN.SH)
*
* Revision 1.4  2006/04/27 09:30:19  william
* some minor bugs
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
