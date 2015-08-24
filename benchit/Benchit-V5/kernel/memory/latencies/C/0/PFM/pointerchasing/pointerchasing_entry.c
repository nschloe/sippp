/***********************************************************************
*
* B e n c h I T - Performance Measurement for Scientific Applications
*
* Core for Memory Access Time (C)
*
* Author: Michael Kluge (kluge@zhr.tu-dresden.de)
*
* Version: 1.0
*
* Date:  2002/12/27
*  Start: 2002/09/27
*
***********************************************************************/

#include "interface.h"
#include "pointerchasing.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <perfmon/pfmlib.h>

#include <perfmon/perfmon.h>
#include <perfmon/pfmlib_itanium2.h>

#define ONE {ptr=(void **) *ptr;}
#define TEN ONE ONE ONE ONE ONE ONE ONE ONE ONE ONE
#define HUN TEN TEN TEN TEN TEN TEN TEN TEN TEN TEN
#define THO HUN HUN HUN HUN HUN HUN HUN HUN HUN HUN


extern long numjumps;
extern int NUM_COUNTERS;
extern char sCounters[10][100];

int pfm_easy_init(int nCounters,char **sCounters);
void pfm_easy_read(int fd, long val[]);

void *jump_around( void *mem, long n);


/** generates a random number between 0 and (max-1)
 *  @param  max maximum random number
 *  @return a random number between 0 and (max-1)
 */
unsigned long random_number( unsigned long max)
{
  return (unsigned long) (((double)max)*rand()/(RAND_MAX+1.0));
}


/** creates a memory are that is randomly linked 
 *  @param mem     the memory area to be used
 *  @param length  the number of bytes that should be used
 */
void make_linked_memory( void *mem, long length) {

  /* some pointers to generate the list */
  void **ptr, **first;
  /** how many ptr we create within the memory */
  long num_ptr=length/sizeof(void *);
  /** the list for all memory locations that are linked */
  long *ptr_numbers;
  /** for the loops */
  long loop_ptrs;
  /** actual random number */
  long act_num;

  /* allocate memory for ptr numbers */
  ptr_numbers=(long *) malloc(num_ptr*sizeof(long));
  if( num_ptr>0 && ptr_numbers==NULL)
    {
      printf("no more core in make_linked_mem(): %ld\n", num_ptr);
      bi_cleanup(mem);
      exit(1);
    }
  /* initialize ptr numbers, the 0 is used as the first
   * number
   */
  for( loop_ptrs=1; loop_ptrs<num_ptr; loop_ptrs++)
    ptr_numbers[loop_ptrs-1]=loop_ptrs;

  /* init first ptr with first memory location */
  ptr=(void **)mem;
  first=ptr;
   
  num_ptr--;

  while( num_ptr>1 ) {
    /* get a random position within the
       remaining list */
    act_num=random_number(num_ptr);
    /* create a link from the last ptr 
       to this ptr */
    *ptr=(void *) (first+ptr_numbers[act_num]);
    /* move pointer to new memory location */
    ptr=first+ptr_numbers[act_num];
    /* remove used ptr number from list of
       pointer numbers, just copies the last 
       number to the actual position */
    ptr_numbers[act_num]=ptr_numbers[num_ptr-1];
    num_ptr--;
  }

  /* the last number is linked to the first */
  *ptr=(void *) first;

  /* free the ptr list */
  free(ptr_numbers);
  IDL(4,printf("\n"));
}


int bi_entry(void *mcb,int problemsize,double *results) {

	static double timeroh=0, calloh=0;
	double start, stop;
	int i;
	int f;
	long length;
	long long c[4];
	void *ptr;
	
	extern double dMemFactor;
	extern long minlength;

	if (dMemFactor >0)
		length = (long)(((double)minlength)*pow(dMemFactor, (problemsize-1)));
	else
		length = minlength - (problemsize-1)*1024*(long)dMemFactor;
	results[0]=(double) length;


	IDL( 2, printf("Making structure\n"));
	make_linked_memory( mcb, length);
	IDL( 2, printf("Enter measurement\n"));
	
	ptr = jump_around( mcb, numjumps); 	

	fd = pfm_easy_init(NUM_COUNTERS,sCounters);
	if (!fd) {
		printf("failure\n");
			exit(0);
	}
	pfm_self_start(fd);		
	start=bi_timer();

	ptr = jump_around( ptr, numjumps );
 	
	stop=bi_timer();
	
	pfm_self_stop(fd);
	pfm_easy_read(fd, c);
	
	
	
	if ((long)ptr == 0)
		printf("#");	
	
	IDL( 2, printf("Done\n"));
		
	 results[1]=(double)(stop-start)/((double)numjumps);
    for ( i =0; i< NUM_COUNTERS; i++)
	 	results[i+2]=(double)(c[i]);
	

  return (0);
}

/*****************************************************************************

LOG-History

$Log: pointerchasing_entry.c,v $
Revision 1.3  2005/11/22 01:26:51  mickler
+ Using BI_GET_CALL_OVERHEAD_FUNC macro now
+ Using bi_timer() function for time measuring

Revision 1.2  2005/09/16 06:08:14  mark
*** empty log message ***

Revision 1.3  2005/09/16 05:42:25  mark
*** empty log message ***

Revision 1.2  2005/09/13 12:49:25  mark
PAPI counter in PARAMETERS file
near stepsize

Revision 1.1  2005/09/02 12:14:47  mark
*** empty log message ***

Revision 1.1  2005/09/02 11:44:11  mark
*** empty log message ***

Revision 1.2  2005/08/24 12:23:20  juckel
added NUMBER_OF_JUMPS to PARAMETERS

Revision 1.1  2005/08/16 14:20:53  juckel
- included kernel from old benchit into new structure
- changed MPI-functionality
  old: only root process runs memory test -> generates 1 data set
  new: for i=1 to numproc do ruu mesurement -> generates numproc data sets

Revision 2.0  2003/12/09 11:19:01  juckel
build of version 2.0

Revision 1.1  2003/12/01 20:52:03  kluge
memaccess_c initial checkin

Revision 1.5  2003/05/06 10:53:49  kluge
latest changes for new interface.h done

Revision 1.4  2003/04/13 15:11:26  juckel
Bugfix in function jump_around: \n- incompatible declarations in cache_test.h and cache_jump.c\n- return w./o. a value in function jump_around

Revision 1.3  2003/03/14 20:59:51  kluge
updated romulus LOCALDEFS and parallel matrix multiply

Revision 1.2  2003/01/28 11:29:19  kluge
new header



*****************************************************************************/
