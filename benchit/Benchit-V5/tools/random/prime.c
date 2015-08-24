/*!
 * Program to find prime numbers
 * Author: Daniel Molka (Daniel.Molka@zih.tu-dresden.de)
 * This program will display all prime numbers between the specified min and max values
 * - The Program will start with creating a table of all prime numbers smaller or equal to sqrt(max)
 * - In the second step all odd values between min and max will be tested if they can be divided by one
 *   of the known prime numbers from the table created in step one.
 *
 * LIMITATIONS:
 * - The Programm can not handle missing memory for temporary storing the results. if that is a problem you
 *   will have to do multiple runs with smaller intervals (min - max)
 * - Missing memory for the table of prime numbers will be handeled automatically, but performance could suffer
 */

#include <stdio.h>

/* sqrt, trunc (needs -lm to complie)*/
#include <math.h>
/* atoll*/
#include <stdlib.h>
/* strcmp*/
#include <string.h>

/*! memory for known prime numbers */
unsigned int *p_list=NULL;
unsigned int p_list_max;   /* size of the array*/
unsigned int pos=0;        /* actual number of elements*/

/*! memory for results */
unsigned long long *result=NULL;
unsigned int anz=0;        /* actual number of elements*/

/*! variables for simple progress bar*/
unsigned int progr;
int count=0;

/*! influences amount of output */
int quiet=0;

/*!
 * tests if the given number is a prime number and puts it into the array "p_list" if it is prime
 * this function is used by main() to build a table of prime numbers up to sqrt(max)
 * p_list has to be initialized with "p_list[0]=2;pos=0;" before calling this function.
 * (the function needs to know that 2 is prime to calculate further prime numbers)
 */
int test(unsigned int value)
{
  int i;
  int limit = (int) trunc(sqrt((double) value)) +1;
  for (i=0;i<=pos;i++)
  {
      if (p_list[i]>limit) break;
      if (value%p_list[i]==0) return 0;
  }
  pos++;
  if (pos < p_list_max)
  {
     p_list[pos]=value;
     return 0;
  }
  else 
  {
    pos--;
    return 1; /* array full -> loop in main() will break;*/
  }
}

/*!
 * Tests if the given number is prime using the prebuild table of known
 * prime numbers. (does not work for 2!)
 * p_list has to be initialized at least with "p_list[0]=2;pos=0;"
 */
int isprime(long long value)
{
  int i;
  int limit = (int) trunc(sqrt((double) value))+1;
  for (i=0;i<=pos;i++)
  {
      if (p_list[i]>limit) break;
      if (value%p_list[i]==0) return 0;
  }
  if (p_list[pos]<limit)  		/* could happen if we don't have enough memory for p_list to store all prime
                                           numbers up to sqrt(max) */
    for (i=p_list[pos];i<=limit;i+=2)
      {
        if (value%i==0) return 0;
      }

  result[anz]=value;
  anz++;

  return 1;
}

/*!
 * Main function
 */
int main(int argc,char** argv)
{
  unsigned long long i,min,max,limit;

/* reed arguments */
  if ((argc!=3)&&(argc!=4))
  {
    printf("prints out all prime numbers between min and max.\nusage: ./prime min max [-q]\n       -q will reduce output\n");
    return -1;
  }
  else
  { 
    if ((argc==4)&&(!strcmp(argv[3],"-q"))) quiet=1;
    min= atoll(argv[1]);
    if ((min%2)==0) min++;
    if (min==1) 
    {
      if (!quiet) printf("min value to small has been set to 3\n");
      min=3;
    }
    max = atoll(argv[2]);
    if (max < min) max=min;
    if (!quiet) printf("min= %lli, max= %lli\n",min,max);
  }
/* end reed arguments */

  /* calculating highest prime number we need to know for testing up to max */
  limit = (int) trunc(sqrt((double) max))+1;
  /* results of sqrt may be incorrect because of limited precision of double -> additional check needed */
  while (((unsigned long long) limit * limit)<max) limit++;

  /* rough guess for amount of prime numbers smaller than max */
  p_list_max=limit/2;

/* reserve memory */
  result=malloc(((max-min+1)/2+1)*sizeof(long long));
  if (result==NULL) {printf("Error: could not reserve enough memory for results\n");fflush(stdout);exit(-1);}
  else if (!quiet) printf("reserved %i Bytes for results\n",((max-min+1)/2+1)*sizeof(long long));

  while (p_list==NULL)
  {
     p_list=malloc(p_list_max*sizeof(int));
     if (p_list==NULL) 
     {
        if (p_list_max==1) {printf("Error: could not reserve enough memory for prime number table\n");fflush(stdout);exit(-1);}
        if (!quiet) printf("Warning: could not reserve %i Bytes - trying smaller table\n",p_list_max*sizeof(int));fflush(stdout);
        p_list_max=p_list_max/2;
     }
     else if (!quiet) printf("reserved %i Bytes for prime number table\n",p_list_max*sizeof(int));
  }
/* end reserve memory*/

  /* initialize p_list */
  p_list[pos]=2;

  if (!quiet) {printf("building table: ");fflush(stdout);}

  /* setup progress bar for step 1 */
  progr=p_list_max/50;
  if (progr==0) progr=1;
  count=0;

  /* building table of prime numbers */
  for (i=3;i<=limit;i+=2)
  {
     count++;
     if (test(i)) break;
     if (count==progr) {count=0;if (!quiet) {printf(".");fflush(stdout);}}
  }

  if (!quiet) {printf("\nsearching:      ");fflush(stdout);}

  /* setup progress bar for step 2 */
  progr=(max-min)/100;
  if (progr==0) progr=1;
  count=0;

  /* testing all numbers between min and max */
  for (i=min;i<=max;i+=2)
  {
    count++;
    isprime(i);
    if (count==progr) {count=0;if (!quiet) printf(".");fflush(stdout);}
  }
  printf("\n");

  /* display results */
  for (i=0;i<anz;i++)
  {
     if (quiet) printf("%lli \n",result[i]); else printf(":- %14lli \n",result[i]);
  }
}
