/*!
 * Program displays prime factor breakdown of the given number
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>


int *p_list;
int p_list_max;

int parts [2][64];
int part_count=0;
int pos=0;
long long number;
int max_factor;

int isfact(int value)
{
  int i;
  int limit = (int) trunc(sqrt((double) value)) +1;
  for (i=0;i<=pos;i++)
  {
      if (p_list[i]>limit) break;
      if (value%p_list[i]==0) return 0;
  }
  if (pos < p_list_max -1)
  {
     pos++;
     p_list[pos]=value;
  }
  else
   if (p_list[pos]<limit) 
      for (i=p_list[pos];i<=limit;i+=2)
      {
        if (value%i==0) return 0;
      }

  // if this point is reached value is a prime number
  // next we check if it devides the given number

  if (number%value==0)
  {
      parts[0][part_count]=value;parts[1][part_count]=0;
      while (number%value==0)
      {
        number=number/value;
        parts[1][part_count]++;
      }
      printf(" * %i^%i",parts[0][part_count],parts[1][part_count]);fflush(stdout);
      part_count++;

     max_factor = (int) trunc(sqrt((double) number))+1;
  }

  return 1;
}

int main(int argc, char** argv)
{  
  long long i;

  if (argc!=2)
  {
     printf("Prime factor breakdown\nusage ./factor number\n");
     return -1;
  }
  else number=atoll(argv[1]);

  printf("%lli = 1",number);fflush(stdout);

  if (number%2==0)
  {
      parts[0][0]=2;parts[1][0]=0;
      while (number%2==0)
      {
        number=number/2;
        parts[1][0]++;
      }
      part_count++;
      printf(" * %i^%i",parts[0][0],parts[1][0]);fflush(stdout);
  }

  max_factor = (int) trunc(sqrt((double) number))+1;
  p_list_max=max_factor/2+1;
  p_list=malloc(p_list_max*sizeof(int));

  i=3;p_list[0]=2;
  while (i<max_factor)
  {
     isfact(i);
     i+=2;
  }
  if (number>1) printf(" * %lli^%i\n",number,1); else printf("\n"); fflush(stdout);
}
