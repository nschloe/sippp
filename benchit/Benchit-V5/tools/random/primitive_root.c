/*!
 * Program searches primitive roots for a giveb prime number
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "big_int.h"


int *p_list;
int p_list_max;

long long parts [64];
int part_count=0;
int pos=0;
long long number,num;
int max_factor;

int isfact(int value)
{
  int i;
  int limit = (int) trunc(sqrt((double) value))+1;
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

  // if we reach this point value is a prime number
  // next we check if it devides the given number

  if (number%value==0)
  {
      parts[part_count]=value;
      while (number%value==0)
      {
        number=number/value;
      }
      part_count++;

     max_factor = (int) trunc(sqrt((double) number))+1;
  }

  return 1;
}

unsigned long long potenz(long long x, long long y, long long m)
{
   unsigned long long res;
   unsigned long long mask = 1;
   int128 res_128,x_128,m_128;

   if (y==0) return x%m;

   printf("%llu ^ %llu = ",x,y);fflush(stdout);

   int128_from_int(&res_128,0,0,0,1);
   int128_from_longlong(&x_128,0,x);
   int128_from_longlong(&m_128,0,m);

   mask = mask<<63;

   while (y&mask==0) mask= mask >> 1;

   do
   {
        if (y&mask)
        {
            int128_mult(res_128,x_128,&res_128);    //res=res*x
            int128_mod(res_128,m_128,&res_128);     //res=res%m
            int128_mult(res_128,res_128,&res_128);  //res=res*res
            int128_mod(res_128,m_128,&res_128);     //res=res%m
        }
        else
        {
            int128_mult(res_128,res_128,&res_128);  //res=res*res
            int128_mod(res_128,m_128,&res_128);     //res=res%m
        }
	mask = mask >> 1;
   }
   while (mask>1);

   if (y&mask)
   {
      int128_mult(res_128,x_128,&res_128);    //res=res*x
      int128_mod(res_128,m_128,&res_128);     //res=res%m
   }

   res=int128_getlower(res_128);

   printf("%s mod %llu\n",int128_toString(res_128,10,20),m);

   return res;
}

int isroot(long long value)
{
  long long i,j,q,g,p,tmp;
  for (i=0;i<part_count;i++)
  {
      g = value;
      q = parts[i];
      p = num;
      if ((potenz(g,(p-1)/q,p))==1) return 0;
  }
  return 1;
}

int main(int argc, char** argv)
{  
  long long i;

  if (argc!=2)
  {
     printf("Program searches primitive roots for the given prime number\nusage ./prim_root primenumber\n");
     return -1;
  }
  else number=atoll(argv[1]);num=number;number--;

  if (number%2==0)
  {
      parts[0]=2;
      while (number%2==0)
      {
        number=number/2;
      }
      part_count++;
  }

  max_factor = (int) trunc(sqrt((double) number))+1;
  p_list_max=max_factor/2+1;
  p_list=malloc(p_list_max*sizeof(int));

  printf("Primfactorzerlegung ...");fflush(stdout);

  i=3;p_list[0]=2;
  while (i<max_factor)
  {
     isfact(i);
     i+=2;
  }
  if (number>1)
  {
     parts[part_count]=number;
     part_count++;
  }
  printf (" ok\n");
  for (i=part_count;i>0;i--)
  {
     printf("%lli - testing ...\n",parts[i-1]);fflush(stdout);
     if (isroot(parts[i-1])) printf ("...OK\n"); else printf ("...failed\n");
  }
}
