/*!
 * tests the distribution of the random number generator by approxymating pi
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "random.h"


int main()
{ 
  unsigned long long current;
  unsigned long long cycle=0;
  double pi;
  unsigned long long x,y;
  double in,out;
  unsigned long long radius=400000000000;
  int pos=0;
  unsigned long long startvalue=(unsigned long long)174352;

 bi_random_init(startvalue,radius+1);

 do
 {
   pos++;
   cycle++;

   x=bi_random48();
   y=bi_random48();

   if ((sqrt(((double)x*(double)x)+((double)y*(double)y)))<(double)radius) in++; else out++;
   pi = in/(in+out)*4;

   if (pos==1000000)
   {
     printf("%6lli million calls of bi_random(): pi= %10.9f\n",cycle/500000,pi);
     pos=0;
   }
 }
 while (cycle<10000000);

 // length of the sequence
 long long length;
 // number of intervalls
 long long amount;
 // size of intervalls
 long long size;

 // counters
 int *counter;

 long long min,max,i;
 long long tmp;

 for (length=1000;length<=1100000000;length*=2)
 {
   //printf("testing with sequence length %i\n",length);
   for (amount=2;amount<=17000;amount*=4)
   {
	//printf("  using %li intervalls\n",amount);
        counter=malloc(sizeof(int)*amount);
	for (size=1;size<=1000000;size*=1000)
        {
	    //printf("       size of intervalls = %i\n",size);
            memset(counter,0,sizeof(int)*amount);
            min=length;max=0;
            bi_random_init(startvalue,amount*size);
            for(i=0;i<=length;i++)
            {
                tmp=bi_random48()/size;
		counter[tmp]++;
                if (max<counter[tmp]) max=counter[tmp];
            }
            for(i=0;i<amount;i++) if (counter[i]<min) min=counter[i];
            printf("length: %8i, %5i intervals of size %8i -> min=%8i, max=%8i, ratio %5.4f\n",(int)length,(int)amount,(int)size,(int)min,(int)max,(double)min/(min+max));
        }
	free(counter);
   }
 }


 return 0;
}
