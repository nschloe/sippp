/*!
 * searches the fixpoint for the given parameters (f = (((a*f)+b) %m) )
 */

#include <stdlib.h>
#include "big_int.h"
#include <stdio.h>

int main(int argc, char** argv)
{
    int128 m;
    int128 a;
    int128 b;
    int128 f1,f2,tmp,fix;
    int128 x;
    int128 one;

    int res=0;

    unsigned long long progr;
    unsigned long long count=0;

    if (argc!=4)
    {
       printf("Program to find the fixpoint for given parameters m,a and b\nusage: ./fixpoint m a b\n");exit(-1);
    }

    int128_from_string(&m,argv[1]);
    int128_from_string(&a,argv[2]);
    int128_from_string(&b,argv[3]);
    int128_from_int(&x,0,0,0,0);
    int128_from_int(&one,0,0,0,1);

    int128_from_int(&tmp,0,0,0,50);
    int128_div(a,tmp,&tmp);
    progr=int128_getlower(tmp);
    if (progr==0) progr=1;
    count=0;

    int128_sub(a,one,&tmp);

    printf("m= %s, a= %s, b= %s\n",int128_toString(m,10,39),int128_toString(a,10,39),int128_toString(b,10,39));

    for(;int128_unsigned_compare(x,a)!=1;int128_add(x,one,&x))
    {
        //f1 = ((x*m) -b ) / (a-1)
        int128_mult(x,m,&f1);
        int128_sub(f1,b,&f1);
        int128_div(f1,tmp,&f1);

        //f2 = ((f1*a)-b) % m
        int128_mult(f1,a,&f2);
        int128_add(f2,b,&f2);
        int128_mod(f2,m,&f2);

        count++;
        if (count==progr) {count=0;printf(".");fflush(stdout);}

        if (int128_unsigned_compare(f1,f2)==0) 
        {
            //printf("\nx=%s: f1=%s, f2=%s\n",int128_toString(x,10,39),int128_toString(f1,10,39),int128_toString(f2,10,39));
            int128_copy(f1,&fix);
            res++;
        }
    }

    if (res==1) printf("\nfound Fixpoint: %s\n",int128_toString(fix,10,39));
    else if (res>1) printf("\nERROR: Found multiple Fixpoints (-> bad parameters)\n");
    else printf("\nno Fixpoint found\n");

    return 0;
}
