/*!
 * small test suite for the big_int library
 */
#include "big_int.h"
#include <stdio.h>

int main(int argc, char ** argv)
{
  int128 test1,test2,test3,test4;
  unsigned long long tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8;
  int err,res;

  int128_from_int(&test1,0,0,1,0xfffffffe);
  int128_from_int(&test2,0,0,0,0xffffffff);
  int128_from_int(&test3,0x7fffffff,0xffffffff,0xffffffff,0xffffffff);

 // int128_from_string(&test1,"10");
 // int128_from_string(&test2,"-19");
 // int128_from_string(&test3,"0");

  //int128_neg(&test1);
  //int128_neg(&test2);

  printf("hex:\n");

  printf("  test1: %s, upper= %016llx, lower= %016llx\n",int128_toString(test1,16,32),int128_getupper(test1),int128_getlower(test1));
  printf("  test2: %s, upper= %016llx, lower= %016llx\n",int128_toString(test2,16,32),int128_getupper(test2),int128_getlower(test2));
  printf("  test3: %s, upper= %016llx, lower= %016llx\n",int128_toString(test3,16,32),int128_getupper(test3),int128_getlower(test3));

  printf("dezimal:\n");

  printf("  test1: %s\n",int128_toString(test1,10,39));
  printf("  test2: %s\n",int128_toString(test2,10,39));
  printf("  test3: %s\n",int128_toString(test3,10,39));

  printf("test:\n");

   res=int128_unsigned_compare(test1,test2);

   if (res==0) printf(" %s = %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39));
   if (res==-1) printf(" %s < %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39));
   if (res==1) printf(" %s > %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39));

    err = int128_add(test1,test2,&test3);
    if (err) {
                printf("   ERROR: add :: %i\n",err);//exit(-1);
             }
    printf (" %s + %s = %s\n",int128_toString(test1,16,32),int128_toString(test2,16,32),int128_toString(test3,16,32));
    printf (" %s + %s = %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39),int128_toString(test3,10,39));

    err = int128_sub(test1,test2,&test3);
    if (err) {
                printf("   ERROR: sub :: %i\n",err);//exit(-1);
             }
    printf (" %s - %s = %s\n",int128_toString(test1,16,32),int128_toString(test2,16,32),int128_toString(test3,16,32));
    printf (" %s - %s = %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39),int128_toString(test3,10,39));

    err = int128_mult(test1,test2,&test3);
    if (err) {
                printf("   ERROR: mul :: %i\n",err);//exit(-1);
             }
    printf (" %s * %s = %s\n",int128_toString(test1,16,32),int128_toString(test2,16,32),int128_toString(test3,16,32));
    printf (" %s * %s = %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39),int128_toString(test3,10,39));

    err = int128_div(test1,test2,&test3);
    if (err) {
                printf("   ERROR: div :: %i\n",err);//exit(-1);
             }
    printf (" %s / %s = %s\n",int128_toString(test1,16,32),int128_toString(test2,16,32),int128_toString(test3,16,32));
    printf (" %s / %s = %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39),int128_toString(test3,10,39));

    err = int128_mod(test1,test2,&test3);
    if (err) {
                printf("   ERROR: mod :: %i\n",err);//exit(-1);
             }
    printf (" %s %% %s = %s\n",int128_toString(test1,16,32),int128_toString(test2,16,32),int128_toString(test3,16,32));
    printf (" %s %% %s = %s\n",int128_toString(test1,10,39),int128_toString(test2,10,39),int128_toString(test3,10,39));
}
