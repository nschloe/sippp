/*! Little library providing some basic operations for 128-Bit Integers
 *  Author: Daniel Molka (Daniel.Molka@zih.tu-dresden.de)
 *  tested only on x86!
 *  assumptions: int       : 32Bit
 *               long long : 64Bit
 */

#include "big_int.h"
#include <stdlib.h>
#include <string.h>

/*!
 * stores the overflow of an addition or multiplication
 */
static int128 overflow;

/*!
 * @brief creats an 128-Bit Integer from 4 32-Bit Integers
 * @param[in] part0  Bits 127 .. 96 
 * @param[in] part1  Bits 95  .. 64
 * @param[in] part2  Bits 63  .. 32
 * @param[in] part3  Bits 31  ..  0
 * @param[out] dest  the 128-Bit Integer
 */
void int128_from_int(int128 *dest,unsigned int part0, unsigned int part1, unsigned int part2, unsigned int part3)
{
  (*dest).part[0]=part0;
  (*dest).part[1]=part1;
  (*dest).part[2]=part2;
  (*dest).part[3]=part3;
}

/*!
 * @brief creats an 128-Bit Integer from 2 64-Bit Integers
 * @param[in] part01 Bits 127 .. 64 
 * @param[in] part23 Bits 63  ..  0
 * @param[out] dest  the 128-Bit Integer
 */
void int128_from_longlong(int128 *dest,unsigned long long part01,unsigned long long part23)
{
  (*dest).part[0]=(unsigned int)(part01>>32);
  (*dest).part[1]=(unsigned int)part01;
  (*dest).part[2]=(unsigned int)(part23>>32);
  (*dest).part[3]=(unsigned int)part23;
}

/*!
 * @brief creats an 128-Bit Integer from a String
 * @param[in] src String containing a signed decimal number
 * @param[out] dest  the 128-Bit Integer
 * @return -1 in if the number in the String is too large (dest will be 0)
 *         -2 if the string contains characters other than {0,...,9} or '-' as first character (dest will be 0)
 *          0 if no error occurs
 */
int int128_from_string(int128 *dest, char* src)
{
   int sign=0,i=0,tmp;
   int128 tmp1,tmp2,base;

   int128_from_int(&tmp1,0,0,0,0);
   int128_from_int(dest,0,0,0,0);  // dest will be 0 in case of an error
   int128_from_int(&base,0,0,0,10);// String is treated as decimal number

   if (src[0]=='-') {sign=1;i++;}

   while (src[i]!='\0')
   {
        int128_mult(tmp1,base,&tmp1);
        tmp=(int)src[i]-48;
        if ((tmp<0)||(tmp>10)) return -2;  // illegal character
        int128_from_int(&tmp2,0,0,0,tmp);
        i++;
        int128_add(tmp1,tmp2,&tmp1);
        if (tmp1.part[0]&0x80000000) return -1; // overflow (first bit is reserved for sign)
   }

   if (sign) int128_neg(&tmp1);

   int128_copy(tmp1,dest);

   return 0;
}

/*!
 * @brief sets one bit
 * @param[in/out] num the 128-Bit number that should be modified
 * @param[in] pos the Bit-position within the number
 * @param[in] bit 1 if the Bit should be set to 1
 *                0 if the Bit should be set to 0
 */
void int128_setbit(int128 *num,int pos,int bit)
{
   int128 mask;

   if(bit)
   {
       int128_from_int(&mask,0,0,0,1);
       if (pos) int128_rotate_left(&mask,pos);
       (*num).part[0]=(*num).part[0]|mask.part[0];
       (*num).part[1]=(*num).part[1]|mask.part[1];
       (*num).part[2]=(*num).part[2]|mask.part[2];
       (*num).part[3]=(*num).part[3]|mask.part[3];
   }
   else
   {
       int128_from_int(&mask,0xffffffff,0xffffffff,0xffffffff,0xfffffffe);
       if (pos) int128_rotate_left(&mask,pos);
       (*num).part[0]=(*num).part[0]&mask.part[0];
       (*num).part[1]=(*num).part[1]&mask.part[1];
       (*num).part[2]=(*num).part[2]&mask.part[2];
       (*num).part[3]=(*num).part[3]&mask.part[3];
   }
}
/*!
 * @brief returns a bit
 * @param[in] num the 128-Bit number from which the Bit should be read
 * @param[in] pos the Bit-position within the number
 * @return 1 if the specified Bit is 1
 *         0 if the specified Bit is 0
 */
int int128_getbit(int128 num,int pos)
{
    int128 mask,tmp;

    int128_from_int(&mask,0,0,0,1);
    if (pos) int128_rotate_left(&mask,pos);
    tmp.part[0]=num.part[0]&mask.part[0];
    tmp.part[1]=num.part[1]&mask.part[1];
    tmp.part[2]=num.part[2]&mask.part[2];
    tmp.part[3]=num.part[3]&mask.part[3];

    if ((tmp.part[0])||(tmp.part[1])||(tmp.part[2])||(tmp.part[3])) return 1;
    return 0;
}

/*!
 * @brief returns a 32-Bit word
 * @param[in] num the 128-Bit number from which the Word should be read
 * @param[in] pos the number of the word
 *                0: Bits 127 .. 96
 *                1: Bits  95 .. 64
 *                2: Bits  63 .. 32
 *                3: Bits  31 ..  0
 * @return the specified 32-Bit word
 */
unsigned int int128_toInt(int128 num,int pos)
{
   return num.part[pos];
}
/*!
 * @brief returns the upper 64-Bit
 * @param[in] num the 128-Bit number from which should be read
 * @return Bits 127 .. 64
 */
unsigned long long int128_getupper(int128 num)
{
   unsigned long long tmp;
   tmp=(unsigned long long)num.part[0]<<32;
   tmp=tmp|num.part[1];
   return tmp;
}
/*!
 * @brief returns the lower 64-Bit
 * @param[in] num the 128-Bit number from which should be read
 * @return Bits 63 .. 0
 */
unsigned long long int128_getlower(int128 num)
{
   unsigned long long tmp;
   tmp=(unsigned long long)num.part[2]<<32;
   tmp=tmp|num.part[3];
   return tmp;
}

/*!
 * @brief copies a 128-Bit number into another
 * @param[in] src Quelle
 * @param[out] dest Ziel
 */
void int128_copy(int128 src, int128 *dest)
{
  (*dest).part[0]=src.part[0];
  (*dest).part[1]=src.part[1];
  (*dest).part[2]=src.part[2];
  (*dest).part[3]=src.part[3];
}

/*!
 * @brief convertes a number into a string
 * the number will be treated as signed.
 * if the specified length is too short the string will contain the last digits
 * @param[in] src the 128-Bit number that should be converted
 * @param[in] basis the basis of the returned string (dual,decimal,hex,...)
 * @param[in] length the length of the produced string
 * @return string representation of the number
 */
char * int128_toString(int128 src,int basis,int length)
{
   int i,j,sign;
   char tab[36] = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'};
   int128 tmp1,tmp2,zero,base,inval;
   char* res;

   res=malloc(length+2);

   if (basis>36)
   {
      strncpy(res,"Error:basis is too large\0",length);
      res[length+1]='\0';
      return res;
   }

   int128_copy(src,&tmp1);
   int128_from_int(&zero,0,0,0,0);
   int128_from_int(&base,0,0,0,basis);

   int128_from_int(&inval,0x80000000,0,0,0);  /* this number stays negativ wenn being negated
                                                 this causes the div and mod Operations to produce
                                                 unexpected results as they are both signed */

   sign=(src.part[0]&0x80000000);
   if (sign) int128_neg(&tmp1);

   i=0;

   if (int128_unsigned_compare(tmp1,inval)==0) /* we have the invalid number as input
                                                  -> the first calls to div and mod need compensation
                                                     as tmp1 appears to be negativ to them, although it is 
                                                     ment to be a positiv number in this algorithm */
     while (int128_unsigned_compare(tmp1,zero)==1)
     {
       int128_mod(tmp1,base,&tmp2);
       if (i==0)                               /* fixing wrong result of mod, what is a bit tricky
                                                  remember: mod sees tmp1 as negativ number
                                                  but will produce positiv result tmp2 (as base is positiv)*/
       { 
            int128_sub(tmp2,base,&tmp2);       /* first we sub base to get the correct negativ result for the negativ input*/
            int128_neg(&tmp2);                 /* the negation then produces the result we need as we know
                                                  tmp1 actually was a positive number in this context. */
            int128_mod(tmp2,base,&tmp2);       /* if the result was zero the compensation above will turn tmp2 into base
                                                  but we need {0..base-1} */
       }
       j=int128_toInt(tmp2,3);
       res[length-i]=tab[j];
       i++;if(i>length) break;
       int128_div(tmp1,base,&tmp1);
       if (i==1)                                /* just fixing the wrong sign from the result of div */
       { 
            int128_neg(&tmp1);
       }
     }
   else                                         /* normal case without compensation */
     while (int128_unsigned_compare(tmp1,zero)==1)
     {
       int128_mod(tmp1,base,&tmp2);
       j=int128_toInt(tmp2,3);
       res[length-i]=tab[j];
       i++;if(i>length) break;
       int128_div(tmp1,base,&tmp1);
     }
   res[length+1]='\0';
   while (i<length)
   {
      res[length-i]=tab[0];
      i++;
   }
 
   if (sign) res[0]='-'; else res[0]=' ';

   return res;
}

/*!
 * @brief rotates the number to the left
 * @param[in/out] num the number to be rotated
 * @param[in] length defines the distance 
 */
void int128_rotate_left(int128 *num,int length)
{
   length=length%128;
   int tmp;
   while (length>0)
   {
       tmp=((*num).part[0]&0x80000000)>>31;
       (*num).part[0]=((*num).part[0]<<1)|(((*num).part[1]&0x80000000)>>31);
       (*num).part[1]=((*num).part[1]<<1)|(((*num).part[2]&0x80000000)>>31);
       (*num).part[2]=((*num).part[2]<<1)|(((*num).part[3]&0x80000000)>>31);
       (*num).part[3]=((*num).part[3]<<1)|tmp;
       length--;
   }
}
/*!
 * @brief rotates the number to the right
 * @param[in/out] num the number to be rotated
 * @param[in] length defines the distance 
 */
void int128_rotate_right(int128 *num,int length)
{
   length=length%128;
   int tmp;
   while (length>0)
   {
       tmp=(*num).part[3]&0x00000001;
       (*num).part[3]=((*num).part[3]>>1)|((*num).part[2]&0x00000001)<<31;
       (*num).part[2]=((*num).part[2]>>1)|((*num).part[1]&0x00000001)<<31;
       (*num).part[1]=((*num).part[1]>>1)|((*num).part[0]&0x00000001)<<31;
       (*num).part[0]=((*num).part[0]>>1)|tmp<<31;
       length--;
   }
}
/*!
 * @brief shifts the number to the left
 * @param[in/out] num the number to be shifted
 * @param[in] length defines the distance
 * @param[in] input defines what is shifted in from the right side
 * @return the bits shifted out on the left side 
 */
int int128_shift_left(int128 *num,int length,int input)
{
   int res=0,mask1=0,mask2=0xffffffff;

   length=length%128;
   int128_rotate_left(num,length);
   if (length>32) return 0;

   input=input>>(32-length);

   while (length>0)
   {
      mask1=mask1<<1|1;
      mask2=mask2<<1;
      length--;
   }

   res=(*num).part[3]&mask1;
   (*num).part[3]=((*num).part[3]&mask2)|(input&mask1);

   return res;
}
/*!
 * @brief shifts the number to the right
 * @param[in/out] num the number to be shifted
 * @param[in] length defines the distance
 * @param[in] input defines what is shifted in from the left side
 * @return the bits shifted out on the right side 
 */
int int128_shift_right(int128 *num,int length,int input)
{
   int res=0,_length,mask1=0,mask2=0xffffffff;

   if (length <= 32)
   {
     _length=length;
     while (_length>0)
     {
        mask1=mask1<<1|1;
        mask2=mask2<<1;
        _length--;
     }

     res=((*num).part[3]&mask1)<<(32-length);
     (*num).part[3]=((*num).part[3]&mask2)|(input&mask1);
   }

   length=length%128;
   int128_rotate_right(num,length);
   if (length>32) return 0;

   return res;
}

/*!
 * @brief compares 2 signed numbers
 * @param[in] src1 the first number
 * @param[in] src2 the second number
 * @return -1 if src1 is smaller than src2
 *          0 if both numbers are equal
 *          1 if src1 is greater than src2
 */
int int128_compare(int128 src1,int128 src2)
{
   if((src1.part[0]&0x80000000)&&(!((src2.part[0]&0x80000000)))) return -1;
   if((src2.part[0]&0x80000000)&&(!((src1.part[0]&0x80000000)))) return 1;

   if (src1.part[0]<src2.part[0]) return -1;
   if (src1.part[0]>src2.part[0]) return 1;
   if (src1.part[1]<src2.part[1]) return -1;
   if (src1.part[1]>src2.part[1]) return 1;
   if (src1.part[2]<src2.part[2]) return -1;
   if (src1.part[2]>src2.part[2]) return 1;
   if (src1.part[3]<src2.part[3]) return -1;
   if (src1.part[3]>src2.part[3]) return 1;
   return 0;
}

/*!
 * @brief compares 2 unsigned numbers
 * @param[in] src1 the first number
 * @param[in] src2 the second number
 * @return -1 if src1 is smaller than src2
 *          0 if both numbers are equal
 *          1 if src1 is greater than src2
 */
int int128_unsigned_compare(int128 src1,int128 src2)
{
   if (src1.part[0]<src2.part[0]) return -1;
   if (src1.part[0]>src2.part[0]) return 1;
   if (src1.part[1]<src2.part[1]) return -1;
   if (src1.part[1]>src2.part[1]) return 1;
   if (src1.part[2]<src2.part[2]) return -1;
   if (src1.part[2]>src2.part[2]) return 1;
   if (src1.part[3]<src2.part[3]) return -1;
   if (src1.part[3]>src2.part[3]) return 1;
   return 0;
}
/*!
 * @brief negates a number
 * @param[in/out] num the number that should be negated
 */
void int128_neg(int128 * num)
{
   int128 mask,tmp;
   int128_from_int(&mask,0xffffffff,0xffffffff,0xffffffff,0xffffffff);

   // inverting using xor
   tmp.part[0]=(*num).part[0]^mask.part[0];
   tmp.part[1]=(*num).part[1]^mask.part[1];
   tmp.part[2]=(*num).part[2]^mask.part[2];
   tmp.part[3]=(*num).part[3]^mask.part[3];

   int128_from_int(num,0,0,0,1); 

   // adding 1
   int128_add(tmp,(*num),num);
}
/*!
 * @brief addition (signed and unsigned)
 * splitts the numbers into their parts and uses 32-Bit addition to calculate the result
 * stores overflow for later calls to int128_get_overflow
 * @param[in] src1 first number
 * @param[in] src2 second number
 * @param[out] dest sum of src1 and src2
 * @return -1 in case of an overflow
 *          0 if no error occurs
 */
int int128_add(int128 src1, int128 src2, int128 *dest)
{
   int err=0,sign=0;

   unsigned long long temp[4];

   sign=((src1.part[0]&0x80000000))^((src2.part[0]&0x80000000));

   temp[3]=(unsigned long long)src1.part[3]+src2.part[3];
   temp[2]=(unsigned long long)src1.part[2]+src2.part[2]+(temp[3]>>32);
   temp[1]=(unsigned long long)src1.part[1]+src2.part[1]+(temp[2]>>32);
   temp[0]=(unsigned long long)src1.part[0]+src2.part[0]+(temp[1]>>32);

   (*dest).part[0]=(unsigned int) temp[0];
   (*dest).part[1]=(unsigned int) temp[1];
   (*dest).part[2]=(unsigned int) temp[2];
   (*dest).part[3]=(unsigned int) temp[3];
   err=(unsigned int) (temp[0]>>32);

   /* store overflow information (useful only for unsigned use)*/
   overflow.part[0]=0;
   overflow.part[1]=0;
   overflow.part[2]=0;
   overflow.part[3]=err;

   /* the overflow flag is for signed use! */
   if ((err==0)&&(sign==((*dest).part[0]&0x80000000))) return 0;

   return -1;
}
/*!
 * @brief subtraction (signed and unsigned)
 * negates second nummber and uses int128_add for calculating the result
 * @param[in] src1 first number
 * @param[in] src2 second number
 * @param[out] dest difference of src1 and src2
 * @return -1 in case of an overflow
 *          0 if no error occurs
 */
int int128_sub(int128 src1, int128 src2, int128 *dest)
{
   int128 tmp;
   int res;

   int128_copy(src2,&tmp);
   int128_neg(&tmp);
   res = int128_add(src1,tmp,dest);

   // overflow flag is inverted for subtraction
   return !res;
}

/*!
 * @brief multiplication (signed and unsigned)
 * splitts the numbers into their parts and uses 32-Bit multiplication and addition to calculate the result
 * stores overflow for later calls to int128_get_overflow
 * @param[in] src1 first number
 * @param[in] src2 second number
 * @param[out] dest product of src1 and src2
 * @return -1 in case of an overflow
 *          0 if no error occurs
 */
int int128_mult(int128 src1, int128 src2, int128 *dest)
{
   int sign;
   int mask=0xffffffff;

   unsigned long long res[8];
   unsigned long long temp[16];

   sign=((src1.part[0]&0x80000000))^((src2.part[0]&0x80000000));

   /* multiply each part of src1 with each part of src2 */
   temp[0]=(unsigned long long)src1.part[0]*src2.part[0];
   temp[1]=(unsigned long long)src1.part[1]*src2.part[0];
   temp[2]=(unsigned long long)src1.part[2]*src2.part[0];
   temp[3]=(unsigned long long)src1.part[3]*src2.part[0];
   temp[4]=(unsigned long long)src1.part[0]*src2.part[1];
   temp[5]=(unsigned long long)src1.part[1]*src2.part[1];
   temp[6]=(unsigned long long)src1.part[2]*src2.part[1];
   temp[7]=(unsigned long long)src1.part[3]*src2.part[1];
   temp[8]=(unsigned long long)src1.part[0]*src2.part[2];
   temp[9]=(unsigned long long)src1.part[1]*src2.part[2];
   temp[10]=(unsigned long long)src1.part[2]*src2.part[2];
   temp[11]=(unsigned long long)src1.part[3]*src2.part[2];
   temp[12]=(unsigned long long)src1.part[0]*src2.part[3];
   temp[13]=(unsigned long long)src1.part[1]*src2.part[3];
   temp[14]=(unsigned long long)src1.part[2]*src2.part[3];
   temp[15]=(unsigned long long)src1.part[3]*src2.part[3];

   /* sum up intermediate results */
   res[7]=(unsigned long long) (temp[15])&0xffffffff;
   res[6]=(unsigned long long)(res[7]>>32)+(unsigned int) temp[11]+(unsigned int) temp[14]+    (unsigned int)(temp[15]>>32);
   res[5]=(unsigned long long)(res[6]>>32)+(unsigned int) temp[7]+ (unsigned int) temp[10]+    (unsigned int)(temp[11]>>32)+(unsigned int) temp[13]+    (unsigned int)(temp[14]>>32);
   res[4]=(unsigned long long)(res[5]>>32)+(unsigned int) temp[3]+ (unsigned int) temp[6]+     (unsigned int)(temp[7]>>32)+ (unsigned int) temp[9]+     (unsigned int)(temp[10]>>32)+(unsigned int) temp[12]+    (unsigned int)(temp[13]>>32);
   res[3]=(unsigned long long)(res[4]>>32)+(unsigned int) temp[2]+ (unsigned int)(temp[3]>>32)+(unsigned int) temp[5]+      (unsigned int)(temp[6]>>32)+(unsigned int) temp[8]+      (unsigned int)(temp[9]>>32)+(unsigned int)(temp[12]>>32);
   res[2]=(unsigned long long)(res[3]>>32)+(unsigned int) temp[1]+ (unsigned int) temp[4]+     (unsigned int)(temp[2]>>32)+ (unsigned int)(temp[5]>>32)+(unsigned int)(temp[8]>>32);
   res[1]=(unsigned long long)(res[2]>>32)+(unsigned int) temp[0]+ (unsigned int)(temp[1]>>32)+(unsigned int)(temp[4]>>32);
   res[0]=(unsigned long long)(res[1]>>32)+(unsigned int)(temp[0]>>32);

  /* set result */
  (*dest).part[0]=(int)res[4];
  (*dest).part[1]=(int)res[5];
  (*dest).part[2]=(int)res[6];
  (*dest).part[3]=(int)res[7];

  /* store overflow information (useful only for unsigned use)*/
  overflow.part[0]=(int)res[0];
  overflow.part[1]=(int)res[1];
  overflow.part[2]=(int)res[2];
  overflow.part[3]=(int)res[3];

  /* the overflow flag is for signed use! */
  if ((res[0]==0)&&(res[1]==0)&&(res[2]==0)&&(res[3]==0)&&(sign==(res[4]&0x80000000))) return 0;

  return -1;

}

/*!
 * @brief returns the overflow of a prior multiplication or addition
 * @return overflow from last multiplication or addition
 */
int128 int128_getoverflow(void)
{
   return overflow;
}

/*!
 * @brief division (only signed)
 * @param[in] src1 dividend
 * @param[in] src2 divisor
 * @param[out] dest quotient of src1 and src2
 * @return -1 if divisor is 0
 *          0 if no error occurs
 */
int int128_div(int128 src1, int128 src2, int128 *dest)
{
   int sign,i,n;
   int128 d,r,q,tmp;

   /*store sign information */
   sign=((src1.part[0]&0x80000000))^((src2.part[0]&0x80000000));

   int128_from_int(&tmp,0,0,0,0);

   int128_copy(src1,&r);
   int128_copy(src2,&d);

   /*remove signs*/
   if (src1.part[0]&0x80000000) int128_neg(&r);
   if (src2.part[0]&0x80000000) int128_neg(&d);

   int128_copy(tmp,dest);
   int128_copy(tmp,&q);

   /*abort if divisor is 0*/
   if (int128_compare(tmp,d)==0) return -1;

  if (int128_compare(r,d)==0) /* skip calculation if dividend and divisor are equal */
   {
      int128_from_int(&q,0,0,0,1);
   }
   else
   {
     n=1;
     while ((int128_unsigned_compare(d,r)<0)&&(!(d.part[0]&0x80000000))) /* shift divisor left until it's greater than dividend*/
     {
         int128_shift_left(&d,1,0);
         n++;
     }
     for (i=0;i<n;i++)                                                   /* shift back right and calculate result-bits */
     {
	if (int128_unsigned_compare(d,r)<=0)
        {
	   int128_shift_left(&q,1,0x80000000);
           int128_sub(r,d,&r);
        }
        else int128_shift_left(&q,1,0);
        int128_shift_right(&d,1,0);
     }
   }

   /*restore sign */
   if (sign) int128_neg(&q);

   /* set result */
   int128_copy(q,dest);

   return 0;
}
/*!
 * @brief modulo (only signed)
 * @param[in] src1 dividend
 * @param[in] src2 divisor
 * @param[out] dest remainder of src1/src2
 * @return -1 if divisor is 0
 *          0 if no error occurs
 */
int int128_mod(int128 src1, int128 src2, int128 *dest)
{ 
   int i,n;
   int128 d,r,q,tmp;

   int128_from_int(&tmp,0,0,0,0);

   int128_copy(src1,&r);
   int128_copy(src2,&d);

   /*remove signs*/
   if (src1.part[0]&0x80000000) int128_neg(&r);
   if (src2.part[0]&0x80000000) int128_neg(&d);

   int128_copy(r,dest);
   int128_copy(tmp,&q);

   /*abort if divisor is 0*/
   if (int128_compare(tmp,d)==0) return -1;
   if (int128_compare(r,d)==0) /* skip calculation if dividend and divisor are equal */
   {
      int128_from_int(&r,0,0,0,0);
   }
   else
   {
     n=1;
     while ((int128_unsigned_compare(d,r)<0)&&(!(d.part[0]&0x80000000))) /* shift divisor left until it's greater than dividend*/
     {
         int128_shift_left(&d,1,0);
         n++;
     }
     for (i=0;i<n;i++)                                                    /* shift back right and calculate result-bits */
     {
	if (int128_unsigned_compare(d,r)<=0)
        {
	   int128_shift_left(&q,1,0x80000000);
           int128_sub(r,d,&r);
        }
        else int128_shift_left(&q,1,0);
        int128_shift_right(&d,1,0);
     }
   }

   /* do some compensation for negative input */
   if (src1.part[0]&0x80000000) {
      int128_neg(&r);
      if ((!(src2.part[0]&0x80000000))&&(int128_compare(tmp,r)!=0)) 
        { 
           int128_add(r,src2,&r);
        }
   }
   else if ((src2.part[0]&0x80000000)&&(int128_compare(tmp,r)!=0)) int128_add(r,src2,&r);

   /* set result */
   int128_copy(r,dest);

   return 0;

}
