/*! Little library providing some basic operations for 128-Bit Integers
 *  Author: Daniel Molka (Daniel.Molka@zih.tu-dresden.de)
 *  tested only on x86!
 *  assumptions: int       : 32Bit
 *               long long : 64Bit
 */


#ifndef BIG_INT_H
#define BIG_INT_H

/*!
 * the 128-Bit Integer datatype
 * part[0]: Bits 127 .. 96
 * part[1]: Bits  95 .. 64
 * part[2]: Bits  63 .. 32
 * part[3]: Bits  31 ..  0
 * can be used for signed and unsigned numbers
 */
typedef struct int128_t
{
    unsigned int part[4];
} int128;

/*!
 * creats an 128-Bit Integer from 4 32-Bit Integers
 */
void int128_from_int(int128 *dest,unsigned int part0, unsigned int part1, unsigned int part2, unsigned int part3);
/*!
 * creats an 128-Bit Integer from 2 64-Bit Integers
 */
void int128_from_longlong(int128 *dest,unsigned long long part01,unsigned long long part23);
/*!
 * creates an 128-Bit Integer from a String (containing a decimal number)
 */
int int128_from_string(int128 *dest, char* src);

/*!
 * sets one bit
 */
void int128_setbit(int128 *num,int pos,int bit);
/*!
 * returns a bit
 */
int int128_getbit(int128 num,int pos);
/*!
 * returns a 32-Bit word
 */
unsigned int int128_toInt(int128 num,int pos);
/*!
 * returns the upper 64-Bit
 */
unsigned long long int128_getupper(int128 num);
/*!
 * returns the lower 64-Bit
 */
unsigned long long int128_getlower(int128 num);

/*!
 * copies a number into another
 */
void int128_copy(int128 src, int128 *dest);
/*!
 * returns a string representing the number (only signed)
 */
char * int128_toString(int128 src,int basis,int length);

/*!
 * rotates the number to the left by length Bits
 */
void int128_rotate_left(int128 *num,int length);
/*!
 * rotates the number to the right by length Bits
 */
void int128_rotate_right(int128 *num,int length);

/*!
 * shifts the number to the left by length Bits
 */
int int128_shift_left(int128 *num,int length,int input);
/*!
 * shifts the number to the right by length Bits
 */
int int128_shift_right(int128 *num,int length,int input);

/*!
 * compares 2 signed numbers
 */
int int128_compare(int128 src1,int128 src2);
/*!
 * compares 2 unsigned numbers
 */
int int128_unsigned_compare(int128 src1,int128 src2);

/*!
 * negates a number
 */
void int128_neg(int128 * num);
/*!
 * addition (signed and unsigned)
 */
int int128_add(int128 src1, int128 src2, int128 *dest);
/*!
 * subtraction (signed and unsigned)
 */
int int128_sub(int128 src1, int128 src2, int128 *dest);
/*!
 * multiplication (signed and unsigned)
 */
int int128_mult(int128 src1, int128 src2, int128 *dest);
/*!
 * returns the overflow of a prior multiplication or addition
 */
int128 int128_getoverflow(void);
/*!
 * division (only signed)
 */
int int128_div(int128 src1, int128 src2, int128 *dest);
/*!
 * modulo (only signed)
 */
int int128_mod(int128 src1, int128 src2, int128 *dest);

#endif
