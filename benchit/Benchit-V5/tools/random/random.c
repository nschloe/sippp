/*atoll*/
#include <stdlib.h>

/* variables for random number generator */

/*! user defined maximum size of output */
static unsigned long long random_max32,random_max48;

/*!
 * The random number generator uses 2 independent generators and returns the bitwise xor of them
 * both generators use this formula: r(n+1) = ((a * r(n)) +b) mod m
 * the parameters are defined in the bi_random_init() function
 */

/*! parameters for the first generator*/
static unsigned long long random_value1=0;
static unsigned long long rand_a1=0;
static unsigned long long rand_b1=0;
static unsigned long long rand_m1=1;
static unsigned long long rand_fix1=0;

/*! parameters for the second generator */
static unsigned long long random_value2=0;
static unsigned long long rand_a2=0;
static unsigned long long rand_b2=0;
static unsigned long long rand_m2=1;
static unsigned long long rand_fix2=0;

/* end variables for random number generator */

/*! @brief returns a 32-Bit pseudo random number
 *  using this function without a prior call to bi_random_init() is undefined!
 *  bi_random32() and bi_random48() share one state so a call to
 *  bi_random32() will affect the next result of bi_random48() and vice versa.
 *  The functions only differ in the output format and the possible range.
 *  @return random number
 */
unsigned int bi_random32(void)
{
  random_value1 = (random_value1 * rand_a1 + rand_b1)%rand_m1;
  random_value2 = (random_value2 * rand_a2 + rand_b2)%rand_m2;
  return (unsigned int) (random_value1^random_value2) % random_max32;
}

/*! @brief returns a 48-Bit pseudo random number
 *  using this function without a prior call to bi_random_init() is undefined!
 *  bi_random32() and bi_random48()share one state so a call to
 *  bi_random48() will affect the next result of bi_random32() and vice versa.
 *  The functions only differ in the output format and the possible range.
 *  @return random number
 */
unsigned long long bi_random48(void)
{
  random_value1 = (random_value1 * rand_a1 + rand_b1)%rand_m1;
  random_value2 = (random_value2 * rand_a2 + rand_b2)%rand_m2;
  return (random_value1^random_value2) % random_max48;
}

/*! @brief initalizes random number generator
 *  Initializes the random number generator with the values given to the function.
 *  The random number generator uses 2 independent generators and returns the bitwise xor of them.
 *  both generators use this formula: r(n+1) = ((a * r(n)) +b) mod m.
 *  @param[in] start start value for random number generation
 *  @param[in] max the generator will allways return numbers smaller than max
 *                 if max is 0 bi_random32 will return numbers between 0 and 2^32 -1
 *                             bi_random48 will return numbers between 0 and 2^48 -1
 */
void bi_random_init(unsigned long long start,unsigned long long max)
{
  /* setting up parameters (direct assignment of long long values causes compiler warning) */
  rand_a1 = atoll("25799");
  rand_b1 = atoll("76546423423");
  rand_m1 = atoll("568563987265559");
  rand_fix1 = atoll("298651465807007");

  rand_a2 = atoll("131");
  rand_b2 = atoll("91723615256891");
  rand_m2 = atoll("338563987265599");
  rand_fix2 = atoll("283167315359180");

  /* setup the max values returned to user*/
  random_max32 = ((unsigned long long) 1) << 32;
  random_max48 = ((unsigned long long) 1) << 48;
  if (max>0)
  {
    if (max < random_max32) random_max32 = max;
    if (max < random_max48) random_max48 = max;
  }

  /* the first generator is initialized with the user defined start value */
  random_value1 = start%rand_m1;
  if (random_value1==rand_fix1) random_value1=atoll("43277143270890");  /* Fixpoint can't be used */

  /* the second generator is initialized with the first random number generated by the first generator*/
  random_value2 = bi_random48();
  if (random_value2==rand_fix2) random_value2 = atoll("678157495234");  /* Fixpoint can't be used */
}
