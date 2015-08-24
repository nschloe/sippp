/*
 * kernel_wrapper.h
 * UFla, 10/2006
 */

#if ! defined BENCHIT_KERNEL_MAIN_H
#define BENCHIT_KERNEL_MAIN_H

extern void* init_(int*);
extern void  cleanup_();
extern void  kernel_main_(int*,int*,double*);

#endif /* ! defined BENCHIT_KERNEL_MAIN_H */
