/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel:  3D Fast Fourier Transform, Non-Powers of 2,
 *          single precision, complex data, MKL (C)
 * Contact: developer@benchit.org
 *
 * Last change by: $Author: molka $
 * $Revision: 1.4 $
 * $Date: 2007/06/22 07:52:06 $
 *******************************************************************/

/* This file has been automatically generated by the BenchIT kernel generator. */

#ifndef __work_h
#define __work_h

#if (defined (_CRAYMPP) || \
     defined (_USE_SHORT_AS_INT))
typedef short myinttype;
#elif (defined (_USE_LONG_AS_INT))
typedef long myinttype;
#else
typedef int myinttype;
#endif

#include <mkl.h>

 /* The data structure that holds all the data.*/
typedef struct mydata
{
   myinttype min;
   myinttype max;
   myinttype steps;
   myinttype* problemsizes;
   float* in;
   float* out;
   float* inout;
   DFTI_DESCRIPTOR_HANDLE my_desc_handle;
} mydata_t;

#endif


/********************************************************************
 * Log-History
 *
 * $Log: fft.h,v $
 * Revision 1.4  2007/06/22 07:52:06  molka
 * newly generated FFT kernels after template change
 *
 *
 *******************************************************************/

