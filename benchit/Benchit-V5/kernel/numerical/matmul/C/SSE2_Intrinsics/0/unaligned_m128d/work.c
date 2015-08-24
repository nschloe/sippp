/********************************************************************
 * BenchIT - Performance Measurement for Scientific Applications
 *
 * Kernel: SSE2 Matrix Multiply (C), unaligned data
 * Contact: benchit@zih.tu-dresden.de
 *
 * Last change by: $Author: hackenb $
 * $Revision: 1.3 $
 * $Date: 2005/12/15 09:28:29 $
 *******************************************************************/

#include "stdio.h"
#include "stdlib.h"
#include <string.h>
#include "work.h"
#include <emmintrin.h>

void multasseijk_( double *a, double *b, double *c, int *size );
void multasseikj_( double *a, double *b, double *c, int *size );
void multassejik_( double *a, double *b, double *c, int *size );
void multassejki_( double *a, double *b, double *c, int *size );
void multassekji_( double *a, double *b, double *c, int *size );
void multassekij_( double *a, double *b, double *c, int *size );

double getlanguage_( void );

void multasseijk_(double* a, double *b, double *c,int *size)
{
	int i,j,k;
	int s=*size;
	// upper limit for loops
	int limit=s-s%2;
	// xmm Register
	__m128d xmm_a,xmm_b,xmm_c,xmm_temp;
	for (i=0;i<s;i++)
		{
		for (j=0;j<limit;j=j+2)
		{
			xmm_c=_mm_loadu_pd(&c[i*s+j]);
			for (k=0;k<s;k++)
			{
				xmm_a=_mm_load1_pd(&a[i*s+k]);
				xmm_b=_mm_loadu_pd(&b[k*s+j]);
				xmm_temp=_mm_mul_pd(xmm_a,xmm_b);
				xmm_c=_mm_add_pd(xmm_c,xmm_temp);
			}
			_mm_storeu_pd(&c[i*s+j],xmm_c);
		}
		for (j=limit;j<s;j++)
			for (k=0;k<s;k++)
			{
				c[i*s+j]=c[i*s+j]+a[i*s+k]*b[k*s+j];
			}
	}
}

void multasseikj_(double* a, double *b, double *c,int *size)
{
	int i,j,k;
	int s=*size;
	// upper limit for loops
	int limit=s-s%2;
	// xmm Register
	__m128d xmm_a,xmm_b,xmm_c,xmm_temp;
	for (i=0;i<s;i++)
		{
		for (k=0;k<s;k++)
		{
		xmm_a=_mm_load1_pd(&a[i*s+k]);
		for (j=0;j<limit;j=j+2)
		{
			xmm_c=_mm_loadu_pd(&c[i*s+j]);
			xmm_b=_mm_loadu_pd(&b[k*s+j]);
			xmm_temp=_mm_mul_pd(xmm_a,xmm_b);
			xmm_c=_mm_add_pd(xmm_c,xmm_temp);
			_mm_storeu_pd(&c[i*s+j],xmm_c);
		}
		}
		for (k=0;k<s;k++)
		{
		for (j=limit;j<s;j++)
			c[i*s+j]=c[i*s+j]+a[i*s+k]*b[k*s+j];
		}
	}
}

void multassejik_(double* a, double *b, double *c,int *size)
{
	int i,j,k;
	int s=*size;
	// upper limit for loops
	int limit=s-s%2;
	// xmm Register
	__m128d xmm_a,xmm_b,xmm_c,xmm_temp;
	for (j=0;j<limit;j=j+2)
	{
		for (i=0;i<s;i++)
		{
			xmm_c=_mm_loadu_pd(&c[i*s+j]);
			for (k=0;k<s;k++)
			{
				xmm_a=_mm_load1_pd(&a[i*s+k]);
				xmm_b=_mm_loadu_pd(&b[k*s+j]);
				xmm_temp=_mm_mul_pd(xmm_a,xmm_b);
				xmm_c=_mm_add_pd(xmm_c,xmm_temp);
			}
			_mm_storeu_pd(&c[i*s+j],xmm_c);
		}
	}
	for (j=limit;j<s;j++)
		for (i=0;i<s;i++)
			for (k=0;k<s;k++)
				c[i*s+j]=c[i*s+j]+a[i*s+k]*b[k*s+j];
}


void multassejki_(double* a, double *b, double *c,int *size)
{
	int i,j,k;
	int s=*size;
	// upper limit for loops
	int limit=s-s%2;
	// xmm Register
	__m128d xmm_a,xmm_b,xmm_c,xmm_temp;
	for (j=0;j<limit;j=j+2)
	{
		for (k=0;k<s;k++)
		{
			xmm_b=_mm_loadu_pd(&b[k*s+j]);
			for (i=0;i<s;i++)
			{
				xmm_c=_mm_loadu_pd(&c[i*s+j]);
				xmm_a=_mm_load1_pd(&a[i*s+k]);
				xmm_temp=_mm_mul_pd(xmm_a,xmm_b);
				xmm_c=_mm_add_pd(xmm_c,xmm_temp);
				_mm_storeu_pd(&c[i*s+j],xmm_c);
			}
		}
	}
	for (j=limit;j<s;j++)
		for (k=0;k<s;k++)
			for (i=0;i<s;i++)
				c[i*s+j]=c[i*s+j]+a[i*s+k]*b[k*s+j];
}

void multassekij_(double* a, double *b, double *c,int *size)
{
	int i,j,k;
	int s=*size;
	// upper limit for loops
	int limit=s-s%2;
	// xmm Register
	__m128d xmm_a,xmm_b,xmm_c,xmm_temp;
	for (k=0;k<s;k++)
	{
		for (i=0;i<s;i++)
		{
			xmm_a=_mm_load1_pd(&a[i*s+k]);
			for (j=0;j<limit;j=j+2)
			{
				xmm_c=_mm_loadu_pd(&c[i*s+j]);
				xmm_b=_mm_loadu_pd(&b[k*s+j]);
				xmm_temp=_mm_mul_pd(xmm_a,xmm_b);
				xmm_c=_mm_add_pd(xmm_c,xmm_temp);
				_mm_storeu_pd(&c[i*s+j],xmm_c);
			}
		}
		for (i=0;i<s;i++)
			for (j=limit;j<s;j++)
			{
				c[i*s+j]=c[i*s+j]+a[i*s+k]*b[k*s+j];
			}
	}
}

void multassekji_(double* a, double *b, double *c,int *size)
{
	int i,j,k;
	int s=*size;
	// upper limit for loops
	int limit=s-s%2;
	// xmm Register
	__m128d xmm_a,xmm_b,xmm_c,xmm_temp;
	for (k=0;k<s;k++)
	{
		for (j=0;j<limit;j=j+2)
		{
			xmm_b=_mm_loadu_pd(&b[k*s+j]);
			for (i=0;i<s;i++)
			{
				xmm_a=_mm_load1_pd(&a[i*s+k]);
				xmm_c=_mm_loadu_pd(&c[i*s+j]);
				xmm_temp=_mm_mul_pd(xmm_a,xmm_b);
				xmm_c=_mm_add_pd(xmm_c,xmm_temp);
				_mm_storeu_pd(&c[i*s+j],xmm_c);
			}
		}
		for (j=limit;j<s;j++)
			for (i=0;i<s;i++)
			{
				c[i*s+j]=c[i*s+j]+a[i*s+k]*b[k*s+j];
			}
	}
}

double getlanguage_()
{
  return 1.0;
}


/***************************************************************************** 
LOG-History
 
$Log: work.c,v $
Revision 1.3  2005/12/15 09:28:29  hackenb
new variable names
modified/unified header and footer
MIN/MAX/INCREMENT iterating strategy

Revision 1.2  2005/11/09 11:59:52  rschoene
commenting ... bah

Revision 1.1  2005/11/09 09:12:23  rschoene
initial SSE2 matmul (double precision, unaligned data)

*****************************************************************************/
