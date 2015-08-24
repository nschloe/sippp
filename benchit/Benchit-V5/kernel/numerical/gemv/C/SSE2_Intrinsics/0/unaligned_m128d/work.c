/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*  Contact: benchit@zih.tu-dresden.de
*
*  C DGEMV kernel (SSE2,unaligned data)
*  This file: the kernel itself, the work which is measured.
*
*  Author: Robert Schoene (robert.schoene@tu-dresden.de)
*  Last change by: $Author: rschoene $
*  $Revision: 1.2 $
*  $Date: 2006/04/10 08:23:06 $
*
******************************************************************************/

#include <emmintrin.h>
#include "work.h"


void sseunalignIJ_(int sizeVector,int sizeAusgabe,double alpha,double beta, double* x, double *A, double *y)
{
	int i,j,iXsize;
	// upper limit for j-loops
	int upperLimitJ=sizeAusgabe-sizeAusgabe%2;
	// upper limit for i-loops
	int upperLimitI=sizeVector-sizeVector%2;
	// xmm Register
	__m128d xmm_gamma,xmm_y,xmm_x,xmm_a0,xmm_a1,xmm_a2,xmm_a3;
	// load beta into all places of xmm_gamma
	xmm_gamma=_mm_load1_pd(&beta);
	// calculate y=beta*y parallel
	for (j=0;j<upperLimitJ;j=j+2)
	{
		xmm_y=_mm_loadu_pd(&y[j]);
		xmm_y=_mm_mul_pd(xmm_y,xmm_gamma);
		_mm_storeu_pd(&y[j],xmm_y);
	}
	// and maybe sequential (if the size of y is not a multiple of 2)
	for (j=upperLimitJ;j<sizeAusgabe;j++)
	{
		y[j]=beta*y[j];
	}
	//
	// now : x=x, A=A, y=beta*y
	//

	// load alpha into all places of xmm_gamma
	xmm_gamma=_mm_load1_pd(&alpha);

	for (i=0;i<upperLimitI;i=i+2)
	{
		// temporary variable for i*sizeAusgabe
		iXsize=i*sizeAusgabe;
		// load x[i] and x[i+1] (the next 2 entries for vector x)
		xmm_a0=_mm_loadu_pd(&x[i]);
		// multiply them with gamma
		xmm_a0=_mm_mul_pd(xmm_a0,xmm_gamma);
		// write gamma*x[i+1] into both sides of xmm_a1
		xmm_a1=_mm_shuffle_pd(xmm_a0,xmm_a0,_MM_SHUFFLE2(1,1));
		// write gamma*x[i] into both sides of xmm_a0
		xmm_a0=_mm_shuffle_pd(xmm_a0,xmm_a0,_MM_SHUFFLE2(0,0));
		// do for the two next elements of A
		for (j=0;j<upperLimitJ;j=j+2)
		{
			// load destination vector y
			xmm_y=_mm_loadu_pd(&y[j]);
			// load next 2 elements of A [i][j] and a[i][j+1]
			xmm_x=_mm_loadu_pd(&A[iXsize+j]);
			// multiply them with gamma*x[i]
			xmm_x=_mm_mul_pd(xmm_x,xmm_a0);
			// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
			//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
			xmm_y=_mm_add_pd(xmm_y,xmm_x);
			// load A [i+1][j] and A[i+1][j+1]
			xmm_x=_mm_loadu_pd(&A[((i+1)*(sizeAusgabe))+j]);
			// see above
			xmm_x=_mm_mul_pd(xmm_x,xmm_a1);
			xmm_y=_mm_add_pd(xmm_y,xmm_x);
			// store
			// y[j]=y[j]+A[i][j]*gamma*x[i]+A[i+1]*gamma*x[i]
			// y[j+1] equivalent
			_mm_storeu_pd(&y[j],xmm_y);
		}
		for (j=upperLimitJ;j<sizeAusgabe;j++)
		{
			y[j]=y[j]+alpha*(A[iXsize+j]*x[i]+A[((i+1)*sizeAusgabe)+j]*x[i+1]);
		}
	}
	for (i=upperLimitI;i<sizeVector;i++)
	{
		// temporary variable for i*sizeAusgabe
		iXsize=i*sizeAusgabe;
		// load x[i]
		xmm_a0=_mm_load1_pd(&x[i]);
		// multiply it with gamma
		xmm_a0=_mm_mul_pd(xmm_a0,xmm_gamma);
		for (j=0;j<upperLimitJ;j=j+2)
		{
			// load destination vector y
			xmm_y=_mm_loadu_pd(&y[j]);
			// load next 2 elements of A [i][j] and a[i][j+1]
			xmm_x=_mm_loadu_pd(&A[iXsize+j]);
			// multiply them with gamma*x[i]
			xmm_x=_mm_mul_pd(xmm_x,xmm_a0);
			// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
			//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
			xmm_y=_mm_add_pd(xmm_y,xmm_x);
			// store
			// y[j]=y[j]+A[i][j]*gamma*x[i]
			// y[j+1] equivalent
			_mm_storeu_pd(&y[j],xmm_y);
		}
		for (j=upperLimitJ;j<sizeAusgabe;j++)
		{
			y[j]=y[j]+alpha*(A[iXsize+j]*x[i]+A[((i+1)*sizeAusgabe)+j]*x[i+1]);
		}
	}
}

void sseunalignJI_(int sizeVector,int sizeAusgabe,double alpha,double beta, double* x, double *A, double *y)
{
	int i,j,iXsize;
	// upper limit for j-loops
	int upperLimitJ=sizeAusgabe-sizeAusgabe%2;
	// upper limit for i-loops
	int upperLimitI=sizeVector-sizeVector%2;
	// xmm Register
	__m128d xmm_gamma,xmm_y,xmm_x,xmm_a0,xmm_a1,xmm_a2,xmm_a3;
	// load beta into all places of xmm_gamma
	xmm_gamma=_mm_load1_pd(&beta);
	// calculate y=beta*y parallel
	for (j=0;j<upperLimitJ;j=j+2)
	{
		xmm_y=_mm_loadu_pd(&y[j]);
		xmm_y=_mm_mul_pd(xmm_y,xmm_gamma);
		_mm_storeu_pd(&y[j],xmm_y);
	}
	// and maybe sequential (if the size of y is not a multiple of 2)
	for (j=upperLimitJ;j<sizeAusgabe;j++)
	{
		y[j]=beta*y[j];
	}
	//
	// now : x=x, A=A, y=beta*y
	//

	// load alpha into all places of xmm_gamma
	xmm_gamma=_mm_load1_pd(&alpha);

	// do for the two next elements of y
	for (j=0;j<upperLimitJ;j=j+2)
	{
		// load destination vector y
		xmm_y=_mm_loadu_pd(&y[j]);
		for (i=0;i<upperLimitI;i=i+2)
		{
			// temporary variable for i*sizeAusgabe
			iXsize=i*sizeAusgabe;
			// load x[i] and x[i+1] (the next 2 entries for vector x)
			xmm_a0=_mm_loadu_pd(&x[i]);
			// multiply them with gamma
			xmm_a0=_mm_mul_pd(xmm_a0,xmm_gamma);
			// write gamma*x[i+1] into both sides of xmm_a1
			xmm_a1=_mm_shuffle_pd(xmm_a0,xmm_a0,_MM_SHUFFLE2(1,1));
			// write gamma*x[i] into both sides of xmm_a0
			xmm_a0=_mm_shuffle_pd(xmm_a0,xmm_a0,_MM_SHUFFLE2(0,0));
			// load next 2 elements of A [i][j] and a[i][j+1]
			xmm_x=_mm_loadu_pd(&A[iXsize+j]);
			// multiply them with gamma*x[i]
			xmm_x=_mm_mul_pd(xmm_x,xmm_a0);
			// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
			//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
			xmm_y=_mm_add_pd(xmm_y,xmm_x);
			// load A [i+1][j] and A[i+1][j+1]
			xmm_x=_mm_loadu_pd(&A[((i+1)*(sizeAusgabe))+j]);
			// see above
			xmm_x=_mm_mul_pd(xmm_x,xmm_a1);
			xmm_y=_mm_add_pd(xmm_y,xmm_x);
		}
		for (i=upperLimitI;i<sizeVector;i++)
		{
			// temporary variable for i*sizeAusgabe
			iXsize=i*sizeAusgabe;
			// load x[i]
			xmm_a0=_mm_load1_pd(&x[i]);
			// multiply it with gamma
			xmm_a0=_mm_mul_pd(xmm_a0,xmm_gamma);
			// load next 2 elements of A [i][j] and a[i][j+1]
			xmm_x=_mm_loadu_pd(&A[iXsize+j]);
			// multiply them with gamma*x[i]
			xmm_x=_mm_mul_pd(xmm_x,xmm_a0);
			// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
			//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
			xmm_y=_mm_add_pd(xmm_y,xmm_x);
		}
		// store
		// y[j]=y[j]+A[i][j]*gamma*x[i]+A[i+1]*gamma*x[i]
		// y[j+1] equivalent
		_mm_storeu_pd(&y[j],xmm_y);
	}
	for (j=upperLimitJ;j<sizeAusgabe;j++)
	{
		for (i=0;i<sizeVector;i++)
		{
			y[j]=y[j]+alpha*A[i*sizeAusgabe+j]*x[i];
		}
	}
}

/*****************************************************************************

LOG-History

$Log: work.c,v $
Revision 1.2  2006/04/10 08:23:06  rschoene
changed header/footer

Revision 1.1  2006/04/05 07:24:49  rschoene
gemv, double precision sse unaligned

(based on Robert Wlochs c kernel skeleton)
*****************************************************************************/
