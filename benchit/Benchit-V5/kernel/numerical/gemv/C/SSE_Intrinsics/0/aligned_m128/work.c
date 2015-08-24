/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*  Contact: benchit@zih.tu-dresden.de
*
*  C SGEMV kernel (SSE, aligned data)
*  This file: the kernel itself, the work which is measured.
*
*  Author: Robert Schoene (robert.schoene@tu-dresden.de)
*  Last change by: $Author: rschoene $
*  $Revision: 1.2 $
*  $Date: 2006/04/10 08:23:06 $
*
******************************************************************************/

#include <xmmintrin.h>
#include "work.h"


void ssealignIJ_(int sizeVector,int sizeAusgabe,float alpha,float beta, float* x, float *A, float *y)
{
	//IDL(-1,printf("\tIJ:%i\n",sizeVector););
	int i,j,iXsize;
	// upper limit for j-loops
	int upperLimitJ=sizeAusgabe-sizeAusgabe%4;
	// upper limit for i-loops
	int upperLimitI=sizeVector-sizeVector%4;
	// xmm Register
	__m128 xmm_gamma,xmm_y,xmm_x,xmm_a0,xmm_a1,xmm_a2,xmm_a3;
	// load beta into all places of xmm_gamma
	xmm_gamma=_mm_load1_ps(&beta);
	// calculate y=beta*y parallel
	for (j=0;j<upperLimitJ;j=j+4)
	{
		xmm_y=_mm_load_ps(&y[j]);
		xmm_y=_mm_mul_ps(xmm_y,xmm_gamma);
		_mm_store_ps(&y[j],xmm_y);
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
	xmm_gamma=_mm_load1_ps(&alpha);

	// if sizeAusgabe is a multiple of 2, every A[m][4n] (m,n E N+) can be loaded aligned
	if (sizeAusgabe%4==0)
	{
		for (i=0;i<upperLimitI;i=i+4)
		{
			//IDL(-1,printf("\t\ti:%i\n",i););
			// temporary variable for i*sizeAusgabe
			iXsize=i*sizeAusgabe;
			// load x[i] and x[i+1] (the next 2 entries for vector x)
			xmm_a0=_mm_load_ps(&x[i]);
			// multiply them with gamma
			xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
			xmm_a3=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(3,3,3,3));
			xmm_a2=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(2,2,2,2));
			// write gamma*x[i+1] into both sides of xmm_a1
			xmm_a1=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(1,1,1,1));
			// write gamma*x[i] into both sides of xmm_a0
			xmm_a0=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(0,0,0,0));
			// do for the two next elements of A
			for (j=0;j<upperLimitJ;j=j+4)
			{
				//IDL(-1,printf("\t\t\tj:%i\n",j););
				// load destination vector y
				xmm_y=_mm_load_ps(&y[j]);
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_load_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// load A [i+1][j] and A[i+1][j+1]
				xmm_x=_mm_load_ps(&A[((i+1)*(sizeAusgabe))+j]);
				// see above
				xmm_x=_mm_mul_ps(xmm_x,xmm_a1);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_load_ps(&A[((i+2)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a2);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_load_ps(&A[((i+3)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a3);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// store
				// y[j]=y[j]+A[i][j]*gamma*x[i]+A[i+1]*gamma*x[i]
				// y[j+1] equivalent
				_mm_store_ps(&y[j],xmm_y);
			}
			//(upperLimit is the same as sizeAusgabe)
		}
		for (i=upperLimitI;i<sizeVector;i++)
		{
			//IDL(-1,printf("\t\ti:%i\n",i););
			// temporary variable for i*sizeAusgabe
			iXsize=i*sizeAusgabe;
			// load x[i]
			xmm_a0=_mm_load1_ps(&x[i]);
			// multiply it with gamma
			xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
			for (j=0;j<upperLimitJ;j=j+4)
			{
				//IDL(-1,printf("\t\t\tj:%i\n",j););
				// load destination vector y
				xmm_y=_mm_load_ps(&y[j]);
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_load_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// store
				// y[j]=y[j]+A[i][j]*gamma*x[i]
				// y[j+1] equivalent
				_mm_store_ps(&y[j],xmm_y);
			}
			
		}
	}
	// A isn't aligned
	else
	{
		for (i=0;i<upperLimitI;i=i+4)
		{
			//IDL(-1,printf("\t\ti:%i\n",i););
			// temporary variable for i*sizeAusgabe
			iXsize=i*sizeAusgabe;
			// load x[i] and x[i+1] (the next 2 entries for vector x)
			xmm_a0=_mm_load_ps(&x[i]);
			// multiply them with gamma
			xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
			xmm_a3=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(3,3,3,3));
			xmm_a2=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(2,2,2,2));
			// write gamma*x[i+1] into both sides of xmm_a1
			xmm_a1=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(1,1,1,1));
			// write gamma*x[i] into both sides of xmm_a0
			xmm_a0=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(0,0,0,0));
			// do for the two next elements of A
			for (j=0;j<upperLimitJ;j=j+4)
			{
				//IDL(-1,printf("\t\t\tj:%i\n",j););
				// load destination vector y
				xmm_y=_mm_load_ps(&y[j]);
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_load_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// load A [i+1][j] and A[i+1][j+1]
				xmm_x=_mm_loadu_ps(&A[((i+1)*(sizeAusgabe))+j]);
				// see above
				xmm_x=_mm_mul_ps(xmm_x,xmm_a1);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_loadu_ps(&A[((i+2)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a2);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_loadu_ps(&A[((i+3)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a3);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// store
				// y[j]=y[j]+A[i][j]*gamma*x[i]+A[i+1]*gamma*x[i]
				// y[j+1] equivalent
				_mm_store_ps(&y[j],xmm_y);
			}
			for (j=upperLimitJ;j<sizeAusgabe;j++)
			{
				//IDL(-1,printf("\t\t\tj:%i\n",j););
				y[j]=y[j]+alpha*(A[iXsize+j]*x[i]+A[((i+1)*sizeAusgabe)+j]*x[i+1]);
			}
		}
		for (i=upperLimitI;i<sizeVector;i++)
		{
			//IDL(-1,printf("\t\ti:%i\n",i););
			// temporary variable for i*sizeAusgabe
			iXsize=i*sizeAusgabe;
			// load x[i]
			xmm_a0=_mm_load1_ps(&x[i]);
			// multiply it with gamma
			xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
			for (j=0;j<upperLimitJ;j=j+4)
			{
				//IDL(-1,printf("\t\t\tj:%i\n",j););
				// load destination vector y
				xmm_y=_mm_load_ps(&y[j]);
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_loadu_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// store
				// y[j]=y[j]+A[i][j]*gamma*x[i]
				// y[j+1] equivalent
				_mm_store_ps(&y[j],xmm_y);
			}
			for (j=upperLimitJ;j<sizeAusgabe;j++)
			{
				//IDL(-1,printf("\t\t\tj:%i\n",j););
				y[j]=y[j]+alpha*(A[iXsize+j]*x[i]+A[((i+1)*sizeAusgabe)+j]*x[i+1]);
			}
		}
	}
}

void ssealignJI_(int sizeVector,int sizeAusgabe,float alpha,float beta, float* x, float *A, float *y)
{
	//IDL(-1,printf("\tJI:%i\n",sizeVector););
	int i,j,iXsize;
	// upper limit for j-loops
	int upperLimitJ=sizeAusgabe-sizeAusgabe%4;
	// upper limit for i-loops
	int upperLimitI=sizeVector-sizeVector%4;
	// xmm Register
	__m128 xmm_gamma,xmm_y,xmm_x,xmm_a0,xmm_a1,xmm_a2,xmm_a3;
	// load beta into all places of xmm_gamma
	xmm_gamma=_mm_load1_ps(&beta);
	// calculate y=beta*y parallel
	for (j=0;j<upperLimitJ;j=j+4)
	{
		xmm_y=_mm_load_ps(&y[j]);
		xmm_y=_mm_mul_ps(xmm_y,xmm_gamma);
		_mm_store_ps(&y[j],xmm_y);
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
	xmm_gamma=_mm_load1_ps(&alpha);

	// if sizeAusgabe is a multiple of 2, every A[m][2n] (m,n E N+) can be loaded aligned
	if (sizeAusgabe%4==0)
	{
		// do for the two next elements of y
		for (j=0;j<upperLimitJ;j=j+4)
		{
			// load destination vector y
			xmm_y=_mm_load_ps(&y[j]);
			for (i=0;i<upperLimitI;i=i+4)
			{
				// temporary variable for i*sizeAusgabe
				iXsize=i*sizeAusgabe;
				// load x[i] and x[i+1] (the next 2 entries for vector x)
				xmm_a0=_mm_load_ps(&x[i]);
				// multiply them with gamma
				xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
				xmm_a3=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(3,3,3,3));
				xmm_a2=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(2,2,2,2));
				// write gamma*x[i+1] into both sides of xmm_a1
				xmm_a1=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(1,1,1,1));
				// write gamma*x[i] into both sides of xmm_a0
				xmm_a0=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(0,0,0,0));
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_load_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// load A [i+1][j] and A[i+1][j+1]
				xmm_x=_mm_load_ps(&A[((i+1)*(sizeAusgabe))+j]);
				// see above
				xmm_x=_mm_mul_ps(xmm_x,xmm_a1);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_load_ps(&A[((i+2)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a2);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_load_ps(&A[((i+3)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a3);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
			}
			for (i=upperLimitI;i<sizeVector;i++)
			{
				// temporary variable for i*sizeAusgabe
				iXsize=i*sizeAusgabe;
				// load x[i]
				xmm_a0=_mm_load1_ps(&x[i]);
				// multiply it with gamma
				xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_load_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				
			}
			// store
			// y[j]=y[j]+A[i][j]*gamma*x[i]+A[i+1]*gamma*x[i]
			// y[j+1] equivalent
			_mm_store_ps(&y[j],xmm_y);
		}
	}
	// A[2m+1][2n] isn't aligned
	else
	{
		// do for the two next elements of y
		for (j=0;j<upperLimitJ;j=j+4)
		{
			// load destination vector y
			xmm_y=_mm_load_ps(&y[j]);
			for (i=0;i<upperLimitI;i=i+4)
			{
				// temporary variable for i*sizeAusgabe
				iXsize=i*sizeAusgabe;
				// load x[i] and x[i+1] (the next 2 entries for vector x)
				xmm_a0=_mm_load_ps(&x[i]);
				// multiply them with gamma
				xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
				xmm_a3=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(3,3,3,3));
				xmm_a2=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(2,2,2,2));
				// write gamma*x[i+1] into both sides of xmm_a1
				xmm_a1=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(1,1,1,1));
				// write gamma*x[i] into both sides of xmm_a0
				xmm_a0=_mm_shuffle_ps(xmm_a0,xmm_a0,_MM_SHUFFLE(0,0,0,0));
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_load_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				// load A [i+1][j] and A[i+1][j+1]
				xmm_x=_mm_loadu_ps(&A[((i+1)*(sizeAusgabe))+j]);
				// see above
				xmm_x=_mm_mul_ps(xmm_x,xmm_a1);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_loadu_ps(&A[((i+2)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a2);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
				xmm_x=_mm_loadu_ps(&A[((i+3)*(sizeAusgabe))+j]);
				xmm_x=_mm_mul_ps(xmm_x,xmm_a3);
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
			}
			for (i=upperLimitI;i<sizeVector;i++)
			{
				// temporary variable for i*sizeAusgabe
				iXsize=i*sizeAusgabe;
				// load x[i]
				xmm_a0=_mm_load1_ps(&x[i]);
				// multiply it with gamma
				xmm_a0=_mm_mul_ps(xmm_a0,xmm_gamma);
				// load next 2 elements of A [i][j] and a[i][j+1]
				xmm_x=_mm_loadu_ps(&A[iXsize+j]);
				// multiply them with gamma*x[i]
				xmm_x=_mm_mul_ps(xmm_x,xmm_a0);
				// add y[  j  ] = y[  j  ] + A[  i  ][  j  ] * gamma * x[  i  ]
				//     y[ j+1 ] = y[ j+1 ] + A[  i  ][ j+1 ] * gamma * x[  i  ]
				xmm_y=_mm_add_ps(xmm_y,xmm_x);
			}
			// store
			// y[j]=y[j]+A[i][j]*gamma*x[i]+A[i+1]*gamma*x[i]
			// y[j+1] equivalent
			_mm_store_ps(&y[j],xmm_y);
		}
		for (j=upperLimitJ;j<sizeAusgabe;j++)
		{
			for (i=0;i<sizeVector;i++)
			{
				y[j]=y[j]+alpha*A[i*sizeAusgabe+j]*x[i];
			}
		}
	}
}

/*****************************************************************************

LOG-History

$Log: work.c,v $
Revision 1.2  2006/04/10 08:23:06  rschoene
changed header/footer

Revision 1.1  2006/04/05 07:24:49  rschoene
gemv, single precision sse unaligned

(based on Robert Wlochs c kernel skeleton)
*****************************************************************************/
