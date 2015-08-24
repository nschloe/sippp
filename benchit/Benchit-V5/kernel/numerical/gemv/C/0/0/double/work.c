/******************************************************************************
*
*  B e n c h I T - Performance Measurement for Scientific Applications
*  Contact: benchit@zih.tu-dresden.de
*
*  C DGEMV kernel
*  This file: the kernel itself, the work which is measured.
*
*  Author: Robert Schoene (robert.schoene@tu-dresden.de)
*  Last change by: $Author: rschoene $
*  $Revision: 1.2 $
*  $Date: 2006/04/10 08:23:06 $
*
******************************************************************************/

#include "work.h"

void ij_(int sizeVector,int sizeAusgabe,double alpha,double beta, double* x, double *A, double *y)
{
	int i,j;
        double temp;
	for (j=0;j<sizeAusgabe;j++)
	{
		y[j]=beta*y[j];
	}
	//
	// now : x=x, A=A, y=beta*y
	//

	for (i=0;i<sizeVector;i++)
	{
                temp=alpha*x[i];
		for (j=0;j<sizeAusgabe;j++)
		{
			y[j]=y[j]+A[i*sizeAusgabe+j]*temp;
		}
	}
}

void ji_(int sizeVector,int sizeAusgabe,double alpha,double beta, double* x, double *A, double *y)
{
	int i,j;
        double temp;
	for (j=0;j<sizeAusgabe;j++)
	{
		y[j]=beta*y[j];
	}
	//
	// now : x=x, A=A, y=beta*y
	//

	for (j=0;j<sizeAusgabe;j++)
	{
                temp=0.0;
		for (i=0;i<sizeVector;i++)
		{
			temp=temp+A[i*sizeAusgabe+j]*x[i];
		}
                temp=temp*alpha;
                y[j]=y[j]+temp;
	}
}

/*****************************************************************************

LOG-History

$Log: work.c,v $
Revision 1.2  2006/04/10 08:23:06  rschoene
changed header/footer

Revision 1.1  2006/04/05 07:24:49  rschoene
gemv, double precision

(based on Robert Wlochs c kernel skeleton)
*****************************************************************************/
