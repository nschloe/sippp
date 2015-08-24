C
C �B e n c h I T - Performance Measurement for Scientific Applications
C
C �Core for Matrix Multiply (Fortran 77)
C �Author: Michael Kluge (kluge@zhr.tu-dresden.de)
C
C �$Revision: 1.1 $
C �$Date: 2005/08/20 15:48:22 $
C �$State: Exp $
C
	SUBROUTINE multaijk( a, b, c, n)
	INTEGER*4 n,i,j,k
	double precision a(n,n), b(n,n), c(n,n)
	do 100 i=1,n
	do 100 j=1,n
	do 100 k=1,n
	   c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
c
	SUBROUTINE multaikj( a, b, c, n)
	INTEGER*4 n,i,j,k
	double precision a(n,n), b(n,n), c(n,n)
	do 100 i=1,n
	do 100 k=1,n
	do 100 j=1,n
	   c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
c
	SUBROUTINE multajik( a, b, c, n)
	INTEGER*4 n,i,j,k
	double precision a(n,n), b(n,n), c(n,n)
	do 100 j=1,n
	do 100 i=1,n
	do 100 k=1,n
	   c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
c
	SUBROUTINE multajki( a, b, c, n)
	INTEGER*4 n,i,j,k
	double precision a(n,n), b(n,n), c(n,n)
	do 100 j=1,n
	do 100 k=1,n
	do 100 i=1,n
	   c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
c
	SUBROUTINE multakij( a, b, c, n)
	INTEGER*4 n,i,j,k
	double precision a(n,n), b(n,n), c(n,n)
	do 100 k=1,n
	do 100 i=1,n
	do 100 j=1,n
	   c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
c
	SUBROUTINE multakji( a, b, c, n)
	INTEGER*4 n,i,j,k
	double precision a(n,n), b(n,n), c(n,n)
	do 100 k=1,n
	do 100 j=1,n
	do 100 i=1,n
	   c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
C
C
C	LOG-History
C	$Log: matmul_f_core.f,v $
C	Revision 1.1  2005/08/20 15:48:22  rschoene
C	Matrix Multiply for F77 checkin
C	(merged from src3-matmulc and src2-matmul-f77)
C
C	Revision 1.1.1.1  2004/12/14 21:22:57  william
C	Release 3.0 - created new cvs-tree src2
C	
C	Revision 2.0  2003/12/09 11:18:57  juckel
C	build of version 2.0
C	
C	Revision 1.3  2003/05/23 09:15:21  kluge
C	bug removed: wrong index while accessing matrix
C	
C	Revision 1.2  2003/01/28 11:44:42  kluge
C	new header
C	
C
