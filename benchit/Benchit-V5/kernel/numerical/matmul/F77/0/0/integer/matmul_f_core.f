CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BenchIT - Performance Measurement for Scientific Applications
C
C Kernel: Matrix Multiply in F77 with integers
C Contact: benchit@zih.tu-dresden.de
C
C Last change by: $Author: rschoene $
C $Revision: 1.2 $
C $Date: 2006/04/24 11:32:56 $
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

	SUBROUTINE multaijk( a, b, c, n)
	INTEGER*4 n,i,j,k
	INTEGER*4 a(n,n), b(n,n), c(n,n)
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
	INTEGER*4 a(n,n), b(n,n), c(n,n)
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
	INTEGER*4 a(n,n), b(n,n), c(n,n)
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
	INTEGER*4 a(n,n), b(n,n), c(n,n)
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
	INTEGER*4 a(n,n), b(n,n), c(n,n)
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
	INTEGER*4 a(n,n), b(n,n), c(n,n)
	do 100 k=1,n
	do 100 j=1,n
	do 100 i=1,n
	   c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
	
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Log-History
C 
C $Log: matmul_f_core.f,v $
C Revision 1.2  2006/04/24 11:32:56  rschoene
C added descriptions, removed warning
C
C Revision 1.1  2006/04/24 11:16:11  rschoene
C initial commit for matmul-f77-int (mostly taken from matmul-f77-double)
C
C 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
