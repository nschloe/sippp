CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BenchIT - Performance Measurement for Scientific Applications
C
C Kernel: measure Bandwidth inspired by STREAM benchmark (FORTRAN-version)
C
C according to the rules, reffer this Benchmark as:
C "BenchIT kernel based on a variant of the STREAM benchmark code"
C when publishing results
C
C This file contains the work, that is done: copy,scale,add and triad
C
C Contact: developer@benchit.org
C
C Last change by: $Author: rschoene $
C $Revision: 1.1 $
C $Date: 2007/02/13 12:48:43 $
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


	SUBROUTINE copy( c ,a , n)
	INTEGER*4 n,i
	double precision a(n)
	double precision c(n)
	do 100 i=1,n
	   c(i)=a(i)
 100	CONTINUE
	RETURN
	END
	
	SUBROUTINE scale( b, c, scalar, n)
	INTEGER*4 n,i
	double precision b(n)
	double precision c(n)
	double precision scalar
	do 100 i=1,n
	   b(i)=scalar*c(i)
 100	CONTINUE
	RETURN
	END
	
	SUBROUTINE add( c, a, b, n)
	INTEGER*4 n,i
	double precision a(n)
	double precision b(n)
	double precision c(n)
	do 100 i=1,n
	   c(i)=a(i)+b(i)
 100	CONTINUE
	RETURN
	END
	
	SUBROUTINE triad( a, b, c, scalar, n)
	INTEGER*4 n,i
	double precision a(n)
	double precision b(n)
	double precision c(n)
	double precision scalar
	do 100 i=1,n
	   a(i)=b(i)+scalar*c(i)
 100	CONTINUE
	RETURN
	END
