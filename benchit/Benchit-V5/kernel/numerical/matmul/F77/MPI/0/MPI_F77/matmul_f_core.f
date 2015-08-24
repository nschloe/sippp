	SUBROUTINE matmul ( a, b, c, rowsa, colsa, rowsb, colsb)
	INTEGER colsa, rowsa, colsb, rowsb
	double precision a( rowsa, colsa)
	double precision b( rowsb, colsb)
	double precision c( rowsa, colsb)
	INTEGER i, j, k
	do 100 j=1,colsb
	   do 100 k=1,colsa
	      do 100 i=1,rowsa
		 c(i,j)=c(i,j)+a(i,k)*b(k,j)
 100	CONTINUE
	RETURN
	END
C
C	LOG-History
C	$Log: matmul_f_core.f,v $
C	Revision 1.1  2005/12/06 15:48:22  william
C	created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names
C
C
