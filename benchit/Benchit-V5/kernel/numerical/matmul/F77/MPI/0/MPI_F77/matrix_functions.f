        SUBROUTINE matrixinit( a, b, sizeall, sizeone)
        INTEGER sizeall, sizeone, i, j
        double precision a(sizeall,sizeall), b(sizeall,sizeone)
        double precision fill
        fill=0.0
        do 10 j=1,sizeall
           do 10 i=1,sizeall
              a(i,j)=1.0
10      continue
        call matrixfill( b, sizeall, sizeone, fill)
        RETURN
        END
C
        SUBROUTINE matrixfill( a, rows, cols, value)
        INTEGER rows, cols
        double precision a(rows,cols), value
        do 20 j=1,cols
           do 20 i=1,rows
              a(i,j)=value
20      continue
        RETURN
        END
C
        SUBROUTINE matrixprint( a, rows, cols)
        INTEGER rows, cols, i, j
        double precision a(rows,cols)
        do 30 j=1,rows
           WRITE(*,*) (a(j,i), i=1,cols)
30      continue  
        RETURN
        END
C
C	LOG-History
C	$Log: matrix_functions.f,v $
C	Revision 1.1  2005/12/06 15:48:22  william
C	created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names
C
C
