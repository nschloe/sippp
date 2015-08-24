C       ************************************************************
C       scatter matrix over binary tree; before the function is 
C       called, process 0 holds the whole matrix a; after the
C       function call every process has his part in matrix b;
C       should only be used after a call to mpi_variables !!
C       ************************************************************
	SUBROUTINE mpimatrixscatter ( a, b, sizeall, sizeone)
	INCLUDE 'mpif.h'
	INTEGER size,daddy,child0,child1,rank
	INTEGER childcount0, childcount1, i, j
	INTEGER sizeall, sizeone
	INTEGER status(MPI_STATUS_SIZE)
	COMMON size, rank, child0, child1, daddy
	COMMON childcount0, childcount1	
	double precision a(sizeall,sizeall)
	double precision b(sizeall,sizeone)
	double precision temp(sizeall,sizeall)
	INTEGER ierror, sendtype0, sendtype1, recvtype
	INTEGER lengthrecv
C       now we can start
	status(1)=0
C	call matrixnull( temp, sizeall, sizeall)
	if( rank.eq.0) then
	   if (child0.lt.size) then
	      call MPI_Type_vector( sizeone*childcount0, sizeall, 
     &                              sizeall, MPI_DOUBLE_PRECISION, 
     &                              sendtype0, ierror)
	      call MPI_Type_commit( sendtype0, ierror)
C             sende sizeone ganze spalten f�r cildcount0 kinder
	      call MPI_Send( a(1,sizeone+1), 1, sendtype0, child0, 1, 
     &                       MPI_COMM_WORLD, ierror)
	      call MPI_Type_free( sendtype0, ierror)
	   end if
	   if (child1.lt.size) then
	      call MPI_Type_vector( sizeone*childcount1, sizeall,
     &                              sizeall, MPI_DOUBLE_PRECISION,
     &                              sendtype1, ierror)
	      call MPI_Type_commit( sendtype1, ierror)
C             sende sizeone ganze spalten f�r cildcount1 kinder
	      call MPI_Send( a(1,1+(childcount0+1)*sizeone), 1, 
     &                       sendtype1, 
     &                       child1, 1, MPI_COMM_WORLD, ierror)
	      call MPI_Type_free( sendtype1, ierror)
	   end if
C	   copy my own part
	   do 10 i=1,sizeone
	      do 10 j=1,sizeall
		 b(j,i)=a(j,i)
 10	   continue
	else
	   lengthrecv=sizeone*(childcount0+childcount1+1)
	   call MPI_Type_vector( lengthrecv, sizeall,
     &                           sizeall, MPI_DOUBLE_PRECISION,
     &                           recvtype, ierror)
	   call MPI_Type_commit( recvtype, ierror)
	   call MPI_Recv( temp(1,1), 1, recvtype, daddy, 1,
     &                    MPI_COMM_WORLD, status, ierror)
C	   write(*,*) 'Matrix temp after recv in rank ',rank
C	   call matrixprint( temp, sizeall, sizeall)
	   call MPI_Type_free( recvtype, ierror)
	   do 20 i=1,sizeone
	      do 20 j=1,sizeall
		 b(j,i)=temp(j,i)
 20	   continue
	   if(child0.lt.size) then
	      call MPI_Type_vector( sizeone*childcount0, sizeall,
     &                              sizeall, MPI_DOUBLE_PRECISION,
     &                              sendtype0, ierror)
	      call MPI_Type_commit( sendtype0, ierror)
	      call MPI_Send( temp(1,sizeone+1), 1, sendtype0, child0,
     &                       1, MPI_COMM_WORLD, ierror)
	      call MPI_Type_free( sendtype0, ierror)
	   end if
	   if(child1.lt.size) then
	      call MPI_Type_vector( sizeone*childcount1, sizeall,
     &                              sizeall, MPI_DOUBLE_PRECISION,
     &                              sendtype1, ierror)
	      call MPI_Type_commit( sendtype1, ierror)
	      call MPI_Send( temp(1,1+(childcount0+1)*sizeone), 1, 
     &                       sendtype1,
     &                       child1, 1, MPI_COMM_WORLD, ierror)    
	      call MPI_Type_free( sendtype1, ierror)
	   end if
	end if
	RETURN
	END
C
C       ************************************************************
C       gather matrix over binary tree; before the function is 
C       called, ervery process holds its part of the matrix in 
C       the variable b; after the function call process 0 has the
C       whole matrix in variable a;
C       should only be used after a call to mpi_variables !!
C       ************************************************************
	SUBROUTINE mpimatrixgather ( a, b, sizeall, sizeone)
	INCLUDE 'mpif.h'
	INTEGER size,daddy,rank,child0,child1
	INTEGER sizeall,sizeone
	INTEGER childcount0, childcount1, i, j
	INTEGER status(MPI_STATUS_SIZE)
	COMMON size, rank, child0, child1, daddy
	COMMON childcount0, childcount1	
	double precision a(sizeall,sizeall)
	double precision b(sizeall,sizeone)
	double precision temp(sizeall,sizeall)
	INTEGER ierror, sendtype, recvtype0, recvtype1
	INTEGER lengthsend
C       now we can start
	if( rank.eq.0) then
	   if (child0.lt.size) then
	      call MPI_Type_vector( sizeone*childcount0, sizeall,
     &                              sizeall, MPI_DOUBLE_PRECISION, 
     &                              recvtype0, ierror)
	      call MPI_Type_commit( recvtype0, ierror)	      
C             empfange sizeone ganze spalten f�r cildcount0 kinder
	      call MPI_Recv( a(1,sizeone+1), 1, recvtype0, child0, 1,
     &                       MPI_COMM_WORLD, status, ierror)
	      call MPI_Type_free( recvtype0, ierror)
	   end if
	   if (child1.lt.size) then
	      call MPI_Type_vector( sizeone*childcount1, sizeall,
     &                              sizeall, MPI_DOUBLE_PRECISION, 
     &                              recvtype1, ierror)
	      call MPI_Type_commit( recvtype1, ierror)	      
C             empfange sizeone ganze spalten f�r cildcount1 kinder
	      call MPI_Recv( a(1,1+(childcount0+1)*sizeone), 1, 
     &                       recvtype1, child1, 1, MPI_COMM_WORLD,
     &                       status, ierror)
	      call MPI_Type_free( recvtype1, ierror)
	   end if
C          copy the results of my own work
	   do 10 i=1,sizeone
	      do 10 j=1,sizeall
		 a(j,i)=b(j,i)
 10	   continue
	else
	   lengthsend=sizeone*(childcount0+childcount1+1)
C          if we have childs, wait for the colums of the childs
	   if(child0.lt.size) then
   	      call MPI_Type_vector( sizeone*childcount0, sizeall,
     &                              sizeall, MPI_DOUBLE_PRECISION, 
     &                              recvtype0, ierror)
	      call MPI_Type_commit( recvtype0, ierror)	      
	      call MPI_Recv( temp(1,sizeone+1), 1, recvtype0, child0,
     &                       1, MPI_COMM_WORLD, status, ierror)
	      call MPI_Type_free( recvtype0, ierror)
	   end if
	   if(child1.lt.size) then
   	      call MPI_Type_vector( sizeone*childcount1, sizeall,
     &                              sizeall, MPI_DOUBLE_PRECISION, 
     &                              recvtype1, ierror)
	      call MPI_Type_commit( recvtype1, ierror)	      
	      call MPI_Recv( temp(1,1+(childcount0+1)*sizeone), 1,
     &                       recvtype1, child1, 1, MPI_COMM_WORLD,
     &                       status, ierror)
	      call MPI_Type_free( recvtype1, ierror)
	   end if
C          copy my own work to temp
	   do 20 i=1,sizeone
	      do 20 j=1,sizeall
		 temp(j,i)=b(j,i)
 20	   continue
C          send all the results to my daddy
C	   write(*,*) 'send ', lengthsend, ' cols to ',daddy
	   call MPI_Type_vector( lengthsend, sizeall,
     &                           sizeall, MPI_DOUBLE_PRECISION, 
     &                           sendtype, ierror)
	   call MPI_Type_commit( sendtype, ierror)	      
	   call MPI_Send( temp(1,1), 1, sendtype, daddy, 1,
     &                    MPI_COMM_WORLD, ierror)
	   call MPI_Type_free( sendtype, ierror)
	end if
	RETURN
	END
C
C       ***********************************************************
C       rotates matrix n over all processes using a ring structure
C       ***********************************************************
	SUBROUTINE mpishiftmatrix ( n, rows, cols)
	INCLUDE 'mpif.h'
	INTEGER rows, cols, i, j
	double precision n(rows,cols)
	double precision temp(rows,cols)
	INTEGER size,daddy,rank,child0,child1
	INTEGER status(MPI_STATUS_SIZE,2), ierror
	COMMON size, rank, child0, child1, daddy
	INTEGER left, right, request(2)
	left=rank-1
	if(left.lt.0) then
	  left=size-1
	end if
	right=rank+1
	if(right.eq.size) then
	  right=0
	end if
	call MPI_Isend( n, rows*cols, MPI_DOUBLE_PRECISION, right, 2,
     &             MPI_COMM_WORLD, request(1), ierror)
	call MPI_Irecv( temp, rows*cols, MPI_DOUBLE_PRECISION, left, 2,
     &             MPI_COMM_WORLD, request(2), ierror)
	call MPI_Waitall( 2, request, status)
	do 400 j=1,cols
	   do 400 i=1,rows
	      n(i,j)=temp(i,j)
 400	continue
	RETURN
	END
C
C       *********************************************************
C       Broadcast of matrix n over binary tree; function
C       should only be used after a call to mpi_variables !!
C       *********************************************************
	SUBROUTINE mpimatrixbroadcast ( n, rows, cols)
	INCLUDE 'mpif.h'
	INTEGER rows, cols
	double precision n( rows, cols)
	INTEGER size,child0,child1,rank, daddy
	COMMON size, rank, child0, child1, daddy
	INTEGER status(MPI_STATUS_SIZE), ierror
C       now we can start
	if (rank.eq.0) then
	   if (child0.lt.size) then
	      call MPI_Send ( n, rows*cols, MPI_DOUBLE_PRECISION,
     &                        child0, 1, MPI_COMM_WORLD, ierror)
	   end if
	   if (child1.lt.size) then
	      call MPI_Send ( n, rows*cols, MPI_DOUBLE_PRECISION,
     &                        child1, 1, MPI_COMM_WORLD, ierror)
	   end if
	else
	   call MPI_Recv ( n, rows*cols, MPI_DOUBLE_PRECISION,
     &                     daddy, 1, MPI_COMM_WORLD, status, ierror)

	   if (child0.lt.size) then
	      call MPI_Send ( n, rows*cols, MPI_DOUBLE_PRECISION, 
     &                        child0, 1, MPI_COMM_WORLD, ierror)
	   end if
	   if (child1.lt.size) then
	      call MPI_Send ( n, rows*cols, MPI_DOUBLE_PRECISION,
     &                        child1, 1, MPI_COMM_WORLD, ierror)
	   end if
	end if
	RETURN
	END
C
C	LOG-History
C	$Log: mpi_matrix_functions.f,v $
C	Revision 1.1  2005/12/06 15:48:23  william
C	created two matmul-fortran-kernels for mpi - first implements mpi via fortran - second via c - hence the names
C
C	
