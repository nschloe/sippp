! ============================================================================
! *** MODULE TRIDIAGMOD <<<
! ***
! *** This module defines the type TRIDIAG representing tridiagonal matrices
! *** and provides routines for solving linear equation systems with such
! *** a matrix.
! ***
! ============================================================================
module TRIDIAGMOD

  USE KINDMOD
  USE ERRORMOD

  implicit none

  private

  public :: tridiag,init,clean,unity,tridiag_gauss,                          &
            tridiag_LU_solve,                                                &
            defect_tridiag, tridiag_matmul,tridiag_assign,                   &
            PUT,OPERATOR(+),OPERATOR(-),OPERATOR(*)

  !--------------------------------------------------------------
  type tridiag
      integer(INP)                          :: N
      real(RNP)   ,dimension(:),allocatable :: diag
      real(RNP)   ,dimension(:),allocatable :: lower,upper
  end type tridiag
  !--------------------------------------------------------------


  INTERFACE PUT
      MODULE PROCEDURE tridiag_put
  END INTERFACE

  INTERFACE INIT
      MODULE PROCEDURE tridiag_init
  END INTERFACE

  INTERFACE OPERATOR ( + )
      MODULE PROCEDURE tridiag_add
  END INTERFACE

  INTERFACE OPERATOR ( - )
      MODULE PROCEDURE tridiag_subtract
  END INTERFACE

  INTERFACE OPERATOR ( * )
      MODULE PROCEDURE scalar_mult
  END INTERFACE


  contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE tridiag_init <<<
! ----------------------------------------------------------------------------
subroutine tridiag_init(N,A)

  !arguments
  integer(INP) ,intent(in)  :: N !dimension
  type(TRIDIAG),intent(out) :: A

  if(N.le.2) &
  call STOP_ON_ERROR('INIT (in TRIDIAG)',"Won't allocate such a small N.")

  if(allocated(A%DIAG).or.allocated(A%lower).or.allocated(A%upper)) &
& call STOP_ON_ERROR('INIT (in TRIDIAG)','A already allocated (partly).')

  allocate(A%DIAG(1:N))
  allocate(A%lower(1:N-1))
  allocate(A%upper(1:N-1))

  A%N = N

  A%DIAG  = 0.0_RNP
  A%lower = 0.0_RNP
  A%upper = 0.0_RNP

end subroutine tridiag_init
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE tridiag_init >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE CLEAN <<<
! ----------------------------------------------------------------------------
subroutine clean(A)

  !arguments
  type(TRIDIAG),intent(inout) :: A

  A%N = 0

  if(allocated(A%diag))  deallocate(A%diag)
  if(allocated(A%lower)) deallocate(A%lower)
  if(allocated(A%upper)) deallocate(A%upper)

end subroutine clean
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE CLEAN >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION tridiag_add <<<
! ***
! *** Add two matrices of type tridiag.
! ***
! ----------------------------------------------------------------------------
function tridiag_add(A,B) result(C)

  !arguments
  type(TRIDIAG),intent(in) :: A,B
  type(tridiag)            :: C

  if (A%N.ne.B%N) &
& CALL STOP_ON_ERROR('tridiag_add (in tridiag)','Input matrices not matching.')

  call init(A%N,C)

  C%lower = A%lower + B%lower
  C%diag  = A%diag  + B%diag
  C%upper = A%upper + B%upper

  RETURN
end function tridiag_add
! ----------------------------------------------------------------------------
! *** END of FUNCTION tridiag_add >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION tridiag_subtract <<<
! ***
! *** Subtract two matrices of type tridiag.
! ***
! ----------------------------------------------------------------------------
function tridiag_subtract(A,B) result(C)

  !arguments
  type(TRIDIAG),intent(in) :: A,B
  type(tridiag)            :: C

  if (A%N.ne.B%N) &
& CALL STOP_ON_ERROR('tridiag_subtract (in tridiag)','Input matrices not matching.')

  call init(A%N,C)

  C%lower = A%lower - B%lower
  C%diag  = A%diag  - B%diag
  C%upper = A%upper - B%upper

  RETURN
end function tridiag_subtract
! ----------------------------------------------------------------------------
! *** END of FUNCTION tridiag_subtract >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION scalar_mult <<<
! ***
! *** Multiplies a matrix A with a scalar ALPHA.
! ***
! ----------------------------------------------------------------------------
function scalar_mult(alpha,A) result(C)

  !arguments
  real(RNP)    ,intent(in) :: alpha
  type(TRIDIAG),intent(in) :: A

  type(TRIDIAG)            :: C

  call init(A%N,C)

  C%lower = alpha * A%lower
  C%diag  = alpha * A%diag
  C%upper = alpha * A%upper

end function scalar_mult
! ----------------------------------------------------------------------------
! *** END of FUNCTION scalar_mult >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE scalar_mult <<<
! ***
! *** Multiplies a matrix A with a scalar ALPHA.
! ***
! ----------------------------------------------------------------------------
subroutine tridiag_put(A)

  !arguments
  type(TRIDIAG),intent(in) :: A

  write(*,*) 'lower sub-diagonal:'
  write(*,*) A%lower
  write(*,*)

  write(*,*) '          diagonal:'
  write(*,*) A%diag
  write(*,*)

  write(*,*) 'upper sub-diagonal:'
  write(*,*) A%upper

  return
end subroutine tridiag_put
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE scalar_mult >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE UNITY <<<
! ***
! *** Returns unity matrix of rank N to argument I.
! ***
! ----------------------------------------------------------------------------
subroutine unity(N,I)

  !arguments
  integer(inp) ,intent(in)  :: N
  type(TRIDIAG),intent(out) :: I

  call INIT(N,I)

  I%diag = 1.0_RNP

end subroutine unity
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE UNITY >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE tridiag_gauss <<<
! ***
! *** Solves a tridiagonal equation system with the lower diagonal DL, central
! *** diagonal D, upper diagonal DU and right hand side RHS with the standard
! *** Gau"s method.
! *** The index numbering needs to be as follows:
! ***
! ***  D(1) DU(1)
! *** DL(1)  D(2) DU(2)
! ***    .      .     .
! ***      .      .     .
! ***
! *** At the end of the routine, B will contain the solution vector.
! ***
! ----------------------------------------------------------------------------
subroutine tridiag_gauss(N,A,B)

  !arguments
  integer(INP)            ,intent(in)    :: N
  type(TRIDIAG)           ,intent(in)    :: A
  real(RNP),dimension(1:N),intent(inout) :: B

  !locals
  real(RNP)   ,dimension(2:N) :: t
  real(RNP)                   :: alpha
  integer(INP)                :: i

  if(N.ne.A%N) &
& call STOP_ON_ERROR('tridiag_gauss','Input arguments not matching.')

  alpha = A%diag(1)

  if(abs(alpha).lt.TOL) &
& call STOP_ON_ERROR('tridiag_gauss','Matrix singular to working precision.')

  B(1) = B(1)/alpha

  do i=2,N
      t(i) = A%upper(i-1)/alpha
      alpha = A%diag(i)-A%lower(i-1)*t(i)
      if(abs(alpha)<TOL) &
    & call STOP_ON_ERROR('tridiag_gauss','Matrix singular to working precision.')
      B(i) = (B(i)-A%lower(i-1)*B(i-1))/alpha
  end do

  do i=N-1,1,-1
      B(i) = B(i)-t(i+1)*B(i+1)
  end do

end subroutine tridiag_gauss
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE tridiag_gauss >>>
! ----------------------------------------------------------------------------

! ! ----------------------------------------------------------------------------
! ! *** SUBROUTINE tridiag_LAPACK <<<
! ! ***
! ! *** Solves a tridiagonal equation system with the lower diagonal DL, central
! ! *** diagonal D, upper diagonal DU and right hand side B.
! ! *** The index numbering needs to be as follows:
! ! ***
! ! ***  D(1) DU(1)
! ! *** DL(1)  D(2) DU(2)
! ! ***    .      .     .
! ! ***      .      .     .
! ! ***
! ! *** Uses the LAPACK routine DGTSV. The following is deliberately taken
! ! *** from its documentation:
! ! ***
! ! *  =====================================================================
! ! *  Purpose
! ! *  =======
! ! *
! ! *  DGTSV  solves the equation
! ! *
! ! *     A*X = B,
! ! *
! ! *  where A is an n by n tridiagonal matrix, by Gaussian elimination with
! ! *  partial pivoting.
! ! *
! ! *  Note that the equation  A'*X = B  may be solved by interchanging the
! ! *  order of the arguments DU and DL.
! ! *
! ! *  Arguments
! ! *  =========
! ! *
! ! *  N       (input) INTEGER
! ! *          The order of the matrix A.  N >= 0.
! ! *
! ! *  NRHS    (input) INTEGER
! ! *          The number of right hand sides, i.e., the number of columns
! ! *          of the matrix B.  NRHS >= 0.
! ! *
! ! *  DL      (input/output) DOUBLE PRECISION array, dimension (N-1)
! ! *          On entry, DL must contain the (n-1) sub-diagonal elements of
! ! *          A.
! ! *
! ! *          On exit, DL is overwritten by the (n-2) elements of the
! ! *          second super-diagonal of the upper triangular matrix U from
! ! *          the LU factorization of A, in DL(1), ..., DL(n-2).
! ! *
! ! *  D       (input/output) DOUBLE PRECISION array, dimension (N)
! ! *          On entry, D must contain the diagonal elements of A.
! ! *
! ! *          On exit, D is overwritten by the n diagonal elements of U.
! ! *
! ! *  DU      (input/output) DOUBLE PRECISION array, dimension (N-1)
! ! *          On entry, DU must contain the (n-1) super-diagonal elements
! ! *          of A.
! ! *
! ! *          On exit, DU is overwritten by the (n-1) elements of the first
! ! *          super-diagonal of U.
! ! *
! ! *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
! ! *          On entry, the N by NRHS matrix of right hand side matrix B.
! ! *          On exit, if INFO = 0, the N by NRHS solution matrix X.
! ! *
! ! *  LDB     (input) INTEGER
! ! *          The leading dimension of the array B.  LDB >= max(1,N).
! ! *
! ! *  INFO    (output) INTEGER
! ! *          = 0: successful exit
! ! *          < 0: if INFO = -i, the i-th argument had an illegal value
! ! *          > 0: if INFO = i, U(i,i) is exactly zero, and the solution
! ! *               has not been computed.  The factorization has not been
! ! *               completed unless i = N.
! ! *
! ! *  =====================================================================
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine tridiag_LAPACK(N,A,B)
! 
!   !arguments
!   integer(INP)               ,intent(in)     :: N
!   type(tridiag)              ,intent(in)     :: A
!   real(RNP), dimension(1:N)  ,intent(inout)  :: B
! 
!   !locals
!   integer(INP)  :: INFO
!   type(tridiag) :: A_temp
! 
!   interface
!     ! F77 interface to LAPACK procedure DGTSV
!     SUBROUTINE DGTSV(N,NRHS,DL,D,DU,B,LDB,INFO)
!       INTEGER           N,NRHS,LDB,INFO
!       DOUBLE PRECISION  DL(*),D(*),DU(*),B(LDB,*)
!     END SUBROUTINE DGTSV
!   end interface
! 
!   if (A%N.ne.N) &
!     call STOP_ON_ERROR('tridiag_LAPACK','Dimensions not matching.')
! 
!   !do it!
!   call init(N,A_temp)
!   A_temp = A
!   call DGTSV( N, 1, A_temp%lower, A_temp%diag, A_temp%upper, B, N, INFO )
!   call clean(A_temp)
! 
!   if (INFO<0) then
!       call STOP_ON_ERROR('tridiag_LAPACK','An argument had an illegal value.')
!   elseif (INFO>0) then
!       call STOP_ON_ERROR('tridiag_LAPACK','U(i,i) is exactly zero, and the solution has not been computed. ')
!   endif
! 
! end subroutine tridiag_LAPACK
! ! ----------------------------------------------------------------------------
! ! *** END of SUBROUTINE tridiag_LAPACK >>>
! ! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE tridiag_LU_solve <<<
! ***
! *** A is assumed to contain
! ***
! ***  UD(1)  U(1)
! ***   L(1)   UD(2)  U(2)
! ***     .       .     .
! ***       .       .     .
! ***
! *** where UD and U compose U of the LU decomposistion.
! ***
! ----------------------------------------------------------------------------
subroutine tridiag_LU_solve(N,A,B)

  !arguments
  integer(INP)            ,intent(in)    :: N
  type(TRIDIAG)           ,intent(in)    :: A
  real(RNP),dimension(1:N),intent(inout) :: B

  !locals
  integer(INP) :: i

  if(N.ne.A%N) &
  call STOP_ON_ERROR('tridiag_LU_solve','Dimension not matching.')

  if(minval(abs(A%diag))<TOL) &
  call STOP_ON_ERROR('tridiag_LU_solve','Matrix singular to working precision.')

  !solve the lower triangular system Ly=f
  do i=2,N
     B(i) = B(i) - A%lower(i-1)* B(i-1)
  enddo

  !solve the upper tridiagonal system Ux=y
  B(N) = B(N)/A%diag(N)
  do i=N-1,1,-1
     B(i) = ( B(i) - A%upper(i)*B(i+1) ) / A%diag(i)
  enddo

end subroutine tridiag_LU_solve
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE tridiag_LU_solve >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE defect_tridiag <<<
! ***
! *** Returns the value ||A*X-B||_{\infty}
! *** for A being a tridiagonal matrix of the form
! ***
! ***  D(1) DU(1)
! *** DL(1)  D(2) DU(2)
! ***    .      .     .
! ***      .      .     .
! ***
! ----------------------------------------------------------------------------
real(RNP) function defect_tridiag(A,B,X)

  !arguments
  type(tridiag)                  ,intent(in) :: A
  real(RNP),dimension(1:A%N),intent(in) :: B,X

  !locals
  real(RNP)    :: alpha
  integer(INP) :: i

  defect_tridiag = abs( A%diag(1)*X(1) + A%upper(1)*X(2) - B(1))

  do i=2,A%N-1
      alpha = abs( A%lower(i-1)*X(i-1) + A%diag(i)*X(i) + A%upper(i)*X(i+1) - B(i))
      if (defect_tridiag.lt.alpha) defect_tridiag = alpha
  enddo
  alpha = abs( A%lower(A%N-1)*X(A%N-1) + A%diag(A%N)*X(A%N) - B(A%N))
  if (defect_tridiag.lt.alpha) defect_tridiag = alpha

end function defect_tridiag
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE defect_tridiag >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION tridiag_matmul <<<
! ***
! *** Does a matrix vector multiplication with
! *** A*X = Y
! *** with a tridiagonal matrix A.
! ***
! ***  D(1) DU(1)
! *** DL(1)  D(2) DU(2)
! ***    .      .     .
! ***      .      .     .
! ***
! ----------------------------------------------------------------------------
function tridiag_matmul(A,X) result(Y)

  !arguments
  type(tridiag)             ,intent(in)  :: A
  real(RNP),dimension(1:A%N),intent(in)  :: X

  !result value
  real(RNP),dimension(1:A%N) :: Y

  !locals
  integer(INP) :: i


  Y(1) = A%diag(1)*X(1) + A%upper(1)*X(2)
  do i=2,A%N-1
      Y(i) = A%lower(i-1)*X(i-1) + A%diag(i)*X(i) + A%upper(i)*X(i+1)
  enddo
  Y(A%N) = A%lower(A%N-1)*X(A%N-1) + A%diag(A%N)*X(A%N)


end function tridiag_matmul
! ----------------------------------------------------------------------------
! *** END of FUNCTION tridiag_matmul >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine tridiag_assign <<<
! ***
! ***  B = alpha* I + beta* A
! ***
! ----------------------------------------------------------------------------
subroutine tridiag_assign(A,alpha,beta,B)

  !arguments
  type(tridiag),intent(in)    :: A
  real(RNP)    ,intent(in)    :: alpha,beta
  type(tridiag),intent(inout) :: B

  if (A%N.ne.B%N) then
      CALL STOP_ON_ERROR( 'tridiag_assign (in TRIDIAGMOD)' , &
                          'Input matrices not matching.'     )

      call clean(B)
      call tridiag_init(A%N,B)
  endif

  B%diag = alpha* 1.0_RNP + beta* A%diag
  B%upper =                 beta* A%upper
  B%lower =                 beta* A%lower

  RETURN
end subroutine tridiag_assign
! ----------------------------------------------------------------------------
! *** END of subroutine tridiag_assign >>>
! ----------------------------------------------------------------------------

end module TRIDIAGMOD
! ============================================================================
! *** END of MODULE TRIDIAGMOD >>>
! ============================================================================