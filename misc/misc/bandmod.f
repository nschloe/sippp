! ============================================================================
! *** MODULE BANDMOD <<<
! ***
! *** This module defines the type BANDMATRIX representing general band
! ***  matrices and provides routines for solving linear equation systems
! *** with such a matrix.
! ***
! ============================================================================
module BANDMOD

  USE kindmod
  USE errormod

  implicit none

  private

  public :: bandmatrix, &
            init,clean, &
            bandmatrix_add, &
            put,bandmatrix_set,bandmatrix_get, &
            bandmatrix_matrix_vector_product, &
            bandmatrix_solver,bandmatrix_LU_solver, &
            bandmatrix_assign
  !--------------------------------------------------------------
  type bandmatrix
      integer(INP)                            :: N,kl,ku
      real(RNP)   ,dimension(:,:),allocatable :: data
  end type bandmatrix
  !--------------------------------------------------------------

  INTERFACE PUT
      MODULE PROCEDURE bandmatrix_put
  END INTERFACE

  INTERFACE init
      MODULE PROCEDURE bandmatrix_init
  END INTERFACE

  INTERFACE clean
      MODULE PROCEDURE bandmatrix_clean
  END INTERFACE

!   INTERFACE OPERATOR ( + )
!       MODULE PROCEDURE bandmatrix_add
!   END INTERFACE
! 
!   INTERFACE OPERATOR ( - )
!       MODULE PROCEDURE bandmatrix_subtract
!   END INTERFACE

!   INTERFACE OPERATOR ( * )
!       MODULE PROCEDURE bandmatrix_matrix_vector_product
!   END INTERFACE

  contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_init <<<
! ----------------------------------------------------------------------------
subroutine bandmatrix_init(N,kl,ku,A)

  !arguments
  integer(INP)    ,intent(in)  :: N,kl,ku !dimension and bandwidths
  type(bandmatrix),intent(out) :: A

  if(N.le.2 .OR. kl>N .OR. ku>N) &
    call STOP_ON_ERROR('INIT (in BANDMOD)',"Have a look at the arguments.")

  if(allocated(A%data)) &
    call STOP_ON_ERROR('INIT (in BANDMOD)','A is already allocated.')

  A%N   = N
  A%kl = kl
  A%ku = ku

  allocate(A%data(1:kl+ku+1,1:n))
  A%data = 0.0_RNP

end subroutine bandmatrix_init
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE bandmatrix_init >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_clean <<<
! ----------------------------------------------------------------------------
subroutine bandmatrix_clean(A)

  !arguments
  type(bandmatrix),intent(inout) :: A

  A%N   = 0
  A%kl = 0
  A%ku = 0

  if(allocated(A%data)) deallocate(A%data)

end subroutine bandmatrix_clean
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE bandmatrix_clean >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION bandmatrix_add <<<
! ***
! *** Add two matrices of type bandmatrix.
! ***
! ----------------------------------------------------------------------------
function bandmatrix_add(A,B) result(C)

  !arguments
  type(bandmatrix),intent(in) :: A,B
  type(bandmatrix)            :: C

  !locals
  integer(INP) :: klC,kuC

  if (A%N.ne.B%N) &
    CALL STOP_ON_ERROR( 'bandmatrix_add (in BANDMOD)' , &
                        'Input matrices not matching.'  )

  klC = max(A%kl,B%kl)
  kuC = max(A%ku,B%ku)

  call INIT(A%N,klC,kuC,C)

  C%data(kuC+1-A%ku:kuC+1+A%kl,:) =  C%data(kuC+1-A%ku:kuC+1+A%kl,:) &
                                      + A%data
  C%data(kuC+1-B%ku:kuC+1+B%kl,:) =  C%data(kuC+1-B%ku:kuC+1+B%kl,:) &
                                      + B%data

  RETURN
end function bandmatrix_add
! ----------------------------------------------------------------------------
! *** END of FUNCTION bandmatrix_add >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine bandmatrix_assign <<<
! ***
! ***  B = alpha* I + beta* A
! ***
! ----------------------------------------------------------------------------
subroutine bandmatrix_assign(A,alpha,beta,B)

  !arguments
  type(bandmatrix),intent(in)    :: A
  real(RNP)       ,intent(in)    :: alpha,beta
  type(bandmatrix),intent(inout) :: B

  !locals
  integer(INP) :: klmax,kumax

  if (A%N.ne.B%N) &
    CALL STOP_ON_ERROR( 'bandmatrix_assign (in BANDMOD)' , &
                        'Input matrices not matching.'     )

  if(A%kl.gt.B%kl .OR. A%ku.gt.B%ku) then
      call SHOW_WARNING('bandmatrix_assign (in BANDMOD)', &
                        'Need to increase bandwith.'      )
      klmax = max(A%kl,B%kl)
      kumax = max(A%ku,B%ku)
      call bandmatrix_clean(B)
      call bandmatrix_init(A%N,klmax,kumax,B)
  else
      B%data = 0.0_RNP !clean old entries in B
  endif

  B%data(B%ku+1-A%ku:B%ku+1+A%kl,:) = beta* A%data
  B%data(B%ku+1,:)                  = B%data(B%ku+1,:) + alpha* 1.0_RNP

  RETURN
end subroutine bandmatrix_assign
! ----------------------------------------------------------------------------
! *** END of subroutine bandmatrix_assign >>>
! ----------------------------------------------------------------------------


! ! ----------------------------------------------------------------------------
! ! *** FUNCTION bandmatrix_alpha_unity <<<
! ! ***
! ! *** Muliplies a given band matrix with alpha, then adds the unity matrix.
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine bandmatrix_alpha_unity(alpha,A,B)
! 
!   !arguments
!   real(RNP)                      :: alpha
!   type(bandmatrix),intent(in)    :: A
!   type(bandmatrix),intent(inout) :: B
! 
!   if (A%N.ne.B%N .OR. A%kl.gt.B%kl .OR. A%ku.gt.B%ku) &
!     CALL STOP_ON_ERROR( 'bandmatrix_alpha_unity (in BANDMOD)' , &
!                         'Input matrices not matching.'          )
! 
!   call bandmatrix_assign(A,alpha,B)
!   B%data(B%ku+1,:) = B%data(B%ku+1,:) + 1.0_RNP
! 
!   RETURN
! end subroutine bandmatrix_alpha_unity
! ! ----------------------------------------------------------------------------
! ! *** END of FUNCTION bandmatrix_alpha_unity >>>
! ! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_put <<<
! ***
! ***
! ----------------------------------------------------------------------------
subroutine bandmatrix_put(A)

  !arguments
  type(bandmatrix),intent(in) :: A

  !locals
  integer(INP) :: i,j

  do i=1,A%N
      do j=1,i-A%kl-1
!           write(*,'(1X,ES9.2\)')  0.0_RNP
          write(*,'(10X)',ADVANCE='NO')
      enddo
      do j=max(1,i-A%kl),min(A%N,i+A%ku)
          write(*,'(1X,ES9.2\)') A%data(A%ku+1+i-j,j)
      enddo
      do  j=i+A%ku+1,A%N
!            write(*,'(1X,ES9.2\)') 0.0_RNP
          write(*,'(10X)',ADVANCE='NO')
      enddo
!       write(*,'(1X,ES9.2\)') ( 0.0_RNP, j=1,i-A%kl-1 )
!       write(*,'(1X,ES9.2\)') &
!         ( A%data(A%ku+1+i-j,j), j=max(1,i-A%kl),min(A%N,i+A%ku) )
!       write(*,'(1X,ES9.2\)') ( 0.0_RNP, j=i+A%ku+1,A%N )
      write(*,*)
  enddo

  return
end subroutine bandmatrix_put
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE bandmatrix_put >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_set <<<
! ***
! ***
! ----------------------------------------------------------------------------
subroutine bandmatrix_set(A,i,j,datum)

  !arguments
  type(bandmatrix),intent(inout) :: A
  integer(INP)    ,intent(in)    :: i,j
  real(RNP)       ,intent(in)    :: datum

  !locals
  type(bandmatrix) :: B

  if(i.lt.1 .OR. j.lt.1 .OR. i.gt.A%N .OR. j.gt.A%N) &
    call STOP_ON_ERROR('bandmatrix_set (in BANDMOD)', &
                       'Illegal matrix position.'     )

  if ( i.gt.j+A%kl .OR. j.gt.i+A%ku ) then
      call SHOW_WARNING('bandmatrix_set (in BANDMOD)', &
                        'Need to increase bandwith.'   )

      call bandmatrix_init(A%N,max(i-j,A%kl),max(j-i,A%ku), B)
      call bandmatrix_assign(A,0.0_RNP,1.0_RNP,B) !copy A to B
      call bandmatrix_clean(A)
      call bandmatrix_init(B%N,B%kl,B%ku, A)
      call bandmatrix_assign(B,0.0_RNP,1.0_RNP,A) !copy B to A
      call bandmatrix_clean(B)
  endif

  A%data(A%ku+1+i-j,j) = datum

  return
end subroutine bandmatrix_set
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE bandmatrix_set >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_get <<<
! ***
! ***
! ----------------------------------------------------------------------------
real(RNP) FUNCTION bandmatrix_get(A,i,j)

  !arguments
  type(bandmatrix),intent(in) :: A
  integer(INP)    ,intent(in) :: i,j

  if ( i.gt.j+A%kl .OR. i.lt.1 .OR. j.gt.i+A%ku .OR. j.lt.1 ) &
    call STOP_ON_ERROR('bandmatrix_get (in BANDMOD)', &
                       'Illegal matrix position.'     )

  bandmatrix_get = A%data(A%ku+1+i-j,j)

  return
end function bandmatrix_get
! ----------------------------------------------------------------------------
! *** END of FUNCTION bandmatrix_get >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_matrix_vector_product <<<
! ***
! ***
! ----------------------------------------------------------------------------
FUNCTION bandmatrix_matrix_vector_product(A,x) result(Y)

  !arguments
  type(bandmatrix)                 ,intent(in)  :: A
  real(RNP)       ,dimension(1:A%N),intent(in)  :: x

  !result value
  real(RNP)       ,dimension(1:A%N) :: y

  !locals
  integer(INP) :: i,j

! write(*,*) ' >>> bandmatrix_matrix_vector_product'

  y = 0.0_RNP

  do j=1,A%N
      do i= max(1,j-A%ku) , min(A%N,j+A%kl)
          y(i) = y(i) + A%data(A%ku+1+i-j,j) * x(j)
      enddo
  enddo

! write(*,*) '     bandmatrix_matrix_vector_product >>>'

  return
end function bandmatrix_matrix_vector_product
! ----------------------------------------------------------------------------
! *** END of FUNCTION bandmatrix_matrix_vector_product >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_solver <<<
! ***
! ***
! ----------------------------------------------------------------------------
subroutine bandmatrix_solver(N,kl,ku,IPIV,A,x)

  !arguments
  integer(INP)                   ,intent(in)    :: N,kl,ku
  integer(INP)    ,dimension(1:N),intent(out)   :: IPIV
  type(bandmatrix)               ,intent(inout) :: A
  real(RNP)       ,dimension(1:N),intent(inout) :: x

  !locals
  integer(INP) :: INFO

  interface
    ! F77 interface to LAPACK procedure DGBSV
    SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO )
      INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   AB( LDAB, * ), B( LDB, * )
    END SUBROUTINE DGBSV
  end interface

  if (N.ne.A%N) &
  call STOP_ON_ERROR('bandmatrix_solver (in BANDMOD)', &
                     'Input values not matching.'      )

  if(size(A%data,1).lt.2*kl+ku+1) &
  call STOP_ON_ERROR('bandmatrix_solver (in BANDMOD)',                  &
                     'Input matrix not big enough to store LU factors.' )

  !call the man
  call DGBSV( N, kl, ku, 1, A%data, size(A%data,1), IPIV, x, N, INFO )

  if(INFO<0) &
    call STOP_ON_ERROR('bandmatrix_solver (in BANDMOD)',          &
                       'DGBSV reports an illegal argument value.' )
  if(INFO>0) &
    call STOP_ON_ERROR('bandmatrix_solver (in BANDMOD)',       &
                       'Matrix singular to working precision.' )

  return
end subroutine bandmatrix_solver
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE bandmatrix_solver >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE bandmatrix_LU_solver <<<
! ***
! ***
! ----------------------------------------------------------------------------
subroutine bandmatrix_LU_solver(N,kl,ku,IPIV,LU,x)

  !arguments
  integer(INP)                   ,intent(in)    :: N,kl,ku
  integer(INP)    ,dimension(1:N),intent(in)    :: IPIV
  type(bandmatrix)               ,intent(in)    :: LU
  real(RNP)       ,dimension(1:N),intent(inout) :: x

  !locals
  integer(INP) :: INFO

  interface
    ! F77 interface to LAPACK procedure DGBSV
      SUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO)
      CHARACTER          TRANS
      INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
      INTEGER            IPIV( * )
      DOUBLE PRECISION   AB( LDAB, * ), B( LDB, * )
    END SUBROUTINE DGBTRS
  end interface

! write(*,*) N,LU%N

  if (N.ne.LU%N) &
  call STOP_ON_ERROR('bandmatrix_LU_solver (in BANDMOD)', &
                     'Input values not matching.'      )

  if(size(LU%data,1).lt.2*kl+ku+1) &
  call STOP_ON_ERROR('bandmatrix_LU_solver (in BANDMOD)',            &
                     "Input matrix can't contain the LU factors." )

  !call the man
  call DGBTRS( 'N',N, kl, ku, 1, LU%data, size(LU%data,1), IPIV, x, N, INFO )

  if(INFO<0) &
    call STOP_ON_ERROR('bandmatrix_LU_solver (in BANDMOD)',          &
                       'DGBSV reports an illegal argument value.' )

  return
end subroutine bandmatrix_LU_solver
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE bandmatrix_LU_solver >>>
! ----------------------------------------------------------------------------


end module BANDMOD
! ============================================================================
! *** END of MODULE BANDMOD >>>
! ============================================================================