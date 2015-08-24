! ============================================================================
! *** MODULE PRECONDITIONERS <<<
! ***
! *** Provides a quite general set of iterative solvers for linear equation
! *** systems, most prominently being GMRES.
! ***
! ============================================================================
module PRECONDITIONERS

use KINDMOD
use ERRORMOD
use RK_PARAMETERS
use OPERATORS
use MODTRIDIAG

implicit none

private

public :: LowerBlock1,LowerBlock2

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE LowerBlock1 <<<
! ***
! ----------------------------------------------------------------------------
subroutine LowerBlock1(N,rhs,Y)

  !arguments
  integer(INP)                 ,intent(in)  :: N
  real(RNP)   ,dimension(1:N*s),intent(in)  :: rhs
  real(RNP)   ,dimension(1:N*s),intent(out) :: Y

  !locals
  integer(INP)               :: i,j
  type(tridiag)              :: D
  type(tridiag),pointer      :: pointL

  !-----------------------------------
  flop_count = flop_count + s*( 1+ (3*N-2) + N + (8*N-7) + 3*N + (N+1)*(s-1) )
  !-----------------------------------

  !initialise D and a temporary vector
  call init(N,D)

  call L_return(pointL)

  Y = rhs

  do i=1,s

      do j=1,i-1
         Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
                          - tau * A(i,j)* mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
      enddo

      D = tau* A(i,j)* pointL
      D%diag = D%diag + 1.0_RNP

      call tridiag_gauss(D,Y(N*(i-1)+1:N*i))

  enddo

  !clean up
  call clean(D)

end subroutine LowerBlock1
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE LowerBlock1 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE LowerBlock2 <<<
! ***
! ----------------------------------------------------------------------------
subroutine LowerBlock2(N,rhs,Y)

  !arguments
  integer(INP)                 ,intent(in)  :: N
  real(RNP)   ,dimension(1:N*s),intent(in)  :: rhs
  real(RNP)   ,dimension(1:N*s),intent(out) :: Y

  !locals
  integer(INP)                  :: i,j
  type(tridiag)                 :: D
  type(tridiag),pointer         :: pointL
  real(RNP)    ,dimension(N) :: L_times_Yj

  !-----------------------------------
  flop_count = flop_count + s*( 1+ (3*N-2) + N + (8*N-7) + 3*N + (N+1)*(s-1) )
  !-----------------------------------

  !initialise D and a temporary vector
  call init(N,D)

  call L_return(pointL)

  Y = rhs

  do j=1,s

      D      = tau* A(i,j)* pointL
      D%diag = D%diag + 1.0_RNP
      call tridiag_gauss(D,Y(N*(j-1)+1:N*j))

      L_times_Yj = mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )

      do i=j+1,s
         Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i) - tau* A(i,j)* L_times_Yj
      enddo

  enddo

  !clean up
  call clean(D)

end subroutine LowerBlock2
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE LowerBlock2 >>>
! ----------------------------------------------------------------------------


! !-------------------------------------------------------------------
! ! *** SUBROUTINE FIRK_UpperBlockSOR <<<
! ! ***
! ! *** Preconditioner for the equation system
! ! ***
! ! ***   ( I \otimes I + tau * A \otimes L ) Y = RHS
! ! ***
! ! *** which appears when solving a parabolic problem with a Runge--Kutta
! ! *** method. A is assumed to be full, hence direct solvers will be
! ! *** inefficient. For the use of GMRES (A generally not symmetric!) the block
! ! *** diagonal preconditiner
! ! ***
! ! ***   B^{-1} = (   I \otimes I
! ! ***              + tau ( diag(A) + omega upper_triangle(A) ) \otimes L)^{-1}
! ! ***
! ! *** is implemented here.
! ! ***
! ! *** For OMEGA=1, this is exactly the upper block Gauss--Seidel
! ! *** preconditioner.
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine FIRK_UpperBlockSOR(Ns,rhs,Y)
! 
!   !arguments
!   integer(INP)                ,intent(in)  :: Ns
!   real(RNP)   ,dimension(1:Ns),intent(in)  :: rhs
!   real(RNP)   ,dimension(1:Ns),intent(out) :: Y
! 
!   !locals
!   integer(INP)                  :: i,j,N
!   type(tridiag)                 :: D
!   type(tridiag),pointer         :: pointL
!   real(RNP)    ,dimension(Ns/s) :: L_times_Yj
! 
!   N = Ns/s !blocksize
! 
!   !-----------------------------------
!   flop_count = flop_count + s*( 1+ (3*N-2) + N + (8*N-7) + 3*N + (N+1)*(s-1) )
!   !-----------------------------------
! 
!   !initialise D
!   call init(N,D)
! 
!   call L_return(pointl)
! 
!   Y = rhs
! 
!   do j=s,1,-1
! 
! !       do j=i+1,s
! !          Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
! !                           - omega* tau * A(i,j)                              &
! !                                  * mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
! !       enddo
! ! 
! !       D = tau * A(i,i) * pointL
! !       D%diag = D%diag + 1.0_RNP
! ! 
! !       call tridiag_gauss(D,Y(N*(i-1)+1:N*i))
! 
!       D = tau * A(j,j) * pointL
!       D%diag = D%diag + 1.0_RNP
! 
!       call tridiag_gauss(D,Y(N*(j-1)+1:N*j))
! 
!       L_times_Yj = mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
! 
!       do i=j-1,1,-1
!          Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i) - omega* tau* A(i,j)* L_times_Yj
!       enddo
! 
!   enddo
! 
!   !clean up
!   call clean(D)
! 
! end subroutine FIRK_UpperBlockSOR
! ! ----------------------------------------------------------------------------
! ! *** END of SUBROUTINE FIRK_UpperBlockSOR >>>
! ! ----------------------------------------------------------------------------


end module PRECONDITIONERS
! ============================================================================
! *** END of MODULE PRECONDITIONERS >>>
! ============================================================================
