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

public :: set_preconditioner, get_preconditioner, preconditioner


! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  character(LEN=2) :: cond='--'
  real(RNP)        :: omega=1.0_RNP
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE set_preconditioner <<<
! ***
! *** Determines the preconditioner to use.
! ----------------------------------------------------------------------------
subroutine set_preconditioner(pc)

  !arguments
  character(LEN=2) :: pc

  cond = pc

end subroutine set_preconditioner
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE set_preconditioner >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_preconditioner <<<
! ***
! *** Determines the preconditioner to use.
! ----------------------------------------------------------------------------
FUNCTION get_preconditioner()

  !result
  character(LEN=2) :: get_preconditioner

  get_preconditioner = cond

end FUNCTION get_preconditioner
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_preconditioner >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE preconditioner <<<
! ***
! *** The identity precondioner. Does nothing.
! ----------------------------------------------------------------------------
subroutine preconditioner(n,rhs,x)

  !arguments
  integer(INP)               ,intent(in)  :: n
  real(RNP)   ,dimension(1:n),intent(in)  :: rhs
  real(RNP)   ,dimension(1:n),intent(out) :: x

  select case (cond)
      case ('ID');  call IDENTITY(n,rhs,x)
      case ('BJ');  call FIRK_BlockJacobi(n,rhs,x)
      case ('LS');  call FIRK_LowerBlockSOR(n,rhs,x)
      case ('US');  call FIRK_UpperBlockSOR(n,rhs,x)
      case ('SS');  call FIRK_BlockSSOR(n,rhs,x)
      case default; call STOP_ON_ERROR( 'preconditioner (in PRECONDITIONERS)', &
                                        'preconditioner not supported.'   )
  end select

end subroutine preconditioner
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE preconditioner >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE IDENTITY <<<
! ***
! *** The identity precondioner. Does nothing.
! ----------------------------------------------------------------------------
subroutine IDENTITY(n,rhs,x)

  !arguments
  integer(INP)               ,intent(in)  :: n
  real(RNP)   ,dimension(1:n),intent(in)  :: rhs
  real(RNP)   ,dimension(1:n),intent(out) :: x

  x = rhs

end subroutine IDENTITY
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE IDENTITY >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE FIRK_BlockJacobi <<<
! ***
! *** Preconditioner for the equation system
! ***
! ***   ( I \otimes I + tau * A \otimes L ) Y = RHS
! ***
! *** which appears when solving a parabolic problem with a Runge--Kutta
! *** method. A is assumed to be full, hence direct solvers will be
! *** inefficient. For the use of GMRES (A generally not symmetric!) the block
! *** diagonal preconditiner
! ***
! ***   B^{-1} = ( I \otimes I + tau diag(A) \otimes L)^{-1}
! ***
! *** is implemented here.
! ***
! ----------------------------------------------------------------------------
subroutine FIRK_BlockJacobi(Ns,rhs,Y)

  !arguments
  integer(INP)                ,intent(in)  :: Ns
  real(RNP)   ,dimension(1:Ns),intent(in)  :: rhs
  real(RNP)   ,dimension(1:Ns),intent(out) :: Y

  !locals
  integer(INP)          :: i,N
  type(tridiag)         :: D
  type(tridiag),pointer :: pointL

  N = Ns/s

  !initialise D
  call init(N,D)

  call L_return(pointl)

  Y = rhs

  do i=1,s

      D = tau_save * A(i,i) * pointL
      D%diag = D%diag + 1.0_RNP

      call tridiag_gauss(D,Y(N*(i-1)+1:N*i))

  enddo

  !clean up
  call clean(D)

end subroutine FIRK_BlockJacobi
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE FIRK_BlockJacobi >>>
! ----------------------------------------------------------------------------


! ! ----------------------------------------------------------------------------
! ! *** SUBROUTINE FIRK_LowerBlockGaussSeidel <<<
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
! ! ***   B^{-1} = ( I \otimes I + tau lower_triangle(A) \otimes L)^{-1}
! ! ***
! ! *** is implemented here.
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine FIRK_LowerBlockGaussSeidel(Ns,rhs,Y)
! 
!   !arguments
!   integer(INP)                ,intent(in)  :: Ns
!   real(RNP)   ,dimension(1:Ns),intent(in)  :: rhs
!   real(RNP)   ,dimension(1:Ns),intent(out) :: Y
! 
!   !locals
!   integer(INP)          :: i,j,N
!   type(tridiag)         :: D
!   type(tridiag),pointer :: pointL
! 
!   N = Ns/s
! 
!   !initialise D
!   call init(N,D)
! 
!   call L_return(pointl)
! 
!   Y = rhs
! 
!   do i=1,s
! 
!       do j=1,i-1
!          Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
!                           - mvmult_tridiag( tau_save * A(i,j) * pointL , Y(N*(j-1)+1:N*j) )
!       enddo
! 
!       D = tau_save * A(i,i) * pointL
!       D%diag = D%diag + 1.0_RNP
! 
!       call tridiag_gauss(D,Y(N*(i-1)+1:N*i))
! 
!   enddo
! 
!   !clean up
!   call clean(D)
! 
! end subroutine FIRK_LowerBlockGaussSeidel
! ! ----------------------------------------------------------------------------
! ! *** END of SUBROUTINE FIRK_LowerBlockGaussSeidel >>>
! ! ----------------------------------------------------------------------------


! ! ----------------------------------------------------------------------------
! ! *** SUBROUTINE FIRK_UpperBlockGaussSeidel <<<
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
! ! ***   B^{-1} = ( I \otimes I + tau upper_triangle(A) \otimes L)^{-1}
! ! ***
! ! *** is implemented here.
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine FIRK_UpperBlockGaussSeidel(Ns,rhs,Y)
! 
!   !arguments
!   integer(INP)                ,intent(in)  :: Ns
!   real(RNP)   ,dimension(1:Ns),intent(in)  :: rhs
!   real(RNP)   ,dimension(1:Ns),intent(out) :: Y
! 
!   !locals
!   integer(INP)          :: i,j,N
!   type(tridiag)         :: D
!   type(tridiag),pointer :: pointL
! 
!   N = Ns/s
! 
!   !initialise D
!   call init(N,D)
! 
!   call L_return(pointl)
! 
!   Y = rhs
! 
!   do i=s,1,-1
! 
!       do j=i+1,s
!          Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
!                           - mvmult_tridiag( tau_save * A(i,j) * pointL , Y(N*(j-1)+1:N*j) )
!       enddo
! 
!       D = tau_save * A(i,i) * pointL
!       D%diag = D%diag + 1.0_RNP
! 
!       call tridiag_gauss(D,Y(N*(i-1)+1:N*i))
! 
!   enddo
! 
!   !clean up
!   call clean(D)
! 
! end subroutine FIRK_UpperBlockGaussSeidel
! ! ----------------------------------------------------------------------------
! ! *** END of SUBROUTINE FIRK_UpperBlockGaussSeidel >>>
! ! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE FIRK_LowerBlockSOR <<<
! ***
! *** Preconditioner for the equation system
! ***
! ***   ( I \otimes I + tau * A \otimes L ) Y = RHS
! ***
! *** which appears when solving a parabolic problem with a Runge--Kutta
! *** method. A is assumed to be full, hence direct solvers will be
! *** inefficient. For the use of GMRES (A generally not symmetric!) the block
! *** diagonal preconditiner
! ***
! ***   B^{-1} = (   I \otimes I
! ***              + tau ( diag(A) + omega lower_triangle(A) ) \otimes L)^{-1}
! ***
! *** is implemented here.
! ***
! *** For OMEGA=1, this is exactly the lower block Gauss--Seidel
! *** preconditioner.
! ***
! ----------------------------------------------------------------------------
subroutine FIRK_LowerBlockSOR(Ns,rhs,Y)

  !arguments
  integer(INP)                ,intent(in)  :: Ns
  real(RNP)   ,dimension(1:Ns),intent(in)  :: rhs
  real(RNP)   ,dimension(1:Ns),intent(out) :: Y

  !locals
  integer(INP)          :: i,j,N
  type(tridiag)         :: D
  type(tridiag),pointer :: pointL

  N = Ns/s

  !initialise D
  call init(N,D)

  call L_return(pointl)

  Y = rhs

  do i=1,s

      do j=1,i-1
         Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
                          - omega* tau_save * A(i,j)                         &
                                 * mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
      enddo

      D = tau_save * A(i,i) * pointL
      D%diag = D%diag + 1.0_RNP

      call tridiag_gauss(D,Y(N*(i-1)+1:N*i))

  enddo

  !clean up
  call clean(D)

end subroutine FIRK_LowerBlockSOR
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE FIRK_LowerBlockSOR >>>
! ----------------------------------------------------------------------------


!-------------------------------------------------------------------
! *** SUBROUTINE FIRK_UpperBlockSOR <<<
! ***
! *** Preconditioner for the equation system
! ***
! ***   ( I \otimes I + tau * A \otimes L ) Y = RHS
! ***
! *** which appears when solving a parabolic problem with a Runge--Kutta
! *** method. A is assumed to be full, hence direct solvers will be
! *** inefficient. For the use of GMRES (A generally not symmetric!) the block
! *** diagonal preconditiner
! ***
! ***   B^{-1} = (   I \otimes I
! ***              + tau ( diag(A) + omega upper_triangle(A) ) \otimes L)^{-1}
! ***
! *** is implemented here.
! ***
! *** For OMEGA=1, this is exactly the upper block Gauss--Seidel
! *** preconditioner.
! ***
! ----------------------------------------------------------------------------
subroutine FIRK_UpperBlockSOR(Ns,rhs,Y)

  !arguments
  integer(INP)                ,intent(in)  :: Ns
  real(RNP)   ,dimension(1:Ns),intent(in)  :: rhs
  real(RNP)   ,dimension(1:Ns),intent(out) :: Y

  !locals
  integer(INP)          :: i,j,N
  type(tridiag)         :: D
  type(tridiag),pointer :: pointL

  N = Ns/s

  !initialise D
  call init(N,D)

  call L_return(pointl)

  Y = rhs

  do i=s,1,-1

      do j=i+1,s
         Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
                          - omega* tau_save * A(i,j)                         &
                                 * mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
      enddo

      D = tau_save * A(i,i) * pointL
      D%diag = D%diag + 1.0_RNP

      call tridiag_gauss(D,Y(N*(i-1)+1:N*i))

  enddo

  !clean up
  call clean(D)

end subroutine FIRK_UpperBlockSOR
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE FIRK_UpperBlockSOR >>>
! ----------------------------------------------------------------------------


!-------------------------------------------------------------------
! *** SUBROUTINE FIRK_BlockSSOR <<<
! ***
! *** Preconditioner for the equation system
! ***
! ***   ( I \otimes I + tau * A \otimes L ) Y = RHS
! ***
! *** which appears when solving a parabolic problem with a Runge--Kutta
! *** method. A is assumed to be full, hence direct solvers will be
! *** inefficient. For the use of GMRES (A generally not symmetric!) the block
! *** diagonal preconditiner
! ***
! ***   B = (I \otimes I + tau (diag(A) + omega lower_triangle(A)) \otimes L)
! ***       (I \otimes I + tau  diag(A)                            \otimes L)^{-1}
! ***       (I \otimes I + tau (diag(A) + omega upper_triangle(A)) \otimes L)
! ***
! *** is implemented here.
! ***
! *** For OMEGA=1, this is exactly the upper block Gauss--Seidel
! *** preconditioner.
! ***
! ----------------------------------------------------------------------------
subroutine FIRK_BlockSSOR(Ns,rhs,Y)

  !arguments
  integer(INP)                ,intent(in)  :: Ns
  real(RNP)   ,dimension(1:Ns),intent(in)  :: rhs
  real(RNP)   ,dimension(1:Ns),intent(out) :: Y

  !locals
  integer(INP)          :: i,j,N
  type(tridiag)         :: D
  type(tridiag),pointer :: pointL

  N = Ns/s

  !initialise D
  call init(N,D)

  call L_return(pointl)

  Y = rhs

  !-------------------------------------------------------------------
  ! first solve equation system with
  ! (I \otimes I + tau (diag(A) + omega lower_triangle(A)) \otimes L)
  !-------------------------------------------------------------------
  do i=1,s

      do j=1,i-1
         Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
                          - omega* tau_save * A(i,j)                         &
                                 * mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
      enddo

      D = tau_save * A(i,i) * pointL
      D%diag = D%diag + 1.0_RNP

      call tridiag_gauss(D,Y(N*(i-1)+1:N*i))

  enddo
  !-------------------------------------------------------------------


  !-------------------------------------------------------------------
  ! then do multiplication with
  ! I \otimes I + tau  diag(A) \otimes L
  !-------------------------------------------------------------------
  do i=1,s
      Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                    &
                       + tau_save* A(i,i)                                    &
                                 * mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
  enddo
  !-------------------------------------------------------------------


  !-------------------------------------------------------------------
  ! then solve equation system with
  ! (I \otimes I + tau (diag(A) + omega upper_triangle(A)) \otimes L)
  !-------------------------------------------------------------------
  do i=s,1,-1

      do j=i+1,s
         Y(N*(i-1)+1:N*i) = Y(N*(i-1)+1:N*i)                                 &
                          - omega* tau_save * A(i,j)                         &
                                 * mvmult_tridiag( pointL , Y(N*(j-1)+1:N*j) )
      enddo

      D = tau_save * A(i,i) * pointL
      D%diag = D%diag + 1.0_RNP

      call tridiag_gauss(D,Y(N*(i-1)+1:N*i))

  enddo
  !-------------------------------------------------------------------

  !clean up
  call clean(D)

end subroutine FIRK_BlockSSOR
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE FIRK_BlockSSOR >>>
! ----------------------------------------------------------------------------


end module PRECONDITIONERS
! ============================================================================
! *** END of MODULE PRECONDITIONERS >>>
! ============================================================================
