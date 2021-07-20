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
use intermod

implicit none

private

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  character(LEN=2) :: cond='--' !preconditioner to use
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** PUBLIC MODULE ENTITIES <<<
! ----------------------------------------------------------------------------
public :: set_preconditioner, get_preconditioner, preconditioner
! ----------------------------------------------------------------------------
! *** End of PUBLIC MODULE ENTITIES <<<
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
! *** Choose the block preconditioner.
! ***
! *** On entry, x contains the right hand side whereas on exit, x will contain
! *** the solution to the preconditioning system.
! ***
! ----------------------------------------------------------------------------
subroutine preconditioner(N,s,x)

  !arguments
  integer(INP)               ,intent(in)    :: N,s
  real(RNP)   ,dimension(N,s),intent(inout) :: x

  select case (cond)
      case ('ID');  call IDENTITY(N,s,x)
      case ('BJ');  call FIRK_BlockJacobi(N,s,x)
      case ('LS');  call FIRK_LowerBlockSOR(N,s,x)
      case ('US');  call FIRK_UpperBlockSOR(N,s,x)
      case ('SS');  call FIRK_BlockSSOR(N,s,x)
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
subroutine IDENTITY(N,s,x)

  !arguments
  integer(INP)               ,intent(in)    :: N,s
  real(RNP)   ,dimension(N,s),intent(inout) :: x

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
subroutine FIRK_BlockJacobi(N,s,Y)

  !arguments
  integer(INP)                ,intent(in)   :: N,s
  real(RNP)   ,dimension(N,s),intent(inout) :: Y

  !locals
  integer(INP) :: i

  do i=1,s
      call M_alpha_solve( N, tau_save * A(i,i), Y(:,i) )
  enddo

end subroutine FIRK_BlockJacobi
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE FIRK_BlockJacobi >>>
! ----------------------------------------------------------------------------


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
! ----------------------------------------------------------------------------
subroutine FIRK_LowerBlockSOR(N,s,Y)

  !arguments
  integer(INP)               ,intent(in)    :: N,s
  real(RNP)   ,dimension(N,s),intent(inout) :: Y

  !locals
  integer(INP)              :: i,j
  real(RNP)   ,dimension(N) :: Lj

  do j=1,s

      call M_alpha_solve( N, tau_save*A(j,j), Y(:,j) )

      Lj = L_eps_multiply( N, Y(:,j) )
      do i=j+1,s
          Y(:,i) = Y(:,i) - tau_save* A(i,j)* Lj
      enddo

  enddo

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
! ----------------------------------------------------------------------------
subroutine FIRK_UpperBlockSOR(N,s,Y)

  !arguments
  integer(INP)               ,intent(in)    :: N,s
  real(RNP)   ,dimension(N,s),intent(inout) :: Y

  !locals
  integer(INP)              :: i,j
  real(RNP)   ,dimension(N) :: Lj

  do j=s,1,-1

      call M_alpha_solve( N, tau_save*A(j,j), Y(:,j) )

      Lj = L_eps_multiply( N, Y(:,j) )
      do i=1,j-1
         Y(:,i) = Y(:,i) - tau_save * A(i,j)* Lj
      enddo

  enddo

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
! ***
! ----------------------------------------------------------------------------
subroutine FIRK_BlockSSOR(N,s,Y)

  !arguments
  integer(INP)               ,intent(in)    :: N,s
  real(RNP)   ,dimension(N,s),intent(inout) :: Y

  !locals
  integer(INP) :: i,j
  real(RNP)    :: omega

  omega = 1.0_RNP

  !-------------------------------------------------------------------
  ! first solve equation system with
  ! (I \otimes I + tau (diag(A) + lower_triangle(A)) \otimes L)
  !-------------------------------------------------------------------
  do i=1,s

      do j=1,i-1
         Y(:,i) = Y(:,i) - omega* tau_save * A(i,j)* L_eps_multiply( N, Y(:,j) )
      enddo

      call M_alpha_solve( N, tau_save*A(i,i), Y(:,i) )

  enddo
  !-------------------------------------------------------------------


  !-------------------------------------------------------------------
  ! then do multiplication with
  ! I \otimes I + tau  diag(A) \otimes L
  !-------------------------------------------------------------------
  do i=1,s
      Y(:,i) = Y(:,i) + tau_save* A(i,i)* L_eps_multiply( N, Y(:,i) )
  enddo
  !-------------------------------------------------------------------


  !-------------------------------------------------------------------
  ! then solve equation system with
  ! (I \otimes I + tau (diag(A) + omega upper_triangle(A)) \otimes L)
  !-------------------------------------------------------------------
  do i=s,1,-1

      do j=i+1,s
         Y(:,i) = Y(:,i) - omega* tau_save * A(i,j)* L_eps_multiply( N, Y(:,j) )
      enddo

      call M_alpha_solve( N, tau_save*A(i,i), Y(:,i) )

  enddo
  !-------------------------------------------------------------------

end subroutine FIRK_BlockSSOR
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE FIRK_BlockSSOR >>>
! ----------------------------------------------------------------------------


end module PRECONDITIONERS
! ============================================================================
! *** END of MODULE PRECONDITIONERS >>>
! ============================================================================