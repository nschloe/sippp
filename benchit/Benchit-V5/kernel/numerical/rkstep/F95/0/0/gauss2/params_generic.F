! ============================================================================
! *** MODULE params_generic <<<
! ***
! *** Provides all parameter functions which appear in the parabolic
! *** reaction diffusion equation
! ***
! ***     u_t + L_eps = f      on (0,1)x(0,1]
! ***     u(0,t) = gamma_0   , u(1,t) = gamma_1   for all t,
! ***     u(x,0) = u_0                            for all x,
! ***
! *** where
! ***
! ***     L_eps = -eps u_xx + b(x) u .
! ***
! ============================================================================
module params_generic

  USE KINDMOD

  implicit none

  private

  public c

  ! lower bound on b
  real(RNP),parameter,public :: rho = 1.0_RNP
  real(RNP)          ,public :: eps = 1.0E-2_RNP

  contains

! ----------------------------------------------------------------------------
! *** FUNCTION c(x) <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function c(x)

  !arguments
  real(RNP), intent(in) :: x

  c = 1.0_RNP

end function c
! ---------------------------------- ------------------------------------------
! *** END of FUNCTION b >>>
! ----------------------------------------------------------------------------

end module params_generic
! ============================================================================
! *** END of MODULE params_generic >>>
! ============================================================================