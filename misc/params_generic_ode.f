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

  public c,gamma0,gamma1,u0

  ! strict lower bound on b
  real(RNP),parameter,public :: rho = 1.0_RNP, & !lower bound on c
                                x0  = 0.0_RNP, &
                                x1  = 1.0_RNP, &
                                T1  = 2.0_RNP

  real(RNP),public :: eps = 1.0E-2_RNP

  contains

! ----------------------------------------------------------------------------
! *** FUNCTION c(x) <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function c(x)

  !arguments
  real(RNP), intent(in) :: x

  c = 1.0_RNP !sqrt(1+x) !exp(x) !1.0_RNP

end function c
! ---------------------------------- ------------------------------------------
! *** END of FUNCTION b >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION f(t) <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function f(x,t)

  !arguments
  real(RNP),intent(in) :: x,t

  f = Pi*cos(Pi*t) + c(x)*sin(Pi*t)

end function f
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_f_single >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE u0 <<<
! ***
! *** u_0(x)
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function u0(x)

  !arguments
  real(RNP),intent(in)  :: x

  u0 = 0.0_RNP

end function u0
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE u0 >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION gamma0 <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function gamma0(t)

  !arguments
  real(RNP), intent(in)  :: t

  gamma0 = sin(Pi*t)

end function gamma0
! ----------------------------------------------------------------------------
! *** END of FUNCTION gamma0 >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION gamma1 <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function gamma1(t)

  !arguments
  real(RNP), intent(in)  :: t

  gamma1 = sin(Pi*t)

end function gamma1
! ----------------------------------------------------------------------------
! *** END of FUNCTION gamma1 >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION solution(x,t) <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function solution(x,t)

  !arguments
  real(RNP), intent(in) :: x,t

  solution = sin(Pi*t)

end function solution
! ----------------------------------------------------------------------------
! *** END of FUNCTION solution >>>
! ----------------------------------------------------------------------------

end module params_generic
! ============================================================================
! *** END of MODULE params_generic >>>
! ============================================================================