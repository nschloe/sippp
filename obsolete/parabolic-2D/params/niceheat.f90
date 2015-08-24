! ============================================================================
! *** MODULE params_generic <<<
! ***
! *** Provides all parameter functions which appear in the parabolic
! *** reaction diffusion equation
! ***
! ***     u_t + L_eps = f      on (0,1)x(0,1]
! ***     u(x,t) = gamma_0     for all x in Gamma and  all t,
! ***     u(x,0) = u_0         for all x,
! ***
! *** where
! ***
! ***     L_eps = -eps \Delta u + c(x) u .
! ***
! *** The set-up is meant to reside in the two-dimensional square [0,1]x[0,1].
! ***
! ============================================================================
module params_generic

  USE KINDMOD

  implicit none

  ! strict lower bound on b
  real(RNP),parameter :: c   = 1.0_RNP, &
                         rho = 1.0_RNP, & !lower bound on c
                         x0  = 0.0_RNP, &
                         x1  = 1.0_RNP, &
                         T1  = 1.5_RNP

  real(RNP) :: eps=1.0_RNP

  contains

! ----------------------------------------------------------------------------
! *** FUNCTION gamma(x,t) <<<
! ***
! ----------------------------------------------------------------------------
real(RNP) function gamma(x,t)

  !arguments
  real(RNP),dimension(1:2),intent(in) :: x
  real(RNP),               intent(in) :: t

  gamma = 0.0_RNP

end function gamma
! ----------------------------------------------------------------------------
! *** END of FUNCTION gamma >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION f(t) <<<
! ***
! ----------------------------------------------------------------------------
real(RNP) function f(x,t,eps)

  !arguments
  real(RNP),dimension(1:2),intent(in) :: x
  real(RNP),               intent(in) :: t,eps

  f = 0.07_RNP

end function f
! ----------------------------------------------------------------------------
! *** END of FUNCTION f >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE u0 <<<
! ***
! *** u_0(x)
! ***
! ----------------------------------------------------------------------------
real(RNP) function u0(x,eps)

  !arguments
  real(RNP),dimension(1:2),intent(in) :: x
  real(RNP),               intent(in) :: eps

!   u0 = sin(2.0_RNP*Pi*x(1)) * sin(3.0_RNP*Pi*x(2))

  u0 = x(1) * (1.0_RNP-x(1)) * x(2) * (1.0_RNP-x(2))                 &
     * (  exp(- 50.0_RNP*((x(1)-0.75_RNP)**2+(x(2)-0.75_RNP)**2))    &
        + exp(-100.0_RNP*((x(1)-0.20_RNP)**2+(x(2)-0.45_RNP)**2))    &
        + exp(-150.0_RNP*((x(1)-0.80_RNP)**2+(x(2)-0.50_RNP)**2))    &
        + exp(-  0.1_RNP*((x(1)-0.60_RNP)**2+(x(2)-0.05_RNP)**2)) )

end function u0
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE u0 >>>
! ----------------------------------------------------------------------------

end module params_generic
! ============================================================================
! *** END of MODULE params_generic >>>
! ============================================================================