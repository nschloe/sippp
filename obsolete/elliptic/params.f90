! ============================================================================
! *** MODULE PARAMS <<<
! ***
! *** Provides all parameter functions which appear in the elliptic
! *** pde equation
! ***
! ***     L_eps = f   on (0,1)
! ***     u(0) = gamma_0, u(1) = gamma_1
! ***
! *** where
! ***
! ***     L_eps = -eps u_xx + b(x) u .
! ***
! ============================================================================
module PARAMS

  USE KINDMOD

  implicit none

  real(dbl_kind),parameter :: rho    = 1.0_DBL_KIND, &   !strict lower bound on b
                              x0     = 0.0_DBL_KIND,  &
                              x1     = 1.0_DBL_KIND,  &
                              gamma0 = 0.0_DBL_KIND , &   !boundary conditions
                              gamma1 = 0.0_DBL_KIND

  contains

! ----------------------------------------------------------------------------
! *** FUNCTION b(x) <<<
! ***
! ----------------------------------------------------------------------------
real(dbl_kind) function b(x)

  !arguments
  real(DBL_KIND), intent(in) :: x

  b = 1.0_DBL_KIND

end function b
! ---------------------------------- ------------------------------------------
! *** END of FUNCTION b >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION f(t) <<<
! ***
! ----------------------------------------------------------------------------
real(dbl_kind) function f(x,eps)

  !arguments
  real(DBL_KIND), intent(in) :: x,eps

  !locals
  real(DBL_KIND) :: rhoeps

  rhoeps = sqrt(rho)/sqrt(eps)

    f = ( rho*( exp(-x*rhoeps) + exp(-(1.0_DBL_KIND-x)*rhoeps) )                            &
        + b(x) * (1.0_DBL_KIND-exp(-x*rhoeps))*(1.0_DBL_KIND-exp(-(1.0_DBL_KIND-x)*rhoeps)) &
        )/ (1.0_DBL_KIND-exp(-rhoeps))

end function f
! ----------------------------------------------------------------------------
! *** END of FUNCTION f >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION solution(x,t) <<<
! ***
! ----------------------------------------------------------------------------
real(dbl_kind) function solution(x,eps)

  !arguments
  real(DBL_KIND), intent(in) :: x,eps

  !locals
  real(DBL_KIND) :: rhoeps

  rhoeps = sqrt(rho)/sqrt(eps)

  solution = (1.0_DBL_KIND-exp(-x*rhoeps))*(1.0_DBL_KIND-exp(-(1.0_DBL_KIND-x)*rhoeps)) &
           / (1.0_DBL_KIND-exp(-rhoeps))

end function solution
! ----------------------------------------------------------------------------
! *** END of FUNCTION solution >>>
! ----------------------------------------------------------------------------

end module PARAMS
! ============================================================================
! *** END of MODULE PARAMS >>>
! ============================================================================
