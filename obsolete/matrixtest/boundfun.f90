! ############################################################
! *** MODULE BOUNDFUN
! ***
! *** Provides functions F that serve as upper bounds in the
! *** sense of
! ***
! *** || (z-A)^{-1} ||_{\infty} <= alpha / F
! ***
! *** The individual function receive the value of the left
! *** hand side, 
! ***
! ############################################################
module boundfun

  implicit none

  contains

  ! ==========================================================================
  ! *** FUNCTION bfun_abs
  ! ***
  ! *** Simple absolute value.
  ! ***
  ! ==========================================================================
  double precision function bfun_abs(z)

    !arguments
    double complex,intent(in) :: z

    bfun_abs = ABS(z)

  end function bfun_abs
  ! ==========================================================================
  ! *** END FUNCTION bfun_abs
  ! ==========================================================================

  ! ==========================================================================
  ! *** FUNCTION bfun_V1
  ! ***
  ! *** Distance to the line [-4*kappa-c , 0] on the negative real axis.
  ! ***
  ! ==========================================================================
  double precision function bfun_V1(z,kappa,c)

    !arguments
    double complex  ,intent(in) :: z
    double precision,intent(in) :: kappa,c

    if (REAL(z).ge.0.0D0) then
        bfun_V1 = ABS(Z)
    elseif (REAL(z).le. -4.0D0*kappa-c) then
        bfun_V1 = ABS(Z+4.0D0*kappa+c)
    else
        bfun_V1 = ABS(AIMAG(z))
    endif

  end function bfun_V1
  ! ==========================================================================
  ! *** END FUNCTION bfun_V1
  ! ==========================================================================

end module boundfun
! ############################################################
! *** END MODULE BOUNDFUN
! ############################################################