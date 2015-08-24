! ============================================================================
! *** MODULE RK_PARAMETERS <<<
! ***
! *** Provides space for storing some parameters related to the Runge--Kutta
! *** methods currently used.
! ***
! *** Several other modules can access this space and problems with circular
! *** dependencies are avoided.
! ***
! ============================================================================
module RK_PARAMETERS

use KINDMOD

implicit none

private

public  :: s, A, b, c, tau_save

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  integer                 ,parameter :: s = 2
  real(RNP),dimension(2,2),parameter :: A = RESHAPE( (/ 0.25_RNP , 0.25_RNP - SQRT(3.0_RNP)/6.0_RNP ,     &
                                                        0.25_RNP + SQRT(3.0_RNP)/6.0_RNP , 0.25_RNP /),   &
                                                     (/2,2/), ORDER=(/2,1/) )
  real(RNP),dimension(2) :: b = (/ 0.5_RNP , 0.5_RNP /)
  real(RNP),dimension(2) :: c = (/ 0.5_RNP - SQRT(3.0_RNP)/6.0_RNP , 0.5_RNP + SQRT(3.0_RNP)/6.0_RNP /)
  real(RNP) :: tau_save
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

end module RK_PARAMETERS
! ============================================================================
! *** END of MODULE RK_PARAMETERS >>>
! ============================================================================
