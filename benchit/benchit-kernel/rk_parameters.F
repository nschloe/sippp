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

public  :: RK_init, s, A, b, c, tau_save

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  logical                              :: RK_init=.FALSE.
  integer                              :: s=2
  real(RNP),dimension(:,:),allocatable :: A
  real(RNP),dimension(:)  ,allocatable :: b,c
  real(RNP)                            :: tau_save
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

end module RK_PARAMETERS
! ============================================================================
! *** END of MODULE RK_PARAMETERS >>>
! ============================================================================
