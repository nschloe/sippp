! ============================================================================
! *** MODULE OPERATORS <<<
! ***
! *** Provides discretizations of the operators used, such as
! ***
! *** where   L_eps = -eps \Delta + c(x)
! ***
! ============================================================================
module OPERATORS

use KINDMOD
use params_generic
use MODTRIDIAG
USE MESHES

implicit none

private

public :: L_return, L_multiply, force_L_init, L_clean

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  logical              :: init_L = .FALSE.
  type(mesh)           :: msh_save = mesh('--',0_INP)
  real(RNP)            :: eps_save = 0.0_RNP
  type(tridiag),target :: L_eps
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

contains

! ----------------------------------------------------------------------------
! *** subroutine L_return <<<
! ***
! *** Wrapper for all *_return functions in this module.
! ***
! ----------------------------------------------------------------------------
subroutine L_return(pointL)

  !arguments
  type(tridiag),pointer :: pointL

  if(.not.init_L .OR. .NOT.mesh_eq(msh_save,get_mesh()) .OR. eps.NE.eps_save) &
  call L_init()

  pointL => L_eps

end subroutine L_return
! ----------------------------------------------------------------------------
! *** END of subroutine L_return >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine L_multiply <<<
! ***
! *** Wrapper for all *_multiply functions in this module.
! ***
! ----------------------------------------------------------------------------
function L_multiply(X) result(Z)

  !arguments
  real(RNP),dimension(1:get_mesh_N()),intent(in) :: X
  real(RNP),dimension(1:get_mesh_N())            :: Z

  if(.not.init_L .OR. .NOT.mesh_eq(msh_save,get_mesh()) .OR. eps.NE.eps_save) &
  call L_init()

  Z = mvmult_tridiag(L_eps,X)

end function L_multiply
! ----------------------------------------------------------------------------
! *** END of subroutine L_multiply >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine L_init <<<
! ***
! *** L_eps = -eps \Delta + c(x)
! ***
! ----------------------------------------------------------------------------
subroutine L_init()

  !locals
  integer(INP) :: i
  real(RNP)    :: h1,h2

  call L_clean()
  call init(get_mesh_N(),L_eps)

  h2 = get_meshpoint(1) - get_meshpoint(0)

  do i=1,get_mesh_N()
      h1 = h2                                     !left gap
      h2 = get_meshpoint(i+1) - get_meshpoint(i)  !right gap
      if (i>1)            L_eps%lower(i-1) = - 2.0_RNP*eps/(h1+h2)/h1
                          L_eps%diag(i)    =   2.0_RNP*eps/h1     /h2 +  c(get_meshpoint(i))
      if (i<get_mesh_N()) L_eps%upper(i)   = - 2.0_RNP*eps/(h1+h2)/h2
  enddo

  ! set module variables
  init_L = .TRUE.
  msh_save   = get_mesh()
  eps_save   = eps

end subroutine L_init
! ----------------------------------------------------------------------------
! *** END of subroutine L_init >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine force_L_init <<<
! ***
! *** Not crucial to normal operation of the module, but sometimes one might
! *** wish to force updating of L although not immediately necessary.
! ***
! ----------------------------------------------------------------------------
subroutine force_L_init()

  call L_init()

end subroutine force_L_init
! ----------------------------------------------------------------------------
! *** END of subroutine force_L_init >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine L_clean <<<
! ***
! *** L_eps = -eps \Delta + c(x)
! ***
! ----------------------------------------------------------------------------
subroutine L_clean()

  call clean(L_eps)
  init_L = .FALSE.

end subroutine L_clean
! ----------------------------------------------------------------------------
! *** END of subroutine L_clean >>>
! ----------------------------------------------------------------------------


end module OPERATORS
! ============================================================================
! *** END of MODULE OPERATORS >>>
! ============================================================================