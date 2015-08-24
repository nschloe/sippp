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

public :: L_return, L_multiply

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  logical              :: init_L_eps = .FALSE.
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

  call reaction_diffusion_return(pointL)

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
  real(RNP) ,dimension(1:current_mesh%N),intent(in) :: X
  real(RNP) ,dimension(1:current_mesh%N)            :: Z

  Z = reaction_diffusion_multiply(X)

end function L_multiply
! ----------------------------------------------------------------------------
! *** END of subroutine L_multiply >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine reaction_diffusion_init <<<
! ***
! *** L_eps = -eps \Delta + c(x)
! ***
! ----------------------------------------------------------------------------
subroutine reaction_diffusion_init()

  !locals
  integer(INP) :: i
  real(RNP)    :: h1,h2

  call init(current_mesh%N,L_eps)

  h2 = get_meshpoint(1) - get_meshpoint(0)
  do i=1,current_mesh%N
      h1 = h2                                     !left gap
      h2 = get_meshpoint(i+1) - get_meshpoint(i)  !right gap
      if (i>1)              L_eps%lower(i-1) = - 2.0_RNP*eps/(h1+h2)/h1
                            L_eps%diag(i)    =   2.0_RNP*eps/h1     /h2 +  c(get_meshpoint(i))
      if (i<current_mesh%N) L_eps%upper(i)   = - 2.0_RNP*eps/(h1+h2)/h2
  enddo

  ! set module variables
  init_L_eps = .TRUE.
  msh_save   = current_mesh
  eps_save   = eps

end subroutine reaction_diffusion_init
! ----------------------------------------------------------------------------
! *** END of subroutine reaction_diffusion_init >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine reaction_diffusion_return <<<
! ***
! *** Returns the L_eps as defined in above subroutine
! *** reaction_diffusion_init.
! ***
! ----------------------------------------------------------------------------
subroutine reaction_diffusion_return(pointL)

  !arguments
  type(tridiag),pointer            :: pointL

  if(.not.init_L_eps .OR. .NOT.mesh_up_to_date(msh_save) .OR. eps.NE.eps_save) &
  call reaction_diffusion_init()

  pointL => L_eps

end subroutine reaction_diffusion_return
! ----------------------------------------------------------------------------
! *** END of subroutine reaction_diffusion_return >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** function reaction_diffusion_multiply <<<
! ***
! *** Provides matrix multiplication with L_eps as defined in above subroutine
! *** reaction_diffusion_init.
! ***
! ----------------------------------------------------------------------------
function reaction_diffusion_multiply(X) result(Z)

  !arguments
  real(RNP) ,dimension(1:current_mesh%N),intent(in) :: X
  real(RNP) ,dimension(1:current_mesh%N)            :: Z

  if(.not.init_L_eps .OR. .NOT.mesh_up_to_date(msh_save) .OR. eps.NE.eps_save) &
  call reaction_diffusion_init()

  Z = mvmult_tridiag(L_eps,X)

end function reaction_diffusion_multiply
! ----------------------------------------------------------------------------
! *** END of function reaction_diffusion_multiply >>>
! ----------------------------------------------------------------------------

end module OPERATORS
! ============================================================================
! *** END of MODULE OPERATORS >>>
! ============================================================================
