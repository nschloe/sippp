! ============================================================================
! *** MODULE params_mesh <<<
! ***
! *** Provides routines for computing the values of the right hand side and of
! *** the exact solution on the mesh point in an economic way.
! ***
! ============================================================================
MODULE params_mesh

USE kindmod
USE params_generic
USE meshes

IMPLICIT NONE

PRIVATE

PUBLIC get_f,get_solution

interface get_f
  module procedure get_f_single
  module procedure get_f_mesh
end interface get_f

interface get_solution
  module procedure get_solution_single
  module procedure get_solution_mesh
end interface get_solution

CONTAINS

! ----------------------------------------------------------------------------
! *** FUNCTION get_f_mesh <<<
! ***
! ----------------------------------------------------------------------------
function get_f_mesh(t) result(res)

  !arguments
  real(RNP) ,intent(in) :: t

  !result
  real(RNP),dimension(1:current_mesh%N) :: res

  !locals
  integer(INP) :: i

  do i=1,current_mesh%N
      res(i) = 2.0_RNP/(1.0_RNP+t)**2 *get_meshpoint(i)  &
             + c(get_meshpoint(i)) * 2.0_RNP*t /(1.0_RNP+t) * get_meshpoint(i)
  enddo

end function get_f_mesh
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_f_mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_f_single <<<
! ***
! ----------------------------------------------------------------------------
function get_f_single(i,t) result(res)

  !arguments
  integer(INP),intent(in) :: i
  real(RNP)   ,intent(in) :: t

  !result
  real(RNP) :: res

  res = 2.0_RNP/(1.0_RNP+t)**2 *get_meshpoint(i)  &
      + c(get_meshpoint(i)) * 2.0_RNP*t /(1.0_RNP+t) * get_meshpoint(i)

end function get_f_single
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_f_single >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_solution_mesh <<<
! ***
! ----------------------------------------------------------------------------
function get_solution_mesh(t) result(res)

  !arguments
  real(RNP) ,intent(in) :: t

  real(RNP),dimension(0:current_mesh%N+1) :: res

  res = 2.0_RNP*t/(1.0_RNP+t) * get_meshpoint()

end function get_solution_mesh
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_solution_mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_solution_all(t) <<<
! ***
! ----------------------------------------------------------------------------
function get_solution_single(i,t) result(res)

  !arguments
  integer(INP),intent(in) :: i
  real(RNP)   ,intent(in) :: t

  real(RNP):: res

  res = 2.0_RNP*t/(1.0_RNP+t) * get_meshpoint(i)

end function get_solution_single
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_solution_single >>>
! ----------------------------------------------------------------------------

END MODULE params_mesh
! ============================================================================
! *** END of MODULE params_mesh >>>
! ============================================================================