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

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  logical                            :: spatial_init = .FALSE.
  type(mesh)                         :: msh_save = mesh('--',0_INP)
  real(RNP),dimension(:),allocatable :: spatial_temp1
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

CONTAINS

! ----------------------------------------------------------------------------
! *** SUBROUTINE spatial_initialisation <<<
! ***
! *** Calculate useful terms which will be used by other functions in this
! *** module later on.
! ***
! ----------------------------------------------------------------------------
subroutine spatial_initialisation()

  !locals
  real(RNP) :: rhoeps

! write(*,*) ' >>> spatial_initialisation'

  if(allocated(spatial_temp1)) deallocate(spatial_temp1)
  allocate(spatial_temp1(0:current_mesh%N+1))

  !let spatial_temp1 have the mesh
  spatial_temp1 = get_meshpoint()

  msh_save     = current_mesh
  spatial_init = .TRUE.

! write(*,*) '     spatial_initialisation >>>'

end subroutine spatial_initialisation
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE spatial_initialisation >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_f_mesh <<<
! ***
! ----------------------------------------------------------------------------
function get_f_mesh(t) result(res)

  !arguments
  real(RNP) ,intent(in) :: t

  !result
  real(RNP),dimension(1:current_mesh%N) :: res

  res = 0.0_RNP

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

  res = 0.0_RNP

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

  !locals
  integer(INP) :: i

  !results
  real(RNP),dimension(0:current_mesh%N+1) :: res

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_up_to_date(msh_save) )                &
  call spatial_initialisation()
  !-------------------------------------------------------------------

  forall (i=0:current_mesh%N+1) &
  res(i) = (t + 0.5_RNP*spatial_temp1(i)*spatial_temp1(i)/eps) * ERFC(0.5_RNP*spatial_temp1(i)/SQRT(eps*t))      &
         - SQRT(t/PI/eps)*spatial_temp1(i)*EXP(-0.25_RNP*spatial_temp1(i)*spatial_temp1(i)/eps/t)

end function get_solution_mesh
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_solution_mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_solution_single(t) <<<
! ***
! ----------------------------------------------------------------------------
function get_solution_single(i,t) result(res)

  !arguments
  integer(INP),intent(in) :: i
  real(RNP)   ,intent(in) :: t

  real(RNP):: res

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_up_to_date(msh_save) )                &
  call spatial_initialisation()
  !-------------------------------------------------------------------

  res = (t + 0.5_RNP*spatial_temp1(i)*spatial_temp1(i)/eps) * ERFC(0.5_RNP*spatial_temp1(i)/SQRT(eps*t))      &
      - SQRT(t/PI/eps)*spatial_temp1(i)*EXP(-0.25_RNP*spatial_temp1(i)*spatial_temp1(i)/eps/t)

end function get_solution_single
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_solution_single >>>
! ----------------------------------------------------------------------------

END MODULE params_mesh
! ============================================================================
! *** END of MODULE params_mesh >>>
! ============================================================================