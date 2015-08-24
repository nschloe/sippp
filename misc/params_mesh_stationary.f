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
  real(RNP),dimension(:),allocatable :: spatial_temp1,spatial_temp2
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

  rhoeps = sqrt(rho)/sqrt(eps)

  if(allocated(spatial_temp1)) deallocate(spatial_temp1)
! write(*,*) 'waypoint 2a',current_mesh%N+1,allocated(spatial_temp1)
  allocate(spatial_temp1(0:current_mesh%N+1))
! write(*,*) 'waypoint 2b'
  if(allocated(spatial_temp2)) deallocate(spatial_temp2)
! write(*,*) 'waypoint 2c'
  allocate(spatial_temp2(0:current_mesh%N+1))
! write(*,*) 'waypoint 2d'

  spatial_temp1 = (1.0_RNP-exp(- get_meshpoint()          *rhoeps))       &
                * (1.0_RNP-exp(-(1.0_RNP- get_meshpoint())*rhoeps))       &
                / (1.0_RNP - exp(-rhoeps))

  spatial_temp2 = (   exp(- get_meshpoint()*rhoeps)                       &
                    + exp(-(1.0_RNP- get_meshpoint())*rhoeps) )           &
                / (1.0_RNP - exp(-rhoeps))

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

  !locals
  integer(INP) :: i

! write(*,*) ' >>> get_f_mesh'

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_up_to_date(msh_save) )                &
  call spatial_initialisation()
  !-------------------------------------------------------------------

  do i=1,current_mesh%N
      res(i) = c(get_meshpoint(i)) * spatial_temp1(i) + rho * spatial_temp2(i)
  enddo

! write(*,*) '     get_f_mesh >>>'

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

  !locals
  real(RNP) :: sinpt,cospt

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_up_to_date(msh_save) )                &
  call spatial_initialisation()
  !-------------------------------------------------------------------

  sinpt = sin(Pi*t)
  cospt = cos(Pi*t)

  res = c(get_meshpoint(i)) * spatial_temp1(i) + rho * spatial_temp2(i)

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

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_up_to_date(msh_save) )                &
  call spatial_initialisation()
  !-------------------------------------------------------------------

  res = spatial_temp1

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

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_up_to_date(msh_save) )                &
  call spatial_initialisation()
  !-------------------------------------------------------------------

  res = spatial_temp1(i)

end function get_solution_single
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_solution_single >>>
! ----------------------------------------------------------------------------

END MODULE params_mesh
! ============================================================================
! *** END of MODULE params_mesh >>>
! ============================================================================