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
  real(RNP),dimension(:),allocatable :: g,gx,gxx,spatial_temp4
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

CONTAINS

! ----------------------------------------------------------------------------
! *** SUBROUTINE spatial_initiation <<<
! ***
! *** Calculate useful terms which will be used by other functions in this
! *** module later on.
! ***
! ----------------------------------------------------------------------------
subroutine spatial_initiation(msh)

  !arguments
  type(mesh),intent(in) :: msh

  !locals
  integer(INP) :: i
  real(RNP)    :: rhoeps,temp1,temp2,temp3

  rhoeps = sqrt(rho)/sqrt(eps)

  if(allocated(g)) deallocate(g)
  allocate(g(0:msh%N+1))

  if(allocated(gx)) deallocate(gx)
  allocate(gx(0:msh%N+1))

  if(allocated(gxx)) deallocate(gxx)
  allocate(gxx(0:msh%N+1))

  if(allocated(spatial_temp4)) deallocate(spatial_temp4)
  allocate(spatial_temp4(0:msh%N+1))

  temp1 = 1.0_RNP - exp(-rhoeps)
  do i=0,msh%N+1
      temp2 = exp(-get_meshpoint(msh,i)*rhoeps)
      temp3 = exp(-(1.0_RNP-get_meshpoint(msh,i))*rhoeps)
      !--------------------------------------------
      g(i)     = (1.0_RNP-temp2)*(1.0_RNP-temp3)/temp1
      gx(i)    = rhoeps    * (   temp2 - temp3 )/temp1
      gxx(i)   = rhoeps**2 * ( - temp2 - temp3 )/temp1
      !--------------------------------------------
      spatial_temp4(i) = (get_meshpoint(msh,i)-0.5_RNP)**2
  end do

  msh_save     = msh
  spatial_init = .TRUE.

end subroutine spatial_initiation
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE spatial_initiation >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_f_mesh <<<
! ***
! ----------------------------------------------------------------------------
function get_f_mesh(msh,t) result(res)

  !arguments
  type(mesh),intent(in) :: msh
  real(RNP) ,intent(in) :: t

  !result
  real(RNP),dimension(0:msh%N+1) :: res

  !locals
  real(RNP)    :: h,hx,hxx,ht
  integer(INP) :: i

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_eqv(msh,msh_save))                    &
  call spatial_initiation(msh)
  !-------------------------------------------------------------------

  do i=0,msh%N+1
      !--------------------------------------------
      h   = exp(-20.0_RNP*(T1-t)*spatial_temp4(i))
      hx  = h * 40.0_RNP * (t-T1) * (get_meshpoint(msh,i)-0.5_RNP)
      hxx = h * 40.0_RNP * (T1-t)*(40.0_RNP*(T1-t)*spatial_temp4(i)-1.0_RNP)
      ht  = h * 20.0_RNP * spatial_temp4(i)
      !--------------------------------------------

      !--------------------------------------------
      res(i) = g(i)*ht - eps*(gxx(i)*h + 2.0_RNP*gx(i)*hx + g(i)*hxx)        &
             + c(get_meshpoint(msh,i))*g(i)*h
      !--------------------------------------------
  enddo

end function get_f_mesh
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_f_mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_f_single <<<
! ***
! ----------------------------------------------------------------------------
function get_f_single(msh,i,t) result(res)

  !arguments
  type(mesh)  ,intent(in) :: msh
  integer(INP),intent(in) :: i
  real(RNP)   ,intent(in) :: t

  !result
  real(RNP) :: res

  !locals
  real(RNP)    :: h,hx,hxx,ht

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_eqv(msh,msh_save))                    &
  call spatial_initiation(msh)
  !-------------------------------------------------------------------

  !--------------------------------------------
  h   = exp(-20.0_RNP*(T1-t)*spatial_temp4(i))
  hx  = h * 40.0_RNP * (t-T1) * (get_meshpoint(msh,i)-0.5_RNP)
  hxx = h * 40.0_RNP * (T1-t)*(40.0_RNP*(T1-t)*spatial_temp4(i)-1.0_RNP)
  ht  = h * 20.0_RNP * spatial_temp4(i)
  !--------------------------------------------

  !--------------------------------------------
  res = g(i)*ht - eps*(gxx(i)*h + 2.0_RNP*gx(i)*hx + g(i)*hxx)               &
      + c(get_meshpoint(msh,i))*g(i)*h
  !--------------------------------------------

end function get_f_single
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_f_single >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_solution_mesh <<<
! ***
! ----------------------------------------------------------------------------
function get_solution_mesh(msh,t) result(res)

  !arguments
  type(mesh),intent(in) :: msh
  real(RNP) ,intent(in) :: t

  real(RNP),dimension(0:msh%N+1) :: res

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_eqv(msh,msh_save))                    &
  call spatial_initiation(msh)
  !-------------------------------------------------------------------

  res = g * exp(-20.0_RNP*(T1-t)*spatial_temp4)

end function get_solution_mesh
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_solution_mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_solution_all(t) <<<
! ***
! ----------------------------------------------------------------------------
function get_solution_single(msh,i,t) result(res)

  !arguments
  type(mesh)  ,intent(in) :: msh
  integer(INP),intent(in) :: i
  real(RNP)   ,intent(in) :: t

  real(RNP):: res

  !-------------------------------------------------------------------
  ! calculate the spatial part once and then keep it
  !-------------------------------------------------------------------
  if (.NOT.spatial_init .OR. .NOT.mesh_eqv(msh,msh_save))                    &
  call spatial_initiation(msh)
  !-------------------------------------------------------------------

  res = g(i) * exp(-20.0_RNP*(T1-t)*spatial_temp4(i))

end function get_solution_single
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_solution_single >>>
! ----------------------------------------------------------------------------

END MODULE params_mesh
! ============================================================================
! *** END of MODULE params_mesh >>>
! ============================================================================