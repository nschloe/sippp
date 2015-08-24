! ----------------------------------------------------------------------------
! *** SUBROUTINE INIT <<<
! ----------------------------------------------------------------------------
subroutine INIT(N)

  use KINDMOD
  use rk_parameters
  use params_generic, only:eps

  implicit none

  ! Arguments
  integer(INP),intent(in) :: N ! Max problem size

!   call init_GAUSS1()
!   call init_GAUSS2()
  call init_GAUSS3()

  eps = 1.0E-2_RNP
  tau = 5.0E-2_RNP

  return
end subroutine INIT
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE INIT >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE CLEANUP <<<
! ----------------------------------------------------------------------------
subroutine CLEANUP

  use rk_parameters

  implicit none

  call clean_RK()

  return
end subroutine CLEANUP
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE CLEANUP >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE individual_init <<<
! ----------------------------------------------------------------------------
subroutine individual_init(N)

  use kindmod
  use meshes
  use rk_parameters
  use operators

  implicit none

  ! Arguments
  integer(INP),intent(in) :: N

  ! create the mesh
  call set_mesh('UN',N)
  call force_init_meshpoints()

  ! set up correspondig matrix
  call force_L_init()

  !set up right hand side and starting guess for iteration
  if (allocated(RHS)) deallocate(RHS)
  if (allocated(Y))   deallocate(Y)
  allocate(RHS(1:N*s)); RHS=1.0_RNP
  allocate(  Y(1:N*s));   Y=0.0_RNP

  precond_count = 0_INP
  flop_count    = 0_INP

  return
end subroutine individual_init
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE individual_init >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE individual_cleanup <<<
! ----------------------------------------------------------------------------
subroutine individual_cleanup

  use KINDMOD
  use meshes
  use RK_parameters
  use operators

  implicit none

  !clean mesh
  call clean_meshpoints()

  !clean matrix
  call L_clean()

  if (allocated(RHS)) deallocate(RHS)
  if (allocated(Y))   deallocate(Y)

  return
end subroutine individual_cleanup
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE individual_cleanup >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE KERNEL_MAIN <<<
! ***
! *** KERNEL_MAIN wrapper. This procedure is called from the
! *** bi_entry C function.
! ----------------------------------------------------------------------------
subroutine KERNEL_MAIN(N,version,num_it)

  use KINDMOD
  use preconditioners
  use solvers
  use RK_parameters
  use ERRORMOD
  use meshes

  implicit none

  ! Parameter
  integer,parameter :: P_max = 1e9

  ! Arguments
  integer(INP),intent(in)  :: N        ! Problem size
  integer(INP),intent(in)  :: version  ! Kernel version
  real(RNP),intent(out)    :: num_it   ! number of iterations

  !locals
  integer(INP) :: num_it_INT

  if (N<0.OR.N>P_max) call STOP_ON_ERROR('KERNEL_MAIN','Invalid problem size.')

  select case(version)

  case(1)

    call set_preconditioner('ID')

  case(2)

    call set_preconditioner('BJ')

  case(3)

    call set_preconditioner('LS')

  case(4)

    call set_preconditioner('US')

  case(5)

    call set_preconditioner('SS')

  case default

    call STOP_ON_ERROR('KERNEL_MAIN','Invalid kernel version.')

  end select

if (version.ne.1) then
  call GMRES(N*s,rhs,Y,stage_matrix_multiplication,num_it_INT)
endif

!   num_it = REAL(num_it_INT,RNP)
!   num_it = REAL(precond_count,RNP)
  num_it = REAL(flop_count,RNP)

  return
end subroutine KERNEL_MAIN
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE KERNEL_MAIN >>>
! ----------------------------------------------------------------------------