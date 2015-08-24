! ----------------------------------------------------------------------------
! *** SUBROUTINE KERNEL_MAIN <<<
! ***
! *** KERNEL_MAIN wrapper. This procedure is called from the
! *** bi_entry C function.
! ----------------------------------------------------------------------------
subroutine KERNEL_MAIN(N,version,num_it)

  use KINDMOD
  use preconditioners
  use RK_parameters

  implicit none

  ! Parameter
  integer,parameter :: P_max = 20

  ! Arguments
  integer(INP),intent(in)  :: N        ! Problem size
  integer(INP),intent(in)  :: version  ! Kernel version
  real(RNP),   intent(out) :: num_it   ! FLOP/sec
  real(RNP),dimension(N*S) :: rhs,Y

  if (N<0.OR.N>P_max) call STOP_ON_ERROR('KERNEL_MAIN: Invalid problem size.')

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

    call STOP_ON_ERROR('KERNEL_MAIN: Invalid kernel version.')

  end select

  rhs = 1.0_RNP

  call GMRES(N*s,rhs,Y,30,num_it)

  return
end subroutine KERNEL_MAIN
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE KERNEL_MAIN >>>
! ----------------------------------------------------------------------------
