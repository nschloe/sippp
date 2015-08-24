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

public  :: s, A, b, c, tau, RHS, Y, stage_matrix_multiplication,             &
           init_GAUSS1, init_GAUSS2, init_GAUSS3, clean_rk,                  &
           precond_count, flop_count

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  integer(INP) :: s
  real(RNP),dimension(:,:),allocatable :: A
  real(RNP),dimension(:)  ,allocatable :: b
  real(RNP),dimension(:)  ,allocatable :: c

  real(RNP)    :: tau
  integer(RNP) :: precond_count = 0_INP
  integer(RNP) :: flop_count    = 0_INP

  real(RNP),dimension(:),allocatable :: RHS
  real(RNP),dimension(:),allocatable :: Y
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

contains

! ----------------------------------------------------------------------------
! *** FUNCTION stage_matrix_multiplication <<<
! ----------------------------------------------------------------------------
function stage_matrix_multiplication(Ns,Y) result(Z)

  use operators

  !arguments
  integer(INP)              ,intent(in) :: Ns
  real(RNP)   ,dimension(Ns),intent(in) :: Y   ! stage vector
  real(RNP)   ,dimension(Ns)            :: Z

  !locals
  integer(INP)            :: i,j,N
  real(RNP),dimension(Ns) :: L

  Z = Y

  N = Ns/s

  do j=1,s

      L = L_multiply( Y(N*(j-1)+1:N*j) )

      do i=1,s
          Z(N*(i-1)+1:N*i) = Z(N*(i-1)+1:N*i) + tau * A(i,j) * L
      end do
  end do

end function stage_matrix_multiplication
! ----------------------------------------------------------------------------
! *** END of FUNCTION stage_matrix_multiplication >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE init_GAUSS1 <<<
! ***
! ----------------------------------------------------------------------------
subroutine init_GAUSS1()

  call clean_rk()

  s = 1
  allocate(A(s,s))
  allocate(b(s))
  allocate(c(s))

  A =  0.5_RNP
  b =  1.0_RNP
  c =  0.5_RNP

end subroutine init_GAUSS1
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE init_GAUSS1 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE init_GAUSS2 <<<
! ***
! ----------------------------------------------------------------------------
subroutine init_GAUSS2()

  call clean_rk()

  s = 2
  allocate(A(s,s))
  allocate(b(s))
  allocate(c(s))

  A      = RESHAPE( (/ 0.25_RNP , 0.25_RNP - SQRT(3.0_RNP)/6.0_RNP ,         &
                        0.25_RNP + SQRT(3.0_RNP)/6.0_RNP , 0.25_RNP /),      &
                    (/2,2/), ORDER=(/2,1/) )

  b      = (/ 0.5_RNP, 0.5_RNP /)

  c      = (/ 0.5_RNP - SQRT(3.0_RNP)/6.0_RNP, &
              0.5_RNP + SQRT(3.0_RNP)/6.0_RNP /)

end subroutine init_GAUSS2
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE init_GAUSS2 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE init_GAUSS3 <<<
! ***
! ----------------------------------------------------------------------------
subroutine init_GAUSS3()

  call clean_rk()

  s = 3
  allocate(A(s,s))
  allocate(b(s))
  allocate(c(s))

  A = RESHAPE( (/ 5.0_RNP/36.0_RNP , (10.0_RNP-3.0_RNP*SQRT(15.0_RNP))/45.0_RNP , (25.0_RNP-6.0_RNP*SQRT(15.0_RNP))/180.0_RNP,    &
                  (10.0_RNP+3.0_RNP*SQRT(15.0_RNP))/72.0_RNP, 2.0_RNP/9.0_RNP   , (10.0_RNP-3.0_RNP*SQRT(15.0_RNP))/72.0_RNP ,    &
                  (25.0_RNP+6.0_RNP*SQRT(15.0_RNP))/180.0_RNP , (10.0_RNP+3.0_RNP*SQRT(15.0_RNP))/45.0_RNP, 5.0_RNP/36.0_RNP  /), &
                (/3,3/) , ORDER=(/2,1/) )

  b      = (/ 5.0_RNP/18.0_RNP, 4.0_RNP/9.0_RNP , 5.0_RNP/18.0_RNP /)

  c      = (/ 0.5_RNP - SQRT(15.0_RNP)/10.0_RNP,                             &
              0.5_RNP                          ,                             &
              0.5_RNP + SQRT(15.0_RNP)/10.0_RNP /)

end subroutine init_GAUSS3
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE init_GAUSS3 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE clean_rk <<<
! ***
! ----------------------------------------------------------------------------
subroutine clean_rk()

  s = 0_INP
  if (allocated(A)) deallocate(A)
  if (allocated(b)) deallocate(b)
  if (allocated(c)) deallocate(c)

end subroutine clean_rk
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE clean_rk >>>
! ----------------------------------------------------------------------------


end module RK_PARAMETERS
! ============================================================================
! *** END of MODULE RK_PARAMETERS >>>
! ============================================================================