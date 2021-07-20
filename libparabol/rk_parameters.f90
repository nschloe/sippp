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
use errormod

implicit none

private

! ----------------------------------------------------------------------------
! *** PUBLIC MODULE ENTITIES <<<
! ----------------------------------------------------------------------------
public  :: RK_init, s, A, b, c, tau_save,rk_method_save,method_type,         &
           set_runge_kutta
! ----------------------------------------------------------------------------
! *** END of PUBLIC MODULE ENTITIES <<<
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  logical                              :: RK_init=.FALSE.
  integer                              :: s
  real(RNP),dimension(:,:),allocatable :: A
  real(RNP),dimension(:)  ,allocatable :: b,c
  real(RNP)                            :: tau_save
  character(LEN=2)                     :: rk_method_save='--',               &
                                          method_type='--'  !EX: explicit,
                                                            !DI: diagonally implicit,
                                                            !FI: fully implicit
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------


contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE set_runge_kutta <<<
! ***
! *** Determine the Runge--Kutta method that will be used.
! ***
! ----------------------------------------------------------------------------
subroutine set_runge_kutta(rk_method)

  ! Arguments
  character(LEN=2) :: rk_method

  if ( .NOT.rk_init .OR. rk_method.NE.rk_method_save) then

      if(allocated(A)) deallocate(A)
      if(allocated(b)) deallocate(b)
      if(allocated(c)) deallocate(c)

      select case (rk_method)
      case ('EE');  call Setup_EULER_EXPLICIT()
      case ('EI');  call Setup_EULER_IMPLICIT()
      case ('EM');  call Setup_EULER_MODIFIED()
      case ('CN');  call Setup_CRANK_NICOLSON()
      case ('G1');  call Setup_GAUSS1()
      case ('G2');  call Setup_GAUSS2()
      case ('G3');  call Setup_GAUSS3()
      case ('R1');  call Setup_RADAU1()
      case ('R2');  call Setup_RADAU2()
      case ('RK');  call Setup_CLASSICAL_RUNGE_KUTTA()
      case ('HN');  call Setup_HEUN()
      case ('C2');  call Setup_CUSTOM2()
      case ('C3');  call Setup_CUSTOM3()
      case ('L3');  call Setup_LOBATTO3C()
      case default; call STOP_ON_ERROR( 'set_runge_kutta (in RUNGE-KUTTA)',  &
                                        'Runge--Kutta method not supported.' )
      end select

      ! Set module variables
      RK_init = .TRUE.
  endif

  return
end subroutine set_runge_kutta
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE set_runge_kutta >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_EULER_EXPLICIT <<<
! ***
! ----------------------------------------------------------------------------
subroutine Setup_EULER_EXPLICIT()

  s = 1
  allocate(A(s,s),b(s),c(s))

  A(1,:) = (/ 0.0_RNP /)
  b      = (/ 1.0_RNP /)
  c      = (/ 0.0_RNP /)

  method_type='EX'

end subroutine Setup_EULER_EXPLICIT
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_EULER_EXPLICIT >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_EULER_IMPLICIT <<<
! ***
! ----------------------------------------------------------------------------
subroutine Setup_EULER_IMPLICIT()

  s = 1
  allocate(A(s,s),b(s),c(s))

  A(1,:) = (/ 1.0_RNP /)
  b      = (/ 1.0_RNP /)
  c      = (/ 1.0_RNP /)

  method_type='DI'

end subroutine Setup_EULER_IMPLICIT
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_EULER_IMPLICIT >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_EULER_MODIFIED <<<
! ***
! *** Modified Euler method.
! ***
! ----------------------------------------------------------------------------
subroutine Setup_EULER_MODIFIED()

  s = 2
  allocate(A(s,s),b(s),c(s))

  A      = RESHAPE( (/ 0.0_RNP , 0.0_RNP ,    &
                       1.0_RNP , 0.0_RNP  /), &
                    (/s,s/), ORDER=(/2,1/)    )

  b      = (/ 0.5_RNP, 0.5_RNP /)

  c      = (/ 0.0_RNP, &
              1.0_RNP /)

  method_type='EX'

end subroutine Setup_EULER_MODIFIED
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_EULER_MODIFIED >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_CRANK_NICOLSON <<<
! ***
! ----------------------------------------------------------------------------
subroutine Setup_CRANK_NICOLSON()

  s = 2
  allocate(A(s,s),b(s),c(s))

  A      = RESHAPE( (/ 0.0_RNP , 0.0_RNP ,   &
                       0.5_RNP , 0.5_RNP /), &
                    (/s,s/), ORDER=(/2,1/) )

  b      = (/ 0.5_RNP, 0.5_RNP /)

  c      = (/ 0.0_RNP, &
              1.0_RNP /)

  method_type='DI'

end subroutine Setup_CRANK_NICOLSON
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_CRANK_NICOLSON >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_GAUSS1 <<<
! ***
! ----------------------------------------------------------------------------
subroutine Setup_GAUSS1()

  s = 1
  allocate(A(s,s),b(s),c(s))
  A(1,:) = (/ 0.5_RNP /)
  b      = (/ 1.0_RNP /)
  c      = (/ 0.5_RNP /)
  RK_init = .TRUE.

  method_type='DI'

end subroutine Setup_GAUSS1
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_GAUSS1 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_GAUSS2 <<<
! ***
! ----------------------------------------------------------------------------
subroutine Setup_GAUSS2()

  s = 2
  allocate(A(s,s),b(s),c(s))

  A      = RESHAPE( (/ 0.25_RNP , 0.25_RNP - SQRT(3.0_RNP)/6.0_RNP ,         &
                       0.25_RNP + SQRT(3.0_RNP)/6.0_RNP , 0.25_RNP /),       &
                    (/s,s/), ORDER=(/2,1/) )

  b      = (/ 0.5_RNP, 0.5_RNP /)

  c      = (/ 0.5_RNP - SQRT(3.0_RNP)/6.0_RNP,                               &
              0.5_RNP + SQRT(3.0_RNP)/6.0_RNP /)

  method_type='FI'

end subroutine Setup_GAUSS2
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_GAUSS2 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_GAUSS3 <<<
! ***
! ----------------------------------------------------------------------------
subroutine Setup_GAUSS3()

  s = 3
  allocate(A(s,s),b(s),c(s))

  A = RESHAPE( (/ 5.0_RNP/36.0_RNP                           ,               &
                 (10.0_RNP-3.0_RNP*SQRT(15.0_RNP))/45.0_RNP  ,               &
                 (25.0_RNP-6.0_RNP*SQRT(15.0_RNP))/180.0_RNP ,               &
                 (10.0_RNP+3.0_RNP*SQRT(15.0_RNP))/72.0_RNP  ,               &
                  2.0_RNP/9.0_RNP                            ,               &
                 (10.0_RNP-3.0_RNP*SQRT(15.0_RNP))/72.0_RNP  ,               &
                 (25.0_RNP+6.0_RNP*SQRT(15.0_RNP))/180.0_RNP ,               &
                 (10.0_RNP+3.0_RNP*SQRT(15.0_RNP))/45.0_RNP  ,               &
                  5.0_RNP/36.0_RNP  /),                                      &
                (/s,s/) , ORDER=(/2,1/) )

  b      = (/ 5.0_RNP/18.0_RNP, 4.0_RNP/9.0_RNP , 5.0_RNP/18.0_RNP /)

  c      = (/ 0.5_RNP - SQRT(15.0_RNP)/10.0_RNP,                         &
              0.5_RNP                          ,                         &
              0.5_RNP + SQRT(15.0_RNP)/10.0_RNP /)

  method_type='FI'

end subroutine Setup_GAUSS3
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_GAUSS3 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_CLASSICAL_RUNGE_KUTTA <<<
! ***
! *** Order 4.
! ***
! ----------------------------------------------------------------------------
subroutine Setup_CLASSICAL_RUNGE_KUTTA()

  s = 4
  allocate(A(s,s),b(s),c(s))

  A = RESHAPE( (/ 0.0_RNP, 0.0_RNP, 0.0_RNP, 0.0_RNP ,    &
                  0.5_RNP, 0.0_RNP, 0.0_RNP, 0.0_RNP ,    &
                  0.0_RNP, 0.5_RNP, 0.0_RNP, 0.0_RNP ,    &
                  0.0_RNP, 0.0_RNP, 1.0_RNP, 0.0_RNP /) , &
                (/s,s/) , ORDER=(/2,1/) )

  b      = (/ 1.0_RNP/6.0_RNP, 1.0_RNP/3.0_RNP, 1.0_RNP/3.0_RNP, 1.0_RNP/6.0_RNP /)

  c      = (/ 0.0_RNP , &
              0.5_RNP , &
              0.5_RNP , &
              1.0_RNP  /)

  method_type='EX'

end subroutine Setup_CLASSICAL_RUNGE_KUTTA
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_CLASSICAL_RUNGE_KUTTA >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_HEUN <<<
! ***
! *** Order 3.
! ***
! ----------------------------------------------------------------------------
subroutine Setup_HEUN()

  s = 3
  allocate(A(s,s),b(s),c(s))

  A = RESHAPE( (/ 0.0_RNP        , 0.0_RNP        , 0.0_RNP ,    &
                  1.0_RNP/3.0_RNP, 0.0_RNP        , 0.0_RNP ,    &
                  0.0_RNP        , 2.0_RNP/3.0_RNP, 0.0_RNP /) , &
                (/s,s/) , ORDER=(/2,1/) )

  b      = (/ 1.0_RNP/4.0_RNP, 0.0_RNP, 3.0_RNP/4.0_RNP /)

  c      = (/ 0.0_RNP        , &
              1.0_RNP/3.0_RNP, &
              2.0_RNP/3.0_RNP  /)

  method_type='EX'

end subroutine Setup_HEUN
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_HEUN >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_RADAU1 <<<
! ***
! *** order 3
! ----------------------------------------------------------------------------
subroutine Setup_RADAU1()

  s = 2
  allocate(A(s,s),b(s),c(s))

  A      = RESHAPE( (/ 0.25_RNP , -0.25_RNP ,       &
                       0.25_RNP ,  5.0_RNP/12.0_RNP /), &
                    (/s,s/), ORDER=(/2,1/) )

  b      = (/ 0.25_RNP ,  0.75_RNP /)

  c      = (/ 0.0_RNP,        &
              2.0_RNP/3.0_RNP /)

  method_type='FI'

end subroutine Setup_RADAU1
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_RADAU1 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_RADAU2 <<<
! ***
! *** order 3
! ----------------------------------------------------------------------------
subroutine Setup_RADAU2()

  s = 2
  allocate(A(s,s),b(s),c(s))

  A      = RESHAPE( (/ 5.0_RNP/12.0_RNP , -1.0_RNP/12.0_RNP ,   &
                       0.75_RNP         ,  0.25_RNP         /), &
                    (/s,s/), ORDER=(/2,1/) )

  b      = (/ 0.75_RNP ,  0.25_RNP /)

  c      = (/ 1.0_RNP/3.0_RNP, &
              1.0_RNP          /)

  method_type='FI'

end subroutine Setup_RADAU2
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_RADAU2 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_CUSTOM2 <<<
! ***
! *** order 3 for this particular gamma
! ----------------------------------------------------------------------------
subroutine Setup_CUSTOM2()

  real(RNP) :: gamma

  s = 2
  allocate(A(s,s),b(s),c(s))

  gamma = 0.5_RNP + SQRT(3.0_RNP)/6.0_RNP

  A      = RESHAPE( (/ gamma                 , 0.0_RNP ,   &
                       1.0_RNP-2.0_RNP*gamma , gamma   /), &
                    (/s,s/), ORDER=(/2,1/) )

  b      = (/ 0.5_RNP ,  0.5_RNP /)

  c      = (/ gamma        , &
              1.0_RNP-gamma /)

  method_type='DI'

end subroutine Setup_CUSTOM2
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_CUSTOM2 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_CUSTOM3 <<<
! ***
! *** order 3 for this particular gamma
! ----------------------------------------------------------------------------
subroutine Setup_CUSTOM3()

  real(RNP) :: gamma,b1,b2

  s = 3
  allocate(A(s,s),b(s),c(s))

  gamma = 0.5_RNP + SQRT(3.0_RNP)/6.0_RNP

  b1 =  1.5_RNP - gamma - 0.25_RNP/gamma
  b2 = -0.5_RNP         + 0.25_RNP/gamma

  A      = RESHAPE( (/ 0.0_RNP , 0.0_RNP , 0.0_RNP , &
                       gamma   , gamma   , 0.0_RNP , &
                       b1      , b2      , gamma    /), &
                    (/s,s/), ORDER=(/2,1/) )

  b      = (/ b1 , b2 , gamma /)

  c      = (/ 0.0_RNP       , & 
              2.0_RNP*gamma , &
              1.0_RNP        /)

  method_type='DI'

end subroutine Setup_CUSTOM3
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_CUSTOM3 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_LOBATTO3C <<<
! ***
! *** order 8
! ----------------------------------------------------------------------------
subroutine Setup_LOBATTO3C()

  real(RNP) :: gamma,b1,b2

  s = 5
  allocate(A(s,s),b(s),c(s))

  gamma = 0.5_RNP + SQRT(3.0_RNP)/6.0_RNP

  b1 =  1.5_RNP - gamma - 0.25_RNP/gamma
  b2 = -0.5_RNP         + 0.25_RNP/gamma

  A      = RESHAPE( (/  1.0_RNP/20.0_RNP,                                    &
                       -7.0_RNP/60.0_RNP,                                    &
                        2.0_RNP/15.0_RNP,                                    &
                       -7.0_RNP/60.0_RNP,                                    &
                        1.0_RNP/20.0_RNP,                                    &
                        1.0_RNP/20.0_RNP,                                    &
                       29.0_RNP/180.0_RNP,                                   &
                       ( 47.0_RNP-15.0_RNP*SQRT(21.0_RNP))/315.0_RNP,        &
                       (203.0_RNP-30.0_RNP*SQRT(21.0_RNP))/1260.0_RNP,       &
                       -3.0_RNP/140.0_RNP,                                   &
                        1.0_RNP/20.0_RNP,                                    &
                       (329.0_RNP+105.0_RNP*SQRT(21.0_RNP))/2880.0_RNP,      &
                       73.0_RNP/360.0_RNP,                                   &
                       (329.0_RNP-105.0_RNP*SQRT(21.0_RNP))/2880.0_RNP,      &
                        3.0_RNP/160.0_RNP,                                   &
                        1.0_RNP/20.0_RNP,                                    &
                       (203.0_RNP+30.0_RNP*SQRT(21.0_RNP))/1260.0_RNP,       &
                       ( 47.0_RNP+15.0_RNP*SQRT(21.0_RNP))/315.0_RNP,        &
                        29.0_RNP/180.0_RNP,                                  &
                        -3.0_RNP/140.0_RNP,                                  &
                         1.0_RNP/20.0_RNP,                                   &
                        49.0_RNP/180.0_RNP,                                  &
                        16.0_RNP/45.0_RNP,                                   &
                        49.0_RNP/180.0_RNP,                                  &
                        1.0_RNP/20.0_RNP        /),  (/s,s/), ORDER=(/2,1/)  )

  b      = (/  1.0_RNP/20.0_RNP , 49.0_RNP/180.0_RNP, 16.0_RNP/45.0_RNP ,    &
              49.0_RNP/180.0_RNP,  1.0_RNP/20.0_RNP                     /)

  c      = (/ 0.0_RNP,                           & 
              (7.0_RNP-SQRT(21.0_RNP))/14.0_RNP, &
              0.5_RNP,                           &
              (7.0_RNP+SQRT(21.0_RNP))/14.0_RNP, &
              1.0_RNP      /)

  method_type='FI'

end subroutine Setup_LOBATTO3C
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_LOBATTO3C >>>
! ----------------------------------------------------------------------------


end module RK_PARAMETERS
! ============================================================================
! *** END of MODULE RK_PARAMETERS >>>
! ============================================================================