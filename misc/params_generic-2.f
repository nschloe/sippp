! ============================================================================
! *** MODULE params_generic <<<
! ***
! *** Provides all parameter functions which appear in the parabolic
! *** reaction diffusion equation
! ***
! ***     u_t + L_eps = f      on (0,1)x(0,1]
! ***     u(0,t) = gamma_0   , u(1,t) = gamma_1   for all t,
! ***     u(x,0) = u_0                            for all x,
! ***
! *** where
! ***
! ***     L_eps = -eps u_xx + b(x) u .
! ***
! ============================================================================
module params_generic

  USE KINDMOD

  implicit none

  private

  public c,gamma0,gamma1,u0

  ! strict lower bound on b
  real(RNP),parameter,public :: rho = 1.0_RNP, & !lower bound on c
                                x0  = 0.0_RNP, &
                                x1  = 1.0_RNP, &
                                T1  = 1.0_RNP

  real(RNP),public :: eps = 1.0E-2_RNP

  contains

! ----------------------------------------------------------------------------
! *** FUNCTION c(x) <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function c(x)

  !arguments
  real(RNP), intent(in) :: x

  c = 1.0_RNP

end function c
! ---------------------------------- ------------------------------------------
! *** END of FUNCTION c >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION f(t) <<<
! ***
! ----------------------------------------------------------------------------
pure function f(x,t) result(res)

  !arguments
  real(RNP)                  ,intent(in) :: x,t
  real(RNP) :: res

  !locals
  real(RNP) :: rhoeps,g,gx,gxx,h,hx,hxx,ht
  real(RNP) :: temp1,temp2,temp3,temp4
  integer(INP) :: i

  rhoeps = sqrt(rho)/sqrt(eps)

  !--------------------------------------------
  temp1 = (1.0_RNP-exp(-rhoeps))
  temp2 = exp(-x*rhoeps)
  temp3 = exp(-rhoeps*(1.0_RNP-x))
  g   = (1.0_RNP-temp2)*(1.0_RNP-temp3)
  gx  =  rhoeps   * (   temp2 - temp3 )
  gxx = rhoeps**2 * ( - temp2 - temp3 )
  !--------------------------------------------

  !--------------------------------------------
  temp4 = (x-0.5_RNP)**2
  h   = exp(-20.0_RNP*(T1-t)*temp4)
  hx  = h * 40.0_RNP * (t-T1) * (x-0.5_RNP)
  hxx = h * 40.0_RNP * (T1-t)*(40.0_RNP*(T1-t)*temp4-1.0_RNP)
  ht  = 20.0_RNP*temp4 * h
  !--------------------------------------------

  !--------------------------------------------
  res = (g * ht - eps*(gxx*h + 2.0_RNP*gx*hx + g*hxx) + c(x) * g*h)/temp1
  !--------------------------------------------

end function f
! ----------------------------------------------------------------------------
! *** END of FUNCTION f >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION f(t) <<<
! ***
! ----------------------------------------------------------------------------
function f_alt(Nx,x,t) result(res)

  !arguments
  integer(INP)               ,intent(in) :: Nx
  real(RNP),dimension(0:Nx+1),intent(in) :: x
  real(RNP)                  ,intent(in) :: t

  real(RNP),dimension(1:Nx) :: res

  !locals
  integer(INP) :: i
  real(RNP)     :: rhoeps,temp1,temp2,temp3,h,hx,hxx,ht
  logical                           ,save :: f_init=.FALSE.
  real(RNP),dimension(:),allocatable,save :: g,gx,gxx,temp4

  if (.not.f_init) then
      rhoeps = sqrt(rho)/sqrt(eps)
      temp1 = (1.0_RNP-exp(-rhoeps))
      allocate(g(1:Nx))
      allocate(gx(1:Nx))
      allocate(gxx(1:Nx))
      allocate(temp4(1:Nx))
      do i=1,Nx
          temp2 = exp(-x(i)*rhoeps)
          temp3 = exp(-(1.0_RNP-x(i))*rhoeps)
          !--------------------------------------------
          g(i)     = (1.0_RNP-temp2)*(1.0_RNP-temp3)/temp1
          gx(i)    = rhoeps    * (   temp2 - temp3 )/temp1
          gxx(i)   = rhoeps**2 * ( - temp2 - temp3 )/temp1
          !--------------------------------------------
          temp4(i) = (x(i)-0.5_RNP)**2
      end do
      f_init = .TRUE.
  endif

  do i=1,Nx
      !--------------------------------------------
      h   = exp(-20.0_RNP*(T1-t)*temp4(i))
      hx  = h * 40.0_RNP * (t-T1) * (x(i)-0.5_RNP)
      hxx = h * 40.0_RNP * (T1-t)*(40.0_RNP*(T1-t)*temp4(i)-1.0_RNP)
      ht  = h * 20.0_RNP * temp4(i)
      !--------------------------------------------

      !--------------------------------------------
      res(i) = g(i)*ht - eps*(gxx(i)*h + 2.0_RNP*gx(i)*hx + g(i)*hxx)        &
             + c(x(i))*g(i)*h
      !--------------------------------------------
  enddo

end function f_alt
! ----------------------------------------------------------------------------
! *** END of FUNCTION f >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** function u0 <<<
! ***
! *** u_0(x)
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function u0(x)

  !arguments
  real(RNP),intent(in) :: x

  !locals
  real(RNP) :: rhoeps

  rhoeps = sqrt(rho)/sqrt(eps)

  u0 = (1.0_RNP-exp(-x*rhoeps))*(1.0_RNP-exp(-(1.0_RNP-x)*rhoeps))           &
     / (1.0_RNP-exp(-rhoeps))                                                &
     * exp(-20.0_RNP*(T1)*(x-0.5_RNP)**2)

end function u0
! ----------------------------------------------------------------------------
! *** END of function u0 >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION gamma0 <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function gamma0(t)

  !arguments
  real(RNP), intent(in)  :: t

  gamma0 = 0.0_RNP

end function gamma0
! ----------------------------------------------------------------------------
! *** END of FUNCTION gamma0 >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION gamma1 <<<
! ***
! ----------------------------------------------------------------------------
real(RNP) function gamma1(t)

  !arguments
  real(RNP), intent(in)  :: t

  gamma1 = 0.0_RNP

end function gamma1
! ----------------------------------------------------------------------------
! *** END of FUNCTION gamma1 >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION solution(x,t) <<<
! ***
! ----------------------------------------------------------------------------
pure real(RNP) function solution(x,t)

  !arguments
  real(RNP), intent(in) :: x,t

  !locals
  real(RNP) :: rhoeps

  rhoeps = sqrt(rho)/sqrt(eps)

  solution = (1.0_RNP-exp(-x*rhoeps))*(1.0_RNP-exp(-(1.0_RNP-x)*rhoeps))     &
           / (1.0_RNP-exp(-rhoeps))                                          &
           * exp(-20.0_RNP*(T1-t)*(x-0.5_RNP)**2)

end function solution
! ----------------------------------------------------------------------------
! *** END of FUNCTION solution >>>
! ----------------------------------------------------------------------------

end module params_generic
! ============================================================================
! *** END of MODULE params_generic >>>
! ============================================================================