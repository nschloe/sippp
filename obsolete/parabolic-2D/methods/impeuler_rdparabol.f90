! ============================================================================
! *** MODULE SOLVE_RDPARABOL <<<
! ***
! ***
! *** u_t + L_eps u = f        on (0,1)x(0,1],
! *** u(0,t) = u(1,t) = 0      for t \in (0,1],
! *** u(x,0) = 0               for x \in [0,1],
! ***
! *** where   L_eps = -eps u_xx + b(x) u
! *** and         f = f(t).
! ***
! *** IMPLICIT EULER METHOD
! ***
! ============================================================================
module SOLVE_RDPARABOL

use KINDMOD
use PARAMS

implicit none

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE get_rhs <<<
! ***
! *** Given U, this routines returns the vector
! ***
! *** (I - tau/2 L_eps)*u + tau/2 * (f(t)+f(t-tau)) + lhs_terms
! ***
! *** for the right hand side of the considered equation system.
! ***
! *** Also includes terms coming from the boundary values on the left hand
! *** side of the equation system (appearing only in first and last
! *** component when using a tridiagonal equation system).
! ***
! ----------------------------------------------------------------------------
subroutine get_rhs(N,rhs,x,u,eps,tau,t)

  !arguments
  integer(INT_KIND)                 ,intent(in)  :: N
  real(DBL_KIND)   ,dimension(1:N)  ,intent(out) :: rhs
  real(DBL_KIND)   ,dimension(0:N+1),intent(in)  :: x
  real(DBL_KIND)   ,dimension(1:N)  ,intent(in)  :: u
  real(DBL_KIND)                    ,intent(in)  :: tau,eps,t

  !locals
  integer(INT_KIND) :: k

  do k=1,N
      rhs(k) = u(k) +  tau*f(x(k),t,eps)
  enddo

  !respect left hand side terms
  rhs(1) = rhs(1) + 2.0_DBL_KIND*tau*eps/(  x(2)-  x(0))/(  x(1)-x(0)) * gamma0(t)
  rhs(N) = rhs(N) + 2.0_DBL_KIND*tau*eps/(x(N+1)-x(N-1))/(x(N+1)-x(N)) * gamma1(t)

end subroutine get_rhs
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE get_rhs >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE get_diags <<<
! ***
! *** Returns the three diagonals LO (lower), C (central), and UP (upper)
! *** the discretization of the operator
! ***
! *** (I + tau/2 L_eps) .
! ***
! *** Look out for GET_RHS to include terms that are due to appear on the
! *** right hand side of the equation system (->boundary values!).
! ***
! ***
! ----------------------------------------------------------------------------
subroutine get_diags(N,DL,D,DU,x,tau,eps)

  !arguments
  integer(int_kind),                   intent(in)  :: N
  real(DBL_KIND)   , dimension(1:N)  , intent(out) :: D
  real(DBL_KIND)   , dimension(1:N-1), intent(out) :: DL,DU
  real(DBL_KIND)   , dimension(0:N+1), intent(in)  :: x
  real(DBL_KIND)   ,                   intent(in)  :: tau,eps

  !locals
  integer(int_kind) :: i
  real(DBL_KIND)    :: h1,h2

  h2 = x(1) - x(0)
  do i=1,N
      h1 = h2             !left gap
      h2 = x(i+1) - x(i)  !right gap
      if (i>1) DL(i-1) =              - tau*  eps*2.0_DBL_KIND/(h1+h2)/h1
               D(i)    = 1.0_DBL_KIND + tau*( eps*2.0_DBL_KIND/h1     /h2 + b(x(i)) )
      if (i<N) DU(i)   =              - tau*  eps*2.0_DBL_KIND/(h1+h2)/h2
  enddo

end subroutine get_diags
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE get_diags >>>
! ----------------------------------------------------------------------------

! ! ----------------------------------------------------------------------------
! ! *** SUBROUTINE solve <<<
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine solve(Nx,x,Nt,tau,eps,u)
! 
!   !arguments
!   integer(int_kind)                  ,intent(in)  :: Nx,Nt
!   real(dbl_kind)                     ,intent(in)  :: tau,eps
!   real(dbl_kind)   ,dimension(0:Nx+1),intent(in)  :: x
!   real(dbl_kind)   ,dimension(0:Nx+1),intent(out) :: u
! 
!   !locals
!   real(dbl_kind)                   :: t
!   real(dbl_kind),dimension(1:Nx)   :: RHS,D
!   real(dbl_kind),dimension(1:Nx-1) :: DL,DU
!   integer(int_kind)                :: i,j
! 
!   call u0(u,x,eps)
! 
!   call get_diags(Nx,DL,D,DU,x(0:Nx+1),tau,eps)
! 
!   !####################################################
!   !solve step by step
!   t = 0.0_DBL_KIND
!   do j=1,Nt
! 
!       t = t + tau
!       !---------------------------------
!       !get rhs
!       call get_rhs(RHS,x(0:Nx+1),u(1:Nx),eps,tau,t)
!       !---------------------------------
!       U = RHS
! 
!       !---------------------------------
!       call tridiag_gauss(Nx,DL,D,DU,u)
!       u(0)    = gamma0(t)
!       u(Nx+1) = gamma1(t)
!       !---------------------------------
! 
!   enddo
!   !####################################################
! 
! end subroutine solve
! ! ----------------------------------------------------------------------------
! ! *** END of SUBROUTINE solve >>>
! ! ----------------------------------------------------------------------------

end module SOLVE_RDPARABOL
! ============================================================================
! *** END of MODULE SOLVE_RDPARABOL >>>
! ============================================================================