! ============================================================================
! *** MODULE SOLVE_RDPARABOL <<<
! ***
! ============================================================================
module SOLVE_RDPARABOL

use KINDMOD
use PARAMS
use LINSOLVE

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
subroutine get_rhs(rhs,x,u,eps,tau,t)

  !arguments
  real(DBL_KIND), dimension(1:), intent(out) :: rhs
  real(DBL_KIND), dimension(0:), intent(in)  :: x
  real(DBL_KIND), dimension(1:), intent(in)  :: u
  real(DBL_KIND),                intent(in)  :: tau,eps,t

  !locals
  integer(INT_KIND) :: N,k
  real   (DBL_KIND) :: uxx,ukm1,ukp1,g,h1,h2

  N = size(u,1)

  h2 = x(1)-x(0)
  do k=1,N

    h1 = h2             !left gap
    h2 = x(k+1) - x(k)  !right gap

    !-----------------
    ! get u_{xx}
    if (k==1) then
        ukm1 = gamma0(t)
    else
        ukm1 = u(k-1)
    endif
    if (k==N) then
        ukp1 = gamma1(t)
    else
        ukp1 = u(k+1)
    endif
    uxx = 2.0_DBL_KIND *( (ukm1/h1+ukp1/h2)/(h1+h2) - u(k)/(h1*h2) )
    !-----------------

    g = tau * ( f(x(k),t,eps) + f(x(k),t-tau,eps) )/2.0_DBL_KIND
!   g = tau * f(x,t-tau/2.0_DBL_KIND)

    rhs(k) = u(k) - tau/2.0_DBL_KIND * ( -eps*uxx  + b(x(k))*u(k) ) + g

  enddo

  !respect left hand side terms
  rhs(1) = rhs(1) + tau*eps/(  x(2)-  x(0))/(  x(1)-x(0)) * gamma0(t)
  rhs(N) = rhs(N) + tau*eps/(x(N+1)-x(N-1))/(x(N+1)-x(N)) * gamma1(t)

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
! *** The implemeted scheme has order of consistency 2 for uniform meshes
! *** and order of consistency 1 for non-uniform meshes.
! ***
! ----------------------------------------------------------------------------
subroutine get_diags(lo,c,up,x,N,tau,eps)

  !arguments
  real(DBL_KIND)   , dimension(1:N)  , intent(out) :: c
  real(DBL_KIND)   , dimension(1:N-1), intent(out) :: lo,up
  real(DBL_KIND)   , dimension(0:N+1), intent(in)  :: x
  integer(int_kind),                   intent(in)  :: N
  real(DBL_KIND)   ,                   intent(in)  :: tau,eps

  !locals
  integer(int_kind) :: i
  real(DBL_KIND)    :: h1,h2


!   lo =              - tau *eps/ h**2 /2.0_DBL_KIND
!   c  = 1.0_DBL_KIND + tau *eps/ h**2
!   up = lo
!   lo =                tau/2.0_DBL_KIND *(            -eps/(h**2) )
!   c  = 1.0_DBL_KIND + tau/2.0_DBL_KIND *(2.0_DBL_KIND*eps/(h**2) )
!   up =                tau/2.0_DBL_KIND *(            -eps/(h**2) )
!   do i=1,N
!       c(i) = c(i) + tau/2.0_DBL_KIND * b(x(i))
!   enddo

  h2 = x(1) - x(0)
  do i=1,N
      h1 = h2             !left gap
      h2 = x(i+1) - x(i)  !right gap
      if (i>1) lo(i-1) =              - tau*eps/(h1+h2)/h1
               c(i)    = 1.0_DBL_KIND + tau/2.0_DBL_KIND *( eps*2.0_DBL_KIND/(h1*h2) + b(x(i)) )
      if (i<N) up(i)   =              - tau*eps/(h1+h2)/h2
!       if (i>1) lo(i-1) =                tau/2.0_DBL_KIND *( -eps*2.0_DBL_KIND/(h1+h2)/h2          )
!                c(i)    = 1.0_DBL_KIND + tau/2.0_DBL_KIND *(  eps*2.0_DBL_KIND/(h1*h2)    + b(x(i)))
!       if (i<N) up(i)   =                tau/2.0_DBL_KIND *( -eps*2.0_DBL_KIND/(h1+h2)/h1          )
  enddo

end subroutine get_diags
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE get_diags >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE solve <<<
! ***
! ----------------------------------------------------------------------------
subroutine solve(Nt,x,tau,eps)

  !arguments
  integer(int_kind)              ,intent(in) :: Nt
  real(dbl_kind)                 ,intent(in) :: tau,eps
  real(dbl_kind)   ,dimension(0:),intent(in) :: x

  !locals
  real(dbl_kind),dimension(0:size(x,1)-1) :: u
  real(dbl_kind)                          :: t
  real(dbl_kind),dimension(1:size(x,1)-2) :: rhs,c
  real(dbl_kind),dimension(1:size(x,1)-3) :: lo,up
  integer(int_kind)                       :: i,j,Nx

  Nx = size(x,1)-2

  call u0(u,x,eps)

  call get_diags(lo,c,up,x(0:Nx+1),Nx,tau,eps)

  !####################################################
  !solve step by step and put solution to standard out
  t = 0.0_DBL_KIND
  do i=0,Nx+1
      write(*,fmt='(3(F20.15))') t,x(i),u(i)
  enddo
  do j=1,Nt

      t = t + tau
      !---------------------------------
      call get_rhs(rhs,x(0:Nx+1),u(1:Nx),eps,tau,t)
      !---------------------------------

      !---------------------------------
      call tridiag_generic(u(1:Nx),lo,c,up,rhs)
      u(0)    = gamma0(t)
      u(Nx+1) = gamma1(t)
      !---------------------------------

! if(t>1.869.and.t<1.875) then
!     do i=1,Nx
!         if(x(i)>0.19.and.x(i)<0.2) write(*,*) x(i),t,u(i)
!     enddo
! endif

      do i=0,Nx+1
          write(*,fmt='(3(F20.15))') t,x(i),u(i)
      enddo

  enddo
  !####################################################

end subroutine solve
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE solve >>>
! ----------------------------------------------------------------------------

end module SOLVE_RDPARABOL
! ============================================================================
! *** END of MODULE SOLVE_RDPARABOL >>>
! ============================================================================