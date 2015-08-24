! ============================================================================
! *** MODULE SOLVE_ELLIPTIC <<<
! ***
! *** Provides part of the routines for solving the elliptic pde
! ***
! *** -eps u_xx + b u = f
! *** u(0)=gamma0, u(1)=gamma1
! ***
! ============================================================================
module SOLVE_ELLIPTIC

  use KINDMOD
  use PARAMS
  use LINSOLVE

  implicit none

  contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE get_diags <<<
! ***
! *** Returns the three diagonals DL (lower), D (central), and DU (upper)
! *** the discretization of operator.
! ***
! *** Look out for GET_RHS to include terms that are due to appear on the
! *** right hand side of the equation system (->boundary values!).
! ***
! *** The implemeted scheme has order of consistency 2 for uniform meshes
! *** and order of consistency 1 for non-uniform meshes.
! ----------------------------------------------------------------------------
subroutine get_diags(N,DL,D,DU,x,eps)

  !arguments
  integer(int_kind),                   intent(in)  :: N
  real(DBL_KIND)   , dimension(1:N)  , intent(out) :: D
  real(DBL_KIND)   , dimension(1:N-1), intent(out) :: DL,DU
  real(DBL_KIND)   , dimension(0:N+1), intent(in)  :: x
  real(DBL_KIND)   ,                   intent(in)  :: eps

  !DLcals
  integer(int_kind) :: i
  real(DBL_KIND)    :: h1,h2

  h2 = x(1) - x(0)
  do i=1,N
      h1 = h2             !left gap
      h2 = x(i+1) - x(i)  !right gap
      if (i>1) DL(i-1) = - 2.0_DBL_KIND*eps/(h1+h2)/h1
               D(i)    =   2.0_DBL_KIND*eps/ h1    /h2 + b(x(i))
      if (i<N) DU(i)   = - 2.0_DBL_KIND*eps/(h1+h2)/h2
  enddo

end subroutine get_diags
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE get_diags >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE get_rhs <<<
! ***
! *** Given U, this routines returns the vector
! ***
! *** f + lhs_terms
! ***
! *** for the right hand side of the considered equation system.
! ***
! *** Also includes terms coming from the boundary values on the left hand
! *** side of the equation system (appearing only in first and last
! *** component when using a tridiagonal equation system).
! ***
! ----------------------------------------------------------------------------
subroutine get_rhs(rhs,x,eps)

  !arguments
  real(DBL_KIND), dimension(1:), intent(out) :: rhs
  real(DBL_KIND), dimension(0:), intent(in)  :: x
  real(DBL_KIND),                intent(in)  :: eps

  !locals
  integer(INT_KIND) :: N,k

  N = size(rhs,1)

  do k=1,N
    rhs(k) = f(x(k),eps)
  enddo

  !respect left hand side terms
  rhs(1) = rhs(1) + 2.0_DBL_KIND*eps/(  x(2)-  x(0))/(  x(1)-x(0)) * gamma0
  rhs(N) = rhs(N) + 2.0_DBL_KIND*eps/(x(N+1)-x(N-1))/(x(N+1)-x(N)) * gamma1

end subroutine get_rhs
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE get_rhs >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE solve <<<
! ***
! *** Actually solve the discretised elliptic pde; can return the defect
! *** of the residual when solving the equation system.
! ***
! ----------------------------------------------------------------------------
subroutine solve(Nx,x,eps,u,res)

  !arguments
  integer(int_kind)               ,intent(in)            :: Nx !number of inner nodes
  real(dbl_kind)                  ,intent(in)            :: eps
  real(dbl_kind),dimension(0:Nx+1),intent(in)            :: x
  real(dbl_kind),dimension(0:Nx+1),intent(out)           :: u
  real(dbl_kind)                  ,intent(out),optional  :: res

  !locals
  real(dbl_kind),dimension(:)     ,allocatable :: rhs
  real(dbl_kind),dimension(1:Nx)               :: D
  real(dbl_kind),dimension(1:Nx-1)             :: DL,DU

  call get_diags(Nx,DL,D,DU,x(0:Nx+1),eps)

  !####################################################
  !solve and put solution to U
  !---------------------------------
  !store rhs in U
  call get_rhs(u(1:Nx),x(0:Nx+1),eps)
  !---------------------------------

  if (present(res)) then
      allocate(rhs(1:Nx))
      rhs = u(1:Nx)
  endif

  !---------------------------------
!   call tridiag_LAPACK(Nx,DL,D,DU,u(1:Nx))
  call tridiag_gauss(Nx,DL,D,DU,u(1:Nx))
  u(0)    = gamma0
  u(Nx+1) = gamma1
  !---------------------------------
  !####################################################

if (present(res)) then
  call get_diags(Nx,DL,D,DU,x(0:Nx+1),eps)
! write(*,*) 'Nx',Nx
! write(*,*) 'DL',DL
! write(*,*) 'D ',D
! write(*,*) 'DU',DU
! write(*,*) 'rhs',rhs
! write(*,*) 'u',u(1:Nx)
! write(*,*) 'check_diags',check_tridiag(Nx,DL,D,DU,rhs,u(1:Nx))
  res = defect_tridiag(Nx,DL,D,DU,rhs(1:Nx),u(1:Nx))
  deallocate(rhs)
endif

end subroutine solve
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE solve >>>
! ----------------------------------------------------------------------------

end module SOLVE_ELLIPTIC
! ============================================================================
! *** END of MODULE SOLVE_ELLIPTIC >>>
! ============================================================================