! ============================================================================
! *** PROGRAM MAIN_ELLPTIC <<<
! ***
! ============================================================================
program main_elliptic

  use KINDMOD
  use MESHES
  use PUT_ELLIPTIC

  implicit none

  real(dbl_kind) ,dimension(:), allocatable :: x,u

  real(dbl_kind)    :: eps

!   real(dbl_kind)    :: h,eps0,tau0
  real(dbl_kind)    :: kappa,sigma,q
  integer(int_kind) :: i,Nx,eps_steps,N_steps
  integer(int_kind),dimension(:),allocatable :: N_table
  real(dbl_kind)   ,dimension(:),allocatable :: eps_table


  !#################################
  ! set problem parameters
  Nx = 2**12

  eps = 1.0E-12

  allocate( x(0:Nx+1) )
  !#################################

  !---------------------------------------------------------------------------
  ! choose mesh
  !---------------------------------------------------------------------------
!   call uniform(x)

!   q     = 0.25_DBL_KIND
!   sigma = 2.0_DBL_KIND
!   call shishkin(x,q,sqrt(eps),sigma,sqrt(rho))

!   kappa = 1.0_DBL_KIND
!   sigma = 2.0_DBL_KIND
!   rho   = 1.0_DBL_KIND
!   call bakhvalov(x,kappa,sqrt(eps),sigma,sqrt(rho))

!   x = x*(x1-x0)+x0
  !---------------------------------------------------------------------------

!   !---------------------------------------------------------------------------
!   ! solve numerically
!   !---------------------------------------------------------------------------
!   allocate( u(0:Nx+1) )
!   call solve(Nx,x,eps,u,sigma)
!   !---------------------------------------------------------------------------

!   !---------------------------------------------------------------------------
!   ! solve numerically and put
!   !---------------------------------------------------------------------------
!   call put_approximation(Nx,x,eps)
!   !---------------------------------------------------------------------------


!   !---------------------------------------------------------------------------
!   ! put exact solution to standard out
!   !---------------------------------------------------------------------------
!   call put_exact_solution(Nx,x,eps)
!   !---------------------------------------------------------------------------


   !--------------------------------------------------------------------------
   ! LaTeX convergence table
   N_steps = 5
   allocate(  N_table(1:N_steps)); N_table(1)   = 4096
   do i=2,N_steps
      N_table(i)   = N_table(i-1)   *2
   enddo

   eps_steps = 15
   allocate(eps_table(1:eps_steps)); eps_table(1) = 1.0E-0_DBL_KIND
   do i=2,eps_steps
      eps_table(i) = eps_table(i-1) * 1.0E-2_DBL_KIND
   enddo

   call put_maxerr_exact_table(N_table,N_steps,eps_table,eps_steps)
   !--------------------------------------------------------------------------

end program main_elliptic
! ============================================================================
! *** END of PROGRAM MAIN_ELLPTIC >>>
! ============================================================================