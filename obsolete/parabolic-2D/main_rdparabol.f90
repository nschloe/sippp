! ============================================================================
! *** PROGRAM MAIN <<<
! ***
! *** Two-dimensional setup.
! ============================================================================
program main

  use KINDMOD
  use MESHES
!   use SOLVE_RDPARABOL
  use PUT_PARABOL

  implicit none

  real(RNP) ,dimension(:), allocatable :: x

  real(RNP)    :: tau
  type(mesh)   :: msh

!   real(RNP)    :: h,eps0,tau0
!   real(RNP)    :: kappa,sigma,q
  integer(INP) :: Nx,Nt!,eps_steps,N_steps,N0
!   integer(INP) :: N_mult,i
!   real(RNP)    :: tau_mult,eps_mult
!   integer(INP),dimension(:),allocatable :: N_table
!   real(RNP)   ,dimension(:),allocatable :: tau_table,eps_table


  !#################################
  ! set problem parameters
  Nx = 99
!   Nt = 10 *2**0
  tau = 1.0E-2_RNP

  eps = 1.0E-4_RNP

  Nt  = int(T1/tau,INP)
  allocate( x(0:Nx+1) )
  !#################################

  !---------------------------------------------------------------------------
  ! get mesh
  !---------------------------------------------------------------------------
  msh = mesh('UN',Nx)
  x = get_meshpoint(msh)

!   call shishkin(Nx,x,q=0.25_RNP,sqrteps=sqrt(eps),sigma=2.0_RNP,sqrtrho=sqrt(rho))

!   kappa = 1.0_RNP
!   sigma = 2.0_RNP
!   call bakhvalov(x,kappa,sqrt(eps),sigma,sqrt(rho))

  x = x*(x1-x0)+x0
  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! solve step by step and put out
  !---------------------------------------------------------------------------
  call put_approximation(Nx,x(0:Nx+1),Nt,tau,eps)
  !---------------------------------------------------------------------------


!   !---------------------------------------------------------------------------
!   ! plot exact solution
!   !---------------------------------------------------------------------------
!    call put_exact_solution(Nx,x,Nt,tau,eps)
!   !---------------------------------------------------------------------------

!   !---------------------------------------------------------------------------
!   ! plot exact solution for PicTeX
!   !---------------------------------------------------------------------------
!    call put_exact_solution_pictex(Nx,x,3.0_RNP/3.0_RNP,eps)
!   !---------------------------------------------------------------------------


!      write(*,*) maxerr_exact(Nx,x,Nt,tau,eps)

!      call uniform(x2,x0,x1)
!      write(*,*) maxerr_approx(x,x2,Nt,tau,eps)

! deallocate(x)
! allocate(x(0:2048*1))
! call uniform(x)
! write(*,*) maxerr_exact(x,1,1.0_RNP,1.0E-8_RNP)
! stop

!    !--------------------------------------------------------------------------
!    ! LaTeX convergence table
!    N_steps = 6
!    allocate(  N_table(1:N_steps)); N_table(1)   = 2**7
!    allocate(tau_table(1:N_steps)); tau_table(1) = 0.5_RNP
!    do i=2,N_steps
!       N_table(i)   = N_table(i-1)   *2
!       tau_table(i) = tau_table(i-1) *0.5_RNP
!    enddo
! 
!    eps_steps = 10
!    allocate(eps_table(1:eps_steps)); eps_table(1) = 1.0_RNP
!    do i=2,eps_steps
!       eps_table(i) = eps_table(i-1) * 1.0E-2_RNP
!    enddo
! 
!    call put_maxerr_exact_table(N_steps,N_table,tau_table,eps_steps,eps_table)
!    !--------------------------------------------------------------------------

end program main
! ============================================================================
! *** END of PROGRAM MAIN >>>
! ============================================================================