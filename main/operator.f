! ============================================================================
! *** MODULE OPERATORS <<<
! ***
! *** Provides discretizations of the operators used, such as
! ***
! *** where   L_eps = -eps \Delta + c(x)
! ***
! ============================================================================
module OPERATORS

use KINDMOD
use ERRORMOD
use parameters
use tridiagmod
USE MESHES

implicit none

private

! ----------------------------------------------------------------------------
! *** PUBLIC MODULE ENTIITES <<<
! ----------------------------------------------------------------------------
public :: L_eps_multiply, M_alpha_solve, rhs_adjustment, get_f_tilde
! ----------------------------------------------------------------------------
! *** End of PUBLIC MODULE ENTIITES >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  type(tridiag) :: L_eps
  logical       :: init_L_eps      = .FALSE.
  real(RNP)     :: eps_L_eps       = 0.0_RNP
  type(mesh)    :: msh_L_eps       = mesh('--',0_INP)

  type(tridiag) :: M_alpha
  logical       :: M_alpha_needs_reallocation=.TRUE.
  real(RNP)     :: alpha_M_alpha = 0.0_RNP
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

contains

! ----------------------------------------------------------------------------
! *** FUNCTION L_eps_needs_update <<<
! ***
! ----------------------------------------------------------------------------
logical function L_eps_needs_update()

  L_eps_needs_update = .NOT.init_L_eps                 .OR. &
                       .NOT.mesh_up_to_date(msh_L_eps) .OR. &
                       get_eps().NE.eps_L_eps

  return
end function L_eps_needs_update
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE L_eps_needs_update >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION L_eps_needs_reallocation <<<
! ***
! ----------------------------------------------------------------------------
logical function L_eps_needs_reallocation()

  L_eps_needs_reallocation = .NOT.init_L_eps               .OR. &
                             msh_L_eps%N.NE.current_mesh%N

  M_alpha_needs_reallocation = .TRUE.

  return
end function L_eps_needs_reallocation
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE L_eps_needs_reallocation >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE L_eps_init <<<
! ***
! *** Determine the space discretisation method that will be used.
! ***
! ----------------------------------------------------------------------------
subroutine L_eps_init()

  !locals
  integer(INP) :: i
  real(RNP)    :: h1,h2


  if(L_eps_needs_reallocation()) then
      call clean(L_eps)
      call init(current_mesh%N,L_eps)
  endif

  h2 = get_meshpoint(1) - get_meshpoint(0)
  do i=1,current_mesh%N
      h1 = h2                                     !left gap
      h2 = get_meshpoint(i+1) - get_meshpoint(i)  !right gap
      if (i>1)              L_eps%lower(i-1) = -2.0_RNP*get_eps()/(h1+h2)/h1
                            L_eps%diag(i)    =  2.0_RNP*get_eps()/h1     /h2 +  c(get_meshpoint(i))
      if (i<current_mesh%N) L_eps%upper(i)   = -2.0_RNP*get_eps()/(h1+h2)/h2
  enddo

  ! Set module variables
  init_L_eps = .TRUE.
  msh_L_eps  = current_mesh
  eps_L_eps  = get_eps()

  return
end subroutine L_eps_init
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE L_eps_init >>>
! ----------------------------------------------------------------------------



! ----------------------------------------------------------------------------
! *** function get_f_tilde <<<
! ***
! *** Return a processed right hand side in the sense of the method.
! *** For a FDM this will simply return f(x(i)) at the i-th element, but
! *** other methods (such as FEM) may be more sophisticated.
! ***
! ----------------------------------------------------------------------------
function get_f_tilde(N,t) result(Z)

  !arguments
  integer(INP),intent(in) :: N
  real(RNP)   ,intent(in) :: t

  !result
  real(RNP),dimension(1:N) :: Z

  Z = get_f(t)

  return
end function get_f_tilde
! ----------------------------------------------------------------------------
! *** END of function get_f_tilde >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** function reaction_diffusion_multiply <<<
! ***
! *** Provides matrix multiplication with L_eps as defined in above subroutine
! *** reaction_diffusion_init.
! ***
! ----------------------------------------------------------------------------
function L_eps_multiply(N,X) result(Z)

  !arguments
  integer(INP)             ,intent(in) :: N
  real(RNP) ,dimension(1:N),intent(in) :: X
  real(RNP) ,dimension(1:N)            :: Z

  if(L_eps_needs_update()) call L_eps_init()

  Z = tridiag_matmul(L_eps,X)

end function L_eps_multiply
! ----------------------------------------------------------------------------
! *** END of function L_eps_multiply >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine rhs_adjustment <<<
! ***
! *** Returns the vector that adjusts the right hand side of the equation
! *** system
! ***
! ***    alpha*L_eps u  = f
! ***
! *** in the case of non-homogeneous boundary conditions.
! ***
! ----------------------------------------------------------------------------
subroutine rhs_adjustment(N,t,alpha,F)

  !arguments
  integer(INP)                            :: N
  real(RNP)                ,intent(in)    :: t,alpha
  real(RNP) ,dimension(1:N),intent(inout) :: F

  !locals
  real(RNP) :: h1,h2

  h1 = get_meshpoint(1) - get_meshpoint(0)
  h2 = get_meshpoint(2) - get_meshpoint(1)
  F(1) = F(1) + alpha* 2.0_RNP*get_eps()/(h1+h2)/h1 *gamma0(t)

  h1 = get_meshpoint(N)   - get_meshpoint(N-1)
  h2 = get_meshpoint(N+1) - get_meshpoint(N)
  F(N) = F(N) + alpha* 2.0_RNP*get_eps()/(h1+h2)/h2 *gamma1(t)

end subroutine rhs_adjustment
! ----------------------------------------------------------------------------
! *** END of subroutine rhs_adjustment >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine M_alpha_init <<<
! ***
! *** M_alpha =  I + alpha* L_eps
! ***
! ----------------------------------------------------------------------------
subroutine M_alpha_init(alpha)

  !arguments
  real(RNP),intent(in) :: alpha

  !locals
  integer(INP) :: N

  N = current_mesh%N

  if (M_alpha_needs_reallocation) then
      call clean(M_alpha)
      call init(N,M_alpha)
      M_alpha_needs_reallocation = .FALSE.
  endif

  call tridiag_assign(L_eps,1.0_RNP,alpha,M_alpha)

  !set module variables
  alpha_M_alpha              = alpha
  M_alpha_needs_reallocation = .FALSE.

end subroutine M_alpha_init
! ----------------------------------------------------------------------------
! *** END of subroutine M_alpha_init >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** function M_alpha_solve <<<
! ***
! *** Provides the means for solving an equation system of the type
! ***
! ***    (I + alpha*L_eps) u = RHS,
! ***
! *** where the right hand side is given in U on entry.
! ***
! ----------------------------------------------------------------------------
subroutine M_alpha_solve(N,alpha,U)

  !arguments
  integer(INP)             ,intent(in)    :: N
  real(RNP)                ,intent(in)    :: alpha
  real(RNP) ,dimension(1:N),intent(inout) :: U

  ! update L_eps if necessary
  if(L_eps_needs_update()) call L_eps_init()

  ! update M_alpha if necessary
  if(alpha.ne.alpha_M_alpha .OR. M_alpha_needs_reallocation) call M_alpha_init(alpha)

  call tridiag_gauss(N,M_alpha,U)

end subroutine M_alpha_solve
! ----------------------------------------------------------------------------
! *** END of function M_alpha_solve >>>
! ----------------------------------------------------------------------------

end module OPERATORS
! ============================================================================
! *** END of MODULE OPERATORS >>>
! ============================================================================                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      fined in above subroutine
! *** reaction_diffusion_init.
! ***
! ----------------------------------------------------------------------------
function L_eps_multiply(X) result(Z)

  !arguments
  real(RNP) ,dimension(1:current_mesh%N),intent(in) :: X
  real(RNP) ,dimension(1:current_mesh%N)            :: Z

! write(*,*) ' >>> L_eps_multiply'

  if(L_eps_needs_update()) call L_eps_init()

  Z = bandmatrix_matrix_vector_product(L_eps,X)

! write(*,*) '     L_eps_multiply >>>'

end function L_eps_multiply
! ----------------------------------------------------------------------------
! *** END of function L_eps_multiply >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** subroutine M_alpha_init <<<
! ***
! *** M_alpha =  I + alpha* L_eps
! ***
! ----------------------------------------------------------------------------
subroutine M_alpha_init(alpha)

  !arguments
  real(RNP),intent(in) :: alpha

  !locals
  integer(INP) :: N

  N = current_mesh%N

  if (M_alpha_needs_reallocation) then
      call clean(M_alpha)
      call init(N,kl_L_eps,kl_L_eps+ku_L_eps,M_alpha) !allocate a bit bigger
                                                      !to save save space
                                                      !for the factorization
      if(allocated(IPIV)) deallocate(IPIV)
      allocate(IPIV(1:N))

      M_alpha_needs_reallocation = .FALSE.
  endif

  call bandmatrix_assign(L_eps,1.0_RNP,alpha,M_alpha)

  !set module variables
  alpha_M_alpha = alpha
  init_M_alpha  = .TRUE.

end subroutine M_alpha_init
! ----------------------------------------------------------------------------
! *** END of subroutine M_alpha_init >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** function M_alpha_solve <<<
! ***
! *** Provides the means for solving an equation system of the type
! ***
! ***    (I + alpha*L_eps) u = RHS,
! ***
! *** where the right hand side is given in U on entry.
! ***
! ----------------------------------------------------------------------------
subroutine M_alpha_solve(alpha,U)

  !arguments
  real(RNP)                             ,intent(in)    :: alpha
  real(RNP) ,dimension(1:current_mesh%N),intent(inout) :: U

! write(*,*) ' >>> M_alpha_solve'

  ! update L_eps if necessary
  if(L_eps_needs_update()) call L_eps_init()

  ! update M_alpha if necessary
  if(.NOT.init_M_alpha .OR. alpha.ne.alpha_M_alpha) then
      ! Prepare the equation system and solve with LU.
      call M_alpha_init(alpha)
      call bandmatrix_solver(current_mesh%N,kl_L_eps,ku_L_eps,IPIV,M_alpha,U)
  else
      ! Just solve LU.
      call bandmatrix_LU_solver(current_mesh%N,kl_L_eps,ku_L_eps,IPIV,M_alpha,U)
  endif

! write(*,*) '     M_alpha_solve >>>'

end subroutine M_alpha_solve
! ----------------------------------------------------------------------------
! *** END of function M_alpha_solve >>>
! ----------------------------------------------------------------------------


end module OPERATORS
! ============================================================================
! *** END of MODULE OPERATORS >>>
! ============================================================================                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              