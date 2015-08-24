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
use params_generic
use params_mesh
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
                       eps.NE.eps_L_eps

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
  real(RNP)    :: h1,h2,gammam,gammap


  if(L_eps_needs_reallocation()) then
      call clean(L_eps)
      call init(current_mesh%N,L_eps)
  endif

  h2 = get_meshpoint(1) - get_meshpoint(0)
  do i=1,current_mesh%N
      h1 = h2                                     !left gap
      h2 = get_meshpoint(i+1) - get_meshpoint(i)  !right gap

      gammam = h2*h2/(h1+h2)/h1
      gammap = h1*h1/(h1+h2)/h2

      if (i>1)              L_eps%lower(i-1) = -2.0_RNP*eps/(h1+h2)/h1 + (1.0_RNP-gammam)       /6.0_RNP* c(get_meshpoint(i-1))
                            L_eps%diag(i)    =  2.0_RNP*eps/h1     /h2 + (4.0_RNP+gammam+gammap)/6.0_RNP* c(get_meshpoint(i))
      if (i<current_mesh%N) L_eps%upper(i)   = -2.0_RNP*eps/(h1+h2)/h2 + (1.0_RNP-gammap)       /6.0_RNP* c(get_meshpoint(i+1))
  enddo

  ! Set module variables
  init_L_eps = .TRUE.
  msh_L_eps  = current_mesh
  eps_L_eps  = eps

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
! *** other methods (such as FEM) may be more sophisticated).
! ***
! ----------------------------------------------------------------------------
function get_f_tilde(t) result(Z)

  !arguments
  real(RNP),intent(in) :: t

  !result
  real(RNP),dimension(1:current_mesh%N) :: Z

  !locals
  integer(INP) :: i
  real(RNP)    :: h1,h2,gammam,gammap

  h2 =  get_meshpoint(1)-get_meshpoint(0)

  do i=1,current_mesh%N

      h1 = h2                                   !left gap
      h2 = get_meshpoint(i+1)-get_meshpoint(i)  !right gap

      gammam = h2*h2/(h1+h2)/h1
      gammap = h1*h1/(h1+h2)/h2

      Z(i) = (1.0_RNP-gammam)       /6.0_RNP* get_f(i-1,t) &
           + (4.0_RNP+gammam+gammap)/6.0_RNP* get_f(i,t)   &
           + (1.0_RNP-gammap )      /6.0_RNP* get_f(i+1,t)

   enddo

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
function L_eps_multiply(X) result(Z)

  !arguments
  real(RNP) ,dimension(1:current_mesh%N),intent(in) :: X
  real(RNP) ,dimension(1:current_mesh%N)            :: Z

! write(*,*) ' >>> L_eps_multiply'

  if(L_eps_needs_update()) call L_eps_init()

  Z = tridiag_matmul(L_eps,X)

! write(*,*) '     L_eps_multiply >>>'

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
subroutine rhs_adjustment(t,alpha,F)

  !arguments
  real(RNP)                             ,intent(in)    :: t,alpha
  real(RNP) ,dimension(1:current_mesh%N),intent(inout) :: F

  !locals
  real(RNP) :: h1,h2,gammam,gammap

  h1 = get_meshpoint(1) - get_meshpoint(0)
  h2 = get_meshpoint(2) - get_meshpoint(1)
  gammam = h2*h2/(h1+h2)/h1
  gammap = h1*h1/(h1+h2)/h2
  F(1) = F(1) - alpha*gamma0(t)*(-2.0_RNP*eps/(h1+h2)/h1 &
                                + (1.0_RNP-gammam)/6.0_RNP* c(get_meshpoint(0))  )

  h1 = get_meshpoint(current_mesh%N)   - get_meshpoint(current_mesh%N-1)
  h2 = get_meshpoint(current_mesh%N+1) - get_meshpoint(current_mesh%N)
  gammam = h2*h2/(h1+h2)/h1
  gammap = h1*h1/(h1+h2)/h2
  F(current_mesh%N) = F(current_mesh%N)                                      &
                    - alpha*gamma1(t)*( -2.0_RNP*eps/(h1+h2)/h2              &
                                      + (1.0_RNP-gammap )/6.0_RNP* c(get_meshpoint(current_mesh%N+1)) )

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
subroutine M_alpha_solve(alpha,U)

  !arguments
  real(RNP)                             ,intent(in)    :: alpha
  real(RNP) ,dimension(1:current_mesh%N),intent(inout) :: U

! write(*,*) ' >>> M_alpha_solve'

  ! update L_eps if necessary
  if(L_eps_needs_update()) call L_eps_init()

  ! update M_alpha if necessary
  if(alpha.ne.alpha_M_alpha .OR. M_alpha_needs_reallocation) call M_alpha_init(alpha)

  call tridiag_gauss(current_mesh%N,M_alpha,U)

! write(*,*) '     M_alpha_solve >>>'

end subroutine M_alpha_solve
! ----------------------------------------------------------------------------
! *** END of function M_alpha_solve >>>
! ----------------------------------------------------------------------------


end module OPERATORS
! ============================================================================
! *** END of MODULE OPERATORS >>>
! ============================================================================