! ============================================================================
! *** MODULE RUNGE_KUTTA <<<
! ***
! *** Provides necessary routines for solving the parabolic de
! ***
! *** u_t + L_eps u = f        on (0,1)x(0,1],
! *** u(0,t) = u(1,t) = 0      for t \in (0,1],
! *** u(x,0) = 0               for x \in [0,1],
! ***
! *** where   L_eps = -eps u_xx + b(x) u
! *** and         f = f(t).
! ***
! *** The Crank-Nicolson method in direction T and a very basic differential
! *** scheme are used for solving the spatial problems.
! ***
! ============================================================================
module RUNGE_KUTTA

   use KINDMOD
   use MESHES
   use errormod
   use RK_PARAMETERS
   use intermod

   implicit none

   private

! ----------------------------------------------------------------------------
! *** PUBLIC MODULE ENTITIES <<<
! ----------------------------------------------------------------------------
   public set_runge_kutta, runge_kutta_step
! ----------------------------------------------------------------------------
! *** END of PUBLIC MODULE ENTITIES >>>
! ----------------------------------------------------------------------------

!   real(RNP),dimension(1:10) :: roundoff = 0.0_RNP

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE runge_kutta_step <<<
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
   subroutine runge_kutta_step(u, roundoff, tau, t)

      !arguments
      real(RNP), dimension(1:current_mesh%N), intent(inout) :: u, roundoff
      real(RNP), intent(in)    :: tau, t

      !locals
      integer(INP)                          :: i, N
      real(RNP), dimension(current_mesh%N, s) :: Y, rhs
      real(RNP), dimension(1:current_mesh%N) :: temp1, temp2

      N = current_mesh%N

      tau_save = tau ! make tau available to lower level routines

      !------------------------------------------------------
      ! step 1: get right hand side of the equation system
      !         for the stage values
      rhs = stage_rhs(N, u, t, tau)
      !------------------------------------------------------

      !------------------------------------------------------
      ! step 2: solve equation system for stage values Y
      select case (method_type)
      case ('EX')
         call explicit_solver(tau, RHS, Y)

      case ('DI')
         call diagonally_implicit_solver(tau, RHS, Y)

      case ('FI')
         call fully_implicit_solver(u, RHS, Y)

      case default
         call STOP_ON_ERROR('solve_stage_system (in RUNGE-KUTTA)', &
                            'Unknown method type.')
      end select
      !------------------------------------------------------

      !------------------------------------------------------
      ! step 3: update u_n with rounding error treatment
      !         according to M/oller
      temp1 = roundoff
      do i = 1, s
         temp1 = temp1 + tau*b(i)*(-L_eps_multiply(N, Y(:, i)) + get_f_tilde(N, t + c(i)*tau))
         call rhs_adjustment(N, t + c(i)*tau, tau*b(i), temp1)
      end do
      temp2 = u + temp1
      roundoff = temp1 - (temp2 - u)
      u = temp2
      !------------------------------------------------------

   end subroutine runge_kutta_step
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE runge_kutta_step >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE solve_stage_system <<<
! ***
! *** Given a N*s-dimensional right hand side RHS, this routine returns the
! *** solution of the equation system for the stage values Y.
! ***
! *** The actual method obviously depends on whether we're dealing with an
! *** explicit, a diagonally implicit, or a fully implicit method.
! ***
! ----------------------------------------------------------------------------
   subroutine explicit_solver(tau, RHS, Y)

!   use OPERATORS

      !arguments
      real(RNP), intent(in)  :: tau
      real(RNP), dimension(current_mesh%N, s), intent(in)  :: RHS
      real(RNP), dimension(current_mesh%N, s), intent(out) :: Y

      !locals
      integer(INP)                             :: i, j, N
      real(RNP), dimension(1:current_mesh%N) :: LYj

      N = current_mesh%N

      Y = RHS

      do j = 1, s - 1
         LYj = L_eps_multiply(N, Y(:, j))
         do i = j + 1, s
            Y(:, i) = Y(:, i) - tau*A(i, j)*LYj
         end do
      end do

   end subroutine explicit_solver
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE explicit_solver >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE diagonally_implicit_solver <<<
! ***
! *** Given a N*s-dimensional right hand side RHS, this routine returns the
! *** solution of the equation system for the stage values Y.
! ***
! *** The actual method obviously depends on whether we're dealing with an
! *** explicit, a diagonally implicit, or a fully implicit method.
! ***
! ----------------------------------------------------------------------------
   subroutine diagonally_implicit_solver(tau, RHS, Y)

      !arguments
      real(RNP), intent(in)  :: tau
      real(RNP), dimension(current_mesh%N, s), intent(in)  :: RHS
      real(RNP), dimension(current_mesh%N, s), intent(out) :: Y

      !locals
      integer(INP)                          :: i, j, N
      real(RNP), dimension(1:current_mesh%N) :: LYj

      N = current_mesh%N

      Y = RHS

      do j = 1, s
         if (A(j, j) .ne. 0.0_RNP) call M_alpha_solve(current_mesh%N, tau*A(j, j), Y(:, j))

         LYj = L_eps_multiply(N, Y(:, j))
         do i = j + 1, s
            Y(:, i) = Y(:, i) - tau*A(i, j)*LYj
         end do
      end do

! write(*,*) 'maxval',maxval(abs(stage_matrix_multiplication(N,s,Y)-RHS))

   end subroutine diagonally_implicit_solver
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE diagonally_implicit_solver >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE fully_implicit_solver <<<
! ***
! *** Given a N*s-dimensional right hand side RHS, this routine returns the
! *** solution of the equation system for the stage values Y.
! ***
! *** The actual method obviously depends on whether we're dealing with an
! *** explicit, a diagonally implicit, or a fully implicit method.
! ***
! ----------------------------------------------------------------------------
   subroutine fully_implicit_solver(u, RHS, Y)

      use preconditioners
      use SOLVERS

      !arguments
      real(RNP), dimension(1:current_mesh%N), intent(in)  :: u
      real(RNP), dimension(current_mesh%N, s), intent(in)  :: RHS
      real(RNP), dimension(current_mesh%N, s), intent(out) :: Y

      !locals
      integer(INP) :: i, N

      N = current_mesh%N

      ! Starting value for the following GMRES iteration. A not so smart
      ! choice of Y=0.0 doesn't seem to make a big difference, though.
      forall (i=1:s) Y(:, i) = u + c(i)*rhs(:, i)

      call set_preconditioner('LS')
      call GMRES(N, s, RHS, Y, stage_matrix_multiplication)

   end subroutine fully_implicit_solver
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE fully_implicit_solver >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION stage_matrix_multiplication <<<
! ***
! *** Provides multiplication with the stage matrix as appearing in
! ***
! ***   ( I\otimes I + tau * ( A \otimes L ) ) Y = -I \otimes L + F.
! ***
! *** It is used to solve the latter equation system with an iteration scheme
! *** such as GMRES.
! ***
! *** Motivation is that for each Runge-Kutta step, one of the systems
! ***
! ***    Y = L ( y_{n-1} + tau * A* Y) + F
! ***
! *** needs to be solved.
! ***
! ----------------------------------------------------------------------------
   function stage_matrix_multiplication(N, s, Y) result(Z)

      !arguments
      integer(INP), intent(in) :: N, s
      real(RNP), dimension(N, s), intent(in) :: Y   ! stage vector

      !result
      real(RNP), dimension(N, s) :: Z

      !locals
      integer(INP)           :: i, j
      real(RNP), dimension(N) :: L

      Z = Y

      do j = 1, s
         L = L_eps_multiply(N, Y(:, j))
         forall (i=1:s) Z(:, i) = Z(:, i) + tau_save*A(i, j)*L
      end do

   end function stage_matrix_multiplication
! ----------------------------------------------------------------------------
! *** END of FUNCTION stage_matrix_multiplication >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION stage_rhs <<<
! ***
! *** Provides the right hand side of the equation system
! ***
! ***   ( I\otimes I + tau * ( A \otimes L ) ) Y = -I \otimes L + F.
! ***
! *** It is used to solve the latter equation system with an iteration scheme
! *** such as GMRES.
! ***
! *** Motivation is that for each Runge-Kutta step, one of the systems
! ***
! ***    Y = L ( y_{n-1} + tau * A* Y) + F
! ***
! *** needs to be solved.
! ***
! ----------------------------------------------------------------------------
   function stage_rhs(N, u, t, tau) result(F)

      use rk_parameters

      !arguments
      integer(INP), intent(in) :: N
      real(RNP), dimension(1:N), intent(in) :: u   ! = y_{n-1}
      real(RNP), intent(in) :: t, tau

      !result
      real(RNP), dimension(N, s) :: F

      !locals
      integer(INP) :: i, j

      do i = 1, s
         F(:, i) = u

         do j = 1, s
            F(:, i) = F(:, i) + tau*A(i, j)*get_f_tilde(N, t + c(j)*tau)
            call rhs_adjustment(N, t + c(j)*tau, tau*A(i, j), F(:, i))
         end do

      end do

   end function stage_rhs
! ----------------------------------------------------------------------------
! *** END of FUNCTION stage_rhs >>>
! ----------------------------------------------------------------------------

end module RUNGE_KUTTA
! ============================================================================
! *** END of MODULE RUNGE_KUTTA >>>
! ============================================================================
