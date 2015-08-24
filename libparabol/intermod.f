! ============================================================================
! *** MODULE INTERMOD <<<
! ***
! *** Provides interfaces for user defined functions such as everything that
! *** is related to L_eps
! ***
! ============================================================================
module INTERMOD

  use kindmod

  implicit none

  ! --------------------------------------------------------------------------
  ! *** function get_eps <<<
  ! ***
  ! --------------------------------------------------------------------------
  interface get_eps
      pure function parameters_mp_get_eps()
          use kindmod
          real(RNP) :: parameters_mp_get_eps
      end function
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of SUBROUTINE get_eps >>>
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function set_eps <<<
  ! ***
  ! --------------------------------------------------------------------------
  interface set_eps
      subroutine parameters_mp_set_eps(x)
          use kindmod
          real(RNP) :: x
      end subroutine
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of SUBROUTINE set_eps >>>
  ! --------------------------------------------------------------------------

  ! --------------------------------------------------------------------------
  ! *** function get_rho <<<
  ! ***
  ! --------------------------------------------------------------------------
  interface get_rho
      pure function parameters_mp_get_rho()
          use kindmod
          real(RNP) :: parameters_mp_get_rho
      end function
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of SUBROUTINE get_rho >>>
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function get_T1 <<<
  ! ***
  ! --------------------------------------------------------------------------
  interface get_T1
      pure function parameters_mp_get_T1()
          use kindmod
          real(RNP) :: parameters_mp_get_T1
      end function
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of SUBROUTINE get_T1 >>>
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function u0 <<<
  ! ***
  ! *** Initial condition on U.
  ! ***
  ! --------------------------------------------------------------------------
  interface u0
      pure function parameters_mp_u0(x)
          use kindmod
          real(RNP)            :: parameters_mp_u0
          real(RNP),intent(in) :: x
      end function parameters_mp_u0
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of SUBROUTINE u0 >>>
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function gamma0 <<<
  ! ***
  ! *** Boundary condition on the left hand side of the domain.
  ! ***
  ! --------------------------------------------------------------------------
  interface gamma0
      pure function parameters_mp_gamma0(t)
          use kindmod
          real(RNP)            :: parameters_mp_gamma0
          real(RNP),intent(in) :: t
      end function
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of SUBROUTINE gamma0 >>>
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function gamma1 <<<
  ! ***
  ! *** Boundary condition on the right hand side of the domain.
  ! ***
  ! --------------------------------------------------------------------------
  interface gamma1
      pure function parameters_mp_gamma1(t)
          use kindmod
          real(RNP)            :: parameters_mp_gamma1
          real(RNP),intent(in) :: t
      end function
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of SUBROUTINE gamma1 >>>
  ! --------------------------------------------------------------------------

  ! --------------------------------------------------------------------------
  ! *** INTERFACE get_solution <<<
  ! ***
  ! *** Get the exact solution of the pde
  ! ***
  ! --------------------------------------------------------------------------
  interface get_solution
      function parameters_mp_get_solution_mesh(N,t) result(res)
          use kindmod
          integer(INP)               :: N
          real(RNP)                  :: t
          real(RNP),dimension(0:N+1) :: res
      end function

      function parameters_mp_get_solution_single(N,i,t) result(res)
          use kindmod
          integer(INP) :: N,i
          real(RNP)    :: t,res
      end function
  end interface
  ! --------------------------------------------------------------------------
  ! *** END of INTERFACE get_solution >>>
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function reaction_diffusion_multiply <<<
  ! ***
  ! *** Provides matrix multiplication with L_eps.
  ! ***
  ! --------------------------------------------------------------------------
  interface L_eps_multiply
      function operators_mp_L_eps_multiply(N,X) result(Z)
          use kindmod
          integer(INP)              :: N
          real(RNP) ,dimension(1:N) :: X
          real(RNP) ,dimension(1:N) :: Z
      end function
  end interface
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function M_alpha_solve <<<
  ! ***
  ! *** Provides the means for solving an equation system of the type
  ! ***
  ! ***    (I + alpha*L_eps) u = RHS,
  ! ***
  ! *** where the right hand side is given in U on entry.
  ! ***
  ! --------------------------------------------------------------------------
  interface M_alpha_solve
      subroutine operators_mp_M_alpha_solve(N,alpha,U)
          use kindmod
          integer(INP)              :: N
          real(RNP)                 :: alpha
          real(RNP) ,dimension(1:N) :: U
       end subroutine
  end interface
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** function get_f_tilde <<<
  ! ***
  ! *** Return a processed right hand side in the sense of the method.
  ! *** For a FDM this will simply return f(x(i)) at the i-th element, but
  ! *** other methods (such as FEM) may be more sophisticated.
  ! ***
  ! --------------------------------------------------------------------------
  interface get_f_tilde
      function operators_mp_get_f_tilde(N,t) result(z)
          use kindmod
          integer(INP)             :: N
          real(RNP)                :: t
          real(RNP),dimension(1:N) :: Z
      end function
  end interface
  ! --------------------------------------------------------------------------


  ! --------------------------------------------------------------------------
  ! *** subroutine rhs_adjustment <<<
  ! ***
  ! *** Returns the vector that adjusts the right hand side of the equation
  ! *** system
  ! ***
  ! ***    alpha*L_eps u  = f
  ! ***
  ! *** in the case of non-homogeneous boundary conditions.
  ! ***
  ! --------------------------------------------------------------------------
  interface rhs_adjustment
      subroutine operators_mp_rhs_adjustment(N,t,alpha,F)
          use kindmod
          integer(INP)              :: N
          real(RNP)                 :: t,alpha
          real(RNP) ,dimension(1:N) :: F
      end subroutine
  end interface
  ! --------------------------------------------------------------------------

end module INTERMOD
! ============================================================================
! *** END of MODULE INTERMOD >>>
! ============================================================================