! ============================================================================
! *** MODULE SOLVE_RDPARABOL <<<
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
! *** ====================
! *** TWO-DIMENSIONAL CASE
! *** ====================
! ***
! ============================================================================
module SOLVE_RDPARABOL

use KINDMOD
use params_generic
use SPARSEMOD

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
! *** The case of non-homogenous boundary condition might need more testing.
! ***
! ----------------------------------------------------------------------------
subroutine get_rhs(N,rhs,x,u,eps,tau,t)

  !arguments
  integer(INP)                  ,intent(in)  :: N  !strictly inner nodes (1D)
  real(RNP)   ,dimension(1:N**2),intent(out) :: rhs
  real(RNP)   ,dimension(0:N+1) ,intent(in)  :: x
  real(RNP)   ,dimension(1:N**2),intent(in)  :: u
  real(RNP)                     ,intent(in)  :: tau,eps,t

  !locals
  integer(INP) :: i,j,k
  real   (RNP) :: Delta_u,uim1,uip1,hi1,hi2, &
                          ujm1,ujp1,hj1,hj2, &
                  lhs_terms,xx(1:2)

  k=1

  hi2 = x(1) - x(0)
  do i=1,N

      hi1 = hi2            !left gap
      hi2 = x(i+1) - x(i)  !right gap

      hj2 = x(1) - x(0)
      do j=1,N

          hj1 = hj2            !left gap
          hj2 = x(j+1) - x(j)  !right gap

          lhs_terms = 0.0_RNP

          !-----------------
          ! get \Delta u
          if (i.eq.1) then
              xx = (/x(i-1),x(j)/)
              uim1 = gamma(xx,t-tau)
              lhs_terms = lhs_terms &
                        + tau*eps/(  x(2)-  x(0))/(  x(1)-x(0)) * gamma(xx,t)
          else
              uim1 = u(k-N)
          endif

          if (i.eq.N) then
              xx = (/x(i+1),x(j)/)
              uip1 = gamma(xx,t-tau)
              lhs_terms = lhs_terms &
                        + tau*eps/(x(N+1)-x(N-1))/(x(N+1)-x(N)) * gamma(xx,t)
          else
              uip1 = u(k+N)
          endif

          if (j.eq.1) then
              xx = (/x(i),x(j-1)/)
              ujm1 = gamma(xx,t-tau)
              lhs_terms = lhs_terms &
                        + tau*eps/(  x(2)-  x(0))/(  x(1)-x(0)) * gamma(xx,t)
          else
              ujm1 = u(k-1)
          endif

          if (j.eq.N) then
              xx = (/x(i),x(j+1)/)
              ujp1 = gamma(xx,t-tau)
              lhs_terms = lhs_terms &
                        + tau*eps/(x(N+1)-x(N-1))/(x(N+1)-x(N)) * gamma(xx,t)
          else
              ujp1 = u(k+1)
          endif

          Delta_u = 2.0_RNP*( (uim1/hi1+uip1/hi2)/(hi1+hi2) - u(k)/hi1/hi2 ) &
                  + 2.0_RNP*( (ujm1/hj1+ujp1/hj2)/(hj1+hj2) - u(k)/hj1/hj2 )
          !-----------------

          xx = (/x(i),x(j)/)
          rhs(k) = u(k) - tau/2.0_RNP * ( -eps*Delta_u  + c*u(k) )           &
                        + tau/2.0_RNP * ( f(xx,t,eps) + f(xx,t-tau,eps) )    &
                        + lhs_terms
!                         +        tau * f(x(k),t-tau/2.0_RNP,eps)
          k = k+1

      enddo
  enddo

end subroutine get_rhs
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE get_rhs >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE BUILD_STANDARD_2D <<<
! ***
! *** Builds up A%ROWSTART and A%COLUMN for a sparse type matrix for a finite
! *** difference methods that uses the standard 5-point-star for discretisa-
! *** tion of the Operator
! ***
! *** I + tau/2 L_{eps}
! ***
! *** where
! ***
! *** L_{eps} = -eps \Delta u + c u
! ***
! *** The argument x defines the grid on which the whole thing happens.
! ***
! ----------------------------------------------------------------------------
subroutine BUILD_STANDARD_2D(N,A,x,tau,eps,c)

  !arguments
  integer(INP),              intent(in)  :: N !size of x (stricly int. nodes)
  type(SPARSE),              intent(out) :: A
  real(RNP),dimension(0:N+1),intent(in)  :: x
  real(RNP),                 intent(in)  :: tau, eps, c

  !local variables
  integer(INP) :: NN, & !size of the equation system
                & NNZ   !number of offdiagonal non-zero elements (upper right)
  integer(INP) :: i,j,k,l
  real(RNP)    :: hi1,hi2, hj1,hj2
  logical      :: rowstarter

  NN  = N**2
  NNZ = 2*N*(N-1)

  A%N = NN

  allocate(A%DIAG(1:NN))
  allocate(A%OFFDIAG(1:NNZ))
  allocate(A%COLUMN(1:NNZ))
  allocate(A%ROWSTART(1:NN))

  k = 1 !the current row
  l = 1 !the offdiagonal element number
  hi2 = x(1) - x(0)
  do i=1,N

      hi1 = hi2            !left gap
      hi2 = x(i+1) - x(i)  !right gap

      hj2 = x(1) - x(0)
      do j=1,N
          rowstarter = .TRUE.

          hj1 = hj2            !left gap
          hj2 = x(j+1) - x(j)  !right gap

!           if (i>1) A(k,k-N) = - tau*eps/(hi1+hi2)/hi1
!           if (j>1) A(k,k-1) = - tau*eps/(hj1+hj2)/hj1

          A%DIAG(k) =  1.0_RNP + tau/2.0_RNP * &
                    & ( eps*2.0_RNP/(hi1*hi2) + eps*2.0_RNP/(hj1*hj2) + c )
          if (j<N) call ADD_ELEM(l,A,k,k+1,-tau*eps/(hj1+hj2)/hj2,rowstarter)
          if (i<N) call ADD_ELEM(l,A,k,k+N,-tau*eps/(hi1+hi2)/hi2,rowstarter)
          k = k+1
      enddo
  enddo

  A%ROWSTART(NN) = NNZ+1

end subroutine BUILD_STANDARD_2D
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE BUILD_STANDARD_2D >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE ADD_ELEM <<<
! ***
! *** Inserts an element at i,j with the content DATA and modifies
! *** A%COLUMN and A%ROWSTART accordingly. Note that these two vectors
! *** need to be allocated beforehand.
! ***
! ----------------------------------------------------------------------------
subroutine ADD_ELEM(k,A,i,j,data,rowstarter)

  !arguments
  integer(INP),intent(inout) :: k !current position in A%ROWSTART,A%COLUMN
  type(SPARSE),intent(inout) :: A
  integer(INP),intent(in)    :: i,j
  real(RNP)   ,intent(in)    :: data
  logical     ,intent(inout) :: rowstarter

  !local variables
!   integer(INP) :: k


  if( .not.allocated(A%DIAG)    .or. &
      .not.allocated(A%OFFDIAG) .or. &
      .not.allocated(A%COLUMN)  .or. &
      .not.allocated(A%ROWSTART)       ) &
 call STOP_ON_ERROR('ADD_ELEM (in SPARSEMOD)','A not properly allocated.')

  if(i.ge.j) then
      call SHOW_WARNING('ADD_ELEM (in SPARSEMOD)','Tried to add element at left lower part or diagonal.')
      return
  endif

  A%COLUMN(k) = j
  if (rowstarter) then
      A%ROWSTART(i) = k
      rowstarter    = .FALSE.
  endif
  A%OFFDIAG(k) = data
  k = k+1

end subroutine ADD_ELEM
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE ADD_ELEM >>>
! ----------------------------------------------------------------------------

end module SOLVE_RDPARABOL
! ============================================================================
! *** END of MODULE SOLVE_RDPARABOL >>>
! ============================================================================