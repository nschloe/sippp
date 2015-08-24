! ============================================================================
! *** MODULE SOLVERS <<<
! ***
! *** Provides a quite general set of iterative solvers for linear equation
! *** systems, most prominently being GMRES.
! ***
! ============================================================================
module SOLVERS

use KINDMOD
use PRECONDITIONERS
use RK_parameters

implicit none

private

public :: GMRES

! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  integer(INP),parameter :: restart  = 30! restart value
  integer(INP),parameter :: min_iter = 1
  integer(INP),parameter :: max_iter = 10000
  real(RNP)   ,parameter :: restol   = 1.0E-15_RNP
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE GMRES <<<
! ***
! *** Well-known GMRES algorithm.
! ----------------------------------------------------------------------------
subroutine GMRES(N,b,x,Atimes,ic)

  !arguments
  integer(INP)               ,intent(in)    :: N
  real(RNP)   ,dimension(1:n),intent(in)    :: b
  real(RNP)   ,dimension(1:n),intent(inout) :: x
  integer(INP)               ,intent(out)   :: ic

  interface  !tell me how matrix multiplication with the system matrix A works
  function Atimes(n,x)
    USE KINDMOD
    integer(INP)             :: n
    real(RNP),dimension(1:n) :: Atimes,x
  end function
  end interface

  !locals
  integer(INP)                              :: i,j,k
  real(RNP)                                 :: temp, norm, beta
  real(RNP)   ,dimension(N,restart+1)       :: v
  real(RNP)   ,dimension(N)                 :: z
  real(RNP)   ,dimension(restart+1,restart) :: h
  real(RNP)   ,dimension(restart)           :: c, s
  real(RNP)   ,dimension(restart+1)         :: gamma

  ic = 0_INP

  v(:,1) = b - Atimes(n,x)

  do
      !-----------------------------------------------
      flop_count = flop_count + 2*N
      !-----------------------------------------------
      norm = SQRT(DOT_PRODUCT(v(:,1),v(:,1)))

      if (norm.le.restol) exit

      v(:,1) = v(:,1) / norm
      !-----------------------------------------------

      ! initialize first term  of rhs of the Hessenberg system
      gamma(1) = norm

      i = 0
      do
          if ( i == restart .or.                                             &
               ((norm <= restol .or. ic >= max_iter) .and. ic >= min_iter)   &
             )  exit

          ic = ic + 1
          i  = i  + 1

          !preconditioner

          call preconditioner(n,v(:,i), z)

          v(:,i+1) = Atimes(n,z)

          !--------------------------------------------
          ! Modified Gram-Schmidt algorithm
          flop_count = flop_count + i*( 3*N ) + N
          !--------------------------------------------

          do j = 1,i
            temp     = DOT_PRODUCT( v(:,j), v(:,i+1) )
            h(j,i)   = temp
            v(:,i+1) = v(:,i+1) - temp * v(:,j)
          end do
          temp     = SQRT(DOT_PRODUCT (v(:,i+1), v(:,i+1)))
          h(i+1,i) = temp
          if ( temp /= 0.0_RNP) v(:,i+1) = v(:,i+1) / temp
          !--------------------------------------------

          !--------------------------------------------
          ! update factorization of h,
          ! perform previous transformations on i-th column of h
          flop_count = flop_count + (i-1)*6
          !--------------------------------------------
          do k=2,i
            temp     =  h(k-1,i)
            h(k-1,i) =  c(k-1)*temp + s(k-1)*h(k,i)
            h(k,i)   = -s(k-1)*temp + c(k-1)*h(k,i)
          end do
          !--------------------------------------------

          !--------------------------------------------
          flop_count = flop_count + 3
          !--------------------------------------------
          beta = SQRT(h(i,i)**2 + h(i+1,i)**2)
          !--------------------------------------------

          ! if gamma is zero then any small value will do.
          ! will affect only residual estimate
          if (beta .eq. 0.0_RNP) beta = EPSILON(beta)

          !--------------------------------------------
          ! get next plane rotation
          flop_count = flop_count + 4
          !--------------------------------------------
          c(i)       = h(i  ,i)/beta
          s(i)       = h(i+1,i)/beta
          gamma(i+1) = -s(i)*gamma(i)
          gamma(i)   =  c(i)*gamma(i)
          !--------------------------------------------

          !--------------------------------------------
          ! determine residual norm and test for convergence
          flop_count = flop_count + 3
          !--------------------------------------------
          h(i,i) = c(i)*h(i,i) + s(i)*h(i+1,i)
          norm = ABS(gamma(i+1))
          !--------------------------------------------

      end do

      !--------------------------------------------
      ! now compute solution.
      ! first solve upper triangular system.
      flop_count = flop_count + 2*i + i**2 - i*(i+1)/2
      !--------------------------------------------
      do k = i, 1, -1
          gamma(k) = ( gamma(k) - DOT_PRODUCT(h(k,k+1:i), gamma(k+1:i)) ) / h(k,k)
      end do
      !--------------------------------------------

      !--------------------------------------------
      ! form linear combination of v(*,i)'s to get solution
      ! and call preconditioner
      flop_count = flop_count + N
      !--------------------------------------------
      call preconditioner(n, MATMUL(v(:,1:i), gamma(1:i)), z)
      x = x + z
      !--------------------------------------------

      !--------------------------------------------
      ! drop out if required
      if (norm <= restol .or. IC >=max_iter) exit
      !--------------------------------------------

      !--------------------------------------------
      ! else compute residual vector and continue
      flop_count = flop_count + 2*i + N*(restart+1)
      !--------------------------------------------
      do j = i+1, 2, -1
          gamma(j-1) = -s(j-1)*gamma(j)
      end do
      gamma(2:i+1) = c(1:i) * gamma(2:i+1)
      v(:,1) = MATMUL(v,gamma)
      !--------------------------------------------

  end do

! write(*,*) 'iteration count:',ic
! write(*,*) 'norm',norm

end subroutine GMRES
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE GMRES >>>
! ----------------------------------------------------------------------------

end module SOLVERS
! ============================================================================
! *** END of MODULE SOLVERS >>>
! ============================================================================