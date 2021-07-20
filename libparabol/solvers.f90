! ============================================================================
! *** MODULE SOLVERS <<<
! ***
! *** Provides a quite general set of iterative solvers for linear equation
! *** systems, most prominently being GMRES.
! ***
! ============================================================================
module SOLVERS

use KINDMOD
use ERRORMOD
use PRECONDITIONERS

implicit none

  private

  public :: GMRES

  integer(INP),parameter :: m      = 30          !restart value
  real(RNP)   ,parameter :: errtol = 1.0E-15_RNP !error tolerance

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE GMRES <<<
! ***
! *** Well-known GMRES algorithm, applied to a block system with s blocks of
! *** size N
! ----------------------------------------------------------------------------
subroutine GMRES(N,s,b,x,Atimes)

  !arguments
  integer(INP)               ,intent(in)    :: N,s
  real(RNP)   ,dimension(N,s),intent(in)    :: b
  real(RNP)   ,dimension(N,s),intent(inout) :: x

  interface  !tell me how matrix multiplication with the system matrix A works
  function Atimes(N,s,x)
    USE KINDMOD
    integer(INP)             :: N,s
    real(RNP),dimension(N,s) :: Atimes,x
  end function
  end interface

  !locals
  integer(INP)                    :: i,j,k
  integer(INP)                    :: ic, min_iter, max_iter
  real(RNP)                       :: t, norm, beta
  real(RNP)   ,dimension(N,s,m+1) :: v
  real(RNP)   ,dimension(N,s)     :: z
  real(RNP)   ,dimension(m+1,m)   :: h
  real(RNP)   ,dimension(m)       :: c, ss
  real(RNP)   ,dimension(m+1)     :: gamma

  min_iter = 1
  max_iter = 1e5

  ic = 0
  v(:,:,1) = b - Atimes(N,s,x)

  do

      !the rhs construction is a DOT_PRODUCT replacement
      norm = SQRT(SUM( v(:,:,1)*v(:,:,1) ))

      if (norm.le.errtol) exit

      v(:,:,1) = v(:,:,1) / norm

      ! initialize first term  of rhs of the Hessenberg system
      gamma(1) = norm

      i = 0
      do
          if (i == m .or. ((norm <= errtol .or. ic >= max_iter) .and. ic >= min_iter))  exit

          ic = ic + 1
          i  = i  + 1

          !preconditioner
          z = v(:,:,i)

          call preconditioner(N,s,z)

          v(:,:,i+1) = Atimes(N,s,z)

          !--------------------------------------------
          ! Modified Gram-Schmidt algorithm
          !--------------------------------------------
          do j = 1,i
            t          = SUM( v(:,:,j)*v(:,:,i+1) )
            h(j,i)     = t
            v(:,:,i+1) = v(:,:,i+1) - t * v(:,:,j)
          end do
          t = SQRT(SUM( v(:,:,i+1) * v(:,:,i+1)))
          h(i+1,i) = t
          if ( t /= 0.0_RNP) v(:,:,i+1) = v(:,:,i+1) / t
          !--------------------------------------------

          ! update factorization of h,
          ! perform previous transformations on i-th column of h
          do k=2,i
            t        = h(k-1,i)
            h(k-1,i) =   c(k-1)*t + ss(k-1)*h(k,i)
            h(k,i)   = -ss(k-1)*t +  c(k-1)*h(k,i)
          end do

          beta = SQRT(h(i,i)**2 + h(i+1,i)**2)

          ! if gamma is zero then any small value will do.
          ! will affect only residual estimate
          if (beta .eq. 0.0_RNP) beta = EPSILON(beta)

          ! get next plane rotation
          c(i)       = h(i  ,i)/beta
          ss(i)      = h(i+1,i)/beta
          gamma(i+1) = -ss(i)*gamma(i)
          gamma(i)   =   c(i)*gamma(i)

          ! determine residual norm and test for convergence
          h(i,i) = c(i)*h(i,i) + ss(i)*h(i+1,i)
          norm = ABS(gamma(i+1))

      end do

      ! now compute solution.
      ! first solve upper triangular system.
      do k = i, 1, -1
          gamma(k) = ( gamma(k) - DOT_PRODUCT(h(k,k+1:i), gamma(k+1:i)) ) / h(k,k)
      end do

      ! form linear combination of v(*,i)'s to get solution
      ! and call preconditioner.
      ! the next line is replacement for the tensor multiplication
      forall(k=1:s)  z(:,k) = MATMUL(v(:,k,1:i), gamma(1:i))
      call preconditioner(N,s,z)
      x = x + z

      ! drop out if required
      if (norm <= errtol .or. IC >=max_iter) exit

      ! else compute residual vector and continue
      do j = i+1, 2, -1
          gamma(j-1) = -ss(j-1)*gamma(j)
      end do
      gamma(2:i+1) = c(1:i) * gamma(2:i+1)
      !again, the next line is tensor product replacement
      forall(k=1:s)  v(:,k,1) = MATMUL(v(:,k,:), gamma)

  end do

  if(ic.eq.max_iter) call ISSUE_WARNING('GMRES (in SOLVERS)',                &
                                        'Maximum no. of iterations reached.')

end subroutine GMRES
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE GMRES >>>
! ----------------------------------------------------------------------------

end module SOLVERS
! ============================================================================
! *** END of MODULE SOLVERS >>>
! ============================================================================