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
use RK_parameters

implicit none

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE GMRES <<<
! ***
! *** Well-known GMRES algorithm.
! ----------------------------------------------------------------------------
subroutine GMRES(n,b,x,m,ic)

  !arguments
  integer(INP)               ,intent(in)    :: n
  real(RNP)   ,dimension(1:n),intent(in)    :: b
  real(RNP)   ,dimension(1:n),intent(inout) :: x
  integer(INP)               ,intent(in)    :: m !restart value
  integer(INP)               ,intent(out)   :: ic

!   interface  !tell me how matrix multiplication with the system matrix A works
!   function stage_matrix_multiplication(n,x)
!     USE KINDMOD
!     integer(INP)             :: n
!     real(RNP),dimension(1:n) :: stage_matrix_multiplication,x
!   end function
!   end interface

  !locals
  integer(INP)                  :: i,j,k
  integer(INP)                  :: min_iter, max_iter
  real(RNP)                     :: t, norm, beta
  real(RNP)   ,dimension(n,m+1) :: v
  real(RNP)   ,dimension(n)     :: z
  real(RNP)   ,dimension(m+1,m) :: h
  real(RNP)   ,dimension(m)     :: c, s
  real(RNP)   ,dimension(m+1)   :: gamma

  min_iter = 1
  max_iter = 1e5

  ic = 0
  v(:,1) = b - stage_matrix_multiplication(n,x)
  do

      norm = SQRT(DOT_PRODUCT(v(:,1),v(:,1)))
      if (norm.le.tol) exit

      v(:,1) = v(:,1) / norm

      ! initialize first term  of rhs of the Hessenberg system
      gamma(1) = norm

      i = 0
      do
          if (i == m .or. ((norm <= tol .or. ic >= max_iter) .and. ic >= min_iter))  exit

          ic = ic + 1
          i  = i  + 1

          !preconditioner
          call preconditioner(n,v(:,i), z)

          v(:,i+1) = stage_matrix_multiplication(n,z)

          !--------------------------------------------
          ! Modified Gram-Schmidt algorithm
          !--------------------------------------------
          do j = 1,i
            t        = DOT_PRODUCT( v(:,j), v(:,i+1) )
            h(j,i)   = t
            v(:,i+1) = v(:,i+1) - t * v(:,j)
          end do
          t = SQRT(DOT_PRODUCT (v(:,i+1), v(:,i+1)))
          h(i+1,i) = t
          if ( t /= 0.0_RNP) v(:,i+1) = v(:,i+1) / t
          !--------------------------------------------

          ! update factorization of h,
          ! perform previous transformations on i-th column of h
          do k=2,i
            t        = h(k-1,i)
            h(k-1,i) =  c(k-1)*t + s(k-1)*h(k,i)
            h(k,i)   = -s(k-1)*t + c(k-1)*h(k,i)
          end do

          beta = SQRT(h(i,i)**2 + h(i+1,i)**2)

          ! if gamma is zero then any small value will do.
          ! will affect only residual estimate
          if (beta .eq. 0.0_RNP) beta = EPSILON(beta)

          ! get next plane rotation
          c(i)       = h(i  ,i)/beta
          s(i)       = h(i+1,i)/beta
          gamma(i+1) = -s(i)*gamma(i)
          gamma(i)   =  c(i)*gamma(i)

          ! determine residual norm and test for convergence
          h(i,i) = c(i)*h(i,i) + s(i)*h(i+1,i)
          norm = ABS(gamma(i+1))

      end do

      ! now compute solution.
      ! first solve upper triangular system.
      do k = i, 1, -1
          gamma(k) = ( gamma(k) - DOT_PRODUCT(h(k,k+1:i), gamma(k+1:i)) ) / h(k,k)
      end do

      ! form linear combination of v(*,i)'s to get solution
      ! and call preconditioner
      call preconditioner(n, MATMUL(v(:,1:i), gamma(1:i)), z)
      x = x + z

      ! drop out if required
      if (norm <= tol .or. IC >=max_iter) exit

      ! else compute residual vector and continue
      do j = i+1, 2, -1
          gamma(j-1) = -s(j-1)*gamma(j)
      end do
      gamma(2:i+1) = c(1:i) * gamma(2:i+1)
      v(:,1) = MATMUL(v,gamma)
  end do

write(*,*) 'iteration count:',ic
! write(*,*) norm

end subroutine GMRES
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE GMRES >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION stage_matrix_multiplication <<<
! ----------------------------------------------------------------------------
function stage_matrix_multiplication(Ns,Y) result(Z)

  use operators

  !arguments
  integer(INP)              ,intent(in) :: Ns
  real(RNP)   ,dimension(Ns),intent(in) :: Y   ! stage vector
  real(RNP)   ,dimension(Ns)            :: Z

  !locals
  integer(INP)            :: i,j,N
  real(RNP),dimension(Ns) :: L

  Z = Y

  N = Ns/s

  do j=1,s
      L = L_multiply( Y(N*(j-1)+1:N*j) )
      do i=1,s
          Z(N*(i-1)+1:N*i) = Z(N*(i-1)+1:N*i) + tau_save * A(i,j) * L
      end do
  end do

end function stage_matrix_multiplication
! ----------------------------------------------------------------------------
! *** END of FUNCTION stage_matrix_multiplication >>>
! ----------------------------------------------------------------------------


end module SOLVERS
! ============================================================================
! *** END of MODULE SOLVERS >>>
! ============================================================================
