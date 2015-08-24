! ============================================================================
! *** MODULE ITSOLVMOD <<<
! ***
! ***
! ============================================================================
module ITSOLVMOD

USE KINDMOD
USE ERRORMOD
USE SPARSEMOD

implicit none

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE Jacobi <<<
! ***
! *** Solves equation system A*u=f with initial approximation u
! *** with the Jacobi method.
! ----------------------------------------------------------------------------
subroutine Jacobi(A,f,u,reltol,abstol,max_it)

  !arguments
  type(SPARSE)          ,intent(in)    :: A       !matrix of the equation system
  real(RNP),dimension(:),intent(in)    :: f       !right hand side
  real(RNP),dimension(:),intent(inout) :: u       !initial approximation & result vector
  real(RNP)             ,intent(in)    :: reltol,abstol    !relative/absolute tolerance
  integer(INP)          ,intent(in)    :: max_it    !maximal number of iterations

  !local variables
  integer(INP)                       :: it,i,j,n
  real(RNP)                          :: tol0,rho
  real(RNP),dimension(:),allocatable :: temp

  n = A%N

  !check sizes
  if ( (size(f)/=n).or.(size(u)/=n) ) &
& call STOP_ON_ERROR('Jacobi (in SPARSEMOD)','Vector/matrix sizes do not match.')

  if (any(abs(A%DIAG)<abstol)) &
& call STOP_ON_ERROR('Jacobi (in SPARSEMOD)','Zero(s) on the main diagonal.')

  allocate(temp(1:n))

  call MATVEC(A,u,temp)
  call NORMINF(f-temp,rho)

  ! norm(r)/norm(r0) <= tol
  tol0 = rho*reltol

  it = 0
  !start: main loop
  do while ( (rho.gt.tol0) .or. (rho.gt.abstol) )

      if (it.eq.max_it) then
          call SHOW_WARNING('Jacobi (in SPARSEMOD)','Maximal number of iterations reached.')
          exit
      endif

      it = it+1
      !--------------------------------
      ! update
      temp = f
      do i=1,A%N-1
          do j=A%ROWSTART(i),A%ROWSTART(i+1)-1
              !left lower triangle
              temp(A%COLUMN(j)) = temp(A%COLUMN(j)) - A%OFFDIAG(j) * u(i)
              !right upper triangle
              temp(i)           = temp(i)           - A%OFFDIAG(j) * u(A%COLUMN(j))
          enddo
      enddo
      do i=1,n
          u(i) = temp(i) / A%DIAG(i)
      enddo
      ! update done
      !--------------------------------

      !compute norm of residual
      call MATVEC(A,u,temp)
      call NORMINF(f-temp,rho)
  enddo
  !end: main loop

  if (allocated(temp)) deallocate(temp)

end subroutine Jacobi
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Jacobi >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE GaussSeidel <<<
! ***
! *** Solves equation system A*u=f with initial approximation u
! *** with the Gauss-Seidel method.
! ----------------------------------------------------------------------------
subroutine GaussSeidel(A,f,u,reltol,abstol,max_it)

  !arguments
  type(SPARSE)               ,intent(in)    :: A       !matrix of the equation system
  real(RNP),dimension(:),intent(in)    :: f       !right hand side
  real(RNP),dimension(:),intent(inout) :: u       !initial approximation & result vector
  real(RNP)             ,intent(in)    :: reltol,abstol    !relative/absolute tolerance
  integer(INP)          ,intent(in)    :: max_it    !maximal number of iterations

  !local variables
  integer(INP)                            :: it,i,j,n
  real(RNP)                          :: tol0,rho
  real(RNP),dimension(:),allocatable :: temp

  n = A%N

  !check sizes
  if ( (size(f)/=n).or.(size(u)/=n) ) &
& call STOP_ON_ERROR('GaussSeidel (in SPARSEMOD)','Vector/matrix sizes do not match.')

  if (any(A%DIAG.eq.0.0)) &
& call STOP_ON_ERROR('GaussSeidel (in SPARSEMOD)','Zero(s) on the main diagonal.')

  allocate(temp(1:n))

  call MATVEC(A,u,temp)
  call NORMINF(f-temp,rho)

  ! norm(r)/norm(r0) <= tol
  tol0 = rho*reltol

  it = 0
  !start: main loop
  do while ( (rho.gt.tol0) .or. (rho.gt.abstol) )

      if (it.eq.max_it) then
          call SHOW_WARNING('GaussSeidel (in SPARSEMOD)','Maximal number of iterations reached.')
          exit
      endif

      it = it+1
      !--------------------------------
      ! update

      !right upper triangle
      do i=1,n-1
          u(i) = f(i)
          do j=A%ROWSTART(i),A%ROWSTART(i+1)-1
              u(i) = u(i) - A%OFFDIAG(j) * u(A%COLUMN(j))
          enddo
      enddo
      u(n) = f(n)

      !finish off with left lower triangle
      do i=1,n-1
          u(i) = u(i)/A%DIAG(i)
          do j=A%ROWSTART(i),A%ROWSTART(i+1)-1
              u(A%COLUMN(j)) = u(A%COLUMN(j)) - A%OFFDIAG(j) * u(i)
          enddo
      enddo

      u(n) = u(n)/A%DIAG(n)
      ! update done
      !--------------------------------

      !compute norm of residual
      call MATVEC(A,u,temp)
      call NORMINF(f-temp,rho)
  enddo
  !end: main loop

  if (allocated(temp)) deallocate(temp)

end subroutine GaussSeidel
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE GaussSeidel >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE SOR <<<
! ***
! *** Solves equation system A*u=f with initial approximation u
! *** with the SOR method.
! ----------------------------------------------------------------------------
subroutine SOR(A,f,u,omega,reltol,abstol,max_it)

  !arguments
  type(SPARSE)               ,intent(in)    :: A       !matrix of the equation system
  real(RNP),dimension(:),intent(in)    :: f       !right hand side
  real(RNP),dimension(:),intent(inout) :: u       !initial approximation & result vector
  real(RNP)             ,intent(in)    :: omega   !relaxation parameter
  real(RNP)             ,intent(in)    :: reltol,abstol    !relative/absolute tolerance
  integer(INP)          ,intent(in)    :: max_it    !maximal number of iterations

  !local variables
  integer(INP)                            :: i,j,n,it
  real(RNP)                          :: tol0,rho
  real(RNP),dimension(:),allocatable :: temp

  n = A%N

  !check sizes
  if ( (size(f)/=n).or.(size(u)/=n) ) &
& call STOP_ON_ERROR('SOR (in SPARSEMOD)','Vector/matrix sizes do not match.')

  if (any(A%DIAG.eq.0.0)) &
& call STOP_ON_ERROR('SOR (in SPARSEMOD)','Zero(s) on the main diagonal.')

  allocate(temp(1:n))

  call MATVEC(A,u,temp)
  call NORMINF(f-temp,rho)

  ! norm(r)/norm(r0) <= tol
  tol0 = rho*reltol
  it = 0
  !start: main loop
  do while ( (rho.gt.tol0) .or. (rho.gt.abstol) )

      if (it.eq.max_it) then
          call SHOW_WARNING('GaussSeidel (in SPARSEMOD)','Maximal number of iterations reached.')
          exit
      endif

      it = it+1
      !--------------------------------
      ! update

      !diagonal and right upper triangle
      do i=1,n-1
          temp(i) = f(i)-A%DIAG(i)*u(i)
          do j=A%ROWSTART(i),A%ROWSTART(i+1)-1
              temp(i) = temp(i) - A%OFFDIAG(j) * u(A%COLUMN(j))
          enddo
      enddo
      temp(n) = f(n)-A%DIAG(n)*u(n)

      !finish off with left lower triangle
      do i=1,n-1
          u(i) = u(i) + omega/A%DIAG(i) * temp(i)
          do j=A%ROWSTART(i),A%ROWSTART(i+1)-1
              temp(A%COLUMN(j)) = temp(A%COLUMN(j)) - A%OFFDIAG(j) * u(i)
          enddo
      enddo
      u(n) = u(n) + omega/A%DIAG(n) * temp(n)
      ! update done
      !--------------------------------

      !compute norm of residual
      call MATVEC(A,u,temp)
      call NORMINF(f-temp,rho)
  enddo
  !end: main loop

  if (allocated(temp)) deallocate(temp)

end subroutine SOR
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE SOR >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE CG <<<
! ***
! *** Solves equation system A*u=f with initial approximation u
! *** with the Conjugate Gradient method.
! ----------------------------------------------------------------------------
subroutine CG(A,f,u,reltol,abstol,max_it)

  !arguments
  type(SPARSE)                ,intent(in)    :: A       !matrix of the equation system
  real(RNP),dimension(:),intent(in)    :: f       !right hand side
  real(RNP),dimension(:),intent(inout) :: u       !initial approximation & result vector
  real(RNP)             ,intent(in)    :: reltol,abstol    !relative/absolute tolerance
  integer(INP)          ,intent(in)    :: max_it    !maximal number of iterations

  !local variables
  integer(INP)                         :: n,it
  real(RNP),dimension(:)  ,allocatable :: res,p,w
  real(RNP),dimension(1:2)             :: rho        !rho(i) represents \rho^{(it-i)}
  real(RNP)                            :: tol0,alpha

  n = A%N

  !check sizes
  if ( (size(f)/=n).or.(size(u)/=n) ) &
& call STOP_ON_ERROR('CG (in SPARSEMOD)','Vector/matrix sizes do not match.')

  allocate(res(1:n))
  allocate(p(1:n))
  allocate(w(1:n))

  call MATVEC(A,u,res)
  res = f-res
  rho(1) = DOT_PRODUCT(res,res)

  ! norm_2(r)^2/norm_2(r0)^2 <= tol^2
  tol0 = rho(1)*reltol*reltol

  it = 0
  p = res
  !start: main loop
  do while ( (rho(1).gt.tol0) .or. (rho(1).gt.abstol) )

      if (it.eq.max_it) then
          call SHOW_WARNING('CG (in SPARSEMOD)','Maximal number of iterations reached.')
          exit
      endif

      it = it+1

      call MATVEC(A,p,w)
      alpha = rho(1) / DOT_PRODUCT(p,w)
      u = u + alpha*p
      res = res - alpha*w
      rho(2) = rho(1)
      rho(1) = DOT_PRODUCT(res,res)

      !get p ready for next step
      p = res + rho(1)/rho(2)*p
  enddo
  !end: main loop

  if (allocated(res)) deallocate(res)
  if (allocated(p))   deallocate(p)
  if (allocated(w))   deallocate(w)

end subroutine CG
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE CG >>>
! ----------------------------------------------------------------------------


! ! ----------------------------------------------------------------------------
! ! *** SUBROUTINE BlockGaussSeidel <<<
! ! ***
! ! *** Solves equation system A*u=f with initial approximation u
! ! *** with the Gauss-Seidel method. The blocks on the main diagonal
! ! *** need to be tridiagonal for this version to work.
! ! ----------------------------------------------------------------------------
! subroutine BlockGaussSeidel(A,f,u,blocksize,reltol,abstol,max_it)
! 
!   !arguments
!   type(SPARSE)                ,intent(in)    :: A       !matrix of the equation system
!   real(RNP),dimension(:),intent(in)    :: f       !right hand side
!   real(RNP),dimension(:),intent(inout) :: u       !initial approximation & result vector
!   real(RNP)             ,intent(in)    :: reltol,abstol    !relative/absolute tolerance
!   integer(INP)          ,intent(in)    :: max_it    !maximal number of iterations
! 
!   !local variables
!   integer(INP)                            :: it,i,j,k,n
!   real(RNP)                          :: tol0,rho
!   real(RNP),dimension(:),allocatable :: temp
! 
!   n = A%N
! 
!   !check sizes
!   if ( (size(f)/=n).or.(size(u)/=n) ) &
! & call STOP_ON_ERROR('BlockGaussSeidel (in SPARSEMOD)','Vector/matrix sizes do not match.')
! 
!   if (mod(n,blocksize).ne.0) &
! & call STOP_ON_ERROR('BlockGaussSeidel (in SPARSEMOD)','Invalid block size.')
! 
!   blocknum = n/blocksize
! 
!   !------------------------------------------
!   !initiate vector which points for each row
!   !to the element which is the first one not
!   !sitting on the main diagonal block
!   allocate(offdiagblockstart(1:n-blocksize))
!   do k=1,blocknum-1
!       do i= (k-1)*blocksize+1 , k*blocksize
!           j=ROWSTART(i)
!           do while (A%COLUMN(j).le.k*blocksize)
!               j=j+1
!           enddo
!           offdiagblockstart(i) = j
!       enddo
!   enddo
!   !------------------------------------------
! 
! 
!   allocate(temp(1:n))
! 
!   call MATVEC(A,u,temp)
!   call NORMINF(f-temp,rho)
! 
!   ! norm(r)/norm(r0) <= tol
!   tol0 = rho*reltol
! 
!   it = 0
!   !start: main loop
!   do while ( (rho.gt.tol0) .or. (rho.gt.abstol) )
! 
!       if (it.eq.max_it) then
!           call SHOW_WARNING('BlockGaussSeidel (in SPARSEMOD)','Maximal number of iterations reached.')
!           exit
!       endif
! 
!       it = it+1
!       !--------------------------------
!       ! update
!       !right upper block triangle
!       do k=1,blocknum-1
!           do i= (k-1)*blocksize+1 , k*blocksize
!               u(i) = f(i)
!               do j=offdiagblockstart(i),A%ROWSTART(i+1)-1
!                   u(i) = u(i) - A%OFFDIAG(j) * u(A%COLUMN(j))
!               enddo
!           enddo
!       enddo
!       do i= n-blocksize+1 , n
!           u(i) = f(i)
!       enddo
! 
!       !finish off with left lower block triangle
!       do k=1,blocknum-1
!           !SOLVE tridiag eqnsystem: u(k) = u(k)/A%DIAG(k)
!           do i= (k-1)*blocksize+1 , k*blocksize
!               do j=offdiagblockstart(i),A%ROWSTART(i+1)-1
!                   u(A%COLUMN(j)) = u(A%COLUMN(j)) - A%OFFDIAG(j) * u(i)
!               enddo
!            enddo
!       enddo
!       !SOLVE tridiag eqnsystem: u(n) = u(n)/A%DIAG(n)
!       ! update done
!       !--------------------------------
! 
!       !compute norm of residual
!       call MATVEC(A,u,temp)
!       call NORMINF(f-temp,rho)
!   enddo
!   !end: main loop
! 
!   if (allocated(temp))               deallocate(temp)
!   if (allocated(offdiagblockstart))  deallocate(offdiagblockstart)
! 
! end subroutine BlockGaussSeidel
! ! ----------------------------------------------------------------------------
! ! *** END of SUBROUTINE BlockGaussSeidel >>>
! ! ----------------------------------------------------------------------------


end module ITSOLVMOD
! ============================================================================
! *** END of MODULE ITSOLVMOD >>>
! ============================================================================
