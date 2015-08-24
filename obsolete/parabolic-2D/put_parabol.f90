! ============================================================================
! *** MODULE PUT_PARABOL <<<
! ***
! ============================================================================
module PUT_PARABOL

  use KINDMOD
  use params_generic
  use SPARSEMOD
  use ITSOLVMOD
  use SOLVE_RDPARABOL
  use MESHES
!   use LINSOLVE

  implicit none

  contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE put_approximation <<<
! ***
! ----------------------------------------------------------------------------
subroutine put_approximation(Nx,x,Nt,tau,eps)

  !arguments
  integer(INP),                  intent(in) :: Nx !number of strictly inner points
  real(RNP)   ,dimension(0:Nx+1),intent(in) :: x
  integer(INP),                  intent(in) :: Nt
  real(RNP)   ,                  intent(in) :: tau,eps

  !locals
  integer(INP)                    :: i,j,k
  real(RNP)   ,dimension(1:Nx**2) :: u
  real(RNP)   ,dimension(1:Nx**2) :: rhs
  real(RNP)   ,dimension(1:2)     :: xx
  type(sparse)                    :: A
  real(RNP)                       :: t

  write(*,FMT='(2I4)') Nx+2, & !number of nodes in 1D (including boundaries)
                       Nt+1    !total number of timelevels (including T=0)

  k = 1
  do i=1,Nx
      xx(1) = x(i)
      do j=1,Nx
          xx(2) = x(j)
          u(k)  = u0(xx,eps)
          k = k+1
      enddo
  enddo

  !Build equation system
  call BUILD_STANDARD_2D(Nx,A,x,tau,eps,c)
  call get_rhs(Nx,rhs,x,u,eps,tau,t)

!   call put(A)
!   write(*,*) rhs

  !####################################################
  !solve step by step and put to standard out
  t = 0.0_RNP
  call put_atom(Nx,x,t,u)
  do j=1,Nt

      t = t + tau
      !---------------------------------
      !get rhs
      call get_rhs(Nx,rhs,x,u,eps,tau,t)
      !---------------------------------

      !---------------------------------
      !solve the man
      call CG(A,rhs,u,1.0E-10_RNP,1.0E-10_RNP,100)
      !---------------------------------

      !---------------------------------
      !...and tell the truth
      call put_atom(Nx,x,t,u)
      !---------------------------------

  enddo
  !####################################################

end subroutine put_approximation
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_approximation >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE put_atom <<<
! ***
! *** Helper routine for put_approximation. Puts the numerical solution of one
! *** time step to standard out.
! ***
! ----------------------------------------------------------------------------
subroutine put_atom(Nx,x,t,u)

  !arguments
  integer(INP),                   intent(in) :: Nx !number of strictly inner points
  real(RNP)   ,dimension(0:Nx+1), intent(in) :: x
  real(RNP)   ,                   intent(in) :: t
  real(RNP)   ,dimension(1:Nx**2),intent(in) :: u

  !locals
  integer(INP)                    :: i,j,k
  real(RNP)                       :: uk,xx(1:2)

  k = 1
  do i=0,Nx+1
      do j=0,Nx+1
          if(i.eq.0 .or. i.eq.Nx+1 .or. j.eq.0 .or. j.eq.Nx+1) then
              xx = (/ x(i) , x(j) /)
              uk = gamma(xx,t)
          else
              uk = u(k)
              k = k+1
          endif
          write(*,FMT='(4(F20.15,2X))') t,x(i),x(j),uk
      enddo
  enddo

end subroutine put_atom
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_atom >>>
! ----------------------------------------------------------------------------

end module PUT_PARABOL
! ============================================================================
! *** END of MODULE PUT_PARABOL >>>
! ============================================================================