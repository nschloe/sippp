! ############################################################
! *** PROGRAM MATRIXTEST
! ***
! *** Computes the maximum norm of the inverse of a complex
! *** valued matrix (tridiagonal) of the form
! ***
! *** main diagonal:  z + 2* (eps/h)^2 + c
! *** first upper and lower diagonals: -(eps/h)^2
! ***
! ############################################################
program matrixtest

  use INORM
  use BOUNDFUN
  use MESHES
  use matrices
  use params_generic

  implicit none

  real(RNP) :: alpha,kappa
  real(RNP) :: infnorm
  real(RNP) :: x0,x1,y0,y1, REALz,AIMAGz
  real(RNP),dimension(:),allocatable :: x !1D mesh in [0,1]
  complex(CNP),dimension(:),allocatable :: z_temp !1D mesh in [0,1]
  complex(cnp) :: z
  integer      :: xsteps,ysteps
  integer      :: N
  integer      :: i,j,ix,jy,jybreak,k
  real(rnp)   ,dimension(:,:),allocatable :: A
  complex(cnp),dimension(:,:),allocatable :: Az
  type(mesh) :: msh

  N = 100 !number of strictly interior points

  eps = 1.0E-3_RNP

  alpha = 3.0D0
  kappa = eps * (N+1)**2

  !-------------------------
  ! define the area that is being searched through
  !-------------------------
!   x0 = 8.1D0 * (- alpha/(alpha-1.0D0) * (4.0D0*kappa+rho) )
!   x1 =  abs(x0)/10.0D0
  x0 = -5
  x1 =   1
  xsteps = 50
!   y0 = - 1.0D1 * (4.0D0*kappa+c)*alpha/(alpha**2-1.0D0)
!   y1 = + 1.0D1 * (4.0D0*kappa+c)*alpha/(alpha**2-1.0D0)
  y0 = -10
  y1 =  10
  ysteps = 1000
  !-------------------------

  allocate(A (N,N)) !the discretisation matrix, real
  allocate(Az(N,N)) !the matrix zI-A, complex

  !-----------
  ! write outfile header
!   write(*,FMT='(A,F7.3)') '#  alpha=',alpha
!   write(*,FMT='(A,F7.3)') '#  kappa=',kappa
!   write(*,*)
!   write(*,FMT='(A)') '#  area specs'
!   write(*,FMT='(3(F12.4,3X))') alpha,kappa,c
!   write(*,FMT='(3(F12.4,3X))') x0,(x1-x0)/xsteps,x1
!   write(*,FMT='(3(F12.4,3X))') y0,(y1-y0)/ysteps,y1
!   write(*,*)
!   write(*,*)
!   write(*,FMT='(A)') '# actual data:'
  !-----------

  ! -------------------------------
  !get point distribution
  ! -------------------------------
  allocate(x(0:N+1))
!   msh = mesh('UN',N)
!   msh = mesh('SH',N)
  msh = mesh('UN',N)

!   call uniform(x(0:N+1))
!   call shishkin(N,x(0:N+1),q=0.25_RNP,sqrteps=sqrt(eps),sigma=2.0_RNP,sqrtgamma=sqrt(c))
!   call bakhvalov(x(0:N+1),kappa=1.0_RNP,sqrteps=sqrt(eps),sigma=2.0_RNP,sqrtgamma=sqrt(c))
  ! -------------------------------

  ! -------------------------------
  !fill the matrix
  ! -------------------------------
  call create_1D(msh,A)
  ! -------------------------------

  allocate(z_temp(0:2*xsteps))
  k=0

  do ix = 0,xsteps
      REALz = x0 + (x1-x0)/xsteps *ix
      jy = 0
      do jy=0,ysteps

          AIMAGz = y0 + (y1-y0)/ysteps *jy

          z = CMPLX( REALz , AIMAGz)

          ! -------------------------------
          !fill the matrix
          ! -------------------------------
          Az = (0.0_RNP,0.0_RNP)
          do i=1,N
              if (i>1) Az(i,i-1) =     A(i,i-1)
                       Az(i,i)   = z + A(i,i  )
              if (i<N) Az(i,i+1) =     A(i,i+1)
          enddo
          ! -------------------------------

          ! ######################################################################
          infnorm = INorm_invert(N,Az)
!           infnorm = INorm_condest(N,Az)
          ! ######################################################################

!           ! ######################################################################
!           ! put it to standard out!
!           ! ######################################################################
! 
! !           !rely on Fortran rounding towards 0 here (ysteps/2)
! !           if (jy.gt.ysteps/2) then
! ! !            do j = jy,ysteps
! ! !              AIMAGz =  y0 + (y1-y0)/ysteps *jy
! ! !              write(*,FMT="(2(F12.3,5X),I1)") REALz,AIMAGz,1
! ! !            enddo
! !             exit
! !           endif

! write(*,*) infnorm,alpha/bfun_abs(z)
          if ( infnorm.ge.alpha/bfun_abs(z) ) then
                  z_temp(k) = z
                  k=k+1
write(*,*)'.'
!                   write(*,FMT="('(',F12.3,',',F12.3,')')") REALz,AIMAGz
                  exit
!                   jy = jy+1
!           else
!               write(*,FMT="(2(F12.3,5X),I1)") REALz,AIMAGz,0
! !               jybreak = jy
! !               do jy = jybreak , ysteps-jybreak
! !                 AIMAGz =  y0 + (y1-y0)/ysteps *jy
! !                 write(*,FMT="(2(F12.3,5X),I1)") REALz,AIMAGz,0
! !               enddo
! !               do jy = ysteps-jybreak+1 , ysteps
! !                 AIMAGz =  y0 + (y1-y0)/ysteps *jy
! !                 write(*,FMT="(2(F12.3,5X),I1)") REALz,AIMAGz,1
! !               enddo
          endif
!           ! ######################################################################

!           ! ######################################################################
!           ! put it to standard out!
!           ! ######################################################################
!           write(*,FMT="(2(F12.3,5X),L)") REAL(z),AIMAG(Z),(infnorm.lt.alpha/bfun_abs(z))
!           ! ######################################################################

      enddo
  enddo

  do i=k-1,0,-1
    write(*,FMT="('(',F12.3,',',F12.3,')')") REAL(z_temp(i))/100,-AIMAG(z_temp(i))/100
  enddo

  do i=0,k-1
    write(*,FMT="('(',F12.3,',',F12.3,')')") REAL(z_temp(i))/100,AIMAG(z_temp(i))/100
  enddo

end program matrixtest
! ############################################################
! *** END PROGRAM MATRIXTEST
! ############################################################
