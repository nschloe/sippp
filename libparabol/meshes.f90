! Provides one-dimensional meshes on [0,1] for use in solving pdes.
! 
! The parameters EPS and RHO all refer to the differential equation
! 
! -eps u_xx + c u = f
! (+) bc (+) ic
module MESHES

   USE KINDMOD
   USE ERRORMOD
   USE intermod, ONLY: get_rho, get_eps

   implicit none

   private

   public mesh, set_mesh, mesh_up_to_date, get_meshpoint, current_mesh

   type mesh
      character(LEN=2) :: type  !UN (uniform), SH (Shishkin),...
      integer(INP)     :: N     !number of strictly inner points
   end type mesh

   INTERFACE get_meshpoint
      MODULE PROCEDURE get_meshpoint_single
      MODULE PROCEDURE get_meshpoint_all
   END INTERFACE get_meshpoint

   type(mesh)            :: current_mesh = mesh('--', 0_INP)
   logical               :: init_x = .FALSE.
   real(RNP), allocatable :: x_save(:)
   real(RNP)             :: eps_save = 0.0_RNP, &
                            sigma = 2.0_RNP !expected convergence rate

contains

   subroutine set_mesh(type, N)

      ! Arguments
      character(LEN=2) :: type ! UN=uniform, SH=Shishkin,...
      integer(INP)     :: N    ! number of interior mesh points

      ! write(*,*) ' >>> set_mesh'

      if (.NOT. mesh_up_to_date(mesh(type, N)) .OR. get_eps() .NE. eps_save) then
         if (ALLOCATED(x_save)) deallocate (x_save)
         allocate (x_save(0:N + 1))

         select case (type)
         case ('UN'); call Setup_Mesh_uniform(N)
         case ('SH'); call Setup_Mesh_Shishkin(N)
         case ('BA'); call Setup_Mesh_Bakhvalov(N)
         case default; call STOP_ON_ERROR('set_mesh (in MESHES)', &
 'Mesh type not supported.')
         end select

         ! Set module variables
         current_mesh = mesh(type, N)
         eps_save = get_eps()
         init_x = .TRUE.
      end if

! write(*,*) '     set_mesh >>>'

      return
   end subroutine set_mesh

   logical function mesh_up_to_date(msh)

      ! Arguments
      type(mesh), intent(in) :: msh

      mesh_up_to_date = (msh%N .EQ. current_mesh%N .AND. msh%type .EQ. current_mesh%type)

      return
   end function mesh_up_to_date

   function get_meshpoint_single(idx) result(x_idx)

      ! Arguments
      integer, intent(in) :: idx  !point index

      ! Result
      real(RNP) :: x_idx

      if (.NOT. init_x) call STOP_ON_ERROR('get_meshpoint_single (in MESHES)', &
                                           'Mesh not initialised.')

      x_idx = x_save(idx)

      return
   end function get_meshpoint_single

   function get_meshpoint_all() result(x)

      ! Result
      real(RNP), dimension(0:current_mesh%N + 1) :: x

      if (.NOT. init_x) call STOP_ON_ERROR('get_meshpoint_all (in MESHES)', &
                                           'Mesh not initialised.')

      x = x_save

      return
   end function get_meshpoint_all

   ! Get uniform x on [0,1].
   subroutine Setup_Mesh_uniform(N)
      integer(INP), intent(in) :: N
      integer(INP) :: i
      do i = 0, N + 1
         x_save(i) = real(i, RNP)/real(N + 1, RNP)
      end do
   end subroutine Setup_Mesh_uniform

   subroutine Setup_Mesh_Shishkin(Nx)
      integer(INP), intent(in) :: Nx !no. of strictly interior points
      !locals
      integer(INP) :: N, N1, N2, i, k
      real(RNP)    :: lambda, &
                      q, & !percentage of nodes located in the boundary area
                      sqrteps, & !square root of the diffusion parameter
                      sqrtrho    !square root of lower bound on C

      sqrteps = sqrt(get_eps())
      sqrtrho = sqrt(get_rho())

      q = 0.25_RNP

      if (Nx .le. 2) &
         call STOP_ON_ERROR('Setup_Mesh_Shishkin', 'Insufficient number of nodes.')
      if (q .ge. 0.5_RNP .or. q .le. 0.0_RNP) &
         call STOP_ON_ERROR('Setup_Mesh_Shishkin', 'Parameter q needs to fulfil 0<q<1/2.')

      N = Nx + 1 !number of subintervals

      !pattern : [0,lambda],[lambda,1-lambda],[1-lambda,1]
      if (ABS(get_rho()) .lt. TOL) then
!       call ISSUE_WARNING('shishkin','RHO seems to be 0. Adjusting LAMBDA.')
         lambda = min(q, sqrteps*sigma*log(real(N, RNP)))
      else
         lambda = min(q, sqrteps*sigma/sqrtrho*log(real(N, RNP)))
      end if

      !N1: subintervals on each of the boundary areas (left & right)
      N1 = ceiling(q*N, INP)

      N2 = N - 2*N1

      k = 1

      !left boundary
      x_save(0) = 0.0_RNP
      do i = 1, N1
         x_save(k) = x_save(k - 1) + lambda/N1
         k = k + 1
      end do

      !centre
      do i = 1, N2
         x_save(k) = x_save(k - 1) + (1.0_RNP - 2.0_RNP*lambda)/N2
         k = k + 1
      end do

      !right boundary
      do i = 1, N1
         x_save(k) = x_save(k - 1) + lambda/N1
         k = k + 1
      end do

   end subroutine Setup_Mesh_Shishkin

   ! Returns a graded Bakhvalov mesh as described in
   ! 
   ! N. S. Balakhov
   ! Towards optimization of methods for solving boundary value problems
   ! in the presence of boundary layers.
   ! Zh. Vychisl. Math. i Mat. Fiz., 9:841-859,1969, In Russian.
   ! 
   ! For further reference see
   ! 
   ! Lin{\"s}, Torsten and Madden, Niall
   ! Parameter uniform approximations for time-dependent
   ! reaction-diffusion problems
   subroutine Setup_Mesh_Bakhvalov(Nx)

      !arguments
      integer(INP), intent(in) :: Nx !number of inner node points

      !locals
      integer(INP) :: N1, N2, i
      real(RNP)    :: C, Cfrac, lambda, rest, &
                      kappa, &
                      sqrteps, & !sqare root of the diffusion parameter
                      sqrtrho    !square root of strict lower bound on C

      if (ABS(get_rho()) .lt. TOL) &
    & call STOP_ON_ERROR('Setup_Mesh_Bakhvalov', 'RHO seems to be 0.')

      sqrteps = sqrt(get_eps())
      sqrtrho = sqrt(get_rho())

      kappa = 1.0_RNP

      !transition point between layers
      lambda = sqrteps*sigma/sqrtrho*log(kappa/sqrteps)

      if (lambda .le. 0.0_RNP) then
         do i = 0, Nx + 1
            x_save(i) = real(i, RNP)/real(Nx + 1, RNP)
         end do
      elseif (lambda .ge. 0.5_RNP) then
         C = 2.0_RNP*sigma*kappa/sqrtrho*(1.0_RNP - exp(-sqrtrho/2.0_RNP/sqrteps/sigma))

         !int_{x_{i-1}}^{x_i} M{BA} = Cfrac
         Cfrac = C/(Nx + 1)

         ! (half of the) area, including the middle point
         N2 = ceiling(real(Nx)/2)
         x_save(0) = 0.0_RNP
         do i = 1, N2
            x_save(i) = -sqrteps*sigma/sqrtrho &
                        *log(exp(-sqrtrho*x_save(i - 1)/sqrteps/sigma) &
                             - Cfrac*sqrtrho/kappa/sigma)
         end do

         ! do the rest by symmetry
         do i = N2 + 1, Nx + 1
            x_save(i) = 1.0_RNP - x_save(Nx + 1 - i)
         end do
      else
         C = 1.0_RNP + 2.0_RNP*((kappa - sqrteps)*sigma/sqrtrho - lambda)

         !int_{x_{i-1}}^{x_i} M{BA} = Cfrac
         Cfrac = C/(Nx + 1)

         !boundary layer
         x_save(0) = 0.0_RNP
         !that many nodes fit into the boundary layer:
         N1 = floor((kappa - sqrteps)*sigma/sqrtrho/Cfrac)
         do i = 1, N1
            x_save(i) = -sqrteps*sigma/sqrtrho &
                        *log(exp(-sqrtrho*x_save(i - 1)/sqrteps/sigma) &
                             - Cfrac*sqrtrho/kappa/sigma)
         end do

         ! transition between layers
         ! int_{x_{i}}^{lambda} M_{BA} =
         rest = kappa*sigma/sqrtrho &
                *(exp(-sqrtrho*x_save(N1)/sqrteps/sigma) - sqrteps/kappa)
         x_save(N1 + 1) = lambda + (Cfrac - rest)

         ! (half of the) inner layer, including the middle point
         N2 = ceiling(real(Nx)/2, INP)
         do i = N1 + 2, N2
            x_save(i) = x_save(i - 1) + Cfrac
         end do

         ! do the rest by symmetry
         do i = N2 + 1, Nx + 1
            x_save(i) = 1.0_RNP - x_save(Nx + 1 - i)
         end do
      end if

   end subroutine Setup_Mesh_Bakhvalov

end module MESHES
