! ============================================================================
! *** MODULE MESHES <<<
! ***
! *** Provides one-dimensional meshes on [0,1] for use in solving pdes.
! ***
! *** The parameters EPS and RHO all refer to the differential equation
! ***
! *** -eps u_xx + b u = f
! *** (+) bc (+) ic
! ***
! ============================================================================
module MESHES

  USE KINDMOD
  USE ERRORMOD
  USE params_generic, ONLY: rho,eps

  implicit none

  private

! ----------------------------------------------------------------------------
! *** PUBLIC MODULE ENTITIES <<<
! ----------------------------------------------------------------------------
  public mesh,set_mesh,get_mesh,get_mesh_N,get_meshpoint,                    &
         force_init_meshpoints,clean_meshpoints,mesh_eq
! ----------------------------------------------------------------------------
! *** END of PUBLIC MODULE ENTITIES >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** TYPE mesh <<<
! ----------------------------------------------------------------------------
  type mesh
      character(LEN=2) :: type  !UN (uniform), SH (Shishkin),...
      integer(INP)     :: N     !number of strictly inner points
  end type mesh
! ----------------------------------------------------------------------------
! *** END of TYPE mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** INTERFACES <<<
! ----------------------------------------------------------------------------
  INTERFACE get_meshpoint
      MODULE PROCEDURE get_meshpoint_single
      MODULE PROCEDURE get_meshpoint_all
  END INTERFACE get_meshpoint
! ----------------------------------------------------------------------------
! *** END of INTERFACES >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** MODULE VARIABLES <<<
! ----------------------------------------------------------------------------
  type(mesh)            :: current_mesh = mesh('--',0_INP)
  type(mesh)            :: mesh_save    = mesh('--',0_INP)
  logical               :: init_x = .FALSE.
  real(RNP),allocatable :: x_save(:)
  real(RNP)             :: eps_save = 0.0_RNP
! ----------------------------------------------------------------------------
! *** END of MODULE VARIABLES >>>
! ----------------------------------------------------------------------------

  contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE set_mesh <<<
! ----------------------------------------------------------------------------
subroutine set_mesh(type,N)

  ! Arguments
  character(LEN=2) :: type ! UN=uniform, SH=Shishkin,...
  integer(INP)     :: N    ! number of interior mesh points

  current_mesh = mesh(type,N)

  return
end subroutine set_mesh
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE set_mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** function get_mesh <<<
! ----------------------------------------------------------------------------
pure type(mesh) function get_mesh()

  get_mesh = current_mesh

  return
end function get_mesh
! ----------------------------------------------------------------------------
! *** END of function get_mesh >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** function get_mesh <<<
! ----------------------------------------------------------------------------
pure integer(INP) function get_mesh_N()

  get_mesh_N = current_mesh%N

  return
end function get_mesh_N
! ----------------------------------------------------------------------------
! *** END of function get_mesh_N >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE init_meshpoints <<<
! ----------------------------------------------------------------------------
subroutine init_meshpoints()

  call clean_meshpoints()
  allocate(x_save(0:current_mesh%N+1))

  select case (current_mesh%type)
  case ('UN');  call Setup_Mesh_uniform(current_mesh%N)
  case ('SH');  call Setup_Mesh_Shishkin(current_mesh%N)
  case ('BA');  call Setup_Mesh_Bakhvalov(current_mesh%N)
  case default; call STOP_ON_ERROR( 'init_meshpoints (in MESHES)',           &
                                    'Mesh type not supported.'   )
  end select

  ! Set module variables
  mesh_save = current_mesh
  eps_save  = eps
  init_x    = .TRUE.

  return
end subroutine init_meshpoints
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE init_meshpoints >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE force_init_meshpoints <<<
! ***
! *** Not crucial to normal operation of the module, but sometimes one might
! *** wish to force updating of the vector of meshpoints although not
! *** immediately necessary.
! ***
! ----------------------------------------------------------------------------
subroutine force_init_meshpoints()

  call init_meshpoints()

  return
end subroutine force_init_meshpoints
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE force_init_meshpoints >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE clean_meshpoints <<<
! ----------------------------------------------------------------------------
subroutine clean_meshpoints()

  if (ALLOCATED(x_save)) deallocate(x_save)
  init_x       = .FALSE.

  return
end subroutine clean_meshpoints
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE clean_meshpoints >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION mesh_eq <<<
! ----------------------------------------------------------------------------
logical function mesh_eq(msh1,msh2)

  ! arguments
  type(mesh),intent(in) :: msh1, msh2

  mesh_eq = (msh1%N.eq.msh2%N .AND. msh1%type.EQ.msh2%type )

  return
end function mesh_eq
! ----------------------------------------------------------------------------
! *** END of FUNCTION mesh_eq >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION meshpoints_up_to_date <<<
! ----------------------------------------------------------------------------
logical function meshpoints_up_to_date()

  meshpoints_up_to_date = (      init_x                          &
                           .AND. mesh_eq(mesh_save,current_mesh) &
                           .AND. eps.EQ.eps_save                 )

  return
end function meshpoints_up_to_date
! ----------------------------------------------------------------------------
! *** END of FUNCTION meshpoints_up_to_date >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_meshpoint_single <<<
! ----------------------------------------------------------------------------
real(RNP) function get_meshpoint_single(idx)

  ! Arguments
  integer,intent(in) :: idx  !point index

  if (.NOT.meshpoints_up_to_date()) call init_meshpoints()

  get_meshpoint_single = x_save(idx)

  return
end function get_meshpoint_single
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_meshpoint_single >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** FUNCTION get_meshpoint_all <<<
! ----------------------------------------------------------------------------
function get_meshpoint_all() result(x)

  ! Result
  real(RNP),dimension(0:current_mesh%N+1) :: x

  if (.NOT.meshpoints_up_to_date()) call init_meshpoints()

  x = x_save

  return
end function get_meshpoint_all
! ----------------------------------------------------------------------------
! *** END of FUNCTION get_meshpoint_all >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_Mesh_uniform <<<
! ***
! *** Get uniform x on [0,1].
! ----------------------------------------------------------------------------
subroutine Setup_Mesh_uniform(N)

  !arguments
  integer(INP),intent(in) :: N

  !locals
  integer(INP) :: i

  do i=0,N+1
      x_save(i) = real(i,RNP)/real(N+1,RNP)
  enddo

end subroutine Setup_Mesh_uniform
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_Mesh_uniform >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_Mesh_Shishkin <<<
! ***
! ----------------------------------------------------------------------------
subroutine Setup_Mesh_Shishkin(Nx)

  !arguments
  integer(INP),intent(in) :: Nx !no. of strictly interior points

  !locals
  integer(INP) :: N,N1,N2,i,k
  real(RNP)    :: lambda,  &
                  q,       & !percentage of nodes located in the boundary area
                  sqrteps, & !square root of the diffusion parameter
                  sigma,   & !expected convergence rate
                  sqrtrho    !square root of lower bound on C

  sqrteps = sqrt(eps)
  sqrtrho = sqrt(rho)

  q = 0.25_RNP

  sigma = 2.0_RNP

  if(Nx.le.2) &
      call STOP_ON_ERROR('shishkin','Insufficient number of nodes.')
  if(q.ge.0.5_RNP .or. q.le.0.0_RNP) &
      call STOP_ON_ERROR('shishkin','Parameter q needs to fulfil 0<q<1/2.')

  N = Nx+1 !number of subintervals

  !pattern : [0,lambda],[lambda,1-lambda],[1-lambda,1]
  lambda = min(q,sqrteps*sigma/sqrtrho * log(real(N,RNP)))

  !N1: subintervals of each of the boundary areas (left & right)
  N1 = ceiling(q*N,INP)

  N2 = N - 2*N1

  k = 1

  !left boundary
  x_save(0) = 0.0_RNP
  do i=1,N1
      x_save(k) = x_save(k-1) + lambda/N1
      k = k+1
  enddo

  !centre
  do i=1,N2
      x_save(k) = x_save(k-1) + (1.0_RNP-2.0_RNP*lambda)/N2
      k = k+1
  enddo

  !right boundary
  do i=1,N1
      x_save(k) = x_save(k-1) + lambda/N1
      k = k+1
  enddo

end subroutine Setup_Mesh_Shishkin
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_Mesh_Shishkin >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE Setup_Mesh_Bakhvalov <<<
! ***
! *** Returns a graded Bakhvalov mesh as described in
! ***
! *** N. S. Balakhov
! *** Towards optimization of methods for solving boundary value problems
! *** in the presence of boundary layers.
! *** Zh. Vychisl. Math. i Mat. Fiz., 9:841-859,1969, In Russian.
! ***
! *** For further reference see
! ***
! *** Lin{\"s}, Torsten and Madden, Niall
! *** Parameter uniform approximations for time-dependent
! *** reaction-diffusion problems
! ***
! ----------------------------------------------------------------------------
subroutine Setup_Mesh_Bakhvalov(Nx)

  !arguments
  integer(INP),intent(in) :: Nx

  !locals
  integer(INP) :: N1,N2,i
  real(RNP)    :: C,Cfrac,lambda,rest,   &
                  kappa,   &
                  sqrteps, & !sqare root of the diffusion parameter
                  sigma,   & !expected convergence rate
                  sqrtrho    !square root of strict lower bound on C

  sqrteps = sqrt(eps)
  sqrtrho = sqrt(rho)

  kappa = 1.0_RNP

  sigma = 2.0_RNP

  !transition point between layers
  lambda = sqrteps*sigma/sqrtrho * log(kappa/sqrteps)

  if (lambda.le.0.0_RNP) then
      !=======================================================================
      do i=0,Nx+1
          x_save(i) = real(i,RNP)/real(Nx+1,RNP)
      enddo
      !=======================================================================
  elseif (lambda.ge.0.5_RNP) then
      !=======================================================================
      C = 2.0_RNP*sigma*kappa/sqrtrho * (1.0_RNP -exp(-sqrtrho/2.0_RNP/sqrteps/sigma))

      !int_{x_{i-1}}^{x_i} M{BA} = Cfrac
      Cfrac = C / (Nx+1)

      !-----------------------------------------------------
      ! (half of the) area, including the middle point
      N2 = ceiling(real(Nx)/2)
      x_save(0) = 0.0_RNP
      do i=1,N2
          x_save(i) = -sqrteps*sigma/sqrtrho                                 &
                    * log(   exp(-sqrtrho*x_save(i-1)/sqrteps/sigma)         &
                           - Cfrac*sqrtrho/kappa/sigma               )
      enddo
      !-----------------------------------------------------

      !-----------------------------------------------------
      ! do the rest by symmetry
      do i = N2+1,Nx+1
          x_save(i) = 1.0_RNP - x_save(Nx+1-i)
      enddo
      !-----------------------------------------------------
      !=======================================================================
  else
      !=======================================================================
      C = 2.0_RNP*kappa*sigma/sqrtrho * (1.0_RNP -sqrteps/kappa) &
        + 1.0_RNP-2.0_RNP*lambda

      !int_{x_{i-1}}^{x_i} M{BA} = Cfrac
      Cfrac = C / (Nx+1)

      !-----------------------------------------------------
      !boundary layer
      x_save(0) = 0.0_RNP
      !that many nodes fit into the boundary layer:
      N1 = floor( kappa*sigma/sqrtrho*( 1.0_RNP-sqrteps/kappa )/Cfrac )
      do i=1,N1
          x_save(i) = -sqrteps*sigma/sqrtrho                                 &
                    * log(   exp(-sqrtrho*x_save(i-1)/sqrteps/sigma)         &
                           - Cfrac*sqrtrho/kappa/sigma               )
      enddo
      !-----------------------------------------------------

      !-----------------------------------------------------
      ! transition between layers
      ! int_{x_{i}}^{lambda} M_{BA} =
      rest = kappa*sigma/sqrtrho                                             &
           * ( exp(-sqrtrho*x_save(N1)/sqrteps/sigma) - sqrteps/kappa )
      x_save(N1+1) = lambda + (Cfrac-rest)
      !-----------------------------------------------------

      !-----------------------------------------------------
      ! (half of the) inner layer, including the middle point
      N2 = ceiling(real(Nx)/2,INP)
      do i=N1+2,N2
          x_save(i) = x_save(i-1) + Cfrac
      enddo
      !-----------------------------------------------------

      !-----------------------------------------------------
      ! do the rest by symmetry
      do i = N2+1,Nx+1
          x_save(i) = 1.0_RNP - x_save(Nx+1-i)
      enddo
      !-----------------------------------------------------

      !=======================================================================
  endif

end subroutine Setup_Mesh_Bakhvalov
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE Setup_Mesh_Bakhvalov >>>
! ----------------------------------------------------------------------------

end module MESHES
! ============================================================================
! *** END of MODULE MESHES >>>
! ============================================================================