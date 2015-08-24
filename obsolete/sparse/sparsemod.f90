! ============================================================================
! *** MODULE SPARSEMOD <<<
! ***
! *** Fortran95 implementation of a sparse matrix type (CSR)
! *** with commonly used routines.
! ***
! *** The routines are programmed such that only the upper right of the matrix
! *** is stored and processed.
! ***
! ============================================================================
module SPARSEMOD

USE KINDMOD
USE ERRORMOD

implicit none

!--------------------------------------------------------------
! Sparse matrix type: MSR storage technique using the symmetry
! (only storing the upper right triangle row-wise)
type SPARSE
    integer(INP)                          :: N
    real(RNP)   ,dimension(:),allocatable :: DIAG, OFFDIAG
    integer(INP),dimension(:),allocatable :: COLUMN,ROWSTART
end type SPARSE
!--------------------------------------------------------------

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE INIT <<<
! ----------------------------------------------------------------------------
subroutine INIT(A,N,NNZ)
  !arguments
  type(SPARSE),intent(out) :: A
  integer(INP),intent(in)  :: N,NNZ !dimension, non-zero elements without diagonal elements

  if(allocated(A%DIAG).or.allocated(A%OFFDIAG).or.allocated(A%COLUMN).or.allocated(A%ROWSTART)) &
& call STOP_ON_ERROR('INIT (in SPARSEMOD)','A already allocated (partly).')

  allocate(A%DIAG(1:N))
  allocate(A%OFFDIAG(1:NNZ))
  allocate(A%COLUMN(1:NNZ))
  allocate(A%ROWSTART(1:N))

  A%N = N

  A%DIAG    = 0.0_RNP
  A%OFFDIAG = 0.0_RNP
  A%COLUMN   = 0_INP
  A%ROWSTART = 0_INP
end subroutine INIT
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE INIT >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE INIT_FRAME <<<
! ***
! *** Initialize the matrix such that the matrix contains zeros exept for the
! *** first and last rows and lines, respectively, where 1s are to be found.
! *** In that manner, the 1s "frame" the matrix.
! ----------------------------------------------------------------------------
subroutine INIT_FRAME(A,N)
  !arguments
  type(SPARSE),intent(out) :: A
  integer(INP),intent(in)  :: N !dimension

  !locals
  integer(INP) :: i
  integer(INP) :: NNZ !number of non-zero elements without diagonal elements (for internal usage)

  if(allocated(A%DIAG).or.allocated(A%OFFDIAG).or.allocated(A%COLUMN).or.allocated(A%ROWSTART)) &
& call STOP_ON_ERROR('INIT_FRAME (in SPARSEMOD)','A already allocated (partly).')

  A%N = N

  NNZ = 2*N-3

  !initiate A%DIAG
  allocate(A%DIAG(1:N))
  A%DIAG(1)     = 1.0_RNP
  A%DIAG(2:N-1) = 0.0_RNP
  A%DIAG(N)     = 1.0_RNP

  !initiate A%ROWSTART; get NNZ
  allocate(A%ROWSTART(1:N))
  A%ROWSTART(1) = 1
  do i=2,N
      A%ROWSTART(i) = N-2+i
  enddo

  !initiate A%OFFDIAG
  allocate(A%OFFDIAG(1:NNZ))
  A%OFFDIAG = 1.0_RNP

  !initiate A%COLUMN
  allocate(A%COLUMN(1:NNZ))
  do i=1,N-1
      A%COLUMN(i) = i+1
  enddo
  A%COLUMN(N:NNZ) = N

end subroutine INIT_FRAME
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE INIT_FRAME >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE INIT_RANDOM <<<
! ----------------------------------------------------------------------------
subroutine INIT_RANDOM(A,N,NNZ_out)
  !arguments
  type(SPARSE)     ,intent(out)          :: A
  integer(INP),intent(in)           :: N       !dimension
  integer(INP),intent(out),optional :: NNZ_out !OPTIONAL: number of non-zero elements without diagonal elements

  !locals
  integer(INP) :: i,k
  integer(INP) :: NNZ !number of non-zero elements without diagonal elements (for internal usage)

  if(allocated(A%DIAG).or.allocated(A%OFFDIAG).or.allocated(A%COLUMN).or.allocated(A%ROWSTART)) &
& call STOP_ON_ERROR('INIT_RANDOM (in SPARSEMOD)','A already allocated (partly).')

  A%N = N

  !initiate A%DIAG
  allocate(A%DIAG(1:N))
  call RANDOM_NUMBER(A%DIAG)

  !initiate A%ROWSTART; get NNZ
  allocate(A%ROWSTART(1:N))
  A%ROWSTART(1) = 1
  NNZ = 0
  do i=2,N
      call RANDOM_INT(0_INP,N-i,k)
      A%ROWSTART(i) = A%ROWSTART(i-1) + k
      NNZ = NNZ + k
  enddo

  !initiate A%OFFDIAG
  allocate(A%OFFDIAG(1:NNZ))
  call RANDOM_NUMBER(A%OFFDIAG)

  !initiate A%COLUMN
  allocate(A%COLUMN(1:NNZ))
  do i=1,N-1
      call RANDOM_INT_VEC(i+1_INP,N,A%COLUMN(A%ROWSTART(i):A%ROWSTART(i+1)-1))
  enddo

  if (present(NNZ_out)) NNZ_out = NNZ

end subroutine INIT_RANDOM
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE INIT_RANDOM >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE RANDOM_INT <<<
! ***
! *** Returns a random integer value between the positive integers A and B
! ----------------------------------------------------------------------------
subroutine RANDOM_INT(A,B,RND)
  !arguments
  integer(INP),intent(in)  :: A,B
  integer(INP),intent(out) :: RND

  !locals
  real(RNP) :: temp

  if(A.gt.B) call STOP_ON_ERROR('RANDOM_INT (in SPARSEMOD)','A > B.')

  call RANDOM_NUMBER(temp)
  RND = FLOOR(temp*(B-A+1) + A)

end subroutine RANDOM_INT
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE RANDOM_INT >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE RANDOM_INT_VEC <<<
! ***
! *** Returns a vector filled with random integer values between the positive
! *** integers A and B. The vector values are all different from each other.
! *** The vector is ordered.
! ----------------------------------------------------------------------------
subroutine RANDOM_INT_VEC(A,B,RNDVEC)
  !arguments
  integer(INP)             ,intent(in)  :: A,B
  integer(INP),dimension(:),intent(out) :: RNDVEC

  !locals
  integer(INP)               :: i,N,modN,temp,k
  logical,dimension(:),allocatable :: mask

  N = size(RNDVEC,1)
  modN = B-A+1

  if(N.gt.modN) &
& call STOP_ON_ERROR('RANDOM_INT_VEC (in SPARSEMOD)','Vector size too large.')

  allocate(mask(1:modN))
  mask = .FALSE.

  do i=1,N
      do
          call RANDOM_INT(1_INP,modN,temp)
          if (.not.mask(temp)) exit
      enddo
      mask(temp) = .TRUE.
  enddo

  k=1
  do i=1,modN
      if (mask(i)) then
          RNDVEC(k) = A+i-1
          k = k+1
      endif
  enddo

  if(allocated(mask)) deallocate(mask)

end subroutine RANDOM_INT_VEC
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE RANDOM_INT_VEC >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE CLEAN <<<
! ----------------------------------------------------------------------------
subroutine CLEAN(A)
  !arguments
  type(SPARSE),intent(inout) :: A

  A%N = 0

  if(allocated(A%DIAG))     deallocate(A%DIAG)
  if(allocated(A%OFFDIAG))  deallocate(A%OFFDIAG)
  if(allocated(A%COLUMN))   deallocate(A%COLUMN)
  if(allocated(A%ROWSTART)) deallocate(A%ROWSTART)
end subroutine CLEAN
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE CLEAN >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE PUT <<<
! ***
! *** puts upper triangular part of A to standard output
! ----------------------------------------------------------------------------
subroutine PUT(A)
  !arguments
  type(SPARSE),intent(in) :: A

  !local variables
  integer(INP)     :: i,j,k,nextrowstart
  character(LEN=9) :: FORMATDBL,FORMATX,FORMAT0

  FORMATDBL = '(2X,F6.3)'
  FORMATX   = '(8X)'
  FORMAT0   = '(7X,I1)'

  write(*,*)

  nextrowstart = A%ROWSTART(1)
  do i=1,A%N-1

      !Show nothing for lower triangular part
      do j=1,i-1
          write(*,FMT=FORMATX,ADVANCE='NO')
      enddo

      !Show main diagonal element
      write(*,FMT=FORMATDBL,ADVANCE='NO') A%DIAG(i)

      !Show upper triangular part
      k = nextrowstart
      j = i+1
      nextrowstart = A%ROWSTART(i+1)
      do while (k.lt.nextrowstart)
          do while (j.lt.A%COLUMN(k))
              write(*,FMT=FORMAT0,ADVANCE='NO') 0
              j=j+1
          enddo
          write(*,FMT=FORMATDBL,ADVANCE='NO') A%OFFDIAG(k)
          k=k+1
          j=j+1
      enddo
      do while (j.le.A%N)
          write(*,FMT=FORMAT0,ADVANCE='NO') 0
          j=j+1
      enddo

      write(*,*)
  enddo

  !do last row
  do j=1,A%N-1
      write(*,FMT=FORMATX,ADVANCE='NO')
  enddo
  write(*,FMT=FORMATDBL,ADVANCE='NO') A%DIAG(A%N)
  write(*,*)

end subroutine PUT
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE PUT >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE PUT2EPS <<<
! ***
! *** Puts the nonzero pattern of the matrix A to an Encapsulated PostScript
! *** file.
! ----------------------------------------------------------------------------
subroutine PUT2EPS(A,filename)

  !arguments
  type(SPARSE)    ,intent(in) :: A
  character(LEN=*),intent(in) :: filename

  !local variables
  integer(INP) :: i,j,k,N , nextrowstart
  real(RNP)    :: tol

  tol = 1e-10_RNP

  N = A%N

  open(unit=30,status='REPLACE',file=filename)

  !write EPS header
  write(30,FMT='(A/,A,2(I5,1X)/)') '%!PS-Adobe-3.0 EPSF-3.0',&
                                 & '%%BoundingBox: 1 1 ', A%N+1, A%N+1

  !define box procedure
  write(30,FMT='(7(A/))') '% ----- Define box procedure -----',&
                        & '/box', &
                        & '{  0 1 rlineto',    &
                        & '   1 0 rlineto',    &
                        & '   0 -1 rlineto',   &
                        & '  closepath } def', &
                        & '% --------------------------------'

  nextrowstart = A%ROWSTART(1)
  do i=1,A%N-1

      if (A%DIAG(i)>tol) write(30,FMT='(2I4,A)')  A%N-i+1,i,' moveto box fill'

      !Show upper triangular part
      k = nextrowstart
      j = i+1
      nextrowstart = A%ROWSTART(i+1)
      do while (k.lt.nextrowstart)
          do while (j.lt.A%COLUMN(k))
              j=j+1
          enddo
          write(30,FMT='(2I4,A/,2I4,A)') A%N-i+1,j,' moveto box fill', &
                                       & A%N-j+1,i,' moveto box fill'
          k=k+1
          j=j+1
      enddo
  enddo

  if (A%DIAG(A%N)>tol) write(30,FMT='(2I4,A)')  1,A%N,' moveto box fill'

end subroutine PUT2EPS
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE PUT2EPS >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE BUILD_FROM_CONNECTLIST <<<
! ***
! *** Builds up A%ROWSTART and A%COLUMN for a sparse type matrix for FEM
! *** equation system using the connection information provided by a node
! *** descendants list obtained in the last refining step.
! ----------------------------------------------------------------------------
subroutine BUILD_FROM_CONNECTLIST(A,connections,numnodes)

  !arguments
  type(SPARSE)               ,intent(out) :: A
  integer(INP),dimension(:,:),intent(in)  :: connections
  integer(INP)               ,intent(in)  :: numnodes

  !local variables
  integer(INP)                            :: i,j,NNZ,k,kNext,num
  integer(INP),dimension(:)  ,allocatable :: num_gt_connections
  logical     ,dimension(:,:),allocatable :: mask

!Some FORTRAN compilers complain about A%DIAG and friends being accessed without prior allocation here
!
!   if(allocated(A%DIAG).or.allocated(A%OFFDIAG).or.allocated(A%COLUMN).or.allocated(A%ROWSTART)) &
! & call STOP_ON_ERROR('BUILD_FROM_CHILDLIST (in SPARSEMOD)','A already allocated.')

  A%N = numnodes

  allocate(A%DIAG(1:numnodes)); A%DIAG=0.0

  !this vector will store the number of connections with index > i
  allocate(num_gt_connections(1:numnodes)); num_gt_connections=0

  !MASK will store where these nodes are
  allocate(mask(1:size(connections,1)-1,1:numnodes)); mask = .FALSE.

  allocate(A%ROWSTART(1:numnodes))

  NNZ = 0
  do i=1,numnodes
      A%ROWSTART(i) = NNZ+1
      do j=2,connections(1,i) !connections(1,:) stores the position of the last neighbor
          if(connections(j,i).gt.i) then !only count neighbors with index >i
              num_gt_connections(i) = num_gt_connections(i) + 1 !build up NUM_GT_CONNECTIONS and MASK for later
              mask(j-1,i) = .TRUE.
          endif
      enddo
      NNZ = NNZ + num_gt_connections(i)
  enddo

  allocate(A%OFFDIAG(1:NNZ)); A%OFFDIAG=0.0

  allocate(A%COLUMN(1:NNZ))
  k = 1
  do i=1,numnodes
      num = num_gt_connections(i)
      !sort the vector of connections (repr. columns is matrix) and write sorted list to A%COLUMN
      kNext = k+num
      call FILTER_AND_SORT( connections(2:,i) , mask(:,i) , A%COLUMN(k:kNext-1))
      k = kNext
  enddo

  if(allocated(num_gt_connections)) deallocate(num_gt_connections)
  if(allocated(mask))               deallocate(mask)

end subroutine BUILD_FROM_CONNECTLIST
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE BUILD_FROM_CONNECTLIST >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE FILTER_AND_SORT <<<
! ***
! *** Sorts the first N elements of TWISTED which are not masked by
! *** MASK into SORTED. N = SIZE(SORTED).
! ----------------------------------------------------------------------------
subroutine FILTER_AND_SORT(twisted,mask,sorted)

  !arguments
  integer(INP),dimension(:),intent(in)    :: twisted
  integer(INP),dimension(:),intent(out)   :: sorted
  logical     ,dimension(:),intent(inout) :: mask

  !local variables
  integer(INP) :: N,j,k

  N = size(sorted)

  if (size(twisted).ne.size(mask)) &
& call STOP_ON_ERROR('FILTER_AND_SORT (in SPARSEMOD)','Input vector sizes do not match.')

  do j=1,N
      k = MINLOC(twisted,1,mask)
      sorted(j) = twisted(k)
      mask(k) = .FALSE.
  enddo

end subroutine FILTER_AND_SORT
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE FILTER_AND_SORT >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE ADD_TO_ELEM <<<
! ***
! *** Adds DATA at (i,j) in given matrix A. A needs to be fully allocated,
! *** A%ROWSTART and A%COLUMN must be set up already. I<=J needs to be true
! *** for this routine to do something: A only stores right upper triangle!
! ----------------------------------------------------------------------------
subroutine ADD_TO_ELEM(A,i,j,data)

  !arguments
  type(SPARSE),intent(inout) :: A
  integer(INP),intent(in)    :: i,j
  real(RNP)   ,intent(in)    :: data

  !local variables
  integer(INP) :: k


  if( .not.allocated(A%DIAG)    .or. &
      .not.allocated(A%OFFDIAG) .or. &
      .not.allocated(A%COLUMN)  .or. &
      .not.allocated(A%ROWSTART)       ) &
 call STOP_ON_ERROR('ADD_TO_ELEM (in SPARSEMOD)','A not allocated.')

  if(i>j) return

  if(i.eq.j) then
      A%DIAG(i) = A%DIAG(i) + data
      return
  endif

  do k=A%ROWSTART(i),A%ROWSTART(i+1)-1
      if (A%COLUMN(k).eq.j) then
          A%OFFDIAG(k) = A%OFFDIAG(k) + data
          return
      endif
  enddo

  call STOP_ON_ERROR('ADD_TO_ELEM (in SPARSEMOD)','Could not add element.')

end subroutine ADD_TO_ELEM
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE ADD_TO_ELEM >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE MATVEC <<<
! ***
! *** matrix vector multiplication
! ----------------------------------------------------------------------------
subroutine MATVEC(A,u,z)

  !arguments
  type(SPARSE)               ,intent(in)  :: A
  real(RNP),dimension(:),intent(in)  :: u
  real(RNP),dimension(:),intent(out) :: z

  !local variables
  integer(INP) :: i,j


  !check sizes
  if ( (size(z)/=A%N).or.(size(u)/=A%N) ) &
& call STOP_ON_ERROR('MATVEC (in SPARSEMOD)','Vector/matrix sizes do not match.')

  !do diagonal part
  do i=1,A%N
      z(i) = A%DIAG(i) * u(i)
  enddo

  do i=1,A%N-1
      do j=A%ROWSTART(i),A%ROWSTART(i+1)-1
          !do lower triangular part
          z(A%COLUMN(j)) = z(A%COLUMN(j)) + A%OFFDIAG(j) * u(i)
          !do upper triangular part
          z(i) = z(i) + A%OFFDIAG(j) * u(A%COLUMN(j))
      enddo
  enddo

end subroutine MATVEC
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE MATVEC >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE NORM1 <<<
! ***
! *** Returns 1-vector norm
! ----------------------------------------------------------------------------
subroutine NORM1(v,norm)
  !arguments
  real(RNP),dimension(:),intent(in)  :: v
  real(RNP)             ,intent(out) :: norm
  !local variables
  integer(INP) :: i

  norm = 0.0
  do i=1,size(v)
      norm = norm + abs(v(i))
  enddo

end subroutine NORM1
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE NORM1 >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE NORMINF <<<
! ***
! *** Returns maximum vector norm
! ----------------------------------------------------------------------------
subroutine NORMINF(v,norm)
  !arguments
  real(RNP),dimension(:),intent(in)  :: v
  real(RNP)             ,intent(out) :: norm

  norm = maxval(abs(v),1)

end subroutine NORMINF
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE NORMINF >>>
! ----------------------------------------------------------------------------


end module SPARSEMOD
! ============================================================================
! *** END of MODULE SPARSEMOD >>>
! ============================================================================
