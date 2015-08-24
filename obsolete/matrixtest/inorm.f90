! ############################################################
! *** MODULE INORM
! ***
! *** Provides routines that compute (approximate) the maximum
! *** norm of the inverse of a matrix.
! ***
! *** LAPACK functions are used.
! ***
! ############################################################
module inorm

  use kindmod
  use errormod

  implicit none

  interface
     !LU factoring
     SUBROUTINE ZGETRF(M, N, A, LDA, IPIVOT, INFO)
     INTEGER M, N, LDA, INFO
     DOUBLE COMPLEX A(LDA,*)
     INTEGER IPIVOT(*)
     END SUBROUTINE
  end interface

  contains

  ! ==========================================================================
  ! *** FUNCTION Inorm_invert
  ! ***
  ! *** In the following part, the matrix is actually inverted and the norm of
  ! *** the inverse is calculated.
  ! ***
  ! ==========================================================================
  real(RNP) function Inorm_invert(N,A)

    !arguments
    integer                    ,intent(in)    :: N
    complex(CNP),dimension(N,N),intent(inout) :: A

    !locals
    integer   :: info,ldwork,i
    real(RNP) :: s
    complex(CNP),dimension(:)  ,allocatable :: work
    integer     ,dimension(1:N)             :: ipivot

    interface
      !matrix inversion using LU factoring
      SUBROUTINE ZGETRI(N, A, LDA, IPIVOT, WORK, LDWORK, INFO)
      INTEGER N, LDA, LDWORK, INFO
      DOUBLE COMPLEX A(LDA,*), WORK(*)
      INTEGER IPIVOT(*)
      END SUBROUTINE
    end interface

    ! -------------------------------
    !compute LU factoring
    call ZGETRF(N,N,A,N,ipivot,info)
    if(info/=0) STOP 'Failed to call ZGETRF. Abort.'
    ! -------------------------------

    !first do a workspace query
    allocate(work(1))
    LDWORK = -1
    call ZGETRI(N,A,N,ipivot,work,ldwork,info)
    if(info/=0) STOP 'Failed to call ZGETRI (first call). Abort.'

    LDWORK = INT(work(1))
    deallocate(work)

    !invert the man
    allocate(work(ldwork))
    call ZGETRI(N,A,N,ipivot,work,ldwork,info)
    if(info/=0) stop 'Failed to call ZGETRI (second call). Abort.'
    deallocate(work)

    !compute infinity norm of the inverse
    Inorm_invert = 0.0D0
    do i=1,N
      s = SUM(ABS(A(i,:)))
      if (s>Inorm_invert) Inorm_invert=s
    enddo

  end function Inorm_invert
  ! ==========================================================================
  ! *** END FUNCTION Inorm_invert
  ! ==========================================================================

  ! ==========================================================================
  ! *** FUNCTION INorm_condest
  ! ***
  ! *** Estimate the reciprocal of the condition number.
  ! ***
  ! ==========================================================================
  real(RNP) function INorm_condest(N,A)

    !arguments
    integer                       ,intent(in)    :: N
    double complex, dimension(N,N),intent(inout) :: A

    !locals
    real(RNP)        :: s
    double precision :: Anorm,rcond
    integer          :: info,i
    double complex  ,dimension(1:2*N) :: work
    double precision,dimension(1:2*N) :: rwork
    integer         ,dimension(1:N)   :: ipivot

    interface
      !condition number estimation
      SUBROUTINE ZGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, RWORK, INFO )
        CHARACTER          NORM
        INTEGER            INFO, LDA, N
        DOUBLE PRECISION   ANORM, RCOND
        DOUBLE PRECISION   RWORK( * )
        COMPLEX*16         A( LDA, * ), WORK( * )
      END SUBROUTINE
    end interface

    ! -------------------------------
    !compute infinity norm of A
    Anorm = 0.0D0
    do i=1,N
        s = SUM(ABS(A(i,:)))
        if (s>Anorm) Anorm = s
    enddo
    ! -------------------------------

    ! -------------------------------
    !compute LU factoring
    call ZGETRF(N,N,A,N,ipivot,info)
    if(info/=0) STOP 'Failed to call ZGETRF. Abort.'
    ! -------------------------------

    CALL ZGECON('I',N,A,N,Anorm,rcond,work,rwork,info)
    if(info/=0) STOP 'Failed to call ZGECON. Abort.'

    INorm_condest = 1.0D0/rcond/Anorm

  end function INorm_condest
  ! ==========================================================================
  ! *** END FUNCTION INorm_condest
  ! ==========================================================================

end module inorm
! ############################################################
! *** END MODULE INORM
! ############################################################