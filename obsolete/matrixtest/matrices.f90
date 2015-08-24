! ############################################################################
! *** MODULE MATRICES
! ***
! *** Routine to create matrices belonging to the differential
! *** equation
! ***
! *** -eps u'' + c u     + hom. boundary conditions
! ***
! *** for given point distributions.
! ***
! ############################################################################
module matrices

  use kindmod
  use meshes
  use params_generic

  implicit none

  contains

  ! ==========================================================================
  ! *** SUBROUTINE create_1D
  ! ***
  ! *** Given a point distribution, this routine creates the standard
  ! *** discretisation matrix of
  ! ***
  ! ***      -eps*\partial'' + c
  ! ***
  ! *** in one space dimension.
  ! ***
  ! ==========================================================================
  subroutine create_1D(msh,A)

    !arguments
    type(mesh)                       ,intent(in)    :: msh
    real(RNP) ,dimension(msh%N,msh%N),intent(inout) :: A  !system matrix

    !locals
    integer   :: i
    real(RNP) :: h1,h2

    h2 = get_meshpoint(msh,1)-get_meshpoint(msh,0)

    do i=1,msh%N
        h1 = h2
        h2 = get_meshpoint(msh,i+1) - get_meshpoint(msh,i)
        if(i>1)     A(i,i-1) =  - eps* 2.0_RNP/h1/(h1+h2)
                    A(i,i)   =    eps* 2.0_RNP/h1/h2      + c
        if(i<msh%N) A(i,i+1) =  - eps* 2.0_RNP/h2/(h1+h2)
    enddo

  end subroutine create_1D
  ! ==========================================================================
  ! *** END SUBROUTINE create_1D
  ! ==========================================================================

end module matrices
! ############################################################################
! *** END MODULE MATRICES
! ############################################################################