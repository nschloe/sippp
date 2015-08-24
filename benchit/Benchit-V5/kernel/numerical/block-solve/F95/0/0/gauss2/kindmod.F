! ============================================================================
! *** MODULE C_Kinds
! ***
! *** Kind type parameter for integer and real data as used in C
! ============================================================================
module KINDMOD

  implicit none

  private

  integer,parameter,public :: INP = 4           ! normal integer
  integer,parameter,public :: RNP = KIND(1.0D0) ! double precision real
 
  real(RNP),parameter,public :: TOL = 1.0e-15_RNP

end module KINDMOD
! ============================================================================
! *** END of MODULE C_Kinds
! ============================================================================
