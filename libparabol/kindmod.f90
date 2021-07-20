! ============================================================================
! *** MODULE KINDMOD <<<
! ***
! *** Kind type parameter for integer and real data
! ============================================================================
module KINDMOD

  implicit none

  private

  integer,parameter,public :: INP = 4,                        & !integer number precision
                            & RNP = SELECTED_REAL_KIND(15,7), & !real number precision
                            & CNP = SELECTED_REAL_KIND(12,5)    !complex number precision

  real(RNP),parameter,public :: TOL = 1.0e-15_RNP ,&
                              & PI  = 3.14159265358979323846264338327950_RNP

end module KINDMOD
! ============================================================================
! *** END of MODULE KINDMOD >>>
! ============================================================================