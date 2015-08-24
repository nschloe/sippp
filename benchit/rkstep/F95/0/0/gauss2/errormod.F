! ============================================================================
! *** MODULE ERRORMOD <<<
! ***
! *** Unified warning messages.
! ============================================================================
module ERRORMOD

implicit none

contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE STOP_ON_ERROR <<<
! ----------------------------------------------------------------------------
subroutine STOP_ON_ERROR(routinename,message)
  character(LEN=*),intent(in) :: routinename,message

  write (*,'(//3X,3A)') routinename,': ',message
  write (*,'(3X,2A/)')  routinename,': Aborting due to fatal error.'
  STOP

end subroutine STOP_ON_ERROR
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE STOP_ON_ERROR >>>
! ----------------------------------------------------------------------------


! ----------------------------------------------------------------------------
! *** SUBROUTINE SHOW_WARNING <<<
! ----------------------------------------------------------------------------
subroutine SHOW_WARNING(routinename,message)
  character(LEN=*),intent(in) :: routinename,message

  write (*,'(//3X,3A/)') routinename,': ',message

end subroutine SHOW_WARNING
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE SHOW_WARNING >>>
! ----------------------------------------------------------------------------


end module ERRORMOD
! ============================================================================
! *** END of MODULE ERRORMOD >>>
! ============================================================================