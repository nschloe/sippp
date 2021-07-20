module ERRORMOD

   implicit none

contains

   subroutine STOP_ON_ERROR(routinename, message)
      character(LEN=*), intent(in) :: routinename, message
      write (*, '(//3X,3A)') routinename, ': ', message
      write (*, '(3X,2A/)') routinename, ': Aborting due to fatal error.'
      STOP
   end subroutine STOP_ON_ERROR

   subroutine ISSUE_WARNING(routinename, message)
      character(LEN=*), intent(in) :: routinename, message
      write (*, '(//3X,3A/)') routinename, ': ', message
   end subroutine ISSUE_WARNING

end module ERRORMOD
