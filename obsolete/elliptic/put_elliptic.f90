! ============================================================================
! *** MODULE PUT_ELLIPTIC <<<
! ***
! *** Provides different types of routines which process the solutions found
! *** by SOLVE_ELLIPTIC and put the results to screen.
! ***
! ============================================================================
module PUT_ELLIPTIC

  use KINDMOD
  use MESHES
  use PARAMS
  use SOLVE_ELLIPTIC

  implicit none

  contains

! ----------------------------------------------------------------------------
! *** SUBROUTINE put_approximation <<<
! ***
! ----------------------------------------------------------------------------
subroutine put_approximation(Nx,x,eps)

  !arguments
  integer(int_kind),                    intent(in) :: Nx !number of strictly inner points
  real(DBL_KIND)   , dimension(0:Nx+1), intent(in) :: x
  real(DBL_KIND)   ,                    intent(in) :: eps

  !DLcals
  integer(int_kind)                    :: i
  real(DBL_KIND)   , dimension(0:Nx+1) :: u


  call solve(Nx,x,eps,u)

  do i=0,Nx+1
      write(*,fmt='(2(F20.15))') x(i),u(i)
  enddo

end subroutine put_approximation
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_approximation >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE put_exact_solution <<<
! ***
! ----------------------------------------------------------------------------
subroutine put_exact_solution(Nx,x,eps)

  !arguments
  integer(int_kind),                    intent(in)  :: Nx !number of strictly inner points
  real(DBL_KIND)   , dimension(0:Nx+1), intent(in)  :: x
  real(DBL_KIND)   ,                    intent(in)  :: eps

  !DLcals
  integer(int_kind)                    :: i

  do i=0,Nx+1
      write(*,fmt='(2(F20.15))') x(i),f(x(i),eps)
  enddo

end subroutine put_exact_solution
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_exact_solution >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** FUNCTION maxerr_exact <<<
! ***
! *** Returns the exact (to computational precision) maximal absolute
! *** error.
! *** Note that a function SOLUTION(x,t) with the exact solution
! *** of the problem needs to be accessible for this routine to work.
! ***
! ----------------------------------------------------------------------------
real(dbl_kind) function maxerr_exact(Nx,x,eps)

  !arguments
  integer(int_kind)                  ,intent(in) :: Nx   !number of strictly interior nodes
  real(dbl_kind)   ,dimension(0:Nx+1),intent(in) :: x
  real(dbl_kind)                     ,intent(in) :: eps

  !locals
  real(dbl_kind)   ,dimension(0:Nx+1) :: u
  real(dbl_kind)                      :: s
  integer(int_kind)                   :: i


  call solve(Nx,x,eps,u,s)

  maxerr_exact = 0.0_DBL_KIND

  !####################################################
  do i=1,Nx
      s = solution(x(i),eps)
      if (abs(u(i)-s).gt.maxerr_exact) maxerr_exact=abs(u(i)-s)
  enddo
  !####################################################

end function maxerr_exact
! ----------------------------------------------------------------------------
! *** END of FUNCTION maxerr_exact >>>
! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE put_maxerr_exact_table <<<
! ***
! *** Puts a LaTeX tabular filled with error values (as retrieved in function
! *** MAXERR_EXACT) to standard out.
! ***
! ----------------------------------------------------------------------------
subroutine put_maxerr_exact_table(N_table,N_steps,eps_table,eps_steps)

  !arguments
  integer(int_kind),dimension(1:),intent(in) :: N_table
  integer(int_kind),              intent(in) :: N_steps
  real(dbl_kind)   ,dimension(1:),intent(in) :: eps_table
  integer(int_kind),              intent(in) :: eps_steps

  !locals
  integer(int_kind)                             :: i,j
  real(dbl_kind),dimension(0:maxval(N_table)+1) :: x
  real(dbl_kind),dimension(1:size(N_table,1))   :: merr

  real(dbl_kind),dimension(1:maxval(N_table)) :: u
  real(dbl_kind)                              :: prec

  write(*,fmt='(A)',ADVANCE='NO') '\begin{tabular}{'
  do j=1,N_steps+1
      write(*,fmt='(A)',ADVANCE='NO') 'l'
  enddo
  write(*,fmt='(A)') '}'
  write(*,fmt='(A)') '\hline'
  write(*,fmt='(A)',ADVANCE='NO') '$\varepsilon$/$N \tau$'
  do i=1,N_steps
      write(*,fmt='(A,I6,A)',ADVANCE='NO') ' & $    N=',N_table(i),' $'
  enddo
  write(*,fmt='(A)') ' \\'
  write(*,fmt='(A)') '\hline'

  do i=1,eps_steps
      write(*,fmt='(ES7.1)',advance='no') eps_table(i)
      !-----------------------------------------------------------------------
      ! do part 1 of the row
      do j=1,N_steps
          call uniform(x(0:N_table(j)+1))
!           call shishkin(N_table(j),x(0:N_table(j)+1),0.25_DBL_KIND,sqrt(eps_table(i)),2.0_DBL_KIND,sqrt(rho))
!           call bakhvalov(x(0:N_table(j)+1),1.0_DBL_KIND,sqrt(eps_table(i)),2.5_DBL_KIND,sqrt(rho))

          merr(j) = maxerr_exact(N_table(j),x(0:N_table(j)+1),eps_table(i))
          write(*,fmt='(A,ES10.4,ES10.4)',advance='no') ' & ',merr(j)
      enddo
      !-----------------------------------------------------------------------
      write(*,fmt='(A)') '   \\'
      write(*,fmt='(4x)',ADVANCE='NO')
      !-----------------------------------------------------------------------
      ! do part 2 of the row: numerical order of convergence
      do j=1,N_steps-1
          write(*,fmt='(A,F7.3)',advance='no') '    & ', log(     merr(j)/ merr(j+1)) &
                                                        /log(real(N_table(j+1),DBL_KIND)/N_table(j))
      enddo
      write(*,fmt='(A)',advance='yes') '     &   ---        \\'
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      ! do part 3 of the row: numerical order of convergence
!       write(*,fmt='(4x)',ADVANCE='NO')
!       do j=1,N_steps
! call solve(N_table(j),x(0:N_table(j)+1),eps_table(i),u,prec)
! write(*,fmt='(A,ES7.1)',advance='no') '    & ', prec
!       enddo
!       write(*,fmt='(A)',advance='yes') '   \\'
      !-----------------------------------------------------------------------
  enddo
  write(*,fmt='(A)') '\hline'
  write(*,fmt='(A)') '\end{tabular}'

end subroutine put_maxerr_exact_table
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_maxerr_exact_table >>>
! ----------------------------------------------------------------------------

end module PUT_ELLIPTIC
! ============================================================================
! *** END of MODULE PUT_ELLIPTIC >>>
! ============================================================================