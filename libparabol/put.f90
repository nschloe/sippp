module PUTMOD

   use KINDMOD
   use intermod
   use MESHES
   use RUNGE_KUTTA

   implicit none

   private

   public :: put_approximation, put_exact_solution_tikz, maxerr_exact, &
             put_maxerr_exact_table, put_exact_solution

contains

   subroutine put_approximation(Nt, tau)

      !arguments
      integer(INP), intent(in) :: Nt
      real(RNP), intent(in) :: tau

      !Locals
      real(RNP), dimension(0:current_mesh%N + 1)  :: x
      integer(INP)                                :: i, j
      real(RNP), dimension(0:current_mesh%N + 1) :: u, z
      real(RNP)                                   :: t

!   write(*,*) current_mesh%N,Nt

      x = get_meshpoint()
      forall (i=0:current_mesh%N + 1) u(i) = u0(x(i))
      z = 0.0_RNP !will contain round-off error

      !####################################################
      !solve step by step and put to standard out
      t = 0.0_RNP

      write (*, '(3(1X,F20.15))') (t, x(i), u(i), i=0, current_mesh%N + 1)

      do j = 1, Nt

         call runge_kutta_step(u(1:current_mesh%N), z(1:current_mesh%N), tau, t)

         t = t + tau
         u(0) = gamma0(t)
         u(current_mesh%N + 1) = gamma1(t)

         write (*, '(3(1X,F20.15))') (t, x(i), u(i), i=0, current_mesh%N + 1)

      end do
      !####################################################

      return
   end subroutine put_approximation

   subroutine put_exact_solution(Nt, tau)

      !arguments
      integer(INP), intent(in) :: Nt
      real(RNP), intent(in) :: tau

      !DLcals
      integer(INP) :: i, j
      real(RNP)    :: t

      t = 0.0_RNP
      do j = 0, Nt
         write (*, fmt='(3(F20.15))') t, get_meshpoint(0), gamma0(t)
         do i = 1, current_mesh%N
            write (*, fmt='(3(F20.15))') t, get_meshpoint(i), get_solution(current_mesh%N, i, t)
         end do
         write (*, fmt='(3(F20.15))') t, get_meshpoint(current_mesh%N + 1), gamma1(t)
         t = t + tau
      end do

   end subroutine put_exact_solution

! ----------------------------------------------------------------------------
! *** SUBROUTINE put_exact_solution_tikz <<<
! ***
! *** Puts the exact solution to standard out in a Tikz friendly way.
! ***
! ----------------------------------------------------------------------------
   subroutine put_exact_solution_tikz(t0)

      !arguments
!   integer(INP),intent(in) :: Nt
      real(RNP), intent(in) :: t0

      !locals
      integer(INP)     :: i

22    FORMAT('  (', F7.4, ',', F7.4, ' )')
23    FORMAT('  (', F7.4, ',', F7.4, ' ) };')

      write (*, '(A)') '\begin{tikzpicture}'
      write (*, '(A)') '\draw plot[smooth] coordinates{'

      write (*, 22) get_meshpoint(0), gamma0(t0)
      do i = 1, current_mesh%N
         write (*, 22) get_meshpoint(i), get_solution(current_mesh%N, i, t0)
      end do
      write (*, 23) get_meshpoint(current_mesh%N + 1), gamma1(t0)

      write (*, '(A)') '\end{tikzpicture}'

   end subroutine put_exact_solution_tikz
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_exact_solution_tikz >>>
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
   real(RNP) function maxerr_exact(Nt, tau)

      !arguments
      integer(INP)         :: Nt
      real(RNP), intent(in) :: tau

      !Locals
      real(RNP), dimension(0:current_mesh%N + 1) :: x
      integer(INP)                               :: i, j
      real(RNP), dimension(0:current_mesh%N + 1) :: u, z
      real(RNP)                                  :: t, s

      Nt = CEILING(get_T1()/tau)

      maxerr_exact = 0.0_RNP

      x = get_meshpoint()
      forall (i=0:current_mesh%N + 1) u(i) = u0(x(i))

      z = 0.0_RNP !will contain round-off error

      !####################################################
      !solve step by step and look out for maxerr
      t = 0.0_RNP
      do j = 1, Nt

         call runge_kutta_step(u(1:current_mesh%N), z(1:current_mesh%N), tau, t)
         t = t + tau
         u(0) = gamma0(t)
         u(current_mesh%N + 1) = gamma1(t)

         s = maxval(abs(u - get_solution(current_mesh%N, t)))
         if (s .gt. maxerr_exact) maxerr_exact = s
      end do
      !####################################################

      return
   end FUNCTION maxerr_exact
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
   subroutine put_maxerr_exact_table(N_steps, N_table, K_table, eps_steps, &
                                     eps_table, mesh_type)

      !arguments
      integer(INP), intent(in) :: N_steps, eps_steps
      integer(INP), dimension(1:N_steps), intent(in) :: N_table, K_table
      real(RNP), dimension(1:eps_steps), intent(in) :: eps_table
      character(LEN=2)                               :: mesh_type

      !locals
      integer(INP)                   :: i, j
      real(RNP), dimension(1:N_steps) :: merr

      write (*, fmt='(A/A\)') '\begin{table}\centering{\tt', '\begin{tabular}{'

      write (*, '(A\)') ('l', i=1, N_steps + 2)

      write (*, '(A/A\)') '} \toprule', &
         '$\varepsilon$'

      write (*, '(" &    $N=",I6,"$"\)') (N_table(i), i=1, N_steps)
      write (*, '(" &",28X,A)') '\\\midrule'

      do i = 1, eps_steps

         !reset eps in params_generic
         call set_eps(eps_table(i))

         write (*, '(ES7.1,"       "\)') eps_table(i)
         !-----------------------------------------------------------------------
         ! do part 1 of the row
         do j = 1, N_steps
            !reset mesh
            call set_mesh(mesh_type, N_table(j))

            !get error
            merr(j) = maxerr_exact(K_table(j), get_T1()/K_table(j))

            write (*, '("&",4X,ES9.3,"  "\)') merr(j)
         end do
         !-----------------------------------------------------------------------
         write (*, '(A/A\)') '& $\eta_{\varepsilon}^{N,K}$ \\', "              "
         !-----------------------------------------------------------------------
         ! do part 2 of the row: numerical order of convergence
         write (*, '("&  ",F7.3,"      "\)') (log(merr(j)/merr(j + 1)) &
                                              /log(real(N_table(j + 1), RNP)/N_table(j)), &
                                              j=1, N_steps - 1)
         write (*, '(A)') '&    ---        & $r_{\varepsilon}^{N,K}$    \\'
         !-----------------------------------------------------------------------
      end do
      write (*, '(A/A/A/A)') '\bottomrule', '\end{tabular}}', &
         '\caption{captiontext}', '\end{table}'

   end subroutine put_maxerr_exact_table
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_maxerr_exact_table >>>
! ----------------------------------------------------------------------------

end module PUTMOD
! ============================================================================
! *** END of MODULE PUT_PARABOL >>>
! ============================================================================
! ! ***
! ! *** Returns the exact (to computational precision) maximal absolute
! ! *** error.
! ! *** Note that a function SOLUTION(x,t) with the exact solution
! ! *** of the problem needs to be accessible for this routine to work.
! ! ***
! ! ----------------------------------------------------------------------------
! real(RNP) function maxerr_approx(Nt,tau,method1,method2)
!
!   !arguments
!   integer(INP)                :: Nt
!   real(RNP)       ,intent(in) :: tau
!   character(LEN=2),intent(in) :: method1,method2 !method1 is to be evaluated,
!                                                  !method2 helps with error bounds
!
!   !Locals
!   real(RNP)   ,dimension(0:current_mesh%N+1) :: x
!   integer(INP)                               :: i,j
!   real(RNP)   ,dimension(0:current_mesh%N+1) :: u1,u2,z1,z2,error
!   real(RNP)                                  :: t,s
!
!   Nt = CEILING(T1/tau)
!
!   maxerr_exact = 0.0_RNP
!
!   x = get_meshpoint()
!   forall(i=0:current_mesh%N+1) u1(i) = u0(x(i))
!   u2 = u1
!
!   z1 = 0.0_RNP !will contain round-off error
!   z2 = 0.0_RNP
!
!   !####################################################
!   !solve step by step and look out for maxerr
!   t = 0.0_RNP
!   do j=1,Nt
!
!       call runge_kutta_step(u(1:current_mesh%N),z1(1:current_mesh%N),tau,t)
!       t = t + tau
! !       u(0)                = gamma0(t)
! !       u(current_mesh%N+1) = gamma1(t)
!
!       s = maxval(abs( u2 - u1 ))
!       if (s.gt.maxerr_exact) maxerr_exact=s
!   enddo
!   !####################################################
!
!   return
! end FUNCTION maxerr_approx
! ! ----------------------------------------------------------------------------
! ! *** END of FUNCTION maxerr_approx >>>
! ! ----------------------------------------------------------------------------

! ! ----------------------------------------------------------------------------
! ! *** SUBROUTINE put_maxerr_approx_table <<<
! ! ***
! ! *** Puts a LaTeX tabular filled with error values (as retrieved in function
! ! *** MAXERR_APPROX) to standard out.
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine put_maxerr_approx_table(N0,tau0,N_steps,N_mult,tau_mult,eps0,eps_steps,eps_mult,T,x,x2)
!
!   !arguments
!   integer(INP)              ,intent(in) :: N0,N_steps,eps_steps
!   integer(INP)              ,intent(in) :: N_mult,tau_mult,eps_mult
!   real(RNP)                 ,intent(in) :: tau0,eps0,T
!   real(RNP)   ,dimension(0:),intent(in) :: x,x2
!
!   !locals
!   integer(INP)                   :: i,j
!   integer(INP)                   :: Nx,Nt,Nt0
!   real(RNP)                      :: tau,eps,log2
!   real(RNP),dimension(1:N_steps) :: merr
!
!   log2 = log(2.0_RNP)
!
!   Nt0 = int(T/tau0,INP)
!
!   write(*,fmt='(A)',ADVANCE='NO') '\begin{tabular*}{\textwidth}{'
!   do j=1,N_steps+1
!       write(*,fmt='(A)',ADVANCE='NO') 'l'
!   enddo
!   write(*,fmt='(A)') '}'
!   write(*,fmt='(A)') '\hline'
!   write(*,fmt='(A)',ADVANCE='NO') '$\varepsilon$/$N \tau$'
!   Nx = N0
!   do i=1,N_steps
!       write(*,fmt='(A,I4,A)',ADVANCE='NO') ' & $N=',Nx,'         $'
!       Nx = N_mult *Nx
!   enddo
!   write(*,fmt='(A)') ' \\'
!
!   write(*,fmt='(22x)',ADVANCE='NO')
!   do j=1,N_steps
!       write(*,fmt='(A,F3.1,A,I1,A,I4,A)',ADVANCE='NO') &
!                                    & ' & $\tau=',tau0,'/',tau_mult,'^',j-1,'$'
!   enddo
!   write(*,fmt='(A)') ' \\'
!   write(*,fmt='(A)') '\hline'
!
!   eps = eps0
!   do i=1,eps_steps
!       write(*,fmt='(F5.3,A,I1,A,I2,A)',advance='no') &
!                                      & eps0,' $\cdot ',eps_mult,'^{-',i-1,'}$'
!       !-----------------------------------------------------------------------
!       ! do part 1 of the row
!       Nx  = N0
!       tau = tau0
!       Nt  = Nt0
!       do j=1,N_steps
!           merr(j) = maxerr_approx(x,x2,Nt,tau,eps)
!           write(*,fmt='(A,ES10.4,ES10.4)',advance='no') ' & ',merr(j)
!           Nx  = N_mult*Nx
!           tau = tau/real(tau_mult,RNP)
!           Nt  = tau_mult*Nt
!       enddo
!       !-----------------------------------------------------------------------
!       write(*,fmt='(A)') ' \\'
!       write(*,fmt='(17x)',ADVANCE='NO')
!       !-----------------------------------------------------------------------
!       ! do part 2 of the row: numerical order of convergence
!       do j=1,N_steps-1
!           write(*,fmt='(A,F6.4)',advance='no') '     & ', log(merr(j)/merr(j+1))/log2
!       enddo
!       write(*,fmt='(A)',advance='yes') '     & \\'
!       !-----------------------------------------------------------------------
!       eps = eps/real(eps_mult,RNP)
!   enddo
!   write(*,fmt='(A)') '\hline'
!   write(*,fmt='(A)') '\end{tabular*}'
!
! end subroutine put_maxerr_approx_table
! ! ----------------------------------------------------------------------------
! ! *** END of SUBROUTINE put_maxerr_approx_table >>>
! ! ----------------------------------------------------------------------------

! ----------------------------------------------------------------------------
! *** SUBROUTINE put_maxerr_exact_table <<<
! ***
! *** Puts a LaTeX tabular filled with error values (as retrieved in function
! *** MAXERR_EXACT) to standard out.
! ***
! ----------------------------------------------------------------------------
subroutine put_maxerr_exact_table(N_steps, N_table, K_table, eps_steps, &
                                  eps_table, mesh_type)

   !arguments
   integer(INP), intent(in) :: N_steps, eps_steps
   integer(INP), dimension(1:N_steps), intent(in) :: N_table, K_table
   real(RNP), dimension(1:eps_steps), intent(in) :: eps_table
   character(LEN=2)                               :: mesh_type

   !locals
   integer(INP)                             :: i, j, K
   real(RNP), dimension(0:maxval(N_table) + 1) :: x
   real(RNP), dimension(1:N_steps)           :: merr

!   real(RNP),dimension(1:maxval(N_table)) :: u

   write (*, fmt='(A/A\)') '\begin{table}\centering{\tt', '\begin{tabular}{'

   write (*, '(A\)') ('l', i=1, N_steps + 2)

   write (*, '(A/A\)') '} \toprule', &
      '$\varepsilon$'

   write (*, '(" &    $N=",I6,"$"\)') (N_table(i), i=1, N_steps)
   write (*, '(" &",28X,A)') '\\\midrule'

   do i = 1, eps_steps

      !reset eps in params_generic
      eps = eps_table(i)

      write (*, '(ES7.1,"       "\)') eps_table(i)
      !-----------------------------------------------------------------------
      ! do part 1 of the row
      do j = 1, N_steps
         !reset mesh
         call set_mesh(mesh_type, N_table(j))

         !get error
         merr(j) = maxerr_exact(K_table(j), T1/K_table(j))

         write (*, '("&",4X,ES9.3,"  "\)') merr(j)
      end do
      !-----------------------------------------------------------------------
      write (*, '(A/A\)') '& $\eta_{\varepsilon}^{N,K}$ \\', "              "
      !-----------------------------------------------------------------------
      ! do part 2 of the row: numerical order of convergence
      write (*, '("&  ",F7.3,"      "\)') (log(merr(j)/merr(j + 1)) &
                                           /log(real(K_table(j + 1), RNP)/K_table(j)), &
                                           j=1, N_steps - 1)
      write (*, '(A)') '&    ---        & $r_{\varepsilon}^{N,K}$    \\'
      !-----------------------------------------------------------------------
   end do
   write (*, '(A/A/A/A)') '\bottomrule', '\end{tabular}}', &
      '\caption{captiontext}', '\end{table}'

end subroutine put_maxerr_exact_table
! ----------------------------------------------------------------------------
! *** END of SUBROUTINE put_maxerr_exact_table >>>
! ----------------------------------------------------------------------------

! ! ----------------------------------------------------------------------------
! ! *** SUBROUTINE put_maxerr_exact_table_generic_format <<<
! ! ***
! ! *** Puts a LaTeX tabular filled with error values (as retrieved in function
! ! *** MAXERR_EXACT) to standard out.
! ! ***
! ! ----------------------------------------------------------------------------
! subroutine put_maxerr_exact_table_generic_format(Nx_steps,Nx_table,tau_table,eps_steps,eps_table)
!
!   !argumeKs
!   iKeger(INP),                       iKeK(in) :: Nx_steps,eps_steps
!   iKeger(INP),dimension(1:Nx_steps) ,iKeK(in) :: Nx_table
!   real(RNP)   ,dimension(1:eps_steps),iKeK(in) :: tau_table,eps_table
!
!   !locals
!   iKeger(INP)                              :: i,j,K
!   real(RNP),dimension(0:maxval(Nx_table)+1) :: x
!   real(RNP),dimension(1:Nx_steps)           :: merr
!
! !   real(RNP),dimension(1:maxval(Nx_table)) :: u
!
!   do i=1,eps_steps
!       write(*,fmt='(ES7.1)') eps_table(i)
!       do j=1,Nx_steps
! !           call uniform(x(0:Nx_table(j)+1))
! !           call shishkin(Nx_table(j),x(0:Nx_table(j)+1),0.25_RNP,sqrt(eps_table(i)),2.0_RNP,sqrt(rho))
! !           call bakhvalov(x(0:Nx_table(j)+1),1.0_RNP,sqrt(eps_table(i)),2.5_RNP,sqrt(rho))
!           K = iK(T1/tau_table(j))
!           merr(j) = maxerr_exact(Nx_table(j),K,tau_table(j))
!           write(*,fmt='(2(2x,ES20.14))') tau_table(j),merr(j)
!       enddo
!       write(*,*)
!   enddo
!
! end subroutine put_maxerr_exact_table_generic_format

end module PUTMOD
