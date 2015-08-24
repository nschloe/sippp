      subroutine tstcas( n, m, itype, a, b, c)
      Integer n, m, itype
      Real*8 a( n, n ), b( n, n ), c( n, n ), temp

      Integer i11 ,j1 ,j2 , i, j

C     **** Initialization  
      i11 = 0
      j1 = 0
      j2 = 0
      i = 0
      j = 0

      if ( itype.eq.1 ) then 
         Do i11 = 1, m
            do j = 1, n
               do i = 1, n
                  do k = 1, n
                     c( i, j ) = c( i, j ) + a( i, k ) * b( k ,j )
                  enddo
               end do
            end do
         enddo   
      else
         if ( itype.eq.2 ) then 
            do i11 = 1, m
               do j = 1, n
                  do i = 1, n
                     temp = c( i, j )
                     do k = 1, n
                        temp = temp + a( i, k ) * b( k, j )
                     enddo
                     c( i, j ) = temp
                  enddo  
               end do
            end do
         elseif ( itype.eq.3 ) then 
            do i11 = 1, m
               do j = 1, n
                  do k = 1, n
                     do i = 1, n
                        c( i, j ) = c( i, j ) + a( i, k ) * b( k, j )
                     enddo
                  enddo  
               end do
            end do
         elseif ( itype.eq.4 ) then 
            do i11 = 1, m
               do k = 1, n
                  do j = 1, n
                     do i = 1, n
                        c( i, j ) = c( i, j ) + a( i, k ) * b( k, j )
                     enddo
                  enddo  
               end do
            end do
         elseif ( itype.eq.5 ) then 
            do i11 = 1, m
               do k = 1, n
                  do i = 1, n
                     do j = 1, n
                        c( i, j ) = c( i, j ) + a( i, k ) * b( k, j )
                     enddo
                  enddo  
               end do
            end do
         elseif ( itype.eq.6 ) then 
            do i11 = 1, m
               do i = 1, n
                  do k = 1, n
                     do j = 1, n
                        c( i, j ) = c( i, j ) + a( i, k ) * b( k, j )
                     enddo
                  enddo  
               end do
            end do
         elseif ( itype.eq.7 ) then 
            do i11 = 1, m
               do i = 1, n
                  do j = 1, n
                     do k = 1, n
                        c( i, j ) = c( i, j ) + a( i, k ) * b( k, j )
                     enddo
                  enddo  
               end do
            end do
         endif
      endif

C      sum = 0
C      Do j=1,n
C         do i=1,n
C            sum = sum + c(i,j)
C         enddo
C      enddo
      end
