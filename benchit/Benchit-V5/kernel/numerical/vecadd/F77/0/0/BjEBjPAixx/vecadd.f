CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  BenchIT - Performance Measurement for Scientific Applications
C
C  <Description>
C
C  Author: Thomas William (benchit@zih.tu-dresden.de)
C  Last change by: $Author: william $
C  $Revision: 1.3 $
C  $Date: 2006/01/30 13:54:58 $
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


C This is the actual measuring function. It adds two vectors.
C Solving the equation: b(j)=b(j)+a(ixx)
C ***
      subroutine vecadd(n ,m, unroll, a, b, sum)
      Integer n, m, unroll 
      REAL*8 a( n ), b( n ), sum
      
      Integer i11 , i , i1, i2, j

C     **** Initialization  
      i11 = 0
      i1 = 0
      i2 = 0
      i = 0
      j = 0

      if ( unroll.eq.1 ) then 
         Do i11 = 1, m
            do i = 1, n
               do j = 1, n
                  b( j ) = b( j ) + a( i )
               end do
            end do
         enddo   
      else
         Do i11 = 1, m   
            i1 = 1
            i2 = MOD( n, unroll )
            do i = 1, i2 
               do j = 1, n
                  b( j ) = b( j ) + a( i )
               end do
               i1 = i1 + 1
            enddo 
         enddo
         if ( unroll.eq.2 ) then 
           Do i11 = 1, m
             do i = i1, n, 2
                 do j = 1, n
                    b( j ) = b( j ) + a( i )
                    b( j ) = b( j ) + a( i+1 )
                 end do
              end do
           enddo   
         elseif (unroll.eq.3) then 
            Do i11 = 1, m                                
               do i = i1, n, 3
                  do j = 1, n
                     b( j ) = b( j ) + a( i )
                     b( j ) = b( j ) + a( i+1 )
                     b( j ) = b( j ) + a( i+2 )
                  end do
               end do
            enddo   
         elseif (unroll.eq.4) then 
            Do i11 = 1, m
               do i = i1, n, 4
                  do j = 1, n
                     b( j ) = b( j ) + a( i )
                     b( j ) = b( j ) + a( i+1 )
                     b( j ) = b( j ) + a( i+2 ) 
                     b( j ) = b( j ) + a( i+3 ) 
                  end do
               end do
            enddo   
         elseif (unroll.eq.5) then 
            Do i11 = 1, m
               do i = i1, n, 5
                  do j = 1, n
                     b( j ) = b( j ) + a( i )
                     b( j ) = b( j ) + a( i+1 )
                     b( j ) = b( j ) + a( i+2 ) 
                     b( j ) = b( j ) + a( i+3 )     
                     b( j ) = b( j ) + a( i+4 ) 
                  end do
               end do
            enddo   
         elseif (unroll.eq.6) then 
            Do i11 = 1, m
               do i = i1, n, 6
                  do j = 1, n
                     b( j ) = b( j ) + a( i )
                     b( j ) = b( j ) + a( i+1 )
                     b( j ) = b( j ) + a( i+2 )
                     b( j ) = b( j ) + a( i+3 ) 
                     b( j ) = b( j ) + a( i+4 )     
                     b( j ) = b( j ) + a( i+5 ) 
                  end do
               end do
            enddo   
         elseif (unroll.eq.7) then 
            Do i11 = 1, m
               do i = i1, n, 7
                  do j = 1, n
                     b( j ) = b( j ) + a( i )
                     b( j ) = b( j ) + a( i+1 ) 
                     b( j ) = b( j ) + a( i+3 )     
                     b( j ) = b( j ) + a( i+3 ) 
                     b( j ) = b( j ) + a( i+4 )
                     b( j ) = b( j ) + a( i+5 )
                     b( j ) = b( j ) + a( i+6 )
                  end do
               end do
            enddo   
         elseif (unroll.eq.8) then 
            Do i11 = 1, m
               do i = i1, n, 8
                  do j = 1, n
                     b( j ) = b( j ) + a( i )
                     b( j ) = b( j ) + a( i+1 )
                     b( j ) = b( j ) + a( i+2 )
                     b( j ) = b( j ) + a( i+3 ) 
                     b( j ) = b( j ) + a( i+4 )     
                     b( j ) = b( j ) + a( i+5 ) 
                     b( j ) = b( j ) + a( i+6 )
                     b( j ) = b( j ) + a( i+7 ) 
                  end do
               end do
            enddo   
         endif
      endif

      sum = 0
      Do i=1,n
         sum = sum + b(i)
      enddo
      end
