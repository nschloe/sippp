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

C ****f* vecadd2_funcs.c/measurement::tstcas_
C SYNOPSIS
C double gettimeroverhead()
C DESCRIPTION
C This is the actual measuring function. It adds to vectors.
C Solving the equation: b(jxx)=b(jxx)+a(i)
C ***
      subroutine vecadd( n, m, unroll, a, b )
      Integer n, m, unroll
      REAL*8 a( n ), b( n )
      
      Integer i11, j1, j2, i, j

C     **** Initialization  
      i11 = 0
      j1 = 0
      j2 = 0
      i = 0
      j = 0

      if ( unroll.eq.1 ) then 
         Do i11 = 1, m
            do j = 1, n
               do i = 1, n
                  b( j ) = b( j ) + a( i )
               end do
            end do
         enddo   
      else
         Do i11 = 1, m   
            j1 = 1
            j2 = MOD( n, unroll )
            do j = 1, j2 
               do i= 1, n
                  b( j ) = b( j ) + a( i )
               end do
               j1 = j1 + 1
            enddo 
         enddo
         if ( unroll.eq.2 ) then 
           Do i11 = 1, m 
             do j = j1, n, 2
                 do i = 1, n
                    b( j )   = b( j )   + a( i )
                    b( j+1 ) = b( j+1 ) + a( i )
                 end do
              end do
           enddo   
         elseif ( unroll.eq.3 ) then 
            Do i11 = 1, m                                
               do j = j1, n, 3
                  do i = 1, n
                     b( j )   = b( j )   + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                  end do
               end do
            enddo   
         elseif ( unroll.eq.4 ) then 
            Do i11 = 1, m
               do j = j1, n, 4
                  do i = 1, n
                     b( j )   = b( j )   + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                  end do
               end do
            enddo   
         elseif ( unroll.eq.5 ) then 
            Do i11 = 1, m
               do j = j1, n, 5
                  do i = 1, n
                     b( j )   = b( j )   + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                  end do
               end do
            enddo   
         elseif ( unroll.eq.6 ) then 
            Do i11 = 1, m
               do j = j1, n, 6
                  do i = 1, n
                     b( j )   = b( j )   + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                  end do
               end do
            enddo   
         elseif ( unroll.eq.7 ) then 
            Do i11 = 1, m
               do j = j1, n, 7
                  do i = 1, n
                     b( j )   = b( j )   + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                  end do
               end do
            enddo   
         elseif ( unroll.eq.8 ) then 
            Do i11 = 1, m
               do j = j1, n, 8
                  do i = 1, n
                     b( j )   = b( j )   + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                  end do
               end do
            enddo   
         elseif ( unroll.eq.9 ) then 
            Do i11 = 1, m
               do j = j1, n, 9
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                  end do
               end do
            enddo   
         elseif ( unroll.eq.10 ) then 
            Do i11 = 1, m
               do j = j1, n, 10
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                     b( j+9 ) = b( j+9 ) + a( i )
                  end do
               end do
            enddo   
         elseif ( unroll.eq.11 ) then 
            Do i11 = 1, m
               do j = j1, n, 11
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                     b( j+9 ) = b( j+9 ) + a( i )
                     b( j+10 ) = b( j+10 ) + a( i )
                  end do
               end do
            enddo   
         elseif ( unroll.eq.12 ) then 
            Do i11 = 1, m
               do j = j1, n, 12
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                     b( j+9 ) = b( j+9 ) + a( i )
                     b( j+10 ) = b( j+10 ) + a( i )
                     b( j+11 ) = b( j+11 ) + a( i )
                  end do
               end do
            enddo   
         elseif ( unroll.eq.13 ) then 
            Do i11 = 1, m
               do j = j1, n, 13
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                     b( j+9 ) = b( j+9 ) + a( i )
                     b( j+10 ) = b( j+10 ) + a( i )
                     b( j+11 ) = b( j+11 ) + a( i )
                     b( j+12 ) = b( j+12 ) + a( i ) 
                  end do
               end do
            enddo   
         elseif ( unroll.eq.14 ) then 
            Do i11 = 1, m
               do j = j1, n, 14
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                     b( j+9 ) = b( j+9 ) + a( i )
                     b( j+10 ) = b( j+10 ) + a( i )
                     b( j+11 ) = b( j+11 ) + a( i )
                     b( j+12 ) = b( j+12 ) + a( i ) 
                     b( j+13 ) = b( j+13 ) + a( i )     
                  end do
               end do
            enddo   
         elseif ( unroll.eq.15 ) then 
            Do i11 = 1, m
               do j = j1, n, 15
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                     b( j+9 ) = b( j+9 ) + a( i )
                     b( j+10 ) = b( j+10 ) + a( i )
                     b( j+11 ) = b( j+11 ) + a( i )
                     b( j+12 ) = b( j+12 ) + a( i ) 
                     b( j+13 ) = b( j+13 ) + a( i )     
                     b( j+14 ) = b( j+14 ) + a( i ) 
                  end do
               end do
            enddo   
         elseif ( unroll.eq.16 ) then 
            Do i11 = 1, m
               do j = j1, n, 16
                  do i = 1, n
                     b( j   ) = b( j   ) + a( i )
                     b( j+1 ) = b( j+1 ) + a( i )
                     b( j+2 ) = b( j+2 ) + a( i )
                     b( j+3 ) = b( j+3 ) + a( i ) 
                     b( j+4 ) = b( j+4 ) + a( i )     
                     b( j+5 ) = b( j+5 ) + a( i ) 
                     b( j+6 ) = b( j+6 ) + a( i )
                     b( j+7 ) = b( j+7 ) + a( i ) 
                     b( j+8 ) = b( j+8 ) + a( i )
                     b( j+9 ) = b( j+9 ) + a( i )
                     b( j+10 ) = b( j+10 ) + a( i )
                     b( j+11 ) = b( j+11 ) + a( i )
                     b( j+12 ) = b( j+12 ) + a( i ) 
                     b( j+13 ) = b( j+13 ) + a( i )     
                     b( j+14 ) = b( j+14 ) + a( i ) 
                     b( j+15 ) = b( j+15 ) + a( i )
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
