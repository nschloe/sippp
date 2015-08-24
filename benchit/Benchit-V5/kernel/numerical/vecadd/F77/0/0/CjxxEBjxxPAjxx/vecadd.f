CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  BenchIT - Performance Measurement for Scientific Applications
C
C  <Description>
C
C  Author: Thomas William (benchit@zih.tu-dresden.de)
C  Last change by: $Author: william $
C  $Revision: 1.2 $
C  $Date: 2005/12/07 18:36:10 $
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C ****f* vecadd3_funcs.c/measurement::tstcas_
C SYNOPSIS
C double gettimeroverhead()
C DESCRIPTION
C This is the actual measuring function. It adds to vectors.
C Solving the equation: c(jxx)=b(jxx)+a(jxx)
C ***

      subroutine vecadd( n, m, unroll, a, b, c )
      Integer n, m, unroll
      REAL*8 a( n ), b( n ), c( n )
      
      Integer i11, j1, ji, j

C     **** Initialization  
      i11 = 0
      j1 = 0
      j2 = 0
      ji = 0
      j = 0

      if ( unroll.eq.1 ) then 
         Do i11 = 1, m
            do j = 1, n
               c( j ) = b( j ) + a( j )
            end do
         enddo   
      else
         if ( unroll.eq.17 ) unroll = 64
         Do i11 = 1, m   
            j1 = 1
            j2 = MOD( N, unroll )
            do j = 1, n
               c( j ) = b( j ) + a( j )
               j1 = j1 + 1
            end do
         enddo
         if ( unroll.eq.2 ) then 
           Do i11 = 1, m
             do j = j1, n, 2
               c( j ) = b(j ) + a( j )
               c( j+1 ) = b( j+1 ) + a( j+1 )
             end do
           enddo   
         elseif ( unroll.eq.3 ) then 
            Do i11 = 1, m                                
               do j = j1, n, 3
                 c( j ) = b( j ) + a( j )
                 c( j+ 1) = b( j+1 ) + a( j+1 )
                 c( j+ 2) = b( j+2 ) + a( j+2 )
               end do
            enddo   
         elseif ( unroll.eq.4 ) then 
            Do i11 = 1, m
               do j = j1, n, 4
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
               end do
            enddo   
         elseif ( unroll.eq.5 ) then 
            Do i11 = 1, m
               do j = j1, n, 5
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
               end do
            enddo   
         elseif ( unroll.eq.6 ) then 
            Do i11 = 1, m
               do j = j1, n, 6
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
               end do
            enddo   
         elseif ( unroll.eq.7 ) then 
            Do i11 = 1, m
               do j = j1, n, 7
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
               end do
            enddo   
         elseif ( unroll.eq.8 ) then 
            Do i11 = 1, m
               do j = j1, n, 8
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
                 c( j+7 ) = b( j+7 ) + a( j+7 )
               end do
            enddo   
         elseif ( unroll.eq.9 ) then 
            Do i11 = 1, m
               do j = j1, n, 9
                 c( j ) = b( j ) + a( j )
                 c( j+1) = b( j+1 ) + a( j+1 )
                 c( j+2) = b( j+2 ) + a( j+2 )
                 c( j+3) = b( j+3 ) + a( j+3 )
                 c( j+4) = b( j+4 ) + a( j+4 )
                 c( j+5) = b( j+5 ) + a( j+5 )
                 c( j+6) = b( j+6 ) + a( j+6 )
                 c( j+7) = b( j+7 ) + a( j+7 )
                 c( j+8) = b( j+8 ) + a( j+8 )
               end do
            enddo   
         elseif ( unroll.eq.10 ) then 
            Do i11 = 1, m
               do j = j1, n, 10
                 c( j ) = b( j ) + a( j )
                 c( j+1) = b( j+1 ) + a( j+1 )
                 c( j+2) = b( j+2 ) + a( j+2 )
                 c( j+3) = b( j+3 ) + a( j+3 )
                 c( j+4) = b( j+4 ) + a( j+4 )
                 c( j+5) = b( j+5 ) + a( j+5 )
                 c( j+6) = b( j+6 ) + a( j+6 )
                 c( j+7) = b( j+7 ) + a( j+7 )
                 c( j+8) = b( j+8 ) + a( j+8 )
                 c( j+9) = b( j+9 ) + a( j+9 )
               end do
            enddo   
         elseif ( unroll.eq.11 ) then 
            Do i11 = 1, m
               do j = j1, n, 11
                 c( j ) = b( j ) + a(j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
                 c( j+7 ) = b( j+7 ) + a( j+7 )
                 c( j+8 ) = b( j+8 ) + a( j+8 )
                 c( j+9 ) = b( j+9 ) + a( j+9 )
                 c( j+10 ) = b( j+10 ) + a( j+10 )
               end do
            enddo   
         elseif ( unroll.eq.12 ) then 
            Do i11 = 1, m
               do j = j1, n, 12
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
                 c( j+7 ) = b( j+7 ) + a( j+7 )
                 c( j+8 ) = b( j+8 ) + a( j+8 )
                 c( j+9 ) = b( j+9 ) + a( j+9 )
                 c( j+10 ) = b( j+10 ) + a( j+10 )
                 c( j+11 ) = b( j+11 ) + a( j+11 )
               end do
            enddo   
         elseif ( unroll.eq.13 ) then 
            Do i11 = 1, m
               do j = j1, n, 13
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
                 c( j+7 ) = b( j+7 ) + a( j+7 )
                 c( j+8 ) = b( j+8 ) + a( j+8 )
                 c( j+9 ) = b( j+9 ) + a( j+9 )
                 c( j+10 ) = b( j+10 ) + a( j+10 )
                 c( j+11 ) = b( j+11 ) + a( j+11 )
                 c( j+12 ) = b( j+12 ) + a( j+12 )
               end do
            enddo   
         elseif ( unroll.eq.14 ) then 
            Do i11 = 1, m
               do j = j1, n, 14
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
                 c( j+7 ) = b( j+7 ) + a( j+7 )
                 c( j+8 ) = b( j+8 ) + a( j+8 )
                 c( j+9 ) = b( j+9 ) + a( j+9 )
                 c( j+10 ) = b( j+10 ) + a( j+10 )
                 c( j+11 ) = b( j+11 ) + a( j+11 )
                 c( j+12 ) = b( j+12 ) + a( j+12 )
                 c( j+13 ) = b( j+13 ) + a( j+13 )
               end do
            enddo   
         elseif ( unroll.eq.15 ) then 
            Do i11 = 1, m
               do j = j1, n, 15
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
                 c( j+7 ) = b( j+7 ) + a( j+7 )
                 c( j+8 ) = b( j+8 ) + a( j+8 )
                 c( j+9 ) = b( j+9 ) + a( j+9 )
                 c( j+10 ) = b( j+10 ) + a( j+10 )
                 c( j+11 ) = b( j+11 ) + a( j+11 )
                 c( j+12 ) = b( j+12 ) + a( j+12 )
                 c( j+13 ) = b( j+13 ) + a( j+13 )
                 c( j+14 ) = b( j+14 ) + a( j+14 )
               end do
            enddo   
         elseif ( unroll.eq.16 ) then 
            Do i11 = 1, m
               do j = j1, n, 16
                 c( j ) = b( j ) + a( j )
                 c( j+1 ) = b( j+1 ) + a( j+1 )
                 c( j+2 ) = b( j+2 ) + a( j+2 )
                 c( j+3 ) = b( j+3 ) + a( j+3 )
                 c( j+4 ) = b( j+4 ) + a( j+4 )
                 c( j+5 ) = b( j+5 ) + a( j+5 )
                 c( j+6 ) = b( j+6 ) + a( j+6 )
                 c( j+7 ) = b( j+7 ) + a( j+7 )
                 c( j+8 ) = b( j+8 ) + a( j+8 )
                 c( j+9 ) = b( j+9 ) + a( j+9 )
                 c( j+10 ) = b( j+10 ) + a( j+10 )
                 c( j+11 ) = b( j+11 ) + a( j+11 )
                 c( j+12 ) = b( j+12 ) + a( j+12 )
                 c( j+13 ) = b( j+13 ) + a( j+13 )
                 c( j+14 ) = b( j+14 ) + a( j+14 )
                 c( j+15 ) = b( j+15 ) + a( j+15 )
               end do
            enddo   
         elseif ( unroll.eq.64 ) then 
            Do i11 = 1, m
               do j = j1, n, unroll
                 do ji = j, j + unroll
                   c( ji ) = b( ji )+a( ji )
                 enddo
               end do
            enddo   
         endif
      endif

C      sum = 0
C      Do i=1,n
C         sum = sum + c(i)
C      enddo
      end
