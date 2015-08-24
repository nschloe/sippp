#gnuplotfile
set title "numerical.matmul.F77.0.0.double"
set xlabel "Matrix Size"
set xtics ("0 " 0.000000e+00,"100 " 1.000000e+02,"200 " 2.000000e+02,"300 " 3.000000e+02,"400 " 4.000000e+02,"500 " 5.000000e+02,"600 " 6.000000e+02)
set xrange [0.000000e+00:6.000000e+02]
set ytics ("0 " 0.000000e+00,"1G" 1.000000e+09,"2G" 2.000000e+09,"3G" 3.000000e+09,"4G" 4.000000e+09,"5G" 5.000000e+09,"6G" 6.000000e+09,"7G" 7.000000e+09)
set yrange [0.000000e+00:7.000000e+09]
set ylabel "FLOPS"
set data style points
set term postscript eps color solid
set output "InI2_1G6__mars-mkl__2007_07_13__16_07_34.bit.gp.eps"
plot "InI2_1G6__mars-mkl__2007_07_13__16_07_34.bit" using 1:2 title 'FLOPS (ijk)', "InI2_1G6__mars-mkl__2007_07_13__16_07_34.bit" using 1:3 title 'FLOPS (ikj)', "InI2_1G6__mars-mkl__2007_07_13__16_07_34.bit" using 1:4 title 'FLOPS (jik)', "InI2_1G6__mars-mkl__2007_07_13__16_07_34.bit" using 1:5 title 'FLOPS (jki)', "InI2_1G6__mars-mkl__2007_07_13__16_07_34.bit" using 1:6 title 'FLOPS (kij)', "InI2_1G6__mars-mkl__2007_07_13__16_07_34.bit" using 1:7 title 'FLOPS (kji)'