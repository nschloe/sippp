#gnuplotfile
set title "numerical.LM.F95.0.0.factorised"
set xlabel "Polynomial degree (P)"
set xtics ("0 " 0.000000e+00,"2.5 " 2.500000e+00,"5 " 5.000000e+00,"7.5 " 7.500000e+00,"10 " 1.000000e+01,"12.5 " 1.250000e+01,"15 " 1.500000e+01)
set xrange [0.000000e+00:1.500000e+01]
set ytics ("0 " 0.000000e+00,"500M" 5.000000e+08,"1000M" 1.000000e+09,"1500M" 1.500000e+09,"2000M" 2.000000e+09,"2500M" 2.500000e+09,"3000M" 3.000000e+09,"3500M" 3.500000e+09,"4000M" 4.000000e+09)
set yrange [0.000000e+00:4.000000e+09]
set ylabel "Performance [FLOP/sec]"
set data style points
set term postscript eps color solid
set output "InI2_1G6__mars-mkl__2007_07_12__14_35_23.bit.gp.eps"
plot "InI2_1G6__mars-mkl__2007_07_12__14_35_23.bit" using 1:2 title 'Loops-ij ', "InI2_1G6__mars-mkl__2007_07_12__14_35_23.bit" using 1:3 title 'Loops-ji ', "InI2_1G6__mars-mkl__2007_07_12__14_35_23.bit" using 1:4 title 'MATMUL', "InI2_1G6__mars-mkl__2007_07_12__14_35_23.bit" using 1:5 title 'DGEMV '