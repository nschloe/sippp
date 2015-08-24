#gnuplotfile
set title "numerical.rkstep.F95.0.0.gauss2"
set xlabel "Problemdimension N"
set xtics ("0 " 0.000000e+00,"100 " 1.000000e+02,"200 " 2.000000e+02,"300 " 3.000000e+02,"400 " 4.000000e+02,"500 " 5.000000e+02,"600 " 6.000000e+02,"700 " 7.000000e+02,"800 " 8.000000e+02,"900 " 9.000000e+02,"1000 " 1.000000e+03,"1100 " 1.100000e+03)
set xrange [0.000000e+00:1.100000e+03]
set ytics ("0 " 0.000000e+00,"100M" 1.000000e+08,"200M" 2.000000e+08,"300M" 3.000000e+08,"400M" 4.000000e+08,"500M" 5.000000e+08,"600M" 6.000000e+08,"700M" 7.000000e+08,"800M" 8.000000e+08,"900M" 9.000000e+08,"1000M" 1.000000e+09)
set yrange [0.000000e+00:1.000000e+09]
set ylabel "Anz. der Gleitkommeoperationen"
set data style points
set term postscript eps color solid
set output "InIt_1G6__0__2007_08_20__17_53_02.bit.gp.eps"
plot "InIt_1G6__0__2007_08_20__17_53_02.bit" using 1:2 title 'ID', "InIt_1G6__0__2007_08_20__17_53_02.bit" using 1:3 title 'BlockJac', "InIt_1G6__0__2007_08_20__17_53_02.bit" using 1:4 title 'LowerBLockSOR', "InIt_1G6__0__2007_08_20__17_53_02.bit" using 1:5 title 'UpperBLockSOR', "InIt_1G6__0__2007_08_20__17_53_02.bit" using 1:6 title 'BlockSSOR'