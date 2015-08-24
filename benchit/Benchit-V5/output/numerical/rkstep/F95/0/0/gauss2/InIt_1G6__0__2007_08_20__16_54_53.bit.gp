#gnuplotfile
set title "numerical.rkstep.F95.0.0.gauss2"
set xlabel "Problemdimension N"
set xtics ("0 " 0.000000e+00,"100 " 1.000000e+02,"200 " 2.000000e+02,"300 " 3.000000e+02,"400 " 4.000000e+02,"500 " 5.000000e+02,"600 " 6.000000e+02,"700 " 7.000000e+02,"800 " 8.000000e+02,"900 " 9.000000e+02,"1000 " 1.000000e+03,"1100 " 1.100000e+03)
set xrange [0.000000e+00:1.100000e+03]
set ytics ("0 " 0.000000e+00,"250 " 2.500000e+02,"500 " 5.000000e+02,"750 " 7.500000e+02,"1000 " 1.000000e+03,"1250 " 1.250000e+03,"1500 " 1.500000e+03,"1750 " 1.750000e+03,"2000 " 2.000000e+03,"2250 " 2.250000e+03)
set yrange [0.000000e+00:2.250000e+03]
set ylabel "Anz. der Iterationen"
set data style points
set term postscript eps color solid
set output "InIt_1G6__0__2007_08_20__16_54_53.bit.gp.eps"
plot "InIt_1G6__0__2007_08_20__16_54_53.bit" using 1:2 title 'ID', "InIt_1G6__0__2007_08_20__16_54_53.bit" using 1:3 title 'BlockJac', "InIt_1G6__0__2007_08_20__16_54_53.bit" using 1:4 title 'LowerBLockSOR', "InIt_1G6__0__2007_08_20__16_54_53.bit" using 1:5 title 'UpperBLockSOR', "InIt_1G6__0__2007_08_20__16_54_53.bit" using 1:6 title 'BlockSSOR'