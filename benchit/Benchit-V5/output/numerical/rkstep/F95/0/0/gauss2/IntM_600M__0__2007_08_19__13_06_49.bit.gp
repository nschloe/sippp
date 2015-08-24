#gnuplotfile
set title "numerical.rkstep.F95.0.0.gauss2"
set xlabel "Problem dimension N"
set xtics ("0 " 0.000000e+00,"100 " 1.000000e+02,"200 " 2.000000e+02,"300 " 3.000000e+02,"400 " 4.000000e+02,"500 " 5.000000e+02,"600 " 6.000000e+02,"700 " 7.000000e+02,"800 " 8.000000e+02,"900 " 9.000000e+02,"1000 " 1.000000e+03,"1100 " 1.100000e+03)
set xrange [0.000000e+00:1.100000e+03]
set ytics ("0 " 0.000000e+00,"250m" 2.500000e-01,"500m" 5.000000e-01,"750m" 7.500000e-01,"1000m" 1.000000e+00,"1250m" 1.250000e+00,"1500m" 1.500000e+00,"1750m" 1.750000e+00,"2000m" 2.000000e+00)
set yrange [0.000000e+00:2.000000e+00]
set ylabel "No. of iterations"
set data style points
set term postscript eps color solid
set output "IntM_600M__0__2007_08_19__13_06_49.bit.gp.eps"
plot "IntM_600M__0__2007_08_19__13_06_49.bit" using 1:2 title 'ID', "IntM_600M__0__2007_08_19__13_06_49.bit" using 1:3 title 'BlockJac', "IntM_600M__0__2007_08_19__13_06_49.bit" using 1:4 title 'LowerBLockSOR', "IntM_600M__0__2007_08_19__13_06_49.bit" using 1:5 title 'UpperBLockSOR', "IntM_600M__0__2007_08_19__13_06_49.bit" using 1:6 title 'BlockSSOR'