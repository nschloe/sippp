#gnuplotfile
set title "numerical.rkstep.F95.0.0.gauss2"
set xlabel "Problem dimension N"
set xtics ("0 " 0.000000e+00,"25 " 2.500000e+01,"50 " 5.000000e+01,"75 " 7.500000e+01,"100 " 1.000000e+02,"125 " 1.250000e+02,"150 " 1.500000e+02,"175 " 1.750000e+02,"200 " 2.000000e+02,"225 " 2.250000e+02)
set xrange [0.000000e+00:2.250000e+02]
set ytics ("0 " 0.000000e+00,"25 " 2.500000e+01,"50 " 5.000000e+01,"75 " 7.500000e+01,"100 " 1.000000e+02,"125 " 1.250000e+02)
set yrange [0.000000e+00:1.250000e+02]
set ylabel "No. of iterations"
set data style points
set term postscript eps color solid
set output "IntM_600M__0__2007_08_18__21_55_50.bit.gp.eps"
plot "IntM_600M__0__2007_08_18__21_55_50.bit" using 1:2 title 'ID', "IntM_600M__0__2007_08_18__21_55_50.bit" using 1:3 title 'BlockJac', "IntM_600M__0__2007_08_18__21_55_50.bit" using 1:4 title 'LowerBLockSOR', "IntM_600M__0__2007_08_18__21_55_50.bit" using 1:5 title 'UpperBLockSOR', "IntM_600M__0__2007_08_18__21_55_50.bit" using 1:6 title 'BlockSSOR'