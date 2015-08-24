#gnuplotfile
set title "numerical.rkstep.F95.0.0.gauss2"
set xlabel "Problem dimension N"
set xtics ("0 " 0.000000e+00,"250 " 2.500000e+02,"500 " 5.000000e+02,"750 " 7.500000e+02,"1000 " 1.000000e+03,"1250 " 1.250000e+03,"1500 " 1.500000e+03,"1750 " 1.750000e+03,"2000 " 2.000000e+03,"2250 " 2.250000e+03)
set xrange [0.000000e+00:2.250000e+03]
set ytics ("0 " 0.000000e+00,"10m" 1.000000e-02,"20m" 2.000000e-02,"30m" 3.000000e-02,"40m" 4.000000e-02,"50m" 5.000000e-02,"60m" 6.000000e-02,"70m" 7.000000e-02)
set yrange [0.000000e+00:8.000000e-02]
set ylabel "No. of iterations"
set data style points
set term postscript eps color solid
set output "IntM_600M__0__2007_08_19__01_47_17.bit.gp.eps"
plot "IntM_600M__0__2007_08_19__01_47_17.bit" using 1:2 title 'ID', "IntM_600M__0__2007_08_19__01_47_17.bit" using 1:3 title 'BlockJac', "IntM_600M__0__2007_08_19__01_47_17.bit" using 1:4 title 'LowerBLockSOR', "IntM_600M__0__2007_08_19__01_47_17.bit" using 1:5 title 'UpperBLockSOR', "IntM_600M__0__2007_08_19__01_47_17.bit" using 1:6 title 'BlockSSOR'