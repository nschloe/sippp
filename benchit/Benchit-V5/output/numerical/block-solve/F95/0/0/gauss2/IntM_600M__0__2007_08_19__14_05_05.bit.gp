#gnuplotfile
set title "numerical.block-solve.F95.0.0.gauss2"
set xlabel "Problem dimension N"
set xtics ("0 " 0.000000e+00,"100 " 1.000000e+02,"200 " 2.000000e+02,"300 " 3.000000e+02,"400 " 4.000000e+02,"500 " 5.000000e+02,"600 " 6.000000e+02,"700 " 7.000000e+02,"800 " 8.000000e+02,"900 " 9.000000e+02,"1000 " 1.000000e+03,"1100 " 1.100000e+03)
set xrange [0.000000e+00:1.100000e+03]
set ytics ("0 " 0.000000e+00,"2.5m" 2.500000e-03,"5m" 5.000000e-03,"7.5m" 7.500000e-03,"10m" 1.000000e-02,"12.5m" 1.250000e-02,"15m" 1.500000e-02,"17.5m" 1.750000e-02,"20m" 2.000000e-02)
set yrange [0.000000e+00:2.250000e-02]
set ylabel "No. of floating points ops"
set data style points
set term postscript eps color solid
set output "IntM_600M__0__2007_08_19__14_05_05.bit.gp.eps"
plot "IntM_600M__0__2007_08_19__14_05_05.bit" using 1:2 title 'LowerBlock ver.1', "IntM_600M__0__2007_08_19__14_05_05.bit" using 1:3 title 'LowerBlock ver.2'