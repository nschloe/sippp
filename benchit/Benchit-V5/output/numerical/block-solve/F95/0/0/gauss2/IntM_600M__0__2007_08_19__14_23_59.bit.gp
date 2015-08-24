#gnuplotfile
set title "numerical.block-solve.F95.0.0.gauss2"
set xlabel "Problem dimension N"
set xtics ("0 " 0.000000e+00,"2.5k" 2.500000e+03,"5k" 5.000000e+03,"7.5k" 7.500000e+03,"10k" 1.000000e+04,"12.5k" 1.250000e+04,"15k" 1.500000e+04,"17.5k" 1.750000e+04)
set xrange [0.000000e+00:1.750000e+04]
set ytics ("0 " 0.000000e+00,"5m" 5.000000e-03,"10m" 1.000000e-02,"15m" 1.500000e-02,"20m" 2.000000e-02,"25m" 2.500000e-02,"30m" 3.000000e-02,"35m" 3.500000e-02,"40m" 4.000000e-02)
set yrange [0.000000e+00:4.500000e-02]
set ylabel "No. of floating points ops"
set data style points
set term postscript eps color solid
set output "IntM_600M__0__2007_08_19__14_23_59.bit.gp.eps"
plot "IntM_600M__0__2007_08_19__14_23_59.bit" using 1:2 title 'LowerBlock ver.1', "IntM_600M__0__2007_08_19__14_23_59.bit" using 1:3 title 'LowerBlock ver.2'