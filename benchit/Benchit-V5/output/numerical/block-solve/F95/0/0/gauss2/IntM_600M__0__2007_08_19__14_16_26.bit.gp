#gnuplotfile
set title "numerical.block-solve.F95.0.0.gauss2"
set xlabel "Problem dimension N"
set xtics ("0 " 0.000000e+00,"1k" 1.000000e+03,"2k" 2.000000e+03,"3k" 3.000000e+03,"4k" 4.000000e+03,"5k" 5.000000e+03,"6k" 6.000000e+03,"7k" 7.000000e+03,"8k" 8.000000e+03,"9k" 9.000000e+03,"10k" 1.000000e+04,"11k" 1.100000e+04)
set xrange [0.000000e+00:1.100000e+04]
set ytics ("0 " 0.000000e+00,"25m" 2.500000e-02,"50m" 5.000000e-02,"75m" 7.500000e-02,"100m" 1.000000e-01,"125m" 1.250000e-01)
set yrange [0.000000e+00:1.500000e-01]
set ylabel "No. of floating points ops"
set data style points
set term postscript eps color solid
set output "IntM_600M__0__2007_08_19__14_16_26.bit.gp.eps"
plot "IntM_600M__0__2007_08_19__14_16_26.bit" using 1:2 title 'LowerBlock ver.1', "IntM_600M__0__2007_08_19__14_16_26.bit" using 1:3 title 'LowerBlock ver.2'