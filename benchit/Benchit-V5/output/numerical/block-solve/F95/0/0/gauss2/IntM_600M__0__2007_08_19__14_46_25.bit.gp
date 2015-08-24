#gnuplotfile
set title "numerical.block-solve.F95.0.0.gauss2"
set xlabel "Problem dimension N"
set xtics ("7.5k" 7.500000e+03,"7.50025k" 7.500250e+03,"7.5005k" 7.500500e+03,"7.50075k" 7.500750e+03,"7.501k" 7.501000e+03,"7.50125k" 7.501250e+03,"7.5015k" 7.501500e+03,"7.50175k" 7.501750e+03,"7.502k" 7.502000e+03)
set xrange [7.500000e+03:7.502000e+03]
set ylabel "No. of floating points ops"
set data style points
set term postscript eps color solid
set output "IntM_600M__0__2007_08_19__14_46_25.bit.gp.eps"
plot "IntM_600M__0__2007_08_19__14_46_25.bit" using 1:2 title 'LowerBlock ver.1', "IntM_600M__0__2007_08_19__14_46_25.bit" using 1:3 title 'LowerBlock ver.2'