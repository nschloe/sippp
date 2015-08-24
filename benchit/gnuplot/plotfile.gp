set data style points
set grid back
set xlabel "Problemdimension N"
set xrange      [0:1000]
set xtics 100
set ylabel "Rechenzeit in Sekunden"
set yrange      [0:8E-2]
#set ytics 
set key left top

set terminal pdf enhanced font "Helvetica,8"
set output "test.pdf"

set style line 1 linetype 1 linewidth 1 pointtype 1 pointsize 0.5
set style line 2 linetype 2 linewidth 1 pointtype 1 pointsize 0.5
set style line 3 linetype 3 linewidth 1 pointtype 1 pointsize 0.5
set style line 4 linetype 4 linewidth 1 pointtype 1 pointsize 0.5
set style line 5 linetype 7 linewidth 1 pointtype 1 pointsize 0.5

plot \
"../Benchit-V5/output/numerical/rkstep/F95/0/0/gauss2/InIt_1G6__0__2007_08_20__19_37_39.bit" using 1:($3*1/1) axes x1y1 title "B_J" ls 2, \
"../Benchit-V5/output/numerical/rkstep/F95/0/0/gauss2/InIt_1G6__0__2007_08_20__19_37_39.bit" using 1:($4*1/1) axes x1y1 title "B_{fGS}" ls 3, \
"../Benchit-V5/output/numerical/rkstep/F95/0/0/gauss2/InIt_1G6__0__2007_08_20__19_37_39.bit" using 1:($5*1/1) axes x1y1 title "B_{bGS}" ls 4, \
"../Benchit-V5/output/numerical/rkstep/F95/0/0/gauss2/InIt_1G6__0__2007_08_20__19_37_39.bit" using 1:($6*1/1) axes x1y1 title "B_{SSOR}" ls 5

#"InIt_1G6__0__2007_08_20__19_37_39.bit" using 1:($2*1/1) axes x1y1 title "keine Vorkond." ls 1, \
