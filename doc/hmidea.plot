set terminal postscript eps enhanced
set output "hmidea.eps"
set style line 1 lt 1 lw 1
set style line 2 lt 2 lw 1 
set style line 3 lt 3 lw 1 
set style line 4 lt 6 lw 1 
set style line 5 lt 1 lw 3 
set style line 6 lt 2 lw 3 
set style line 7 lt 3 lw 3 
set style line 8 lt 6 lw 3
set xlabel "Train Size"
set ylabel "HM True"
set key right bottom box
plot "pc1.dat" using 1:2 title "pc1" w lp ls 1, \
     "kc1.dat" using 1:2 title "kc1" w lp ls 6, \
     "kc2.dat" using 1:2 title "kc2" w lp ls 5, \
     "kc3.dat" using 1:2 title "kc3" w lp ls 4, \
     "cm1.dat" using 1:2 title "cm1" w lp ls 3