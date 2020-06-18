#
# gnuplot(1) template for plotting time series with filledcurves for new cases
#

set title 'Covid19 New Cases - {{ title }}'
set grid
set xdata time
set timefmt '%Y-%m-%dT%H:%M:%S+00:00Z'
set format x '%Y-%m-%d'
set xtics rotate by 45 right
set xlabel 'Time'
set ylabel 'Cases'
set key outside right center
set datafile separator ','
set autoscale fix
set style fill transparent solid 0.3
plot '{{ datafile }}' using 1:6:(0) title 'New Confirmed' with lines, \
     '' using 1:7:(0) title 'New Deaths' with lines, \
     '' using 1:8:(0) title 'New Recovered' with lines, \
     '' using 1:9:(0) title 'New Active' with lines
