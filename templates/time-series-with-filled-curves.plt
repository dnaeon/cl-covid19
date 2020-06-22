#
# gnuplot(1) template for plotting time series with filledcurves
#

set title 'Covid19 Cases - {{ title }}'
set grid
set xdata time
set timefmt '%Y-%m-%dT%H:%M:%S+00:00Z'
set format x '%Y-%m-%d'
set mouse mouseformat 3 # timefmt
set xtics rotate by 45 right
set xlabel 'Time'
set ylabel 'Cases'
set key outside right center
set datafile separator comma
set autoscale fix
set style fill transparent solid 0.3
plot '{{ datafile }}' using (column('timestamp')):(column('confirmed')):(0) title 'Confirmed' with filledcurves, \
     '' using (column('timestamp')):(column('deaths')):(0) title 'Deaths' with filledcurves, \
     '' using (column('timestamp')):(column('recovered')):(0) title 'Recovered' with filledcurves, \
     '' using (column('timestamp')):(column('active')):(0) title 'Active' with filledcurves
