#
# gnuplot(1) template for plotting histograms of cases per country
#

set title 'Covid19 Cases - {{ title }}'
set grid
set xtics rotate by 45 right
set xlabel 'Country'
set ylabel 'Cases'
set key outside right center
set datafile separator comma

set autoscale fix

set style data histograms
set style histogram rowstacked
set boxwidth 1 relative
set style fill solid 1.0 border -1

plot '{{ datafile }}' using 2 title 'Confirmed', \
     '' using 3 title 'Deaths', \
     '' using 4 title 'Recovered', \
     '' using 5 title 'Active', \
     '' using 6 title 'New Confirmed', \
     '' using 7 title 'New Deaths', \
     '' using 8 title 'New Recovered', \
     '' using 9:xticlabels(11) title 'New Active'
