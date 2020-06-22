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

plot '{{ datafile }}' using (column('confirmed')) title 'Confirmed', \
     '' using (column('deaths')) title 'Deaths', \
     '' using (column('recovered')) title 'Recovered', \
     '' using (column('active')) title 'Active', \
     '' using (column('new_confirmed')) title 'New Confirmed', \
     '' using (column('new_deaths')) title 'New Deaths', \
     '' using (column('new_recovered')) title 'New Recovered', \
     '' using (column('new_active')):xticlabels(11) title 'New Active'
