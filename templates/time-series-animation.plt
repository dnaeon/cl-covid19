#
# gnuplot(1) template for generating animation of time series with lines
#

set title 'Covid19 Cases - {{ title }}'
set grid
set xtics rotate by 45 right
set xlabel 'Time'
set ylabel 'Cases'
set key outside right center
set datafile separator comma
set autoscale fix
set style fill transparent solid 0.3

set terminal gif animate delay {{ delay }} size {{ height }},{{ width }}
set output '{{ destination }}'

set timefmt '%Y-%m-%dT%H:%M:%S+00:00Z'
set format x '%Y-%m-%d'

# Get the min and max values, so that we can properly set the yrange
stats '{{ datafile }}' using (column('confirmed')) nooutput
ymin = int(STATS_min)
ymax = int(STATS_max)
records = int(STATS_records)

#set ytics format " %.0f"

# Get the min and max timestamps, so that we can properly set the xrange
xmin = system("head -2  {{ datafile }} | tail -1 | cut -d ',' -f 1")
xmax = system("tail -2 {{ datafile }} | head -1 | cut -d ',' -f 1")

# Slightly larger yrange, so that we don't go off the grid
set yrange [ymin:ymax+(ymax*0.1)]

set xdata time
set xrange [xmin:xmax]

# The `every` keyword below will iterate through each each record
# and plot the data points up to the current `i` value, e.g.
# plot first record, then plot the first two records,
# then plot the first three records, etc. until we plot
# all records, which when combined in the final GIF
# will create an animation of the evolution of the data points.
do for [i=0:records-1] {
    plot '{{ datafile }}' every ::::i using (column('timestamp')):(column('confirmed')) title 'Confirmed' with lines linewidth {{ line-width }}, \
    	 '' every ::::i using (column('timestamp')):(column('deaths')) title 'Deaths' with lines linewidth {{ line-width }}, \
    	 '' every ::::i using (column('timestamp')):(column('recovered')) title 'Recovered' with lines linewidth {{ line-width }}, \
    	 '' every ::::i using (column('timestamp')):(column('active')) title 'Active' with lines linewidth {{ line-width }}
}
