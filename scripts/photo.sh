#!/bin/bash

## Bash script with ImageMagick commands to modify and combine photos for illustration
## Ghislain Vieilledent <ghislain.vieilledent@cirad.fr>

##=========
## Cyclones

## Text size
init_pointsize=48
init_figsize=800
ts=$(($init_pointsize*600/$init_figsize))

## Images
dir1="photos/cyclones"
f1="Fanele_track_crop.jpg"
dir2="photos/bosake_cyclone/selected"
f2="491_Menabe_crop.jpg"
dir3="figs"
f3="fcc_KMNP.png"
# Montage
montage -tile 1x2 -geometry +0+0 "$dir1/$f1" "$dir2/$f2" "figs/m1.jpg"
# Annotate
convert -fill white -gravity NorthWest -annotate +5+5 "A" -pointsize $ts "figs/m1.jpg" "figs/m2.jpg"
convert -gravity NorthWest -annotate +5+405 "B" -pointsize $ts "figs/m2.jpg" "figs/cyclones.jpg"
convert -gravity NorthWest -annotate +5+5 "C" -pointsize $ts "$dir3/$f3" "figs/m3.jpg"
# Montage
montage -tile 2x1 -geometry +0+0 "figs/cyclones.jpg" "figs/m3.jpg" "figs/fig_box.jpg"
# Clean
rm figs/m[1-3].jpg

# End