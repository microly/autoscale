# autoscale
show how to make the scale can be automatically found by ggplot2.

most of the code is copied from:  
https://stackoverflow.com/questions/49961135/how-to-tell-ggplot2-to-use-an-user-created-scale-for-a-new-aesthetic

reference:
the code in ggplot2/R/scale-type.R

Based on the code in ggplot2/R/scale-type.R, there should be a scale named `scale_mag_continuous` in the parent environment of `find_scale` function. Then, this scale can be find automatically.

```
library(autoscale)

geo <- tibble(lon = 1:10, lat = 1:10, mag = 1:10, angle = 1:10)

ggplot(geo, aes(lon, lat)) +
    geom_arrow(aes(mag = mag, angle = angle))

ggplot(geo, aes(lon, lat)) +
    geom_arrow(aes(mag = mag, angle = angle)) +
    scale_mag_continuous()
```
