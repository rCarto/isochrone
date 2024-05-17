
# isochrone

install

``` r
remotes::install_github("rCarto/isochrone")
```

Build Isochrones

``` r
library(sf)
```

    ## Linking to GEOS 3.11.1, GDAL 3.6.2, PROJ 9.1.1; sf_use_s2() is TRUE

``` r
library(isochrone)
library(mapsf)
library(terra)
```

    ## terra 1.7.71

``` r
server = "http://xxxx6xxxxxx"


res1 <- isochrone(
  loc = c(5.653536, 53.019535),
  breaks = seq(0, 30, 5),
  res = 100, 
  osrm.server = server,
  osrm.profile = "car") |>
  st_transform('EPSG:3857')

res2 <- isochrone(
  loc = c(5.653536, 53.019535),
  breaks = seq(0, 30, 5),
  res = 100, 
  osrm.server = server,
  osrm.profile = "car",
  method = "smooth")|>
  st_transform('EPSG:3857')

par(mfrow = c(1,2))
mf_map(x = res1, var = "isomin", type = "choro",
       breaks = sort(unique(c(res1$isomin, res1$isomax))),
       pal = "Lajolla", border = NA, leg_pos = "topleft",
       leg_val_rnd = 0, rev = TRUE, 
       leg_frame = TRUE, leg_title = "Isochrones\n(min)")
mf_title("osrm_pkg")
mf_map(x = res2, var = "isomin", type = "choro",
       breaks = sort(unique(c(res2$isomin, res2$isomax))),
       pal = "Lajolla", border = NA, leg_pos = "topleft",
       leg_val_rnd = 0, rev = TRUE, 
       leg_frame = TRUE, leg_title = "Isochrones\n(min)")
mf_title("smooth")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
apotheke.sf <- st_read(system.file("gpkg/apotheke.gpkg", package = "osrm"),
                       quiet = TRUE)
# Get isochones with lon/lat coordinates
iso1 <- isochrone(loc = c(13.43, 52.47), breaks = seq(0, 12, 2), res = 100,  
                  osrm.server = server,
                  osrm.profile = "car")
iso2 <- isochrone(loc = c(13.43, 52.47), breaks = seq(0, 12, 2), res = 100,
                  method = "smooth",
                  osrm.server = server,
                  osrm.profile = "car")

par(mfrow = c(1,2))  
plot(iso1["isomax"], breaks = sort(unique(c(iso1$isomin, iso1$isomax))))
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
plot(iso2["isomax"], breaks = sort(unique(c(iso2$somin, iso2$isomax))))
```

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# Get isochones with an sf POINT
iso3 <- isochrone(loc = apotheke.sf[11, ], breaks = seq(0, 12, 2), res = 100, 
                  osrm.server = server,
                  osrm.profile = "car")
iso4 <- isochrone(loc = apotheke.sf[11, ], breaks = seq(0, 12, 2), res = 100,
                  method = "smooth", 
                  osrm.server = server,
                  osrm.profile = "car")
# Map
mf_map(x = iso3, var = "isomin", type = "choro",
       breaks = sort(unique(c(iso3$isomin, iso3$isomax))),
       pal = "Burg", border = NA, leg_pos = "topleft",
       leg_val_rnd = 0,
       leg_frame = TRUE, leg_title = "Isochrones\n(min)")
mf_map(x = iso4, var = "isomin", type = "choro",
       breaks = sort(unique(c(iso4$isomin, iso4$isomax))),
       pal = "Burg", border = NA, leg_pos = "topleft",
       leg_val_rnd = 0,
       leg_frame = TRUE, leg_title = "Isochrones\n(min)")
```

![](README_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->
