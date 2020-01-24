make_bbox <- function(lng, lat, spacing, proj) {
  if (is.na(spacing[1])) {
    lng_seq <- lng
  } else {
    lng_seq <- seq(lng[1], lng[2], length.out = ceiling(diff(lng) / spacing[1]))
  }
  if (is.na(spacing[2])) {
    lat_seq <- lat
  } else {
    lat_seq <- seq(lat[1], lat[2], length.out = ceiling(diff(lat) / spacing[2]))
  }
  bb <- rbind(
    data.frame(lng = lng_seq, lat = lat[1]),
    data.frame(lng = lng[2], lat = lat_seq),
    data.frame(lng = rev(lng_seq), lat = lat[2]),
    data.frame(lng = lng[1], lat = rev(lat_seq))
  ) %>%
  {SpatialPolygons(list(Polygons(list(Polygon(.)), "bb")))}
  if (!missing(proj)) {
    projection(bb) <- proj
  }
  return(bb)
}



lng_label <- function(x) {
  ifelse(x < 0, paste0("E", abs(round(x))), paste0("W", round(x)))
}
lat_label <- function(x) {
  ifelse(x < 0, paste0("S", abs(round(x))), paste0("N", round(x)))
}
make_graticules <- function(lng_breaks, lat_breaks, spacing, proj) {
  if (is.na(spacing[1])) {
    lng_seq <- c(-180, 180)
  } else {
    lng_seq <- seq(-180, 180, length.out = ceiling(360 / spacing[1]))
  }
  if (is.na(spacing[2])) {
    lat_seq <- c(-90, 90)
  } else {
    lat_seq <- seq(-90, 90, length.out = ceiling(180 / spacing[2]))
  }
  meridians <- lapply(lng_breaks,
                      function(x, lat_seq) {
                        Lines(list(Line(cbind(x, lat_seq))), ID = lng_label(x))
                      }, lat_seq)
  parallels <- lapply(lat_breaks,
                      function(x, lng_seq) {
                        Lines(list(Line(cbind(lng_seq, x))), ID = lat_label(x))
                      }, lng_seq)
  grat <- SpatialLines(c(meridians, parallels))
  if (!missing(proj)) {
    projection(grat) <- proj
  }
  return(grat)
}
