rgrid <- function(loc, dmax, res) {
  # create a regular grid centerd on loc
  coords <- sf::st_coordinates(loc)
  xf <- coords[1, 1]
  yf <- coords[1, 2]
  boxCoordX <- seq(
    from = xf - dmax,
    to = xf + dmax,
    length.out = res
  )
  boxCoordY <- seq(
    from = yf - dmax,
    to = yf + dmax,
    length.out = res
  )
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  sgrid <- data.frame(
    ID = seq(1, nrow(sgrid), 1),
    COORDX = sgrid[, 1],
    COORDY = sgrid[, 2]
  )
  sgrid <- sf::st_as_sf(sgrid,
    coords = c("COORDX", "COORDY"),
    crs = st_crs(loc), remove = FALSE
  )
  return(sgrid)
}








# x <- x_v
input_route <- function(x, id, single = TRUE, all.ids = FALSE) {
  # test various cases (vector, data.frame, sf or sfc)
  oprj <- NA
  if (single) {
    if (is.vector(x)) {
      if (length(x) == 2 && is.numeric(x)) {
        if (x[1] > 180 || x[1] < -180 || x[2] > 90 || x[2] < -90) {
          stop(
            paste0(
              "longitude is bounded by the interval [-180, 180], ",
              "latitude is bounded by the interval [-90, 90]"
            ),
            call. = FALSE
          )
        }
        lon <- clean_coord(x[1])
        lat <- clean_coord(x[2])
        return(list(id = id, lon = lon, lat = lat, oprj = oprj))
      } else {
        stop(
          paste0(
            '"', id, '" should be a numeric vector of length 2, ',
            "i.e., c(lon, lat)."
          ),
          call. = FALSE
        )
      }
    }
    if (inherits(x = x, what = c("sfc", "sf"))) {
      oprj <- st_crs(x)
      if (length(st_geometry(x)) > 1) {
        message(paste0('Only the first row/element of "', id, '" is used.'))
      }
      if (inherits(x, "sfc")) {
        x <- x[1]
        idx <- id
      } else {
        x <- x[1, ]
        idx <- row.names(x)
      }
      if (sf::st_geometry_type(x, by_geometry = FALSE) != "POINT") {
        stop(paste0('"', id, '" geometry should be of type POINT.'),
          call. = FALSE
        )
      }
      x <- sf::st_transform(x = x, crs = 4326)
      coords <- sf::st_coordinates(x)
      lon <- clean_coord(coords[, 1])
      lat <- clean_coord(coords[, 2])
      return(list(id = idx, lon = lon, lat = lat, oprj = oprj))
    }
    if (inherits(x = x, what = c("data.frame", "matrix"))) {
      if (nrow(x) > 1) {
        message(paste0('Only the first row of "', id, '" is used.'))
        x <- x[1, , drop = FALSE]
      }
      idx <- row.names(x)
      if (is.null(idx)) {
        idx <- id
      }
      x <- unlist(x)
      if (length(x) == 2 && is.numeric(x)) {
        lon <- clean_coord(x[1])
        lat <- clean_coord(x[2])
        return(list(id = idx, lon = lon, lat = lat, oprj = oprj))
      } else {
        stop(paste0('"', id, '" should contain coordinates.'),
          call. = FALSE
        )
      }
    } else {
      stop(
        paste0(
          '"', id, '" should be a vector of coordinates, ',
          "a data.frame or a matrix ",
          "of coordinates, an sfc POINT object or an ",
          "sf POINT object."
        ),
        call. = FALSE
      )
    }
  } else {
    if (inherits(x = x, what = c("sfc", "sf"))) {
      oprj <- st_crs(x)
      lx <- length(st_geometry(x))
      if (lx < 2) {
        stop('"loc" should have at least 2 rows or elements.',
          call. = FALSE
        )
      }
      type <- sf::st_geometry_type(x, by_geometry = FALSE)
      type <- as.character(unique(type))
      if (length(type) > 1 || type != "POINT") {
        stop('"loc" geometry should be of type POINT', call. = FALSE)
      }
      if (inherits(x, "sfc")) {
        id1 <- "src"
        id2 <- "dst"
        if (all.ids) {
          rn <- 1:lx
        }
      } else {
        rn <- row.names(x)
        id1 <- rn[1]
        id2 <- rn[lx]
      }
      x <- sf::st_transform(x = x, crs = 4326)
      coords <- sf::st_coordinates(x)
      lon <- clean_coord(coords[, 1])
      lat <- clean_coord(coords[, 2])
      if (!all.ids) {
        return(list(id1 = id1, id2 = id2, lon = lon, lat = lat, oprj = oprj))
      } else {
        return(list(id = rn, lon = lon, lat = lat, oprj = oprj))
      }
    }
    if (inherits(x = x, what = c("data.frame", "matrix"))) {
      lx <- nrow(x)
      if (lx < 2) {
        stop('"loc" should have at least 2 rows.', call. = FALSE)
      }
      if (ncol(x) == 2 && is.numeric(x[, 1, drop = TRUE]) && is.numeric(x[, 2, drop = TRUE])) {
        lon <- clean_coord(x[, 1, drop = TRUE])
        lat <- clean_coord(x[, 2, drop = TRUE])
        rn <- row.names(x)
        if (is.null(rn)) {
          rn <- 1:lx
        }
        id1 <- rn[1]
        id2 <- rn[lx]
        if (!all.ids) {
          return(list(id1 = id1, id2 = id2, lon = lon, lat = lat, oprj = oprj))
        } else {
          return(list(id = rn, lon = lon, lat = lat, oprj = oprj))
        }
      } else {
        stop(paste0('"loc" should contain coordinates.'),
          call. = FALSE
        )
      }
    } else {
      stop(
        paste0(
          '"loc" should be ',
          "a data.frame or a matrix ",
          "of coordinates, an sfc POINT object or an ",
          "sf POINT object."
        ),
        call. = FALSE
      )
    }
  }
}









# create short and clean coordinates
clean_coord <- function(x) {
  format(round(as.numeric(x), 5),
    scientific = FALSE, justify = "none",
    trim = TRUE, nsmall = 5, digits = 5
  )
}



#' @importFrom sf st_as_sf st_crs st_transform st_convex_hull st_union
#' st_intersects st_bbox st_buffer st_distance st_make_grid st_sfc
fill_grid <- function(destinations, measure, sgrid, res, tmax) {
  rpt <- st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)
  rpt <- st_transform(rpt, 3857)
  rpt$measure <- measure
  b <- as.numeric(st_distance(sgrid[1, ], sgrid[2, ]) / 2)
  xx <- st_make_grid(
    x = st_buffer(
      x = st_as_sfc(st_bbox(sgrid)),
      dist = b
    ),
    n = c(res, res)
  )
  ag_pt <- function(x) {
    if (length(x) > 0) {
      min(rpt[["measure"]][x], na.rm = TRUE)
    } else {
      NA
    }
  }
  inter <- st_intersects(xx, rpt)
  sgrid$measure <- unlist(lapply(inter, ag_pt))
  sgrid[is.infinite(sgrid$measure), "measure"] <- NA
  sgrid[is.nan(sgrid$measure), "measure"] <- NA
  # sgrid[sgrid$measure > tmax, "measure"] <- tmax + 1
  sgrid
}
