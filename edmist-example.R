# Initialization
## load packages
library(emdist)
library(prioritizr)

## define function to create pair-wise matrix for Earth Mover Distances

#' Calculate Earth Mover Distances to compare solutions
#'
#' @param data `sf::st_sf()` object with planning units.
#'
#' @param  solution_names `character` vector of column names in `data`
#'   that have solutions. The solutions should have binary values
#'   indicating if each planning unit is selected or not within a solution.
#'
#' @return A `matrix` of pair-wise distances comparing solutions. Each
#'   row and column corresponds to a different solution, and larger
#'   values mean that solutions are more different to each other.
#'
#' @export
earth_mover_distances <- function(data, solution_names) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(data, "sf"),
    !isTRUE(sf::st_is_longlat(data)),
    is.character(solution_names),
    assertthat::noNA(solution_names),
    all(assertthat::has_name(data, solution_names))
  )
  # initialize output matrix
  out <- matrix(
    NA_real_, ncol = length(solution_names), nrow = length(solution_names),
    dimnames = list(solution_names, solution_names)
  )
  # calculate centroids for planning units
  coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(data)))
  # add an extra column with ones to represent equal weights
  ## this is because emdist::emd wants a matrix of coordinates with 1st
  ## column denoting the weights
  coords <- cbind(1, coords)
  # calculate values for matrix
  for (i in solution_names) {
    for (j in solution_names) {
      out[i, j] <- emdist::emd(
        A = coords[data[[i]] > 0.5, , drop = FALSE],
        B = coords[data[[j]] > 0.5, , drop = FALSE]
      )
    }
  }
  # return result
  out
}

# Preliminary processing
## load sf data with polygon planning units
data(sim_pu_sf)

## create some solutions,
## we'll just randomly select some planning units
sim_pu_sf$solution_1 <- round(runif(nrow(sim_pu_sf)) >= 0.7)
sim_pu_sf$solution_2 <- round(runif(nrow(sim_pu_sf)) >= 0.7)
sim_pu_sf$solution_3 <- round(runif(nrow(sim_pu_sf)) >= 0.7)

## visualize solutions
plot(sols[, c("sol", "sol1")])

# Main processing
## calculate Earth Mover distances
emd_results <- earth_mover_distances(
  data = sim_pu_sf,
  solution_names = c("solution_1", "solution_2", "solution_3")
)

## print results
print(emd_results)
