library(DescTools)
library(emdist)
library(prioritizr)

# Load a solution
sol <- readRDS("Output/solutions/s2-EM-Percentile-tos-585.rds") %>% 
  dplyr::select(solution_1) %>% 
  dplyr::rename(sol = solution_1) %>% 
  tibble::as_tibble()
sol1 <- readRDS("Output/solutions/s3-EM-Percentile-phos-585.rds") %>% 
  dplyr::select(solution_1) %>% 
  dplyr::rename(sol1 = solution_1) %>% 
  tibble::as_tibble()
sol2 <- readRDS("Output/solutions/s4-EM-Percentile-o2os-585.rds") %>% 
  dplyr::select(solution_1) %>% 
  dplyr::rename(sol2 = solution_1) %>% 
  tibble::as_tibble()
sols <- dplyr::left_join(sol, sol1) %>% 
  dplyr::left_join(., sol2) %>% 
  sf::st_as_sf(sf_column_name = "geometry")
vec <- c("sol", "sol1", "sol2")


hist(x = sol$transformed)
quantile


# Earth mover's
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


tmp <- earth_mover_distances(data = sols, solution_names = vec)
