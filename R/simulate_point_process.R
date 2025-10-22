#' Simulate a spatial pattern
#'
#' @param spatial_association a matrix with elements encoding the strength of species associations. Strength in (0, 1) indicates attraction, = 0 indicates null relationship, in (-1, 0) indicates repulsion
#' @param gamma the gamma diversity
#' @param radius the range of effective size
#' @param num_candidates the number of candidate points that the sample are from
#' @param Nsteps the number of total simulated points
#'
#' @export
simulate_point_process <- function(spatial_association,
                                   gamma,
                                   radius,
                                   num_candidates = 1000,
                                   Npoints = 50,
                                   count_max = 10
                                   ) {
  # initialize species locations
  num_initial <- 2
  species_location <- 1:gamma %>%
    map(~ matrix(runif(2 * num_initial, min = 0, max = 1),
      nrow = num_initial, ncol = 2
    ))
  names(species_location) <- LETTERS[1:gamma]
  for (i in 1:length(species_location)) {
    colnames(species_location[[i]]) <- c("x", "y")
  }

  # Number of simulated points
  for (i in 1:Npoints) {
    invader <- i %% gamma + 1

    winner <- sample_spatial_point(
      species_location,
      spatial_association,
      gamma,
      invading_species = invader,
      num_candidates,
      radius,
      count_max
    )
    species_location[[invader]] <- species_location[[invader]] %>%
      rbind(winner)
  }

  species_location %>%
    map(as_tibble) %>%
    bind_rows(.id = "species")
}
