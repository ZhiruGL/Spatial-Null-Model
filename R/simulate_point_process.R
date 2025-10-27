#' Simulate a spatial pattern
#'
#' @param spatial_association a matrix with elements encoding the strength of species associations. Strength in (0, 1) indicates attraction, = 0 indicates null relationship, in (-1, 0) indicates repulsion
#' @param gamma the gamma diversity
#' @param radius the range of effective size
#' @param num_candidates the number of candidate points that the sample are from
#' @param Npoints the number of total simulated points
#'count_max Not sure what is in this case at this moment= define later
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
    map(~ matrix(runif(2 * num_initial, min = 0, max = 1), # Generates random uniform unmber between 0 and 1 and 2*num_initial such numbers
      nrow = num_initial, ncol = 2
    ))
  #These numbers are arranged into a matrix into a matrix with num_initial rows and 2 columns
  #The map returns a list of legnth gamma with each element being a num_initial x 2 matrix of random cooridnates
  names(species_location) <- LETTERS[1:gamma]
    #then letters are given to the species locations as the names of the matrix

  for (i in 1:length(species_location)) {
    colnames(species_location[[i]]) <- c("x", "y")
  }
    #the loop goes through each matrix in the list
    #for each species, we assigns column names x and y to that species' location matrix

  # Number of simulated points
  # Running a loop to simulate additional spatial points
  for (i in 1:Npoints) {
    invader <- i %% gamma + 1
    #loop i from 1 to Npoints: we are simulating that many of new points
    #invader line: cycles through species in order then go back to species A
      #Each iteration picks a species to act as the invader that adds a new point
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
    #this calls the sample_spatial point function, check that file for more detail
  }

  species_location %>%
    map(as_tibble) %>%
    bind_rows(.id = "species")
}
