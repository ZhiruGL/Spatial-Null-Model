#' Generate a new spatial location for a given species
#'
#' @param species_location a tibble with the locations of species samples
#' @param spatial_association a matrix with elements encoding the strength of species associations. Strength in (0, 1) indicates attraction, = 0 indicates null relationship, in (-1, 0) indicates repulsion
#' @param gamma the gamma diversity
#' @param invading_species the label of species to sample (an integer between 1 to gamma)
#' @param num_candidates the number of candidate points that the sample can
#' @export
sample_spatial_point <- function(species_location,
                                 spatial_association,
                                 gamma,
                                 invading_species,
                                 num_candidates,
                                 radius,
                                 count_max) {
  resident_species <- setdiff(1:gamma, invading_species)

  candidates <- matrix(runif(2 * num_candidates, min = 0, max = 1),
    nrow = num_candidates, ncol = 2
  )
  if(length(radius) == 1){
    counts <- species_location %>%
      map(~ fields::rdist(candidates, .)) %>%
      map(~ apply(., 1, function(x) sum(x < radius)))
  } else if(length(radius) == gamma){
    distances <- species_location %>%
      map(~ fields::rdist(candidates, .))
    counts <- 1:gamma %>%
      map(function(x){
        1:num_candidates %>%
          map_dbl(~sum(distances[[x]][.,] < radius[x]))
      })
  }
  counts <- counts %>%
    map(~ifelse(. < count_max, ., count_max))

  weights <- spatial_association[invading_species, ] %>%
    {
      tan(pi / 2 * .)
    } %>%
    map_dbl(function(x) ifelse(x >= 0, x + 1, 1 / abs(x - 1)))

  1:gamma %>%
    map(~ weights[.]^counts[[.]]) %>%
    {
      Reduce(`*`, .)
    } %>%
    {
      sample(1:num_candidates, size = 1, prob = .)
    } %>%
    {
      candidates[., ]
    }
}

