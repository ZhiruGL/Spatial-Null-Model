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
  #makes a vector of all species except the current invader
  #used later to compare invader vs existing species
  candidates <- matrix(runif(2 * num_candidates, min = 0, max = 1),
    nrow = num_candidates, ncol = 2
  )
  #generates num_candidates random (x,y) points in a 1X1 spatial area
  #Each row represents a possible place the invader could occupy
  if(length(radius) == 1){.
    # one radius for all speies,
    counts <- species_location %>%
      map(~ fields::rdist(candidates, .)) %>%
      #then we use fields::rdist() to compute all pairwise distaince between candidates and that species' existing points
      map(~ apply(., 1, function(x) sum(x < radius)))
      #counts how many individuals of that species lie within radius
  } else if(length(radius) == gamma){# different radius
    distances <- species_location %>%
      map(~ fields::rdist(candidates, .))
    counts <- 1:gamma %>%
      map(function(x){
        1:num_candidates %>%
          map_dbl(~sum(distances[[x]][.,] < radius[x]))
      })
    #essentially the same idea, but uses a species-specific radius here
  }
  counts <- counts %>%
    map(~ifelse(. < count_max, ., count_max))
#this is to cap extremely high counts from dominating
  #sets an upper limit count_max
  weights <- spatial_association[invading_species, ] %>%
    {
      tan(pi / 2 * .)
    } %>%
    map_dbl(function(x) ifelse(x >= 0, x + 1, 1 / abs(x - 1)))
#compute weights based on species associations
  #takes the row of spatial association corresponding to the invading species
  #treansofmr this values via the link function
  #then we see wether it is positive or negative association
  #let us understand how strongly the invader prefers or avoids each resident species
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
#combine counts and weight
#for each species s, raise its weight to the power of its local count at each candidate point
#multiply all species' scores together to get an overall suitability of candidate species
#then this scores will be used as. probabilities in sample() to randomly pick one candidate point
#finally, return the coordinates of the chosen point

