#' Generate a new spatial location for a given species
#'
#' @param species_location a tibble with the locations of species samples
#' @param spatial_association a matrix with elements encoding the strength of species associations. Strength in (0, 1) indicates attraction, = 0 indicates null relationship, in (-1, 0) indicates repulsion
#' @param gamma the gamma diversity
#' @param invading_species the label of species to sample (an integer between 1 to gamma)
#' @param num_candidates the number of candidate points that the sample can be input
#' @export
sample_spatial_point <- function(species_location,
                                 spatial_association,
                                 gamma, #=nrow(spatial_association)
                                 invading_species,
                                 num_candidates,
                                 radius,
                                 count_max) {
  resident_species <- setdiff(1:gamma, invading_species) #generate a list of all other species except the invading species/species being considered to be added

  candidates <- matrix(runif(2 * num_candidates, min = 0, max = 1),
    nrow = num_candidates, ncol = 2
  ) #This is to generate a matrix of potential coordiantes where the invader spceies could be added into the map

  if(length(radius) == 1){
    counts <- species_location %>%
      map(~ fields::rdist(candidates, .)) %>%
      map(~ apply(., 1, function(x) sum(x < radius))) #if the radius for every species' pair's interaction is the same, then we calculate the interacting ones with each candidaite coordinates
    #this output gamma lists of numeric variables, with the j value in the ith list represent the number of species i that is interacting with candidate j
  } else if(length(radius) == gamma){# different radius
    distances <- species_location %>%
      map(~ fields::rdist(candidates, .))
    counts <- 1:gamma %>%
      map(function(x){
        1:num_candidates %>%
          map_dbl(~sum(distances[[x]][.,] < radius[x]))
      })
  # the same idea just that the radius of interaction are varing based on different interacting species
  }


  counts <- counts %>%
    map(~ifelse(. < count_max, ., count_max))
#there is a maximum number of interaction that could occur which is predefined as a paramemter in the function

  weights <- spatial_association[invading_species, ] %>%
    {
      tan(pi / 2 * .)
    } %>%
    map_dbl(function(x) ifelse(x >= 0, x + 1, 1 / abs(x - 1)))
#This is to calculate the weight using the spatial association number
# The output will be a vector of numeric outputs with each of the values represents the updated weights of interaction based on the idea of positive/negative interaction
  1:gamma %>%
    map(~ weights[.]^counts[[.]]) %>%
    #the output at this step is a list of vector
    #the length of the list is gamma where as the length of each vector within the list is the number of candidate
    {
      Reduce(`*`, .)
    #this step will multiply the vector within the lsit cumulatively
    #the result will be a vector with the length of the candidate count
    #the value in the vector represents the weight of each candidate being chosen
      #the weight is proportion to the probability of the candidate being chosen
    } %>%
    {
      sample(1:num_candidates, size = 1, prob = .)
      #this is to choose a single points from the num_candidates based on the weight calculated above
      #not necessarily chose the highest probability one
    } %>%
    {
      candidates[., ]
      #the the coordinates of the chosen candidate were singled out
    }
}


