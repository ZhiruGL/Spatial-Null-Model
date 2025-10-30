#' Plot the spatial associations
#'
#' @import ggraph tidygraph
#' @return A graph visualizing spatial associations
#' @param spatial_association a matrix with elements encoding the strength of species associations. Strength in (0, 1) indicates attraction, = 0 indicates null relationship, in (-1, 0) indicates repulsion
#' @export

plot_spatial_association <- function(spatial_association){
  p <- spatial_association %>%
    as.table() %>%
    as.data.frame() %>%
    transmute(
      from = Var1,
      to = Var2,
      weight = Freq # this will reshape the matrix into two columns of interacting species and one value of interacting coefficient
    ) %>%
    mutate(
      association = case_when(
        weight > 0 ~ "attractive",
        weight == 0 ~ "null",
        weight < 0 ~ "repulsive"
      )# this add in a new column which helps to evaluate the number of the weight value and give the categorical value to the weight value observed
    ) %>%
    mutate(
      transparency = ifelse(weight == 0, 1, abs(weight))# This gives a transparency score that is proportional to the freq value range from 0-1
    ) %>%
    filter(
      as.numeric(from) <= as.numeric(to)
      #this is to ensure that each pair of species only occur once
    ) %>%
    mutate(from = as.character(from),
           to   = as.character(to)) %>% ## this is added so that the name is now character, other wise we would see the error "name" is not found
    as_tbl_graph() %>% # this is turning the data frae into a tbl_graph object, which is a format used by ggplot for visualizing networks
    ggraph(layout = 'linear', circular = TRUE) + #this creates a circular layout where nodes are arranged evenly around a circle
    geom_edge_link(aes(color = association,
                       alpha = transparency,
                       label = weight),
                   # arrow = arrow(
                   #   length = unit(4, 'mm'), type ='open', ends = 'both'
                   #   ),
                   start_cap = circle(5, 'mm'),
                   end_cap = circle(5, 'mm')) +. # handles self-loops, color encodes the type fo association, alpha encodes the strengths and label display the numeric weight of the interaction
    geom_edge_loop(aes(color = association, alpha = transparency,
                       label = weight),
                   start_cap = circle(5, 'mm'),
                   end_cap = circle(5, 'mm')) +
    geom_node_point(size = 4) + #each points becoms a labeled point around the circle
    geom_node_label(aes(label = name)) + #This will assign custom colors: blue(attractive), gray(null),red/brown(repulsive)
    scale_edge_colour_manual(
      values=c("#4594B8", "gray", "#8E5750")
    ) +
    scale_edge_alpha_identity() +
    coord_fixed() +
    theme_graph(foreground = "white",
                fg_text_colour = "white",
                base_family = 'Helvetica') +
    theme(
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.text=element_text(size=12)
    )#applies a clean theme
  p }
  # the output is that each node represent the species
  # each edege is the pairwise spatial association
  #edge color is the type(attractive/null/repulsive)
  #edge transparency:the magnitude of association
  #layout is circular: speceis arranged around a ring

