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
      weight = Freq
    ) %>%
    mutate(
      association = case_when(
        weight > 0 ~ "attractive",
        weight == 0 ~ "null",
        weight < 0 ~ "repulsive"
      )
    ) %>%
    mutate(
      transparency = ifelse(weight == 0, 1, abs(weight))
    ) %>%
    filter(
      as.numeric(from) <= as.numeric(to)
    ) %>%
    as_tbl_graph() %>%
    ggraph(layout = 'linear', circular = TRUE) +
    geom_edge_link(aes(color = association,
                       alpha = transparency,
                       label = weight),
                   # arrow = arrow(
                   #   length = unit(4, 'mm'), type ='open', ends = 'both'
                   #   ),
                   start_cap = circle(5, 'mm'),
                   end_cap = circle(5, 'mm')) +
    geom_edge_loop(aes(color = association, alpha = transparency,
                       label = weight),
                   start_cap = circle(5, 'mm'),
                   end_cap = circle(5, 'mm')) +
    geom_node_point(size = 4) +
    geom_node_label(aes(label = name)) +
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
    )
  p
}
