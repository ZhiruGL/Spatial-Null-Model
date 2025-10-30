library(ggplot2)
# Create a 3x3 interaction matrix
set.seed(123)  # optional, for reproducibility

dcf <- matrix(runif(9, min =-1, max = 1), nrow = 3)

# Assign row and column names
rownames(dcf) <- c("dog", "cat", "fly")
colnames(dcf) <- c("dog", "cat", "fly")

# Print the matrix
dcf

result=simulate_point_process(spatial_association=dcf,
                              gamma=3,
                              radius=0.1,
                              num_candidates = 1000,
                              Npoints = 50,
                              count_max = 10,
                              names=TRUE)
print(result)
ggplot(result, aes(x = x, y = y, color = species)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Simulated Spatial Pattern by Species",
    x = "X coordinate",
    y = "Y coordinate"
  ) +
  coord_equal()

