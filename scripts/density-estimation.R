# 95% density envelope experiments
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-05-16

rm(list = ls())

################################################################################
# https://stackoverflow.com/questions/25940726/how-to-estimate-the-area-of-95-contour-of-a-kde-object-from-ks-r-package
library(ks)
library(pracma)

################################################################################
# Making up some data. Two clusters of points, one with six times as many points
n1 <- 300
n2 <- 50

x1 <- rnorm(n = n1, mean = 5, sd = 0.25)
y1 <- rnorm(n = n1, mean = 5, sd = 0.25)
x2 <- rnorm(n = n2, mean = 8, sd = 0.25)
y2 <- rnorm(n = n2, mean = 2, sd = 0.25)

df <- data.frame(x = c(x1, x2),
                 y = c(y1, y2))

# Matrix is just easier to work with for density
mat <- as.matrix(df)
bandwidth_select <- ks::Hscv(x = mat)
dens_function <- ks::kde(x = mat, 
                         H = bandwidth_select,
                         compute.cont = TRUE)
# Calculate contour lines; contourLines will produce as many elements as there 
# are non-overlapping contours
all_contours <- contourLines(x = dens_function$eval.points[[1]],
                            y = dens_function$eval.points[[2]],
                            z = dens_function$estimate,
                            levels = dens_function$cont["5%"])

# Iterate over all elements in all_contours, calculating the area of the polygon
# and summing them
area_95 <- 0
plot(x = df$x, y = df$y)
for (one_contour in all_contours) {
  area_95 <- area_95 + abs(pracma::polyarea(x = one_contour$x,
                                            y = one_contour$y))
  points(one_contour, type = "l")
}
area_95

area_list <- lapply(X = all_contours, 
                    FUN = function(x) {
                      abs(pracma::polyarea(x = x[["x"]], 
                                           y = x[["y"]]))
                    })
total_area <- sum(unlist(area_list))
