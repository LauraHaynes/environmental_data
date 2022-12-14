require(here)
dat_ginkgo = read.csv(
  here("environmental_data" , "data", "ginkgo_data_2021.csv")
)

par(mfrow = c(1,1))

plot(dat_ginkgo$max_depth, dat_ginkgo$max_width,
    main = "Scatterplot of Gingko \n Leaf Depth & Width",
    xlab = "Gingko leaf max depth",
    ylab = "Gingko leaf max width",
    xlim = c(0, 150),
    ylim = c(0, 130),
    col = "purple",
    pch = 20,
    cex = 1
)

require(palmerpenguins)

hist(penguins$flipper_length_mm,
  breaks = 10,
)  

Example 3.1
require(here)
dat_ginkgo = read.csv(
  here("environmental_data" , "data", "ginkgo_data_2021.csv")
)

boxplot(
  dat_ginkgo$petiole_length,
  main = "Ginkgo petiole length"
)

#boxplot(dat_ginkgo$petiole_length ~ seeds_present)

#histograms

require(here)
dat_ginkgo = read.csv(
  here("environmental_data" , "data", "ginkgo_data_2021.csv"))
dat_newginkgo = read.csv(
  here("environmental_data" , "data", "ginkgo_data_2022.csv"))

par(mfrow = c(2,2))
hist(dat_newginkgo$max_depth, main = "2022 Ginkgo Leaf Depth")
hist(dat_ginkgo$max_depth,main = "2021 Ginkgo Leaf Depth")
hist(dat_newginkgo$max_width, main = "2022 Ginkgo Leaf Width")
hist(dat_ginkgo$max_width, main = "2021 Ginkgo Leaf Width")

?library()
library()
