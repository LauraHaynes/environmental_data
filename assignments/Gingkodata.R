require(here)
ginkgo = read.csv(here("environmental_data" , "data", "ginkgo_data_2022.csv"))
head(ginkgo)
unique(subset(ginkgo, seeds_present, site_id))
head(ginkgo$site_id)
(nrow(ginkgo))/10

trees=(data.frame(ginkgo$site_id, ginkgo$seeds_present)) 
nrow(subset(unique(trees, select=site_id))) 

sum(ginkgo$seeds_present=="TRUE")/10 

?plot()

plot(
  ginkgo$max_depth,
  ginkgo$max_width,
  main = "Ginkgo Leaves",
  xlab = "Max leaf depth",
  ylab = "Max leaf width"
)

