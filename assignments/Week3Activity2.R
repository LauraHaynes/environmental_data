require(here)

dat_catrate = read.csv(here("environmental_data" , "data", "catrate.csv"))
dat_delomys = read.csv(here("environmental_data" , "data", "delomys.csv"))
dat_rope = read.csv(here("environmental_data" , "data", "rope.csv"))

class(dat_catrate)
head(dat_catrate)
head(dat_delomys)
head(dat_rope)

boxplot(dat_delomys$body_mass, main = "Laura Haynes" ,
        xlab="delomys body_mass"
        )

