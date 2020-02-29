\donttest{
## Warning: an active internet connection is needed
## Get the data regarding Strasbourg University
unistra <- wdesr_get_data("Q61716176")

## Get data for Clermont and Bordeaux universities
clermont <- wdesr_get_data(c("Q3551576", "Q20791505"))
}
