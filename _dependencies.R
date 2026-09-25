# Packages renv's dependency scan cannot see. rnaturalearth::ne_countries(scale = "medium")
# and ne_states() load these at run time; 0.data.r uses both for the denial-track map (Figure 1).
# Listing them here keeps them in renv.lock on the next renv::snapshot().
library(rnaturalearthdata)
library(rnaturalearthhires)
