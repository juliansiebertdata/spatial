# SETUP --------------------------

## install packman if necessary
if (!`pacman` %in% installed.packages()) {{
install.packages(`pacman`)
}}

## load librarys ----
pacman: :p_load(sf, rnaturalearth, tidyverse, here, h3jsr)


# TIDY DATA --------------------------

# WORKFLOW --------------------------