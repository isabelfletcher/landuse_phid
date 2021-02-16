

# ======================= LAND COVER LAND USE: PHID TUTORIAL =========================

## Rory Gibb
## PHID Group meeting, 18/02/2021

## Tutorial outline:

# 1. Load, visualise and compare forest extent in 2000 estimated using 2 different datasets (ESA-CCI and Hansen)
# 2. Make some cool maps
# 3. Compare forest change trends at district-level estimated using the two datasets
# 4. Compare forest change metrics to population distribution across districts
# 5. Compare polygons to points




# ------------- Setup and dependencies ----------------

# directory
setwd("C:/Users/roryj/Documents/PhD/202007_lshtm_dengue/teaching/phid_landuse/landuse_phid/")

# install and load dependencies
# all of these are core to using R as a GIS
#install.packages(c("raster", "rgdal", "sp", "sf", "exactextractr", "ncdf4", "ggplot2", "dplyr", "fasterize", "RColorBrewer", "gridExtra"))
library(raster); library(rgdal); library(sp); library(sf); library(exactextractr); library(ncdf4)
library(ggplot2); library(fasterize); library(RColorBrewer); library(gridExtra); library(dplyr)

# theme for mapping
maptheme <- theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14, hjust=0.5),
        axis.title = element_blank())

# nice colour scale
colScale <- colorRampPalette(RColorBrewer::brewer.pal(9, name="YlGnBu"))(300)
  


# =================== Load and view areal data ========================

# load shapefile: district polygons for our area of interest
# a cluster of districts in the central coastal-to-highlands transition in Vietnam (Dak Lak and Khanh Hoa provinces)
# sections of this region have been hotspots of recent forest loss in the country
districts <- sf::st_read("./data/focal_districts.shp")
districts$xx = 1

# generate some point location data (e.g. villages/study sites/disease occurrences)
study_sites <- sp::spsample(as_Spatial(districts), n=10, type="random")
study_sites <- sf::st_as_sf(SpatialPointsDataFrame(study_sites, data.frame(SiteID = 1:10)))

# visualise districts and study sites
ggplot() + 
  geom_sf(data=districts, fill="grey95") + 
  geom_sf(data=study_sites, col="red") +
  maptheme





# ============== Exercise 1: View forest cover in 2000 for study area using two different datasets ================


# -------- Dataset 1: ESA-CCI Land Cover from 2000 to 2018 ------------

# Annual mosaics at 300m resolution that provide decent thematic resolution and are given categorical classifications
# Each layer of the brick corresponds to a year
esa <- raster::brick("./data/esacci_timeseries.nc")
names(esa)
plot(esa)

# numeric values in the raster correspond to land cover classifications
# these are provided in the attached .csv which maps ESA classes to other broad classifications (including those used by the IPCC)
esa_class <- read.csv("./data/esacci_to_ipcc_cats.csv", stringsAsFactors = FALSE) %>%
  dplyr::rename("CCI_var"=1)
esa_class

# let's take a look at the CCI classes that are defined as "forest" under the IPCC classification scheme
# we can see it's a wide variety covering different degrees of cover and different sorts of forest canopy type
# "forest" includes both canopy cover > 15%, and also "mosaic" habitats with over 50% tree/shrub cover
esa_for_class <- esa_class[ esa_class$IPCC_class == "forest", ]
esa_for_class$CCI_class

# create a raster of forest cover at the start of the timeseries (2000) using the above categories
# logical operator here: returns value of "1" if the raster cell value is in the esa_for_class values
# we'll take a look at this shortly
esa_2000_for <- esa[[ 1 ]] %in% esa_for_class$CCI_var



# ---------- Dataset 2: Global Forest Change ("Hansen") ------------

# Hansen forest cover provides very fine temporal (yearly) and spatial (30m) resolution for a single thematic class: tree cover
# Benefit over ESA-CCI is much finer spatial resolution and much simpler interpretation (here "forest" just equals "trees")
# Drawback is less thematic information on forest type (e.g. evergreen, flooded, decidious)

# These data are also structured differently: we are provided with a base year raster of proportion tree cover in the first year of the timeseries (2000)
# And a "change" raster whose values denote the year in which the cell changed from "forest" to "non-forest" state

# Base year raster for 2000 (values denote % cover of trees)
gfc_base <- raster::raster("./data/hansen_forestcover_2000.tif")

# Change raster (values denote year of forest loss)
gfc_loss <- raster::raster("./data/hansen_forestchange_20012019.tif")




# --------- Visual comparison of forest cover in base year for all 6 districts -----------

# mask esa raster to only include cells within districts of interest
mask_esa <- fasterize::fasterize(districts, esa_2000_for, field="xx")
esa_2000_for <- raster::mask(esa_2000_for, mask_esa)

# same for gfc
mask_gfc <- fasterize::fasterize(districts, gfc_base, field="xx")
gfc_base <- raster::mask(gfc_base, mask_gfc)

# create plot of ESA
esadfx <- as.data.frame(esa_2000_for, xy=TRUE)
esadfx$layer <- ifelse(esadfx$layer == TRUE, 1, 0)
p1 <- ggplot() + 
  geom_raster(data=esadfx, aes(x, y, fill=layer)) +
  geom_sf(data=districts, fill=NA) + 
  geom_sf(data=study_sites, col="red") +
  scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover") +
  maptheme +
  ggtitle("ESA-CCI forest cover 2000")

# create plot of Hansen (takes much longer as 100 times more cells to be plotted)
gfcdfx <- as.data.frame(gfc_base, xy=TRUE)
p2 <- ggplot() + 
  geom_raster(data=gfcdfx, aes(x, y, fill=hansen_forestcover_2000)) +
  geom_sf(data=districts, fill=NA) + 
  geom_sf(data=study_sites, col="red") +
  scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover") +
  maptheme +
  ggtitle("Hansen forest cover 2000")

# plot the two alongside each other and save to examine in detail
# can see that the Hansen offers a much finer resolution perspective on patterns of tree cover across the study area
# this could be important!
p_comb <- gridExtra::grid.arrange(p1, p2, nrow=1)
ggsave(p_comb, file="./plots/ForestCover_2000_Comparison.png", device="png", units="in", width=12, height=6, dpi=600)





# ================== Exercise 2: Compare metrics of forest cover across the different districts ========================


# We'll calculate three metrics of forest cover in 2000 across the focal districts and compare how they describe the land cover configuration
# These are total area, proportion cover, and population-weighted cover



# --------------- Extract total forest area in each district -------------------

# for this we also need a raster of grid-cell area (because we need to multiply proportion cover by area of the grid cell to get total area)
# this creates a stack of the esa 2000 forest cover, and also the grid cell area
esa_to_extract <- raster::stack( esa_2000_for, raster::area(esa_2000_for) )
names(esa_to_extract) <- c("forest", "area")
plot(esa_to_extract)

# extract using exactextractr for each districts
# each item in extracted list is a dataframe of all grid cells contained within a given polygon, where rows are cells, and columns are layers of the rasterstack
# with forest (1/0/NA), area of grid cell (km2) and coverage fraction (how much of that grid cell is contained within polygon)
ex_esa <- exactextractr::exact_extract(esa_to_extract, districts)
ex_esa[[1]]

# write a function to multiply these together to get the total area for the district
# use sapply to apply the function to each district and return result
calcForestArea <- function(x){ return( sum(x$forest * x$area * x$coverage_fraction, na.rm=TRUE) ) }  
result <- data.frame(district = shp$areanameen, ESACCI = sapply(ex_esa, calcForestArea))

# do the same for Hansen (GFC) and add to result dataframe
# n.b. here we divide the value of gfc_base by 100 to give proportion rather than % cover
gfc_to_extract <- raster::stack(gfc_base/100, area(gfc_base))
names(gfc_to_extract) <- c("forest", "area")
ex_gfc <- exactextractr::exact_extract(gfc_to_extract, districts)
result$GFC <- sapply(ex_gfc, calcForestArea)




# ---------------- Plot outputs to compare forest cover estimates between districts -----------------

# Metric 1: Total forest cover area
# we can see that relative differences between districts are similar, but GFC usually shows substantially higher forest cover
# this is because marginal/fragmented areas w/ under threshold cover for ESA to classify as "forest" are still included because of the higher spatial res
# this difference might be meaningful if you're studying diseases associated strongly with fragmented/ecotonal landscapes (e.g. yellow fever)
dat <- result %>% reshape2::melt(id.vars=1)
p_totalarea <- ggplot(dat) + 
  geom_bar(aes(district, value, fill=variable), stat="identity", position=position_dodge()) + 
  theme_classic() + 
  scale_fill_viridis_d(begin=0.2, end=0.7, name="") +
  ylab("Forest area (km2)")

# Metric 2: Proportion cover of forest
# This standardises the total forest area by district size, and so enables a more meaningful comparison between different districts
# Given that they vary substantially in area (from 600 to 1300 km2)
result$total_area_km2 <- as.vector(sf::st_area(shp)/10^6)
dat <- result %>% reshape2::melt(id.vars=c(1, 4))
dat$propcover <- dat$value / dat$total_area_km2
p_proparea <- ggplot(dat) + 
  geom_bar(aes(district, propcover, fill=variable), stat="identity", position=position_dodge()) + 
  theme_classic() + 
  scale_fill_viridis_d(begin=0.2, end=0.7, name="") +
  ylab("Proportion forest cover")




# ------------------- Calculating population-weighted forest cover ----------------------

# For epi/infectious disease purposes we're usually interested in how *exposure* to some covariate influences relative risk.
# Often the disease data we have are aggregated to the admin unit level, e.g. counts per month per district.

# One of the challenges of pairing these with district-level land cover estimates is that the relationship between summary LC metrics (e.g. proportion cover)
# and population "exposure" to land cover is highly dependent on how land cover and populations are spatially distributed across the district.
# This is different everywhere, and also depends on the size and shape of the district.
# Consequently, it's difficult to understand how stable and thus comparable this relationship (prop cover -> exposure) is between districts
# aka the "modifiable areal unit problem", aka the bane of life when working with polygon data.

# But what if we flip the question and instead ask, what proportion of the population are "exposed" to forest?
# We can approximate this by calculating the proportion of the population that fall within grid cells defined as forest
# (with the caveat that grid cell resolution (e.g. 300 * 300m) != human daily movement range, but let's ignore that for now).
# This does require us to decide on some theshold beyond which a grid cell is defined as "forest".
# For ESA this is predecided for us, as the dataset is categorical (i.e. cells are either "forest" or not).
# We'd have to decide on this approach for Hansen, for example, saying a grid cell is "forest" if it contains over 50% tree cover.

# Load fine-scale population raster (persons per pixel) from WorldPop at 100m resolution.
# (big caveat: these layers are modelled with certain landcover classes as covariates,
# so this approach requires some careful thought about underlying dependencies/common-cause/collinearity, aka, here be dragons)
pop <- raster::raster("./data/population_2000.tif")
plot(log(pop))

# Estimate population weighted cover for ESA: first aggregate pop to same resolution as ESA (to 300m), harmonise extents and stack
pop_esa <- raster::aggregate(pop, fact=3)
pop_esa <- raster::crop(pop_esa, esa_2000_for)
pop_esa <- raster::resample(pop_esa, esa_2000_for, "ngb")
esa_to_extract <- raster::stack(esa_2000_for, pop_esa)
names(esa_to_extract) <- c("forest", "population")
plot(esa_to_extract)

# extract using exactextractr
ex_esa <- exactextractr::exact_extract(esa_to_extract, districts)

# calculate population weighted cover
calcPWForest = function(x){ 
  x <- x[ !is.na(x$forest), ] # remove NA cells at margin
  x$weight <- (x$population * x$coverage_fraction) / sum(x$population, na.rm=TRUE) # population weights
  return( sum(x$forest * x$weight * x$coverage_fraction, na.rm=TRUE) ) # apply weights
}  
result_pw <- data.frame(district = shp$areanameen, PopulationWeighted = sapply(ex_esa, calcPWForest))

# let's compare proportion cover to this weighted "exposure" metric
result_pw$TotalArea <- result$ESACCI
result_pw$ProportionCover <- result_pw$TotalArea / as.vector(sf::st_area(shp)/10^6)

# If we create a barplot to compare the two
# We can see there's a really huge difference for some districts, which creates a substantially different picture of forest "exposure".
dat <- result_pw[ , c(1,2,4)] %>% reshape2::melt(id.vars=c(1))
p_comparison <- ggplot(dat) + 
  geom_bar(aes(district, value, fill=variable), stat="identity", position=position_dodge()) + 
  theme_classic() + 
  scale_fill_viridis_d(begin=0.2, end=0.7, name="") +
  ylab("Forest cover metric")
p_comparison


# ----------------- Why are these differences so pronounced? ------------------------

# Let's take a look at Ninh Hoa in more detail, because there's such a big difference in the metrics shown in the previous graph.

district_to_compare = "Ninh Hoa"
nh <- districts[ districts$areanameen == district_to_compare, ]
esa_nh <- raster::crop(esa_2000_for, nh)
esa_nh <- raster::mask(esa_nh, fasterize::fasterize(nh, esa_nh, field="xx"))
pop_nh <- raster::crop(pop, nh)
pop_nh <- raster::mask(pop_nh, fasterize::fasterize(nh, pop_nh, field="xx"))

# plot forest
df1 <- as.data.frame(esa_nh, xy=TRUE)
df1$layer <- ifelse(df1$layer == TRUE, 1, 0)
p1 <- ggplot() + 
  geom_raster(data=df1, aes(x, y, fill=layer)) +
  geom_sf(data=nh, fill=NA) + 
  scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover") +
  maptheme +
  ggtitle("ESA-CCI forest cover 2000")

# plot population (log+1 for clarity)
df2 <- as.data.frame(pop_nh, xy=TRUE)
p2 <- ggplot() + 
  geom_raster(data=df2, aes(x, y, fill=log(population_2000+1))) +
  geom_sf(data=nh, fill=NA) + 
  scale_fill_gradientn(colours=colScale, na.value="white", name="Pop") +
  maptheme +
  ggtitle("Population 2000")

# plot alongside
p_comb <- gridExtra::grid.arrange(p1, p2, nrow=1)
ggsave(p_comb, file="./plots/NinhHoa_ForestCover_Population_comparison.png", device="png", units="in", width=12, height=6, dpi=600)

# Visualising both for Ninh Hoa shows that the centre of the district is urbanised and much more focally populated than the margins.
# Whereas the forested area, although very large, covers mainly the less densely-populated margins of the district
# so a simple proportion cover would overestimate the exposure of the population to forested areas
# (assuming minimal human movement between cells, which is a simplification but one we'll stick with for now).

# For an interesting extra exercise, compare this to Krong Pac district, which has an almost identical value for the two metrics.
# Change the district_to_compare variable above to "Krong Pac" and run the code to plot it -- can you see what's different in this instance?




# ====================== Exercise 3: Temporal trends in forest cover change at the district-level =========================

# Next, let's examine what the different datasets suggest about how forest cover changed in our focal districts between 2001 and 2018.
# If we were deriving a covariate, we could take a similar approach to the above and try to roughly approximate "forest loss exposure" within the district over time
# Here the main focus is to compare how similar (or different) the two time series of forest change look for the different source datasets



# ------------- ESA-CCI ------------

# Here we'll need all 19 raster layers (each corresponding to a year) and we'll classify the whole stack based on forest/not forest
# Again we'll stack with area to calculate area of forest at each stage
esa_for <- esa[[ 2:nlayers(esa) ]] %in% esa_for_class$CCI_var
esa_to_extract <- raster::stack(esa_for, raster::area(esa_for[[1]]))
names(esa_to_extract) <- c(paste("forest", 2001:2018, sep="_"), "area")
ex_esa <- exactextractr::exact_extract(esa_to_extract, districts)

# again, each item in the list corresponds to the district, and the dataframe contains forest estimates for each year 
ex_esa[[1]]

# Write a function to calculate the summary info we're interested in
calcForestChange = function(x){
  
  # the xth element of the list
  xx <- ex_esa[[ x ]]
  
  xx <- reshape2::melt(xx, id.vars = c("area", "coverage_fraction")) # make longitudinal
  xx <- xx %>%
    dplyr::filter(value == 1) %>% # keep only forested cells
    dplyr::mutate(area = area * coverage_fraction) %>% # multiply area by coverage fraction
    dplyr::group_by(variable) %>% # for each year
    dplyr::summarise(ForestCover = sum(area, na.rm=TRUE))
  
  # forest change from 1 year to the next 
  # n.b. we set 2001 to NA because we have no data to compare from the year before
  xx$ForestChange <- c(NA, xx$ForestCover[ 2:nrow(xx)] - xx$ForestCover[ 1:(nrow(xx)-1)])
  
  # cumulative forest change
  xx$ForestChangeCumulative <- c(NA, cumsum(xx$ForestChange[ 2:nrow(xx)] ))
  
  # finally, set variable to the year and index with the district name
  xx$variable <- unlist(lapply(strsplit(as.vector(xx$variable), "_"), "[", 2))
  xx <- rename(xx, "Year" = variable)
  xx$district <- districts$areanameen[x]
  return(xx)
}

# run for all districts
result_esa <- do.call(rbind.data.frame, lapply(1:length(ex_esa), calcForestChange))

# plot
#ggplot(result_esa) + geom_line(aes(as.integer(Year), -ForestChangeCumulative)) + facet_wrap(~district) + geom_hline(yintercept=0, lty=2)



# -------------------- GFC Hansen data --------------------------

# Here we can work with the Hansen change raster, which encodes the year in which the grid cell changed from "forest" to "non forest"
# But we still need the base layer to tell us what the starting proportion cover was
# And also n.b. that this only encodes annual *loss* (i.e. it does not show if forest was gained during that time)
gfc_to_extract <- raster::stack(gfc_base, gfc_loss, raster::area(gfc_base))
names(gfc_to_extract) <- c("basecover", "lossyear", "area")
ex_gfc <- exactextractr::exact_extract(gfc_to_extract, districts)

# Here, again, each element of the list contains all our extracted information per district
head(ex_gfc[[1]])

# So let's write a function to calculate yearly estimated area of forest lost
calcForestLossGFC = function(x){
  
  # for the xth element
  xx <- ex_gfc[[x]]
  xx <- xx[ !is.na(xx$basecover), ]
  
  # calculate a column that denotes area of forest per grid cell in base year
  xx$forest_baseyear <- (xx$basecover/100) * xx$area * xx$coverage_fraction
  
  # remove all cells that did not lose forest (coded 0) and calculate total area lost per year based on other years
  xx <- xx[ xx$lossyear != 0, ]
  xx$lossyear <- xx$lossyear + 2000 # add 2000 to get year
  xx <- xx %>%
    dplyr::group_by(lossyear) %>%
    dplyr::summarise(ForestLoss = sum(forest_baseyear))
  
  # calculate cumulative loss
  xx$ForestLossCumulative <- cumsum(xx$ForestLoss)
  xx <- rename(xx, "Year" = 1)
  xx$district <- districts$areanameen[x]
  return(xx)
}

# run for all districts
result_gfc <- do.call(rbind.data.frame, lapply(1:length(ex_esa), calcForestLossGFC))




# ---------------- How do the time series calculated from the two differet datasets compare? ---------------

# Combine and plot the two together: these look really different!
# Part of this difference may be a consequence of ESA-CCI also including forest *gain*
# But these differences seem really striking - what's going on?
ggplot() + 
  geom_line(data = result_gfc, aes(as.integer(Year), ForestLossCumulative), col="red") +
  geom_line(data = result_esa, aes(as.integer(Year), -ForestChangeCumulative), col="blue") + 
  facet_wrap(~district) + geom_hline(yintercept=0, lty=2) + 
  theme_classic()


# Let's plot some district-specific maps comparing forest extent and loss between the two datasets - and see if we can
# What we can see is that this discrepancy seems to be largely caused by the tree cover losses (in Hansen dataset) 
# occurring in areas that were already classed as "not forest" by ESA-CCI. This is because the categorical classification
# Can only class 300m res cells as "forest/not forest", so cells that contain lots of tree-cover are still classed as "not forest" as they fall below this threshold
# This could be problematic for detecting the kinds of deforestation we might be interested in for zoonotic/VBD risk
# Where we're not only interested in very large-scale, detectable clear-cutting
# But also finer-scale fragmentation/edge effects that create human-wildlife-vector interfaces
# Highlights the reason why spatio-temporal resolution on the kind of thematic class we're interested in is so important
# N.B. there is no "true" dataset and both subject to error


### RG: add an extra layer showing the forest in 2000 coloured in grey, and areas of loss overlaid in red

district_to_compare = "Krong Bong"
tc <- districts[ districts$areanameen == district_to_compare, ]
esa_change_for <- raster::crop(esa_for, tc)
esa_change_for <- esa_change_for[[1]] - esa_change_for[[18]] 
esa_change_for <- esa_change_for > 0
gfc_change_for <- raster::crop(gfc_loss, tc)
esa_change_for <- raster::mask(esa_change_for, fasterize::fasterize(tc, esa_change_for, field="xx"))
gfc_change_for <- raster::mask(gfc_change_for, fasterize::fasterize(tc, gfc_change_for, field="xx"))

# plot esa
df1 <- as.data.frame(esa_change_for, xy=TRUE)
df1$layer <- ifelse(df1$layer == TRUE, 1, 0)
p1 <- ggplot() + 
  geom_raster(data=df1, aes(x, y, fill=layer)) +
  geom_sf(data=tc, fill=NA) + 
  scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover loss") +
  maptheme +
  ggtitle("ESA-CCI forest cover loss")

# plot hansen
df2 <- as.data.frame(gfc_change_for, xy=TRUE)
df2$hansen_forestchange_20012019 <- ifelse(df2$hansen_forestchange_20012019 > 1, 1, 0)
p2 <- ggplot() + 
  geom_raster(data=df2, aes(x, y, fill=hansen_forestchange_20012019)) +
  geom_sf(data=tc, fill=NA) + 
  scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover loss") +
  maptheme +
  ggtitle("Hansen forest cover loss")

p_comb <- gridExtra::grid.arrange(p1, p2, nrow=1)



# ===================== Exercise 4: Spatial and temporal trends in forest and deforestation exposure at our study sites =========================