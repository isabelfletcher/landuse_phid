

# ======================= LAND COVER-LAND USE: PHID TUTORIAL =========================

## Rory Gibb
## PHID Group meeting, 18/02/2021

## Tutorial outline:

# We'll load in some spatial district and point-level data for a study area (a handful of districts in Vietnam) and explore forest cover and how it's changed over time.
# To do this, we'll...
# 1. Visualise forest cover across the study area using two different datasets
# 2. Compare the differences between various summary metrics of forest cover at the district-level and look for problems
# 3. Use two datasets to derive time series of forest loss at district-level, and investigate discrepancies between products
# 4. Use fine-scale forest loss data to examine deforestation trends at several focal "study sites"




# ------------- Setup and dependencies ----------------

# directory
setwd("C:/Users/roryj/Documents/PhD/202007_lshtm_dengue/teaching/phid_landuse/landuse_phid/")

# install and load dependencies
# the first line of library() calls are packages that are core to using R as a GIS
#install.packages(c("raster", "rgdal", "sp", "sf", "exactextractr", "ncdf4", "ggplot2", "dplyr", "fasterize", "RColorBrewer", "gridExtra"))
library(raster); library(rgdal); library(sp); library(sf); library(exactextractr); library(ncdf4)
library(ggplot2); library(fasterize); library(RColorBrewer); library(gridExtra); library(dplyr)

# ggplot theme for mapping
maptheme <- theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14, hjust=0.5),
        axis.title = element_blank(),
        strip.background = element_blank())

# nice colour scale for mapping
colScale <- colorRampPalette(RColorBrewer::brewer.pal(9, name="YlGnBu"))(300)
  



# =================== Load and view areal data ========================

# Let's start by loading a shapefile of district polygons for our area of interest:
# a cluster of districts in the central coastal-to-highlands transition in Vietnam (Dak Lak and Khanh Hoa provinces).
# Sections of this region have been hotspots of recent forest loss in the country, and we'd like to examine this in more detail.
districts <- sf::st_read("./data/focal_districts.shp")
districts$xx <- 1

# I've generated and pre-saved some point location data (these could be geolocated study sites/disease case occurrences)
# study_sites <- sp::spsample(as_Spatial(districts), n=10, type="random")
# study_sites <- sf::st_as_sf(SpatialPointsDataFrame(study_sites, data.frame(SiteID = 1:10)))
# write.csv(as.data.frame(as_Spatial(study_sites)) %>% dplyr::rename("x"=2, "y"=3), "./data/studysite_locations.csv", row.names=FALSE)
study_sites <- read.csv("./data/studysite_locations.csv")
study_sites <- st_as_sf(study_sites, coords=c("x", "y"), remove=FALSE)
st_crs(study_sites) <- st_crs(districts)

# visualise districts and study sites
ggplot() + 
  geom_sf(data=districts, fill="grey95") + 
  geom_sf(data=study_sites, col="red") +
  maptheme





# ============== Exercise 1: View forest cover in 2000 for study area using two different datasets ================

# We'll start by viewing what the distribution of tree cover looks like across our entire study area in a baseline year of 2000
# To do this we'll use and compare 2 datasets, firstly the ESA-CCI annual land cover data product at 300m resolution
# And secondly, the Hansen Global Forest Change, Landsat-derived dataset at 30m resolution (i.e. super high-res)


# -------- Dataset 1: ESA-CCI Land Cover from 2000 to 2018 ------------

# This dataset contains annual mosaics at 300m resolution that provide decent thematic resolution and are given categorical classifications
# Each layer of the brick corresponds to a year, which we can see if we plot them.
esa <- raster::brick("./data/esacci_timeseries.nc")
names(esa)
plot(esa)

# The numeric values in the raster correspond to land cover classifications
# These are provided in the associated .csv which maps ESA classes to other broad classifications (including those used by the IPCC)
esa_class <- read.csv("./data/esacci_to_ipcc_cats.csv", stringsAsFactors = FALSE) %>%
  dplyr::rename("CCI_var"=1)
esa_class

# Let's take a look at the CCI classes that are defined as "forest" under the IPCC classification scheme.
# We can see it's a wide variety covering different degrees of cover and different sorts of forest canopy type
# so "forest" includes both canopy cover > 15%, and also "mosaic" habitats with over 50% tree/shrub cover.
esa_for_class <- esa_class[ esa_class$IPCC_class == "forest", ]
esa_for_class$CCI_class

# We create a raster of forest cover at the start of the timeseries (2000) using the above categories
# The logical operator here (%in%) returns value of "1" if the raster cell value is in the esa_for_class values
# We'll take a look at this shortly.
esa_2000_for <- esa[[ 1 ]] %in% esa_for_class$CCI_var



# ---------- Dataset 2: Global Forest Change ("Hansen") ------------

# Hansen forest cover provides very fine temporal (yearly) and spatial (30m) resolution for a single thematic class: tree cover
# One benefit over ESA-CCI is much finer spatial resolution and much simpler interpretation (here "forest" just equals "trees")
# One drawback is that there is less thematic information on forest type (e.g. evergreen, flooded, decidious)

# These data are also structured differently: we are provided with a base year raster of proportion tree cover in the first year of the timeseries (2000)
# And a "change" raster whose values denote the year in which the cell changed from "forest" to "non-forest" state

# Base year raster for 2000 (values denote % cover of trees)
gfc_base <- raster::raster("./data/hansen_forestcover_2000.tif")

# Change raster (values denote year of forest loss)
gfc_loss <- raster::raster("./data/hansen_forestchange_20012019.tif")




# --------- A visual comparison of forest cover in base year for all 6 districts -----------

# Subset rasters to only the area of interest for nice plotting.
# We mask esa raster to only include cells within districts of interest
mask_esa <- fasterize::fasterize(districts, esa_2000_for, field="xx")
esa_2000_for <- raster::mask(esa_2000_for, mask_esa)

# And the same for gfc
mask_gfc <- fasterize::fasterize(districts, gfc_base, field="xx")
gfc_base <- raster::mask(gfc_base, mask_gfc)

# Create a plot of ESA
esadfx <- as.data.frame(esa_2000_for, xy=TRUE)
esadfx$layer <- ifelse(esadfx$layer == TRUE, 1, 0)
p1 <- ggplot() + 
  geom_raster(data=esadfx, aes(x, y, fill=layer)) +
  geom_sf(data=districts, fill=NA) + 
  geom_sf(data=study_sites, col="red") +
  scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover") +
  maptheme +
  ggtitle("ESA-CCI forest cover 2000")

# Create a plot of Hansen (takes much longer as 100 times more cells to be plotted)
gfcdfx <- as.data.frame(gfc_base, xy=TRUE)
p2 <- ggplot() + 
  geom_raster(data=gfcdfx, aes(x, y, fill=hansen_forestcover_2000)) +
  geom_sf(data=districts, fill=NA) + 
  geom_sf(data=study_sites, col="red") +
  scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover") +
  maptheme +
  ggtitle("Hansen forest cover 2000")

# Combine to plot the two alongside each other, and save to examine in detail
# We can see that the Hansen offers a much finer resolution perspective on patterns of tree cover across the study area
# This extra detail could be important!
p_comb <- gridExtra::grid.arrange(p1, p2, nrow=1)
ggsave(p_comb, file="./plots/ForestCover_2000_Comparison.png", device="png", units="in", width=12, height=6, dpi=600)



# ------------ So what's the benefit of a dataset like ESA-CCI, then? ---------------

# Although ESA-CCI lacks fine-scale spatial resolution of specific land cover phenomena (such as tree cover) 
# What it provides is good thematic and temporal resolution; the full database provides annual rasters of 37 LCLU classes from 1992 to 2019
# So, for example, we might instead be interested in the amount of irrigated cropland in our study area (e.g. rice paddies as mosquito breeding sites)
# We can do this using the same approach - and this isn't something that a dataset like Hansen GFC provides.

# Let's look at irrigated cropland cover at the start and end of the time series
esa_crop <- esa[[ c(1, nlayers(esa)) ]] %in% esa_class$CCI_var[ esa_class$CCI_class == "irrigated_cropland" ]
esa_crop <- raster::mask(esa_crop, mask_esa)
esadf <- as.data.frame(esa_crop, xy=TRUE) %>%
  dplyr::rename("Irrigated_2000"=3, "Irrigated_2018"=4) %>%
  reshape2::melt(id.vars = 1:2) %>%
  dplyr::mutate(value = ifelse(value==TRUE, 1, 0))

# We can see that there's a relatively small amount of irrigated cropland, that it's largely concentrated in the furthest east of the study area (Ninh Hoa),
# And also that its extent hasn't changed too much over the time series.
p3 <- ggplot() + 
  geom_raster(data=esadf, aes(x, y, fill=value)) +
  geom_sf(data=districts, fill=NA) + 
  geom_sf(data=study_sites, col="red") +
  scale_fill_gradientn(colours=colScale, na.value="white", name="Irrigated\ncover") +
  maptheme + 
  facet_wrap(~variable)
p3

# The COPERNICUS land cover data provides a similar level of thematic detail with the added benefit of good spatial resolution
# As well as fractional grid cell cover estimates (e.g. x% of the cell is taken up by a particular class)
# However it's only available from 2015 to 2019, so lacks the long-term perspective offered by ESA-CCI.





# ================== Exercise 2: Compare metrics of forest cover between districts ========================

# We'll calculate three metrics of forest cover in 2000 across the focal districts and compare how they describe the land cover configuration
# These are total area, proportion cover, and population-weighted cover



# --------------- Extract total forest area in each district -------------------

# For this we also need a raster of grid-cell area (because we need to multiply proportion cover by area of the grid cell to get total area)
# So we create a stack of the esa 2000 forest cover, with the grid cell area
esa_to_extract <- raster::stack( esa_2000_for, raster::area(esa_2000_for) )
names(esa_to_extract) <- c("forest", "area")
plot(esa_to_extract)

# extract using exactextractr for each district
# each item in the extracted list is a dataframe of all grid cells contained within a given polygon, where rows are cells, and columns are layers of the rasterstack
# with forest (1/0/NA), area of grid cell (km2) and coverage fraction (how much of that grid cell is contained within polygon)
ex_esa <- exactextractr::exact_extract(esa_to_extract, districts)
ex_esa[[1]]

# We write a function to multiply these together to get the total area for the district
# and use sapply to apply the function to each district and return result
calcForestArea <- function(x){ return( sum(x$forest * x$area * x$coverage_fraction, na.rm=TRUE) ) }  
result <- data.frame(district = districts$areanameen, ESACCI = sapply(ex_esa, calcForestArea))

# Then we do the same for Hansen (GFC) and add to result dataframe
# (n.b. here we first divide the value of gfc_base by 100 to give proportion rather than % cover)
gfc_to_extract <- raster::stack(gfc_base/100, area(gfc_base))
names(gfc_to_extract) <- c("forest", "area")
ex_gfc <- exactextractr::exact_extract(gfc_to_extract, districts)
result$GFC <- sapply(ex_gfc, calcForestArea)




# ---------------- Plot outputs to compare forest cover estimates between districts -----------------

# Let's examine metric 1: total forest cover area.
# We can see that relative differences between districts are fairly similar, but GFC usually shows substantially higher forest cover.
# This is because marginal/fragmented areas of tree cover that are under threshold amount of cover for ESA to classify as "forest" 
# are still included, as a consequence of the higher spatial resolution (GFC provides 10 times finer detail).
# This difference might be meaningful if, for example, you're studying diseases associated strongly with fragmented/ecotonal landscapes (e.g. yellow fever, frontier malaria)
dat <- result %>% reshape2::melt(id.vars=1)
p_totalarea <- ggplot(dat) + 
  geom_bar(aes(district, value, fill=variable), stat="identity", position=position_dodge()) + 
  theme_classic() + 
  scale_fill_viridis_d(begin=0.2, end=0.7, name="") +
  ylab("Forest area (km2)")
p_totalarea

# Now metric 2: proportion of the district area that is covered by forest ("proportion cover").
# This approach standardises the total forest area by district size, and so enables a more meaningful comparison between different districts
# given that they vary substantially in area (from 600 to 1300 km2)
result$total_area_km2 <- as.vector(sf::st_area(districts)/10^6)
dat <- result %>% reshape2::melt(id.vars=c(1, 4))
dat$propcover <- dat$value / dat$total_area_km2
p_proparea <- ggplot(dat) + 
  geom_bar(aes(district, propcover, fill=variable), stat="identity", position=position_dodge()) + 
  theme_classic() + 
  scale_fill_viridis_d(begin=0.2, end=0.7, name="") +
  ylab("Proportion forest cover")
p_proparea



# ------------------- Calculating population-weighted forest cover ----------------------

# For epi/infectious disease purposes we're usually interested in how *exposure* to some covariate influences relative risk.
# Often the disease data we have are aggregated to the administrative unit level, e.g. counts per month per district.

# One of the challenges of pairing these with district-level land cover estimates is that the relationship between summary LC metrics (e.g. proportion cover)
# and population "exposure" to land cover is highly dependent on how land cover and populations are spatially distributed across the district.
# This is different everywhere and depends on the size and shape of the district as well as, for example, its agricultural and urbanisation characteristics.
# Consequently, it's difficult to understand how stable and thus comparable this relationship (prop cover -> exposure) is between districts.
# This is an example of the "modifiable areal unit problem", aka the bane of life when working with polygon data.

# But what if we flip the question and instead ask, what proportion of the population are "exposed" to forest?
# We can approximate this by calculating the proportion of the population that fall within grid cells defined as forest
# (with the caveat that grid cell resolution (e.g. 300 * 300m) != human daily movement range, but let's ignore that for now).
# This does require us to decide on some theshold beyond which a grid cell is defined as "forest".
# For ESA this is predecided for us, as the raster dataset is categorical; i.e. cells are either "forest" or not.
# We'd have to decide on this approach for Hansen where grid cells contain proportion cover; for example, by deciding a grid cell is "forest" if it contains over 50% tree cover.

# Load fine-scale population raster (persons per pixel) from WorldPop at 100m resolution.
# (big caveat on the WorldPop data: these layers are modelled with certain landcover classes included as predictor covariates,
# so this approach requires some careful thought about underlying dependencies/common-cause/collinearity, aka, here be dragons)
pop <- raster::raster("./data/population_2000.tif")
plot(log(pop))

# Estimate population weighted cover for ESA: first aggregate population to same resolution as ESA (to 300m), then harmonise extents and stack
pop_esa <- raster::aggregate(pop, fact=3)
pop_esa <- raster::crop(pop_esa, esa_2000_for)
pop_esa <- raster::resample(pop_esa, esa_2000_for, "ngb")
esa_to_extract <- raster::stack(esa_2000_for, pop_esa)
names(esa_to_extract) <- c("forest", "population")
plot(esa_to_extract)

# extract using exactextractr
ex_esa <- exactextractr::exact_extract(esa_to_extract, districts)

# function to calculate population weighted cover for each district
calcPWForest = function(x){ 
  x <- x[ !is.na(x$forest), ] # remove NA cells at margin
  x$weight <- (x$population * x$coverage_fraction) / sum(x$population, na.rm=TRUE) # population weights
  return( sum(x$forest * x$weight * x$coverage_fraction, na.rm=TRUE) ) # apply weights
}  
result_pw <- data.frame(district = districts$areanameen, PopulationWeighted = sapply(ex_esa, calcPWForest))

# Create a dataframe comparing proportion cover to this weighted "exposure" metric
result_pw$TotalArea <- result$ESACCI
result_pw$ProportionCover <- result_pw$TotalArea / as.vector(sf::st_area(districts)/10^6)

# Now, when we create a barplot to compare the two, we can see there's a really huge difference for some districts.
# This creates a substantially different picture of forest "exposure" than proportion cover would suggest. Why is this?
dat <- result_pw[ , c(1,2,4)] %>% reshape2::melt(id.vars=c(1))
p_comparison <- ggplot(dat) + 
  geom_bar(aes(district, value, fill=variable), stat="identity", position=position_dodge()) + 
  theme_classic() + 
  scale_fill_viridis_d(begin=0.2, end=0.7, name="") +
  ylab("Forest cover metric")
p_comparison



# ----------------- Why are these differences so pronounced? ------------------------

# Let's take a look at Ninh Hoa in more detail, because there's such a big difference in the proportion cover/population weighted metrics.

# crops and masks population and forest cover raster to the specified district
district_to_compare <- "Ninh Hoa"
nh <- districts[ districts$areanameen %in% district_to_compare, ]
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
# Whereas the forested area, although large, covers mainly the less densely-populated margins of the district.
# So a simple proportion cover would overestimate the exposure of the population to forested areas
# (assuming minimal human movement between cells, which is a simplification but one we'll stick with for now).
# This plot also shows up a few weird quirks of the ESA-CCI data that are an issue in some parts of the world especially in the early part
# of the time series (a certain blockiness that is not likely to reflect on-the-ground reality).

# For an interesting extra exercise, compare this to Krong Pac district, which has an almost identical value for the two metrics.
# Change the district_to_compare variable above to "Krong Pac" and run the code to plot it -- can you see what's different in this instance?





# ====================== Exercise 3: Temporal trends in forest cover change at the district-level =========================

# Next, let's examine what the different datasets suggest about how forest cover changed in our focal districts between 2001 and 2018.
# If we were deriving a model covariate, we could take a similar approach to the above and try to roughly approximate "forest loss exposure" within the district over time
# But here our main focus is simply to compare how similar (or different) the two time series of forest change look for the different source datasets



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
  datx <- ex_esa[[ x ]]
  
  datx <- reshape2::melt(datx, id.vars = c("area", "coverage_fraction")) # make longitudinal
  datx <- datx %>%
    dplyr::filter(value == 1) %>% # keep only forested cells
    dplyr::mutate(area = area * coverage_fraction) %>% # multiply area by coverage fraction
    dplyr::group_by(variable) %>% # for each year
    dplyr::summarise(ForestCover = sum(area, na.rm=TRUE))
  
  # forest change from 1 year to the next 
  # n.b. we set 2001 to NA because we have no data to compare from the year before
  datx$ForestChange <- c(NA, datx$ForestCover[ 2:nrow(datx)] - datx$ForestCover[ 1:(nrow(datx)-1)])
  
  # cumulative forest change
  datx$ForestChangeCumulative <- c(NA, cumsum(datx$ForestChange[ 2:nrow(datx)] ))
  
  # finally, set variable to the year and index with the district name
  datx$variable <- unlist(lapply(strsplit(as.vector(datx$variable), "_"), "[", 2))
  datx <- rename(datx, "Year" = variable)
  datx$id <- x
  return(datx)
}

# run for all districts and add in district name
result_esa <- do.call(rbind.data.frame, lapply(1:length(ex_esa), calcForestChange))
result_esa <- left_join(result_esa, data.frame(id = 1:nrow(districts), district=districts$areanameen))


# plot
#ggplot(result_esa) + geom_line(aes(as.integer(Year), -ForestChangeCumulative)) + facet_wrap(~district) + geom_hline(yintercept=0, lty=2)



# -------------------- GFC Hansen data --------------------------

# Here we need to work with the Hansen change raster, which encodes the year in which the grid cell changed from "forest" to "non forest"
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
    dplyr::summarise(ForestLoss = sum(forest_baseyear)) %>%
    dplyr::arrange(lossyear) %>%
    dplyr::mutate(ForestLossCumulative = cumsum(ForestLoss),
                  id = x) %>%
    dplyr::rename("Year"=1)
  
  return(xx)
}

# run for all districts and add district names
result_gfc <- do.call(rbind.data.frame, lapply(1:length(ex_gfc), calcForestLossGFC))
result_gfc <- left_join(result_gfc, data.frame(id = 1:nrow(districts), district=districts$areanameen))



# ---------------- How do the time series calculated from the two differet datasets compare? ---------------

# Combine and plot the two together: these trends look really different!
# Part of this difference may be a consequence of ESA-CCI also including forest *gain* whereas Hansen only includes loss in this subsection of the data
# But these differences seem really striking - so what's going on?
ggplot() + 
  geom_line(data = result_gfc, aes(as.integer(Year), ForestLossCumulative), col="red") +
  geom_line(data = result_esa, aes(as.integer(Year), -ForestChangeCumulative), col="blue") + 
  facet_wrap(~district) + geom_hline(yintercept=0, lty=2) + 
  theme_classic()


# One way to examine this is to visualise what forest losses look like spatially and in relation to the extent of baseline forest
# So let's create a function to combine these in a single plot and compare where the two datasets are showing forest losses

# function to plot the baseline forest cover and overlay with areas of expected loss
# argument "districtx" is a single district name or a vector of names to include
comparisonPlot <- function(districtx){
  
  # shapefile for district
  dx <- districts[ districts$areanameen %in% districtx, ]
  
  # crop and mask forest extent in base year
  base_esa <- raster::crop(esa_2000_for, dx)
  base_esa <- raster::mask(base_esa, fasterize::fasterize(dx, base_esa, field="xx"))
  base_gfc <- raster::crop(gfc_base, dx)
  base_gfc <- raster::mask(base_gfc, fasterize::fasterize(dx, base_gfc, field="xx"))  
  
  # layers showing areas where forest was lost
  loss_esa <- (esa_for[[1]] - esa_for[[18]]) > 0
  loss_esa <- crop(loss_esa, dx)
  loss_esa <- raster::mask(loss_esa, fasterize::fasterize(dx, loss_esa, field="xx"))  
  loss_gfc <- crop(gfc_loss > 0, dx)
  loss_gfc <- raster::mask(loss_gfc, fasterize::fasterize(dx, loss_gfc, field="xx"))
  
  # for plotting
  df1 <- as.data.frame(base_esa, xy=TRUE) %>%
    dplyr::mutate(layer = ifelse(layer==TRUE, 1, 0))
  df2 <- as.data.frame(loss_esa, xy=TRUE) %>%
    dplyr::mutate(layer = ifelse(layer==TRUE, 1, 0)) %>%
    dplyr::filter(layer == 1 & !is.na(layer))
  px <- ggplot() + 
    geom_raster(data=df1, aes(x=x, y=y, fill=layer)) + 
    geom_raster(data=df2, aes(x=x, y=y), fill="red", alpha=0.7) +
    scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover\n2000") +
    geom_sf(data=dx, fill=NA) + 
    maptheme + 
    ggtitle("ESA-CCI forest cover (2000) and loss (2001-2019)")
  
  df3 <- as.data.frame(base_gfc, xy=TRUE)
  df4 <- as.data.frame(loss_gfc, xy=TRUE) %>%
    dplyr::mutate(layer = ifelse(layer==TRUE, 1, 0)) %>%
    dplyr::filter(layer == 1 & !is.na(layer))
  py <- ggplot() + 
    geom_raster(data=df3, aes(x=x, y=y, fill=hansen_forestcover_2000)) + 
    geom_raster(data=df4, aes(x=x, y=y), fill="red", alpha=0.7) +
    scale_fill_gradientn(colours=colScale, na.value="white", name="Forest\ncover\n2000") +
    geom_sf(data=dx, fill=NA) + 
    maptheme + 
    ggtitle("Hansen forest cover (2000) and loss (2001-2019)")
  
  return(gridExtra::grid.arrange(px, py, nrow=1))
}

# specific districts
# p1 = comparisonPlot("Mdrak District")
# p2 = comparisonPlot("Khanh Vinh")

# full area
p3 = comparisonPlot(districts$areanameen)
ggsave(p3, file="./plots/ForestLoss_ESA_GFC_comparison.png", device="png", units="in", width=12, height=6, dpi=600)

# The full plot across the entire study area makes it clearer what's driving this discrepancy between datasets:
# the majority of detected tree cover loss shown by the Hansen dataset occurred in areas that were not classified as forest by ESA even in the base year.
# This is because the 300m resolution of ESA-CCI and categorical classifications (rather than proportion cover) means that cells can only be 
# classified as "forest" once they exceed a % cover threshold, below which they fall into a separate agri/shrubland mosaic category.
# This firstly means that a lot of tree cover is missed in the baseline map, but also that the change map potentially very significantly underestimates 
# the amount of tree cover loss that actually occurred in smaller stands, forest edges, fragments and areas of patchy crop/mosaic/forest cover,
# It also misses relatively small-scale losses of tree cover in cells that remain classed as "forest" in ESA because they're still above the classification threshold.
# (Although it's worth caveatting that without validation/ground-truthing there's no one "true" dataset here, and GFC also contains error so is not a gold standard)

# These discrepancies are definitely potentially an issue for disease modelling, because the kinds of risky interfaces 
# We're often particularly interested in for zoonotic/VBD risk are not necessarily driven by large-scale clear-cutting (which ESA would likely detect)
# But are often the kinds of border phenomena - fragmentation, edge effects, ecotonal agricultural landscapes - that facilitate people and ecological communities mixing
# So this comparison highlights why it's really important to try and select a dataset that's best suited to capturing the phenomenon that
# we're interested in investigating (by navigating that balance between spatial, temporal and thematic resolution).





# ===================== Exercise 4: Spatial and temporal trends in deforestation at our study sites =========================

# Lastly, let's take a look at using some point data to examine trends in forest loss at our geolocated study sites.
# The main point of this section is to highlight the value of having finer-scale, geolocated point data, rather than working with polygons
# as this makes it much easier to standardise and compare our land cover (change) exposure variables.
# Let's say that our study sites are villages for which we've collated data on the annual incidence of a disease, let's say malaria
# And we want to know if there's any evidence that deforestation rates are associated with higher risk of malaria.
# We've established from our analyses above that the ESA-CCI dataset may not be the best choice for capturing trends in tree cover *change*
# at the localised, epidemiologically-relevant scale. 
# So for this, we'll just focus on using the GFC data.

# Let's remind ourselves of the geographical distribution of our study districts 
ggplot() + 
  geom_sf(data=districts, fill="grey95") + 
  geom_sf(data=study_sites, col="red") +
  maptheme

# These are point locations, so extracting forest change in a single 30m grid cell at that location is unlikely to tell us anything useful about broad
# patterns of tree cover change in the surroundings of the village.
# What we can instead do is examine tree cover change within a specified radius (e.g. ~5km) of the focal point location.
# So let's make a buffer object that does this, first harmonising the coordinate reference system between our points and our forest raster
# N.B. We'll get a warning here about st_buffer; this is important (and requires some CRS transformation) but for this tutorial we'll ignore this as the error shouldn't be huge
sf::st_crs(study_sites) <- crs(gfc_loss)
buffer_5km <- sf::st_buffer(study_sites, 0.045) # The second argument specifies the radius in arc degrees (~111km == 1 degree at equator; 5km = 5/111)

# Now we can see what our buffer zones look like.
ggplot() + 
  geom_sf(data=districts, fill="grey95") + 
  geom_sf(data=buffer_5km, fill="coral2", color=NA, alpha=0.4) +
  geom_sf(data=study_sites, col="red") +
  maptheme

# The code and function we wrote above to calculate forest loss from the GFC data works just the same with buffers as with polygons, so we just use the same code
# But extract for the buffers instead, and add in the village IDs afterwards
ex_gfc <- exactextractr::exact_extract(gfc_to_extract, buffer_5km)
points_gfc <- do.call(rbind.data.frame, lapply(1:length(ex_gfc), calcForestLossGFC))
points_gfc <- left_join(points_gfc, data.frame(id = 1:nrow(study_sites), SiteID = study_sites$SiteID))

# Now we can look at what these trends look like for our 10 villages; firstly, let's examine cumulative forest loss over the monitoring period.
# We can see that a cluster of villages have experienced particularly high rates of surrounding tree cover loss, whereas others much less so, if any.
ggplot(points_gfc) +
  geom_line(aes(Year, ForestLossCumulative), col="darkred") +
  facet_wrap(~SiteID) + 
  theme_minimal()

# What do these trends look like in terms of annual rates of forest loss?
# Evidence from some validation exercises using the GFC data suggests that most tree cover loss classifications are correctly detected to within 1 year either side of the reported year
# (This lag may be to do with, for example, discrepancies between when during the year the Landsat image was taken, compared to when the stand was lost).
# So let's smooth our annual loss time series with a 3-year rolling mean around the focal year to attempt to smooth this out for visualisation.
points_gfc <- points_gfc %>%
  dplyr::arrange(SiteID, Year) %>%
  dplyr::group_by(SiteID) %>%
  dplyr::mutate(ForestLoss_Smoothed = data.table::frollmean(ForestLoss, 3, align="center"))

# When we plot them, we can see that the villages differ in terms of when forest loss rates were highest; some show high rates much later in the time series than others. 
ggplot(points_gfc) +
  geom_line(aes(Year, ForestLoss_Smoothed), col="darkred") +
  facet_wrap(~SiteID) + 
  theme_minimal() + 
  ylab("Forest loss rate (km2/year)")

# Finally let's make a map showing total forest loss over space (where size represents total loss over the time period)
gfc_finalyear <- points_gfc[ points_gfc$Year == 2019, ] %>% dplyr::select(SiteID, ForestLossCumulative)
study_sites <- left_join(study_sites, gfc_finalyear)
ggplot() + 
  geom_sf(data=districts, fill="grey95") + 
  geom_sf(data=study_sites, aes(size = ForestLossCumulative), alpha=0.3) +
  geom_sf(data=study_sites, col="red") +
  scale_size_continuous(range=c(1, 20), name="Total\nforest\nloss (km2)") +
  maptheme

# What's nice about these estimates is that they're directly comparable between villages and, all else being equal,
# can be interpreted as representing the same measure of exposure to deforestation; 
# this is because each represents forest change in a buffer of the exact same size around the focal village.
# Which is in contrast to the district-level extracted estimates which, as we saw above, 
# are highly sensitive to the size, shape and configuration of the socio-ecological environment of any given district.

# One final point to raise is a reminder that all remotely-sensed land cover products are derived from predictive statistical/ML image classification of the original satellite
# imagery, and as such are subject to predictive error and uncertainty, just like with any other modelled product.
# These issues may vary over space and time (for example, as we saw in the maps above, ESA-CCI can behave very weirdly in the early part of the time series)
# So it always worth, firstly, carefully visualising the land cover data for the time period of interest and cross-referencing multiple datasets to check for signifcant discepancies
# It's also worth checking the literature for studies validating the products of interest in or near your study region (for example, the accuracy of GFC on tropical forests appears to be
# very good for Latin America and SE Asia, but substantially less so for sub-Saharan Africa)
# Finally, always read the documentation carefully to check the product is suitable for the thing you're interested in using it for!


# ================================================== ENDS ========================================================

