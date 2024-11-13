## Spatial Autocorrelation Method

#! Steps
# 1. 

#! Read in MSA UA file
MSA.UA.2021 <- st_read("data/processed/MSA_UA_2021.shp")
#! Read in MSA WAC file
MSA.LEHD.WAC.2021 <- read.csv("data/processed/MSA_LEHD_WAC_2021.csv")

#! Clean WAC data, join to UA shapefile
MSA.LEHD.WAC.2021 <- MSA.LEHD.WAC.2021 %>%
  select(w_geocode, C000) %>%
  mutate(w_geocode = as.character(w_geocode)) %>%
  mutate(w_geocode = substr(w_geocode, 1, 11)) %>%
  `colnames<-`(c("w_geocode", "Jobs")) %>%
  group_by(w_geocode) %>%
  summarize(Jobs = sum(Jobs))

MSA.UA.2021 <- MSA.UA.2021 %>%
  left_join(MSA.LEHD.WAC.2021,
            by = c("GEOID" = "w_geocode")) %>%
  mutate(Jobs = ifelse(is.na(Jobs), 0, Jobs))

## Clear memory
rm(MSA.LEHD.WAC.2021)


#! Setting up dependencies for Moran's I
## Calculate area and job density of each tract (sqm)
MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(Area.sqm = as.numeric(st_area(.))) %>%
  mutate(Job.Density = Jobs / Area.sqm)

## Winsorize the employment density by capping at the 99th percentile
MSA.UA.2021$Job.Density <- pmin(MSA.UA.2021$Job.Density, 
                                quantile(MSA.UA.2021$Job.Density, 0.99))

## Find contiguity-based neighbors (Queen)
queen.neighbors <- poly2nb(MSA.UA.2021, queen = TRUE)

## Get the average number of neighbors per tract
mean.neighbors <- round(mean(sapply(queen.neighbors, length)))

## Create a KNN weighting matrix
knn.weights <- knearneigh(st_coordinates(st_centroid(MSA.UA.2021$geometry)),
                          k = mean.neighbors)
knn.list <- nb2listw(knn2nb(knn.weights), style = "W")

#! Run Local Moran's I and combine to tracts
local.moran <- localmoran(MSA.UA.2021$Job.Density, knn.list)

## Create a df that combines the output of the local moran's I
local.moran <- data.frame(local.moran, attr(local.moran, "quad"))
names(local.moran)[5] <-"p_val"
local.moran$row <- 1:dim(MSA.UA.2021)[1]

## Create a Variable to merge the two data sets
MSA.UA.2021$row <- 1:dim(MSA.UA.2021)[1]
MSA.UA.2021 <- merge(MSA.UA.2021, local.moran, by = "row", 
                     all.x = TRUE, sort = FALSE)

#! Identify subcenters based on P value (alpha = 0.05)
MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(Subcenter = ifelse(p_val < 0.05,
                            "Subcenter",
                            "Not a Subcenter")) %>%
  mutate(Category = ifelse(Subcenter == "Subcenter",
                           as.character(mean),
                           NA)) %>%
  filter(Subcenter == "Subcenter") %>%
  filter(Jobs > 0) %>%
  select(GEOID, Jobs, Category, Subcenter, geometry)

#! Clean up and write out final shapefile
st_write(MSA.UA.2021, "data/output/SpatialAutocorrelation_Subcenters.shp")

#! Read in Observations DF and update
observations.df <- read.csv("data/output/observations_df.csv")

## Dissolve contiguous UAs
adjacency.matrix <- st_touches(MSA.UA.2021)
g <- graph_from_adj_list(adjacency.matrix)
clusters <- components(g)$membership

MSA.UA.2021.observational <- MSA.UA.2021 %>%
  mutate(
    Cluster = clusters,
    Count = 1
  )

MSA.UA.2021.observational <- MSA.UA.2021.observational %>%
  group_by(Cluster) %>%
  summarize(
    geometry = st_union(geometry),
    Jobs = sum(Jobs),
    Count.UA = sum(Count)
  )

MSA.UA.2021.observational <- MSA.UA.2021.observational %>%
  mutate(Area.HA = as.numeric(st_area(.))) %>%
  mutate(Area.HA = Area.HA / 10000)

## Write relevant information to observation df
observations.df$N.Centers[5] <- nrow(MSA.UA.2021.observational)
observations.df$N.Tracts[5] <- sum(MSA.UA.2021.observational$Count.UA)
observations.df$Total.Jobs[5] <- sum(MSA.UA.2021.observational$Jobs, na.rm = T)
observations.df$Area.HA[5] <- sum(MSA.UA.2021.observational$Area.HA, na.rm = T)

write.csv(observations.df,
          "data/output/observations_df.csv",
          row.names = F)

