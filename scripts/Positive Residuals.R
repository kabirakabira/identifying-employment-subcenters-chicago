## Positive Residuals Method

#! Steps
# 1. Read in MSA UA and WAC files.
# 2. Clean up WAC cile, join to UA file.
# 3. Identify CBD, calculate distances to CBD.
# 4. Run a geographically-weighted regression.
# 5. Identify significantly positive tracts.
# 6. Dissolve contiguous tracts - these are subcenters.
# 7. Write out shapefile.

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

#! Identify CBD, calculate distance of all tracts from CBD. Dist in log(km).
MSA.CBD <- MSA.UA.2021 %>%
  filter(Jobs == max(Jobs))

MSA.UA.2021$DistCBD <- as.vector(st_distance(MSA.CBD %>% st_centroid,
                                             MSA.UA.2021 %>% st_centroid))

MSA.UA.2021$DistCBD <- log(MSA.UA.2021$DistCBD / 1000)
## CBD obs will show "Inf" because DistCBD = 0. Set to 0.
MSA.UA.2021$DistCBD <- ifelse(is.infinite(MSA.UA.2021$DistCBD), 0, MSA.UA.2021$DistCBD)

#! Run a GWR where y = Jobs, x = DistCBD. Note that DistCBD has already been log'd.
## Convert the UA from sf to sp
MSA.UA.2021.sp <- as(MSA.UA.2021, "Spatial")

## Find the optimal bandwidth first
gwr.bw <- gwr.sel(Jobs ~ DistCBD,
                  MSA.UA.2021.sp,
                  adapt = TRUE)

## Run the GWR
gwr.fit <- gwr(Jobs ~ DistCBD,
               data = MSA.UA.2021.sp,
               adapt = gwr.bw,
               se.fit = T,
               hatmatrix = T)

gwr.results <- as.data.frame(gwr.fit$SDF)

#! For each observation of the UAs, calculate the residuals and divide by the coefficient SE
## Join results to UAs
MSA.UA.2021 <- cbind(MSA.UA.2021,
                     select(gwr.results,
                            pred,
                            pred.se))

## Calculate Residuals/SE as (ActualVal - PredVal) / (SE of PredVal)
## Positivity is calculated at the 95% level with the critical value = 1.96
MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(
    pred = ifelse(pred < 0, 0, pred),
    resid = (Jobs - pred) / pred.se,
    Subcenter = ifelse(resid > 1.96, "Subcenter", "Not a Center")
  ) %>%
  filter(
    Subcenter == "Subcenter"
  )

#! Dissolve contiguous UAs
adjacency.matrix <- st_touches(MSA.UA.2021)
g <- graph_from_adj_list(adjacency.matrix)
clusters <- components(g)$membership

MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(
    Cluster = clusters,
    Count = 1
  )

MSA.UA.2021 <- MSA.UA.2021 %>%
  group_by(Cluster) %>%
  summarize(
    geometry = st_union(geometry),
    Jobs = sum(Jobs),
    DistCBD = mean(DistCBD),
    Count.UA = sum(Count)
  ) %>%
  filter(Jobs > 10000)

## Clear memory
rm(adjacency.matrix)
rm(g)
rm(MSA.CBD)
rm(MSA.UA.2021.sp)

#! Clean up final shapefile and write out
MSA.UA.2021 <- MSA.UA.2021 %>%
  select(Cluster, Jobs, DistCBD, Count.UA, geometry)

st_write(MSA.UA.2021,
         "data/output/PositiveResiduals_Subcenters.shp")

#! Read in Observations DF and update
observations.df <- read.csv("data/output/observations_df.csv")

MSA.UA.2021.observational <- MSA.UA.2021 %>%
  mutate(Area.HA = as.numeric(st_area(.))) %>%
  mutate(Area.HA = Area.HA / 10000)

## Write relevant information to observation df
observations.df$N.Centers[4] <- nrow(MSA.UA.2021.observational)
observations.df$N.Tracts[4] <- sum(MSA.UA.2021.observational$Count.UA)
observations.df$Total.Jobs[4] <- sum(MSA.UA.2021.observational$Jobs, na.rm = T)
observations.df$Area.HA[4] <- sum(MSA.UA.2021.observational$Area.HA, na.rm = T)

write.csv(observations.df,
          "data/output/observations_df.csv",
          row.names = F)

