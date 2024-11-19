## Double Thresholds Method

#! Steps


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

#! Calculate job density for each tract
## Calculate area in hectares (1 sq meter = 0.0001 hectare)
MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(Area_HA = as.numeric(st_area(geometry)) / 1e4) %>%
  mutate(Job_Density = Jobs / Area_HA)

#! Set Thresholds for Total Jobs and Job Density
threshold.jobs <- mean(MSA.UA.2021$Jobs, na.rm = T) + sd(MSA.UA.2021$Jobs, na.rm = T)
threshold.density <- mean(MSA.UA.2021$Job_Density, na.rm = T)

#! Determine Subcenters based on thresholds
MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(
    Subcenter = ifelse(
      Jobs > threshold.jobs & Job_Density > threshold.density,
      "Subcenter",
      "Not a Subcenter"
    )
  )

#! Clean up and Write out shapefile
MSA.UA.2021 <- MSA.UA.2021 %>%
  select(GEOID, Jobs, Area_HA, Job_Density, Subcenter, geometry) %>%
  filter(Subcenter == "Subcenter")

st_write(MSA.UA.2021,
         "data/output/DoubleThresholds_Subcenters.shp")


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
observations.df$N.Centers[2] <- nrow(MSA.UA.2021.observational)
observations.df$N.Tracts[2] <- sum(MSA.UA.2021.observational$Count.UA)
observations.df$Total.Jobs[2] <- sum(MSA.UA.2021.observational$Jobs, na.rm = T)
observations.df$Area.HA[2] <- sum(MSA.UA.2021.observational$Area.HA, na.rm = T)

write.csv(observations.df,
          "data/output/observations_df.csv",
          row.names = F)

