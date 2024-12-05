## Commuting Flows Method

#! Steps
# 1. Read in MSA UA, OD, WAC files.
# 2. Clean OD and WAC data, join to UA shapefile.
# 3. Calculate required metrics for each tract.
# 4. Identify subcenters.
# 5. Write out shapefile.

#! Read in MSA UA file
MSA.UA.2021 <- st_read("data/processed/MSA_UA_2021.shp")
#! Read in MSA OD file
MSA.LEHD.OD.2021 <- read.csv("data/processed/MSA_LEHD_OD_2021.csv")
#! Read in MSA WAC file
MSA.LEHD.WAC.2021 <- read.csv("data/processed/MSA_LEHD_WAC_2021.csv")

#! Clean OD Data and aggregate up to tracts
MSA.LEHD.OD.2021 <- MSA.LEHD.OD.2021 %>%
  mutate(w_geocode = substr(w_geocode, 1, 11),
         h_geocode = substr(h_geocode, 1, 11))
MSA.LEHD.OD.WorkEqualsHome <- MSA.LEHD.OD.2021 %>%
  filter(w_geocode == h_geocode) %>%
  mutate(w_geocode = as.character(w_geocode),
         h_geocode = as.character(h_geocode))
MSA.LEHD.OD.Inflow <- MSA.LEHD.OD.2021 %>%
  group_by(w_geocode) %>%
  summarize(Inflow = sum(S000)) %>%
  mutate(w_geocode = as.character(w_geocode))
MSA.LEHD.OD.Outflow <- MSA.LEHD.OD.2021 %>%
  group_by(h_geocode) %>%
  summarize(Outflow = sum(S000)) %>%
  mutate(h_geocode = as.character(h_geocode))

#! Clean WAC Data and aggregate up to tracts
MSA.LEHD.WAC.2021 <- MSA.LEHD.WAC.2021 %>%
  select(w_geocode, 9:28) %>%
  pivot_longer(
    cols = starts_with("CNS"),
    names_to = "NAICS",
    names_prefix = "CNS",
    values_to = "Jobs",
    values_drop_na = TRUE
  ) %>%
  mutate(w_geocode = substr(w_geocode, 1, 11)) %>%
  group_by(w_geocode) %>%
  filter(Jobs > 0) %>%
  summarise(NAICS = n_distinct(NAICS),
            Jobs = sum(Jobs)) %>%
  mutate(w_geocode = as.character(w_geocode)) %>%
  filter(Jobs > 0)

#! Join the OD and WAC data to the UA shapefile
## Joining OD Data
MSA.UA.2021 <- MSA.UA.2021 %>%
  left_join(MSA.LEHD.OD.Inflow, by = c("GEOID" = "w_geocode")) %>%
  left_join(MSA.LEHD.OD.Outflow, by = c("GEOID" = "h_geocode")) %>%
  mutate(Inflow = ifelse(is.na(Inflow), 0, Inflow),
         Outflow = ifelse(is.na(Outflow), 0, Outflow))

## Adjust inflow and outflow based on records where home UA == work UA
for (i in 1:nrow(MSA.UA.2021)) {
  ua <- MSA.UA.2021$GEOID[i]
  matched.val <- MSA.LEHD.OD.WorkEqualsHome$S000[match(ua, MSA.LEHD.OD.WorkEqualsHome$w_geocode)]
  if (!is.na(matched.val)) {
    MSA.UA.2021$Inflow[i] <- MSA.UA.2021$Inflow[i] - matched.val
    MSA.UA.2021$Outflow[i] <- MSA.UA.2021$Outflow[i] - matched.val
  }
}

## Joining WAC Data
MSA.UA.2021 <- MSA.UA.2021 %>%
  left_join(MSA.LEHD.WAC.2021, by = c("GEOID" = "w_geocode")) %>%
  mutate(NAICS = ifelse(is.na(NAICS), 0, NAICS),
         Jobs = ifelse(is.na(Jobs), 0, Jobs))

#! Calculate FC, DDI, PC for each UA
MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(
    ## FC: Flow centrality, = Inflow / Outflow
    FlowCentrality = Inflow / Outflow,
    ## DDI: Directional Dominance Index, = Inflow / (average Inflow for all UAs)
    DirectionalDominance = Inflow / (mean(Inflow, na.rm = T)),
    ## PC: Productive completeness, = number of unique NAICS / (average number of unique NAICS per UA)
    ProductiveCompleteness = NAICS / (mean(NAICS, na.rm = T)) 
  ) %>%
  mutate(
    ## In FlowCentrality, we can get NaN or Inf if Inflow |& Outflow == 0
    FlowCentrality = ifelse(!is.finite(FlowCentrality), 0, FlowCentrality)
  )

#! Identify First-order sub-centers on the basis of FC, DDI, PC
MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(
    Subcenter = ifelse(
      (FlowCentrality > 1 &
         DirectionalDominance > 1 &
         ProductiveCompleteness > 1),
      "Subcenter",
      "Not a center"
    )
  )

## Clear memory
rm(MSA.LEHD.OD.2021)
rm(MSA.LEHD.OD.Inflow)
rm(MSA.LEHD.OD.Outflow)
rm(MSA.LEHD.OD.WorkEqualsHome)
rm(MSA.LEHD.WAC.2021)

#! Filter by Subcenter, dissolve contiguous, remove loners
MSA.UA.2021 <- MSA.UA.2021 %>%
  filter(Subcenter == "Subcenter")

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
    Count.UA = sum(Count)
  ) %>%
  filter(Count.UA > 1)

#! Clean up final shapefile and write out
MSA.UA.2021 <- MSA.UA.2021 %>%
  select(Cluster, Jobs, Count.UA, geometry)

st_write(MSA.UA.2021,
         "data/output/CommutingFlows_Subcenters.shp")

#! Read in Observations DF and update
observations.df <- read.csv("data/output/observations_df.csv")

MSA.UA.2021.observational <- MSA.UA.2021 %>%
  mutate(Area.HA = as.numeric(st_area(.))) %>%
  mutate(Area.HA = Area.HA / 10000)

## Write relevant information to observation df
observations.df$N.Centers[1] <- nrow(MSA.UA.2021.observational)
observations.df$N.Tracts[1] <- sum(MSA.UA.2021.observational$Count.UA)
observations.df$Total.Jobs[1] <- sum(MSA.UA.2021.observational$Jobs, na.rm = T)
observations.df$Area.HA[1] <- sum(MSA.UA.2021.observational$Area.HA, na.rm = T)

write.csv(observations.df,
          "data/output/observations_df.csv",
          row.names = F)
