## Density Peaks Method

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
  mutate(Job_Density = Jobs / Area_HA) %>%
  mutate(Log_Job_Density = log(Job_Density, base = 10))

#! Identify CBD, calculate distance of all tracts from CBD.
MSA.CBD <- MSA.UA.2021 %>%
  filter(Jobs == max(Jobs))

MSA.UA.2021$DistCBD <- as.vector(st_distance(MSA.CBD %>% st_centroid,
                                             MSA.UA.2021 %>% st_centroid))

MSA.UA.2021$DistCBD <- MSA.UA.2021$DistCBD / 1000

## CBD obs will show "Inf" because DistCBD = 0. Set to 0.
MSA.UA.2021$DistCBD <- ifelse(is.infinite(MSA.UA.2021$DistCBD), 0, MSA.UA.2021$DistCBD)

#! Identify the 95th Percentile for Job Density
MSA.UA.2021.top5percent <- MSA.UA.2021 %>%
  filter(Job_Density > quantile(MSA.UA.2021$Job_Density, 0.95))

#! Create dispersion graphs
## Create smoothing spline
fit.vt <- smooth.spline(MSA.UA.2021.top5percent$DistCBD,
                        MSA.UA.2021.top5percent$Log_Job_Density,
                        cv = T)
## Identify knots in the smoothing spline
fitted.values <- predict(fit.vt, n = 1000)
change.indices <- which(diff(sign(diff(fitted.values$y))) < 0) + 1
knots <- fitted.values$x[change.indices]
## Plot and save graph
png("data/output/DensityPeaks_SmoothingSpline.png",
    width = 800,
    height = 600)
plot(MSA.UA.2021.top5percent$DistCBD,
     MSA.UA.2021.top5percent$Log_Job_Density,
     xlab = "Distance to CBD (km)",
     ylab = "Log(Job Density)",
     main = "Smoothing Spline with Knots")
lines(fitted.values$x, 
      fitted.values$y, 
      col = "red")
points(knots,
       predict(fit.vt, knots)$y,
       col = "blue",
       pch = 19,
       cex = 1.5)
for (i in knots) {
  abline(v = i,
         col = "gray",
         lty = 3)
}
legend("topright",
       legend = paste("Knot:", round(knots, 2)),
       pch = 19,
       col = "blue",
       bg = "white")
dev.off()

## Create table for knot list
knot.table <- data.frame(Knot = 1:length(knots),
                         Value = round(knots, 2))

#! Identify Employment Subcenters
## Find closest DistCBD value for each knot value
knot.table <- knot.table %>%
  mutate(
    closest_DistCBD = map_dbl(Value, 
                              ~MSA.UA.2021.top5percent$DistCBD[which.min(abs(MSA.UA.2021.top5percent$DistCBD - .x))])
  )

MSA.UA.2021 <- MSA.UA.2021 %>%
  mutate(
    Subcenter = ifelse(
      DistCBD %in% knot.table$closest_DistCBD,
      "Subcenter",
      "Not a Subcenter"
    )
  )

#! Generate Buffers for Mapping
Subcenter.BufferRings <- st_buffer(st_centroid(MSA.CBD),
                                   knot.table$Value[1] * 1000)
for (i in 2:nrow(knot.table)) {
  temp <- st_buffer(st_centroid(MSA.CBD),
                    knot.table$Value[i] * 1000)
  Subcenter.BufferRings <- rbind(Subcenter.BufferRings,
                                 temp)
}

Subcenter.BufferRings <- Subcenter.BufferRings %>%
  mutate(ID = seq(1, nrow(Subcenter.BufferRings), 1)) %>%
  select(ID, geometry)

#! Write out shapefiles of subcenters and buffers
MSA.UA.2021 <- MSA.UA.2021 %>%
  filter(Subcenter == "Subcenter")

st_write(MSA.UA.2021,
         "data/output/DensityPeaks_Subcenters.shp")
st_write(Subcenter.BufferRings,
         "data/output/DensityPeaks_BufferRings.shp")

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
observations.df$N.Centers[3] <- nrow(MSA.UA.2021.observational)
observations.df$N.Tracts[3] <- sum(MSA.UA.2021.observational$Count.UA)
observations.df$Total.Jobs[3] <- sum(MSA.UA.2021.observational$Jobs, na.rm = T)
observations.df$Area.HA[3] <- sum(MSA.UA.2021.observational$Area.HA, na.rm = T)

write.csv(observations.df,
          "data/output/observations_df.csv",
          row.names = F)

