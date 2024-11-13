## Master File

#! Run setup
source("scripts/00 Directory Setup and Data Downloads.R")

#! Run Commuting Flows Method
source("scripts/Commuting Flows.R")
#! Run Double Thresholds Method
source("scripts/Double Thresholds.R")
#! Run Density Peaks Method
source("scripts/Density Peaks.R")
#! Run Positive Residuals Method
source("scripts/Positive Residuals.R")
#! Run Spatial Autocorrelation Model
source("scripts/Spatial Autocorrelation.R")

#! Read in Observation DF, MSA UA, MSA WAC, calculate columns
observations.df <- read.csv("data/output/observations_df.csv")
MSA.UA.2021 <- st_read("data/processed/MSA_UA_2021.shp")
MSA.LEHD.WAC.2021 <- read.csv("data/processed/MSA_LEHD_WAC_2021.csv")

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
  mutate(Jobs = ifelse(is.na(Jobs), 0, Jobs)) %>%
  mutate(Area.HA = as.numeric(st_area(.)) / 10000)

## Clear memory
rm(MSA.LEHD.WAC.2021)

## Populate the observations DF
observations.df <- observations.df %>%
  mutate(
    Jobs.Share.MSA = Total.Jobs / sum(MSA.UA.2021$Jobs, na.rm = T),
    Area.Share.MSA = Area.HA / sum(MSA.UA.2021$Area.HA),
    Job.Density = Total.Jobs / Area.HA
  )

## Write out
write.csv(observations.df,
          "data/output/observations_df.csv",
          row.names = F)

