#! Setting up workspace
## Modifying options
options(scipen = 999)
options(timeout = 999)
## Installing and loading required packages
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
#! Cumulative package loading
## Setup and Download
usePackage("dplyr")
usePackage("sf")
usePackage("tigris")
usePackage("tidyr")
## Commuting Flows
usePackage("spdep")
usePackage("ggplot2")
## Positive Residuals
usePackage("sp")
usePackage("spgwr")
usePackage("igraph")
## Density Peaks
usePackage("purrr")
usePackage("car")
usePackage("descr")
usePackage("DescTools")
usePackage("ggpubr")
usePackage("haven")
usePackage("psych")
usePackage("writexl")
usePackage("openxlsx")
## Double Thresholds
## Spatial Autocorrelation
usePackage("e1071")

#! Create needed directories
unlink("data", recursive = T)
dir.create("data")
dir.create("data/raw")
dir.create("data/processed")
dir.create("data/output")


#! Download MSA Tract boundaries
IL.UA.2021 <- tracts(
  state = "IL",
  county = c("Cook County",
             "DeKalb County",
             "DuPage County",
             "Grundy County",
             "Kane County",
             "Kendall County",
             "Lake County",
             "McHenry County",
             "Will County"),
  year = 2021
)

IN.UA.2021 <- tracts(
  state = "IN",
  county = c("Jasper County",
             "Lake County",
             "Newton County",
             "Porter County"),
  year = 2021
)

WI.UA.2021 <- tracts(
  state = "WI",
  county = c("Kenosha County"),
  year = 2021
)

## Combine the three states into the MSA file
MSA.UA.2021 <- rbind(IL.UA.2021,
                     IN.UA.2021,
                     WI.UA.2021) %>%
  st_transform(crs = 4326)

## Select the necessary columns
MSA.UA.2021 <- MSA.UA.2021 %>%
  select("GEOID", "geometry")

## Clear memory
rm(IL.UA.2021)
rm(IN.UA.2021)
rm(WI.UA.2021)

## Write out MSA.UA.2021
st_write(MSA.UA.2021,
         "data/processed/MSA_UA_2021.shp")




#! Download LEHD O-D Data
## Download the "Main" data files (only for within State commute)
lehd.baseurl <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
### Downloading for IL
download.file(
  paste0(
    lehd.baseurl,
    "il", "/",
    "od", "/",
    "il", "_", 
    "od", "_", 
    "main", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Illinois_OD_Main_2021.csv.gz",
  quiet = F)  
### Downloading for IN
download.file(
  paste0(
    lehd.baseurl,
    "in", "/",
    "od", "/",
    "in", "_", 
    "od", "_", 
    "main", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Indiana_OD_Main_2021.csv.gz",
  quiet = T)
### Downloading for WI
download.file(
  paste0(
    lehd.baseurl,
    "wi", "/",
    "od", "/",
    "wi", "_", 
    "od", "_", 
    "main", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Wisconsin_OD_Main_2021.csv.gz",
  quiet = T)
## Download the "Aux" data files (only for inter-State commute)
### Downloading for IL
download.file(
  paste0(
    lehd.baseurl,
    "il", "/",
    "od", "/",
    "il", "_", 
    "od", "_", 
    "aux", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Illinois_OD_Aux_2021.csv.gz",
  quiet = T)
### Downloading for IN
download.file(
  paste0(
    lehd.baseurl,
    "in", "/",
    "od", "/",
    "in", "_", 
    "od", "_", 
    "aux", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Indiana_OD_Aux_2021.csv.gz",
  quiet = T)
### Downloading for WI
download.file(
  paste0(
    lehd.baseurl,
    "wi", "/",
    "od", "/",
    "wi", "_", 
    "od", "_", 
    "aux", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Wisconsin_OD_Aux_2021.csv.gz",
  quiet = T)

## Read in and combine the downloaded OD data
IL.LEHD.OD.Main.2021 <- read.csv("data/raw/Illinois_OD_Main_2021.csv.gz")
IN.LEHD.OD.Main.2021 <- read.csv("data/raw/Indiana_OD_Main_2021.csv.gz")
WI.LEHD.OD.Main.2021 <- read.csv("data/raw/Wisconsin_OD_Main_2021.csv.gz")
IL.LEHD.OD.Aux.2021 <- read.csv("data/raw/Illinois_OD_Aux_2021.csv.gz")
IN.LEHD.OD.Aux.2021 <- read.csv("data/raw/Indiana_OD_Aux_2021.csv.gz")
WI.LEHD.OD.Aux.2021 <- read.csv("data/raw/Wisconsin_OD_Aux_2021.csv.gz")

MSA.LEHD.OD.2021 <- rbind(IL.LEHD.OD.Main.2021,
                          IN.LEHD.OD.Main.2021,
                          WI.LEHD.OD.Main.2021,
                          IL.LEHD.OD.Aux.2021,
                          IN.LEHD.OD.Aux.2021,
                          WI.LEHD.OD.Aux.2021)

## Write out MSA.LEHD.OD.2021
write.csv(MSA.LEHD.OD.2021,
          "data/processed/MSA_LEHD_OD_2021.csv",
          row.names = F)

## Clear memory
rm(IL.LEHD.OD.Main.2021)
rm(IN.LEHD.OD.Main.2021)
rm(WI.LEHD.OD.Main.2021)
rm(IL.LEHD.OD.Aux.2021)
rm(IN.LEHD.OD.Aux.2021)
rm(WI.LEHD.OD.Aux.2021)


#! Download LEHD WAC Data
### Downloading for IL
download.file(
  paste0(
    lehd.baseurl,
    "il", "/",
    "wac", "/",
    "il", "_", 
    "wac", "_", 
    "S000", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Illinois_WAC_2021.csv.gz",
  quiet = T)
### Downloading for IN
download.file(
  paste0(
    lehd.baseurl,
    "in", "/",
    "wac", "/",
    "in", "_", 
    "wac", "_", 
    "S000", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Indiana_WAC_2021.csv.gz",
  quiet = T)
### Downloading for WI
download.file(
  paste0(
    lehd.baseurl,
    "wi", "/",
    "wac", "/",
    "wi", "_", 
    "wac", "_", 
    "S000", "_",
    "JT00", "_",
    "2021", ".csv.gz"
  ),
  destfile = "data/raw/Wisconsin_WAC_2021.csv.gz",
  quiet = T)

## Read in and combine the downloaded data
IL.LEHD.WAC.2021 <- read.csv("data/raw/Illinois_WAC_2021.csv.gz")
IN.LEHD.WAC.2021 <- read.csv("data/raw/Indiana_WAC_2021.csv.gz")
WI.LEHD.WAC.2021 <- read.csv("data/raw/Wisconsin_WAC_2021.csv.gz")
MSA.LEHD.WAC.2021 <- rbind(IL.LEHD.WAC.2021, IN.LEHD.WAC.2021, WI.LEHD.WAC.2021)

## Write out LEHD WAC Data
write.csv(MSA.LEHD.WAC.2021,
          "data/processed/MSA_LEHD_WAC_2021.csv",
          row.names = F)

## Clear memory
rm(IL.LEHD.WAC.2021)
rm(IN.LEHD.WAC.2021)
rm(WI.LEHD.WAC.2021)
gc()

## Lastly, create a dataframe that will be updated throughout the series of scripts
observations.df <- data.frame(
  Technique = c("Commuting Flows", "Double Thresholds", "Density Peaks", "Positive Residuals", "Spatial Autocorrelation"),
  N.Centers = rep(NA, 5),
  N.Tracts = rep(NA, 5),
  Total.Jobs = rep(NA, 5),
  Area.HA = rep(NA, 5),
  Jobs.Share.MSA = rep(NA, 5),
  Area.Share.MSA = rep(NA, 5),
  Job.Density = rep(NA, 5)
)

write.csv(observations.df,
          "data/output/observations_df.csv",
          row.names = F)