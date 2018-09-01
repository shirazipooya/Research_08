
# 00 Requairment: -----------------------------------------------------------------------------

rm(list = ls())

library(package = dplyr,
        warn.conflicts = FALSE)

plot_map <- function(data)
{
    content <- paste(paste(data$AQMN, 
                           paste("Sub. Region:", data$Sub.region),
                           sep = "<br/>"))
    
    m <- leaflet::leaflet(data = data) %>%
        leaflet::addProviderTiles(provider = leaflet::providers$Stamen.Terrain) %>%
        leaflet::addMarkers(lng = ~ Lon,
                            lat = ~ Lat,
                            popup = content)
    
    print(m)
}

# 01 Extract Stations: ------------------------------------------------------------------------

data <- readr::read_rds(path = "./data/df_raw_data.RDS")

site <- read.csv(file = "./man/Sydney - Air Quality Data.csv",
                 header = TRUE)

plot_map(data = site)

summaryTable <- as.data.frame(x = matrix(data = NA,
                                         nrow = length(unique(data$site)), 
                                         ncol = ncol(data)))

colnames(summaryTable) <- colnames(data)

rownames(summaryTable) <- unique(data$site)

for (s in unique(data$site))
{
    summaryTable[s,] = data %>% 
        filter(site == s) %>% 
        summarise_all(funs(round(x = sum(is.na(.)) * 100 / 43824, digits = 1)))
}

summaryTable$site <- rownames(summaryTable)

summaryTable <- summaryTable %>% 
    select(-date)

rownames(summaryTable) <- NULL

summaryTable_run_01 <- summaryTable %>%
    filter(pm10 < 10) %>% 
    filter(co < 10)

site_run_01 <- site %>% 
    filter(AQMN %in% summaryTable_run_01$site)

plot_map(data = site_run_01)

PROSPECT
LIVERPOOL
CHULLORA
ROZELLE


