library(httr)
library(XML)


url <- "https://en.wikipedia.org/wiki/List_of_countries_by_level_of_military_equipment#List"
r <- GET(url)
airforces <- readHTMLTable(doc = content(r, "text"))[[2]]
View(airforces)

airforces <- airforces[-1, c("Country[note 1]", "Military aircraft[note 3]")]
colnames(airforces) <- c("Country", "MilitaryAircraft")


airforces <- data.frame(apply(airforces, 2, function(s) gsub("\\[.+\\]", "", s)))
airforces$MilitaryAircraft <- as.numeric(gsub(",", "", airforces$MilitaryAircraft))
View(airforces)


library(jsonlite)
url <- "http://public-api.adsbexchange.com/VirtualRadar/AircraftList.json?"
url <- paste0(url, "fMilQ=TRUE")
url

positions <- fromJSON(url)$acList

if (length(positions) != 0) {
  positions <- positions[positions$Type != "TEST", ]
  positions <- positions[!is.na(positions$Lat), ]
}
positions

library(plotly)
library(flipFormat)
g <- list(scope = "world",
          showframe = FALSE, 
          showcoastlines = TRUE,
          projection = list(type = 'mercator'),
          lonaxis = list(range = c(-140, 179)),
          lataxis = list(range = c(-55, 70)),
          resolution = 50)

p <- plot_geo(airforces)

add_trace(p, data = airforces, name = "Airforce",
          z = ~MilitaryAircraft, color = ~MilitaryAircraft,
          colors = 'Blues', locations = ~Country,
          marker = list(line = list(color = toRGB("grey"), width = 0.5)),
          showscale = TRUE, locationmode = "country names",
          colorbar = list(title = 'Airforce', separatethousands = TRUE))
config(p, displayModeBar = F)

layout(geo = g,
       margin = list(l=0, r=0, t=0, b=0, pad=0),
       paper_bgcolor = 'transparent')
aircolors = rep("airborne", nrow(positions))  # создать вектор со статусом каждого самолета
aircolors[positions$Spd < 200 & positions$Alt < 2000] <- "ground/approach"
hovertext = paste0("Operator:", positions$Op, "\nModel:", positions$Mdl, 
                   "\nAltitide(ft):", sapply(positions$Alt, FormatAsReal))
hoverinfo = rep("all", nrow(positions))

p = add_trace(p, data = positions, x = positions$Long, y = positions$Lat, 
              color = aircolors, hovertext = hovertext, showlegend = FALSE)
p
