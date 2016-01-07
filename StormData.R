require(lubridate)
require(stringdist)
require(ggplot2)
require(scales)
require(grid)
require(gridExtra)

# 1. Reading data

data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
data_file <- "repdata-data-StormData.csv.bz2"
download.file(data_url, destfile = data_file, method = "auto")
data_date <- date()

data <- read.csv(bzfile(ds_file))
data_size <- dim(data)
data <- data[, c("EVTYPE", "BGN_DATE",
                 "FATALITIES", "INJURIES",
                 "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]


events <- data.frame(
        EventName = c("Astronomical Low Tide", "Avalanche", "Blizzard",
                      "Coastal Flood", "Cold/Wind Chill", "Debris Flow",
                      "Dense Fog", "Dense Smoke", "Drought", "Dust Devil",
                      "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill",
                      "Flash Flood", "Flood", "Freezing Fog", "Frost/Freeze",
                      "Funnel Cloud", "Hail", "Heat", "Heavy Rain", "Heavy Snow",
                      "High Surf", "High Wind", "Hurricane/Typhoon", "Ice Storm",
                      "Lakeshore Flood", "Lake-Effect Snow", "Lightning",
                      "Marine Hail", "Marine High Wind", "Marine Strong Wind",
                      "Marine Thunderstorm Wind", "Rip Current", "Seiche",
                      "Sleet", "Storm Surge/Tide", "Strong Wind",
                      "Thunderstorm Wind", "Tornado", "Tropical Depression",
                      "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout",
                      "Wildfire", "Winter Storm", "Winter Weather"
        )
)


# 2. Preparing data

# 2.0. Converting event names to upper case
data$EVTYPE <- toupper(data$EVTYPE)
events$EventName <- toupper(events$EventName)

## 2.1. Choosing data with years >= 1996
## http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
## http://www.ncdc.noaa.gov/stormevents/details.jsp?type=collection
data$YEAR <- year(as.Date(data$BGN_DATE, "%m/%d/%Y"))
subdata <- data[data$YEAR >= 1996,]

## 2.2. Choosing data with nonzero health or economic Impacts
subdata <- subdata[subdata$FATALITIES > 0 |
                   subdata$INJURIES > 0 |
                   subdata$PROPDMG > 0 |
                   subdata$CROPDMG > 0,]

## Mapping of demages values useing multiplyers
table(subdata$PROPDMGEXP)[table(subdata$PROPDMGEXP) > 0]
table(subdata$CROPDMGEXP)[table(subdata$CROPDMGEXP) > 0]

multiplier_table <- c("1" = 1, "K" = 1e3, "M" = 1e6, "B" = 1e9)
subdata$PROPDMGEXP <- ifelse(subdata$PROPDMGEXP == "",
                             "1", as.character(subdata$PROPDMGEXP))
subdata$CROPDMGEXP <- ifelse(subdata$CROPDMGEXP == "",
                             "1", as.character(subdata$CROPDMGEXP))
subdata$PROPDMG <- as.numeric(subdata$PROPDMG) *
                              multiplier_table[subdata$PROPDMGEXP]
subdata$CROPDMG <- as.numeric(subdata$CROPDMG) *
                              multiplier_table[subdata$CROPDMGEXP]

## Summing up total health or economic Impacts
fatalities_total = sum(subdata$FATALITIES)
injuries_total = sum(subdata$INJURIES)
propdmg_total = sum(subdata$PROPDMG)
cropdmg_total = sum(subdata$CROPDMG)

# Merging by event names before event names correction
tmp <- merge(subdata, events, by.x = "EVTYPE", by.y = "EventName")

## Summing up total health or economic Impacts
fatalities_before = 100 * sum(tmp$FATALITIES) / fatalities_total
injuries_before = 100 * sum(tmp$INJURIES) / injuries_total
propdmg_before = 100 * sum(tmp$PROPDMG) / propdmg_total
cropdmg_before = 100 * sum(tmp$CROPDMG) / cropdmg_total

# List of manual substitutions
subdata$EVTYPE <- gsub("TSTM", "THUNDERSTORM", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^URBAN/SML STREAM FLD$", "HEAVY RAIN", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^WILD/FOREST FIRE$", "WILDFIRE", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^EXTREME COLD$", "EXTREME COLD/WIND CHILL", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^HURRICANE$", "HURRICANE/TYPHOON", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^TYPHOON$", "HURRICANE/TYPHOON", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^COLD$", "EXTREME COLD/WIND CHILL", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^LANDSLIDE$", "DEBRIS FLOW", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^FOG$", "DENSE FOG", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^GLAZE$", "FREEZING FOG", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^WIND$", "HIGH WIND", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^HEAVY SURF/HIGH SURF$", "HIGH SURF", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^COLD AND SNOW$", "EXTREME COLD/WIND CHILL", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^WINTER WEATHER/MIX$", "EXTREME COLD/WIND CHILL", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^THUNDERSTORM WIND/HAIL$", "THUNDERSTORM WIND", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^EXTREME WINDCHILL$", "EXTREME COLD/WIND CHILL", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^STORM SURGE$", "STORM SURGE/TIDE", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^RIVER FLOODING$", "FLOOD", subdata$EVTYPE)
subdata$EVTYPE <- gsub("^COASTAL FLOODING/EROSION$", "COASTAL FLOOD", subdata$EVTYPE)

# Event names with frequences
raw_event_names <- as.data.frame(table(subdata$EVTYPE))
names(raw_event_names) <- c("EventOldName", "Count")


# Optimal string aligment (restricted Damerau-Levenshtein distance)
dist_matrix <- stringdistmatrix(raw_event_names$EventOldName, events$EventName)

# Matrix of distance between raw event names and standard event names
raw_event_names$DistOSA <- apply(dist_matrix, 1, min)
raw_event_names$EventName <- events[apply(dist_matrix, 1, which.min), ]
raw_event_names$DistNorm <- raw_event_names$DistOSA / 
                                nchar(as.character(raw_event_names$EventName))

## Event names misspelling correction
max_osa_dist <- 4
max_norm_dist <- 0.3
raw_event_names$EventNewName <- ifelse(raw_event_names$DistOSA <= max_osa_dist & 
                                    raw_event_names$DistNorm < max_norm_dist,
                                    raw_event_names$EventName,
                                    "UNCLASSIFIED")
subdata <- merge(subdata, raw_event_names[, c("EventOldName", "EventNewName")],
             by.x = "EVTYPE", by.y = "EventOldName")

# Checksum
delta <- subdata[subdata$EventNewName != "UNCLASSIFIED", ]
fatalities_after = 100 * sum(delta$FATALITIES) / fatalities_total
injuries_after = 100 * sum(delta$INJURIES) / injuries_total
propdmg_after = 100 * sum(delta$PROPDMG) / propdmg_total
cropdmg_after = 100 * sum(delta$CROPDMG) / cropdmg_total

# 3. Data analisis

# Summing up amiount of faatalities and injuries by event types
total_impact <- aggregate(cbind(FATALITIES, INJURIES, PROPDMG, CROPDMG) ~ EventNewName,
                          data = subdata, sum)
total_impact_table <- total_impact
total_impact$PROPDMG <- round(total_impact$PROPDMG / 1e6)
total_impact$CROPDMG <- round(total_impact$CROPDMG / 1e6)

# http://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr
my_ggplot = function(datatable, title = "", lsubtitle = "", rsubtitle = "") {
        datatable <- as.data.frame(datatable)
        names(datatable) <- c("Events", "Left", "Right")
        datatable <- datatable[order(-(datatable$Left + datatable$Right)), ]
        datatable$Events <- factor(datatable$Events,
                                   levels = datatable[order(datatable$Left + 
                                                            datatable$Right),
                                                      "Events"])
        datatable <- datatable[1:15, ]

        g.mid <- ggplot(datatable, aes(x = 1, y = Events)) +
                geom_text(aes(label = Events), position = "identity") +
                ggtitle("") +
                ylab(NULL) +
                scale_x_continuous(expand = c(0,0),
                                   limits = c(0.94,1.065)) +
                theme(
                        axis.title = element_blank(),
                        panel.grid = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        panel.background = element_blank(),
                        axis.text.x = element_text(color = NA),
                        axis.ticks.x = element_line(color = NA),
                        plot.margin = unit(c(1,-1,1,-1), "mm")
                )
        
        g1 <- ggplot(datatable, aes(x = Events, y = Left)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = Left), hjust = 1.4) +
                ggtitle(lsubtitle) +
                theme(
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        plot.margin = unit(c(1,-1,1,0), "mm")
                ) +
                scale_y_reverse(limits = c(max(datatable$Left) * 1.4, 0)) +
                coord_flip()
        
        g2 <- ggplot(datatable, aes(x = Events, y = Right)) +
                xlab(NULL) +
                geom_bar(stat = "identity") +
                ggtitle(rsubtitle) +
                geom_text(aes(label = Right), hjust = -.3) +
                theme(
                        axis.title.x = element_blank(), 
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        plot.margin = unit(c(1,0,1,-1), "mm")
                ) +
                scale_y_continuous(limits = c(0,max(datatable$Right) * 1.3)) +
                coord_flip()

        gg.mid <- ggplot_gtable(ggplot_build(g.mid))
        gg1 <- ggplot_gtable(ggplot_build(g1))
        gg2 <- ggplot_gtable(ggplot_build(g2))
        
        grid.arrange(gg1,gg.mid,gg2,
                     top = textGrob(title, gp=gpar(cex=2)),
                     ncol = 3,
                     widths = c(0.35, 0.25, 0.35))
}

my_ggplot(total_impact[, c("EventNewName", "FATALITIES", "INJURIES")],
          "Most harmful events with respect to population health",
          "Fatalities", "Injuries")
my_ggplot(total_impact[, c("EventNewName", "PROPDMG", "CROPDMG")],
          "Most harmful events with greatest economic consequences\n(in mln USD)",
          "Property Demage", "Crop Demage")


library(knitr)
total_impact_table$PROPDMG <- round(total_impact_table$PROPDMG / 1e6, digits = 3)
total_impact_table$CROPDMG <- round(total_impact_table$CROPDMG / 1e6, digits = 3)
names(total_impact_table) <- c("Event Fame", "Fatalities", "Injuries", "Property Damages, mln USD", "Crop Damages, mln USD")
kable(total_impact_table, digits=3)
