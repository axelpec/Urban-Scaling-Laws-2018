
# -------------------- Loading libraries -------------------------------------------------------------------------------------------------------------

library(sf)
library(sp)
library(cartography)
Sys.setenv(NOAWT=1)
# library(OpenStreetMap)
# Sys.setenv(NOAWT=1)
# library(OSMscale)
library(ggsn)
library(leaflet)
library(tibble)
library(data.table)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(ggmap)
library(corrplot)
library(stringr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(rsconnect)

register_stadiamaps("ffcca74d-61fd-40f5-b624-3088885567aa") 


# ---------------------- Loading data ------------------------------------------------------------------------------------------------------------    

rescaling_factors <- read.csv2("Origine/CSV/rescaling_factors1.csv", stringsAsFactors = F, 
                               header = T, sep = ";")

artificial_cities <- read.csv2("Origine/CSV/artificial_cities_exp00_thre05-00.csv", 
                               na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

artif_cities_Rescaling <- read.csv2("Origine/CSV/artif_cities_exp050_thre0-00_Rescaling.csv", 
                                    na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

artificial_disque <- read.csv2("Origine/CSV/artificial_cities_integ-00_V2.csv", 
                               na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

artificial_cities_average <- read.csv2("Origine/CSV/artif_average_exp050.csv", 
                                       stringsAsFactors = F, header = T, sep = ";")

centre_ville <- read.csv2("Origine/CSV/centers_4326.csv", stringsAsFactors = F, 
                          header = T, sep = ";", encoding = "UTF-8")


housing_average <- read.csv2("Origine/Land-uses_2006/CSV/housing_av_thre05-00.csv", 
                             na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

housing <- read.csv2("Origine/Land-uses_2006/CSV/housing_cities_thre05-00.csv", 
                     na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

industries_average <- read.csv2("Origine/Land-uses_2006/CSV/indus_av_exp050_thre05-00.csv", 
                                na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

industries <- read.csv2("Origine/Land-uses_2006/CSV/indus_cities_exp050_thre05-00.csv", 
                        na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

roads_average <- read.csv2("Origine/Land-uses_2006/CSV/roads_av1.csv", 
                           na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

roads <- read.csv2("Origine/Land-uses_2006/CSV/roads_cities1.csv", 
                   na.strings = "NA", stringsAsFactors = F, header = T, sep = ";")

town_hall <- st_read("Origine/SHP/Hotels_de_ville.shp")

EPCI_2020 <- st_read("Origine/SHP/EPCI_FR.shp")

FUA_2014 <- st_read("Origine/SHP/FUA_Europe_2014.shp")


# ---------------------- Data preparation -----------------------------------------------------------------------------------------------------------

setnames(artificial_cities, "fr022l_clermont.ferrand", "fr022l_clermont-ferrand")
setnames(artif_cities_Rescaling, "fr022l_clermont.ferrand", "fr022l_clermont-ferrand")
setnames(artificial_disque, "fr022l_clermont.ferrand", "fr022l_clermont-ferrand")


# Numerical format
artificial_cities <- map_dfr(artificial_cities[,1:307], as.numeric)
artif_cities_Rescaling <- map_dfr(artif_cities_Rescaling[,1:304], as.numeric)
artificial_cities_average <- map_dfr(artificial_cities_average[,1:11], as.numeric)
artificial_disque <- map_dfr(artificial_disque[,1:307], as.numeric)

housing_average <- map_dfr(housing_average[,1:10], as.numeric)
housing <- map_dfr(housing[,1:307], as.numeric)
industries_average <- map_dfr(industries_average[,1:10], as.numeric)
industries <- map_dfr(industries[,1:307], as.numeric)
roads_average <- map_dfr(roads_average[,1:5], as.numeric)
roads <- map_dfr(roads[,1:304], as.numeric)

rescaling_factors$LU_k_fact <- as.numeric(rescaling_factors$LU_k_fact)
na.omit(rescaling_factors$LU_k_fact)
rescaling_factors$totpop <- as.numeric(rescaling_factors$totpop)
na.omit(rescaling_factors$totpop)

pop_city <- rescaling_factors %>% select(cityfile, LU_k_fact, totpop)

town_hall <-left_join(town_hall, pop_city, by = c("cityfile" = "cityfile"))
town_hall <- na.omit(town_hall)



# Final data table artificialization
DF_artif <- artificial_cities %>% 
  pivot_longer(cols = 3:ncol(artificial_cities), 
               names_to = "City", 
               values_to = "Value")
DF_artif <- na.omit(DF_artif)
pop_city <- rescaling_factors %>% select(cityfile, LU_k_fact, totpop)
DF_artif <- left_join(DF_artif,pop_city, by = c("City" = "cityfile"))
DF_artif$totpop <- as.numeric(DF_artif$totpop)

k_fact <- rescaling_factors %>% select(cityfile, LU_k_fact, totpop)
centre_ville2 <- left_join(centre_ville, k_fact, by = c("cityfile" = "cityfile"))
centre_ville2 <- na.omit(centre_ville2)
centre_ville2 <- arrange(centre_ville2, cityname)
centre_ville2 <- arrange(centre_ville2, country)

cityname <- centre_ville2 %>% select(cityname, cityfile)
DF_artif <- left_join(DF_artif, cityname, by = c("City" = "cityfile"))
DF_artif <- na.omit(DF_artif)


# Final data table artificialization cities rescaling
artif_rescaling <- artif_cities_Rescaling %>% 
  pivot_longer(cols = 3:ncol(artif_cities_Rescaling), 
               names_to = "City", 
               values_to = "Value")
artif_rescaling <- na.omit(artif_rescaling)

artif_rescaling <- left_join(artif_rescaling, pop_city, by = c("City" = "cityfile"))
artif_rescaling$totpop <- as.numeric(artif_rescaling$totpop)

artif_rescaling <- left_join(artif_rescaling, cityname, by = c("City" = "cityfile"))
artif_rescaling <- na.omit(artif_rescaling)



# Preparation values housing land use
housing_artif <- housing %>% 
  pivot_longer(cols = 3:ncol(housing), 
               names_to = "City", 
               values_to = "Value")
housing_artif <- na.omit(housing_artif)
pop_city <- rescaling_factors %>% select(cityfile, LU_k_fact, totpop)
housing_artif <- left_join(housing_artif,pop_city, by = c("City" = "cityfile"))
housing_artif$totpop <- as.numeric(housing_artif$totpop)
housing_artif <- na.omit(housing_artif)

cityname <- centre_ville %>% select(cityfile, cityname)
housing_artif <- left_join(housing_artif, cityname, by = c("City" = "cityfile"))
housing_artif <- na.omit(housing_artif)


# Preparation values industries land use
industries_artif <- industries %>% 
  pivot_longer(cols = 3:ncol(industries), 
               names_to = "City", 
               values_to = "Value")
industries_artif <- na.omit(industries_artif)
pop_city <- rescaling_factors %>% select(cityfile, LU_k_fact, totpop)
industries_artif <- left_join(industries_artif,pop_city, by = c("City" = "cityfile"))
industries_artif$totpop <- as.numeric(industries_artif$totpop)
industries_artif <- na.omit(industries_artif)

industries_artif <- left_join(industries_artif, cityname, by = c("City" = "cityfile"))
industries_artif <- na.omit(industries_artif)


# Preparation values roads land use
roads_artif <- roads %>% 
  pivot_longer(cols = 3:ncol(roads), 
               names_to = "City", 
               values_to = "Value")
roads_artif <- na.omit(roads_artif)
pop_city <- rescaling_factors %>% select(cityfile, LU_k_fact, totpop)
roads_artif <- left_join(roads_artif,pop_city, by = c("City" = "cityfile"))
roads_artif$totpop <- as.numeric(roads_artif$totpop)
roads_artif <- na.omit(roads_artif)

roads_artif <- left_join(roads_artif, cityname, by = c("City" = "cityfile"))
roads_artif <- na.omit(roads_artif)


# Add countries
artif_rescaling$country <- str_sub(artif_rescaling$City, end=2)
str_to_upper(artif_rescaling$country)

housing_artif$country <- str_sub(housing_artif$City, end=2)
str_to_upper(housing_artif$country)

industries_artif$country <- str_sub(industries_artif$City, end=2)
str_to_upper(industries_artif$country)

roads_artif$country <- str_sub(roads_artif$City, end=2)
str_to_upper(roads_artif$country)


# Average artificial land use by countries
artif_country_av <- artif_rescaling %>% 
  group_by(dist_km, country) %>% 
  summarise(mean.x = mean(Value),
            med.x = quantile(Value, probs = 0.50),
            lower.x = quantile(Value, probs = 0.25),
            upper.x = quantile(Value, probs = 0.75),
            std.x = sd(Value))

artif_city_av <- merge(artif_rescaling, artif_country_av, by = c("dist_km","country"))
artif_city_av$diff_av <- artif_city_av$Value - artif_city_av$mean.x


# Average housing land use by countries
housing_country_av <- housing_artif %>% 
  group_by(dist_km, country) %>% 
  summarise(mean.x = mean(Value),
            med.x = quantile(Value, probs = 0.50),
            lower.x = quantile(Value, probs = 0.25),
            upper.x = quantile(Value, probs = 0.75),
            std.x = sd(Value))

housing_city_av <- merge(housing_artif, housing_country_av, by = c("dist_km","country"))
housing_city_av$diff_av <- housing_city_av$Value - housing_city_av$mean.x

diff_av_country_housing <- housing_city_av %>% 
  group_by(dist_km, country) %>% 
  summarise(mean_diff = mean(diff_av))


# Average industries land use by countries
industries_country_av <- industries_artif %>% 
  group_by(dist_km, country) %>% 
  summarise(mean.x = mean(Value),
            med.x = quantile(Value, probs = 0.50),
            lower.x = quantile(Value, probs = 0.25),
            upper.x = quantile(Value, probs = 0.75),
            std.x = sd(Value))

industries_city_av <- merge(industries_artif, industries_country_av, by = c("dist_km","country"))
industries_city_av$diff_av <- industries_city_av$Value - industries_city_av$mean.x

diff_av_country_industries <- industries_city_av %>% 
  group_by(dist_km, country) %>% 
  summarise(mean_diff = mean(diff_av))


# Average roads land use by countries
roads_country_av <- roads_artif %>% 
  group_by(dist_km, country) %>% 
  summarise(mean.x = mean(Value),
            med.x = quantile(Value, probs = 0.50),
            lower.x = quantile(Value, probs = 0.25),
            upper.x = quantile(Value, probs = 0.75),
            std.x = sd(Value))

roads_city_av <- merge(roads_artif, roads_country_av, by = c("dist_km","country"))
roads_city_av$diff_av <- roads_city_av$Value - roads_city_av$mean.x

diff_av_country_roads <- roads_city_av %>% 
  group_by(dist_km, country) %>% 
  summarise(mean_diff = mean(diff_av))



# Artificialization average
artif_country_ES <- artif_country_av[artif_country_av$country=="es", ]
artif_country_ES$country <- "Spain"

artif_country_FR <- artif_country_av[artif_country_av$country=="fr", ]
artif_country_FR$country <- "France"

artificial_cities_average$country <- "Europe"

artificial_cities_average$Value <- artificial_cities_average$artif_av
artif_country_ES$Value <- artif_country_ES$mean.x
artif_country_FR$Value <- artif_country_FR$mean.x

list_vars <- c("dist_km", "country", "Value")
artificial_average_sel <- artificial_cities_average[list_vars]
artificial_country_ES_sel <- artif_country_ES[list_vars]
artificial_country_FR_sel <- artif_country_FR[list_vars]

artif_EU_ES_FR_av <- bind_rows(artificial_average_sel, artificial_country_ES_sel, 
                               artificial_country_FR_sel)


# Urban fabric average
housing_country_ES <- housing_country_av[housing_country_av$country=="es", ]
housing_country_ES$country <- "Spain"

housing_country_FR <- housing_country_av[housing_country_av$country=="fr", ]
housing_country_FR$country <- "France"

housing_average$country <- "Europe"

housing_average$Value <- housing_average$housing_av
housing_country_ES$Value <- housing_country_ES$mean.x
housing_country_FR$Value <- housing_country_FR$mean.x

list_vars <- c("dist_km", "country", "Value")
housing_average_sel <- housing_average[list_vars]
housing_country_ES_av <- housing_country_ES[list_vars]
housing_country_FR_av <- housing_country_FR[list_vars]

housing_EU_ES_FR_av <- bind_rows(housing_average_sel, housing_country_ES_av, housing_country_FR_av)


# Industries average
industries_country_ES <- industries_country_av[industries_country_av$country=="es", ]
industries_country_ES$country <- "Spain"

industries_country_FR <- industries_country_av[industries_country_av$country=="fr", ]
industries_country_FR$country <- "France"

industries_average$country <- "Europe"

industries_average$Value <- industries_average$indus_av
industries_country_ES$Value <- industries_country_ES$mean.x
industries_country_FR$Value <- industries_country_FR$mean.x

list_vars <- c("dist_km", "country", "Value")
industries_average_sel <- industries_average[list_vars]
industries_country_ES_sel <- industries_country_ES[list_vars]
industries_country_FR_sel <- industries_country_FR[list_vars]

industries_EU_ES_FR_av <- bind_rows(industries_average_sel, industries_country_ES_sel, 
                                    industries_country_FR_sel)


# Roads average
roads_country_ES <- roads_country_av[roads_country_av$country=="es", ]
roads_country_ES$country <- "Spain"

roads_country_FR <- roads_country_av[roads_country_av$country=="fr", ]
roads_country_FR$country <- "France"

roads_average$country <- "Europe"

roads_average$Value <- roads_average$artificial_av
roads_country_ES$Value <- roads_country_ES$mean.x
roads_country_FR$Value <- roads_country_FR$mean.x

list_vars <- c("dist_km", "country", "Value")
roads_average_sel <- roads_average[list_vars]
roads_country_ES_sel <- roads_country_ES[list_vars]
roads_country_FR_sel <- roads_country_FR[list_vars]

roads_EU_ES_FR_av <- bind_rows(roads_average_sel, roads_country_ES_sel, roads_country_FR_sel)




# Names cities Spain / France
cityname$country <- str_sub(cityname$cityfile, end=2)
str_to_upper(cityname$country)

cityname_ES <- cityname[cityname$country=="es", ]
cityname_ES$country <- "Spain"
cityname_ES <- arrange(cityname_ES, cityname)

cityname_FR <- cityname[cityname$country=="fr", ]
cityname_FR$country <- "France"
cityname_FR <- arrange(cityname_FR, cityname)



# Disques artificialization
Disque_artif <- artificial_disque %>% 
  pivot_longer(cols = 3:ncol(artificial_disque), 
               names_to = "City", 
               values_to = "Value")
coord_hdv <- town_hall %>% select(X,Y,cityname,cityfile,LU_k_fact,totpop)
Disque_artif <- left_join(Disque_artif, coord_hdv, by = c("City" = "cityfile"))
Disque_artif <- na.omit(Disque_artif)



# Correlation matrix
myTabCor <- cor(artif_cities_Rescaling[,-1], use = "pairwise.complete.obs")


iconv(centre_ville2$country, to='ASCII//TRANSLIT')



# ----------------------- USER INTERFACE --------------------------------------------------------------------------------------------------------

ui <- navbarPage("Artificial land use in European cities in 2006", 
                 theme = shinytheme("superhero"),
                 
                 tabPanel("Interactive map", 
                          h1(tags$b("Geovisualization of artificial land use in European cities in 2006, with urban scaling laws"), 
                             align = "center"), br(), br(), br(), 
                          h3("The size of cities according to their artificial land use"), br(),
                          fluidRow(column(11, htmlOutput("DistanceDisque"))), br(),
                          fluidRow(column(10, align = "center",
                                          selectInput("disque", "Select the distance in km",
                                                      choices = unique(Disque_artif$dist_km), 
                                                      selected = 50))), br(),
                          fluidRow(column(11, leafletOutput("mapLeaflet", height = 670))), br()),   
                 
                 navbarMenu("Methodology",
                            tabPanel("Urban scaling laws",
                                     fluidRow(column(11, h4("Rescaling can be defined as the ability to transform a set of objects from one spatial scale 
                                            to another without changing the structure (Batty, 2015). Without rescaling, comparison between 
                                            cities of different sizes is meaningless because the urbanization process is much more 
                                            advanced in larger cities."))), 
                                     fluidRow(column(11, h4("As Lemoy and Caruso (2018) have shown, through rescaling, the organization of  successions 
                                            of land uses occurs at the same rate if we cancel out the effect of city size. 
                                            Thus, comparison becomes possible for cities of different sizes and locations."))), br(), br(),
                                     h4("Artificial land use is characterized by the square root of the population of each city."), br(),
                                     fluidRow(column(12, offset = 1, img(src = "Schema_echelles.jpg", 
                                                                         height = "82%", width = "82%"))), br(), br(),
                                     h4("The artificial land use profiles are similar if distance to the city center is recalculated using 
                        total population at an exponent of 0.5."), br(),
                                     h4("Variation of the exponent to obtain the optimal rescaling factor. - 
                        Mapping examples for an initial radius of 30 km from the city hall."), br(),
                                     fluidRow(column(12, offset = 1, img(src = "Gif_exposant.gif", 
                                                                         height = "82%", width = "82%"))), br()),
                            
                            tabPanel("Data", 
                                     fluidRow(column(11, uiOutput("InfosArtificialisation"))), br(),
                                     fluidRow(column(12, offset = 5, img(src = "Artificialisation_legende.jpg", 
                                                                         height = "30%", width = "13%"))),  br(), br(),
                                     fluidRow(column(11, uiOutput("InfosFUA"))), br(),
                                     fluidRow(column(11, uiOutput("InfosDonnees"))), br(), br(),
                                     actionButton("Info1", "Read more", icon = icon("info-circle"),
                                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;
                                  padding: 5px; font-size: 85%"), 
                                     fluidRow(column(11, uiOutput("InfoBox1"))), br(), br())),      
                 
                 
                 navbarMenu("European examples", 
                            tabPanel("Rescaled cities",
                                     fluidRow(column(10, htmlOutput("NoteLecture1"))), br(), br(),
                                     fluidRow(column(2, htmlOutput("InfosRescaling4")),
                                              column(3, htmlOutput("InfosRescaling5"))),
                                     fluidRow(column(4, offset = 5, 
                                                     img(src = "fleche.png", height = "32%", 
                                                         width = "20%"))), br(),
                                     fluidRow(column(5, plotOutput("graph4")), 
                                              column(5, plotOutput("graph6"))), br(),
                                     fluidRow(column(5, plotlyOutput("graph3")),
                                              column(5, plotlyOutput("graph5"))), br(),
                                     fluidRow(column(10, htmlOutput("NoteLecture4"))), br()),
                            
                            tabPanel("Summary of artificialization",
                                     fluidRow(column(7, htmlOutput("NoteLecture2"))), br(), br(),
                                     fluidRow(column(5, plotOutput("graph7")), 
                                              column(5, plotOutput("graph27")))),
                            
                            tabPanel("Mapping examples",
                                     fluidRow(column(2, htmlOutput("InfosRescaling1")),
                                              column(3, htmlOutput("InfosRescaling6")),
                                              column(6, htmlOutput("NoteLecture3"))), br(),
                                     fluidRow(column(7, offset = 3, h4("Artificial land use
                                                          equivalent for a 30 km radius in London."))), br(),
                                     splitLayout(plotOutput("map_1"), plotOutput("map_2")), br(),
                                     splitLayout(plotOutput("map_3"), plotOutput("map_4")), br())),
                 
                 
                 navbarMenu("Differences on a national scale: France/Spain",
                            tabPanel("Artificial land use",
                                     fluidRow(column(10, htmlOutput("NoteLecture6"))), br(), br(), br(),
                                     fluidRow(column(2, selectizeInput("villes7", "Select a French city",
                                                                       unique(cityname_FR$cityname))),
                                              column(2, selectizeInput("villes8", "Select a Spain city",
                                                                       unique(cityname_ES$cityname))),
                                              column(3, htmlOutput("InfosRescaling9"))), br(),
                                     fluidRow(column(5, plotOutput("graph13")), 
                                              column(5, plotOutput("graph14")))),
                            
                            tabPanel("Urban fabric land use",
                                     fluidRow(column(2, selectizeInput("villes9", "Select a French city",
                                                                       unique(cityname_FR$cityname))),
                                              column(2, selectizeInput("villes10", "Select a Spain city",
                                                                       unique(cityname_ES$cityname))),
                                              column(3, htmlOutput("InfosRescaling10"))), br(),
                                     fluidRow(column(5, plotOutput("graph15")), 
                                              column(5, plotOutput("graph16"))), br(),
                                     fluidRow(column(10, plotOutput("graph17"))), 
                                     h5("Representation of the deviations from the European average."), br()),
                            
                            tabPanel("Industries land use",
                                     fluidRow(column(2, selectizeInput("villes11", "Select a French city",
                                                                       unique(cityname_FR$cityname))),
                                              column(2, selectizeInput("villes12", "Select a Spain city",
                                                                       unique(cityname_ES$cityname))),
                                              column(3, htmlOutput("InfosRescaling11"))), br(),
                                     fluidRow(column(5, plotOutput("graph18")), 
                                              column(5, plotOutput("graph19"))), br(),
                                     fluidRow(column(10, plotOutput("graph20"))), 
                                     h5("Representation of the deviations from the European average."), br()),
                            
                            tabPanel("Roads land use",
                                     fluidRow(column(2, selectizeInput("villes13", "Select a French city",
                                                                       unique(cityname_FR$cityname))),
                                              column(2, selectizeInput("villes14", "Select a Spain city",
                                                                       unique(cityname_ES$cityname))),
                                              column(3, htmlOutput("InfosRescaling12"))), br(),
                                     fluidRow(column(5, plotOutput("graph21")), 
                                              column(5, plotOutput("graph22"))), br(),
                                     fluidRow(column(10, plotOutput("graph23"))),
                                     h5("Representation of the deviations from the European average."), br())),
                 
                 
                 navbarMenu("And my city, how does it stand?", 
                            tabPanel("Statistical comparisons",
                                     fluidRow(column(2, selectizeInput("country1", "Select the country of your choice ",
                                                                       unique(centre_ville2$country))),
                                              column(2, selectizeInput("country2", "Select the country of your choice ",
                                                                       unique(centre_ville2$country))),
                                              column(2, selectizeInput("country3", "Select the country of your choice ",
                                                                       unique(centre_ville2$country)))), br(),     
                                     fluidRow(column(2, uiOutput("villes3")),
                                              column(2, uiOutput("villes4")),
                                              column(2, uiOutput("villes5")),
                                              column(2, htmlOutput("InfosRescaling2")),
                                              column(3, htmlOutput("InfosRescaling7"))), br(),
                                     fluidRow(column(4, offset = 5, 
                                                     img(src = "fleche.png", height = "32%", 
                                                         width = "20%"))), br(),
                                     fluidRow(column(5, plotOutput("graph8")),
                                              column(5, plotOutput("graph9"))), 
                                     h5("Note for the reader: The black dotted lines show the European average 
                       of artificial land use shares."), br(),
                                     h4("Urban fabric and industries land use: "),
                                     fluidRow(column(5, plotOutput("graph24")),
                                              column(5, plotOutput("graph25"))), br(),
                                     h4("Roads land use: "),
                                     fluidRow(column(5, plotOutput("graph26"))), br()),
                            
                            tabPanel("Mapping comparisons", 
                                     h3("Comparisons of the shares of artificial land use with rescaled distances"), br(),
                                     fluidRow(column(2, selectizeInput("country4", "Select the country of your choice ",
                                                                       unique(centre_ville2$country))),
                                              column(2, selectizeInput("country5", "Select the country of your choice ",
                                                                       unique(centre_ville2$country)))), br(), 
                                     fluidRow(column(2, uiOutput("villes1")),
                                              column(2, uiOutput("villes2")),
                                              column(3, chooseSliderSkin(skin = "Modern", color = "MediumBlue"),
                                                     sliderInput("distance", label = "Choice of distance in km",
                                                                 min = 1, max = 60, step = 0.5, value = 30)),
                                              column(2, htmlOutput("InfosRescaling3")),
                                              column(3, htmlOutput("InfosRescaling8"))),
                                     splitLayout(plotOutput("map_5"), plotOutput("map_6")), br(), 
                                     fluidRow(column(4, offset = 5, 
                                                     img(src = "fleche.png", height = "32%", width = "20%"))), br(),
                                     fluidRow(column(5, plotOutput("graph10")), 
                                              column(5, plotOutput("graph11"))), br(), br(),
                                     htmlOutput("NoteLecture5"), br()),
                            
                            tabPanel("Most similar cities",       
                                     h3("Visualization of the nearest European cities in terms of profiles of 
                       artificial land use"), br(),
                                     fluidRow(column(2, selectizeInput("country6", "Select the country of your choice ",
                                                                       unique(centre_ville2$country)))),
                                     fluidRow(column(2, uiOutput("villes6")),
                                              column(3, sliderInput("correlation", label = h5("Choice of the range of 
                                                                             the correlation matrix"), min = 0, 
                                                                    max = 1, value = c(0.95, 1)))), br(), br(),
                                     # column(5, plotOutput("graph12"))),
                                     # fluidRow(column(3, htmlOutput("SumCities"))), br(), 
                                     fluidRow(column(11, leafletOutput("mapLeaflet2", height = 670))), br(), br()
                                     
                            ))
                 
)



# ------------------------ SERVER PART --------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  
  output$villes1 <- renderUI({
    selectInput("villes1", "Select the 1st city", 
                choices = centre_ville2[centre_ville2$country==input$country4, "cityname"])
  })
  
  output$villes2 <- renderUI({
    selectInput("villes2", "Select the 2nd city", 
                choices = centre_ville2[centre_ville2$country==input$country5, "cityname"])
  })
  
  output$villes3 <- renderUI({
    selectInput("villes3", "Select the 1st city", 
                choices = centre_ville2[centre_ville2$country==input$country1, "cityname"])
  })
  
  output$villes4 <- renderUI({
    selectInput("villes4", "Select the 2nd city", 
                choices = centre_ville2[centre_ville2$country==input$country2, "cityname"])
  })
  
  output$villes5 <- renderUI({
    selectInput("villes5", "Select the 3rd city", 
                choices = centre_ville2[centre_ville2$country==input$country3, "cityname"])
  })
  
  output$villes6 <- renderUI({
    selectInput("villes6", "Select a city", 
                choices = centre_ville2[centre_ville2$country==input$country6, "cityfile"])
  })
  
  
  
  output$InfoBox1 <- renderUI({
    if (input$Info1 %% 2){
      helpText(tags$div(HTML("</br> <FONT size='3pt'>
                             <b> Calculating the rescaling factor: </b> </FONT>")), 
               
               withMathJax("$$k=\\sqrt{\\frac{N_London}{N}}$$"),
               
               tags$div(HTML("<FONT size='3pt'>
                             where N is the population of the city studied. </FONT>")),
               
               tags$div(HTML("</br> <FONT size='3pt'>
                    The choice of a radial analysis for the land-use counting areas is well suited to the general 
                    patterns of center-periphery organization of European cities. </br>
                    The radial approach allows cities to be studied with a complete distribution of land
                    using a reference center for each city.  </br>
                    Cities can be considered as volumes that extend over an area proportional to their 
                    population. </br> </br>
                    <b> To go further: </b> Lemoy R and Caruso G (2018) Evidence for the homothetic 
                    scaling of urban forms, <i> Environment and Planning B </i>. </FONT>")),
               
               tags$a(href = "https://arxiv.org/pdf/1704.06508.pdf", "Free version"), br(),
               tags$a(href = "https://journals.sagepub.com/doi/abs/10.1177/2399808318810532", "Version requiring a subscription")
      )
    }
    else {
      return()}
  })
  
  
  output$DistanceDisque <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Note for the reader: </b> </br>
           As London is the European city with the largest population in Europe, it is chosen 
           as the reference city for the rescaling factors.</br></br>    
           Estimate of the size of the cities needed to obtain a share of artificial land use
           found on average on a disc surrounding London of  </FONT>", 
                  input$disque, " <FONT size='3pt'> km. </FONT> </br>",
                  "<FONT size='3pt'> The artificial land use shares are the averages of artificial 
                  land uses obtained within each disc.
           The radius of each disc is calculated by dividing the selected distance by the rescaling 
           factor of each city.
           </FONT>"))
  })
  
  output$InfosRescaling1 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Rescaling factors </b> </br></br>
                  <b> Paris : </b> 1.026 </br>
                  <b> Madrid : </b> 1.421 </br>
                  <b> Bucarest : </b> 2.377 </br>
                  <b> Innsbruck : </b> 6.608 </FONT>"))
  })
  
  output$InfosRescaling2 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Rescaling factors </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes3], " : ", "</b>",
                  round(centre_ville2$LU_k_fact[centre_ville2$cityname == input$villes3],3), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes4], " : ", "</b>",
                  round(centre_ville2$LU_k_fact[centre_ville2$cityname == input$villes4],3), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes5], " : ", "</b>",
                  round(centre_ville2$LU_k_fact[centre_ville2$cityname == input$villes5],3)))
  })
  
  output$InfosRescaling3 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Rescaling factors </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes1], " : ", "</b>",
                  round(centre_ville2$LU_k_fact[centre_ville2$cityname == input$villes1],3), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes2], " : ", "</b>",
                  round(centre_ville2$LU_k_fact[centre_ville2$cityname == input$villes2],3)))
  })
  
  output$InfosRescaling4 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Rescaling factors </b> </br></br>
                  <b> Paris : </b> 1.026 </br>
                  <b> Madrid : </b> 1.421 </br>
                  <b> Bucarest : </b> 2.377 </br>
                  <b> Innsbruck : </b> 6.608 </FONT>"))
  })
  
  output$InfosRescaling5 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Population in number of inhabitants </b> </br></br>
                  <b> Paris : </b> 11 441 531 </br>
                  <b> Madrid : </b> 5 965 815 </br>
                  <b> Bucarest : </b> 2 133 066 </br>
                  <b> Innsbruck : </b> 276 003 </FONT>"))
  })
  
  output$InfosRescaling6 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Population in number of inhabitants </b> </br></br>
                  <b> Paris : </b> 11 441 531 </br>
                  <b> Madrid : </b> 5 965 815 </br>
                  <b> Bucarest : </b> 2 133 066 </br>
                  <b> Innsbruck : </b> 276 003 </FONT>"))
  })
  
  output$InfosRescaling7 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Population in number of inhabitants </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes3], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes3],0), big.mark = " "), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes4], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes4],0), big.mark = " "), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes5], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes5],0), big.mark = " ")))
  })
  
  output$InfosRescaling8 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Population in number of inhabitants </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes1], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes1],0), big.mark = " "), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes2], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes2],0), big.mark = " ")))
  })
  
  output$InfosRescaling9 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Population in number of inhabitants </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes7], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes7],0), big.mark = " "), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes8], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes8],0), big.mark = " ")))
  })
  
  output$InfosRescaling10 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Population in number of inhabitants </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes9], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes9],0), big.mark = " "), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes10], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes10],0), big.mark = " ")))
  })
  
  output$InfosRescaling11 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Population in number of inhabitants </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes11], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes11],0), big.mark = " "), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes12], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes12],0), big.mark = " ")))
  })
  
  output$InfosRescaling12 <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Population in number of inhabitants </b> </FONT> </br>", 
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes13], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes13],0), big.mark = " "), "</br>",
                  "<b>", centre_ville2$cityname[centre_ville2$cityname == input$villes14], " : ", "</b>",
                  format(round(centre_ville2$totpop[centre_ville2$cityname == input$villes14],0), big.mark = " ")))
  })
  
  
  
  output$SumCities <- renderUI({
    tags$div(HTML("<FONT size='4pt'> <b> Number of similar cities </b> </FONT> </br>", 
                  length(myListSimil)))
  })
  
  
  output$InfosArtificialisation <- renderUI({
    tags$div(HTML("<FONT size='4pt'>
                  <b> Definition of artificial land use: </b> </FONT> </br>",
                  "<FONT size='3pt'>
                  All Urban Atlas land use categories except: water areas, 
                  agricultural land, urban green spaces and forests.
                  </FONT>"))
  })
  
  output$InfosFUA <- renderUI({
    tags$div(HTML("<FONT size='4pt'>
                  <b> Definition of Functional Urban Areas (FUA) : </b> </FONT> </br>",
                  "<FONT size='3pt'>
                  Urban area with one or more urban centers of at least 50,000 inhabitants and its commuting 
                  zone (all municipalities where at least 15% of the working residents work in the urban center). </br>
                  Source : Eurostat
                  </FONT>"))
  })
  
  output$InfosDonnees <- renderUI({
    tags$div(HTML("<FONT size='4pt'>
                  <b> Data : </b> </FONT> </br>",
                  "<FONT size='3pt'>
                  Use of the 2006 Urban Atlas data (developed in the framework of the GMES/Copernicus project) 
                  for the 305 largest European urban areas (FUA) with more than 100 000 inhabitants </br>
                  --> Made from SPOT satellite images at a resolution of 2.5 m which are segmented and classified 
                  automatically when possible </br>
                  --> Photo-interpretation using topographic maps, cadastral boundaries and data on soil sealing </br>
                  --> The smallest mapped area is 0.25 ha </br> </br>
                  ==> Much more precise than Corine Land Cover (1/10,000th versus 1/100,000th)
                  </FONT>"))
  })
  
  output$NoteLecture1 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Note for the reader: </b> </br>
                  The land use curves represent averages over concentric rings of 141 m around City Hall. </br>
                  The artificial surface around the city center thus scales linearly with the population of the 
                  city studied. </br> </br> 
                  The urban area of a city is rescaled to the size of the city of London by its total population 
                  in a homothety. Homothety is a mathematical transformation that multiplies distances by a 
                  factor while preserving shapes.
                  </FONT>"))
  })
  
  output$NoteLecture2 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Note for the reader: </b> </br>
                  This synthesis shows that the rescaling by the square root of the population of each city is quite
                  consistent. Indeed, the difference for the first and last quantile on the share of atificial 
                  land use is a maximum of 0.4 points for the 300 largest European cities. </br></br>
                  The fluctuations around the mean are quite limited and are equally distributed when
                  the positive and negative deviations are compared.
                  </FONT>"))
  })
  
  output$NoteLecture3 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Note for the reader: </b> </br>
                  Using urban scaling laws, Madrid obtains a rescaling factor of 1.421. 
                  Thus, Madrid has the same share of artificial land use at 21.11 km (30/1.421) from its city hall 
                  as at 30 km from London's city hall.
                  </FONT>"))
  })
  
  output$NoteLecture4 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  European cities show strong gradients between the center and the periphery of built-up urban areas. </br>
                  Differences in urban morphologies and structures, such as elevation or the presence of water can explain 
                  some of the differences between cities and the way a city develops.
                  </FONT>"))
  })
  
  output$NoteLecture5 <- renderUI({
    tags$div(HTML("<FONT size='3pt'>
                  <b> Note for the reader: </b> </br>
                  The vertical line on the graph represents the share of artificial land use at the selected distance. </br>
                  The dotted lines show the European average of artificial land use shares.
                  </FONT>"))
  })
  
  output$NoteLecture6 <- renderUI({
    tags$div(HTML("<FONT size='4pt'>
                  Despite urban scaling laws, there are some variations that appear at the margin in Europe. </br> </br> 
                  For example, French cities are more artificial than Spanish cities. 
                  It is therefore interesting to analyze the differences in artificial land uses and to see 
                  which types of artificial spaces can be found in excess in France.
                  </FONT>"))
  })
  
  
  
  
  # output$legende_pop <- renderImage({
  #   list(src = "Application/Essai_application/www/legende_pop.png",
  #        height = 400, width = 120)
  # })
  
  
  # output$graph1 <- renderPlot({
  #   ggplot(data = artificial_cities, aes(x = dist_km)) +
  #     geom_line(aes(y = es001l_madrid, x = dist_km*(((rescaling_factors$totpop[300])/
  #                                                  (rescaling_factors$totpop[127]))^(0.8)),
  #                   color = "Madrid"), size = 1) +
  #     geom_line(aes(y = ro001l_bucuresti, x = dist_km*(((rescaling_factors$totpop[300])/
  #                                                     (rescaling_factors$totpop[249]))^(0.8)),
  #                   color = "Bucarest"), size = 1) +
  #     geom_line(aes(y = fr001l_paris, x = dist_km*(((rescaling_factors$totpop[300])/
  #                                                 (rescaling_factors$totpop[37]))^(0.8)),
  #                   color = "Paris"), size = 1) +
  #     geom_line(aes(y = be007l_namur, x = dist_km*(((rescaling_factors$totpop[300])/
  #                                                 (rescaling_factors$totpop[7]))^(0.8)),
  #                   color = "Namur"), size = 1) +
  #     scale_color_manual(name = "Legend:", 
  #                        values = c("purple", "mediumorchid1", "orange1", "gold1"),
  #                        breaks = c("Paris", "Madrid", "Bucarest", "Namur")) +
  #     scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
  #     scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  #     labs(title = "Artificial land use after rescaling in 2006",
  #          x = "Rescaled distance from City Hall (km)",
  #          y = "Share of artificial land use") +
  #     theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
  #                        legend.title = element_text(size = 13),
  #                        legend.text = element_text(size = 11))
  # })
  
  
  output$graph3 <- renderPlotly({
    ggplotly(
      ggplot(DF_artif, aes(x = dist_km, y = Value, colour = log(totpop), group = City)) + 
        geom_line(show.legend = F,  size = 0.18) +
        scale_colour_gradient(low = "yellow", high = "purple") +
        labs(title = "Artificial land use in 2006", 
             x = "Distance from City Hall (km)", y = "Share of artificial land use") + 
        scale_x_continuous(limit = c(0,56), breaks = c(0,10,20,30,40,50)) +
        scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
        theme_bw() + theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
                           legend.title = element_text(size = 13), 
                           legend.text = element_text(size = 11)))
  })
  
  output$graph4 <- renderPlot({
    ggplot(data = artificial_cities, aes(x = dist_km)) +
      geom_line(aes(y = es001l_madrid, color = "Madrid"), size = 1) +
      geom_line(aes(y = ro001l_bucuresti, color = "Bucarest"), size = 1) +
      geom_line(aes(y = fr001l_paris, color = "Paris"), size = 1) +
      geom_line(aes(y = at005l_innsbruck, color = "Innsbruck"), size = 1) +
      scale_color_manual(name = "Legend:", 
                         values = c("purple", "mediumorchid1", "orange1", "gold1"),
                         breaks = c("Paris", "Madrid", "Bucarest", "Innsbruck")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use in 2006",
           x = "Distance from City Hall (km)",
           y = "Share of artificial land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13),
                         legend.text = element_text(size = 11))
  })
  
  output$graph5 <- renderPlotly({
    ggplotly(
      ggplot(DF_artif, aes(x = dist_km*LU_k_fact, y = Value, color = log(totpop), group = City)) + 
        geom_line(show.legend = F, size = 0.18) +
        scale_colour_gradient(low = "yellow", high = "purple") +
        scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
        scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
        labs(title = "Artificial land use after rescaling in 2006", 
             x = "Rescaled distance from City Hall (km)", 
             y = "Share of artificial land use", color = "Legend:") +
        theme_bw() + theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
                           legend.title = element_text(size = 13), 
                           legend.text = element_text(size = 11)))
  })
  
  output$graph6 <- renderPlot({
    ggplot(data = artificial_cities, aes(x = dist_km)) +
      geom_line(aes(y = es001l_madrid, x = dist_km*(k_fact$LU_k_fact[127]), color = "Madrid"), 
                size = 1) +
      geom_line(aes(y = ro001l_bucuresti, x = dist_km*(k_fact$LU_k_fact[249]), color = "Bucarest"), 
                size = 1) +
      geom_line(aes(y = fr001l_paris, x = dist_km*(k_fact$LU_k_fact[37]), color = "Paris"), 
                size = 1) +
      geom_line(aes(y = at005l_innsbruck, x = dist_km*(k_fact$LU_k_fact[43]), color = "Innsbruck"), 
                size = 1) +
      scale_color_manual(name = "Legend:", 
                         values = c("purple", "mediumorchid1", "orange1", "gold1"),
                         breaks = c("Paris", "Madrid", "Bucarest", "Innsbruck")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use after rescaling in 2006",
           x = "Rescaled distance from City Hall (km)",
           y = "Share of artificial land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13),
                         legend.text = element_text(size = 11))
  })
  
  output$graph7 <- renderPlot({
    ggplot(data = artificial_cities_average, aes(x = dist_km)) + 
      geom_line(aes(y = artif_av, color = "Average"), size = 1.03) + 
      geom_line(aes(y = X50.quant, linetype = "longdash", color = "Median"), size = 1.2) +
      geom_line(aes(y = X10.quant, linetype = "dotted", color = "10 % quantile"), size = 1.2) +
      geom_line(aes(y = X90.quant, linetype = "dotted", color = "90 % quantile"), size = 1.2) +
      geom_line(aes(y = X25.quant, linetype = "dashed", color = "25 % quantile"), size = 1.2) +
      geom_line(aes(y = X75.quant, linetype = "dashed", color = "75 % quantile"), size = 1.2) +
      scale_color_manual(values = c("aquamarine2", "darksalmon", "darksalmon", "aquamarine2", 
                                    "red", "black","darkorchid1")) +
      scale_x_continuous(limit = c(0,75), breaks = c(0,10,20,30,40,50,60,70)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of artificial land use", color = "Legend:") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) +
      guides(linetype = FALSE)
  })
  
  output$graph27 <- renderPlot({
  ggplot(data = artificial_cities_average, aes(x = dist_km)) + 
      geom_line(aes(y = artif_av, color = "Average"), size = 1.03) + 
      geom_line(aes(y = X50.quant, linetype = "longdash", color = "Median"), size = 1.2) +
      geom_line(aes(y = X10.quant, linetype = "dotted", color = "10 % quantile"), size = 1.2) +
      geom_line(aes(y = X90.quant, linetype = "dotted", color = "90 % quantile"), size = 1.2) +
      geom_line(aes(y = X25.quant, linetype = "dashed", color = "25 % quantile"), size = 1.2) +
      geom_line(aes(y = X75.quant, linetype = "dashed", color = "75 % quantile"), size = 1.2) +
      scale_color_manual(values = c("aquamarine2", "darksalmon", "darksalmon", "aquamarine2", 
                                    "red", "black","darkorchid1")) +
    scale_y_log10(limits=c(0.01, 1), labels = scales::label_number(accuracy = 0.01, scale = 1))+
      scale_x_continuous(limit = c(0,75), breaks = c(0,10,20,30,40,50,60,70)) +
      labs(title = "Artificial land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of artificial land use", color = "Legend:") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) +
      guides(linetype = FALSE)
  })
  
  output$graph8 <- renderPlot({
    DF_artif_select <- DF_artif[DF_artif$cityname==input$villes3 | DF_artif$cityname==input$villes4 | DF_artif$cityname==input$villes5, ]
    ggplot() + 
      geom_line(data = DF_artif_select, aes(x = dist_km, y = Value, color = cityname), size = 1.2) + 
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use in 2006", 
           x = "Distance from City Hall (km)", 
           y = "Share of artificial land use", color = "Legend:") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  output$graph9 <- renderPlot({
    DF_artif_select <- DF_artif[DF_artif$cityname==input$villes3 | DF_artif$cityname==input$villes4 | DF_artif$cityname==input$villes5, ]
    ggplot() + 
      geom_line(data = DF_artif_select, aes(x = dist_km*LU_k_fact, y = Value,
                                            color = cityname), size = 1.2) +
      geom_line(data = artificial_average_sel, aes(x = dist_km, y = Value), 
                linetype = "dashed", col = "black", size = 1.2) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of artificial land use", color = "Legend:") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11))
  })
  
  output$graph10 <- renderPlot({
    DF_artif_select <- DF_artif[DF_artif$cityname==input$villes1 | DF_artif$cityname==input$villes2, ]
    ggplot() + 
      geom_line(data = DF_artif_select, aes(x = dist_km, y = Value, color = cityname), size = 1.2) +
      geom_vline(xintercept = input$distance, col = "black", size = 0.9) +
      scale_x_continuous(limit = c(0,75), breaks = c(0,10,20,30,40,50,60,70)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use in 2006", 
           x = "Distance from City Hall (km)", 
           y = "Share of artificial land use", color = "Legend:") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11))
  })
  
  output$graph11 <- renderPlot({
    DF_artif_select <- DF_artif[DF_artif$cityname==input$villes1 | DF_artif$cityname==input$villes2, ]
    ggplot() + 
      geom_line(data = DF_artif_select, aes(x = dist_km*LU_k_fact, y = Value,
                                            color = cityname), size = 1.2) +
      geom_line(data = artificial_average_sel, aes(x = dist_km,  y = Value), 
                linetype = "dashed", col = "darkorchid4", size = 1.2) +
      geom_vline(xintercept = input$distance, col = "black", size = 0.9) +
      scale_x_continuous(limit = c(0,75), breaks = c(0,10,20,30,40,50,60,70)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of artificial land use", color = "Legend:") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11))
  })
  
  output$graph12 <- renderPlot({
    ggplot() +
      geom_line(data = artif_cities_Rescaling, aes(x = dist_km, y = input$villes6), 
                col = "red", size = 1.2) +
      geom_line(data = artificial_cities_average, aes(x = dist_km, y = artif_av), 
                linetype = "dashed", col = "black", size = 1.2) +
      scale_x_continuous(limit = c(0,95), breaks = c(0,10,20,30,40,50,60,70,80,90)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use after rescaling in 2006",
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of artificial land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13),
                         legend.text = element_text(size = 11))
  })
  
  output$graph13 <- renderPlot({
    ggplot(data = artif_EU_ES_FR_av) + 
      geom_line(aes(x = dist_km, y = Value, group = country, color = country, linetype = country), 
                size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,75), breaks = c(0,10,20,30,40,50,60,70)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of artificial land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  output$graph14 <- renderPlot({
    DF_artif_select <- DF_artif[DF_artif$cityname==input$villes7 | DF_artif$cityname==input$villes8, ]
    ggplot() + 
      geom_line(data = DF_artif_select, aes(x = dist_km*LU_k_fact, y = Value, color = cityname), 
                size = 1.2) +
      geom_line(data = artificial_cities_average, aes(x = dist_km, y = artif_av, color = country, 
                                                      linetype = "dashed"), size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,75), breaks = c(0,10,20,30,40,50,60,70)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Artificial land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of artificial land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  output$graph15 <- renderPlot({
    ggplot(data = housing_EU_ES_FR_av) + 
      geom_line(aes(x = dist_km, y = Value, group = country, color = country, linetype = country), 
                size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Urban fabric land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of urban fabric land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + 
      guides(linetype = FALSE)
  })
  
  output$graph16 <- renderPlot({
    housing_artif_select <- housing_artif[housing_artif$cityname==input$villes9 | 
                                            housing_artif$cityname==input$villes10, ]
    ggplot() + 
      geom_line(data = housing_artif_select, aes(x = dist_km, y = Value, color = cityname), 
                size = 1.2) +
      geom_line(data = housing_average, aes(x = dist_km, y = housing_av, color = country, 
                                            linetype = "dashed"), size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Urban fabric land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of urban fabric land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  output$graph17 <- renderPlot({
    sub_housing_fr_es <- housing_city_av[which(housing_city_av$country=="fr"| 
                                                 housing_city_av$country=="es"),]
    attach(sub_housing_fr_es)
    housing_fr_es <- sub_housing_fr_es[which(dist_km==10 | dist_km == 20 | dist_km == 30 |	
                                               dist_km == 40 | dist_km == 50 | dist_km == 60 | 
                                               dist_km == 70),]
    detach(sub_housing_fr_es)
    housing_fr_es$dist2 <- housing_fr_es$dist_km
    housing_fr_es$dist2 <- str_replace_all(housing_fr_es$dist2, "10", "r' = 10km")
    housing_fr_es$dist2 <- str_replace_all(housing_fr_es$dist2, "20", "r' = 20km")
    housing_fr_es$dist2 <- str_replace_all(housing_fr_es$dist2, "30", "r' = 30km")
    housing_fr_es$dist2 <- str_replace_all(housing_fr_es$dist2, "40", "r' = 40km")
    housing_fr_es$dist2 <- str_replace_all(housing_fr_es$dist2, "50", "r' = 50km")
    housing_fr_es$dist2 <- str_replace_all(housing_fr_es$dist2, "60", "r' = 60km")
    housing_fr_es$dist2 <- str_replace_all(housing_fr_es$dist2, "70", "r' = 70km")
    
    ggplot(data = housing_fr_es) + 
      geom_boxplot(aes(x = dist2, y = diff_av, fill = toupper(country)), position = position_dodge(1)) +
      labs(title = "Urban fabric land use after rescaling in France and Spain, in 2006", 
           x = "Boxplots at various distance rescaled to the center", y = "", fill = "Legend:") +
      scale_y_continuous(limits = c(-0.25,0.25)) +
      scale_x_discrete(labels = c("", "", "", "")) +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 14), 
                         legend.text = element_text(size = 12)) +
      facet_grid(. ~ dist2, scales = "free", space = "free") 
  })
  
  output$graph18 <- renderPlot({
    ggplot(data = industries_EU_ES_FR_av) + 
      geom_line(aes(x = dist_km, y = Value, group = country, color = country, linetype = country), 
                size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Industries land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of industries land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + 
      guides(linetype = FALSE)
  })
  
  output$graph19 <- renderPlot({
    industries_artif_select <- industries_artif[industries_artif$cityname==input$villes11 | 
                                                  industries_artif$cityname==input$villes12, ]
    ggplot() + 
      geom_line(data = industries_artif_select, aes(x = dist_km, y = Value, 
                                                    color = cityname), size = 1.2) +
      geom_line(data = industries_average, aes(x = dist_km, y = indus_av, color = country, 
                                               linetype = "dashed"), size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Industries land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of industries land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  output$graph20 <- renderPlot({
    sub_industries_fr_es <- industries_city_av[which(industries_city_av$country=="fr"| 
                                                       industries_city_av$country=="es"),]
    attach(sub_industries_fr_es)
    industries_fr_es <- sub_industries_fr_es[which(dist_km == 10 | dist_km == 20 | dist_km == 30 |	
                                                     dist_km == 40 | dist_km == 50 | dist_km == 60 | 
                                                     dist_km == 70),]
    detach(sub_industries_fr_es)
    industries_fr_es$dist2 <- industries_fr_es$dist_km
    industries_fr_es$dist2 <- str_replace_all(industries_fr_es$dist2, "10", "r' = 10km")
    industries_fr_es$dist2 <- str_replace_all(industries_fr_es$dist2, "20", "r' = 20km")
    industries_fr_es$dist2 <- str_replace_all(industries_fr_es$dist2, "30", "r' = 30km")
    industries_fr_es$dist2 <- str_replace_all(industries_fr_es$dist2, "40", "r' = 40km")
    industries_fr_es$dist2 <- str_replace_all(industries_fr_es$dist2, "50", "r' = 50km")
    industries_fr_es$dist2 <- str_replace_all(industries_fr_es$dist2, "60", "r' = 60km")
    industries_fr_es$dist2 <- str_replace_all(industries_fr_es$dist2, "70", "r' = 70km")
    
    
    ggplot(data = industries_fr_es) + 
      geom_boxplot(aes(x = dist2, y = diff_av, fill = toupper(country)), position = position_dodge(1)) +
      labs(title = "Industries land use after rescaling in France and Spain, in 2006", 
           x = "Boxplots at various distance rescaled to the center", y = "", fill = "Legend:") +
      scale_y_continuous(limits = c(-0.25,0.25)) +
      scale_x_discrete(labels = c("", "", "", "")) +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 14), 
                         legend.text = element_text(size = 12)) +
      facet_grid(. ~ dist2, scales = "free", space = "free") 
  })
  
  output$graph21 <- renderPlot({
    ggplot(data = roads_EU_ES_FR_av) + 
      geom_line(aes(x = dist_km, y = Value, group = country, color = country, linetype = country), 
                size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,0.45), breaks = c(0,0.1,0.2,0.3,0.4)) +
      labs(title = "Roads land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of roads land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + 
      guides(linetype = FALSE)
  })
  
  output$graph22 <- renderPlot({
    roads_artif_select <- roads_artif[roads_artif$cityname==input$villes13 | 
                                        roads_artif$cityname==input$villes14, ]
    ggplot() + 
      geom_line(data = roads_artif_select, aes(x = dist_km, y = Value, 
                                               color = cityname), size = 1.2) +
      geom_line(data = roads_average, aes(x = dist_km, y = artificial_av, color = country, 
                                          linetype = "dashed"), size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("black","blue","red"), 
                         guide = guide_legend(ncol = 1, nrow = 3, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed", "solid", "solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,0.45), breaks = c(0,0.1,0.2,0.3,0.4)) +
      labs(title = "Roads land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of roads land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  output$graph23 <- renderPlot({
    sub_roads_fr_es <- roads_city_av[which(roads_city_av$country=="fr"| 
                                             roads_city_av$country=="es"),]
    attach(sub_roads_fr_es)
    roads_fr_es <- sub_roads_fr_es[which(dist_km == 10 | dist_km == 20 | dist_km == 30 |	
                                           dist_km == 40 | dist_km == 50 | dist_km == 60 | 
                                           dist_km == 70),]
    detach(sub_roads_fr_es)
    roads_fr_es$dist2 <- roads_fr_es$dist_km
    roads_fr_es$dist2 <- str_replace_all(roads_fr_es$dist2, "10", "r' = 10km")
    roads_fr_es$dist2 <- str_replace_all(roads_fr_es$dist2, "20", "r' = 20km")
    roads_fr_es$dist2 <- str_replace_all(roads_fr_es$dist2, "30", "r' = 30km")
    roads_fr_es$dist2 <- str_replace_all(roads_fr_es$dist2, "40", "r' = 40km")
    roads_fr_es$dist2 <- str_replace_all(roads_fr_es$dist2, "50", "r' = 50km")
    roads_fr_es$dist2 <- str_replace_all(roads_fr_es$dist2, "60", "r' = 60km")
    roads_fr_es$dist2 <- str_replace_all(roads_fr_es$dist2, "70", "r' = 70km")
    
    
    ggplot(data = roads_fr_es) + 
      geom_boxplot(aes(x = dist2, y = diff_av, fill = toupper(country)), position = position_dodge(1)) +
      labs(title = "Roads land use after rescaling in France and Spain, in 2006", 
           x = "Boxplots at various distance rescaled to the center", y = "", fill = "Legend:") +
      scale_y_continuous(limits = c(-0.25,0.25)) +
      scale_x_discrete(labels = c("", "", "", "")) +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 14), 
                         legend.text = element_text(size = 12)) +
      facet_grid(. ~ dist2, scales = "free", space = "free") 
  })
  
  
  output$graph24 <- renderPlot({
    housing_artif_sel <- housing_artif[housing_artif$cityname==input$villes3 | 
                                         housing_artif$cityname==input$villes4 | 
                                         housing_artif$cityname==input$villes5, ]
    ggplot() + 
      geom_line(data = housing_artif_sel, aes(x = dist_km, y = Value, color = cityname), 
                size = 1.2) +
      geom_line(data = housing_average, aes(x = dist_km, y = housing_av, color = country, 
                                            linetype = "dashed"), size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("firebrick1","black","forestgreen","dodgerblue"), 
                         guide = guide_legend(ncol = 1, nrow = 4, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed","solid","solid","solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Urban fabric land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of urban fabric land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  
  output$graph25 <- renderPlot({
    industries_artif_sel <- industries_artif[industries_artif$cityname==input$villes3 | 
                                               industries_artif$cityname==input$villes4 | 
                                               industries_artif$cityname==input$villes5, ]
    ggplot() + 
      geom_line(data = industries_artif_sel, aes(x = dist_km, y = Value, color = cityname), 
                size = 1.2) +
      geom_line(data = industries_average, aes(x = dist_km, y = indus_av, color = country, 
                                               linetype = "dashed"), size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("firebrick1","black","forestgreen","dodgerblue"), 
                         guide = guide_legend(ncol = 1, nrow = 4, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed","solid","solid","solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1)) +
      labs(title = "Industries land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of industries land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  output$graph26 <- renderPlot({
    roads_artif_sel <- roads_artif[roads_artif$cityname==input$villes3 | 
                                     roads_artif$cityname==input$villes4 | 
                                     roads_artif$cityname==input$villes5, ]
    ggplot() + 
      geom_line(data = roads_artif_sel, aes(x = dist_km, y = Value, color = cityname), 
                size = 1.2) +
      geom_line(data = roads_average, aes(x = dist_km, y = artificial_av, color = country, 
                                          linetype = "dashed"), size = 1.2) +
      scale_color_manual(name = "Legend:", values = c("firebrick1","black","forestgreen","dodgerblue"), 
                         guide = guide_legend(ncol = 1, nrow = 4, title.position = 'top', 
                                              title = "Legend:")) +
      scale_linetype_manual(values = c("dashed","solid","solid","solid")) +
      scale_x_continuous(limit = c(0,55), breaks = c(0,10,20,30,40,50)) +
      scale_y_continuous(limit = c(0,0.45), breaks = c(0,0.1,0.2,0.3,0.4)) +
      labs(title = "Roads land use after rescaling in 2006", 
           x = "Rescaled distance from City Hall (km)", 
           y = "Share of roads land use") +
      theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                         legend.title = element_text(size = 13), 
                         legend.text = element_text(size = 11)) + guides(linetype = FALSE)
  })
  
  
  
  
  output$map_1 <- renderPlot({
    DisplayBuffer <- function(table, town, buffer){
      # Traitement de la donnée
      city_tab <- table[table$cityname==town,]
      # city_tab <- centre_ville2[centre_ville2$cityname=="Paris",]
      city_tab <- st_as_sf(city_tab, coords = c("X", "Y"), crs = 4326) %>% 
        st_transform(3035) %>% 
        st_buffer(buffer) %>% 
        st_transform(4326)
      # Extension spatiale
      bbox <- st_bbox(city_tab)
      names(bbox) <- c("left","bottom","right","top")
      # LAT1 = st_bbox(city_tab)[2] ; LAT2 = st_bbox(city_tab)[4]
      # LON1 = st_bbox(city_tab)[1] ; LON2 = st_bbox(city_tab)[3]
      map <- get_stadiamap(bbox = bbox, maptype = "stamen_terrain") 
      mytheme <- theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                       axis.title = element_text(face = "bold", size = rel(1)),
                       axis.title.y = element_text(angle = 90, vjust = 2),
                       axis.title.x = element_text(vjust = -0.2))
      # Afficher la carte
      ggmap(map) +
        geom_sf(data = city_tab, inherit.aes = FALSE, alpha = 0, size = 1) +
        labs(title = "PARIS \n Radius = 29,23 km", x = "Longitude", y = "Latitude") + 
        mytheme +
        scalebar(city_tab, location = "bottomleft", dist = round((((30)/(1.026347))*0.5), 0), 
                 dist_unit = "km", height = 0.013, transform = TRUE, 
                 model = "WGS84", st.bottom = FALSE)
    }
    DisplayBuffer(centre_ville2, "Paris", 30000/1.026347)
  })
  
  output$map_2 <- renderPlot({
    DisplayBuffer <- function(table, town, buffer){
      city_tab <- table[table$cityname==town,]
      city_tab <- st_as_sf(city_tab, coords = c("X", "Y"), crs = 4326) %>% 
        st_transform(3035) %>% 
        st_buffer(buffer) %>% 
        st_transform(4326)
      bbox <- st_bbox(city_tab)
      names(bbox) <- c("left","bottom","right","top")
      map <- get_stadiamap(bbox = bbox, maptype = "stamen_terrain", zoom = 12) 
      mytheme <- theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                       axis.title = element_text(face = "bold", size = rel(1)),
                       axis.title.y = element_text(angle = 90, vjust = 2),
                       axis.title.x = element_text(vjust = -0.2))
      ggmap(map) +
        geom_sf(data = city_tab, inherit.aes = FALSE, alpha = 0, size = 1) +
        labs(title = "MADRID \n Radius = 21,11 km", x = "Longitude", y = "Latitude") + 
        mytheme +
        scalebar(city_tab, location = "bottomleft", dist = round((((30)/(1.421352))*0.5), 0), 
                 dist_unit = "km", height = 0.013, transform = TRUE, 
                 model = "WGS84", st.bottom = FALSE)
    }
    DisplayBuffer(centre_ville2, "Madrid", 30000/1.421352)
  })
  
  output$map_3 <- renderPlot({
    DisplayBuffer <- function(table, town, buffer){
      city_tab <- table[table$cityname==town,]
      city_tab <- st_as_sf(city_tab, coords = c("X", "Y"), crs = 4326) %>% 
        st_transform(3035) %>% 
        st_buffer(buffer) %>% 
        st_transform(4326)
      bbox <- st_bbox(city_tab)
      names(bbox) <- c("left","bottom","right","top")
      map <- get_stadiamap(bbox = bbox, maptype = "stamen_terrain", zoom = 13) 
      mytheme <- theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                       axis.title = element_text(face = "bold", size = rel(1)),
                       axis.title.y = element_text(angle = 90, vjust = 2),
                       axis.title.x = element_text(vjust = -0.2))
      ggmap(map) +
        geom_sf(data = city_tab, inherit.aes = FALSE, alpha = 0, size = 1) +
        labs(title = "BUCAREST \n Radius = 12,62 km", x = "Longitude", y = "Latitude") + 
        mytheme +
        scalebar(city_tab, location = "bottomleft", dist = round((((30)/(2.377028))*0.5), 0), 
                 dist_unit = "km", height = 0.013, transform = TRUE, 
                 model = "WGS84", st.bottom = FALSE)
    }
    DisplayBuffer(centre_ville2, "Bucuresti", 30000/2.377028)
  })
  
  output$map_4 <- renderPlot({
    DisplayBuffer <- function(table, town, buffer){
      city_tab <- table[table$cityname==town,]
      city_tab <- st_as_sf(city_tab, coords = c("X", "Y"), crs = 4326) %>% 
        st_transform(3035) %>% 
        st_buffer(buffer) %>% 
        st_transform(4326)
      bbox <- st_bbox(city_tab)
      names(bbox) <- c("left","bottom","right","top")
      map <- get_stadiamap(bbox = bbox, maptype = "stamen_terrain", zoom = 15) 
      mytheme <- theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                       axis.title = element_text(face = "bold", size = rel(1)),
                       axis.title.y = element_text(angle = 90, vjust = 2),
                       axis.title.x = element_text(vjust = -0.2))
      ggmap(map) +
        geom_sf(data = city_tab, inherit.aes = FALSE, alpha = 0, size = 1) +
        labs(title = "INNSBRUCK \n Radius = 4,54 km", x = "Longitude", y = "Latitude") + 
        mytheme +
        scalebar(city_tab, location = "bottomleft", dist = round((((30)/(6.608141))*0.5), 0), 
                 dist_unit = "km", height = 0.013, transform = TRUE, 
                 model = "WGS84", st.bottom = FALSE)
    }
    DisplayBuffer(centre_ville2, "Innsbruck", 30000/6.608141)
  })
  
  output$map_5 <- renderPlot({
    DisplayBuffer <- function(table, town, buffer){
      city_tab <- table[table$cityname==town,]
      city_tab <- st_as_sf(city_tab, coords = c("X", "Y"), crs = 4326) %>% 
        st_transform(3035) %>% 
        st_buffer(buffer) %>% 
        st_transform(4326)
      bbox <- st_bbox(city_tab)
      names(bbox) <- c("left","bottom","right","top")
      map <- get_stadiamap(bbox = bbox, maptype = "stamen_terrain", zoom = 13) 
      mytheme <- theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                       axis.title = element_text(face = "bold", size = rel(1)),
                       axis.title.y = element_text(angle = 90, vjust = 2),
                       axis.title.x = element_text(vjust = -0.2))
      ggmap(map) +
        geom_sf(data = city_tab, inherit.aes = FALSE, alpha = 0, size = 1) +
        labs(x = "Longitude", y = "Latitude") + 
        mytheme +
        scalebar(city_tab, location = "bottomleft", 
                 dist = round((((input$distance)/as.numeric(centre_ville2[centre_ville2$cityname==input$villes1,'LU_k_fact']))*0.5), 0),
                 dist_unit = "km", height = 0.013, transform = TRUE, 
                 model = "WGS84", st.bottom = FALSE)
    }
    DisplayBuffer(centre_ville2, input$villes1, (input$distance*1000)/(as.numeric(centre_ville2[centre_ville2$cityname==input$villes1,'LU_k_fact'])))
  })
  
  output$map_6 <- renderPlot({
    DisplayBuffer <- function(table, town, buffer){
      city_tab <- table[table$cityname==town,]
      city_tab <- st_as_sf(city_tab, coords = c("X", "Y"), crs = 4326) %>% 
        st_transform(3035) %>% 
        st_buffer(buffer) %>% 
        st_transform(4326)
      bbox <- st_bbox(city_tab)
      names(bbox) <- c("left","bottom","right","top")
      map <- get_stadiamap(bbox = bbox, maptype = "stamen_terrain", zoom = 13) 
      mytheme <- theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
                       axis.title = element_text(face = "bold", size = rel(1)),
                       axis.title.y = element_text(angle = 90, vjust = 2),
                       axis.title.x = element_text(vjust = -0.2))
      ggmap(map) +
        geom_sf(data = city_tab, inherit.aes = FALSE, alpha = 0, size = 1) +
        labs(x = "Longitude", y = "Latitude") + 
        mytheme +
        scalebar(city_tab, location = "bottomleft", 
                 dist = round((((input$distance)/as.numeric(centre_ville2[centre_ville2$cityname==input$villes2,'LU_k_fact']))*0.5), 0),              
                 dist_unit = "km", height = 0.013, transform = TRUE, 
                 model = "WGS84", st.bottom = FALSE)
    }
    DisplayBuffer(centre_ville2, input$villes2, (input$distance*1000)/(as.numeric(centre_ville2[centre_ville2$cityname==input$villes2,'LU_k_fact'])))
  })
  
  
  
  
  
  pal <- colorNumeric("Reds", domain = Disque_artif$Value, reverse = F)
  
  output$mapLeaflet <- renderLeaflet({
    Disque_artif_select <- Disque_artif[Disque_artif$dist_km == input$disque,]
    
    Dist_toPlot <- as.numeric(input$disque)
    Disque_artif$label <- paste0("<b>", Disque_artif$cityname, "</b><br>",
                                 "Rescaling factor: ", "<b>", 
                                 round(Disque_artif$LU_k_fact, 3), "</b><br>",
                                 "Equivalent to a disk of radius: ", "<b>",
                                 round(Dist_toPlot/Disque_artif$LU_k_fact, 2), "</b>", " km", "<br>",
                                 "Share of artificial land use: ", "<b>",
                                 round(Disque_artif$Value[Disque_artif$dist_km==Dist_toPlot], 3), "</b>")
    leaflet() %>%
      addPolygons(data = EPCI_2020, color = "blue", fillOpacity = 0.1, weight = 2,
                  group = "EPCI 2020", label = ~NOM_EPCI) %>%
      addPolygons(data = FUA_2014, color = "black", fillOpacity = 0.1, weight = 2,
                  group = "FUA 2014", label = ~URAU_NAME) %>%
      addCircles(data = town_hall, lng = ~X, lat = ~Y, color = "black", fillOpacity = 0.67) %>%
      addCircles(data = Disque_artif_select, lng = ~X, lat = ~Y, 
                 radius = ~(as.numeric(input$disque)*1000)/(LU_k_fact),
                 weight = 1, color = ~pal(Value), fillOpacity = 0.04,
                 label = ~cityname, popup = Disque_artif$label) %>%
      addLegend(position = "topleft", values = Disque_artif_select$Value, pal = pal,
                bins = 5, title = "Artificial land use") %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(metric = TRUE, imperial = F)) %>%
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(provider = providers$NASAGIBS.ViirsEarthAtNight2012, group = "Luminosity in 2012") %>%
      addLayersControl(baseGroups = c("Luminosity in 2012", "Satellite"),
                       overlayGroups = c("EPCI 2020", "FUA 2014"))
    
  })
  
  
  output$mapLeaflet2 <- renderLeaflet({
    town_hall_select <- town_hall[town_hall$cityfile==input$villes6,]
    
    myListSimil <- c()
    myListSimil <- myTabCor[,input$villes6]  
    myListSimil <- myListSimil[myListSimil >= as.numeric(input$correlation[1]) &
                                 myListSimil <= as.numeric(input$correlation[2])]
    myListSimil <- c(names(myListSimil))
    myListSimil <- na.omit(myListSimil)
    myListSimil <- town_hall[town_hall$cityfile %in% myListSimil,]
    
    # town_hall_select$label <- paste0("<b>", town_hall$cityname, "</b><br>",
    #                                  "Population : ", "<b>",
    #                                  format(round(town_hall$totpop, 0), big.mark = " "), "</b>", 
    #                                  " habitants.", "<br>",
    #                                  "Facteur de mise à l'échelle : ", "<b>", 
    #                                  round(town_hall$LU_k_fact, 3), "</b>")
    # 
    # myListSimil$label <- paste0("<b>", town_hall$cityname, "</b><br>",
    #                             "Population : ", "<b>",
    #                             format(round(town_hall$totpop, 0), big.mark = " "), "</b>", 
    #                             " habitants.", "<br>",
    #                             "Facteur de mise à l'échelle : ", "<b>", 
    #                             round(town_hall$LU_k_fact, 3), "</b>")
    
    leaflet() %>%
      addCircles(data = town_hall_select, lng = ~X, lat = ~Y, color = "red",
                 radius = 50, weight = 11, label = ~cityname) %>% 
      # popup = ~label) %>%
      addCircles(data = myListSimil, lng = ~X, lat = ~Y, color = "#3300CC",
                 radius = 30, weight = 7, label = ~cityname) %>%
      # popup = ~label) %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(metric = T, imperial = F)) %>%
      addProviderTiles(provider = providers$Esri.WorldStreetMap)
  })
  
}


shinyApp(ui = ui, server = server)
