library(shiny)
library(shinyjs)

library(sf)
library(dplyr)
library(leaflet)
library(lubridate)




## Geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 
geocode <- function(address = NULL) {
    d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'https://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1'),
        flatten = T)
    
    if(length(d) == 0) return(data.frame())
    
    data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)) %>%
        return()
}


# Shapefile for NUTS3 regions
germany_nuts <- readRDS(file.path("data", "germany_nuts3_incidence.RDS"))

if (max(germany_nuts$sieben_tage, na.rm = TRUE) < 300) {
    pal_leg <-  c("keine Fälle übermittelt", "< 5", "5-25",
                  "25-50", "50-100", "100-150", "150-250", "> 250")
} else if (max(germany_nuts$sieben_tage, na.rm = TRUE) < 500) {
    pal_leg <-  c("keine Fälle übermittelt", "< 5", "5-25",
                  "25-50", "50-100", "100-200", "200-300", "> 300")
} else {
    pal_leg <-  c("keine Fälle übermittelt", "< 5", "5-25",
                  "25-50", "50-150", "150-300", "300-500", "> 500")
}


# Define UI -----------------------------------------------------
ui <- bootstrapPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "simplex"),
    shinyWidgets::setBackgroundColor("#d4dadc"),
    useShinyjs(),
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;} #loadingId{margin: auto;}"),
    
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript"),
    tags$script(
            '
            $(document).keyup(function(event) {
                if (event.key == "Enter") {
                    $("#go").click();
                }
            });
            
            $(document).ready(function () {
                navigator.geolocation.getCurrentPosition(onSuccess, onError);
                
                function onError (err) {
                    Shiny.onInputChange("geolocation", false);
                }
                            
                function onSuccess (position) {
                    setTimeout(function () {
                        var coords = position.coords;
                        console.log(coords.latitude + ", " + coords.longitude);
                        Shiny.onInputChange("geolocation", true);
                        Shiny.onInputChange("lat", coords.latitude);
                        Shiny.onInputChange("long", coords.longitude);
                    }, 1100)
                }
            });
            '
    ),
    
    absolutePanel(
        id = "loadingId",
        top = "45%", left = 0, right = 0, fixed = TRUE, draggable = FALSE, width = 100, height = "5%", style = "z-index:0; min-width: 40px;",
        p("Bitte warten...")
    ),
    
    leafletOutput("map", height = "100%", width = "100%"),

    absolutePanel(
        top = 100, left = 10, draggable = TRUE, width = "20%", style = "z-index:500; min-width: 135px;",
        textInput("address", "Adresse suchen", placeholder = "in Deutschland"),
        checkboxInput("use_location", "Oder nutze deinen aktuellen Standort!"),
        actionButton("go", "Suchen!", class = "btn-primary")
    ),
        
    absolutePanel(
        top = "96.5%", left = 5, draggable = FALSE, width = "20%", height = "3%", style = "z-index:500; min-width: 20px;",
        a(href="https://github.com/STBrinkmann/Covid19_Dashboard_Germany", "Mehr Infos..", target="_blank")
    )
)

# Define server ------------------------------------------------
server <- shinyServer(function(input, output) {
    addResourcePath("myPlots", "/srv/shiny-server/Covid_DE_Dashboard/Plots")

    # Leaflet map
    pal <- c("#D9D9D6", "#BBBCBC", "#FFC27B", "#FF8F1C", "#FE5000", "#A6192E", "#BB29BB")
    leafPal1 <- leaflet::colorFactor(pal, germany_nuts$color, na.color = "white")
    
    output$map <- renderLeaflet({
        leaflet::leaflet(leaflet::leafletOptions(leaflet::leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
            
            addMapPane(name = "polygons", zIndex = 410) %>% 
            addMapPane(name = "maplabels", zIndex = 420) %>%
            
            setMaxBounds(lng1 = -5,
                         lat1 = 40,
                         lng2 = 25,
                         lat2 = 60) %>%
            addProviderTiles("CartoDB.PositronNoLabels",
                             options = providerTileOptions(minZoom = 5, maxZoom = 11)) %>%
            leaflet::addProviderTiles("CartoDB.PositronOnlyLabels",
                                      options = providerTileOptions(minZoom = 5, maxZoom = 11,
                                                                    pane = "maplabels"),
                                      group = "map labels") %>%
            leaflet::setView(10.5, 51, zoom = 6) %>% 
            leaflet::addPolygons(data = germany_nuts,
                                 stroke = F, fill = TRUE, fillOpacity = 1, weight = 1.2,
                                 color = ~leafPal1(color), label = ~NUTS_NAME,
                                 
                                 group = "gadmCHE",
                                 options = leafletOptions(pane = "polygons"),
                                 
                                 highlight = highlightOptions(
                                     weight = 3,
                                     color = "black",
                                     opacity = 1,
                                     fillOpacity = 0.9,
                                     bringToFront = TRUE,
                                     sendToBack = TRUE),
                                 popup = paste0(
                                     "
                                 <html>
                                 <head>
                                 <style>
                                 table, th, td {
                                    border-collapse: collapse;
                                 }
                                 th, td {
                                    padding: 1;
                                    text-align: left;
                                 }
                                 tr.spaceUnder>td {                                     	
                                    line-height: 2.5;
                                 }
                                 </style>
                                 </head>
                                 <body>
                                 <h5><b>", germany_nuts$NUTS_NAME, "</b></h5>",
                                 "<table style=\"width:100%\">
                                 <tr>
                                     <th></th>
                                     <th></th>
                                 </tr>
                                 
                                 <tr class=\"spaceUnder\">
                                     <th>7-Tage-Inzidenz:</th>
                                     <td>", ifelse(!is.na(germany_nuts$sieben_tage), germany_nuts$sieben_tage, "Nicht verfügbar"), "</td>
                                 </tr>
                                 
                                 <tr>
                                     <th>Intensivbetten-Auslastung:</th>
                                     <td></td>
                                 </tr>
                                 
                                 <tr>
                                     <td>&emsp;Gesamt</td>
                                     <td>", germany_nuts$DIVI_ges, "</td>
                                 </tr>
                                 
                                 <tr>
                                     <td>&emsp;Davon COVID-19</td>
                                     <td>", ifelse(germany_nuts$DIVI_ges == "Nicht verfügbar",
                                                   "Nicht verfügbar", germany_nuts$DIVI_covid), "</td>
                                 </tr>
                                 
                                 <tr>
                                     <th><br>7-Tage-Inzidenz<br>der vergangenen 14 Tagen:</th>
                                     <td></td>
                                 </tr>
                            
                                 </table>
                                 
                                 <img src=", file.path("myPlots", paste0(germany_nuts$Kennziffer, ".svg")) ," border=0 height=90% width=90%></img>
                                 
                                 </body>
                                 </html>
                                 "
                                 )) %>% 
            leaflet::addPolylines(data = germany_nuts, 
                                  color = "#2D2926",
                                  stroke = TRUE,
                                  opacity = 1, 
                                  weight = 1,
                                  options = pathOptions(clickable = FALSE, pane = "polygons"),
                                  group = "gadmCHE") %>%
            leaflet::addLegend(data = germany_nuts,
                               "bottomright",
                               opacity = 1.0, 
                               colors = c("white", pal),
                               labels = pal_leg,
                               title = "7-Tage-Inzidenz")
    })
    
    
    # actionButton
    shiny::observeEvent(input$go, {
        
        shiny::withProgress(
            message = "Suche...",
            value = 1/3, {
                # Use Geolocaton
                if (input$use_location) {
                    
                    tryCatch({
                        shiny::validate(
                            shiny::need(input$geolocation, message = F)
                        )
                        
                        if(!input$geolocation) stop()
                    }, error = function(e) {
                        shiny::showModal(shiny::modalDialog(title = "Sorry!", 
                                                            tags$p("Du musst den Standortzugriff erlauben!"),
                                                            footer = shiny::modalButton("Abbrechen"),
                                                            easyClose = TRUE))
                    }
                    )
                    
                    shiny::validate(
                        shiny::need(input$geolocation, message = F)
                    )
                    
                    lat <- input$lat
                    lon <- input$long
                    
                    geo_point <- data.frame(lon = as.numeric(lon), lat = as.numeric(lat))
                    
                    # Use address input
                } else {
                    shiny::validate(
                        shiny::need(nchar(input$address) > 2, message = FALSE)
                    )
                    
                    
                    tryCatch({
                        geo_point <- geocode(address = input$address)
                        
                        if (length(geo_point) == 0) stop()
                    }, error = function(e) {
                        shiny::showModal(shiny::modalDialog(title = "Sorry!", 
                                                            tags$p("Wir konnten diese Adresse nicht finden."), 
                                                            tags$p("Versuch es noch einmal!"),
                                                            footer = shiny::modalButton("Abbrechen"),
                                                            easyClose = TRUE))
                    }
                    )
                    
                    shiny::validate(
                        shiny::need(length(geo_point) > 0, message = FALSE)
                    )
                    
                    lat <- geo_point$lat
                    lon <- geo_point$lon
                }
                
                incProgress(1/3)
                
                # Convert input to sf
                geo_point <- geo_point %>%
                    sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(germany_nuts)) %>% 
                    dplyr::rename(geom = geometry)
                
                # Intersect of NUTS3/LAU and address
                landkreis_intersect <- germany_nuts[geo_point, ]
                
                tryCatch({
                    if (nrow(landkreis_intersect) == 0) stop("Error")
                }, error = function(e) {
                    shiny::showModal(shiny::modalDialog(title = "Sorry!", 
                                                        tags$p("Diese Adresse scheint nicht innerhalb von Deutschland zu liegen."), 
                                                        tags$p("Versuch es noch einmal!"),
                                                        footer = shiny::modalButton("Abbrechen"),
                                                        easyClose = TRUE))
                }
                )
                
                shiny::validate(
                    shiny::need(nrow(landkreis_intersect) > 0, message = FALSE)
                )
                incProgress(1/3)
                
                # Update Leaflet map
                runjs('$(".leaflet-popup-close-button")[0].click();')
                
                leaflet::leafletProxy("map") %>%
                    leaflet::setView(lon, lat, zoom = 10) %>% 
                    leaflet::addPopups(lng = lon, lat = lat, 
                                       popup = paste0(
                                                  "
                                                 <html>
                                                 <head>
                                                 <style>
                                                 table, th, td {
                                                    border-collapse: collapse;
                                                 }
                                                 th, td {
                                                    padding: 1;
                                                    text-align: left;
                                                 }
                                                 tr.spaceUnder>td {
                                                    line-height: 2.5;
                                                 }
                                                 </style>
                                                 </head>
                                                 <body>
                                                 <h5><b>", landkreis_intersect$NUTS_NAME, "</b></h5>",
                                                 "<table style=\"width:100%\">
                                                 <tr>
                                                     <th></th>
                                                     <th></th>
                                                 </tr>
                                                 
                                                 <tr class=\"spaceUnder\">
                                                     <th>7-Tage-Inzidenz:</th>
                                                     <td>", ifelse(!is.na(landkreis_intersect$sieben_tage), landkreis_intersect$sieben_tage, "Nicht verfügbar"), "</td>
                                                 </tr>
                                                 
                                                 <tr>
                                                     <th>Intensivbetten-Auslastung:</th>
                                                     <td></td>
                                                 </tr>
                                                 
                                                 <tr>
                                                     <td>&emsp;Gesamt</td>
                                                     <td>", landkreis_intersect$DIVI_ges, "</td>
                                                 </tr>
                                                 
                                                 <tr>
                                                     <td>&emsp;Davon COVID-19</td>
                                                     <td>", ifelse(landkreis_intersect$DIVI_ges == "Nicht verfügbar",
                                                                   "Nicht verfügbar", landkreis_intersect$DIVI_covid), "</td>
                                                 </tr>
                                                 
                                                 <tr>
                                                     <th><br>7-Tage-Inzidenz<br>der vergangenen 14 Tagen:</th>
                                                     <td></td>
                                                 </tr>
                                            
                                                 </table>
                                                 
                                                 <img src=", file.path("myPlots", paste0(landkreis_intersect$Kennziffer, ".svg"))," border=0 height=90% width=90%></img>
                                                 
                                                 </body>
                                                 </html>
                                                 "
                              ))
                
                incProgress(1/3)
            }
        )
    })
})

# Run the application ------------------------------------------
shinyApp(ui = ui, server = server)
