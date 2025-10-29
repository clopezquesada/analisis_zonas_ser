library(shiny) # Librería para crear aplicaciones Shiny
library(leaflet) # Librería para crear mapas
library(xml2) # Librería para la lectura de datos en formato XML
library(httr) # Librería para la manipulación de URLs
library(dplyr) # Librería para el manejo de datos
library(lubridate) # Librería avanzada para el manejo de fechas y datos 
library(sf) # Librería para el manejo espacial de dataframes
library(stringr) # Librería para le manejo de strings
library(leafgl) # Librería que extiende la librería JavaScript leaflet para renderizar grandes conjuntos de datos
library(readr) # Librería para la lectura avanzada de ficheros
library(geosphere) # Librería que contiene la funcion distHaversine() para calcular la distancia entre dos coordenadas geográficas

# Función para leer los datos de tráfico de tiempo real
leer_trafico_tiempo_real <- function() {
  
  # Se leen los datos de tráfico a tiempo real que están en formato XML
  url <- "https://informo.madrid.es/informo/tmadrid/pm.xml"
  response <- httr::GET(url, timeout(10))
  httr::stop_for_status(response)
  
  xml <- read_xml(content(response, "text", encoding = "UTF-8"))
  
  # Se extrae la fecha y hora de los datos
  fecha_hora <- xml_text(xml_find_first(xml, "//fecha_hora"))
  dt <- dmy_hms(fecha_hora)
  
  # Se guarda el valor de las variables int_tiempo y fin_de_semana como int_tiempo_actual y fin_de_semana_actual
  numero_dia <- lubridate::wday(dt, week_start = 1) - 1 # 0=Lunes, 6=Domingo
  fin_de_semana_actual <- ifelse(numero_dia %in% c(5,6), 1, 0)
  hora <- hour(dt)
  
  int_tiempo_actual <- case_when(
    hora >= 6 & hora < 12 ~ 0,   # Mañana
    hora >= 12 & hora < 19 ~ 1,  # Mediodía
    hora >= 19 & hora < 24 ~ 2,  # Tarde
    TRUE ~ 3                      # Noche
  )
  
  # Se guarda un texto con el tramo horario para mostrar en la aplicación
  tiempo_texto <- c("Mañana (6-12h)", "Mediodía (12-18h)", "Tarde (19-24h)", "Noche (0-6h)")[int_tiempo_actual + 1]
  
  # Datos de tráfico
  pm_nodes <- xml_find_all(xml, "//pm")
  
  # Se extrae del xml los datos de ubicación, st_x, st_y y nivelServicio
  nivelServicio <- as.numeric(xml_text(xml_find_all(pm_nodes, "nivelServicio")))
  st_x <- str_replace(xml_text(xml_find_all(pm_nodes, "st_x")), ",", ".") %>% as.numeric()
  st_y <- str_replace(xml_text(xml_find_all(pm_nodes, "st_y")), ",", ".") %>% as.numeric()
  
  df_trafico_treal <- data.frame(nivelServicio, st_x, st_y) %>% filter(!is.na(nivelServicio) & !is.na(st_x) & !is.na(st_y))
  
  # Se extraen las coordenadas longitud y latitud de las coordenadas st_x y st_y 
  coordenadas <- st_transform(st_as_sf(df_trafico_treal, coords = c("st_x", "st_y"), crs = 32630), 4326)
  df_trafico_treal$latitud <- st_coordinates(coordenadas)[,2]
  df_trafico_treal$longitud <- st_coordinates(coordenadas)[,1]

  # La función devuelve una lista con:
    # Fecha y hora en la que se extraen los datos
    # Texto indicando el grupo temporal al que corresponde
    # Valor de la variable fin_de_semana
    # Valor de la variable int_tiempo
    # Dataframe con los datos de tráfico a tiempo real
  lista <- list(
    fecha_hora = fecha_hora,
    tiempo = tiempo_texto,
    fin_de_semana_actual = fin_de_semana_actual,
    int_tiempo_actual = int_tiempo_actual,
    df_trafico_treal = df_trafico_treal
  )
  return(lista)
}

# Esta función lee los datos resultantes de la aplicación de clustering por grupos
leer_datos_segmentados <- function(int_tiempo_actual, fin_de_semana_actual) {
  
  df_final <- read_csv("df_final.csv")
  colnames(df_final)
  # Se filtran los datos según int_tiempo_actual y fin_de_semana_actual
  df_final_actual <- df_final %>%
    filter(int_tiempo == int_tiempo_actual, fin_de_semana == fin_de_semana_actual)
  
  return(df_final_actual)
  
}

# Esta función relaciona los datos de estacionamiento segmentados con el tráfico a tiempo real por cercanía
asignar_trafico_cercano <- function(datos_aparcamiento, datos_trafico) {
  # Ambos subconjuntos que se pasan como parámetros a la función deben ser dataframes espaciales
  
  # Encontrar índice del vecino más cercano
  id_cercanos <- st_nearest_feature(datos_aparcamiento, datos_trafico)
  
  trafico_info <- datos_trafico[id_cercanos, ] %>%
    st_drop_geometry()  # quitamos geometría para evitar conflictos
  
  # Combinar
  datos_aparcamiento_trafico <- bind_cols(datos_aparcamiento, trafico_info)
  
  
  return(datos_aparcamiento_trafico)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {width:100%; height:100%; margin:0; padding:0;}
      #map {position:absolute; top:0; bottom:0; right:0; left:0;}
      .modal-body {
      font-size: 15px;
    }
    "))
  ),
  leafletOutput("map", height = "100%", width = "100%"),
  absolutePanel(
    h3("Sistema Inteligente de Recomendación de Aparcamiento", style = "font-weight: 700;"),
    top = 60, right = 20, width = 300, draggable = TRUE,
    style = "background: white; padding: 10px; border-radius: 8px;
             box-shadow: 0px 0px 10px rgba(0,0,0,0.3);
             font-family: 'Helvetica Neue', Helvetica;
             text-align: center;
             opacity: 0.85;
             font-weight: 200;",
    textOutput("fecha_hora"),
    actionButton(
      inputId = "boton_info",
      label = "ℹ️",
      style = "material-circle",
      color = "primary",
      size = "sm"
    ),
    h4("Coordenadas de la ubicación"),
    numericInput("lat_input", "Latitud", value = 40.4314164144709),
    numericInput("lon_input", "Longitud", value = -3.70653014533571),
    actionButton("random_location", "Generar ubicación aleatoria"),
    h4(""),
    sliderInput("radio", "Seleccionar radio de búsqueda (km)", min = 0.1, max = 5, value = 1, step = 0.1)
  )
)

server <- function(input, output, session) {
  
  # Output para mostrar la fecha y hora actual en la app  
  output$fecha_hora <- renderText({
    invalidateLater(1000, session)
    paste(format(Sys.time(), " %H:%M:%S %d/%m/%Y"))
  })
  
  # Reactive value para la ubicación actual
  loc <- reactiveVal(c(40.4314164144709, -3.70653014533571))  # Valor inicial de ubicación que muestra la app siempre al abrir
  
  # Reactive para datos dentro de un radio que se indica
  df_mapa <- reactive({
    invalidateLater(300000, session)  # Como los datos de tráfico se refrescan cada 5 minutos se hace que la app se refresque también cada 5 minutos
    ubicacion <- loc()  # loc()
    
    lat_selec <- ubicacion[1]
    lon_selec <- ubicacion[2]
    
    # Radio indicado en UI
    radio_km <- input$radio
    
    # Se cargan datos de aparcamiento, como parámetros se pasan el valor de int_tiempo y de fin_de_semana para filtrar 
    df_parking <- leer_datos_segmentados(leer_trafico_tiempo_real()$int_tiempo_actual, leer_trafico_tiempo_real()$fin_de_semana_actual)
    
    # Se transforma en un dataframe espacial, siguiendo el mismo proceso que en el código del anexo de la memoria
    geo_df_parking <- st_as_sf(df_parking, coords = c("lon", "lat"), crs = 4326)
    geo_df_parking <- st_transform(geo_df_parking, 25830)
    geo_df_parking <- geo_df_parking %>%
      rename(geometria_parking = geometry)
    
    # Se cargan los datos de tráfico de tiempo real
    df_trafico_treal <- leer_trafico_tiempo_real()$df_trafico_treal
    
    # Se transforma en un dataframe espacial, siguiendo el mismo proceso que en el código del anexo de la memoria
    geo_df_trafico <- st_as_sf(df_trafico_treal, coords = c("longitud", "latitud"), crs = 4326)
    geo_df_trafico <- st_transform(geo_df_trafico, 25830)
    geo_df_trafico <- geo_df_trafico %>%
      rename(geometria_trafico = geometry)
    
    # Se asigna a cada registro de aparcamiento el tráfico más cercano
    df_parking_trafico <- asignar_trafico_cercano(geo_df_parking,geo_df_trafico) 
    
    # Aquí en funcion del nivel de servicio crear variable llamada facilidad_aparcamiento_mod
    # que si nivelServicio es mayor que 2 automáticamente pase la zona a baja, ya que 2 indica retenciones y 3 congestión
    df_parking_trafico <- df_parking_trafico %>%
      mutate(facilidad_aparcamiento_mod = ifelse(nivelServicio >= 2, "baja", facilidad_aparcamiento))
    
    df_parking_trafico <- st_transform(df_parking_trafico, crs = 4326)
    
    # Se extrae lon/lat de la geometría actual
    df_parking_trafico <- df_parking_trafico %>%
      mutate(lon = st_coordinates(geometria_parking)[,1],
             lat = st_coordinates(geometria_parking)[,2])
    
    # Se calcula la distancia de la ubicación aleatoria a cada zona de aparcamiento 
    df_parking_trafico_dist <- df_parking_trafico %>%
      mutate(distancia_ubicacion = distHaversine(cbind(lon, lat),
                                                 c(lon_selec, lat_selec)) / 1000)
    
    # Se filtra el conjunto de datos por el radio que introduce el usuario para considerar solo esos registros
    df_parking_trafico_radio <- df_parking_trafico_dist %>%
      filter(distancia_ubicacion <= radio_km)
    
    return(df_parking_trafico_radio)
  })
  
  observeEvent(input$boton_info, {
    showModal(modalDialog(
      title = "Información de la aplicación",
      "Esta aplicación forma parte del Trabajo de Fin de Máster, realizado por Carlota López Quesada, del Máster en Big Data y Business Analytics de la Universidad Nacional de Educación a distancia.",
      br(), br(),
      "La aplicación, dada una ubicación, muestra las zonas de estacionamiento contenidas en un radio seleccionado categorizadas según la facilidad de encontrar aparcamiento en ellas. Las de color verde indican que la facilidad de aparcamiento es alta, las naranjas, facilidad media y las rojas, baja facilidad.
       Además la aplicación permite generar ubicaciones aleatorias para simular un conductor que busca aparcamiento y elegir un un radio de entre 1 y 5 kilómetros para la búsqueda de zonas de aparcamiento.",
      br(), br(),
      "La asignación de los distintos niveles de la facilidad de aparcamiento se lleva a cabo mediante técnicas de clustering realizadas en el trabajo que se complementan con información de tráfico a tiempo real en la aplicación para hacer una recomendación más adecuada.",
      "En particular, del tráfico a tiempo real se considera el nivel de servicio, una variable categórica la cual indica: tráfico fluido, tráfico lento, retenciones y congestión.",
      "Las zonas de estacionamiento con nivel de servicio 2 o 3 aparecen directamente en rojo.",
      "La aplicación se actualiza cada 5 minutos que es el periodo de actualización de los datos de tráfico a tiempo real.",
      br(),br(),
      "Al pulsar en una zona de aparcamiento se muestra su correspondiente distrito, barrio, calle, número de finca, color, tipo de aparcamiento (batería/línea) y nivel de servicio.",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })
  
  observeEvent(input$random_location, {
    lat <- runif(1, 40.410, 40.440)
    lon <- runif(1, -3.725, -3.670)
    loc(c(lat, lon))
    
    updateNumericInput(session, "lat_input", value = lat)
    updateNumericInput(session, "lon_input", value = lon)
  })
  
  output$map <- renderLeaflet({
    latlon <- loc()  
    df_mapa <- df_mapa()
    
    df_mapa <- df_mapa %>%
      mutate(facilidad_aparcamiento_mod = factor(
        facilidad_aparcamiento_mod,
        levels = c("alta", "media", "baja")  # orden deseado
      )) %>%
      mutate(color = case_when(
      color == 0 ~ "Verde",
      color == 1 ~ "Azul",
      color == 2 ~ "Rojo",
      color == 3 ~ "Alta Rotación"
    )) %>%
      mutate(bateria_linea = case_when(
        bateria_linea == 0 ~ "Batería",
        bateria_linea == 1 ~ "Línea"
      )) %>%
      mutate(nivelServicio = case_when(
        nivelServicio == 0 ~ "Trafico fluido",
        nivelServicio == 1 ~ "Tráfico lento",
        nivelServicio == 2 ~ "Retenciones",
        nivelServicio == 3 ~ "Congestión"
      ))

    
    pal <- colorFactor(
      palette = c("green3", "gold1", "brown1"), 
      domain = df_mapa$facilidad_aparcamiento_mod
    )
 
    leaflet(df_mapa) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lat = ~lat, 
        lng = ~lon, 
        radius = 5, 
        stroke = FALSE, 
        fillOpacity = 0.8,
        color = ~pal(facilidad_aparcamiento_mod),
        popup = ~paste0(
          "<b>Calle:</b> ", calle, "<br>",
          "<b>Número finca:</b> ", numero_finca, "<br>",
          "<b>Barrio:</b> ", barrio, "<br>",
          "<b>Distrito:</b> ", distrito, "<br>",
          "<b>Número de plazas:</b> ", numero_plazas, "<br>",
          "<b>Color de la zona de aparcamiento:</b> ", color, "<br>",
          "<b>Tipo de aparcamiento:</b> ", bateria_linea, "<br>",
          "<b>Nivel de servicio: </b>", nivelServicio
        )
      ) %>%
      addMarkers(
        lng = latlon[2], 
        lat = latlon[1], 
        popup = "Ubicación aleatoria",
        icon = icon("fa-solid fa-location-pin", class = NULL, lib = "font-awesome")
      ) %>%
      addLegend(
        "bottomleft", 
        pal = pal, 
        values = ~facilidad_aparcamiento_mod,
        title = "Facilidad de aparcamiento",
        opacity = 1
      )
  })
}

shinyApp(ui, server)
