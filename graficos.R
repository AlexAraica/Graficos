library(writexl)
library(plotly)
library(fmsb)
library(plotly)
library(ggplot2)
library(scales)
library(corrr)
library(dplyr)
library(zoo)
library(readxl)
library(mice)
library(dplyr)
library(tidyr)
library(plotly)


####### cARGAR DATOS

ciudad <- read_excel("limpios_ciudad.xlsx")
pais <- read_excel("limpios_pais.xlsx")
ciudad <- read_excel("limpios_ciudad.xlsx")
pais_porcentaje <- read_excel("porcentaje_pais.xlsx")
ciudad_porcentaje <- read_excel("porcentaje_ciudad.xlsx")

#####


pais7 <- ciudad %>%
  arrange(desc(porcentaje_emision)) %>% 
  slice(01:3) %>% 
  select(Pais, porcentaje_emision, porcentaje_GDP, 
         porcentaje_poblacion)


pais7 <- pais %>%
  arrange(desc(Emisiones)) %>% 
  slice(01:7) %>% 
  select(Pais, Emisiones, GPD, Pobreza, Volumen, Area, Poblacion)


##### NORMALIZAR
ciudades_normalizado <- ciudad %>%
  mutate(across(c(-Ciudad, -Pais, -Pais_id), ~ (.-min(.)) / (max(.) - min(.))))



colores <- c(
  "Argentina" = "#00A1D5",          # Rojo
  "Bahamas" = "#FFC300",            # Amarillo dorado
  "Barbados" = "#3E6C9A",           # Azul oscuro
  "Belize" = "#76D7C4",             # Verde claro
  "Bolivia" = "#C0392B",            # Rojo oscuro
  "Brazil" = "#20854E",             # Verde
  "Chile" = "#EE4C97",              # Azul
  "Colombia" = "#E18727",           # Morado
  "CostaRica" = "#F39C12",         # Naranja
  "Cuba" = "#800000",               # Rojo
  "Curacao" = "#1ABC9C",            # Turquesa
  "DominicanRepublic" = "#3498DB", # Azul
  "Ecuador" = "#F1C40F",            # Amarillo
  "ElSalvador" = "#2980B9",        # Azul
  "FrenchGuiana" = "#16A085",      # Verde
  "Guadeloupe" = "#3498DB",         # Azul
  "Guatemala" = "#1ABC9C",          # Verde
  "Guyana" = "#D35400",             # Naranja
  "Haiti" = "#8E44AD",              # Morado
  "Honduras" = "#1F618D",           # Azul oscuro
  "Jamaica" = "#27AE60",            # Verde
  "Martinique" = "#3498DB",         # Azul
    "Mexico" = "#7876B1",             # Amarillo
  "Nicaragua" = "#1ABC9C",          # Verde
  "Panama" = "#E67E22",             # Naranja
  "Paraguay" = "#E74C3C",           # Rojo
  "Peru" = "#B24745",               # Rojo oscuro
  "PuertoRico" = "#2980B9",        # Azul
  "Suriname" = "#D5DBDB",           # Gris claro
  "TrinidadandTobago" = "#C0392B",# Rojo
  "Uruguay" = "#5DADE2",            # Azul claro
  "Venezuela" = "#7E6148"           # Amarillo
)



####### Graficos

##### circulos

# Crear el gráfico con plotly
fig <- plot_ly(
  data = top30,
  x = ~Emisiones_percapita,
  y = ~Pobreza,
  text = ~Ciudad,  # Nombre de la ciudad en el hover
  size = ~Poblacion,  # Tamaño basado en la población
  color = ~Pais, 
  colors = ~colores,
  marker = list(sizemode = 'diameter', opacity = 0.7, line = list(width = 0)),
  type = 'scatter',
  mode = 'markers'
) %>%
  layout(
    xaxis = list(type = "log", title = "Emisiones de Carbono per cápita"),
    yaxis = list(title = "Pobreza"),
    title = "Relación entre Emisiones de Carbono, Pobreza y Población"
  )

fig



################ RADAR ########################

##### NORMALIZAR
pais7_normalizado <- pais7 %>%
  mutate(across(-Pais, ~ (.-min(.)) / (max(.) - min(.))))

# Mostrar el resultado
print(pais7_normalizado)


data_long <- tidyr::pivot_longer(pais7_normalizado, 
                                 -Pais, names_to = "Categoria", 
                                 values_to = "Valor")

# Crear el gráfico radar
radar <- plot_ly(data_long, 
                 r = ~Valor, 
                 theta = ~Categoria, 
                 color = ~Pais, 
                 colors = ~colores,
                 type = 'scatterpolar', 
                 fill = 'toself') %>%
  layout(title = 'Gráfico Radar',
         polar = list(
           radialaxis = list(
             visible = TRUE,
             range = c(-1.96, 1.96)
           )
         ))

# Mostrar el gráfico
radar


############ PASTEL


ciudad$clasificacion <- cut(
  ciudad$Emisiones,
  breaks = c(-Inf, 1000000, 5000000, Inf),
  labels = c("Bajas", "Medias","Altas"),
  right = FALSE  # Para incluir el límite inferior
)

# Calcular el total de emisiones por categoría

totales_por_categoria <- ciudad %>%
  group_by(clasificacion) %>%
  summarise(Emisiones = sum(Emisiones),
            frecuencia = n()) # Contar cuántas ciudades hay en cada categoría)


totales_por_categoria <- totales_por_categoria %>%
  mutate(
    total_emisiones_miles = Emisiones / 1000,
    total_emisiones_millones = Emisiones / 1000000,
    texto_emisiones = case_when(
      Emisiones >= 1000000 ~ paste0(round(total_emisiones_millones, 1), " millones"),
      Emisiones >= 1000 ~ paste0(round(total_emisiones_miles, 1), " mil"),
      TRUE ~ paste0(Emisiones, " toneladas")
    )
  )


pie <- plot_ly(totales_por_categoria, 
               labels = ~clasificacion, 
               values = ~total_emisiones_millones,
               type = 'pie',
               hole=0.3,
               textinfo = 'label+percent+text',
               text = ~paste("Ciudades: ", frecuencia),
               marker = list(colors = c("#C3C3C3", 
                                        "#636363", 
                                        "#1E1E1E"))) %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(text = "Emisiones<br>CO2", 
                x = 0.5, 
                y = 0.5, 
                font = list(size = 20, color = "black"), 
                showarrow = FALSE)
           ),
         font = list(family = "Arial", size = 14, color = "#666"),
         paper_bgcolor = "#f7f7f7",
         plot_bgcolor = "#ffffff")


# Mostrar el gráfico
pie




























