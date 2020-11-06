library(shiny)
library(dplyr)
library(ggplot2)

library(ggrepel)
library(ggiraph)
library(scales)

library(shinycssloaders)
library(formattable)

Sys.setlocale(category = "LC_TIME", locale = "es_ES.UTF-8") # Meses en español
options(scipen=9999)



# Población de las regiones (Censo 2017) ----

region <- c("Aysén", "Magallanes", "Arica y Parinacota", "Atacama", "Tarapacá", "Los Ríos", "Ñuble", "Antofagasta", "Coquimbo", "Araucanía", "O’Higgins", "Metropolitana", "Valparaíso", "Biobío", "Maule", "Los Lagos")
poblacion <- c(103158, 166533, 226068, 286168, 330558, 384837, 480609, 607534, 757586, 957224, 914555, 7112808, 1815902, 1556805, 1044950, 828708)
poblaciones <- data.frame(region, poblacion)


# Datos global ----
covid_totales <- readr::read_csv("http://localhost:8080/totales_nacionales_diarios") # 5

covid_region <- readr::read_csv("http://localhost:8080/casos_totales_region_incremental") %>% # 3
    mutate(region = forcats::fct_relevel(region, "Total", after = 0))

covid_comuna <- readr::read_csv("http://localhost:8080/casos_totales_comuna_incremental") # 1

covid_hospitalizados <- readr::read_csv("http://localhost:8080/pacientes_uci_region") #8

casos_genero_edad <- readr::read_csv("http://localhost:8080/casos_genero_grupo_edad") %>% # 16
    mutate_if(is.character, as.factor)

casos_totales_comuna <- readr::read_csv("http://localhost:8080/casos_totales_comuna") #2 

casos_activos_comuna <- readr::read_csv("http://localhost:8080/casos_activos_sintomas_comuna") #19

activos_comuna <- readr::read_csv("http://localhost:8080/casos_activos_sintomas_comuna") # 19

# Funciones para evitar repeticion ----------------------------------------

f_total <- function() {
    covid_region %>%
        filter(region == region_elegida()) %>%
        na.omit() %>%
        filter(fecha >= lubridate::ymd("2020-03-22"))
}



f_hospitalizados <- function() {
    covid_hospitalizados %>%
        filter(region != "Metropolitana") %>%
        mutate(region = recode(region,
                               "Tarapaca" = "Tarapacá",
                               "Arica y Parinacota" = "Arica",
                               "Nuble" = "Ñuble",
                               "Del Libertador General Bernardo O’Higgins" = "O'Higgins",
                               "Magallanes y la Antartica" = "Magallanes")) %>%
        group_by(region)
}

f_letalidad <- function() {
    covid_totales %>%
        filter(categoria == "Casos totales" | categoria == "Fallecidos") %>%
        tidyr::pivot_wider(id_cols = fecha, names_from = categoria, values_from = casos) %>%
        rename(Activos = 2) %>%
        mutate(Tasa = Fallecidos / Activos) %>%
        filter(Fallecidos != 0)
}

f_totales_nacionales <- function() {
    covid_totales %>%
        na.omit() %>%
        mutate(categoria = stringr::str_remove(categoria, "Casos ")) %>%
        group_by(categoria) %>%
        mutate(final = casos[fecha == max(fecha)])
}


f_casos_genero_edad <- function() {
    casos_genero_edad %>%
    #Recodificación sugerida por Gregorio
    mutate(grupo_de_edad = recode(grupo_de_edad,
                                  "00 - 04 años" = "0 - 19 años",
                                  "05 - 09 años" = "0 - 19 años",
                                  "10 - 14 años" = "0 - 19 años", 
                                  "15 - 19 años" = "0 - 19 años", 
                                  "20 - 24 años" = "20 - 39 años", #
                                  "25 - 29 años" = "20 - 39 años",  
                                  "30 - 34 años" = "20 - 39 años",
                                  "35 - 39 años" = "20 - 39 años",
                                  "40 - 44 años" = "40 - 59 años", #
                                  "45 - 49 años" = "40 - 59 años",
                                  "50 - 54 años" = "40 - 59 años",
                                  "55 - 59 años" = "40 - 59 años", #
                                  "60 - 64 años" = "60 años y más",
                                  "65 - 69 años" = "60 años y más",
                                  "70 - 74 años" = "60 años y más",
                                  "75 - 79 años" = "60 años y más", 
                                  "80 y más años" = "60 años y más")) %>%
        mutate(sexo = recode(sexo,
                             "M" = "Hombres",
                             "F" = "Mujeres")) %>%
        mutate(grupo_de_edad = stringr::str_replace(grupo_de_edad, " - ", "-"),
            grupo_de_edad = stringr::str_replace(grupo_de_edad, " y más", " +")) %>%
        group_by(fecha, sexo, grupo_de_edad) %>%
        summarize(casos = sum(casos)) %>%
        na.omit() %>%
        group_by(fecha, grupo_de_edad) %>%
        mutate(final = casos[fecha == max(fecha)]) %>%
        ungroup() %>%
        mutate(grupo_de_edad = forcats::fct_reorder(grupo_de_edad, final))
}




# — ----




# Temas ----
tema_lineas <- theme(
    axis.text.y = element_text(size = 13, margin = margin(l = 5, r = 10)),
    axis.text.x = element_text(size = 13, margin = margin(t = 5)),
    axis.title.y = element_text(size = 15, margin = margin(r = 10)),
    axis.ticks = element_blank(), panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"), # linea gris
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
    legend.key.size = unit(1.7, "lines"),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 13, margin = margin(r = 10)),
    # Faceta
    strip.background = element_blank(),
    panel.spacing.x = unit(0.5, "cm"),
    panel.spacing.y = unit(0.8, "cm"),
    strip.text = element_text(family = "Open Sans", face = "bold.italic", size = 15, hjust = 0, margin = margin(t = 0, b = 5, l = 0)),
    text = element_text(family = "Open Sans"),
    plot.title = element_text(size = 18, family = "Open Sans", color = "#891036", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, family = "Open Sans", color = "#891036", margin = margin(b = 15)),
    plot.caption = element_text(size = 12, family = "Open Sans"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
)


tema_barras_label <- theme(
    axis.title.y = element_text(size = 15, angle = 90, margin = margin(r = 5)),
    axis.text.y = element_text(size = 13, margin = margin(r = 5)),
    axis.title.x = element_text(size = 15, margin = margin(t = 5, b = 5)),
    axis.ticks = element_blank(), panel.background = element_blank(),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
    legend.text = element_text(size = 13, margin = margin(r = 20)),
    legend.key.size = unit(1.6, "lines"),
    text = element_text(family = "Open Sans"),
    plot.title.position = "plot",
    plot.caption = element_text(size = 12, family = "Open Sans"),
    plot.subtitle = element_text(size = 18, family = "Open Sans", color = "#891036", margin = margin(b = 15))
)



tema_barras_horizontales_3 <- theme(
    strip.background = element_blank(),
    panel.spacing.y = unit(0.5, "cm"),
    strip.text = element_text(family = "Open Sans Semibold", size = 10, hjust = 0.5, margin = margin(t = 0, b = 2, l = 0)),
    axis.title.y = element_blank(),
    # axis.title.x = element_text(size=15, margin=margin(t=10, b=-5)),
    axis.title.x = element_text(size = 15, margin = margin(t = 10, b = 2)),
    # axis.text.y = element_text(size=13, margin=margin(r=4)),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13, margin = margin(l = 10, r = -20), vjust = -0.2),
    axis.ticks = element_blank(), panel.background = element_blank(),
    panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
    legend.key.size = unit(1.5, "lines"),
    # legend.position = "bottom",
    legend.position = c(.77, .3),
    legend.direction = "vertical",
    legend.text = element_text(size = 13, margin = margin(r = 10)),
    text = element_text(family = "Open Sans"),
    plot.title.position = "plot",
    plot.caption = element_text(size = 12, family = "Open Sans", margin = margin(t = 10)),
    plot.subtitle = element_text(size = 18, family = "Open Sans", color = "#891036", margin = margin(b = 12))
)



escala_porcentaje <- list(scale_y_continuous(labels = percent_format(accuracy = 1)))

escala_miles <- list(scale_y_continuous(labels = function(x) format(x, big.mark = ".")))

escala_plata <- list(scale_y_continuous(labels = function(x) paste0("$", format(x, big.mark = "."))))

leyenda_abajo <- list(theme(legend.position = "bottom"))


linea_gris_x <- list(theme(panel.grid.major.x = element_line(color = "gray90", linetype = "solid")))
linea_gris_x_dashed <- list(theme(panel.grid.major.x = element_line(color = "gray90", linetype = "dashed")))
linea_gris_y <- list(theme(panel.grid.major.y = element_line(color = "gray90", linetype = "solid")))
linea_gris_y_dashed <- list(theme(panel.grid.major.y = element_line(color = "gray90", linetype = "dashed")))

ocultar_titulo_x <- list(theme(axis.title.x = element_blank()))

ocultar_titulo_y <- list(theme(axis.title.y = element_blank()))

ocultar_leyenda <- list(theme(legend.position = "none"))

ocultar_título_leyenda <- list(theme(legend.title = element_blank()))

# Colores ----

# Degradado rojo, morado y azul
degradado1 <- colorRampPalette(c("#DF1A57", "#AF87EB", "#7033cc"))

# Degradado rojo, morado y azul inverso
degradado1_inverso <- colorRampPalette(c("#7e47d1", "#AF87EB", "#DF1A57"))

# Degradado con celeste
degradado2 <- colorRampPalette(c("#DF1A57", "#B077E5", "#7e47d1", "#82BAEB"))

# Degradado inverso
degradado2_inverso <- colorRampPalette(c("#82BAEB", "#7e47d1", "#B077E5", "#DF1A57"))

# Degradado celestes
degradado3 <- colorRampPalette(c("#82BAEB", "#4961D2"))

# Degradado rojo-azul-rojo
degradado4 <- colorRampPalette(c("#DF1A57", "#5933cc", "#DF1A57"))

# Degradado cualitativo
degradado5 <- colorRampPalette(c("#935EDF", "#EF6DB9", "#235AA1", "#82BAEB"))

degradado5_b <- colorRampPalette(c("#935EDF", "#EF6DB9", "#82BAEB", "#235AA1"))

degradado6 <- colorRampPalette(c("#2E54A8", "#82BAEB", "#895EDF", "#CC6ADF"))

degradado6_fuerte <- colorRampPalette(c("#7e47d1", "#5EA9EB", "#703ADF", "#CC58E1"))

# Degradado rojo-azul
degradado7 <- colorRampPalette(c("#DF1A57", "#7033cc"))

degradado7_2 <- colorRampPalette(c("#DF1A57", "#5fa6ec", "#a925d4"))

degradado7_3 <- colorRampPalette(c("#DF1A57", "#c2389f", "#5933cc", "#e481b3", "#5fa6ec"))
# activos.    nuevos.    recuperados totales. fallecidos
# e06c90 casos totales

# 7033cc recuperados

# a925d4 nuevos

# Degradado rojo-azul inverso
degradado7_inverso <- colorRampPalette(c("#7e47d1", "#DF1A57"))

# — ----








