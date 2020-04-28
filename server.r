shinyServer(function(input, output, session) {
  
  
  Sys.setlocale(category = "LC_TIME", locale = "es_ES.UTF-8") # Meses en español
  options(shiny.sanitize.errors = TRUE)
  
  
  # Datos por región ----
  covid_region <- reactive({
    readr::read_csv("http://localhost:8080/casos_totales_region_incremental") %>% # 3
      mutate(
        #fecha = lubridate::ymd(fecha),
        region = forcats::fct_relevel(region, "Total", after = 0)
      )
  })
  
  # Datos por comuna ----
  
  covid_comuna <- reactive({
    req(covid_region())
    readr::read_csv("http://localhost:8080/casos_totales_comuna_incremental") #%>% # 1
      # mutate(
      #   fecha = lubridate::ymd(fecha)
      # )
  })
  
  # Casos totales por comuna ----
  
  casos_totales_comuna <- reactive({
    readr::read_csv("http://localhost:8080/casos_totales_comuna") #2 
  })
  
  # Casos nuevos por fecha de inicio de síntomas por comuna ----
  nuevos_comuna <- reactive({
    nuevos_comuna <-
      readr::read_csv("http://localhost:8080/casos_nuevos_sintomas_comuna") # 15
  })
  
  # Casos activos por fecha de inicio de síntomas y comuna ----
  activos_comuna <- reactive({
    covid_act_comuna <-
      readr::read_csv("http://localhost:8080/casos_activos_sintomas_comuna") # 19
  })
  
  
  # Región casos nuevos ----
  
  covid_region_nuevos <- reactive({
    #req(covid_region())
    readr::read_csv("http://localhost:8080/casos_nuevos_region_incremental") #%>% # 13
      # mutate(
      #   fecha = lubridate::ymd(fecha)
      # )
  })
  
  # Hospitalizados UCI ----
  
  covid_hospitalizados <- reactive({
    readr::read_csv("http://localhost:8080/pacientes_uci_region") #%>% # 8
      # mutate(
      #   fecha = lubridate::ymd(fecha)
      # )
  })
  
  # Pacientes hospitalizados según cama ----
  # Hospitalización de pacientes en sistema integrado
  hosp_integrado <- reactive({
    hosp_integrado <-
      readr::read_csv("http://localhost:8080/hospitalizacion_sistema_integrado") # 24
  })
  
  # Fallecidos total ----
  
  covid_fallecidos <- reactive({
    readr::read_csv("http://localhost:8080/fallecidos_grupo_edad") #%>% # 10
      # mutate(
      #   fecha = lubridate::ymd(fecha)
      # )
  })
  
  # Fallecidos por región ----
  
  covid_fallecidos_region <- reactive({
    readr::read_csv("http://localhost:8080/fallecidos_region_incremental") #%>% # 14
      # mutate(
      #   fecha = lubridate::ymd(fecha)
      # )
  })
  
  # Fallecidos por región incremental ----
  fallecidos_region <- reactive({
    fallecidos_region <-
      readr::read_csv("http://localhost:8080/fallecidos_region_incremental") # 14
  })
  
  # Exámenes PCR ----
  
  covid_examenes <- reactive({
    readr::read_csv("http://localhost:8080/examenes_pcr_region") #%>% # 7
      # mutate(
      #   fecha = lubridate::ymd(fecha)
      # )
  })
  
  # Datos pacientes criticos ----
  pacientes_criticos <- reactive({
    pacientes_criticos <-
      readr::read_csv("http://localhost:8080/pacientes_criticos") %>% # 23
      mutate(categoria = as.factor(categoria))
  })
  
  
  # Datos Hospitalizados totales por grupo de edad y género ----
  hosp_edad_total <- reactive({
    hosp_edad <-
      readr::read_csv("http://localhost:8080/hospitalizados_grupo_edad") %>% # 22
      filter(categoria != "Hospitalizados UCI") %>%
      mutate(
        grupo_de_edad = as.factor(grupo_de_edad),
        categoria = as.factor(categoria)
      )
  })
  
  # Datos Hospitalizados UCI por grupo de edad ----
  hosp_edad_uci <- reactive({
    hosp_edad_uci <-
      readr::read_csv("http://localhost:8080/hospitalizados_grupo_edad") %>% # 22
      filter(categoria == "Hospitalizados UCI")
  })
  
  # Datos Pacientes en UCI por grupo de edad ----
  uci_edad <- reactive({
    uci_edad <-
      readr::read_csv("http://localhost:8080/pacientes_uci_grupo_edad") # 9
  })
  
  # Datos Casos por genero y grupo de edad ----
  casos_genero_edad <- reactive({
    casos_genero_edad <-
      readr::read_csv("http://localhost:8080/casos_genero_grupo_edad") %>% # 16
      mutate_if(is.character, as.factor)
  })
  
  # Datos ventiladores mecánicos a nivel nacional ----
  ventiladores <- reactive({
    readr::read_csv("http://localhost:8080/ventiladores_nacional") # 20
  })
  
  # Totales nacionales ----
  
  covid_totales <- reactive({
    readr::read_csv("http://localhost:8080/totales_nacionales_diarios") #%>% # 5
      # mutate(
      #   fecha = lubridate::ymd(fecha)
      # )
  })
  
  
  #Casos activos por comuna ----
  casos_activos_comuna <- reactive({
    readr::read_csv("http://localhost:8080/casos_activos_sintomas_comuna") #19
  })
  
  
  # Población de las regiones (Censo 2017) ----
  
  region <- c("Aysén", "Magallanes", "Arica y Parinacota", "Atacama", "Tarapacá", "Los Ríos", "Ñuble", "Antofagasta", "Coquimbo", "Araucanía", "O’Higgins", "Metropolitana", "Valparaíso", "Biobío", "Maule", "Los Lagos")
  poblacion <- c(103158, 166533, 226068, 286168, 330558, 384837, 480609, 607534, 757586, 957224, 914555, 7112808, 1815902, 1556805, 1044950, 828708)
  poblaciones <- data.frame(region, poblacion)
  
  #— ----
  # Funciones para evitar repeticion ----------------------------------------
  
  f_total <- function() {
    covid_region() %>%
      filter(region == region_elegida()) %>%
      na.omit() %>%
      filter(fecha >= lubridate::ymd("2020-03-22"))
  }
  
  
  
  f_hospitalizados <- function() {
    covid_hospitalizados() %>%
      # mutate(region2 = case_when(region == region_elegida() ~ "Sí",
      #                            TRUE ~ "No")) %>%
      filter(region != "Metropolitana") %>%
      mutate(region = recode(region,
                             "Tarapaca" = "Tarapacá",
                             "Arica y Parinacota" = "Arica",
                             "Nuble" = "Ñuble",
                             "Del Libertador General Bernardo O’Higgins" = "O'Higgins",
                             "Magallanes y la Antartica" = "Magallanes"
      )) %>%
      group_by(region)
  }
  
  f_letalidad <- function() {
    covid_totales() %>%
      filter(categoria == "Casos activos" | categoria == "Fallecidos") %>%
      tidyr::pivot_wider(id_cols = fecha, names_from = categoria, values_from = casos) %>%
      rename(Activos = 3) %>%
      mutate(Tasa = Fallecidos / Activos)
  }
  
  f_totales_nacionales <- function() {
    covid_totales() %>%
      na.omit() %>%
      mutate(categoria = stringr::str_replace(categoria, "Casos ", "")) %>%
      group_by(categoria) %>%
      mutate(final = casos[fecha == max(fecha)])
  }
  
  f_casos_genero_edad <- function() {
    casos_genero_edad() %>%
      mutate(grupo_de_edad = recode(grupo_de_edad,
                                    "00 - 04 años" = "00 - 14 años",
                                    "05 - 09 años" = "00 - 14 años",
                                    "10 - 14 años" = "00 - 14 años", #
                                    "15 - 19 años" = "15 - 29 años",
                                    "20 - 24 años" = "15 - 29 años",
                                    "25 - 29 años" = "15 - 29 años", #
                                    "30 - 34 años" = "30 - 44 años",
                                    "35 - 39 años" = "30 - 44 años",
                                    "40 - 44 años" = "30 - 44 años", #
                                    "45 - 49 años" = "45 - 59 años",
                                    "50 - 54 años" = "45 - 59 años",
                                    "55 - 59 años" = "45 - 59 años", #
                                    "60 - 64 años" = "60 - 79 años",
                                    "65 - 69 años" = "60 - 79 años",
                                    "70 - 74 años" = "60 - 79 años",
                                    "75 - 79 años" = "60 - 79 años", #
                                    "80 y más años" = "80 y más años"
      )) %>%
      mutate(sexo = recode(sexo,
                           "M" = "Hombres",
                           "F" = "Mujeres"
      )) %>%
      mutate(
        grupo_de_edad = stringr::str_replace(grupo_de_edad, " - ", "-"),
        grupo_de_edad = stringr::str_replace(grupo_de_edad, " y más", "+"),
        grupo_de_edad = stringr::str_replace(grupo_de_edad, "00", "0")
      ) %>%
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
  
  # Región elegida ----
  
  region_elegida <- reactive({
    req(input$selector_region)
    
    input$selector_region
  })
  
  dimension_horizontal <- reactive({
    input$dimension[1]
  })
  
  
  output$region_elegida_explicacion <- renderText({
    if (region_elegida() == "Total") {
      paste("Ninguna región seleccionada. Mostrando casos totales de Chile")
    } else {
      paste(region_elegida())
    }
  })
  
  # Output de datos ----
  
  # fecha más nueva de base regional
  output$fecha_maxima_region <- renderText({
    paste(max(covid_region()$fecha))
  })
  
  # fecha en formato "%%día de %%mes"
  output$fecha_maxima_region_format <- renderText({
    paste(format(max(covid_region()$fecha), "%d de %B"))
  })
  
  # Región elegida (input selector)
  output$region_elegida <- renderText({
    paste(region_elegida())
  })
  
  # Lo mismo pero entre paréntesis
  output$region_elegida_p <- renderText({
    paste0("(", region_elegida(), ")")
  })
  
  # casos RM
  casos_rm_ultimo <- reactive({
    covid_region() %>%
      filter(region == "Metropolitana") %>%
      filter(fecha == max(fecha)) %>%
      summarize(casos = casos)
  })
  output$casos_rm <- renderText({
    format(paste(casos_rm_ultimo()), big.mark = ".")
  })
  
  # casos para la región elegida
  casos_region_ultimo <- reactive({
    covid_region() %>%
      filter(region == region_elegida()) %>%
      filter(fecha == max(fecha)) %>%
      summarize(casos = casos)
  })
  output$casos_region <- renderText({
    format(paste(casos_region_ultimo()), big.mark = ".")
  })
  
  # casos en total
  casos_total_ultimo <- reactive({
    covid_region() %>%
      filter(region == "Total") %>%
      filter(fecha == max(fecha)) %>%
      summarize(casos = casos)
  })
  output$casos_total <- renderText({
    paste(casos_total_ultimo())
  })
  
  # casos fallecidos
  casos_fallecidos <- reactive({
    covid_fallecidos() %>%
      filter(fecha == max(fecha)) %>%
      group_by(fecha) %>%
      summarize(casos = sum(casos)) %>%
      select(casos)
  })
  output$casos_fallecidos <- renderText({
    paste(casos_fallecidos())
  })
  
  # — ----
  
  # Top valores para sidebar ----
  # Cantidad total de exámenes PCR realizados
  # desde el 9 de abril
  total_examenes <- reactive({
    covid_examenes() %>%
      na.omit() %>%
      summarize(casos = sum(casos))
  })
  output$total_examenes <- renderText({
    paste(total_examenes()$casos, "exámenes desde el 9 de abril")
  })
  
  
  # Total de hospitalizados UCI
  total_hospitalizados <- reactive({
    covid_hospitalizados() %>%
      filter(region != "Total") %>%
      filter(fecha == max(fecha)) %>%
      na.omit() %>%
      summarize(casos = sum(casos))
  })
  output$total_hospitalizados <- renderText({
    paste(total_hospitalizados()$casos, "personas")
  })
  
  
  # region con mas casos
  casos_top_region <- reactive({
    covid_region() %>%
      filter(region != "Total") %>%
      filter(fecha == max(fecha)) %>%
      mutate(Rank = dense_rank(desc(casos))) %>%
      select(region, casos, Rank) %>%
      arrange(Rank) %>%
      filter(Rank <= 1)
  })
  output$casos_top_region <- renderText({
    paste0(
      as.character(casos_top_region()$region), ", con ",
      as.numeric(casos_top_region()$casos), " casos"
    )
  })
  
  
  # region con mas casos exceptuando metropolitana
  casos_top_region_no_rm <- reactive({
    covid_region() %>%
      filter(
        region != "Total",
        region != "Metropolitana"
      ) %>%
      filter(fecha == max(fecha)) %>%
      mutate(Rank = dense_rank(desc(casos))) %>%
      select(region, casos, Rank) %>%
      arrange(Rank) %>%
      filter(Rank <= 1)
  })
  output$casos_top_region_no_rm <- renderText({
    paste0(
      as.character(casos_top_region_no_rm()$region), ", con ",
      as.numeric(casos_top_region_no_rm()$casos), " casos"
    )
  })
  
  
  # region con menos casos
  casos_min_region <- reactive({
    covid_region() %>%
      filter(
        region != "Total",
        region != "Metropolitana"
      ) %>%
      filter(fecha == max(fecha)) %>%
      mutate(Rank = dense_rank((casos))) %>%
      select(region, casos, Rank) %>%
      arrange(Rank) %>%
      filter(Rank <= 1)
  })
  output$casos_min_region <- renderText({
    paste0(
      as.character(casos_min_region()$region), ", con ",
      as.numeric(casos_min_region()$casos), " casos"
    )
  })
  
  
  # comuna con mas casos
  o_casos_top_comuna <- reactive({
    covid_comuna() %>%
      filter(fecha == max(fecha)) %>%
      mutate(Rank = dense_rank(desc(casos))) %>%
      select(region, comuna, casos, Rank) %>%
      arrange(Rank) %>%
      filter(Rank <= 1)
  })
  output$o_casos_top_comuna <- renderText({
    paste0(
      as.character(o_casos_top_comuna()$comuna), ", en ",
      as.character(o_casos_top_comuna()$region), ", con ",
      as.numeric(o_casos_top_comuna()$casos), " casos"
    )
  })
  
  
  
  # —----
  
  # TABLAS ----
  
  
  # Top 15 regiones ----
  casos_top_10_region <- reactive({
    req(covid_region())
    
    covid_region() %>%
      filter(region != "Total") %>%
      filter(fecha == max(fecha)) %>%
      mutate(Rank = dense_rank(desc(casos))) %>%
      left_join(poblaciones) %>%
      mutate(Tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
      select(Rank, region, casos, Tasa) %>%
      arrange(Rank) %>%
      # filter(Rank<=10) %>%
      # select(-Rank) %>%
      head(15) %>%
      rename(
        Región = region,
        Casos = casos
      ) %>%
      rename(Puesto = Rank) %>%
      # kable(escape = F) %>%
      # kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
      formattable(
        align = c("l", "l", "c", "c"),
        list(
          Región = formatter("span", style = ~ style(font.style = "bold")),
          area(col = "Casos") ~ color_tile("#fce8ef", "#f3a5c0"),
          area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
        )
      )
  })
  
  output$t_casos_top_10_region <- renderFormattable({
    casos_top_10_region()
  })
  
  #Descarga
  output$regiones_ranking_xlsx <- downloadHandler(
    filename = "regiones_ranking.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_region() %>%
                            filter(region != "Total") %>%
                            filter(fecha == max(fecha)) %>%
                            mutate(Rank = dense_rank(desc(casos))) %>%
                            left_join(poblaciones) %>%
                            mutate(Tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
                            select(Rank, region, casos, Tasa) %>%
                            arrange(Rank) %>%
                            head(15) %>%
                            rename(
                              Región = region,
                              Casos = casos
                            ) %>%
                            rename(Puesto = Rank), filename)
    },
    contentType = "application/xlsx"
  )
  # output$casos_top_15_region <- renderTable("casos_top_15_region")
  
  
  # top 10 regiones con menos casos
  # casos_top_10_min_region <- reactive({
  #     covid_region() %>%
  #         filter(region!="Total") %>%
  #         filter(fecha==max(fecha)) %>%
  #         mutate(Rank = dense_rank((casos))) %>%
  #         left_join(poblaciones) %>%
  #         mutate(Tasa = round((casos/poblacion)*100000, digits=1)) %>%
  #         select(Rank, region, casos, Tasa) %>%
  #         arrange(Rank) %>%
  #         #filter(Rank<=10) %>%
  #         rename(Región=region) %>%
  #         rename(Puesto=Rank) %>%
  #         formattable(align = c("l", "l", "c", "c"),
  #                     list(Región = formatter("span", style = ~ style(font.style = "bold")),
  #                          area(col = "casos", row = 1:16) ~ color_tile("#fce8ef", "#f3a5c0"),
  #                          area(col = "Tasa", row = 1:16) ~ color_tile("#f4e4f4", "#c8b1de")))
  # })
  # output$t_casos_top_10_min_region <- renderFormattable({ casos_top_10_min_region() })
  
  
  # Top 15 comunas casos ----
  casos_top_comuna <- reactive({
    covid_comuna() %>%
      filter(fecha == max(fecha)) %>%
      mutate(Rank = dense_rank(desc(casos))) %>%
      mutate(Tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
      select(Rank, comuna, region, casos, Tasa) %>%
      arrange(Rank) %>%
      filter(Rank <= 20) %>%
      head(15) %>%
      rename(Región = region) %>%
      rename(
        Puesto = Rank,
        Casos = casos,
        Comuna = comuna
      ) %>%
      # kable(escape = F) %>%
      # kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
      formattable(
        align = c("l", "l", "l", "c", "c"),
        list(
          comuna = formatter("span", style = ~ style(font.style = "bold")),
          Región = formatter("span", style = ~ style(font.style = "bold")),
          area(col = "Casos") ~ color_tile("#fce8ef", "#f3a5c0"),
          area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
        )
      )
  })
  
  output$t_casos_top_comuna <- renderFormattable({
    casos_top_comuna()
  })
  
  #Descarga
  output$comunas_ranking_xlsx <- downloadHandler(
    filename = "comunas_ranking.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_comuna() %>%
                            filter(fecha == max(fecha)) %>%
                            mutate(Rank = dense_rank(desc(casos))) %>%
                            mutate(Tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
                            select(Rank, comuna, region, casos, Tasa) %>%
                            arrange(Rank) %>%
                            filter(Rank <= 20) %>%
                            head(15) %>%
                            rename(Región = region) %>%
                            rename(
                              Puesto = Rank,
                              Casos = casos,
                              Comuna = comuna
                            ), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  
  
  #Nuevos comuna ----
  tabla_nuevos_comuna <- reactive({
    
    nuevos_comuna() %>%
      select(semana_epidemiologica, inicio_semana_epidemiologica, fin_semana_epidemiologica,
             region, comuna, poblacion, casos)%>%
      rename(semana=1,
             inicio=2,
             fin=3) %>%
      mutate(etiqueta = paste0(
        format(inicio, "%d/%b"),
        "-",
        format(fin, "%d/%b")
      )) %>%
      filter(fin!=max(fin)) %>%
      filter(fin==max(fin)) %>%
      mutate(tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
      mutate(rank = dense_rank(desc(casos))) %>%
      filter(rank < 20) %>%
      arrange(rank) %>%
      head(15) %>%
      select(rank, comuna, region, casos, tasa) %>%
      rename(Puesto=1,
             Comuna=2,
             Región=3,
             "Casos nuevos"=4,
             "Tasa"=5) %>%
      formattable(
        align = c("l", "l", "l", "c", "c"),
        list(
          Comuna = formatter("span", style = ~ style(font.style = "bold")),
          area(col = "Casos nuevos") ~ color_tile("#fce8ef", "#f3a5c0"),
          area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
        )
      )
  })
  
  output$tabla_nuevos_comuna <- renderFormattable({
    tabla_nuevos_comuna()
  })
  
  #Descarga
  output$comunas_nuevos_ranking_xlsx <- downloadHandler(
    filename = "comunas_nuevos_ranking.xlsx",
    content = function(filename) {
      writexl::write_xlsx(nuevos_comuna() %>%
                            select(semana_epidemiologica, inicio_semana_epidemiologica, fin_semana_epidemiologica,
                                   region, comuna, poblacion, casos)%>%
                            rename(semana=1,
                                   inicio=2,
                                   fin=3) %>%
                            mutate(etiqueta = paste0(
                              format(inicio, "%d/%b"),
                              "-",
                              format(fin, "%d/%b")
                            )) %>%
                            filter(fin!=max(fin)) %>%
                            filter(fin==max(fin)) %>%
                            mutate(tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
                            mutate(rank = dense_rank(desc(casos))) %>%
                            filter(rank < 20) %>%
                            arrange(rank) %>%
                            head(15) %>%
                            select(rank, comuna, region, casos, tasa) %>%
                            rename(Puesto=1,
                                   Comuna=2,
                                   Región=3,
                                   "Casos nuevos"=4,
                                   "Tasa"=5), filename)
    },
    contentType = "application/xlsx"
  )
  
  ## Comunas aumento ----
  
  # selector región
  observe({
    updateSelectInput(session, "selector_tabla_comunas_aumento",
                      choices = levels(as.factor(casos_totales_comuna()$region)),
                      selected = "Metropolitana"
    )
  })
  
  # resultado de selector región
  tabla_comunas_aumento_elegida <- reactive({
    input$selector_tabla_comunas_aumento
  })
  
  #Texto de las semanas contempladas en el cálculo del aumento
  casos_totales_comuna_informes <- reactive({
    
    Sys.setlocale(category = "LC_TIME", locale = "es_ES.UTF-8") # Meses en español
    
    fecha_maxima <- max(casos_totales_comuna()$fecha)
    
    fecha_anterior <- casos_totales_comuna() %>% filter(fecha!=max(fecha))
    
    fecha_anterior2 <- max(fecha_anterior$fecha)
    
    fechas <- paste("Aumentos entre el",
          format(lubridate::ymd(fecha_anterior2), "%d de %B"),
          "y el",
          format(lubridate::ymd(fecha_maxima), "%d de %B")
    )
    
    fechas
  })
  
  output$casos_totales_comuna_informes <- renderText({
    paste(casos_totales_comuna_informes())   })
  
  
  
  #selector_tabla_comunas_aumento
  
  tabla_comunas_aumento <- reactive({
    casos_totales_comuna() %>%
      select(fecha, region, comuna, poblacion, casos_confirmados) %>%
      rename(casos=casos_confirmados) %>%
      filter(region==tabla_comunas_aumento_elegida()) %>%
      group_by(comuna) %>%
      arrange(fecha) %>%
      group_by(comuna) %>%
      mutate(lag = lag(casos)) %>%
      #mutate(aumento = casos/lag) %>%
      #mutate(aumento = (casos-lag)/lag) %>%
      mutate(aumento = round((casos-lag)/lag, digits=1)) %>%
      filter(fecha==max(fecha)) %>%
      ungroup() %>%
      filter(aumento!=Inf) %>%
      mutate(tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
      mutate(rank = dense_rank(desc(aumento))) %>%
      filter(rank < 20) %>%
      arrange(rank, desc(tasa)) %>%
      head(15) %>%
      select(rank, comuna, region, casos, aumento, tasa) %>%
      rename(Puesto=1,
             Comuna=2,
             Región=3,
             Casos=4,
             Aumento=5,
             Tasa=6) %>%
      formattable(
        align = c("l", "l", "l", "c", "c", "c"),
        list(
          # Aumento = formatter("span", 
          #                     x ~ percent(x, digits=1)),
          Comuna = formatter("span", style = ~ style(font.style = "bold")),
          area(col = "Casos") ~ color_tile("#fce8ef", "#f3a5c0"),
          area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de"),
          #area(col = "Aumento") ~ function(x) percent(x, digits = 0),
          area(col = "Aumento") ~ color_tile("#eaf2fa", "#b0bee8")
        )
      )
  })
  
  output$tabla_comunas_aumento <- renderFormattable({
    tabla_comunas_aumento()
  })
  
  #Descarga
  output$comunas_aumento_ranking_xlsx <- downloadHandler(
    filename = "comunas_aumento_ranking.xlsx",
    content = function(filename) {
      writexl::write_xlsx(casos_totales_comuna() %>%
                            select(fecha, region, comuna, poblacion, casos_confirmados) %>%
                            rename(casos=casos_confirmados) %>%
                            filter(region==tabla_comunas_aumento_elegida()) %>%
                            group_by(comuna) %>%
                            arrange(fecha) %>%
                            group_by(comuna) %>%
                            mutate(lag = lag(casos)) %>%
                            #mutate(aumento = casos/lag) %>%
                            #mutate(aumento = (casos-lag)/lag) %>%
                            mutate(aumento = round((casos-lag)/lag, digits=1)) %>%
                            filter(fecha==max(fecha)) %>%
                            ungroup() %>%
                            filter(aumento!=Inf) %>%
                            mutate(tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
                            mutate(rank = dense_rank(desc(aumento))) %>%
                            filter(rank < 20) %>%
                            arrange(rank, desc(tasa)) %>%
                            head(15) %>%
                            select(rank, comuna, region, casos, aumento, tasa) %>%
                            rename(Puesto=1,
                                   Comuna=2,
                                   Región=3,
                                   Casos=4,
                                   Aumento=5,
                                   Tasa=6), filename)
    },
    contentType = "application/xlsx"
  )
  
  ## Casos activos por comuna----
  
  tabla_casos_activos_comuna <- reactive({
    
    casos_activos_comuna() %>% 
      select(fecha, region, comuna, casos, poblacion) %>%
      filter(fecha==max(fecha)) %>%
      filter(comuna!="Total") %>%
      mutate(tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
      mutate(rank = dense_rank(desc(casos))) %>%
      filter(rank < 20) %>%
      arrange(rank, desc(tasa)) %>%
      head(15) %>%
      select(rank, comuna, region, casos, tasa) %>%
      rename(Puesto=1,
             Comuna=2,
             Región=3,
             Casos=4,
             Tasa=5) %>%
      formattable(
        align = c("l", "l", "l", "c", "c"),
        list(
          # Aumento = formatter("span", 
          #                     x ~ percent(x, digits=1)),
          Comuna = formatter("span", style = ~ style(font.style = "bold")),
          #area(col = "Comuna") ~ style(font.style = "bold"),
          area(col = "Casos") ~ color_tile("#fce8ef", "#f3a5c0"),
          area(col = "Tasa") ~ color_tile("#f4e4f4", "#c8b1de")
          #area(col = "Aumento") ~ function(x) percent(x, digits = 0),
          #area(col = "Aumento") ~ color_tile("#eaf2fa", "#b0bee8")
        )
      )
  })
  
  output$tabla_casos_activos_comuna <- renderFormattable({
    tabla_casos_activos_comuna()
  })
  
  #Descarga
  output$comunas_activos_tabla_xlsx <- downloadHandler(
    filename = "comunas_activos_tabla.xlsx",
    content = function(filename) {
      writexl::write_xlsx(casos_activos_comuna() %>% 
                            select(fecha, region, comuna, casos, poblacion) %>%
                            filter(fecha==max(fecha)) %>%
                            filter(comuna!="Total") %>%
                            mutate(tasa = round((casos / poblacion) * 100000, digits = 1)) %>%
                            mutate(rank = dense_rank(desc(casos))) %>%
                            filter(rank < 20) %>%
                            arrange(rank, desc(tasa)) %>%
                            head(15) %>%
                            select(rank, comuna, region, casos, tasa) %>%
                            rename(Puesto=1,
                                   Comuna=2,
                                   Región=3,
                                   Casos=4,
                                   Tasa=5), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  # — ----
  
  #Pestaña 2: REGIONES ----
  
  #SELECTOR ----
  
  observe({
    updateSelectInput(session, "selector_region",
                      choices = levels(covid_region()$region),
                      selected = "Metropolitana"
    )
  })
  
  # Gráfico todas las regiones ----
  
  g_regiones <- reactive({
    # req(
    #   covid_region(),
    #   region_elegida(),
    #   g_reg_nuevos()
    # )
    
    
    p <- covid_region() %>%
      na.omit() %>%
      filter(region != "Total")
    
    if (region_elegida() == "Total") {
      p <- p %>%
        mutate(region2 = case_when(
          region == "Metropolitana" ~ "Sí",
          TRUE ~ "No"
        ))
    } else {
      p <- p %>%
        mutate(region2 = case_when(
          region == region_elegida() ~ "Sí",
          TRUE ~ "No"
        ))
    }
    
    p <- p %>%
      mutate(fecha2 = as.factor(paste0(lubridate::day(fecha), "-", lubridate::month(fecha)))) %>%
      mutate(region = recode(region,
                             "Arica y Parinacota" = "Arica"
      )) %>%
      group_by(region, region2, fecha, fecha2) %>%
      summarize(casos = sum(casos)) %>%
      filter(fecha >= lubridate::ymd("2020-03-10")) %>%
      ggplot(aes(fecha, casos,
                 group = region,
                 col = region2,
                 alpha = region2,
                 size = region2
      )) +
      geom_line(size = 2) +
      geom_text_repel(aes(
        x = max(fecha), y = casos,
        label = ifelse(fecha == max(fecha),
                       as.character(paste0(region, ": ", casos)), ""
        )
      ),
      hjust = 0,
      nudge_x = 4,
      box.padding = unit(4.5, "points"),
      min.segment.length = unit(7, "points"),
      segment.alpha = 0.2, segment.size = 1.5,
      size = 5,
      family = "Open Sans",
      direction = "y"
      ) +
      scale_x_date(
        breaks = seq(from = lubridate::ymd("2020-03-10"), to = max(covid_region()$fecha), length.out = 10),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.32))
      ) +
      scale_alpha_discrete(range = c(0.5, 1)) +
      scale_size_discrete(range = c(2, 4)) +
      scale_color_manual(values = rev(c("#DF1A57", "#7e47d1"))) +
      # scale_y_continuous(expand = expansion(mult=c(0.025, 0.025))) +
      theme(legend.position = "none") +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      # linea_gris_y_dashed +
      labs(
        subtitle = paste("Entre el 10 de marzo y", format(max(covid_region()$fecha), "%d de %B")),
        caption = "Mesa de datos COVID-19, casos totales por regiónt incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos contagiados con Covid-19"
      ) +
      theme(axis.text.x = element_text(
        angle = 45, vjust = 1, hjust = 1,
        margin = margin(t = 0, b = 5)
      ))
    
    p
  })
  
  # Out ----
  output$g_regiones_int <- renderGirafe({
    girafe(
      ggobj = g_regiones(),
      # width_svg = 8,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 9,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$regiones_xlsx <- downloadHandler(
    filename = "covid_region.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_region(), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Región elegida
  # g_total <- reactive({
  #   # req(
  #   #   region_elegida(),
  #   #   covid_region()
  #   # )
  #   
  #   p <- f_total() %>%
  #     ggplot(aes(fecha, casos)) +
  #     geom_line(size = 2, col = "#DF1A57", alpha = 0.6) +
  #     # geom_point(size=4, col="#DF1A57") +
  #     geom_point_interactive(aes(
  #       tooltip = stringr::str_wrap(
  #         paste(
  #           "Se reportaron", casos, "casos de Covid-19 en",
  #           ifelse(region == "Metropolitana",
  #                  paste("la región", region),
  #                  ifelse(region == "Total",
  #                         paste("el país"),
  #                         paste("la región de", region)
  #                  )
  #           ), "al", format(fecha, "%d de %B")
  #         ), 40
  #       ) # , data_id = porcentaje),
  #     ), size = 4, col = "#DF1A57") +
  #     geom_text(aes(
  #       label = casos,
  #       y = casos
  #     ),
  #     col = "#DF1A57", size = 4,
  #     family = "Open Sans",
  #     hjust = 0.5, vjust = -1.4,
  #     show.legend = FALSE
  #     ) +
  #     scale_x_date(
  #       breaks = seq(
  #         from = lubridate::ymd("2020-03-22"), to = max(covid_region()$fecha),
  #         length.out = 15
  #       ),
  #       expand = expansion(add = c(0, 0.6)),
  #       date_labels = "%d/%B"
  #     ) +
  #     scale_fill_manual(values = "#DF1A57") +
  #     coord_cartesian(clip = "off") +
  #     tema_lineas +
  #     ocultar_título_leyenda +
  #     ocultar_titulo_x +
  #     theme(
  #       axis.text.x = element_text(
  #         angle = 45, vjust = 1, hjust = 1,
  #         margin = margin(t = 5, b = 5)
  #       ),
  #       legend.text = element_text(margin = margin(r = 30))
  #     ) +
  #     theme(legend.position = "bottom") +
  #     labs(
  #       subtitle = paste(
  #         ifelse(region_elegida() == "Metropolitana",
  #                paste("Región Metropolitana"),
  #                ifelse(region_elegida() == "Total",
  #                       paste("Datos a nivel nacional"),
  #                       paste("Región de", region_elegida())
  #                )
  #         ),
  #         "\nEntre el 22 de marzo y", format(max(covid_region()$fecha), "%d de %B")
  #       ),
  #       caption = "Mesa de datos COVID-19, casos totales por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
  #       y = "Casos contagiados con Covid-19"
  #     )
  #   p
  # })
  # 
  # # Out
  # output$g_total_int <- renderGirafe({
  #   girafe(
  #     ggobj = g_total(),
  #     # width_svg = 16,
  #     width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
  #     height_svg = 7,
  #     # pointsize=20,
  #     options = list(
  #       opts_tooltip(use_fill = TRUE),
  #       opts_hover(css = "r: 8px"),
  #       opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
  #       opts_sizing(rescale = TRUE, width = .95),
  #       opts_toolbar(position = "topright", saveaspng = FALSE)
  #     )
  #   )
  # })
  # #Descarga
  # output$g_total_xlsx <- downloadHandler(
  #   filename = "total.xlsx",
  #   content = function(filename) {
  #     writexl::write_xlsx(f_total(), filename)
  #   },
  #   contentType = "application/xlsx"
  # )
  
  
  # Gráfico acumulado ----
  
  g_acumulado <- reactive({
    # req(
    #   covid_region(),
    #   g_total(),
    #   region_elegida()
    # )
    
    p <- covid_region() %>%
      na.omit() %>%
      filter(region != "Total")
    
    if (region_elegida() == "Total" | region_elegida() == "Metropolitana") {
      p <- p %>%
        mutate(region2 = case_when(
          region == "X" ~ "Destacada",
          region == "Metropolitana" ~ "Metropolitana",
          TRUE ~ "Regiones"
        ))
    } else {
      p <- p %>%
        mutate(region2 = case_when(
          region == region_elegida() ~ as.character(region_elegida()),
          region == "Metropolitana" ~ "Metropolitana",
          TRUE ~ "Regiones"
        ))
    }
    
    p <- p %>%
      group_by(region2, fecha) %>%
      summarize(casos = sum(casos)) %>%
      group_by(region2) %>%
      mutate(final = casos[fecha == max(fecha)]) %>%
      ungroup() %>%
      mutate(Pais = sum(unique(final))) %>%
      mutate(region = sum(unique(final[region2 != "Metropolitana"]))) %>%
      mutate(Altura = case_when(
        region2 == "Metropolitana" ~ Pais,
        region2 == "Regiones" ~ region,
        TRUE ~ casos
      )) %>%
      # Graficar
      ggplot(aes(fecha, casos,
                 group = forcats::fct_rev(forcats::fct_reorder(region2, final)),
                 col = forcats::fct_rev(forcats::fct_reorder(region2, final)),
                 fill = forcats::fct_rev(forcats::fct_reorder(region2, final))
      )) +
      geom_area() +
      geom_text(aes(
        x = max(fecha) + lubridate::days(1),
        y = Altura, # cumsum(casos),#ifelse(region2=="Metropolitana", casos*2, casos),
        label = ifelse(fecha == max(fecha),
                       as.character(paste0(region2, ":\n", casos, " casos")), ""
        )
      ),
      hjust = 0, size = 5, family = "Open Sans"
      ) +
      scale_x_date(
        breaks = seq(from = lubridate::ymd("2020-03-03"), to = max(covid_region()$fecha), length.out = 10),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.2))
      ) +
      scale_color_manual(
        values = degradado1_inverso(3),
        aesthetics = c("fill", "col")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      theme(legend.position = "none") +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      labs(
        subtitle = paste("Entre el 3 de marzo y", format(max(covid_region()$fecha), "%d de %B")),
        caption = "Mesa de datos COVID-19, casos totales por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos contagiados con Covid-19"
      ) +
      theme(axis.text.x = element_text(
        angle = 45, vjust = 1, hjust = 1,
        margin = margin(t = 3, b = 8)
      ))
    
    p
  })
  
  # Out ----
  output$g_acumulado_int <- renderGirafe({
    girafe(
      ggobj = g_acumulado(),
      # width_svg = 8,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  
  
  #Descarga
  output$regiones_acumulado_xlsx <- downloadHandler(
    filename = "covid_region.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_region(), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Casos nuevos regiones ----
  
  g_reg_nuevos <- reactive({
    # req(
    #   covid_region(),
    #   region_elegida(),
    #   g_acumulado()
    # )
    
    #Agregar selector para hacer en casos nuevos de regiones
    
    covid_region_nuevos() %>%
      filter(region == region_elegida()) %>%
      na.omit() %>%
      filter(fecha >= lubridate::ymd("2020-03-22")) %>%
      ggplot(aes(fecha, casos)) +
      geom_line(size = 2, col = "#DF1A57", alpha = 0.6) +
      # geom_point(size=4, col="#DF1A57") +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron", casos, "casos nuevos de Covid-19 con respecto al día anterior en",
            ifelse(region == "Metropolitana",
                   paste("la región", region),
                   ifelse(region == "Total",
                          paste("el país"),
                          paste("la región de", region)
                   )
            ), "al", format(fecha, "%d de %B")
          ), 40
        ) # , data_id = porcentaje),
      ), size = 4, col = "#DF1A57") +
      geom_label(aes(
        label = ifelse(casos > 0, paste0("+", casos, ""), "0"),
        y = casos
      ),
      label.padding = unit(2.2, "pt"), label.size = 0.4,
      family = "Open Sans", col = "#DF1A57",
      size = 4, hjust = 0.5, vjust = -1, show.legend = FALSE
      ) +
      scale_x_date(
        breaks = seq(
          from = lubridate::ymd("2020-03-22"), to = max(covid_region_nuevos()$fecha),
          by = 1
        ),
        # length.out=15),
        expand = expansion(add = c(0, 0.6)),
        date_labels = "%d/%B"
      ) +
      scale_fill_manual(values = "#DF1A57") +
      scale_y_continuous(labels = function(x) round(x, digits = 0)) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_título_leyenda +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45, vjust = 1, hjust = 1,
          margin = margin(t = 5, b = 5)
        ),
        legend.text = element_text(margin = margin(r = 30))
      ) +
      theme(legend.position = "bottom") +
      # labs(subtitle = paste("Entre el 22 de marzo y", format(max(covid_region()$fecha), "%d de %B") ),
      labs(
        subtitle = paste(
          ifelse(region_elegida() == "Metropolitana",
                 paste("Región Metropolitana"),
                 ifelse(region_elegida() == "Total",
                        paste("Datos a nivel nacional"),
                        paste("Región de", region_elegida())
                 )
          ),
          "\nEntre el 22 de marzo y", format(max(covid_region_nuevos()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos COVID-19, casos nuevos por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos nuevos de Covid-19"
      )
  })
  # Out ----
  
  output$g_reg_nuevos_int <- renderGirafe({
    girafe(
      ggobj = g_reg_nuevos(),
      # width_svg = 16,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  #Descarga
  output$regiones_nuevos_xlsx <- downloadHandler(
    filename = "reg_nuevos.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_region_nuevos(), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Examenes ----
  
  g_examenes <- reactive({
    # req(
    #   covid_totales(),
    #   g_hospitalizados()
    # )
    
    p <- covid_examenes() %>%
      na.omit() %>%
      filter(fecha >= "2020-04-10") %>%
      mutate(region = recode(region,
                             "Tarapaca" = "Tarapacá",
                             "Arica y Parinacota" = "Arica",
                             "Nuble" = "Ñuble",
                             "La Araucania" = "Araucanía",
                             "Del Libertador General Bernardo O’Higgins" = "O'Higgins",
                             "Magallanes y la Antartica" = "Magallanes"
      )) %>%
      mutate(region2 = case_when(
        region == "Metropolitana" ~ "Sí",
        TRUE ~ "No"
      )) %>%
      group_by(region) %>%
      mutate(Orden = casos[fecha == max(fecha)]) %>%
      group_by(region, region2, fecha, Orden) %>%
      summarize(casos = sum(casos)) %>%
      ggplot(aes(fecha, casos,
                 group = region,
                 col = forcats::fct_reorder(region, Orden),
                 fill = forcats::fct_reorder(region, Orden)
      )) +
      geom_line_interactive(aes(
        tooltip = stringr::str_wrap(
          paste("Región de", region), 40
        )
      ), size = 2) +
      geom_text_repel(aes(
        x = max(fecha), y = casos,
        label = ifelse(fecha == max(fecha),
                       as.character(paste0(region, ": ", casos)), ""
        )
      ),
      hjust = 0, nudge_x = 0.2,
      box.padding = unit(1, "points"), min.segment.length = unit(7, "points"), segment.alpha = 0.2,
      size = 5, direction = "y"
      ) +
      scale_x_date(
        breaks = seq(from = lubridate::ymd("2020-04-09"), to = max(covid_examenes()$fecha), by = 1),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.25))
      ) +
      scale_color_manual(values = degradado1(16)) +
      scale_fill_manual(values = degradado1(16)) +
      facet_wrap(~ forcats::fct_rev(region2), ncol = 1, scales = "free_y") +
      theme(
        legend.position = "none",
        strip.text.x = element_blank()
      ) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      labs(
        subtitle = paste("Casos entre el 10 y", format(max(covid_examenes()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, Exámenes PCR por región\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Exámenes PCR realizados"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45, vjust = 1, hjust = 1,
          margin = margin(t = 0, b = 5)
        ),
        axis.text.y = element_text(margin = margin(l = 5, r = 3))
      )
    
    p
  })
  
  # Out ----
  output$g_examenes_int <- renderGirafe({
    girafe(
      ggobj = g_examenes(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 11,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$regiones_examenes_xlsx <- downloadHandler(
    filename = "regiones_examenes.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_examenes(), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Fallecidos region ----
  
  g_fallecidos_region <- reactive({
    # req(
    #   covid_region(),
    #   region_elegida(),
    #   g_regiones()
    # )
    
    p <- covid_fallecidos_region() %>%
      filter(region == region_elegida()) %>%
      na.omit() %>%
      filter(fecha >= lubridate::ymd("2020-03-22")) %>%
      ggplot(aes(fecha, casos)) +
      geom_line(size = 2, col = "#DF1A57", alpha = 0.4) +
      # geom_point(size=4, col="#DF1A57") +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron", casos, "fallecimientos producto de Covid-19 en",
            ifelse(region == "Metropolitana",
                   paste("la región", region),
                   ifelse(region == "Total",
                          paste("el país"),
                          paste("la región de", region)
                   )
            ), "al", format(fecha, "%d de %B")
          ), 40
        ) # , data_id = porcentaje),
      ), size = 4, col = "#DF1A57") +
      geom_text(aes(
        label = casos,
        y = casos
      ),
      col = "#DF1A57", size = 4,
      family = "Open Sans",
      hjust = 0.5, vjust = -1.5,
      show.legend = FALSE
      ) +
      scale_x_date(
        breaks = seq(
          from = lubridate::ymd("2020-03-22"), to = max(covid_fallecidos_region()$fecha),
          length.out = 15
        ),
        expand = expansion(add = c(0, 0.6)),
        date_labels = "%d/%B"
      ) +
      scale_fill_manual(values = "#DF1A57") +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_título_leyenda +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45, vjust = 1, hjust = 1,
          margin = margin(t = 5, b = 5)
        ),
        legend.text = element_text(margin = margin(r = 30))
      ) +
      theme(legend.position = "bottom") +
      labs(
        subtitle = paste(
          ifelse(region_elegida() == "Metropolitana",
                 paste("Región Metropolitana"),
                 ifelse(region_elegida() == "Total",
                        paste("Datos a nivel nacional"),
                        paste("Región de", region_elegida())
                 )
          ),
          "\nEntre el 22 de marzo y", format(max(covid_fallecidos_region()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos COVID-19, casos fallecidos por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Personas fallecidas por Covid-19"
      )
    p
  })
  #
  
  
  
  
  
  
  
  
  
  
  # Out ----
  output$g_fallecidos_region_int <- renderGirafe({
    girafe(
      ggobj = g_fallecidos_region(),
      # width_svg = 8,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$regiones_fallecidos_xlsx <- downloadHandler(
    filename = "regiones_fallecidos.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_fallecidos_region(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  
  
  # Fallecidos por región incremental ----
  
  fallecidos_region_g <- reactive({ # grafico
    p <- fallecidos_region() %>%
      filter(region != "Total") %>%
      na.omit() %>%
      group_by(region) %>%
      mutate(Orden = casos[fecha == max(fecha)]) %>%
      ggplot(
        aes(
          fecha,
          casos,
          group = region,
          fill = forcats::fct_reorder(region, Orden),
          col = forcats::fct_reorder(region, Orden)
        )
      ) +
      geom_line(size = 2) +
      geom_text_repel(
        aes(
          x = max(fecha),
          y = casos,
          label = ifelse(casos > 0,
                         ifelse(
                           fecha == max(fecha),
                           as.character(paste0(region, ": ", casos)), ""
                         ),
                         ""
          )
        ),
        hjust = 0,
        nudge_x = 0.5,
        box.padding = unit(1.7, "points"),
        min.segment.length = unit(8, "points"),
        segment.alpha = 0.3,
        size = 5,
        direction = "y"
      ) +
      scale_x_date(
        breaks = seq(
          from = min(fallecidos_region()$fecha),
          to = max(fallecidos_region()$fecha),
          length.out = 10
        ),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.26))
      ) +
      scale_color_manual(
        values = rev(degradado1(17)),
        aesthetics = c("fill", "col")
      ) +
      theme(legend.position = "none") +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      labs(
        subtitle = paste(
          "Casos entre el",
          format(min(fallecidos_region()$fecha), "%d de %B"),
          "y el",
          format(max(fallecidos_region()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, Fallecidos por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Fallecimientos por región"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(l = 5, b = 5)
        ),
        axis.text.y = element_text(margin = margin(l = 5, r = 3))
      )
    p
  })
  
  # Out ----
  output$fallecidos_region_int <- renderGirafe({
    girafe(
      ggobj = fallecidos_region_g(),
      # width_svg = 16,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 9,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$fallecidos_region_inc_xlsx <- downloadHandler(
    filename = "fallecidos_region_incremental.xlsx",
    content = function(filename) {
      writexl::write_xlsx(fallecidos_region(), filename)
    },
    contentType = "application/xlsx"
  )
  
  # — ----
  
  
  # Pestaña 3: COMUNAS ----
  
  
  #Gráfico general de comunas ----
  
  # selector región
  observe({
    updateSelectInput(session, "selector_region_g_comuna",
                      choices = levels(as.factor(covid_comuna()$region)),
                      selected = "Metropolitana"
    )
  })
  
  # resultado de selector región
  region_g_comuna_elegida <- reactive({
    # req(input$selector_region_g_comuna)
    
    as.character(input$selector_region_g_comuna)
  })
  
  
  # filtrar la región elegida (para contar las comunas que tiene)
  g_comuna_pre <- reactive({
    p <- covid_comuna() %>%
      filter(region == region_g_comuna_elegida()) %>%
      select(comuna, casos, fecha) %>%
      na.omit() %>%
      # filter(fecha >= lubridate::ymd("2020-03-30")) %>%
      droplevels()
  })
  
  # cantidad de comunas de la región elegida
  cantidad_comunas_region <- reactive({
    nlevels(as.factor(as.character(g_comuna_pre()$comuna)))
  })
  
  # filtrar comunas si son mayores a 8
  g_comuna_pre_pre <- reactive({
    req(g_comuna_pre())
    
    # p_p <- g_comuna_pre()
    
    
    # crear filtro
    filtro <- g_comuna_pre() %>%
      group_by(comuna) %>%
      mutate(casos_final = casos[fecha == max(fecha)]) %>%
      ungroup() %>%
      filter(fecha == max(fecha)) %>%
      arrange(desc(casos_final)) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= 8) %>%
      select(comuna) %>%
      pull()
    
    
    # aplicar filtro a regiones con más de 10 comunas
    if (cantidad_comunas_region() >= 8) {
      p_p <- g_comuna_pre() %>%
        filter(comuna %in% filtro)
      # group_by(comuna) %>%
      # mutate(casos_final = casos[fecha == max(fecha)]) %>%
      # ungroup() %>%
      # # arrange(casos_final) %>%
      # mutate(Rank = dense_rank(desc(casos_final))) %>%
      # filter(Rank <= 10) %>%
      # na.omit() %>%
      # droplevels()
    } else {
      p_p <- g_comuna_pre()
    }
    p_p
  })
  
  # cantidad de comunas luego del filtro
  cantidad_comunas_region_2 <- reactive({
    req(g_comuna_pre_pre())
    nlevels(as.factor(as.character(g_comuna_pre_pre()$comuna)))
  })
  
  
  # graficar
  grafico_comunas <- reactive({
    req(g_comuna_pre_pre())
    
    p2 <- g_comuna_pre_pre() %>%
      filter(casos != 0) %>%
      filter(!is.na(casos)) %>%
      na.omit() %>%
      group_by(comuna) %>%
      mutate(final = casos[fecha == max(fecha)]) %>%
      ggplot(aes(fecha, casos,
                 col = forcats::fct_rev(forcats::fct_reorder(stringr::str_wrap(comuna, 12), final))
      )) +
      geom_line(
        size = 2, alpha = 0.4,
        show.legend = FALSE
      ) +
      # geom_point(size=4) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste0(
            "Se reportaron ", casos, " casos de Covid-19 en la comuna de ",
            comuna, " al ", format(fecha, "%d de %B")
          ), 40
        ) # , data_id = porcentaje),
      ), size = 4) +
      geom_text(aes(
        label = ifelse(
          fecha != max(fecha), casos, ""
        ),
        y = casos
      ),
      size = 4, vjust = -1, hjust = 0.5,
      # ccheck_overlap = TRUE,
      show.legend = FALSE
      ) +
      geom_text_repel(
        aes(
          x = max(fecha),
          y = casos,
          label = ifelse(casos > 0,
                         ifelse(
                           fecha == max(fecha),
                           as.character(paste0(stringr::str_wrap(comuna, 12), ": ", casos)), ""
                         ),
                         ""
          )
        ),
        hjust = 0,
        nudge_x = 1,
        box.padding = unit(1.7, "points"),
        min.segment.length = unit(8, "points"),
        segment.alpha = 0.4,
        size = 5,
        direction = "y"
      ) +
      scale_x_date(
        breaks = seq(
          from = min(covid_comuna()$fecha), to = max(covid_comuna()$fecha),
          by = 1
        ), # length.out=10),
        expand = expansion(mult = c(0, 0.25)),
        date_labels = "%d/%B"
      ) +
      scale_color_manual_interactive(drop = TRUE, values = degradado7_2(as.numeric(cantidad_comunas_region_2()))) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_título_leyenda +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, margin = margin(t = 5, b = -10)),
        legend.text = element_text(hjust = 0, margin = margin(r = 20)),
        axis.text.y = element_text(margin = margin(l = 5, r = 3))
      ) +
      theme(legend.position = "bottom") +
      labs(
        subtitle = paste(
          ifelse(region_elegida() == "Metropolitana",
                 paste("Región Metropolitana"),
                 paste("Región de", region_g_comuna_elegida())
          ),
          "\nCasos entre el", format(min(covid_comuna()$fecha), "%d de %B"),
          "y el", format(max(covid_comuna()$fecha), "%d de %B")
        ),
        caption = "Reporte diario Covid-19, Ministerio de Salud Mesa de datos Covid-19, casos totales por comuna incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos contagiados con Covid-19"
      )
    p2
  })
  
  # Out ----
  output$grafico_comunas_int <- renderGirafe({
    girafe(
      ggobj = grafico_comunas(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = ifelse(cantidad_comunas_region() >= 8, 11, 7),
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$grafico_comunas_xlsx <- downloadHandler(
    filename = "grafico_comunas.xlsx",
    content = function(filename) {
      writexl::write_xlsx(g_comuna_pre_pre(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  # Casos nuevos por comuna ----
  # Casos nuevos por fecha de inicio de síntomas por comuna
  
  # primer selector: región
  observe({
    updateSelectInput(session, "selector_region_nuevos_comuna",
                      choices = levels(as.factor(nuevos_comuna()$region)),
                      selected = "Metropolitana"
    )
  })
  # resultado de selector región
  region_nuevos_comuna_elegida <- reactive({
    req(input$selector_region_nuevos_comuna)
    
    input$selector_region_nuevos_comuna
  })
  
  # filtrar datos con la región elegida
  nuevos_comuna1 <- reactive({
    nuevos_comuna() %>%
      select(-starts_with("codigo")) %>%
      filter(region == region_nuevos_comuna_elegida()) %>%
      droplevels()
  })
  
  # segundo selector: comuna
  observeEvent(input$selector_region_nuevos_comuna, {
    req(input$selector_region_nuevos_comuna)
    updateSelectInput(session, "selector_comuna_nuevos_comuna",
                      choices = levels(as.factor(nuevos_comuna1()$comuna)),
                      selected = "La Florida"
    )
  })
  
  # resultado segundo selector: comuna
  comuna_nuevos_comuna_elegida <- reactive({
    req(input$selector_comuna_nuevos_comuna)
    
    input$selector_comuna_nuevos_comuna
  })
  
  # filtrar datos con la comuna elegida
  nuevos_comuna2 <- reactive({
    req(nuevos_comuna1(), input$selector_region_nuevos_comuna)
    
    nuevos_comuna1() %>%
      filter(comuna == comuna_nuevos_comuna_elegida()) %>%
      group_by(semana_epidemiologica) %>%
      mutate(etiqueta = paste0(
        format(inicio_semana_epidemiologica, "%d/%b"),
        "-",
        format(fin_semana_epidemiologica, "%d/%b")
      ))
  })
  
  #graficar
  nuevos_comuna_g <- reactive({ # grafico
    req(
      nuevos_comuna1(), nuevos_comuna2(),
      input$selector_region_nuevos_comuna,
      input$selector_comuna_nuevos_comuna
    )
    
    p <- nuevos_comuna2() %>%
      ungroup() %>%
      mutate(semana_epidemiologica = forcats::fct_reorder(semana_epidemiologica, inicio_semana_epidemiologica)) %>%
      ggplot(aes(semana_epidemiologica, casos,
                 group = comuna
      )) +
      geom_line(size = 2, col = "#DF1A57", alpha = 0.4) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron", casos, "casos nuevos",
            "para la semana epidemiológica", etiqueta
          ),
          40
        )
      ), size = 4, col = "#DF1A57") +
      geom_label(
        aes(
          label = ifelse(casos > 0, paste0("+", casos, ""), "0"),
          y = casos
        ),
        label.padding = unit(2.2, "pt"),
        label.size = 0.4,
        family = "Open Sans",
        col = "#DF1A57",
        size = 4,
        hjust = 0.5,
        vjust = -0.8,
        show.legend = FALSE
      ) +
      scale_fill_manual(values = "#DF1A57") +
      scale_y_continuous(
        labels = function(x) {
          round(x, digits = 0)
        },
        expand = expansion(mult = c(0, 0.5)),
      ) +
      scale_x_discrete(
        breaks = nuevos_comuna2()$semana_epidemiologica,
        labels = nuevos_comuna2()$etiqueta
      ) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_título_leyenda +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 5, b = 5)
        ),
        legend.text = element_text(margin = margin(r = 30))
      ) +
      theme(legend.position = "bottom") +
      labs(
        caption = "Mesa de datos COVID-19, casos nuevos por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos nuevos de Covid-19"
      )
    p
  })
  
  # Out ----
  output$nuevos_comuna_int <- renderGirafe({
    girafe(
      ggobj = nuevos_comuna_g(),
      # width_svg = 16,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$covid_nuevos_comuna_xlsx <- downloadHandler(
    filename = "covid_nuevos_comuna.xlsx",
    content = function(filename) {
      writexl::write_xlsx(nuevos_comuna2(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  # Casos activos por comuna ----
  # Casos activos por fecha de inicio de síntomas y comuna
  
  # primer selector: región
  observe({
    updateSelectInput(session, "selector_region_activos_comuna",
                      choices = levels(as.factor(activos_comuna()$region)),
                      selected = "Metropolitana"
    )
  })
  
  activos_comuna_region_elegida <- reactive({
    req(input$selector_region_activos_comuna)
    
    input$selector_region_activos_comuna
  })
  
  
  
  # filtrar datos con la región elegida
  activos_comuna1 <- reactive({
    activos_comuna() %>%
      select(-starts_with("codigo")) %>%
      filter(region == activos_comuna_region_elegida()) %>%
      droplevels()
  })
  
  # segundo selector: comuna
  observeEvent(input$selector_region_activos_comuna, {
    req(input$selector_region_activos_comuna)
    
    updateSelectInput(session, "selector_comuna_activos_comuna",
                      choices = levels(as.factor(activos_comuna1()$comuna)),
                      selected = "Puente Alto"
    )
  })
  
  # resultado segundo selector: comuna
  activos_comuna_comuna_elegida <- reactive({
    req(input$selector_region_activos_comuna)
    
    input$selector_comuna_activos_comuna
  })
  
  
  # filtrar datos con la comuna elegida
  activos_comuna2 <- reactive({
    req(activos_comuna1(), input$selector_comuna_activos_comuna)
    
    activos_comuna1() %>%
      filter(comuna == activos_comuna_comuna_elegida()) %>%
      droplevels()
  })
  
  
  #grafico
  activos_comuna_g <- reactive({ # grafico
    req(
      activos_comuna1(),
      activos_comuna2(),
      input$selector_comuna_activos_comuna
    )
    
    p <- activos_comuna2() %>%
      na.omit() %>%
      filter(!is.na(fecha)) %>%
      mutate(tasa = (casos / poblacion) * 100000) %>%
      tidyr::pivot_longer(
        cols = c(tasa, casos),
        names_to = "grupo",
        values_to = "casos"
      ) %>%
      mutate(grupo = stringr::str_to_sentence(grupo)) %>%
      ggplot(aes(fecha, casos,
                 fill = grupo,
                 col = grupo
      )) +
      geom_line(size = 2, alpha = 0.6) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron", casos[grupo == "Casos"], "casos de Covid-19 y una tasa de",
            round(casos[grupo == "Tasa"], digits = 1),
            "casos por cada 100 mil habitantes",
            "al", format(fecha, "%d de %B")
          ), 40
        )
      ), size = 4) +
      geom_text(
        aes(
          label = ifelse(grupo == "Casos", casos, ""),
          y = casos
        ),
        size = 4,
        family = "Open Sans",
        hjust = 0.5,
        vjust = -1.4,
        show.legend = FALSE
      ) +
      geom_text(
        aes(
          label = ifelse(grupo == "Tasa",
                         round(casos, digits = 1), ""
          ),
          y = casos
        ),
        size = 4,
        family = "Open Sans",
        hjust = 0.5,
        vjust = -1.4,
        show.legend = FALSE
      ) +
      scale_x_date(
        breaks = seq(
          from = min(activos_comuna2()$fecha),
          to = max(activos_comuna2()$fecha),
          by = 1
        ),
        expand = expansion(add = c(0, 0.6)),
        date_labels = "%d/%B"
      ) +
      scale_fill_manual(
        values = rev(c("#DF1A57", "#7e47d1")),
        aesthetics = c("colour", "fill")
      ) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_título_leyenda +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 5, b = 0)
        ),
        legend.text = element_text(margin = margin(r = 30))
      ) +
      theme(legend.position = "bottom") +
      labs(
        subtitle = paste(
          "\nEntre el",
          format(min(activos_comuna2()$fecha), "%d de %B"),
          "y el",
          format(max(activos_comuna2()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos COVID-19, casos activos por fecha de inicio de síntomas y comuna\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos y tasa de Covid-19"
      )
    p
  })
  
  # Out ----
  output$activos_comuna_int <- renderGirafe({
    girafe(
      ggobj = activos_comuna_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$casos_activos_comuna_xlsx <- downloadHandler(
    filename = "casos_activos_comuna.xlsx",
    content = function(filename) {
      writexl::write_xlsx(activos_comuna2(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  # Ranking casos y tasa de contagios por comuna ----
  
  # selector región
  observe({
    updateSelectInput(session, "selector_region_g_comuna_tasa",
                      choices = levels(as.factor(covid_comuna()$region)),
                      selected = "Metropolitana"
    )
  })
  
  # resultado de selector región
  region_g_comuna_tasa_elegida <- reactive({
    # req(input$selector_region_g_comuna)
    
    input$selector_region_g_comuna_tasa
  })
  
  #filtro de región elegida
  g_comuna_tasa_pre <- reactive({
    p <- covid_comuna() %>%
      filter(region == region_g_comuna_tasa_elegida())
    p
  })
  
  #gráfico
  rank_casos_tasa_comuna_g <- reactive({
    p <- g_comuna_tasa_pre() %>%
      filter(fecha == max(fecha)) %>%
      mutate(Tasa = (casos / poblacion) * 100000) %>%
      mutate(Rank = dense_rank(desc(Tasa))) %>%
      arrange(Rank) %>%
      filter(Rank <= 15) %>% # filtrar las 15 comunas más altas
      tidyr::pivot_longer(cols = c(casos, Tasa), names_to = "Grupo", values_to = "Valor") %>%
      group_by(comuna) %>%
      mutate(Valor = case_when(
        is.na(Valor) ~ 0,
        TRUE ~ Valor
      )) %>%
      mutate(Orden = Valor[Grupo == "Tasa"]) %>%
      mutate(Grupo = recode(Grupo,
                            "casos" = "casos confirmados\nde contagio",
                            "Tasa" = "casos por cada\n100 mil habitantes"
      )) %>%
      ggplot(aes(forcats::fct_reorder(comuna, Orden),
                 Valor,
                 fill = Grupo, col = Grupo
      )) +
      geom_col(position = "dodge", width = 0.7) +
      geom_text(aes(label = round(Valor, digits = 1)), # ifelse(Grupo=="casos por cada\n100 mil habitantes", round(Valor/2, digits=0), Valor)),
                position = position_dodge2(width = 0.7),
                family = "Open Sans",
                angle = 90,
                hjust = -0.2, vjust = 0.5,
                size = 5
      ) +
      scale_fill_manual(
        values = rev(c("#DF1A57", "#7e47d1")),
        aesthetics = c("colour", "fill")
      ) +
      labs(
        subtitle = paste(
          ifelse(region_elegida() == "Metropolitana",
                 paste("Región Metropolitana"),
                 paste("Región de", as.character(region_g_comuna_tasa_elegida()))
          ),
          "\nÚltima actualización:", format(max(covid_comuna()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, casos totales por comuna incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad de casos contagiados y\ntasa de contagios por 100.000 habitantes"
      ) +
      coord_cartesian(clip = "off") + # , ylim=c(0,700)) +
      tema_barras_label +
      ocultar_titulo_x +
      ocultar_título_leyenda +
      linea_gris_y +
      theme(
        axis.text.x = element_text(
          size = 13, angle = 45, hjust = 1,
          margin = margin(t = -5, b = -5)
        ),
        legend.position = "bottom", # c(.305,.94),
        legend.direction = "horizontal",
        legend.text = element_text(size = 13, margin = margin(r = 10)),
        plot.caption = element_text(margin = margin(t = 10)),
        legend.key.size = unit(1.2, "lines")
      )
    
    p
  })
  
  # Out ----
  output$rank_casos_tasa_comuna_int <- renderGirafe({
    girafe(
      ggobj = rank_casos_tasa_comuna_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$rank_casos_tasa_comuna_xlsx <- downloadHandler(
    filename = "rank_casos_tasa_comunas.xlsx",
    content = function(filename) {
      writexl::write_xlsx(g_comuna_tasa_pre() %>%
                            filter(fecha == max(fecha)) %>%
                            mutate(Tasa = (casos / poblacion) * 100000) %>%
                            mutate(Rank = dense_rank(desc(Tasa))) %>%
                            arrange(Rank) %>%
                            filter(Rank <= 15) %>% # filtrar las 15 comunas más altas
                            tidyr::pivot_longer(cols = c(casos, Tasa), names_to = "Grupo", values_to = "Valor") %>%
                            group_by(comuna) %>%
                            mutate(Valor = case_when(
                              is.na(Valor) ~ 0,
                              TRUE ~ Valor
                            )) %>%
                            mutate(Orden = Valor[Grupo == "Tasa"]) %>%
                            mutate(Grupo = recode(Grupo,
                                                  "casos" = "casos confirmados\nde contagio",
                                                  "Tasa" = "casos por cada\n100 mil habitantes"
                            )), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Ranking comunas con mas casos ----
  rank_casos_comuna_g <- reactive({
    
    p <- covid_comuna() %>%
      filter(fecha == max(fecha)) %>%
      mutate(Tasa = (casos / poblacion) * 100000) %>%
      mutate(Rank = dense_rank(desc(casos))) %>%
      arrange(Rank) %>%
      filter(Rank <= 10) %>%
      tidyr::pivot_longer(cols = c(casos, Tasa), names_to = "Grupo", values_to = "Valor") %>%
      group_by(comuna) %>%
      mutate(Valor = case_when(
        is.na(Valor) ~ 0,
        TRUE ~ Valor
      )) %>%
      mutate(Orden = Valor[Grupo == "casos"]) %>%
      mutate(Grupo = recode(Grupo,
                            "casos" = "Casos confirmados\nde contagio",
                            "Tasa" = "Casos por cada\n100 mil habitantes"
      )) %>%
      ggplot(aes(forcats::fct_reorder(comuna, Orden),
                 Valor,
                 fill = Grupo,
                 col = Grupo
      )) +
      geom_col(
        position = position_dodge2(width = 1),
        width = 0.7,
        show.legend = FALSE
      ) +
      geom_text(aes(label = round(Valor, digits = 1)), # ifelse(Grupo=="casos por cada\n100 mil habitantes", round(Valor/2, digits=0), Valor)),
                position = position_dodge2(width = 0.8),
                family = "Open Sans",
                size = 4.5, vjust = 0.5, hjust = -0.2,
                show.legend = FALSE
      ) +
      geom_text(aes(x = comuna, y = -20, label = region), # ifelse(comuna=="Pica", region, "")),
                hjust = 1, vjust = 1.2, size = 5, color = "gray80", family = "Open Sans",
                check_overlap = TRUE, show.legend = FALSE
      ) +
      scale_fill_manual(
        values = rev(c("#DF1A57", "#7e47d1")),
        aesthetics = c("colour", "fill")
      ) +
      labs(
        subtitle = paste("10 comunas del país con mayor cantidad de contagios\nÚltima actualización:", format(max(covid_comuna()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, casos totales por comuna incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad y tasa de contagios por 100.000 habitantes"
      ) +
      coord_flip(clip = "off", ylim = c(0, 800)) +
      tema_barras_horizontales_3 +
      ocultar_título_leyenda +
      theme(
        legend.key = element_blank(),
        legend.background = element_blank()
      ) +
      geom_point(size = 0, alpha = 0) +
      guides(fill = guide_legend(reverse = TRUE)) +
      guides(col = guide_legend(
        reverse = TRUE,
        override.aes = list(size = 5, fill = NA, text = NA, alpha = 1)
      ))
    
    p
  })
  
  # Out ----
  output$rank_casos_comuna_int <- renderGirafe({
    girafe(
      ggobj = rank_casos_comuna_g(),
      # width_svg = 16,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  
  #Descarga
  output$rank_casos_comuna_xlsx <- downloadHandler(
    filename = "rank_casos_comunas.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_comuna() %>%
                            filter(fecha == max(fecha)) %>%
                            mutate(Tasa = (casos / poblacion) * 100000) %>%
                            mutate(Rank = dense_rank(desc(casos))) %>%
                            arrange(Rank) %>%
                            filter(Rank <= 10) %>%
                            tidyr::pivot_longer(cols = c(casos, Tasa), names_to = "Grupo", values_to = "Valor") %>%
                            group_by(comuna) %>%
                            mutate(Valor = case_when(
                              is.na(Valor) ~ 0,
                              TRUE ~ Valor
                            )) %>%
                            mutate(Orden = Valor[Grupo == "casos"]) %>%
                            mutate(Grupo = recode(Grupo,
                                                  "casos" = "Casos confirmados\nde contagio",
                                                  "Tasa" = "Casos por cada\n100 mil habitantes"
                            )), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Comunas con mayor tasa ----
  # Comuna tasas ranking país
  comuna_tasa_ranking_g <- reactive({
    
    p <- covid_comuna() %>%
      filter(fecha == max(fecha)) %>%
      mutate(Tasa = (casos / poblacion) * 100000) %>%
      mutate(Rank = dense_rank(desc(Tasa))) %>%
      # select(region, comuna, casos, Tasa, Rank) %>%
      arrange(Rank) %>%
      filter(Rank <= 10) %>%
      tidyr::pivot_longer(cols = c(casos, Tasa), names_to = "Grupo", values_to = "Valor") %>%
      group_by(comuna) %>%
      mutate(Valor = case_when(
        is.na(Valor) ~ 0,
        TRUE ~ Valor
      )) %>%
      mutate(Orden = Valor[Grupo == "Tasa"]) %>%
      mutate(Grupo = recode(Grupo,
                            "casos" = "Casos confirmados\nde contagio",
                            "Tasa" = "Casos por cada\n100 mil habitantes"
      )) %>%
      ggplot(aes(forcats::fct_reorder(comuna, Orden),
                 Valor,
                 fill = Grupo,
                 col = Grupo
      )) +
      geom_col(
        position = position_dodge2(width = 1),
        width = 0.7,
        show.legend = FALSE
      ) +
      geom_text(aes(label = round(Valor, digits = 1)),
                position = position_dodge2(width = 0.8),
                family = "Open Sans",
                size = 4.5, vjust = 0.5, hjust = -0.2,
                show.legend = FALSE
      ) +
      geom_text(aes(x = comuna, y = -20, label = region), # ifelse(comuna=="Pica", region, "")),
                hjust = 1, vjust = 1.2, size = 5, color = "gray80", family = "Open Sans",
                check_overlap = TRUE, show.legend = FALSE
      ) +
      scale_fill_manual(
        values = rev(c("#DF1A57", "#7e47d1")),
        aesthetics = c("colour", "fill")
      ) +
      labs(
        subtitle = paste("10 comunas del país con mayor tasa de contagios\nÚltima actualización:", format(max(covid_comuna()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, casos totales por comuna incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad y tasa de contagios por 100.000 habitantes"
      ) +
      coord_flip(clip = "off", ylim = c(0, 1000)) +
      tema_barras_horizontales_3 +
      ocultar_título_leyenda +
      theme(
        legend.key = element_blank(),
        legend.background = element_blank()
      ) +
      geom_point(size = 0, alpha = 0) +
      guides(fill = guide_legend(reverse = TRUE)) +
      guides(col = guide_legend(
        reverse = TRUE,
        override.aes = list(size = 5, fill = NA, text = NA, alpha = 1)
      ))
    
    p
  })
  
  # Out ----
  output$comuna_tasa_ranking_int <- renderGirafe({
    girafe(
      ggobj = comuna_tasa_ranking_g(),
      # width_svg = 16,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$comuna_tasa_ranking_xlsx <- downloadHandler(
    filename = "comuna_tasa_ranking.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_comuna() %>%
                            filter(fecha == max(fecha)) %>%
                            mutate(Tasa = (casos / poblacion) * 100000) %>%
                            mutate(Rank = dense_rank(desc(Tasa))) %>%
                            # select(region, comuna, casos, Tasa, Rank) %>%
                            arrange(Rank) %>%
                            filter(Rank <= 10) %>%
                            tidyr::pivot_longer(cols = c(casos, Tasa), names_to = "Grupo", values_to = "Valor") %>%
                            group_by(comuna) %>%
                            mutate(Valor = case_when(
                              is.na(Valor) ~ 0,
                              TRUE ~ Valor
                            )) %>%
                            mutate(Orden = Valor[Grupo == "Tasa"]) %>%
                            mutate(Grupo = recode(Grupo,
                                                  "casos" = "Casos confirmados\nde contagio",
                                                  "Tasa" = "Casos por cada\n100 mil habitantes"
                            )), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  

  
  
  
  
  
  # — ----
  
  #Pestaña 1: GENERAL ----
  
  # Totales nacionales ----
  
  g_totales_nacionales <- reactive({
    #req(covid_totales())
    
    p <- f_totales_nacionales() %>%
      filter(categoria != "Fallecidos") %>% 
      ggplot(aes(fecha, casos,
                 col = forcats::fct_reorder(categoria, final),
                 fill = forcats::fct_reorder(categoria, final)
      )) +
      geom_line(size = 2, alpha = 0.4) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste0(
            stringr::str_to_sentence(categoria), ": ", casos, " casos",
            " al ", format(fecha, "%d de %B")
          ), 40
        )
      ), size = 3) +
      geom_text_repel(aes(
        x = max(fecha), y = casos,
        label = ifelse(fecha == max(fecha),
                       as.character(paste0(stringr::str_to_sentence(categoria), ": ", casos)), ""
        )
      ),
      hjust = 0,
      nudge_x = 2,
      box.padding = unit(2, "points"),
      min.segment.length = unit(7, "points"),
      segment.alpha = 0.2, segment.size = 1.5,
      size = 5,
      family = "Open Sans",
      direction = "y"
      ) +
      scale_x_date(
        breaks = seq(from = min(covid_totales()$fecha), to = max(covid_totales()$fecha), length.out = 10),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.25))
      ) +
      scale_color_manual_interactive(drop = TRUE, values = degradado1(5)) +
      theme(legend.position = "none") +
      coord_cartesian(clip = "off") + # , ylim=c(0,900)) +
      tema_lineas +
      theme(axis.text.x = element_text(
        angle = 45, vjust = 1, hjust = 1,
        margin = margin(t = 0, b = 2)
      )) +
      ocultar_titulo_x +
      labs(
        subtitle = paste("Casos entre el", format(min(covid_totales()$fecha), "%d de %B"), "y el", format(max(covid_totales()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, casos totales nacionales diarios\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad de casos"
      )
    
    p
  })
  
  # Output ----
  output$g_totales_nacionales_int <- renderGirafe({
    girafe(
      ggobj = g_totales_nacionales(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 9,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$g_totales_nacionales_xlsx <- downloadHandler(
    filename = "totales_nacionales.xlsx",
    content = function(filename) {
      writexl::write_xlsx(f_totales_nacionales(), filename)
    },
    contentType = "application/xlsx"
  )
  
  #Casos activos ----
  
  casos_activos_g <- reactive({
    #req(covid_totales())
    
    p <- f_totales_nacionales() %>%
      filter(categoria == "activos") %>% 
      ggplot(aes(fecha, casos,
                 col = forcats::fct_reorder(categoria, final),
                 fill = forcats::fct_reorder(categoria, final)
                 )) +
      geom_line(size = 2, alpha = 0.4) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste0(
            stringr::str_to_sentence(categoria), ": ", casos, " casos",
            " al ", format(fecha, "%d de %B")
          ), 40
        )
      ), size = 3) +
      geom_text(aes(
        label = ifelse(lubridate::day(fecha) %% 3 == 0,
                       ifelse(fecha!=max(fecha), 
                              casos,
                              ""),
                       "")),
      col = "#DF1A57", size = 4,
      family = "Open Sans",
      hjust = 1, vjust = -1.4,
      show.legend = FALSE) +
      geom_text_repel(aes(
        x = max(fecha), y = casos,
        label = ifelse(fecha == max(fecha),
                       as.character(paste0(stringr::str_to_sentence(categoria), ": ", casos)), ""
        )
      ),
      hjust = 0,
      nudge_x = 2,
      box.padding = unit(2, "points"),
      min.segment.length = unit(7, "points"),
      segment.alpha = 0.2, segment.size = 1.5,
      size = 5,
      family = "Open Sans",
      direction = "y"
      ) +
      scale_x_date(
        breaks = seq(from = min(covid_totales()$fecha), to = max(covid_totales()$fecha), length.out = 10),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.2))
      ) +
      scale_color_manual_interactive(drop = TRUE, values = degradado1(5)) +
      theme(legend.position = "none") +
      coord_cartesian(clip = "off") + # , ylim=c(0,900)) +
      tema_lineas +
      theme(axis.text.x = element_text(
        angle = 45, vjust = 1, hjust = 1,
        margin = margin(t = 0, b = 2)
      )) +
      ocultar_titulo_x +
      labs(
        subtitle = paste("Casos activos entre el", format(min(covid_totales()$fecha), "%d de %B"), "y el", format(max(covid_totales()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, casos totales nacionales diarios\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad de casos activos"
      )
    
    p
  })
  
  
  # Out ----
  output$casos_activos_int <- renderGirafe({
    girafe(
      ggobj = casos_activos_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 6,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  
  #Descarga----
  output$casos_activos_total_xlsx <- downloadHandler(
    filename = "activos_totales_nacionales.xlsx",
    content = function(filename) {
      writexl::write_xlsx(f_totales_nacionales() %>% filter(categoria == "activos"), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  # Casos nuevos total ----
  
  g_total_nuevos <- reactive({
    
    covid_region_nuevos() %>%
      filter(region == "Total") %>%
      na.omit() %>%
      filter(fecha >= lubridate::ymd("2020-03-22")) %>%
      ggplot(aes(fecha, casos)) +
      geom_line(size = 2, col = "#DF1A57", alpha = 0.6) +
      # geom_point(size=4, col="#DF1A57") +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron", casos, "casos nuevos de Covid-19 con respecto al día anterior en",
            ifelse(region == "Metropolitana",
                   paste("la región", region),
                   ifelse(region == "Total",
                          paste("el país"),
                          paste("la región de", region)
                   )
            ), "al", format(fecha, "%d de %B")
          ), 40
        ) # , data_id = porcentaje),
      ), size = 4, col = "#DF1A57") +
      geom_label(aes(
        label = ifelse(casos > 0, paste0("+", casos, ""), "0"),
        y = casos
      ),
      label.padding = unit(2.2, "pt"), label.size = 0.4,
      family = "Open Sans", col = "#DF1A57",
      size = 4, hjust = 0.5, vjust = -1, show.legend = FALSE
      ) +
      scale_x_date(
        breaks = seq(
          from = lubridate::ymd("2020-03-22"), to = max(covid_region_nuevos()$fecha),
          by = 1
        ),
        # length.out=15),
        expand = expansion(add = c(0, 0.6)),
        date_labels = "%d/%B"
      ) +
      scale_fill_manual(values = "#DF1A57") +
      scale_y_continuous(labels = function(x) round(x, digits = 0)) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_título_leyenda +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45, vjust = 1, hjust = 1,
          margin = margin(t = 5, b = 5)
        ),
        legend.text = element_text(margin = margin(r = 30))
      ) +
      theme(legend.position = "bottom") +
      # labs(subtitle = paste("Entre el 22 de marzo y", format(max(covid_region()$fecha), "%d de %B") ),
      labs(
        subtitle = paste(
          ifelse(region_elegida() == "Metropolitana",
                 paste("Región Metropolitana"),
                 ifelse(region_elegida() == "Total",
                        paste("Datos a nivel nacional"),
                        paste("Región de", region_elegida())
                 )
          ),
          "\nEntre el 22 de marzo y", format(max(covid_region_nuevos()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos COVID-19, casos nuevos por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos nuevos de Covid-19"
      )
  })
  # Out ----
  
  output$g_total_nuevos_int <- renderGirafe({
    girafe(
      ggobj = g_total_nuevos(),
      # width_svg = 16,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  #Descarga
  output$g_total_nuevos_xlsx <- downloadHandler(
    filename = "reg_nuevos.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_region_nuevos() %>% filter(region=="Total"), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Casos por genero y grupo de edad ----
  
  casos_genero_edad_g <- reactive({ # grafico
    p <- f_casos_genero_edad() %>%
      ggplot(aes(fecha, casos,
                 col = grupo_de_edad
      )) +
      geom_line(size = 2) +
      geom_text_repel(
        aes(
          x = max(fecha),
          y = casos,
          label = ifelse(casos > 0,
                         ifelse(
                           fecha == max(fecha),
                           as.character(paste0(grupo_de_edad, ": ", casos)), ""
                         ),
                         ""
          )
        ),
        hjust = 0,
        nudge_x = 0.3,
        box.padding = unit(1.7, "points"),
        min.segment.length = unit(8, "points"),
        segment.alpha = 0.3,
        size = 5,
        direction = "y"
      ) +
      scale_x_date(
        breaks = seq(
          from = min(casos_genero_edad()$fecha),
          to = max(casos_genero_edad()$fecha),
          by = 2
        ),
        # length.out=10),
        expand = expansion(mult = c(0, 0.27)),
        date_labels = "%d/%B"
      ) +
      scale_color_manual(
        name = "edades",
        values = rev(degradado7(6))
      ) +
      coord_cartesian(clip = "off") +
      facet_wrap(~sexo, ncol = 1) +
      tema_lineas +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 0, b = 2)
        ),
        legend.text = element_text(margin = margin(r = 20)),
        axis.text.y = element_text(margin = margin(l = 5, r = 5))
      ) +
      theme(legend.position = "none") +
      labs(
        subtitle = paste(
          "Casos entre el",
          format(min(casos_genero_edad()$fecha), "%d de %B"),
          "y el",
          format(max(casos_genero_edad()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, Casos por género y grupo de edad\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos según género y edad"
      )
    p
  })
  
  # Out ----
  output$casos_genero_edad_int <- renderGirafe({
    girafe(
      ggobj = casos_genero_edad_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 9,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$casos_genero_edad_xlsx <- downloadHandler(
    filename = "casos_genero_edad.xlsx",
    content = function(filename) {
      writexl::write_xlsx(f_casos_genero_edad(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  #Fallecidos totales ----
  
  fallecidos_total_g <- reactive({
    #req(covid_totales())
    
    p <- f_totales_nacionales() %>%
      filter(categoria == "Fallecidos") %>% 
      ggplot(aes(fecha, casos,
                 col = forcats::fct_reorder(categoria, final),
                 fill = forcats::fct_reorder(categoria, final)
      )) +
      geom_line(size = 2, alpha = 0.4) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste0(
            stringr::str_to_sentence(categoria), ": ", casos, " casos",
            " al ", format(fecha, "%d de %B")
          ), 40
        )
      ), size = 3) +
      geom_text_repel(aes(
        x = max(fecha), y = casos,
        label = ifelse(fecha == max(fecha),
                       as.character(paste0(stringr::str_to_sentence(categoria), ": ", casos)), ""
        )
      ),
      hjust = 0,
      nudge_x = 2,
      box.padding = unit(2, "points"),
      min.segment.length = unit(7, "points"),
      segment.alpha = 0.2, segment.size = 1.5,
      size = 5,
      family = "Open Sans",
      direction = "y"
      ) +
      scale_x_date(
        breaks = seq(from = min(covid_totales()$fecha), to = max(covid_totales()$fecha), length.out = 10),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.25))
      ) +
      scale_color_manual_interactive(drop = TRUE, values = degradado1(5)) +
      theme(legend.position = "none") +
      coord_cartesian(clip = "off") + # , ylim=c(0,900)) +
      tema_lineas +
      theme(axis.text.x = element_text(
        angle = 45, vjust = 1, hjust = 1,
        margin = margin(t = 0, b = 2)
      )) +
      ocultar_titulo_x +
      labs(
        subtitle = paste("Fallecimientos entre el", format(min(covid_totales()$fecha), "%d de %B"), "y el", format(max(covid_totales()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, casos totales nacionales diarios\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad de personas fallecidas"
      )
    
    p
  })
  
  # Out ----
  output$fallecidos_totales_int <- renderGirafe({
    girafe(
      ggobj = fallecidos_total_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 6,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$g_fallecidos_total_xlsx <- downloadHandler(
    filename = "fallecidos_totales_nacionales.xlsx",
    content = function(filename) {
      writexl::write_xlsx(f_totales_nacionales() %>% filter(categoria == "Fallecidos"), filename)
    },
    contentType = "application/xlsx"
  )
  # Fallecidos por edad ----
  
  g_fallecidos <- reactive({
    # req(
    #   covid_totales(),
    #   g_totales_nacionales()
    # )
    
    p <- covid_fallecidos() %>%
      mutate(edad = forcats::fct_relevel(grupo_de_edad, ">=90", after = Inf)) %>%
      na.omit() %>%
      ggplot(aes(fecha, casos, col = grupo_de_edad)) +
      geom_line(size = 2, alpha = 0.4) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron", casos, "personas fallecidas",
            "con una edad de", edad, "producto de Covid-19",
            "al", format(fecha, "%d de %B")
          ), 40
        )
      ), size = 4) +
      geom_text(aes(
        label = ifelse(fecha != max(fecha), casos,
                       ""
        ),
        y = casos
      ),
      size = 5, hjust = 0.5, vjust = -0.8,
      check_overlap = TRUE, show.legend = FALSE
      ) +
      geom_text_repel(aes(
        x = max(fecha), y = casos,
        label = ifelse(casos > 0,
                       ifelse(fecha == max(fecha),
                              as.character(paste0(grupo_de_edad, " años: ", casos)), ""
                       ),
                       ""
        )
      ),
      hjust = 0, nudge_x = 0.3,
      box.padding = unit(1.7, "points"),
      min.segment.length = unit(8, "points"),
      segment.alpha = 0.3,
      size = 5, direction = "y"
      ) +
      scale_x_date(
        breaks = seq(from = lubridate::ymd("2020-04-09"), to = max(covid_fallecidos()$fecha), by = 1), # length.out=10),
        expand = expansion(mult = c(0, 0.27)),
        date_labels = "%d/%B"
      ) +
      scale_color_manual(
        name = "edades",
        values = rev(degradado7(7))
      ) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45, vjust = 1, hjust = 1,
          margin = margin(t = 0, b = 2)
        ),
        legend.text = element_text(margin = margin(r = 20)),
        axis.text.y = element_text(margin = margin(l = 5, r = 5))
      ) +
      theme(legend.position = "none") +
      labs(
        subtitle = paste("Casos entre el 9 y", format(max(covid_fallecidos()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, Fallecidos por grupo de edad\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Defunciones"
      )
    
    p
  })
  
  # Letalidad ----
  
  g_letalidad <- reactive({
    # req(
    #   covid_totales(),
    #   g_fallecidos()
    # )
    
    p <- f_letalidad() %>%
      ggplot(aes(fecha, Tasa)) +
      geom_col_interactive(
        fill = "#DF1A57", width = 0.6,
        aes(
          tooltip = stringr::str_wrap(
            paste(
              "El día", format(fecha, "%d de %B"), "se registró una tasa de mortalidad de un",
              scales::percent(Tasa, accuracy = 0.01),
              "de personas fallecidas entre los", Activos,
              "casos activos"
            ), 40
          )
        )
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_date(
        breaks = seq(from = min(covid_totales()$fecha), to = max(covid_totales()$fecha), length.out = 10),
        date_labels = "%d/%B"
      ) +
      coord_cartesian(ylim = c(0, .05)) +
      geom_text(aes(label = ifelse(Tasa != 0,
                                   ifelse(lubridate::day(fecha) %% 3 == 0,
                                          scales::percent(Tasa, accuracy = 0.01), ""
                                   ),
                                   ""
      )),
      family = "Open Sans",
      angle = 90,
      size = 5, vjust = 0.5, hjust = -0.2
      ) +
      labs( # title="Tasa de letalidad del Covid-19",
        subtitle = paste("Nivel nacional\nÚltima actualización:", format(max(covid_totales()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, casos totales por región incremental\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Tasa de letalidad (proporción de\nfallecimientos entre los casos activos)"
      ) +
      tema_barras_label +
      ocultar_titulo_x +
      linea_gris_y +
      theme(
        axis.text.x = element_text(
          size = 13, angle = 45, hjust = 1,
          margin = margin(t = -5, b = -5)
        ),
        axis.text.y = element_text(margin = margin(r = 5)),
        plot.caption = element_text(margin = margin(t = 10)),
        legend.key.size = unit(1.2, "lines"),
        plot.title.position = "plot"
      )
    p
  })
  
  
  # Hospitalizados ----
  
  g_hospitalizados <- reactive({
    # req(
    #   covid_totales(),
    #   g_letalidad()
    # )
    
    p <- f_hospitalizados() %>%
      mutate(Orden = casos[fecha == max(fecha)]) %>%
      ggplot(aes(fecha, casos,
                 group = region,
                 col = forcats::fct_reorder(region, Orden),
                 col = forcats::fct_reorder(region, Orden)
      )) +
      geom_line_interactive(aes(
        tooltip = stringr::str_wrap(
          paste("Región de", region), 40
        )
      ), size = 2) +
      geom_text_repel(aes(
        x = max(fecha), y = casos,
        label = ifelse(casos > 0,
                       ifelse(fecha == max(fecha),
                              as.character(paste0(region, ": ", casos)), ""
                       ),
                       ""
        )
      ),
      hjust = 0, nudge_x = 0.5,
      box.padding = unit(1.7, "points"),
      min.segment.length = unit(8, "points"),
      segment.alpha = 0.3,
      size = 5, direction = "y"
      ) +
      scale_x_date(
        breaks = seq(from = lubridate::ymd("2020-04-01"), to = max(covid_hospitalizados()$fecha), length.out = 10),
        date_labels = "%d/%B",
        expand = expansion(mult = c(0, 0.26))
      ) +
      scale_color_manual(values = rev(degradado1(15))) +
      scale_fill_manual(values = rev(degradado1(15))) +
      theme(legend.position = "none") +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      labs(
        subtitle = paste("Exceptuando Región Metropolitana\nCasos entre el 1 y", format(max(covid_hospitalizados()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, Pacientes en UCI por región\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Casos hospitalizados en camas UCI"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45, vjust = 1, hjust = 1,
          margin = margin(l = 5, b = 5)
        ),
        axis.text.y = element_text(margin = margin(l = 5, r = 3))
      )
    
    p
  })
  
  
  
  # Output hospitalizados ----
  output$g_hospitalizados_int <- renderGirafe({
    girafe(
      ggobj = g_hospitalizados(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 10,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$g_hospitalizados_xlsx <- downloadHandler(
    filename = "hospitalizados.xlsx",
    content = function(filename) {
      writexl::write_xlsx(f_hospitalizados(), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Output fallecidos ----
  output$g_fallecidos_int <- renderGirafe({
    girafe(
      ggobj = g_fallecidos(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$g_fallecidos_xlsx <- downloadHandler(
    filename = "fallecidos.xlsx",
    content = function(filename) {
      writexl::write_xlsx(covid_fallecidos() %>% na.omit(), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Output letalidad ----
  output$g_letalidad_int <- renderGirafe({
    girafe(
      ggobj = g_letalidad(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$g_letalidad_xlsx <- downloadHandler(
    filename = "letalidad.xlsx",
    content = function(filename) {
      writexl::write_xlsx(f_letalidad() %>% janitor::clean_names(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  # Output recuperados ----
  output$g_recuperados_int <- renderGirafe({
    girafe(
      ggobj = g_recuperados(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  
 
  
  
  
  
  
  
  # — ----
  
  
  
  # Pestaña 4: HOSPITALIZADOS -----
  
  
  
  # Ventiladores mecánicos a nivel nacional ----
  
  ventiladores_g <- reactive({
    p <- ventiladores() %>%
      filter(ventiladores != "total") %>%
      mutate(ventiladores = stringr::str_to_sentence(ventiladores)) %>%
      ggplot(aes(fecha, casos,
                 fill = ventiladores,
                 col = ventiladores
      )) +
      geom_col_interactive(
        width = 0.6,
        position = "stack",
        show.legend = FALSE,
        aes(
          tooltip = stringr::str_wrap(
            paste(
              "El día", format(fecha, "%d de %B"),
              "se registraron",
              casos[ventiladores == "Disponibles"],
              "ventiladores disponibles y",
              casos[ventiladores == "Ocupados"],
              "ventiladores ocupados"
            ), 40
          )
        )
      ) +
      scale_x_date(
        breaks = seq(
          from = min(ventiladores()$fecha),
          to = max(ventiladores()$fecha),
          by = 1
        ),
        date_labels = "%d/%B"
      ) +
      geom_text(
        aes(
          y = casos,
          label = casos
        ),
        position = position_stack(vjust = 0.5),
        family = "Open Sans",
        angle = 90,
        color = "white",
        size = 5,
        vjust = 0.5,
        hjust = 0.5,
        show.legend = FALSE
      ) +
      labs(
        subtitle = paste(
          "Nivel nacional\nÚltima actualización:",
          format(max(ventiladores()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, ventiladores a nivel nacional\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad de ventiladores"
      ) +
      theme(
        legend.key = element_blank(),
        legend.background = element_blank()
      ) +
      tema_barras_label +
      scale_fill_manual(values = degradado1_inverso(2)) +
      scale_color_manual(values = degradado1_inverso(2)) +
      ocultar_titulo_x +
      ocultar_título_leyenda +
      theme(
        axis.text.x = element_text(
          size = 13,
          angle = 45,
          hjust = 1,
          margin = margin(t = -5, b = -5)
        ),
        axis.text.y = element_text(margin = margin(l = 10, r = -15)),
        plot.caption = element_text(margin = margin(t = 10)),
        legend.text = element_text(size = 13, margin = margin(r = 10)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.position = "bottom"
      ) +
      geom_point(size = 0, alpha = 0) +
      guides(fill = guide_legend(reverse = FALSE)) +
      guides(col = guide_legend(
        reverse = FALSE,
        nrow = 2,
        override.aes = list(
          size = 4,
          fill = NA,
          text = NA,
          alpha = 1
        )
      ))
    
    p
  })
  
  
  # Out ----
  output$ventiladores_int <- renderGirafe({
    girafe(
      ggobj = ventiladores_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$ventiladores_xlsx <- downloadHandler(
    filename = "ventiladores.xlsx",
    content = function(filename) {
      writexl::write_xlsx(ventiladores() %>%
                            filter(ventiladores != "total") %>%
                            mutate(ventiladores = stringr::str_to_sentence(ventiladores)), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  # Pacientes críticos ----
  
  pacientes_criticos_g <- reactive({ # grafico
    p <- pacientes_criticos() %>%
      ggplot(aes(fecha, casos,
                 fill = casos
      )) +
      geom_col_interactive(
        width = 0.6,
        show.legend = FALSE,
        aes(
          tooltip = stringr::str_wrap(
            paste(
              "El día", format(fecha, "%d de %B"),
              "se registraron",
              casos,
              "pacientes críticos"
            ), 40
          )
        )
      ) +
      scale_x_date(
        breaks = seq(
          from = min(pacientes_criticos()$fecha),
          to = max(pacientes_criticos()$fecha),
          # length.out = 10
          by = 1
        ),
        date_labels = "%d/%B"
      ) +
      coord_cartesian(ylim = c(0, 120)) +
      geom_text(
        aes(
          y = casos,
          label = casos
        ),
        family = "Open Sans",
        angle = 90,
        color = "black",
        size = 5,
        vjust = 0.5,
        hjust = -0.2,
        show.legend = FALSE
      ) +
      labs(
        subtitle = paste(
          "Nivel nacional\nÚltima actualización:",
          format(max(pacientes_criticos()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, pacientes críticos\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Cantidad de pacientes críticos"
      ) +
      theme(
        legend.key = element_blank(),
        legend.background = element_blank()
      ) +
      tema_barras_label +
      scale_fill_gradient(
        low = "#AF87EB",
        high = "#DF1A57"
      ) +
      ocultar_titulo_x +
      ocultar_título_leyenda +
      theme(
        axis.text.x = element_text(
          size = 13,
          angle = 45,
          hjust = 1,
          margin = margin(t = -5, b = -5)
        ),
        axis.text.y = element_text(margin = margin(l = 10, r = -15)),
        plot.caption = element_text(margin = margin(t = 10)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.position = "none"
      )
    
    p
  })
  
  # Out ----
  output$pacientes_criticos_int <- renderGirafe({
    girafe(
      ggobj = pacientes_criticos_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$pacientes_criticos_xlsx <- downloadHandler(
    filename = "pacientes_criticos.xlsx",
    content = function(filename) {
      writexl::write_xlsx(pacientes_criticos(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  # Pacientes hospitalizados según cama ----
  # Hospitalización de pacientes en sistema integrado
  
  hosp_integrado_g <- reactive({ # grafico
    p <- hosp_integrado() %>%
      na.omit() %>%
      ggplot(aes(fecha, casos,
                 col = tipo_de_cama
      )) +
      geom_line(size = 2, alpha = 0.6) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron un total de", casos, "pacientes en cama de tipo",
            tipo_de_cama,
            "al", format(fecha, "%d de %B")
          ), 40
        )
      ), size = 4) +
      geom_text_repel(
        aes(
          x = max(fecha),
          y = casos,
          label = ifelse(casos > 0,
                         ifelse(
                           fecha == max(fecha),
                           as.character(paste0(tipo_de_cama, ": ", casos)), ""
                         ),
                         ""
          )
        ),
        hjust = 0,
        nudge_x = 0.3,
        box.padding = unit(1.7, "points"),
        min.segment.length = unit(8, "points"),
        segment.alpha = 0.3,
        size = 5,
        direction = "y"
      ) +
      scale_x_date(
        breaks = seq(
          from = min(hosp_integrado()$fecha),
          to = max(hosp_integrado()$fecha),
          by = 1
        ),
        expand = expansion(mult = c(0, 0.27)),
        date_labels = "%d/%B"
      ) +
      scale_color_manual(
        name = "edades",
        values = rev(degradado7(4))
      ) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 0, b = 6)
        ),
        legend.text = element_text(margin = margin(r = 20)),
        axis.text.y = element_text(margin = margin(l = 5, r = 5))
      ) +
      theme(legend.position = "none") +
      labs(
        subtitle = paste(
          "Nivel nacional\nCasos entre el",
          format(min(hosp_integrado()$fecha), "%d de %B"),
          "y el",
          format(max(hosp_integrado()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, Hospitalización de pacientes en sistema integrado\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Pacientes según tipo de cama"
      )
    
    p
  })
  
  # Out ----
  output$hosp_integrado_int <- renderGirafe({
    girafe(
      ggobj = hosp_integrado_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$hosp_integrado_xlsx <- downloadHandler(
    filename = "hosp_integrado.xlsx",
    content = function(filename) {
      writexl::write_xlsx(hosp_integrado(), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  
  # Hospitalizados totales por grupo de edad y género ----
  
  hosp_edad_total_g <- reactive({ # grafico
    p <- hosp_edad_total() %>%
      mutate(
        grupo_de_edad = stringr::str_replace(grupo_de_edad, " - ", "-"),
        grupo_de_edad = stringr::str_replace(grupo_de_edad, " y más", "+"),
        grupo_de_edad = stringr::str_replace(grupo_de_edad, "00", "0")
      ) %>%
      na.omit() %>%
      mutate(sexo = recode(sexo,
                           "M" = "Hombres",
                           "F" = "Mujeres"
      )) %>%
      group_by(fecha, grupo_de_edad) %>%
      mutate(final = casos[fecha == max(fecha)]) %>%
      ungroup() %>%
      mutate(grupo_de_edad = forcats::fct_reorder(grupo_de_edad, final)) %>%
      ggplot(aes(fecha, casos,
                 col = grupo_de_edad
      )) +
      geom_line(size = 2) +
      geom_text_repel(
        aes(
          x = max(fecha),
          y = casos,
          label = ifelse(casos > 0,
                         ifelse(
                           fecha == max(fecha),
                           as.character(paste0(grupo_de_edad, ": ", casos)), ""
                         ),
                         ""
          )
        ),
        hjust = 0,
        nudge_x = 0.3,
        box.padding = unit(1.7, "points"),
        min.segment.length = unit(8, "points"),
        segment.alpha = 0.3,
        size = 5,
        direction = "y"
      ) +
      scale_x_date(
        breaks = seq(
          from = min(hosp_edad_total()$fecha),
          to = max(hosp_edad_total()$fecha),
          length.out = 15
        ),
        expand = expansion(mult = c(0, 0.27)),
        date_labels = "%d/%B"
      ) +
      scale_color_manual(
        name = "edades",
        values = rev(degradado7(7))
      ) +
      coord_cartesian(clip = "off") +
      facet_wrap(~sexo, ncol = 1) +
      tema_lineas +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 0, b = 6)
        ),
        legend.text = element_text(margin = margin(r = 20)),
        axis.text.y = element_text(margin = margin(l = 5, r = 5))
      ) +
      theme(legend.position = "none") +
      labs(
        subtitle = paste(
          "Casos entre el",
          format(min(hosp_edad_total()$fecha), "%d de %B"),
          "y el",
          format(max(hosp_edad_total()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, Hospitalizados por grupo de edad\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Pacientes hospitalizados"
      )
    p
  })
  
  # Out ----
  output$hosp_edad_total_int <- renderGirafe({
    girafe(
      ggobj = hosp_edad_total_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 9,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$hosp_edad_total_xlsx <- downloadHandler(
    filename = "hosp_edad_total.xlsx",
    content = function(filename) {
      writexl::write_xlsx(hosp_edad_total() %>%
                            mutate(
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, " - ", "-"),
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, " y más", "+"),
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, "00", "0")
                            ) %>%
                            na.omit() %>%
                            mutate(sexo = recode(sexo,
                                                 "M" = "Hombres",
                                                 "F" = "Mujeres"
                            )) %>%
                            group_by(fecha, grupo_de_edad) %>%
                            mutate(final = casos[fecha == max(fecha)]) %>%
                            ungroup() %>%
                            mutate(grupo_de_edad = forcats::fct_reorder(grupo_de_edad, final)), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  # Hospitalizados UCI por grupo de edad ----
  
  hosp_edad_uci_g <- reactive({ # grafico
    p <- hosp_edad_uci() %>%
      mutate(
        grupo_de_edad = stringr::str_replace(grupo_de_edad, " - ", "-"),
        grupo_de_edad = stringr::str_replace(grupo_de_edad, " y más", "+"),
        grupo_de_edad = stringr::str_replace(grupo_de_edad, "00", "0")
      ) %>%
      group_by(fecha, grupo_de_edad) %>%
      mutate(final = casos[fecha == max(fecha)]) %>%
      ungroup() %>%
      mutate(grupo_de_edad = forcats::fct_reorder(grupo_de_edad, final)) %>%
      ggplot(aes(fecha, casos,
                 col = grupo_de_edad
      )) +
      geom_line(size = 2) +
      geom_point(size = 4) +
      geom_point_interactive(aes(
        tooltip = stringr::str_wrap(
          paste(
            "Se reportaron", casos, "personas hospitalizadas en UCI",
            "con una edad de", grupo_de_edad, "producto de Covid-19",
            "al", format(fecha, "%d de %B")
          ), 40
        )
      ), size = 4) +
      geom_text_repel(
        aes(
          x = max(fecha),
          y = casos,
          label = ifelse(casos > 0,
                         ifelse(
                           fecha == max(fecha),
                           as.character(paste0(grupo_de_edad, ": ", casos)), ""
                         ),
                         ""
          )
        ),
        hjust = 0,
        nudge_x = 0.3,
        box.padding = unit(1.7, "points"),
        min.segment.length = unit(8, "points"),
        segment.alpha = 0.3,
        size = 5,
        direction = "y"
      ) +
      scale_x_date(
        breaks = seq(
          from = min(hosp_edad_uci()$fecha),
          to = max(hosp_edad_uci()$fecha),
          length.out = 15
        ),
        expand = expansion(mult = c(0, 0.27)),
        date_labels = "%d/%B"
      ) +
      scale_color_manual(
        name = "edades",
        values = rev(degradado7(7))
      ) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 0, b = 6)
        ),
        legend.text = element_text(margin = margin(r = 20)),
        axis.text.y = element_text(margin = margin(l = 5, r = 5))
      ) +
      theme(legend.position = "none") +
      labs(
        subtitle = paste(
          "Casos entre el",
          format(min(hosp_edad_uci()$fecha), "%d de %B"),
          "y el",
          format(max(hosp_edad_uci()$fecha), "%d de %B")
        ),
        caption = "Mesa de datos Covid-19, Hospitalizados por grupo de edad\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Pacientes hospitalizados"
      )
    p
  })
  
  # Out ----
  output$hosp_edad_uci_int <- renderGirafe({
    girafe(
      ggobj = hosp_edad_uci_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$hosp_edad_uci_xlsx <- downloadHandler(
    filename = "hosp_edad_uci.xlsx",
    content = function(filename) {
      writexl::write_xlsx(hosp_edad_uci() %>%
                            mutate(
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, " - ", "-"),
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, " y más", "+"),
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, "00", "0")
                            ) %>%
                            group_by(fecha, grupo_de_edad) %>%
                            mutate(final = casos[fecha == max(fecha)]) %>%
                            ungroup() %>%
                            mutate(grupo_de_edad = forcats::fct_reorder(grupo_de_edad, final)), filename)
    },
    contentType = "application/xlsx"
  )
  
  # Pacientes en UCI por grupo de edad ----
  
  uci_edad_g <- reactive({ # grafico
    p <- uci_edad() %>%
      mutate(
        grupo_de_edad = stringr::str_replace(grupo_de_edad, ">=70", "70+"),
        grupo_de_edad = stringr::str_replace(grupo_de_edad, "<=39", "-39")
      ) %>%
      na.omit() %>%
      ggplot(aes(fecha, casos, col = grupo_de_edad)) +
      geom_line(size = 2) +
      geom_text_repel(
        aes(
          x = max(fecha),
          y = casos,
          label = ifelse(casos > 0,
                         ifelse(
                           fecha == max(fecha),
                           as.character(paste0(grupo_de_edad, " años: ", casos)), ""
                         ),
                         ""
          )
        ),
        hjust = 0,
        nudge_x = 0.3,
        box.padding = unit(1.7, "points"),
        min.segment.length = unit(8, "points"),
        segment.alpha = 0.3,
        size = 5,
        direction = "y"
      ) +
      scale_x_date(
        breaks = seq(
          from = min(uci_edad()$fecha),
          to = max(uci_edad()$fecha),
          by = 1
        ),
        expand = expansion(mult = c(0, 0.27)),
        date_labels = "%d/%B"
      ) +
      scale_color_manual(
        name = "edades",
        values = rev(degradado7(7))
      ) +
      coord_cartesian(clip = "off") +
      tema_lineas +
      ocultar_titulo_x +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          margin = margin(t = 0, b = 6)
        ),
        legend.text = element_text(margin = margin(r = 20)),
        axis.text.y = element_text(margin = margin(l = 5, r = 5))
      ) +
      theme(legend.position = "none") +
      labs(
        subtitle = paste("Casos entre el", format(min(uci_edad()$fecha), "%d de %B"), "y el", format(max(uci_edad()$fecha), "%d de %B")),
        caption = "Mesa de datos Covid-19, Pacientes en UCI por grupo de edad\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación",
        y = "Pacientes UCI"
      )
    p
  })
  
  # Out ----
  output$uci_edad_int <- renderGirafe({
    girafe(
      ggobj = uci_edad_g(),
      # width_svg = 16,
      width_svg = ifelse(dimension_horizontal() < 800, 10, 12), # responsividad horizontal
      height_svg = 7,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        opts_sizing(rescale = TRUE, width = .95),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  #Descarga
  output$uci_edad_xlsx <- downloadHandler(
    filename = "uci_edad.xlsx",
    content = function(filename) {
      writexl::write_xlsx(uci_edad() %>%
                            mutate(
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, ">=70", "70+"),
                              grupo_de_edad = stringr::str_replace(grupo_de_edad, "<=39", "-39")
                            ), filename)
    },
    contentType = "application/xlsx"
  )
  
  
  #— ----
  #MAPAS ----
  
  #Mapa regiones por comunas ----
  # selector región
  observe({
    updateSelectInput(session, "selector_region_mapa_comuna",
                      choices = levels(as.factor(activos_comuna()$region)),
                      selected = "Metropolitana"
    )
  })
  
  # resultado de selector región
  region_mapa_comuna_elegida <- reactive({ as.character(input$selector_region_mapa_comuna) })
  
  #resultado de cifra por graficar
  valor_mapa_regiones <- reactive({ as.character(input$valor_mapa_regiones) })
  
  
  
  #obtener casos activos de comunas
  mapa_activos_comuna_datos <-  reactive({
    
    comunas_casos <- activos_comuna() %>%
      filter(region == region_mapa_comuna_elegida() ) %>% #desde el selector
      filter(fecha==max(fecha)) %>%
      select(codigo_comuna, poblacion, fecha, casos)
    
    if(valor_mapa_regiones()=="tasa") {
      comunas_casos <- comunas_casos %>%
        mutate(casos = round((casos / poblacion) * 100000, digits = 1))
    }
    comunas_casos
  })
  
  
  
  #graficar
  mapa_activos_comuna_g <- reactive({
    #Producir mapa
    mapa1 <- chilemapas::mapa_comunas %>% 
      left_join(
        chilemapas::codigos_territoriales %>% 
          select(matches("comuna"))
      ) %>% 
      left_join(mapa_activos_comuna_datos()) %>% #anexar casos
      filter(!is.na(casos)) %>%
      #graficar
      ggplot(aes(geometry=geometry,
                 fill = casos )) + 
      #geom_sf(col="white") + 
      geom_sf_interactive(col="white",
                          aes(tooltip = paste(nombre_comuna,
                                              "\n",
                                              casos,
                                              ifelse(valor_mapa_regiones()=="casos",
                                                     "casos activos\nal",
                                                     "casos activos por 100 mil habitantes\nal" ),
                                              format(fecha, "%d de %B")))) +
      scale_fill_gradient(high = "#DF1A57",
                          #mid = "#5933cc",
                          #low = "#ded6f5",
                          low = "#f9d2de",
                          na.value = "grey80") +
      coord_sf(expand = FALSE) +
      tema_lineas +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      labs(subtitle = paste(ifelse(region_elegida() == "Metropolitana",
                                   paste("Región Metropolitana"),
                                   ifelse(region_elegida() == "Total",
                                          paste("Datos a nivel nacional"),
                                          paste("Región de", region_elegida()) ) ),
      "\n", format(max(activos_comuna()$fecha), "%d de %B") ),
      caption = "Mesa de datos COVID-19, casos activos por fecha de inicio de síntomas y comuna\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación")
    
    
    
  })
  
  # Out ----
  output$mapa_activos_comuna_int <- renderGirafe({
    girafe(
      ggobj = mapa_activos_comuna_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 8, 10), # responsividad horizontal
      height_svg = 9,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px;"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        #opts_sizing(rescale = TRUE, width = .95),
        opts_sizing(rescale = FALSE),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
  
  #Mapa de Chile ----
  
  #resultado de cifra por graficar
  valor_mapa_pais <- reactive({ as.character(input$valor_mapa_pais) })
  
  mapa_activos_pais_datos <- reactive({
    region_a <- comuna_a %>%
      filter(fecha==max(fecha)) %>%
      group_by(region, codigo_region, fecha) %>%
      summarise(casos=sum(casos),
                poblacion=sum(poblacion)) %>%
      ungroup() %>%
      select(-region) %>%
      filter(!is.na(codigo_region))
      
      if(valor_mapa_pais()=="tasa") {
        region_a <- region_a %>%
          mutate(casos = round((casos / poblacion) * 100000, digits = 1))
      }
    
    region_a
  })
  
  
  mapa_activos_pais_g <- reactive({
    mapa2 <- chilemapas::mapa_comunas %>% 
      #producir mapa
      chilemapas::generar_regiones() %>% 
      left_join(
        chilemapas::codigos_territoriales %>% 
          select(matches("region")) %>% 
          distinct()
      ) %>% 
      left_join(mapa_activos_pais_datos()) %>%
      mutate(nombre_region = str_wrap(nombre_region,width = 20)) %>%
      #graficar
      ggplot(aes(geometry=geometry,
                 fill = casos)) +
      #geom_sf(col="white") + 
      geom_sf_interactive(col="white",
                          aes(tooltip = paste(nombre_region,
                                              "\n",
                                              casos,
                                              ifelse(valor_mapa_pais()=="casos",
                                                     "casos activos\nal",
                                                     "casos activos por cada 100 mil habitantes\nal" ),
                                              format(fecha, "%d de %B")))) +
      #nombre regiones
      geom_sf_text(aes(label=nombre_region),
                   hjust=1,
                   nudge_x = -2) +
      #cifras
      geom_sf_text(aes(label = paste(casos, 
                                     ifelse(valor_mapa_pais()=="casos",
                                            "casos activos",
                                            "casos activos por cada\n100 mil habitantes") )),
                   hjust=0,
                   nudge_x = 2) +
      scale_fill_gradient(high = "#DF1A57",
                          #mid = "#5933cc",
                          #low = "#5933cc",
                          low = "#f9d2de",
                          #low="white",
                          #midpoint=800,
                          na.value = "grey80") +
      coord_sf(xlim = c(-80, -60), 
               #ylim = c(20, 50), 
               expand = FALSE) +
      tema_lineas +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_blank(),
            plot.subtitle = element_blank()) +
      labs(#subtitle = paste(format(max(activos_comuna()$fecha), "%d de %B") ),
           caption = "Mesa de datos COVID-19, casos activos por fecha de inicio de síntomas y comuna\nMinisterio de Ciencia, Tecnología, Conocimiento e Innovación")
    
    
    mapa2
  })
  
  # Out ----
  output$mapa_activos_pais_int <- renderGirafe({
    girafe(
      ggobj = mapa_activos_pais_g(),
      width_svg = ifelse(dimension_horizontal() < 800, 8, 10), # responsividad horizontal
      height_svg = 25,
      options = list(
        opts_tooltip(use_fill = TRUE),
        opts_hover(css = "r: 8px;"),
        opts_selection(css = "r: 8px; stroke:white; stroke-width:2pt;"),
        #opts_sizing(rescale = TRUE, width = .95),
        opts_sizing(rescale = FALSE),
        opts_toolbar(position = "topright", saveaspng = FALSE)
      )
    )
  })
  
})
