shinyUI(fluidPage(
  # Prueba de commit nuevo repo
  # Inactividad ----
  tags$head(tags$script("inactivity.js")),

  # Importar Open Sans
  tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Open Sans:400,600');")),
  # tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Raleway:400,600,700');")),
  tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Oswald:400,800');")),

  # Analytics ----
  tags$head(includeHTML(("google-analytics.html"))),

  # Favicon ----
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),

  tags$head(tags$link(rel = "apple-touch-icon-precomposed", href = "apple-touch-iphone.png")),

  # Definir estilos CSS ----
  tags$h1(
    tags$style("h1{font-family: Oswald; color: white; font-weight:800;}") # títulos h2
  ),
  tags$h2(
    tags$style("h2{font-family: Oswald; color: #fad1de; font-size:10; font-weight:600; !important}") # títulos h2
  ),
  tags$h3(
    tags$style("h3{font-family: Oswald; color: #891036; font-weight:600;}") # títulos h3
  ),
  tags$h4(
    tags$style("h4{font-family: Oswald; font-size: 17pt;  font-weight:500;}") # títulos h4
  ),
  tags$a(
    tags$style("a{font-family: Open Sans; font-weight:400, color: #df1a57; !important}") # Todo el texto body
  ),
  tags$head(
    tags$style("* {font-family: Open Sans; font-weight:400}") # Todo el texto body
  ),


  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #fce8ef;  color: black}
    .tabbable > .nav > li[class=active]    > a {background-color: #df1a57; font-weight: 600; color: #fad1de}
  ")),
  
  #COLORES
  #rosado: fce8ef
  #rojizo: df1a57


  # estilo del selector de preguntas (selectInput)
  tags$style(type = "text/css", "
      .selectize-input {font-size: 10pt; color: white; !important}
      .selectize-input.input-active {font-size: 10pt; color: black; !important}
      .selectize-input.items {background-color: #df1a57; border-color: #df1a57; !important}
      .selectize-input.items:hover {background-color: #df1a57; border-color: #df1a57; !important}
      .selectize-control.single .selectize-input:after {border-color: white transparent transparent transparent; !important}
      "), # texto del selector, texto del selector cuando está desplegado, fondo del selector, fondo hover, color del triangulo
  tags$style(type = "text/css", "
      .selectize-dropdown {font-size: 10pt; color: black; background-color: white; !important}
      .selectize-dropdown .active {color: white; background: #df1a57 !important;}
      "), # elementos del desplegable, hover del desplegable
  
  
  # Estilo de botón de descarga
  tags$style(type = "text/css", "
      .descargar        {background-color: #fce8ef; border-color: #fce8ef; font-size: 8pt; color: black;}
      .descargar:hover  {background-color: #df1a57; border-color: #df1a57; font-size: 8pt; color: white;}
      .descargar.active {background-color: #fce8ef; border-color: #fce8ef; font-size: 8pt; color: black;}"),
  


  # Detector de ancho de pantalla
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),

  # Ocultar errores
  tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  tags$head(tags$style(".shiny-output-error:after{content: 'Error';
visibility: hidden}")),


  # Application title
  titlePanel(
    title = NULL,
    # style = "background-color: #f4a4bd;",
    windowTitle = "Visualizador Covid-19 Chile - DATA UC"
  ),

  # Header ----
  fluidRow(
    column(12,
      style = "background-color: #df1a57;", # f4a4bd;",
      column(
        9,
        h1(HTML("Visualizador Covid-19 Chile "), #&nbsp;
           #icon("clock"),
           img(src="virus_blanco.png", width=40,
               style = "padding-bottom: 10px",),
           style = "padding:10px;"),
        # h1("Visualizador Covid-19 Chile",
        #   style = "padding:10px;"
        # ),
        # h2("Cifras de Coronavirus en Chile en tiempo real",
        #    style="padding:10px;"),
        # br(),
      ),
      column(
        3, align="center", # style = "background-color: #f4a4bd;",
        tags$a(img(
          src = "logodatauc2.png",
          width = 200, style = "padding: 30px",
          align="center"
        ), # style="padding:60px;"),
        href = "http://www.mat.uc.cl"
        ),
        # br(), br(),
      ),
    ),
  ),

  # Sidebar----
  fluidRow(
    column(3,
      style = "background-color: #fce8ef;",
      br(),

      # Información de región:


      h4("Cifras generales ", icon("heartbeat")),
      div(style = "display: inline-block;", em("Última actualización: ")),
      div(style = "display: inline-block;", textOutput("fecha_maxima_region_format")),
      br(), br(),


      div(style = "display: inline-block;", strong("Casos totales confirmados: ")),
      div(style = "display: inline-block;", textOutput("casos_total")),
      br(), br(),

      div(style = "display: inline-block;", strong("Casos Región Metropolitana: ")),
      div(style = "display: inline-block;", textOutput("casos_rm")),
      br(), br(),

      conditionalPanel(
        condition = "input.selector_region != 'Total'",
        div(style = "display: inline-block;", strong("Casos regionales: ")),
        div(style = "display: inline-block;", textOutput("casos_region")),
        div(style = "display: inline-block;", textOutput("region_elegida_p")),
        br(), br(),
      ),

      # div(style = "display: inline-block;", strong("Casos recuperados totales: ")),
      # div(style = "display: inline-block;", textOutput("casos_recuperados")),
      # br(),br(),

      div(style = "display: inline-block;", strong("Casos fallecidos totales: ")),
      div(style = "display: inline-block;", textOutput("casos_fallecidos")),
      br(), br(),


      # div(style = "display: inline-block;", strong("Exámenes PCR realizados en total: ")),
      # div(style = "display: inline-block;", textOutput("total_examenes"), p("desde el 9 de abril")),
      # br(),br(),

      strong("Exámenes PCR realizados en total: "),
      div(style = "display: inline-block;", textOutput("total_examenes")),
      br(), br(),

      div(style = "display: inline-block;", strong("Pacientes actualmente hospitalizados en UCI: ")),
      div(style = "display: inline-block;", textOutput("total_hospitalizados")),
      br(), br(),

      div(style = "display: inline-block;", strong("Región con más casos: ")),
      div(style = "display: inline-block;", textOutput("casos_top_region")),
      br(), br(),

      div(style = "display: inline-block;", strong("Región con más casos (exceptuando la RM): ")),
      div(style = "display: inline-block;", textOutput("casos_top_region_no_rm")),
      br(), br(),

      div(style = "display: inline-block;", strong("Región con menos casos: ")),
      div(style = "display: inline-block;", textOutput("casos_min_region")),
      br(), br(),

      div(style = "display: inline-block;", strong("Comuna con más casos: ")),
      div(style = "display: inline-block;", textOutput("o_casos_top_comuna")),
      br(), br(),
      
      div(align = "center", img(src="virus_rojizo.png", width=40)),
      br(),
      


      p("Todos los datos son obtenidos en tiempo real desde el repositorio de datos de la", em("Mesa de Datos COVID-19"), "del Ministerio de Ciencia, Tecnología, Conocimiento e Innovación."),
      p("Pueden presentarse desfases entre la información regional y comunal por las fechas de publicación de informes."),

      br(),
      p("Aplicación desarrollada por DATA UC"),
      p("Facultad de Matemáticas"),
      p("Pontificia Universidad Católica de Chile"),
      br()
    ),
    # — ----
    # Gráficos ----
    column(
      9,
      fluidRow(
        column(
          12,
          br(),
          # Breve presentación
          p("Esta aplicación presenta de forma gráfica la información relativa a la evolución de la pandemia del Coronavirus COVID-19 en Chile, a partir del repositorio de la Mesa de Datos liderada por el Ministerio de Ciencia, Tecnología, Conocimiento e Innovación. Seleccione una sección para explorar los datos."),
          # br(),
          br(),
          # Pestañas
          tabsetPanel(
            type = "pills",

            # PESTAÑA 1: Generales ----
            tabPanel(
              "Resumen nacional",
              ####
              br(),
              # p("Esta sección contiene gráficos con los últimos datos sobre defunciones por Covid-19, pacientes hospitalizados en camas UCI o críticas producto del Coronavirus, exámenes PCR realizados para diagnosticar Covid-19 en Chile, y la cantidad oficial de personas recuperadas reportadas por la institucionalidad de salud."),
              p("En esta sección se presentan gráficos de estadísticas generales de la pandemia en el país, contemplando últimos datos sobre defunciones por Covid-19, pacientes hospitalizados en camas UCI o críticas producto del Coronavirus, exámenes PCR realizados para diagnosticar Covid-19 en Chile, y la cantidad oficial de personas recuperadas reportadas por la institucionalidad de salud."),
              # br(),

              # Totales nacionales ----
              h3("Resumen de datos totales nacionales de Covid-19"),
              p("En el presente gráfico se presentan estadísticas generales consolidadas para tu análisis: evolución de contagios confirmados; casos activos (que corresponden a personas vivas  confirmadas cuya fecha de inicio de síntomas en la notificación es menor o igual a 14 días a la fecha del reporte actual); casos recuperados (estimaciones, según criterio del MINSAL); casos nuevos (confirmados por exámenes hasta las 21 horas del día anterior a esta publicación) y evolución de defunciones por COVID-19. Los casos totales corresponden a la suma de los casos activos, recuperados y fallecidos. Los casos nuevos se encuentran integrados en la cifra de casos activos."),
              girafeOutput("g_totales_nacionales_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("g_totales_nacionales_xlsx", "Descargar datos", class="descargar")),
              br(),

              # Evolución de casos  ----
              h3("Evolución de casos activos de Covid-19"),
              p("En el presente gráfico se observa la evolución de los casos de contagios 
                por Coronavirus confirmados por exámenes de laboratorio y notificados por 
                el sistema de vigilancia epidemiológica EPIVIGILA del Ministerio de Salud. Esta corresponde a la famosa curva de contagios,
                que considera sólo a las personas activamente afectadas por el virus, y que debería disminuir a medida que la pandemia disminuya."),
              # girafeOutput("g_total_int", width = "100%", height = "100%") %>%
              #   withSpinner(type = 7, size = 1, color = "#fce8ef"),
              girafeOutput("casos_activos_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("casos_activos_total_xlsx", "Descargar datos", class="descargar")),
              br(),


              # Casos nuevos ----
              h3("Casos nuevos de Covid-19"),
              p("Este gráfico indica la cantidad de casos nuevos de contagio de Covid-19 en cada comuna de Chile. Para elegir una comuna, selecciónela en el selector de la parte superior."),
              girafeOutput("g_total_nuevos_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("g_total_nuevos_xlsx", "Descargar datos", class="descargar")),
              br(),


              # Casos por genero y edad ----
              h3("Casos por género y grupo de edad"), # 16
              p("Los siguientes gráficos indican las curvas de evolución de casos totales, separadas por género y por edad."),
              girafeOutput("casos_genero_edad_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("casos_genero_edad_xlsx", "Descargar datos", class="descargar")),
              br(),
              
              #Fallecidos en total ----
              h3("Personas fallecidas en total por Covid-19"),
              p("Este gráfico indica la evolución de fallecimientos producidos por Covid-19 a nivel total en el país."),
              girafeOutput("fallecidos_totales_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("g_fallecidos_total_xlsx", "Descargar datos", class="descargar")),
              br(),


              # Fallecidos por edad ----
              h3("Personas fallecidas por Covid-19 según edad"),
              p("El presente gráfico presenta de forma jerarquizada la evolución del número de fallecidos por Coronavirus, y su distribución por rango etario."),
              girafeOutput("g_fallecidos_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("g_fallecidos_xlsx", "Descargar datos", class="descargar")),
              br(),

              # Letalidad ----
              h3("Tasa de letalidad del Covid-19"),
              p("Este gráfico indica la evolución de la tasa de letalidad del virus; es decir, la proporción diaria de defunciones asociadas al Covid-19 entre las y los afectados por la enfermedad."),
              girafeOutput("g_letalidad_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("g_letalidad_xlsx", "Descargar datos", class="descargar")),
              br(),


              ## Personas fallecidas por grupo de edad (Producto 9) ?????


              # Hospitalizados por región ----
              h3("Casos de pacientes hospitalizados por Covid-19 en camas UCI por región"),
              p("El siguiente gráfico presenta la evolución de pacientes hospitalizados en camas críticas en cada una de las regiones del país, exceptuando la región Metropolitana."),
              girafeOutput("g_hospitalizados_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("g_hospitalizados_xlsx", "Descargar datos", class="descargar")),
              br(),
              # br(),


              # br(),

              # Casos de COVID-19 por región
              # h3("Casos de Covid-19 recuperados"),
              #
              # girafeOutput("g_recuperados_int", width = "100%", height = "100%") %>%
              #   withSpinner(type = 7, size = 1, color = "#fce8ef"),
              # #br(),
              br()
              ####
            ),

            # — ----

            # PESTAÑA 2: Región ----
            tabPanel(
              "Regiones",
              ####
              br(),
              # p("Esta sección contiene datos generales sobre el Coronavirus en Chile: casos totales de infección, casos desagregados por región, y finalmente una visión acumulada que compara los casos de Coronavirus de la Región Metropolitana con el resto de las regiones del país."),
              p("En esta sección podrás visualizar los datos sobre la evolución de los contagios por Coronavirus en Chile, a nivel país y desagregados por región. 
                A continuación, selecciona la región de tu interés para conocer la información local."),
              br(),

              # (selector región)
              p("Selecciona una región de la lista para adaptar los gráficos de esta sección a tu región de interés."),
              selectInput("selector_region",
                width = "70%",
                label = NULL,
                choices = NULL,
                # choices = "Cargando...",#covid_region %>% select(Region)
                selected = NULL
              ),

              div(style = "display: inline-block;", strong("Región seleccionada: ")),
              div(style = "display: inline-block;", textOutput("region_elegida_explicacion")),
              br(),


              # Casos por región -----
              h3("Casos de Covid-19 por región"),
              p("En el siguiente gráfico se presentan las curvas de contagios confirmados comparativas por región. Seleccione una región en el selector para destacarla dentro del gráfico."),

              girafeOutput("g_regiones_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("regiones_xlsx", "Descargar datos", class="descargar")),
              br(),
              


              # Casos acumulados ----
              h3("Casos acumulados de Covid-19"),
              p("En el siguiente gráfico se presenta la información de casos confirmados acumulados desde el 3 de marzo de 2020, fecha de notificación del primer caso en Chile, y su distribución entre la región Metropolitana y el resto de las regiones del país."),

              girafeOutput("g_acumulado_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("regiones_acumulado_xlsx", "Descargar datos", class="descargar")),
              br(),


              # Regiones tabla ----
              h3("Regiones de Chile con más casos de Covid-19"),
              p("En la siguiente tabla se presenta el ranking de las regiones del país con más casos confirmados de COVID-19, información que se complementa con la tasa de contagios por cada 100 mil habitantes."),
              formattableOutput("t_casos_top_10_region") %>% 
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("regiones_ranking_xlsx", "Descargar datos", class="descargar")),
              br(),
              
              #Regiones casos nuevos ----
              h3("Casos nuevos de Covid-19 por región"),
              p("Seleccione una región en el selector de más arriba para visualizar los casos nuevos de su región de interés."),
              
              girafeOutput("g_reg_nuevos_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("regiones_nuevos_xlsx", "Descargar datos", class="descargar")),
              br(),

              # Exámenes ----
              h3("Exámenes PCR para Covid-19"),
              p("En el siguiente gráfico se presenta la distribución de Exámenes PCR (Reacción en cadena de la polimerasa) realizados por laboratorios en cada una de las regiones del país."),
              girafeOutput("g_examenes_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("regiones_examenes_xlsx", "Descargar datos", class="descargar")),
              br(),


              #### tasa de incidencia por regiones histórica (producto 15)


              # Pacientes en UCI por región (Producto 7)

              # Fallecidos por región ----
              h3("Personas fallecidas por Covid-19 por región"),
              p("Seleccione una región del selector para visualizar su curva de personas fallecidas debido a Covid-19"),
              girafeOutput("g_fallecidos_region_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("regiones_fallecidos_xlsx", "Descargar datos", class="descargar")),
              br(),
              


              # Fallecidos por región incremental
              h3("Fallecidos por región incremental"),
              p("Este gráfico indica la evolución de las cantidades de personas fallecidas a través de todas las regiones del país, permitiendo una mirada generalizada del efecto de la pandemia."),
              girafeOutput("fallecidos_region_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("fallecidos_region_inc_xlsx", "Descargar datos", class="descargar")),
              br(),


              # h3("Regiones de Chile con menos casos de Covid-19"),
              # p("En el siguiente ranking se encuentran las regiones del país con menor número de contagiados confirmados de COVID-19, y a su vez la tasa de incidencia respectiva. "),
              # formattableOutput("t_casos_top_10_min_region") %>% withSpinner(type = 7, size = 1, color = "#fce8ef"),
              # br(),
              ####
            ),
            # — ----

            # PESTAÑA 3: Comuna ----
            tabPanel(
              "Comunas",
              ####
              br(),
              # p("Esta sección expone datos de Coronavirus desagregados según las comunas de la región seleccionada."),
              p("En esta sección podrás revisar gráficos descriptivos con la información desagregada a nivel comunal. La información aquí presentada puede tener un desfase respecto del consolidado de casos a nivel regional, debido a los plazos en los cuales se disponen los datos en el repositorio abierto."),
              # br(),

              # Casos según comuna ----
              h3("Casos de Covid-19 según comuna"),
              p("En este gráfico podrás conocer las estadísticas de casos acumulados a nivel comunal, desde la fecha del primer confirmado en alguna de ellas. 
                Esta información se determina a partir de la declaración de residencia habitual de los pacientes confirmados. 
                Selecciona en el siguiente selector la región de tu interés para revisar hasta 8 de sus comunas (las con mayor cantidad de casos)."),
              # (selector)
              strong("Elija una región para ver la evolución de casos en sus comunas:"),
              selectInput("selector_region_g_comuna",
                width = "70%",
                label = NULL,
                choices = NULL,
                selected = NULL
              ),
              girafeOutput("grafico_comunas_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("grafico_comunas_xlsx", "Descargar datos", class="descargar")),
              br(),

              
              
              # Tabla comunas con más casos ----
              h3("Lista de comunas de Chile con más casos de Covid-19"),
              p("En la siguiente tabla se presenta el ranking de las 15 comunas del país con más casos confirmados de COVID-19, información que se complementa con la tasa de contagios por cada 100 mil habitantes, que permite observar la proporción de contagios con respecto a su población. "),
              formattableOutput("t_casos_top_comuna") %>% 
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("comunas_ranking_xlsx", "Descargar datos", class="descargar")),
              br(),




              # Nuevos por comuna ----
              h3("Casos nuevos por comuna"), # 15
              p("Este gráfico expresa la evolución de casos nuevos de acuerdo a la fecha en que expresan sus primeros síntomas;
              es decir, muestrra la cantidad de personas con manifestación clínica confirmada de la enfermedad.
              En este sentido, la información se entrega por semanas epidemiológicas y los datos van siendo actualizados
              retroactivamente a medida que se confirman casos y evoluciona la investigación epidemiológica."),
              p("Selecciona una región para luego elegir la comuna que deseas visualizar."),
              # (selector region)
              strong("Primero, elija una región:"),
              selectInput("selector_region_nuevos_comuna",
                width = "70%",
                label = NULL,
                choices = NULL,
                selected = NULL
              ),
              # (selector comuna)
              strong("Luego, elija una comuna para ver su evolución de casos nuevos"),
              selectInput("selector_comuna_nuevos_comuna",
                width = "70%",
                label = NULL,
                choices = NULL,
                selected = NULL
              ),
              girafeOutput("nuevos_comuna_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("covid_nuevos_comuna_xlsx", "Descargar datos", class="descargar")),
              br(),
              
              
              
              
              # Tabla comunas con más casos nuevos por semana ----
              h3("Lista de comunas de Chile con más casos nuevos por semana"), #15
              p("La siguiente tabla presenta el ranking de las 15 comunas del país 
                que han presentado una mayor cantidad de casos nuevos casos confirmados de COVID-19, 
                de acuerdo a los datos semanales entregados por los informes de epidemiología del Ministerio de Salud. Estos
                datos se van actualizando retroactivamente a medida que evoluciona la investigación epidemiológica."),
              formattableOutput("tabla_nuevos_comuna") %>% 
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("comunas_nuevos_ranking_xlsx", "Descargar datos", class="descargar")),
              br(),

              
              
              
              # Tabla comunas aumento ----
              h3("Lista de comunas de Chile con mayor aumento de casos"),
              p("Esta tabla indica en color azul el aumento de casos de las 15 comunas con mayor aumento dentro de la región seleccionada.
                El aumento de casos se mide comparando los casos del último informe epidemiológico con los del informe anterior. Por ejemplo, un aumento de 2 significa que los casos aumentaron en 2 entre el informe anterior y el más reciente."),
              br(),
              textOutput("casos_totales_comuna_informes"),
              br(),
              # (selector region)
              strong("Elija una región:"),
              selectInput("selector_tabla_comunas_aumento",
                          width = "70%",
                          label = NULL,
                          choices = NULL,
                          selected = NULL
              ),
              formattableOutput("tabla_comunas_aumento") %>% 
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("comunas_aumento_ranking_xlsx", "Descargar datos", class="descargar")),
              br(),


              
              # Casos activos por comuna ----
              h3("Casos activos de Covid-19 por comuna"), # 19
              p("Elie una región y luego una comuna para visualizar la evolución de la curva de casos
              activos de Covid-19 en tu comuna de interés. En base al resultado de la investigación epidemiológica,
              se consideran como casos activos durante los primeros 14 días después de la fecha de inicio de sus síntomas"),
              # (selector region)
              strong("Primero, elija una región:"),
              selectInput("selector_region_activos_comuna",
                width = "70%",
                label = NULL,
                choices = NULL,
                selected = NULL
              ),
              # (selector comuna)
              strong("Luego, elija una comuna para ver su evolución de casos activos:"),
              selectInput("selector_comuna_activos_comuna",
                width = "70%",
                label = NULL,
                choices = NULL,
                selected = NULL
              ),
              girafeOutput("activos_comuna_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("casos_activos_comuna_xlsx", "Descargar datos", class="descargar")),
              br(),
              
              
              
              
              
              # Tabla comunas con más casos activos ----
              h3("Lista de comunas de Chile con más casos activos"), #19
              p("Los casos representados en esta tabla corresponden a las comunas del país con amayor cantidad
              de personas que son capaces de contagiar el Covid-19 según los resultados de investigación
              epidemiológica del Ministerio de Salud. 
               Los casos de Covid-19 se consideran activos durante los primeros 14 días después de la fecha de inicio de sus síntomas clínicos."),
              formattableOutput("tabla_casos_activos_comuna") %>% 
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("comunas_activos_tabla_xlsx", "Descargar datos", class="descargar")),
              br(),
              
              

              # Ranking casos y tasa de contagios por comuna ----
              h3("Comunas con mayor tasa de incidencia de Covid-19 según región"),
              p("Seleccione una región en en el siguiente selector para ver el ranking de comunas en términos de casos de Covid-19 y tasa de incidencia. 
                En el siguiente gráfico podrás conocer el total de casos confirmados y las tasas de incidencia del virus en las 10 comunas con mayor tasa, que corresponde al número de casos confirmados por cada 100 mil habitantes. (Ver notas técnicas)."),
              # (selector)
              strong("Elija una región para ver el ranking de comunas:"),
              selectInput("selector_region_g_comuna_tasa",
                width = "70%",
                label = NULL,
                choices = NULL,
                selected = NULL
              ),
              girafeOutput("rank_casos_tasa_comuna_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("rank_casos_tasa_comuna_xlsx", "Descargar datos", class="descargar")),
              br(),

              
              
              
              
          
              # Ranking comunas con mas casos ----
              h3("Comunas de Chile con mayor cantidad de casos de Covid-19"),
              p("El siguiente gráfico ordena las 10 comunas del país con mayor cantidad casos confirmados, e indica también las tasas de incidencia del virus."),
              girafeOutput("rank_casos_comuna_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("rank_casos_comuna_xlsx", "Descargar datos", class="descargar")),
              br(),


              
              
              
              # Comunas con mayor tasa ----
              h3("Comunas de Chile con mayor tasa de incidencia por Covid-19"),
              p("En el presente gráfico se presenta un ranking de las 10 comunas con mayor tasa de incidencia a nivel nacional. La tasa de incidencia (contagios por cada 100 mil habitantes) permite medir el impacto del virus de forma proporcional con respecto a sus poblaciones."),
              girafeOutput("comuna_tasa_ranking_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("comuna_tasa_ranking_xlsx", "Descargar datos", class="descargar")),
              br(),
              # br(),

              ####
            ),

            # — ----

            # Pestaña 4: Hospitalizaciones ----
            tabPanel(
              "Hospitalizaciones",


              ## Exámenes PCR acumulado por establecimiento (14)


              # Pacientes hospitalizados según cama ----
              h3("Pacientes hospitalizados"),
              p("A continuación, se entrega una visión general del sistema de salud chileno a partir
                de datos diarios sobre
                la cantidad de pacientes en camas Básicas, Media, UCI o UTI, 
                reportados por la Unidad de Gestión de Camas Críticas del Ministerio de Salud."),
              girafeOutput("hosp_integrado_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("hosp_integrado_xlsx", "Descargar datos", class="descargar")),
              br(),
              

              # Hospitalizados por edad y género ----
              h3("Hospitalizados totales por grupo de edad y género"),
              p("Las dos gráficas siguientes presentan la cantidad total de personas hospitalizadas
                desagregadas por género y tramo de edad, entregando una descripción sociodemográfica de los efectos
                del Covid-19 en la población."),
              girafeOutput("hosp_edad_total_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("hosp_edad_total_xlsx", "Descargar datos", class="descargar")),
              br(),
              

              # Pacientes UCI por grupo de edad ----
              h3("Pacientes UCI por grupo de edad"), # 9
              p("El siguiente gráfico desagrega por gruops de edad la evolución de pacientes en Unidad de Cuidados Intensivos (UCI)."),
              girafeOutput("uci_edad_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("uci_edad_xlsx", "Descargar datos", class="descargar")),
              br(),
              

              # Pacientes criticos ----
              h3("Pacientes críticos debido a Covid-19"), # 23
              p("A continuación se expone la evolución de pacientes en condición crítica producida por Covid-19 a nivel nacional."),
              girafeOutput("pacientes_criticos_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("pacientes_criticos_xlsx", "Descargar datos", class="descargar")),
              br(),


              # Ventiladores ----
              h3("Ventiladores mecánicos a nivel nacional"), # 20
              p("Este gráfico entrega la cantidad de ventiladores mecánicos disponibles, en comparación con la cantidad de ventiladores actualmente ocupados por pacientes médicos. 
                Esta información entrega un indicador sobre la evolución de la capacidad de respuesta clínica que tiene el país ante la pandemia."),
              girafeOutput("ventiladores_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("ventiladores_xlsx", "Descargar datos", class="descargar")),
              br(),
              

              # Hospitalizados UCI por grupo de edad ----
              h3("Hospitalizados UCI por grupo de edad"), # 22
              p("El siguiente gráfico desagrega la evolución de pacientes UCI de acuerdo a sus edades, 
                indicando los cohortes de edad que actualmente se ven afectados de forma más grave a nivel nacional."),
              girafeOutput("hosp_edad_uci_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              div(align = "right", downloadButton("hosp_edad_uci_xlsx", "Descargar datos", class="descargar")),
              br(),


              ### síntomas declarados (18)
            ),

            
            # — ----
            #Pestaña 4,5: Prueba ----
            tabPanel(
              "prueba",
              
              strong("Elija una región para analizar:"),
              selectInput("selector_region_scatter",
                          width = "70%",
                          label = NULL,
                          choices = NULL,
                          selected = NULL
              ),
              girafeOutput("grafico_scatter_comuna_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              br(),
              
            ),
            
            # Pestaña 5: Análisis ----
            tabPanel(
              "Análisis",
              br(),
              #Mapa regiones por comuna ----
              h3("Mapa de casos activos por comunas"),
              p("Seleccione una región para obtener un mapa de calor de los casos activos de Covid-19 en cada una de sus comunas, 
                donde la intensidad del color indica la cantidad o tasa de casos activos."),
              p("Toque o pase el cursor sobre las comunas para ver sus nombres y cantidad de casos activos"),
              strong("Elija una región para graficar el mapa:"),
              selectInput("selector_region_mapa_comuna",
                          width = "70%",
                          label = NULL,
                          choices = NULL,
                          selected = NULL
              ),
              p("Utilice los botones para elegir si graficar los casos activos o la tasa de casos por cada 100 mil habitantes 
                (que permite una mejor comparación entre regiones ya que se ajusta a las diferencias de población)."),
              radioButtons("valor_mapa_regiones", "Cifra para graficar:",
                           choiceNames = c("Casos activos por comuna",
                                           "Tasa de activos por cada 100 mil habitantes"),
                           choiceValues = c("casos", 
                                            "tasa"),
                           selected = c("Casos activos por comuna" = "casos")),
              
              girafeOutput("mapa_activos_comuna_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              br(),
              
              #Mapa país ----
              h3("Mapa nacional de casos activos"),
              p("El siguiente mapa presenta la totalidad edl territorio nacional según sus casos de Covid-19. 
                Los botones que están a continuación le permiten graficar casos activos de Covid-19 por región, o bien la tasa de casos activos por cada 100 mil habitantes."),
              radioButtons("valor_mapa_pais", "Cifra para graficar:",
                           choiceNames = c("Casos activos por comuna",
                                           "Tasa de activos por cada 100 mil habitantes"),
                           choiceValues = c("casos", 
                                            "tasa"),
                           #selected = c("Casos activos por comuna" = "casos")),
                           selected = c("Tasa de activos por cada 100 mil habitantes" = "tasa")),
              girafeOutput("mapa_activos_pais_int", width = "100%", height = "100%") %>%
                withSpinner(type = 7, size = 1, color = "#fce8ef"),
              br(),
              
              
              
              
            ),
            # — ----

            # Pestaña 5: Notas ----
            tabPanel(
              "Notas técnicas",
              br(),


              h3("Somos"),
              p("Data UC es la unidad de transferencia tecnológica de la Facultad de Matemáticas de la Pontificia Universidad Católica de Chile, cuya misión es aportar con el desarrollo de aplicaciones innovadoras en el ámbito de la ciencia de datos que contribuyan con el desarrollo del país y sus habitantes. "),

              h3("Objetivo"),
              p("El objetivo de la presente interfaz es facilitarle a los ciudadanos la comprensión del fenómeno sanitario que afecta al país, a través de la visualización de los datos más relevantes. Esta plataforma busca democratizar el acceso a los datos y presentarlos de forma didáctica para que la población esté plenamente informada y pueda efectuar análisis, comparar la situación en las regiones donde viven y obtener conclusiones relevantes."),

              h3("Recursos abiertos"),
              p("Todos los recursos gráficos aquí presentados se encuentran a disposición para el uso libre por parte de las comunidades científicas, medios de comunicación y la comunidad en general. "),

              h3("Fuentes de información"),
              HTML("<p>Los datos presentados en la plataforma corresponden a información 
                oficial publicada por el Ministerio de Salud, la cual ha sido trabajada 
                y procesada por la Mesa de Datos COVID-19, iniciativa liderada por el 
                Ministerio de Ciencia, Tecnología, Conocimiento e Innovación. El repositorio con 
                los datos en bruto <a href='https://github.com/MinCiencia/Datos-COVID19' style='color: #df1a57'>se encuentran en este enlace.</a></p>"),
              HTML("<p>Estos datos son incorporados a la plataforma de visualización 
              de modo más eficiente mediante una <a href='https://github.com/pachamaltese/api-covid19-datauc' style='color: #df1a57'>API desarrollada por el Equipo Data UC y Mauricio Vargas</a>.
                   El objetivo de esta API es facilitar la consulta de los datos proporcionados por el 
                   Ministerio de Ciencia, y en particular su importación mediante el entorno de programación y análisis estadístico R.
                   La API no realiza cambios sobre las cifras reportadas en los datos originales, exceptuando la transformación de los datos
                   hacia la estructura de filas y columnas conocida como <a href='<a href='https://github.com/pachamaltese/api-covid19-datauc' style='color: #df1a57'><em>tidy data</em> (datos ordenados).</a></p>"),
              p("Usando R, la API facilita la importación de productos con una sola línea de código:"),

              code('readr::read_csv("https://coronavirus-api.mat.uc.cl/producto1")'),
              code('data.table::fread("https://coronavirus-api.mat.uc.cl/producto1")'),
              br(), br(),
              HTML("<p>Accede al <a href='https://github.com/pachamaltese/api-covid19-datauc' style='color: #df1a57'>repositorio de la API</a> para acceder a mayor información.</p>"),


              h3("Consideraciones técnicas"),
              p("La plataforma fue desarrollada utilizando el software R y su aplicación de visualización Shiny. 
Todos los datos son obtenidos en tiempo real de los repositorios de la Mesa de Datos, por lo que a información se actualiza en función de dichas publicaciones.  "),

              h3("Equipo realizador"),
              div(style = "display: inline-block;", em("Desarrollo de gráficos e interfaz: ")),
              div(
                style = "display: inline-block;",
                tags$a(
                  href = "http://bastian.olea.biz",
                  "Bastián Olea Herrera",
                  style = "color: #df1a57;"
                ),
              ),
              br(),
              div(style = "display: inline-block;", em("Desarrollo de backend: ")),
              div(
                style = "display: inline-block;",
                tags$a(
                  href = "https://pacha.dev",
                  "Mauricio Vargas Sepúlveda",
                  style = "color: #df1a57;"
                ),
              ),
              br(),
              div(style = "display: inline-block;", em("Dirección y supervisión metodológica: ")),
              div(
                style = "display: inline-block;",
                tags$a(
                  href = "https://www.linkedin.com/in/alexis-alvear-leyton/",
                  "Alexis Alvear Leyton",
                  style = "color: #df1a57;"
                ),
              ),
              br(),
              br(),
              # h3("Preguntas y contacto:"),
              # tags$a(href="mailto:contacto@datauc.cl",
              #        "contacto@datauc.cl",
              #        style="color: black;")

              # p("Esta sección contiene los rankings diarios de regiones y comunas del país con más casos."),
              # br(),
            )
          )
        ),
        br(),
        br(),
        column(12,
          align = "center",
          br(),

          p("Plataforma desarrollada por el equipo DATA UC, usando R Shiny"),
          # p("Desarrollo: Bastián Olea Herrera"),
          # p("Supervisión metodológica: Alexis Alvear Leyton"),
          div(style = "display: inline-block;", em("Desarrollo: ")),
          div(
            style = "display: inline-block;",
            tags$a(
              href = "http://bastian.olea.biz",
              "Bastián Olea Herrera,",
              style = "color: #df1a57;"
            ),
          ),
          # #div(style = "display: inline-block;", em("Desarrollo: ")),
          # div(
          #   style = "display: inline-block;",
          #   tags$a(
          #     href = "https://pacha.dev",
          #     "Mauricio Vargas Sepúlveda",
          #     style = "color: #df1a57;"
          #   ),
          # ),
          br(),
          div(style = "display: inline-block;", em("Supervisión metodológica: ")),
          div(
            style = "display: inline-block;",
            tags$a(
              href = "https://www.linkedin.com/in/alexis-alvear-leyton/",
              "Alexis Alvear Leyton",
              style = "color: #df1a57;"
            ),
          ),
          p("Facultad de Matemáticas"),
          p("Pontificia Universidad Católica de Chile"),
          tags$a(img(
            src = "logodatauc.png",
            width = 200, style = "padding: 10px"
          ),
          href = "http://www.mat.uc.cl"
          )
        )
      ),
    ), # end row 2
  ), # end sidebar
))
