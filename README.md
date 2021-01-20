# Visualizador Covid-19 Chile - DATA UC

Todos los datos son obtenidos en tiempo real desde el repositorio de datos de la Mesa de Datos COVID-19 del Ministerio de Ciencia, Tecnología, Conocimiento e Innovación. Pueden presentarse desfases entre la información regional y comunal por las fechas de publicación de informes.


Aplicación desarrollada por DATA UC

Facultad de Matemáticas

Pontificia Universidad Católica de Chile


El objetivo de la presente interfaz es facilitarle a los ciudadanos la comprensión del fenómeno sanitario que afecta al país, a través de la visualización de los datos más relevantes. Esta plataforma busca democratizar el acceso a los datos y presentarlos de forma didáctica para que la población esté plenamente informada y pueda efectuar análisis, comparar la situación en las regiones donde viven y obtener conclusiones relevantes.

## Recursos abiertos
Todos los recursos gráficos aquí presentados se encuentran a disposición para el uso libre por parte de las comunidades científicas, medios de comunicación y la comunidad en general. El desarrollo de la plataforma se realizó usando el [lenguaje de programación estadística R](https://www.r-project.org), la interfaz de desarrollo integrada [RStudio](https://rstudio.com), la colección de paquetes de ciencia de datos [Tidyverse](https://www.tidyverse.org) y el paquete de publicación online de plataformas interactivas de análisis de datos [Shiny](https://shiny.rstudio.com).

### Fuentes de información
Los datos presentados en la plataforma corresponden a información oficial publicada por el Ministerio de Salud, la cual ha sido trabajada y procesada por la Mesa de Datos COVID-19, iniciativa liderada por el Ministerio de Ciencia, Tecnología, Conocimiento e Innovación. El repositorio con los datos en bruto [se encuentran en este enlace](https://github.com/MinCiencia/Datos-COVID19).

Estos datos son incorporados a la plataforma de visualización de modo más eficiente mediante una [API desarrollada por el Equipo Data UC y Mauricio Vargas](https://github.com/pachamaltese/api-covid19-datauc).

El objetivo de esta API es facilitar la consulta de los datos proporcionados por el Ministerio de Ciencia, y en particular su importación mediante el entorno de programación y análisis estadístico R. La API no realiza cambios sobre las cifras reportadas en los datos originales, exceptuando la transformación de los datos hacia la estructura de filas y columnas conocida como _tidy data_ (datos ordenados).

Usando R, la API facilita la importación de productos con una sola línea de código:

`readr::read_csv("https://coronavirus-api.mat.uc.cl/producto1")`

`data.table::fread("https://coronavirus-api.mat.uc.cl/producto1")`

Accede al [repositorio de la API](https://github.com/pachamaltese/api-covid19-datauc) para acceder a mayor información.


## Consideraciones técnicas

La plataforma fue desarrollada utilizando el software R y su aplicación de visualización Shiny. Todos los datos son obtenidos en tiempo real de los repositorios de la Mesa de Datos, por lo que a información se actualiza en función de dichas publicaciones.


## Equipo realizador
**Desarrollo de gráficos e interfaz:** [Bastián Olea Herrera](http://bastian.olea.biz)
                  
**Desarrollo de backend:** [Mauricio Vargas Sepúlveda](https://pacha.dev)

**Dirección y supervisión metodológica:** [Alexis Alvear Leyton](https://www.linkedin.com/in/alexis-alvear-leyton/)
              
Preguntas y contacto: contacto@datauc.cl
          
Data UC es la unidad de transferencia tecnológica de la Facultad de Matemáticas de la Pontificia Universidad Católica de Chile, cuya misión es aportar con el desarrollo de aplicaciones innovadoras en el ámbito de la ciencia de datos que contribuyan con el desarrollo del país y sus habitantes.