library(shiny) #devtools::install_github("rstudio/shiny")    not
library(shinydashboard) #devtools::install_github("rstudio/shinydashboard")    ok
library(shinyWidgets) #devtools::install_github('dreamRs/shinyWidgets') ok
library(digest)  #install.packages("digest") - sha1    ok
library(shinysky) #devtools::install_github("AnalytixWare/ShinySky")    ok
library(dplyr)
library(DT)
library(RMariaDB) #install.packages("RMariaDB")   ok
library(lubridate)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinyalert) #install.packages('shinyalert')
library(tibble)
library(stringi)  #install.packager('stringi')
#library(rhandsontable) #install.packager('rhandsontable)
# library(ggmap) #distancia entre cps

# #poner text input lado a lado
# textInputRow<-function (inputId, label, value = "") 
# {
#      div(style="display:inline-block",
#          tags$label(label, `for` = inputId), 
#          tags$input(id = inputId, type = "text", value = value, class="input-small"))
# }

shinyUI(
dashboardPage(skin = "blue",
              dashboardHeader(title ="IntelRecruit v.2.0",titleWidth = 200),
              dashboardSidebar(width = 200,
                   sidebarMenuOutput("menu.login"),
                   sidebarMenuOutput("menu.logged")
                        ),
              dashboardBody(
                   fluidPage(
                             useShinyalert(),
                   tabItems(
                        tabItem("portada",
                                imageOutput("imagen.inicio",width = "600px")),
                        tabItem("abc-registro",
                                box(title = "REGISTRAR AVANCE", collapsible = T,
                                    collapsed = F, width = 4,
                                    textOutput("ultimo.proceso"),
                                    splitLayout(cellWidths = c("50%","50%"),
                                                pickerInput("Cproceso","Proceso","",
                                                            options = list('dropupAuto' = T, 'mobile'=T)),
                                                dateInput("CFecha","Fecha",value = Sys.Date(),max = Sys.Date())),
                                    uiOutput("razon.rechazo"),
                                    actionBttn("cmd.guardar.proceso",  NULL, 
                                               style = "simple", color = "success", icon = icon("floppy-o"))
                                ),
                                box(title = 'DETALLE DE CANDIDATO', width = 8, collapsible = T,
                                    htmlOutput("TDetalleCandidatos")),
                                DT::dataTableOutput("tabla.seguimiento"),
                                busyIndicator("Cargando registros...", wait = 1)
                        ),
                        tabItem("abc-candidatos",
                                box(title = 'ABC CANDIDATOS', width = 12, collapsible = T,
                                    splitLayout(cellWidths = c("0%","40%","60%"),
                                                textInput("Tid", "ID"),
                                                textInput("Tnombre", "Nombre"),
                                                textInput("Tdireccion","Direccion")
                                    ),
                                    splitLayout(cellWidths = c("20%","20%","30%","30%"),
                                                textInput("Tcp", "CP", placeholder = "CP de 5 digitos"),
                                                pickerInput("Csexo","Sexo","", 
                                                            options = list('dropupAuto' = T, 'mobile'=T,
                                                                           container=  'body')),
                                                pickerInput("Cescolaridad","Escolaridad","",
                                                            options = list('dropupAuto' = T, 'mobile'=T,
                                                                           container=  'body')),
                                                pickerInput("Cmedio","Adquisicion","",
                                                            options = list('dropupAuto' = T, 'mobile'=T,
                                                                           container=  'body'))
                                    ),
                                    splitLayout(
                                         dateInput("Tnacimiento","Fecha nacimiento",startview = "decade",
                                                   language = "es"),
                                         uiOutput("ui.cvacantes"),
                                         uiOutput("ui.ccliente")),
                                    actionBttn("cmd.nuevo.candidato", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                    actionBttn("cmd.guardar.candidato",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                    actionBttn("cmd.borrar.candidato",  NULL, style = "simple", color = 'danger', icon = icon("minus"))),
                                DT::dataTableOutput("tabla.candidatos"),
                                busyIndicator("Cargando informacion...", wait = 2)
                                ),
                        tabItem("abc-bolsa",
                                box("VACANTES ABIERTAS",width = 9,
                                    DT::dataTableOutput("tabla.bolsa.vacantes")),
                                box("ASIGNAR CANDIDATO A VACANTE", width = 3,
                                    actionBttn("buscar.asignacion","Recomendar...", #mapdist("mexico, 37140","mexico, 37235", mode = 'walking', output = 'simple', override_limit = T)$m
                                               icon = icon("puzzle-piece"), style = "bordered", color = "success" ),
                                    htmlOutput("msg.asignacion"),
                                    actionBttn("guardar.asignacion","Guardar asignacion",
                                               style = "bordered", color = "danger")),
                                box("CANDIDATOS DISPONIBLES", width = 12,
                                    uiOutput("ui.ver.rechazados"),
                                    DT::dataTableOutput("tabla.bolsa.candidatos")),
                                busyIndicator("Cargando informacion...", wait = 1)
                                ),
                        tabItem("abc-gastos", 
                                box("ABC gastos", width = 12, collapsible = T,
                                    splitLayout(cellWidths = c("0%", "40%","40%"),
                                         textInput("Gid", "ID"),       
                                         pickerInput("gastos.conceptos","Conceptos","", 
                                                     options = list('dropupAuto' = T, 'mobile'=T)),
                                         pickerInput("gastos.medios","Medios de adquisicion","",
                                                     multiple = TRUE,
                                                     options = list(
                                                          `actions-box` = TRUE,
                                                          `select-all-text` = "Todos",
                                                          `none-selected-text` = "NINGUNO",
                                                          `deselect-all-text` = "Ninguno",
                                                          size = 10,
                                                          `selected-text-format` = "count > 3",
                                                          container=  'body') 
                                                     )),
                                    splitLayout(cellWidths = c("40%","40%"),
                                    dateInput("fecha.gasto","Fecha de gasto", language = "es",max = Sys.Date()),
                                    textInput("gasto.monto","Monto")),
                                    actionBttn("cmd.nuevo.gasto", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                    actionBttn("cmd.guardar.gasto",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                    actionBttn("cmd.borrar.gasto",  NULL, style = "simple", color = 'danger', icon = icon("minus"))),
                                uiOutput("ui.fechas.filtros.gastos"),
                                DT::dataTableOutput("tabla.gastos"),
                                busyIndicator("Cargando informacion...", wait = 2)),
                        tabItem("abc-users",
                                DT::dataTableOutput("tabla.usuarios")),
                        tabItem("abc-clientes",
                                checkboxInput("clientes.activos","Solo clientes con
                                              vacantes activas", value = T),
                                DT::dataTableOutput("tabla.clientes"),
                                busyIndicator("Cargando clientes...", wait = 1)
                                ),
                        tabItem("abc-vacantes",
                                h5("Las vacantes solo pueden ser creadas y modificadas por un supervisor"),
                                DT::dataTableOutput("tabla.vacantes"),
                                busyIndicator("Cargando vacantes...", wait = 1)
                        ),
                        tabItem("score",
                                box(title = "Vacantes",width = 12, background = "black",
                                valueBoxOutput("ui.vacantes.cumplidas",width = 2),
                                valueBoxOutput("ui.solicitudes.vacante",width = 2),
                                valueBoxOutput("ui.tiempo.promedio",width = 2),
                                #valueBoxOutput("ui.total.vacantes",width = 2),
                                valueBoxOutput("ui.costo.vacante",width = 2),
                                valueBoxOutput("ui.vacantes.abiertas",width = 2),
                                valueBoxOutput("ui.dias.vacantes",width = 2)),
                                box(title = "Dias de proceso",width = 4,
                                    plotOutput("p.tiempos.proceso", height = 200)),
                                box(title = "Embudo",width = 4,
                                    plotOutput("p.embudo", height = 200)),
                                box(title = "Medios" ,width = 4,
                                    plotOutput("p.medios", height = 200)),
                                box(title = "Razones de rechazo",width = 4,
                                    plotOutput("p.razones.rechazo", height = 200)),
                                box(title = "Costo por medio",width = 4,
                                    plotOutput("p.costo.por.medio", height = 200)),
                                    # h5("Total gastado", height=40)),
                                box(title = "En proceso",width = 4,
                                    plotOutput("p.en.proceso", height = 100)),
                                busyIndicator("Calculando scoreboard...", wait = 1)
                        ),
                        tabItem("kpis",
                                box(title = "Solicitudes por vacante",width = 12, background = 'yellow',
                                    # dropdownButton(
                                    #      uiOutput("ui.filtro.procesos"),
                                    #      circle = T, status = 'danger',icon = icon("gear"),
                                    #      width = "300px"),
                                   plotOutput("p.solicitudes.promedio",height = "200px")),
                                box(title = "Tiempo utilizado para cerrar vacantes",width = 12, background = 'orange',
                                   plotOutput("p.tiempos.proceso.score",height = "200px")),
                                box(title = "Porcentaje de vacantes cerradas",width = 12, background = 'green',
                                    plotOutput("p.porcentaje.cerradas",height = "200px")),
                                busyIndicator("Calculando indicadores...", wait = 1)
                      ),
                      tabItem("catalogos",
                              box("Modificar catalogos",
                                   pickerInput("cb.catalogo", label = "Catalogo a modificar",
                                          choices = "",
                                          multiple = F, options = list('dropupAuto' = T, 'mobile'=T,
                                                                      container=  'body')),
                                  
                                  splitLayout(cellWidths = c("0%","100%"),
                                        textInput("Cid","Id"),
                                        textInput("valor","Descripcion")),
                                  actionBttn("cmd.nuevo.valor", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                  actionBttn("cmd.guardar.valor",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                  actionBttn("cmd.borrar.valor",  NULL, style = "simple", color = 'danger', icon = icon("minus"))
                                  ),
                              dataTableOutput("tabla.catalogos",width = "400px"),
                              busyIndicator("Cargando informacion...", wait = 2)
                      )
                   )
                   )
              )
)
)
