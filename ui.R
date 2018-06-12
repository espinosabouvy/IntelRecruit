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
# library(shinyjs)  #install.packages("shinyjs")
library(rhandsontable) #install.packager('rhandsontable)
library(knitr)  #install.packager('rhandsontable)
library(shinyBS)
# library(ggmap) #distancia entre cps

shinyUI(
dashboardPage(skin = "blue",
              dashboardHeader(title ="IntelRecruit v.4.0",titleWidth = 200),
              dashboardSidebar(width = 200,
                   sidebarMenuOutput("menu.login"),
                   sidebarMenuOutput("menu.reclut"),
                   sidebarMenuOutput("menu.logged")
                        ),
              dashboardBody(
                    tags$header(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-89791536-4'></script>
                         <script>
                         window.dataLayer = window.dataLayer || [];
                         function gtag(){dataLayer.push(arguments);}
                         gtag('js', new Date());
                         
                         gtag('config', 'UA-89791536-4');
                         </script>")),     
                   fluidPage(
                             useShinyalert(),
                   tabItems(
                        tabItem("portada",
                                imageOutput("imagen.inicio",width = "600px")),
                        tabItem("abc-candidatos",
                                tabBox(title = 'ABC CANDIDATOS', width = 12,
                                       tabPanel("GENERALES",
                                           splitLayout(cellWidths = c("0%","30%","30%","15%","23%"),
                                                       textInput("Tid", "ID"),
                                                       textInput("Tnombre", "Nombre"),
                                                       textInput("Tdireccion","Direccion"),
                                                       textInput("Tcp", "CP", placeholder = "CP de 5 digitos"),
                                                       dateInput("Tnacimiento","Nacimiento",startview = "decade",
                                                                 language = "es")
                                           ),
                                           splitLayout(cellWidths = c("18%","18%","15%","15%","32%"),
                                                       pickerInput("Csexo","Sexo","", 
                                                                   options = list('dropupAuto' = T, 'mobile'=T,
                                                                                  container=  'body')),
                                                       pickerInput("Cedocivil","Estado Civil",c("SOLTERO","CASADO/UNION LIBRE","DIVORCIADO","VIUDO"),
                                                                   options = list('dropupAuto' = T, 'mobile'=T,
                                                                                  container=  'body')),
                                                       textInput("Ttelefono", "Telefono"),
                                                       textInput("Tcelular", "Celular"),
                                                       textInput("Tcorreo","Correo")
                                           ),
                                           splitLayout(cellWidths = c("18%","18%","14%","24%","24%"),
                                                       pickerInput("Cescolaridad","Maxima escolaridad","",
                                                                   options = list('dropupAuto' = T, 'mobile'=T,
                                                                                  container=  'body')),
                                                       textInput("Timss","No IMSS"),
                                                       pickerInput("Cmedio","Adquisicion","",
                                                                   options = list('dropupAuto' = T, 'mobile'=T,
                                                                                  container=  'body')),
                                                       uiOutput("ui.cvacantes"),
                                                       uiOutput("ui.ccliente")
                                           ),
                                           textAreaInput("hashtags","Etiquetas clave",rows = 2, width = "600px",
                                                         placeholder = "Etiquetas de caracteristicas del candidato que permitan encontrarlo en una busqueda. \nSepara cada etiqueta con un #"),
                                           actionBttn("cmd.nuevo.candidato", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                           actionBttn("cmd.guardar.candidato",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                           actionBttn("cmd.borrar.candidato",  NULL, style = "simple", color = 'danger', icon = icon("minus")),
                                           actionBttn("grafica", NULL, style = "simple", color = 'success', icon = icon("minus")),
                                           bsModal("modalExample", "Your plot", "grafica",size = "large", uiOutput('markdown'))
                                        ),
                                        tabPanel("FAMILIAR",
                                            splitLayout(cellWidths = c("38%", "40%","20%"),
                                                        textInput("Tpadre", "Nombre",placeholder = "Padre"),
                                                        textInput("Tpadredir","Direccion"),
                                                        textInput("Tpadretel", "Telefono")),
                                            splitLayout(cellWidths = c("38%", "40%","20%"),
                                                        cellArgs = list(style = "margin-top: -2em"),
                                                        textInput("Tmadre","",placeholder = "Madre"),
                                                        textInput("Tmadredir",""),
                                                        textInput("Tmadretel","")),
                                            splitLayout(cellWidths = c("38%", "40%","20%"),
                                                        cellArgs = list(style = "margin-top: -2em"),
                                                        textInput("Tesposa","",placeholder = "Esposa"),
                                                        textInput("Tesposadir",""),
                                                        textInput("Tesposatel","")),
                                            numericInput("no.hermanos","Numero de hermanos",0,0,10,1,width = "200px"),
                                            uiOutput("ui.Thermanos"),
                                            tags$head(tags$style(HTML('
                                                                      #Tobsfamiliar{
                                                                      background-color: #dbdbdb;
                                                                      border: none;
                                                                      }'))),
                                            textAreaInput("Tobsfamiliar","Observaciones sobre familia",width = "600px",
                                                          rows = 5, resize = "vertical",placeholder = "Area exclusiva para el reclutador")
                                        ),
                                       # tabPanel("EMPLEOS",
                                       #          lapply(1:3, function(i){
                                       #               t.ingresos <- c("Ultimo", "Penultimo","Ante-penultimo")
                                       #               splitLayout(cellWidths = c("40%","60%"),
                                       #                           cellArgs = list(style = "margin-top: -2em"),
                                       #                           HTML(paste0("<h5 style='padding-top: 20px'; align='right'>",t.ingresos[i],"</h5>")),
                                       #                           textInput(paste0("Tcantingreso",i),placeholder = "Cantidad",label = ""))
                                       #          })
                                       # ),
                                       # tabPanel("REFERENCIAS",
                                       #          lapply(1:3, function(i){
                                       #               t.ingresos <- c("Candidato", "Pareja","Hijos","Padres","Hermanos","Rentas","Pensiones", "Otros")
                                       #               splitLayout(cellWidths = c("40%","60%"),
                                       #                           cellArgs = list(style = "margin-top: -2em"),
                                       #                           HTML(paste0("<h5 style='padding-top: 20px'; align='right'>",t.ingresos[i],"</h5>")),
                                       #                           textInput(paste0("Tcantingreso",i),placeholder = "Cantidad",label = ""))
                                       #          })
                                       # ),
                                       tabPanel("VIVIENDA",
                                             splitLayout(cellWidths = c("20%","20%","18%","20%","20%"),
                                                  pickerInput("Cvivienda","Tipo Vivienda",c("CASA","DUPLEX","HUESPED","DEPARTAMENTO","VECINDAD","OTRO"),
                                                              options = list('dropupAuto' = T, 'mobile'=T,
                                                                             container=  'body')),
                                                  pickerInput("CVtipo","Status vivienda",c("PROPIA","RENTA","HIPOTECA","PRESTADA","CON LOS PADRES","OTRO"),
                                                              options = list('dropupAuto' = T, 'mobile'=T,
                                                                             container=  'body')),
                                                  pickerInput("CVnivel","Nivel vivienda",c("RESIDENCIAL","ALTA-MEDIA","MEDIA-MEDIA","MEDIA-BAJA","BAJA"),
                                                              options = list('dropupAuto' = T, 'mobile'=T,
                                                                             container=  'body')),
                                                  pickerInput("CVservicios","Servicios",c("LUZ","AGUA","PAVIMENTADA","DRENAJE","TELEFONO","VIGILANCIA"),
                                                              options = list('dropupAuto' = T, 'mobile'=F,
                                                                             container=  'body',
                                                                             `actions-box` = TRUE),multiple = T),
                                                  pickerInput("CVdistribucion","Distribucion",c("SALA","COMEDOR","SALA TV","COCINA","COCHERA","JARDIN","ESTUDIO","BIBLIOTECA","CUARTO SERVICIO"),
                                                              options = list('dropupAuto' = T, 'mobile'=F,
                                                                             container=  'body',
                                                                             `actions-box` = TRUE),multiple = T)),
                                             splitLayout(cellWidths = c("20%","20%","18%","20%","20%"),
                                                    pickerInput("cvespacio","Espacio",c("SOBRADO","SUFICIENTE","LIMITADO","INSUFICIENTE"),
                                                                options = list('dropupAuto' = T, 'mobile'=T,
                                                                               container=  'body')),
                                                    pickerInput("CVcondiciones","Condiciones",c("EXCELENTE","BUENO","REGULAR","MALO"),
                                                                options = list('dropupAuto' = T, 'mobile'=T,
                                                                               container=  'body')),
                                                    pickerInput("CVlimpieza","Orden/Limpieza",c("EXCELENTE","BUENO","REGULAR","MALO"),
                                                                options = list('dropupAuto' = T, 'mobile'=T,
                                                                               container=  'body')),
                                                    pickerInput("CVcalidad","Calidad muebles",c("EXCELENTE","BUENO","REGULAR","MALO"),
                                                                options = list('dropupAuto' = T, 'mobile'=T,
                                                                               container=  'body')),
                                                    pickerInput("CVconservacion","Conserva muebles",c("EXCELENTE","BUENO","REGULAR","MALO"),
                                                                options = list('dropupAuto' = T, 'mobile'=T,
                                                                               container=  'body'))),
                                             splitLayout(cellWidths = c("15%","20%","28%","15%","20%"),
                                                    numericInput("CVbanos","Numero de baños",2,0,10,1),
                                                    numericInput("CVrecamaras","Numero de recamaras",2,0,10,1),
                                                    numericInput("CVcocheras","Cochera para cuantos autos",1,0,10,1),
                                                    numericInput("CVtiempo","Años vivir ahi",1,0,80,1),
                                                    numericInput("CVradicar","Años radicar en ciudad",1,0,80,1)),
                                             tags$head(tags$style(HTML('
                                                                      #Tobsvivienda{
                                                                       background-color: #dbdbdb;
                                                                       border: none;
                                                                       }'))),
                                             textAreaInput("Tobsvivienda","Observaciones sobre vivienda",width = "600px",
                                                           rows = 5, resize = "vertical",placeholder = "Area exclusiva para el reclutador")
                                      ),
                                      tabPanel("ECONOMICO",
                                             box(title = "Ingresos",width = 6,
                                                    lapply(1:8, function(i){
                                                         t.ingresos <- c("Candidato", "Pareja","Hijos","Padres","Hermanos","Rentas","Pensiones", "Otros")
                                                         splitLayout(cellWidths = c("40%","60%"),
                                                                     cellArgs = list(style = "margin-top: -2em"),
                                                                     HTML(paste0("<h5 style='padding-top: 20px'; align='right'>",t.ingresos[i],"</h5>")),
                                                              textInput(paste0("Tcantingreso",i),placeholder = "Cantidad",label = ""))
                                                    })
                                            ),
                                            box(title = "Egresos",width = 6,
                                                lapply(1:12, function(i){
                                                     t.egresos <- c("Alimentacion","Renta","Telefono","Agua","Luz","TV paga","Transporte","Colegiatura","Medico/Medicina","Seguros","Vestir", "Otros")
                                                     splitLayout(cellWidths = c("40%","60%"),
                                                                 cellArgs = list(style = "margin-top: -2em"),
                                                                 HTML(paste0("<h5 style='padding-top: 20px'; align='right'>",t.egresos[i],"</h5>")),
                                                                 textInput(paste0("Tcantegreso",i),placeholder = "Cantidad",label = ""))
                                                })
                                            ),
                                            tags$head(tags$style(HTML('
                                                                      #Tobseconomico{
                                                                      background-color: #dbdbdb;
                                                                      border: none;
                                                                      }'))),
                                            textAreaInput("Tobseconomico","Observaciones sobre economía",width = "600px",
                                                          rows = 5, resize = "vertical",placeholder = "Area exclusiva para el reclutador")
                                      ),
                                      tabPanel("ACTIVIDADES",
                                          splitLayout(
                                               textInput("Tdeportivo", "Deportivas",placeholder = "Separar las actividades con ; (punto y coma)"),
                                               textInput("Tcultural","Culturales",placeholder = "Separar las actividades con ; (punto y coma)")),
                                          splitLayout(
                                               textInput("Tpoliticas", "Politicas",placeholder = "Separar las actividades con ; (punto y coma)"),
                                               textInput("Tsindicales","Sindicales",placeholder = "Separar las actividades con ; (punto y coma)")),
                                          splitLayout(
                                               textInput("Treligiosas", "Religiosas",placeholder = "Separar las actividades con ; (punto y coma)"),
                                               textInput("Tactotras","Otras",placeholder = "Separar las actividades con ; (punto y coma)"))
                                      ),
                                      tabPanel("SALUD",
                                          splitLayout(
                                               textInput("Salcohol","Alcohol",placeholder = "Cada cuantos días (0 sin no ingiere)"),
                                               textInput("Scigarro","Tabaco",placeholder = "Cuantos al día (0 sin no fuma)"),
                                               checkboxInput("Sdroga","Consume drogas?"),
                                               checkboxInput("Sseguro","Tiene SGMM?")),
                                          splitLayout(cellWidths = c("78%","20%"),
                                               textInput("Scirugia","Intervenciones quirugicas",placeholder = "Separar las actividades con ; (punto y coma)"),
                                               dateInput("Scirfecha","Ultima intervencion",max = Sys.Date(),startview = "year")),
                                          textInput("Sinternado","Si ha estado internado. ¿Detalle cual fue el motivo?",width = "600px"),
                                          textInput("Scronica","Detalle si tiene enfermedades cronicas",width = "600px"),
                                          textInput("Stratamiento","Detalle si recibe tratamiento medico actualmente",width = "600px"),
                                          textInput("Sorganos","Detalle si le han extirpado algún organo o parte de el",width = "600px"),
                                          textInput("Sintervencion","Detalle si tiene alguna intervencion quirúrgica pendiente",width = "600px"),
                                          tags$head(tags$style(HTML('
                                                                      #Tobssalud{
                                                                    background-color: #dbdbdb;
                                                                    border: none;
                                                                    }'))),
                                            textAreaInput("Tobssalud","Observaciones sobre salud",width = "600px",
                                                          rows = 5, resize = "vertical",placeholder = "Area exclusiva para el reclutador")
                                      )
                                )
                           ),
                           tabItem("abc-registro",
                                box(title = "REGISTRAR AVANCE", collapsible = T,
                                    collapsed = F, width = 3,
                                    textOutput("ultimo.proceso"),
                                    pickerInput("Cproceso","Proceso","",
                                                options = list('dropupAuto' = T, 'mobile'=T)),
                                    uiOutput("ui.Cfecha"),
                                    uiOutput("razon.rechazo"),
                                    actionBttn("cmd.guardar.proceso",  NULL, 
                                               style = "simple", color = "success", icon = icon("floppy-o"))
                                ),
                                column(9,DT::dataTableOutput("tabla.seguimiento")),
                                busyIndicator("Cargando registros...", wait = 1)
                        ),
                        tabItem("abc-bolsa",
                                box("VACANTES ABIERTAS",width = 9,
                                    DT::dataTableOutput("tabla.bolsa.vacantes")),
                                box("ASIGNAR CANDIDATO A VACANTE", width = 3,
                                    # actionBttn("buscar.asignacion","Recomendar", #mapdist("mexico, 37140","mexico, 37235", mode = 'walking', output = 'simple', override_limit = T)$m
                                    #            style = "material-flat", color = "primary" ),
                                    actionBttn("guardar.asignacion","Asignar",
                                               style = "material-flat", color = "success"),
                                    htmlOutput("msg.asignacion")
                                    ),
                                box("CANDIDATOS DISPONIBLES", width = 12,
                                    uiOutput("ui.ver.rechazados"),
                                    DT::dataTableOutput("tabla.bolsa.candidatos")),
                                busyIndicator("Cargando informacion...", wait = 1)
                                ),
                        tabItem("abc-gastos", 
                                box(title = "ABC gastos", width = 12, collapsible = T,
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
                                box(id = "users.box", title = "ABC usuarios", width = 12, collapsible = T,
                                    splitLayout(cellWidths = c("0%","0%","30%","15%","20%","30%"),
                                        textInput("Uid", "ID"),
                                        textInput("Uvac", "Vacantes"),
                                        textInput("Unombre","Nombre"),
                                        textInput("Uuser","Usuario"),
                                        pickerInput("Ulevel","Nivel de usuario",c("supervisor","reclutador"), 
                                                    options = list('dropupAuto' = T, 'mobile'=T))
                                    ),
                                    actionBttn("cmd.nuevo.user", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                    actionBttn("cmd.guardar.user",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                    actionBttn("cmd.borrar.user",  NULL, style = "simple", color = 'danger', icon = icon("minus")),
                                    actionBttn("cmd.reset.user", "Reset contraseña", style = "bordered", color = 'success', icon = icon("unlock"))
                                ),
                                DT::dataTableOutput("tabla.usuarios"),
                                busyIndicator("Cargando usuarios...", wait = 1)
                         ),
                        tabItem("abc-clientes",
                                box(
                                   id = "clientes.box", title = "ABC clientes", width = 12, collapsible = T,
                                    splitLayout(cellWidths = c("0%","40%","55%"),
                                                textInput("Ctid", "ID"),
                                                textInput("Ctcliente","Cliente"),
                                                textInput("Ctdireccion","Direccion")
                                    ),
                                    splitLayout(cellWidths = c("30%","30%"),
                                                textInput("Cttelefono","Telefono"),
                                                textInput("Ctcp","Codigo Postal")
                                    ),
                                    actionBttn("cmd.nuevo.cliente", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                    actionBttn("cmd.guardar.cliente",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                    actionBttn("cmd.borrar.cliente",  NULL, style = "simple", color = 'danger', icon = icon("minus"))
                                ),
                                radioGroupButtons(inputId = "clientes.activos", label = "Filtrar", choices = c("Todos", "Con vacantes activas"), 
                                                  status = "primary", selected = "Con vacantes activas",
                                                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))),
                                DT::dataTableOutput("tabla.clientes"),
                                busyIndicator("Cargando clientes...", wait = 1)
                                ),
                        tabItem("solo-vacantes",
                                p("Las vacantes solo pueden ser modificadas por un supervisor"),
                                DT::dataTableOutput("tabla.vacantes.solo"),
                                busyIndicator("Cargando vacantes...", wait = 1)
                        ),
                        tabItem("abc-vacantes",
                                box(id = "vacantes.box", title = "ABC vacantes", width = 12, collapsible = T,
                                splitLayout(cellWidths = c("0%","30%","30%","15%","15%"),
                                            textInput("Vid", "ID"),
                                            pickerInput("Vcliente","Cliente","", 
                                                        options = list('dropupAuto' = T, 'mobile'=T)),
                                            pickerInput("Vvacante","Vacante","", 
                                                        options = list('dropupAuto' = T, 'mobile'=T)),
                                            pickerInput("Vreclut","Reclutador","", 
                                                        options = list('dropupAuto' = T, 'mobile'=T)),
                                            dateInput("Vfecha","Fecha",value = Sys.Date(),
                                                      language = "es")
                                            ),
                                actionBttn("cmd.nuevo.vacante", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                actionBttn("cmd.guardar.vacante",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                actionBttn("cmd.borrar.vacante",  NULL, style = "simple", color = 'danger', icon = icon("minus"))
                                ),
                                radioGroupButtons(inputId = "vacantes.cerradas", label = "Filtrar", choices = c("Todas", "Solo abiertas"), 
                                                  status = "primary", selected = "Solo abiertas",
                                                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon"))),
                                DT::dataTableOutput("tabla.vacantes"),
                                busyIndicator("Cargando vacantes...", wait = 1)
                        ),
                        tabItem("abc-metas",
                                box(id = "metas.box", title = "ABC metas", width = 12, collapsible = T,
                                    splitLayout(cellWidths = c("0%","40%","15%","15%"),
                                                textInput("Mid", "ID"),
                                                pickerInput("Mdescripcion","KPI","",
                                                            options = list('dropupAuto' = T, 'mobile'=T)),
                                                textInput("Mmeta","Meta",""),
                                                dateInput("Mfecha","Fecha de inicio", value = Sys.Date(),
                                                          language = "es")
                                    ),
                                    actionBttn("cmd.nuevo.meta", NULL, style = "simple",color = "primary", icon = icon("plus")),
                                    actionBttn("cmd.guardar.meta",  NULL, style = "simple", color = "success", icon = icon("floppy-o")),
                                    actionBttn("cmd.borrar.meta",  NULL, style = "simple", color = 'danger', icon = icon("minus"))
                                ),
                                dataTableOutput("tabla.metas"),
                                busyIndicator("Cargando vacantes...", wait = 1)
                        ),
                        tabItem("define-proceso",
                                p("Define los pasos de tu proceso de reclutamiento."),
                                column(6, 
                                       prettyCheckbox(inputId = "txt.name.solicitud", 
                                                      label = "Paso 1 - Solicitud", value = TRUE, 
                                                      icon = icon("check"), status = "success", 
                                                      animation = "rotate", bigger = T),
                                       lapply(2:6, function(i){
                                            textInput(paste0("txt.paso",i),paste("Paso",i),placeholder = "Dejar vacio si no se requiere")
                                       }),
                                       prettyCheckbox(inputId = "txt.name.contratar", 
                                                      label = "Paso final - Contratar", value = TRUE, 
                                                      icon = icon("check"), status = "success", 
                                                      animation = "rotate" ,bigger = T),
                                       actionBttn(inputId = "cmd.definir.proceso", "Guardar proceso",icon = icon("save"),color = "success")),
                                column(12, p("PRECAUCIÓN: Si ya tienes registros de avance de proceso
                                  registrados, solo se cambiarán los nombres (etiqueta) del proceso realizado, no los registros. 
                                  Esto puede causar errores y conflictos, antes de realizar un cambio te sugerimos te pongas en contacto con nosotros
                                  en contacto@magro.com.mx"))
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
                                    plotOutput("p.tiempos.proceso", height = 200)), #ok
                                box(title = "Embudo",width = 4,
                                    plotOutput("p.embudo", height = 200)), #ok
                                box(title = "Medios" ,width = 4,
                                    plotOutput("p.medios", height = 200)), #ok
                                box(title = "Razones de rechazo",width = 4,
                                    plotOutput("p.razones.rechazo", height = 200)),  #ok
                                box(title = "Costo por medio",width = 4,
                                    plotOutput("p.costo.por.medio", height = 200)), #ok
                                    # h5("Total gastado", height=40)),
                                box(title = "En proceso",width = 4,
                                    plotOutput("p.en.proceso", height = 100)),
                                busyIndicator("Calculando scoreboard...", wait = 1)
                        ),
                        tabItem("supervision",
                                box(title = "Generales", width = 12, background = "black",
                                   valueBoxOutput("uis.total.vacantes",width = 3), #ok
                                   valueBoxOutput("uis.total.solicitudes",width = 3),
                                   valueBoxOutput("uis.gastado",width = 3),
                                   valueBoxOutput("uis.vacantes.abiertas",width = 3) #ok
                                ),
                                box(title = "Vacantes", width = 12, background = "black",
                                    valueBoxOutput("uis.vacantes.cerradas",width = 2), #ok
                                    valueBoxOutput("uis.solicitudes.vacante",width = 2),
                                    valueBoxOutput("uis.tiempo.promedio",width = 2),
                                    valueBoxOutput("uis.costo.vacante",width = 2),
                                    valueBoxOutput("uis.abiertas.promedio",width = 3), #ok
                                    valueBoxOutput("uis.dias.vacantes",width = 2)
                                ),
                                box("Vacantes por cliente", width = 6,
                                    plotOutput("ss.vacantes.cliente", height = "400px")),
                                box("Vacantes por reclutador", width = 6,
                                    plotOutput("ss.vacantes.reclut", height = "400px")),
                                busyIndicator("Calculando indicadores...", wait = 1)
                        ),
                        tabItem("kpis",
                                box("SOLICITUDES POR VACANTE",width = 12, background = 'yellow',
                                   plotOutput("p.solicitudes.promedio",height = "200px")),
                                box("TIEMPO UTILIZADO PARA CERRAR VACANTES",width = 12, background = 'orange',
                                   plotOutput("p.tiempos.proceso.score",height = "200px")),
                                box("PORCENTAJE DE VACANTES CERRADAS",width = 12, background = 'green',
                                    plotOutput("p.porcentaje.cerradas",height = "200px")),
                                busyIndicator("Calculando indicadores...", wait = 1)
                         ),
                      tabItem("catalogos",
                              box("",
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
