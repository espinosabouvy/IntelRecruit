

shinyServer(function(input, output, session) {
     options(shiny.reactlog=TRUE)      
     options(shiny.sanitize.errors = FALSE)
     
     #VARIABLES PRINCIPALES -----------------------------------------------------------
     #suponer fecha, para que sys.date() con haga ridiculas las graficas
     #cambiar a sys.date() para el programa, fuera del demo
     demo = T
     if(demo){
          fecha.hoy <- ymd(20180215)  #registro follow 20180311
     } else {
          fecha.hoy = as_date(Sys.Date())
     }
     
     values <- reactiveValues(sessionId = NULL)
     values$sessionId <- as.integer(runif(1, 1, 100000))
     #cat(paste0("Session id: ", values$sessionId))
     session$onSessionEnded(function() {
          observe(cat(paste0("Ended: ", values$sessionId)))
          # #desconectar cuando se cierra la sesion
          # dbDisconnect(con)
     })
     
     #cargar cp
     cp <- read.csv("www/cp.csv")

     #main variables
     b.logged <- FALSE   #global status login
     id_user <- character(0)  #id_user para filtros por usuario logeado
     level <- character(0)   #nivel del usuario logeado
     nombre <- character(0)  #nombre del usuario
     
     #con <- NULL         #global conexion mysql
     fechas.selected <- character(0)  #una frecuencia por default al cargar kpis
     
     #tablas guardadas de consultas
     kpi.tiempo <- NULL
     seguimiento <-  NULL
     embudo <-  NULL
     tabla.de.seguimiento <- NULL
     vacantes.abiertas <- NULL
     vacantes.disponibles <- NULL
     db.tabla.candidatos <- NULL
     db.tabla.gastos <- NULL
     vacantes <- NULL
     tabla.bolsa <- NULL
     c_sexo <- NULL
     c_escolaridad <- NULL
     c_medio <- NULL
     c_procesos <- NULL
     c_rechazo <- NULL
     c_concepto.gastos <- NULL
     c_catalogos <- NULL
     c_reclutadores <- NULL
     
     #funciones de consultas -----------------------------
     #conexion la bd
     conectar <- function(){
          if(!exists("cp")) cp <<- read.csv("www/cp.csv")
          contra <- 'M4gr0-demo'
          usuario <- "demo"
          #conexion con la bd
          con <- tryCatch(
               {return(dbConnect(RMariaDB::MariaDB(), dbname = as.character(cp$BD),
                                 username = usuario, password = contra, host = as.character(cp$host),
                                 port = cp$port))
               },
               error = function(mensaje){
                    cat(paste("No se pudo conectar al server", mensaje))
                    sendSweetAlert(session, "Error conexion","No fue posible conectarse al servidor\n
                                   Intenta nuevamente")
                    return(NA)
               }
          )
     }
     
     q.bolsa.vacantes <- function(){
          con <- conectar()
          query <- paste0("
                         SELECT candidatos.id, candidatos.nombre, vacantes.vacante_original, vacantes.cliente_original, va.id_proceso, va.ultimo_proceso, va.razon_rechazo, cp, c_sexo.`nombre` AS 'sexo', 
                          c_escolaridad.`nombre`AS 'escolaridad', fecha_nacimiento, medios.`nombre` AS 'medio', direccion 
                          FROM candidatos
                          RIGHT JOIN (
                              SELECT vf.id_candidato, vf.`id_vacante`, vf.id_proceso, vp.`nombre` ultimo_proceso, vf.id_razon_rechazo, rr.`nombre` razon_rechazo
                              FROM vacantes_following vf
                              LEFT JOIN vacantes_procesos vp ON vp.id = vf.id_proceso
                              LEFT JOIN razones_rechazo rr ON rr.`id` = vf.`id_razon_rechazo`
                              RIGHT JOIN (
                                   SELECT vf.id_candidato, MAX(vp.orden) orden
                                    FROM vacantes_following vf
                                    LEFT JOIN vacantes_procesos vp ON vp.id = vf.id_proceso
                                    GROUP BY id_candidato
                                    ) ultimo ON ultimo.id_candidato = vf.id_candidato AND 
                                        ultimo.orden = vp.orden
                              LEFT JOIN vacantes vac ON vac.id = vf.id_vacante
                              WHERE vf.id_proceso = 5 OR (vac.id_status = 2 AND vf.id_proceso <>4)
                              AND vf.`cambio_vacante` = 0)
                          AS va ON va.id_candidato = candidatos.`id`
                          LEFT JOIN c_sexo ON candidatos.`id_sexo` = c_sexo.`id`
                          LEFT JOIN c_escolaridad ON candidatos.`id_escolaridad` = c_escolaridad.`id`
                          LEFT JOIN medios ON medios.`id` = candidatos.`id_medio`
                          LEFT JOIN (
                              SELECT vac.`id`, vn.`nombre` AS 'vacante_original', clientes.`nombre` AS 'cliente_original', vac.`id_status`
                              FROM vacantes AS vac
                              LEFT JOIN vacantes_nombre AS vn ON vn.`id`= vac.`id_nombre_vacante`
                              LEFT JOIN clientes ON clientes.`id`= vac.`id_cliente`) 
                          AS vacantes ON vacantes.`id` = va.id_vacante
                          WHERE candidatos.`baja`=0
                          ORDER BY id_candidato")
          consulta <- dbGetQuery(con, query)
          consulta$fecha_nacimiento <- ymd(consulta$fecha_nacimiento)
          return(consulta)
     }
     
     q.total.gastado <- function(id_usuario = NULL, date.inicio = 20000101){
          con <- conectar()
          
          date.inicio <- gsub("-","", date.inicio)
          
          query <- paste0("SELECT SUM(monto) gastado 
                         FROM gastos
                         WHERE baja = 0 
                         AND fecha >= ", gsub("-","",ymd(date.inicio)))
          if(!is.null(id_usuario)){
               query <- paste0(query, " AND id_asesor = ", id_usuario)
          }
          consulta <- dbGetQuery(con, query)
          return(consulta)
     }
  
     q.gastos <- function(id_usuario=id_user, date.inicio = 20000101){
          con <- conectar()
          cat(paste("Fecha de gastos", date.inicio, "\n"))
          
          query <- paste0("
                         SELECT ga.id, gc.id id_concepto, gc.nombre concepto, me.id id_medio, 
                              IF(me.nombre IS NULL, 'NINGUNO', me.nombre) medio_asignado, fecha, monto, id_comun
                         FROM gastos ga
                         LEFT JOIN gastos_conceptos gc ON gc.id = ga.id_concepto
                         LEFT JOIN medios me ON me.id = ga.id_medio
                         WHERE ga.baja = 0
                         AND fecha >= ",gsub("-","",ymd(date.inicio)))

          if (!is.null(id_usuario)){
               query <- paste(query, " AND ga.id_asesor = " , id_usuario)
               }

          gastos <- dbGetQuery(con, query)
          dbDisconnect(con)

          gastos$id_medio <- as.integer(gastos$id_medio)
          gastos$id_concepto <- as.integer(gastos$id_concepto)
          
          return(gastos)
     }
          
     q.kpi.tiempo.proceso <- function(id_status = c(1,2), id_usuario, date.inicio, all=F){
          con <- conectar()
          
          query <- paste0("SELECT va.asesor, va.id AS 'id_vacante', va.fecha_vacante, va.id_status,  vp.orden, 
                    vp.id AS 'id_proceso', vp.nombre AS 'proceso', vf.fecha, rc.nombre AS 'razon_rechazo'
                    FROM vacantes_following AS vf
                    LEFT JOIN vacantes_procesos AS vp ON vp.id = vf.id_proceso 
                    LEFT JOIN razones_rechazo rc ON rc.`id` = vf.id_razon_rechazo 
                    LEFT JOIN 
                         (SELECT vacantes.id, vacantes.`baja`, clientes.`nombre`, vacantes_nombre.`nombre` AS 'vacante', 
                              fecha AS 'fecha_vacante', vacantes.`id_status` AS 'id_status', users.id AS 'id_usuario' ,
                              users.`user` AS 'asesor' 
                         FROM vacantes
                         LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
                         LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
                         LEFT JOIN users ON users.id = vacantes.`id_usuario`
                         WHERE vacantes.`id_status` IN (", paste0(id_status, collapse = ",") , ")) 
                         AS va ON va.id = vf.id_vacante 
                         WHERE va.baja = 0")
          if (all == F){ #agrega filtros a la consulta
               query <- paste(query, " AND va.id_usuario = ", id_usuario, "AND va.fecha_vacante >= ",
                              gsub("-","",ymd(date.inicio)))
          }
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          if(nrow(consulta)>0){
               consulta$fecha <- ymd(consulta$fecha)
               consulta$fecha_vacante <- ymd(consulta$fecha_vacante)
               consulta$proceso <- factor(consulta$proceso, unique((consulta%>%arrange(orden)%>%select(proceso))$proceso))
          }
          return(consulta)
          
     }
     
     q.vacantes <- function(id_status=c(1,2), id_usuario= id_user, all=F, date.inicio= 20000101){
          con <- conectar()
          query <- paste0("SELECT vacantes.id, clientes.`nombre` AS 'cliente', vacantes_nombre.`nombre` AS 'vacante', 
                          vacantes.fecha, vacantes_status.`nombre` AS 'status', users.`user` AS 'asesor',
                          clientes.`codigo_postal`
                          FROM vacantes
                          LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
                          LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
                          LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
                          LEFT JOIN users ON users.id = vacantes.`id_usuario` 
                          WHERE vacantes.`baja`=0 ")
          
          if (all == F){ #agrega filtros a la consulta
               query <- paste(query, " AND vacantes.id_status IN (", paste(id_status,collapse = ",") 
                              ,") AND  vacantes.id_usuario= ", id_usuario, 
                              "AND vacantes.fecha >= ", gsub("-","",ymd(date.inicio)))
          }
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          consulta$fecha <- ymd(consulta$fecha)
          return(consulta)
     }
     
     #seguimiento a candidatos en proceso, vacantes abiertas y/o cerradas
     q.seguimiento <- function(id_status=1, id_usuario= id_user, all=F){
          con <- conectar()
          query <- paste0("SELECT cand.id as 'id_candidato', cand.nombre  AS 'candidato', cand.escolaridad, cand.sexo, cand.medio, vacantes_detalle.*, vp.orden AS 'orden_proceso',
                          vp.id AS 'id_proceso', vp.nombre AS 'proceso', 
                          vf.fecha, vf.comentarios FROM vacantes_following AS vf
                          LEFT JOIN (
                              SELECT candidatos.id, candidatos.nombre, candidatos.baja, c_sexo.`nombre` AS 'sexo', esc.`nombre` AS 'escolaridad', medios.`nombre` AS 'medio' 
                              FROM candidatos
                              LEFT JOIN c_sexo ON c_sexo.`id` = candidatos.`id_sexo`
                              LEFT JOIN c_escolaridad AS esc ON esc.id = candidatos.`id_escolaridad`
                              LEFT JOIN medios ON medios.`id` = candidatos.`id_medio`) 
                              AS cand ON cand.`id` = vf.`id_candidato`
                          LEFT JOIN vacantes_procesos AS vp ON vp.id = vf.id_proceso
                          RIGHT JOIN(
                              SELECT vacantes.`id` as 'id_vacante',clientes.`nombre` as 'cliente', vacantes_nombre.`nombre` AS 'vacante',
                                   vacantes_status.`nombre` AS 'status', users.id AS 'id_user', users.`nombre` AS 'asesor' 
                              FROM vacantes
                              LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
                              LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
                              LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
                              LEFT JOIN users ON users.id = vacantes.`id_usuario`
                              WHERE vacantes.id_status IN (", paste0(id_status, collapse = ","))
          
          if (all == F){ #agrega filtros a la consulta
               query <- paste(query, ") AND  vacantes.id_usuario= ", id_usuario)
          }
          
          query <- paste(query,") AS vacantes_detalle ON vacantes_detalle.id_vacante = vf.`id_vacante`
                         WHERE vf.id_candidato NOT IN 
                              (SELECT DISTINCT(vacantes_following.id_candidato) FROM vacantes_following
                              LEFT JOIN vacantes_procesos on vacantes_procesos.id = vacantes_following.id_proceso
                              WHERE vacantes_procesos.cierra_proceso = 1) AND cand.baja = 0")
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          consulta$fecha <- ymd(consulta$fecha)
          return(consulta)
     
     }
     
     q.candidatos <- function(id_candidatos = NULL, mis.candidatos=NULL){
          con <- conectar()
          query <- paste("SELECT candidatos.id, candidatos.nombre, cp, c_sexo.`nombre` AS 'sexo', 
                              c_escolaridad.`nombre`AS 'escolaridad', fecha_nacimiento, medios.`nombre` AS 'medio', 
                         direccion 
                         FROM candidatos
                         LEFT JOIN c_sexo ON candidatos.`id_sexo` = c_sexo.`id`
                         LEFT JOIN c_escolaridad ON candidatos.`id_escolaridad` = c_escolaridad.`id`
                         LEFT JOIN medios ON medios.`id` = candidatos.`id_medio`
                         WHERE candidatos.`baja`=0 ")
          if(!is.null(id_candidatos)){
               query <- paste0(query, " AND candidatos.`id` IN (",paste0(id_candidatos , collapse = ","),")")
          } else {
               if (mis.candidatos)
                    query <- paste0(query, "AND candidatos.`id` IN (  /*condicional a mis candidatos*/
	                                   SELECT DISTINCT(id_candidato) 
                                        FROM vacantes_following vf
                                        WHERE id_vacante IN (
                                        SELECT id id_vacante FROM vacantes WHERE id_usuario = 5 AND id_status= 1)
                    )")     
          }
          
          query <- paste0(query, " ORDER BY candidatos.id")
          
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          return(consulta)
     }
     
     q.costo.por.medio <- function(id_usuario= id_user, date.inicio = 20000101){
          con <- conectar()
          query <- paste0("
                         SELECT me.nombre medio, cerradas, IF(gastado IS NULL, 0, gastado) gastado, 
                              ROUND(IF(gastado/cerradas IS NULL, 0, gastado/cerradas),0) costo
                         FROM(
                              SELECT ca.id_medio, COUNT(ca.id) cerradas
                              FROM candidatos ca
                              LEFT JOIN vacantes_following vf ON vf.`id_candidato` = ca.id
                              LEFT JOIN vacantes va ON va.id = vf.`id_vacante`
                              WHERE vf.`id_proceso` = 4 AND va.fecha >= ", gsub("-","",ymd(date.inicio)))
          
          if(!is.null(id_usuario)){
               query <- paste0(query,  " AND va.id_usuario = ", id_usuario)
          }
          
          
          query <- paste(query," GROUP BY (ca.id_medio)) AS cerradas
                         LEFT JOIN (
                              SELECT ga.id_medio, SUM(monto) gastado
                              FROM gastos ga
                              WHERE baja = 0 
                              AND ga.fecha >= ", date.inicio,
                              " GROUP BY (id_medio)) AS gasto ON gasto.id_medio = cerradas.id_medio
                         LEFT JOIN medios me ON me.id = cerradas.id_medio")
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          return(consulta)
          
     }
     
     
     q.clientes <- function(){
          con <- conectar()
          query <- paste0("SELECT clientes.*, IF(COUNT(vacantes_abiertas.id_cliente)>0,1,0) AS 'con_vacantes' 
                          FROM clientes 
                          LEFT JOIN 
                              (SELECT id_cliente FROM vacantes WHERE id_status = 1) 
                          AS vacantes_abiertas ON vacantes_abiertas.id_cliente = clientes.`id` 
                          WHERE clientes.`baja`= 0 
                          GROUP BY clientes.`id`")
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          return(consulta)
     }
     
     q.embudo <- function(id.status.vacantes = c(1,2), id_usuario= id_user, all=F, date.inicio=20000101){
          con <- conectar()
          query <- paste0("SELECT vacantes.id, clientes.`nombre`, vacantes_nombre.`nombre` AS 'vacante', 
               vacantes.fecha as 'fecha_vacante', vacantes_status.`nombre` AS 'status', 
               vacantes.`id_usuario`, users.`user` AS 'asesor' , vf.* 
               FROM vacantes
               LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
               LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
               LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
               LEFT JOIN users ON users.id = vacantes.`id_usuario`
               RIGHT JOIN (
               	SELECT  vacantes_following.id_vacante, cand.candidato, cand.sexo, cand.escolaridad, cand.id_medio, cand.medio, 
                    proc.orden, proc.id AS 'id_proceso', proc.nombre AS 'proceso', vacantes_following.`fecha`
               	FROM vacantes_following 
               	LEFT JOIN (SELECT cand.id, cand.nombre AS 'candidato', c_sexo.`nombre` AS 'sexo', 
                              c_escolaridad.`nombre` AS 'escolaridad', cand.id_medio, medios.`nombre` AS 'medio'
               			FROM candidatos AS cand
               			LEFT JOIN c_sexo ON c_sexo.`id` = cand.id_sexo
               			LEFT JOIN c_escolaridad ON c_escolaridad.id = cand.id_escolaridad
               			LEFT JOIN medios ON medios.`id` = cand.id_medio	 
               			) AS cand ON cand.id= vacantes_following.`id_candidato`
               	LEFT JOIN vacantes_procesos AS proc ON proc.id = vacantes_following.`id_proceso`
                ) AS vf ON vf.id_vacante = vacantes.`id` 
                WHERE vacantes.`baja`=0 ")
                          
           if (all == F){ #agrega filtros a la consulta status, usuario y fechas
                query <- paste(query, " AND vacantes.`id_status` IN (", paste0(id.status.vacantes, collapse = ","), 
                              ") AND vacantes.id_usuario = ", id_usuario, 
                              "AND vacantes.fecha >= ", gsub("-","",ymd(date.inicio)))
           }
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          return(consulta)
     }
     
     #frecuencias
     tiempos <- function(leyenda){
          con <- conectar()
          query <- paste("SELECT * FROM frecuencias WHERE baja=0")
          consulta <- dbGetQuery(con, query)%>%filter(nombre == leyenda)
          dbDisconnect(con)
          
          fechas <- 20000101
          if (consulta$dias == 0){
               if(leyenda == "Mes actual"){
                    fechas <- floor_date(fecha.hoy, "month")  #inicio de mes
               }
               if(leyenda =="Año actual") {
                    fechas <- floor_date(fecha.hoy,"year")
               }
          } else {
               fechas <- fecha.hoy - days(consulta$dias)
          }
          return(gsub("-","",fechas))
     }
     
     #termina funciones de consultas-------------------------------------------------
     
     #registros y funcionamientos ---------------------------------------------------
     #carga pantalla para login   
     output$menu.login <- renderMenu({
          sidebarMenu(
               menuItem("Ingresar al sistema", tabName = "portada", icon = icon('sign-in'),selected = T),
               textInput("s.usuario","Usuario:",placeholder = "Usuario: demo",value = ""),
               passwordInput("s.contra", "Contraseña:",placeholder = "Contraseña: a"),
               actionBttn(inputId = "b.login", label = "Entrar", 
                          style = "jelly", color = "primary")
          )
     })
     
     #observa cuando se hace click en login
     observeEvent(input$b.login,{
          con <- conectar()
          query <- paste0("SELECT id, user,nombre,level FROM users WHERE baja = 0 AND
                          user = '", input$s.usuario ,
                          "' AND pass = '",sha1(input$s.contra, 10),"'")
          logins <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          if (nrow(logins)>0){
               level <<- logins$level
               nombre <<- logins$nombre
               id_user <<- logins$id
               cargar_menus()
          } else {
               # sendSweetAlert(session, "Incorrecto","Usuario o contraseña incorrectos",
               #                "error")
               output$menu.logged <- renderMenu({
                    sidebarMenu(h5("Usuario o contraseña incorrectos"))
               })
          }
     })
     
          #graficos de kpis ----------------------------------------------------------
          #kpi
          fun.kpi.tiempos <- reactive({
               if(is.null(input$frecuencia)) return(NULL)
               fechas <- tiempos(input$frecuencia)
               
               #fechas superior
               output$fechas.filtros <- renderText({
                    paste("Del ", ymd(fechas), "al", fecha.hoy)
               }) 
               
               kpi.tiempo <<- q.kpi.tiempo.proceso(id_usuario = id_user, 
                                                   date.inicio = fechas)
               return(kpi.tiempo)
          })
          output$p.solicitudes.promedio <- renderPlot({
               kpi.tiempo <- fun.kpi.tiempos()
               if(is.null(kpi.tiempo)) return(NULL)               
               if(nrow(kpi.tiempo)==0)return(NULL)               
               
               #leer metas
               con <- conectar()
               meta <- dbGetQuery(con, "SELECT valor, fecha_inicio FROM metas_kpi
                                   WHERE id_meta = 1")%>%arrange(fecha_inicio)
               dbDisconnect(con)
               meta$fecha_inicio <- ymd(meta$fecha_inicio)
               #meta$semana <- paste(year(meta$fecha_inicio),"-",week(meta$fecha_inicio))
               
               #crear valores
               fechas.meta <- data.frame("FECHA" =seq(min(kpi.tiempo$fecha_vacante), 
                                                      max(fecha.hoy+7), by = "week"))
               meta.solicitudes <- data.frame("FECHA" = fechas.meta)
               meta.solicitudes <- merge(meta.solicitudes, meta)%>%
                    mutate("META" = ifelse(FECHA>fecha_inicio, valor, 0),
                           "DIAS.DE.INICIO" = FECHA-fecha_inicio)%>%
                    filter(DIAS.DE.INICIO >0)%>%
                    group_by(FECHA)%>%
                    mutate("OK" = min(DIAS.DE.INICIO))%>%
                    filter(DIAS.DE.INICIO == OK)%>%
                    select(FECHA, META)%>%
                    merge(fechas.meta, by= "FECHA", all.y=T)%>%
                    replace(., is.na(.), 0)
               
               meta.solicitudes <- meta.solicitudes%>%
                    mutate("SEMANA" = paste(year(FECHA),"-",week(FECHA)))%>%
                    select(-FECHA)
               
               
               datos <- kpi.tiempo%>%
                    filter(proceso %in% "Solicitud")%>%
                    mutate("SEMANA" = paste(year(fecha),"-",week(fecha)))%>%
                    group_by(asesor, id_vacante, SEMANA)%>%
                    summarise("CANDIDATOS"= n())%>%
                    group_by(asesor, SEMANA)%>%
                    summarise("POR.VACANTE" = mean(CANDIDATOS))%>%
                    merge(meta.solicitudes, by = "SEMANA", all.x = T)
               
               
               ggplot(datos, aes(SEMANA, POR.VACANTE, group = 1)) + 
                    geom_point() +
                    geom_line(col = "dodgerblue2", lwd=1) + 
                    geom_smooth(stat = "smooth", method = "lm", se = F) + 
                    geom_line(aes(SEMANA, META), group =2, col = "green4") +
                    geom_area(aes(SEMANA, META), fill = "red", alpha = 0.1) +
                    theme(legend.position = 'top') + 
                    ylab("Solicitudes") + 
                    xlab("Año-Semana")
          })
          
          output$p.tiempos.proceso.score <- renderPlot({
               kpi.tiempo <- fun.kpi.tiempos()
               if(is.null(kpi.tiempo)) return(NULL)               
               if(nrow(kpi.tiempo)==0)return(NULL)               
               
               con <- conectar()
               meta <- dbGetQuery(con, "SELECT valor, fecha_inicio FROM metas_kpi
                                  WHERE id_meta = 2")%>%arrange(fecha_inicio)
               dbDisconnect(con)
               meta$fecha_inicio <- ymd(meta$fecha_inicio)
               #meta$semana <- paste(year(meta$fecha_inicio),"-",week(meta$fecha_inicio))
               
               #crear valores
               fechas.meta <- data.frame("FECHA" =seq(min(kpi.tiempo$fecha_vacante), 
                                                      max(fecha.hoy+7), by = "week"))
               meta.solicitudes <- data.frame("FECHA" = fechas.meta)
               meta.solicitudes <- merge(meta.solicitudes, meta)%>%
                    mutate("META" = ifelse(FECHA>fecha_inicio, valor, 0),
                           "DIAS.DE.INICIO" = FECHA-fecha_inicio)%>%
                    filter(DIAS.DE.INICIO >0)%>%
                    group_by(FECHA)%>%
                    mutate("OK" = min(DIAS.DE.INICIO))%>%
                    filter(DIAS.DE.INICIO == OK)%>%
                    select(FECHA, META)%>%
                    merge(fechas.meta, by= "FECHA", all.y=T, incomparables = 0)%>%
                    replace(., is.na(.), 0)
               
               meta.tiempo <- meta.solicitudes%>%
                    mutate("SEMANA" = paste(year(FECHA),"-",week(FECHA)))%>%
                    select(-FECHA)
               
               
               dias.prom.vacante <- kpi.tiempo%>%
                    filter(id_status ==2)%>%
                    mutate("SEMANA" = paste(year(fecha_vacante),"-",week(fecha_vacante)),
                           "INGRESO" = as_date(ifelse(proceso == "Ingreso", fecha, fecha.hoy)),
                           "DIAS.ABIERTA" = INGRESO-fecha_vacante)%>%
                    group_by(asesor, SEMANA, id_vacante)%>%
                    summarise("DIAS.VACANTE" = min(DIAS.ABIERTA))%>%
                    group_by(asesor, SEMANA)%>%
                    summarise("X.DIAS.VACANTE" = as.numeric(round(mean(DIAS.VACANTE),1)))%>%
                    merge(meta.tiempo, by = "SEMANA", all.x = T)
               
               
               ggplot(dias.prom.vacante, aes(SEMANA, X.DIAS.VACANTE, group = 1)) + 
                    geom_point() +
                    geom_line(lwd=1, col = "dodgerblue2") + 
                    geom_smooth(stat = "smooth", method = "lm", se = F) + 
                    geom_line(aes(SEMANA, META), group = 2, col = "green4") +
                    geom_area(aes(SEMANA, META), fill = "green3", alpha = 0.1) +
                    theme(legend.position = 'top') + 
                    ylab("Dias promedio") + 
                    xlab("Año-Semana")
          })
          
          output$p.porcentaje.cerradas <- renderPlot({
               kpi.tiempo <- fun.kpi.tiempos()
               if(is.null(kpi.tiempo)) return(NULL)               
               if(nrow(kpi.tiempo)==0)return(NULL)               
               
               con <- conectar()
               meta <- dbGetQuery(con, "SELECT valor, fecha_inicio FROM metas_kpi
                                  WHERE id_meta = 3")%>%arrange(fecha_inicio)
               dbDisconnect(con)
               meta$fecha_inicio <- ymd(meta$fecha_inicio)
               #meta$semana <- paste(year(meta$fecha_inicio),"-",week(meta$fecha_inicio))
               
               #crear valores
               fechas.meta <- data.frame("FECHA" =seq(min(kpi.tiempo$fecha_vacante), 
                                                      max(fecha.hoy+7), by = "week"))
               meta.solicitudes <- data.frame("FECHA" = fechas.meta)
               meta.solicitudes <- merge(meta.solicitudes, meta)%>%
                    mutate("META" = ifelse(FECHA>fecha_inicio, valor, 0),
                           "DIAS.DE.INICIO" = FECHA-fecha_inicio)%>%
                    filter(DIAS.DE.INICIO >0)%>%
                    group_by(FECHA)%>%
                    mutate("OK" = min(DIAS.DE.INICIO))%>%
                    filter(DIAS.DE.INICIO == OK)%>%
                    select(FECHA, META)%>%
                    merge(fechas.meta, by= "FECHA", all.y=T, incomparables = 0)%>%
                    replace(., is.na(.), 0)
               
               meta.cerradas <- meta.solicitudes%>%
                    mutate("SEMANA" = paste(year(FECHA),"-",week(FECHA)))%>%
                    select(-FECHA)
               
               vacantes <- kpi.tiempo%>%
                    mutate("SEMANA" = paste(year(fecha_vacante),"-",week(fecha_vacante)))%>%
                    group_by(asesor, SEMANA)%>%
                    summarise("VACANTES" = n_distinct(id_vacante))
               cerradas <- kpi.tiempo%>%
                    filter(proceso=="Ingreso")%>%
                    mutate("SEMANA" = paste(year(fecha_vacante),"-",week(fecha_vacante)))%>%
                    group_by(asesor, SEMANA)%>%
                    summarise("CERRADAS" = n_distinct(id_vacante))
               
               pct.cerradas <- merge(vacantes, cerradas, all.x = T)%>%
                    replace(., is.na(.), 0)%>%
                    mutate("SALDO" = VACANTES- CERRADAS)%>%
                    mutate("ACUM.VACANTES" = cumsum(VACANTES),
                           "ACUM.CERRADAS" = cumsum(CERRADAS),
                           "PCT.CERRADO" = round(ACUM.CERRADAS/ACUM.VACANTES*100,0))%>%
                    merge(meta.cerradas, by = "SEMANA", all.x=T)
               
                              
               ggplot(pct.cerradas, aes(SEMANA, PCT.CERRADO, group = 1)) + 
                    geom_point()+
                    geom_line(lwd=1, col = "dodgerblue2") + 
                    geom_smooth(stat = "smooth", method = "lm", se = F) + 
                    geom_area(aes(SEMANA, META), fill = "red", alpha = 0.1) +
                    geom_line(aes(SEMANA, META), col = 'green4') +
                    theme(legend.position = 'top') + 
                    ylab("Porcentaje") + 
                    xlab("Año-Semana") + 
                    coord_cartesian(ylim = c(0, 100))
               
          })
          
          #termina graficos de kpis-----------------------------------------------------------------
          
          #scoreboard ------------------------------------------------------------------------------
          #grafico tiempos de proceso

          output$p.tiempos.proceso <- renderPlot({
               if(is.null(input$frecuencia)) return(NULL)
               fechas <- tiempos(input$frecuencia)
               
               # if(is.null(input$score.reclut))return(NULL)
               # id_user<-c_reclutadores[c_reclutadores$nombre==input$score.reclut,]$id

               #fechas superior
               output$fechas.filtros <- renderText({
                    paste("Del ", ymd(fechas), "al",fecha.hoy)
               }) 
               
               kpi.tiempo <<- q.kpi.tiempo.proceso(id_usuario = id_user, 
                                                   date.inicio = fechas)  
               
               tiempo <- kpi.tiempo%>%
                    group_by(asesor, proceso)%>%
                    summarise(dias = round(mean(fecha-fecha_vacante),1))
                    
               output$ui.tiempo.promedio <- renderValueBox({
                    valueBox(
                         tiempo[tiempo$proceso=="Ingreso",]$dias ,"Dias proceso", 
                         icon = icon("clock-o"),
                         color = "light-blue"
                    )
                    
               })
               
               output$ui.dias.vacantes <- renderValueBox({
                    dias.vacantes <- kpi.tiempo%>%
                         filter(id_status==1)%>%
                         group_by(id_vacante)%>%
                         summarise("inicial" = min(fecha_vacante))%>%
                         mutate("dias.vacantes" = fecha.hoy-inicial)
                         
                    valueBox(round(mean(dias.vacantes$dias.vacantes),1), "Dias abiertas",
                                  icon= icon("calendar"),
                                  color = "orange"
                    )
               })
               
               output$p.razones.rechazo <- renderPlot({
                    razones <- kpi.tiempo%>%
                         filter(id_proceso==5)%>%
                         group_by(razon_rechazo)%>%
                         summarise("freq" = n())%>%
                         arrange(desc(freq))%>%
                         mutate("pct" = round(freq/sum(freq)*100,0), 
                                "max" = ifelse(freq==max(freq),1,0))
                    
                    #razones de rechazos
                    ggplot(razones,aes(razon_rechazo, pct)) + 
                         geom_bar(aes(fill = factor(max)), stat = 'identity')  + 
                         coord_flip() +
                         geom_text(aes(label = paste(" ",razon_rechazo,"-",pct,"% ")), hjust = "inward", size = 2.5, color = "black") +
                         #geom_text(aes(label = paste(pct,"%")), hjust = -0.05, size = 3, color = "black") +
                         ylab("") +
                         xlab("") +
                         theme(legend.position = "none", axis.text.y = element_blank(),
                               axis.text.x = element_blank())
               })

               ggplot(tiempo, aes(proceso, as.numeric(dias))) + 
                    geom_bar(stat = 'identity', fill = "turquoise3") + 
                    coord_flip() +
                    #geom_text(aes(label = as.numeric(dias)), vjust = -0.05, size = 3, color = "black") +
                    geom_text(aes(label = paste("",proceso,"-",as.numeric(dias),"")), hjust = "inward", size = 4, 
                              color = "black",
                              check_overlap = T) +
                    xlab("") + 
                    ylab("") +
                    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
          })
          
          output$ui.vacantes.abiertas <- renderValueBox({
               if(is.null(input$frecuencia)) return(NULL)
               fechas <- tiempos(input$frecuencia)
               
               consulta <- q.vacantes(id_status=c(1), id_usuario = id_user, all = F)
                    
               valueBox(
                    nrow(consulta) , "Abiertas",  icon = icon("list"),
                    color = "orange"
               )
          })
          
          output$ui.vacantes.cumplidas <- renderValueBox({          
               if(is.null(input$frecuencia)) return(NULL)
               fechas <- tiempos(input$frecuencia)
               
               consulta <- q.vacantes(id_status=c(1,2), 
                                      id_usuario = id_user, 
                                      all = F,
                                      date.inicio = fechas)
               
               output$ui.total.vacantes <- renderValueBox({
                    if(is.null(input$frecuencia)) return(NULL)
                    valueBox(
                         nrow(consulta) , "Total",  icon = icon("list"),
                         width = 2, color = "light-blue"
                    ) 
               })    

               valueBox(
                    paste0(round(nrow(consulta[consulta$status=="Cerrada",])/nrow(consulta)*100,0),"%") , 
                    "Cerradas",  icon = icon("thumbs-up"),
                    width = 6, color = "light-blue"
               ) 
          }) 
 

          
          output$p.embudo <- renderPlot({
               if(is.null(input$frecuencia)) return(NULL)
               fechas <- tiempos(input$frecuencia)

               embudo <<- q.embudo(id_usuario = id_user, date.inicio = fechas)   
               
               #solicitudes por vacante
               output$ui.solicitudes.vacante <- renderValueBox({
                    valor <- ifelse(nrow(embudo)==0, 0,
                    round(length(unique(embudo$candidato))/length(unique(embudo$id_vacante)),2))
                    valueBox(valor, 
                         "Solicitudes",  icon = icon("bullseye"),
                         width = 2, color = "light-blue"
                    )
                    
               })
               
               output$ui.costo.vacante <- renderValueBox({
                    if(is.null(input$frecuencia)) return(NULL)
                    gastado <- q.total.gastado(id_usuario = id_user, date.inicio = fechas) 
                    cerradas <- nrow(embudo%>%
                         filter(proceso == "Ingreso"))
                    
                    valueBox(round(gastado/cerradas,0), "Costo medio", icon = icon("usd"),
                             width = 6, color = "red"
                    )
               })
               
               #solicitudes por medios
               output$p.medios <- renderPlot({
                    p <- embudo%>%
                         filter(proceso %in% c("Solicitud","Ingreso"))%>%
                         group_by(proceso, medio)%>%
                         summarise("Candidatos" = n())
                    #p$medio <- gsub(" ","\n",p$medio)
               
                    ggplot(p, aes(medio, Candidatos, fill=proceso, label =Candidatos)) + 
                         geom_bar(stat = 'identity', position = 'dodge') +
                         xlab("") + 
                         ylab("") + 
                         theme(legend.position = "top", 
                               legend.title = element_blank(),
                               legend.text = element_text(size = 7),
                               legend.key.size = unit(.3, 'cm'),
                               axis.text.y = element_text(size = 7),
                               axis.text.x = element_blank()) + 
                         coord_flip() + 
                         geom_text(size = 3, hjust = "inward")
                              
               })
               
               #costo por medio
               output$p.costo.por.medio <- renderPlot({
                    if(is.null(input$frecuencia)) return(NULL)
                    fechas <- tiempos(input$frecuencia)
                    
                    costo.x.medio <- q.costo.por.medio(id_usuario = id_user, date.inicio = fechas) 
                    
                    ggplot(costo.x.medio, aes(medio, costo,label = paste0(medio,"- $",costo)), group = 1) + 
                         geom_bar(stat = 'identity',fill = "turquoise3") + 
                         coord_flip() +
                         geom_text(size = 2.5, hjust = 'inward') + 
                         xlab("") + 
                         ylab("") +
                         theme(legend.position = "none", axis.text.y = element_blank(),
                               axis.text.x = element_blank())
                         
               })
               
               output$p.en.proceso <- renderPlot({
                    seguimiento <- q.seguimiento(id_status = 1, id_usuario = id_user)
                    
                    p <- seguimiento%>%
                         group_by(orden_proceso, proceso)%>%
                         summarise("freq" = n())
                    p$proceso <- factor(p$proceso, unique((p%>%arrange((orden_proceso))%>%select(proceso))$proceso))
                    
                    ggplot(p, aes(proceso, 1, label = paste(proceso, "\n", freq), fill = proceso)) + 
                         geom_bar(stat = "identity") + 
                         geom_text(size=5, vjust = 2) + 
                         theme_void() +
                         theme(legend.position = "none", axis.text.y = element_blank(),
                               axis.text.x = element_blank(), 
                               plot.background = element_blank())
               })
               
               pp <- embudo%>%
                    filter(proceso != "Rechazo")%>%
                    group_by(orden, proceso,sexo)%>%
                    summarise("total" = n())%>%
                    ungroup()%>%
                    group_by(sexo)%>%
                    mutate("PCT" = total/max(total)*100)
               
               pp$proceso <- factor(pp$proceso, unique((pp%>%arrange(desc(orden))%>%select(proceso))$proceso))
               
               #embudo de reclutamiento
               ggplot(pp, aes(proceso, 
                              ifelse(sexo=="femenino",-PCT,PCT), fill = sexo, group = sexo, 
                              label = total)) + 
                    geom_bar(stat = 'identity')  + 
                    coord_flip() + 
                    scale_y_continuous(limits = max(pp$PCT) * c(-1,1)) + 
                    geom_line(lwd = 1, col = 'black') +
                    geom_label(size = 3) + 
                    ylab("") +
                    xlab("") +
                    theme(legend.position = 'top', axis.text.x = element_blank())
          })
          #termina kpi y scoreboard --------------------------------------------------------
          
          
          #Registro de avances del proceso--------------------------------------------------
          cargar.seguimiento <- function(){
               
               #las vacantes pueden estar abiertas o cerradas, pero no puede perderse
               #un candidato
               if (level == "super") {
                    seguimiento <<- q.seguimiento(id_status = c(1,2), all = T)%>%
                         mutate_if(is.character, as.factor)
               } else {
                    seguimiento <<- q.seguimiento(id_status = c(1), id_usuario = id_user, all = F)%>%
                         mutate_if(is.character, as.factor)
               }
               
               #formatear
               tabla.de.seguimiento <- seguimiento%>%
                    mutate("proceso" = paste0(orden_proceso,".",proceso))%>%
                    select(id_candidato, candidato, cliente, id_vacante, vacante, proceso, fecha)%>%
                    spread(proceso, fecha)%>%
                    arrange(cliente, vacante, candidato)
               row.names(tabla.de.seguimiento)<- tabla.de.seguimiento$id_candidato
               return(tabla.de.seguimiento)
               
          }
          
          output$tabla.seguimiento <- DT::renderDataTable({
               tabla.de.seguimiento <<- cargar.seguimiento()
               
               tabla.print <- tabla.de.seguimiento%>%
                    select(everything(), -id_candidato, -id_vacante)
               
               #formatear columnas
               tabla.print[,1:4] <- lapply(tabla.print[,1:4], factor)
               
               datatable(data = tabla.print,
                             rownames = T,
                             selection ='single', 
                             filter = "top",
                             autoHideNavigation = T,
                              extensions = 'Scroller',
                             options = list(dom = 't',
                                            scrollX = TRUE,
                                            scrollY = 400,
                                            scroller = TRUE,
                                            fixedHeader = TRUE))
          })
          
          #detalle de candidatos y seguimiento de procesos
          observeEvent(c(input$tabla.seguimiento_rows_selected),{
               
               ren <- input$tabla.seguimiento_rows_selected
               id_cand <- row.names(tabla.de.seguimiento[ren,])
               id_vacante <- tabla.de.seguimiento[ren,]$id_vacante
               
               con <- conectar()
               consulta <- q.candidatos(id_candidatos = id_cand)%>%
                    mutate_if(is.character, as.factor)
               dbDisconnect(con)
               
               output$TDetalleCandidatos <- renderUI({
                    edad <- as.numeric(floor((fecha.hoy-ymd(consulta$fecha_nacimiento))/365))
                    detalles.candidato <- paste("<b>Nombre:</b> ", consulta$nombre ,
                                                "<br/><b>Direccion:</b> ", consulta$direccion, "C.P. ", consulta$cp,
                                                "<br/><b>Edad:</b> ", edad ,"    <br/><b>Sexo:</b> ", consulta$sexo, 
                                                "<br/><b>Escolaridad:</b> ", consulta$escolaridad,
                                                "<br/><b>Adquisicion por: </b>", consulta$medio)
                    HTML(paste(detalles.candidato))
               })
               
               output$ultimo.proceso <- renderText({
                    orden.ultimo.proceso <- max(seguimiento[seguimiento$id_candidato==id_cand,]$orden_proceso)
                    
                    #solo permite procesos más adelantados
                    updatePickerInput(session, "Cproceso", 
                                      choices = c_procesos[c_procesos$orden>orden.ultimo.proceso,]$nombre)
                    
                    paste("Ultimo proceso:",
                          seguimiento[seguimiento$id_candidato==id_cand & 
                                           seguimiento$orden_proceso== orden.ultimo.proceso,]$proceso
                    )
               })
               

          })
          
          #guardar avance - mensaje confirmacion
          observeEvent(input$cmd.guardar.proceso,{
               ren <- input$tabla.seguimiento_rows_selected
               id_proceso <- c_procesos[c_procesos$nombre == input$Cproceso,]$id
               
               if(input$Cproceso == "" | is.null(input$CFecha)){
                    # sendSweetAlert(session, "Error", "No se ha seleccionado un proceso o fecha que registrar",
                    #                type = "error")
                    shinyalert(title = "Error",
                               "No se ha seleccionado un proceso o fecha que registrar",
                               type = "error",animation = TRUE)
               } else {
                    if(id_proceso==4){ #si se cierra la vacante
                         msg <- "¿Estas seguro de registrar el proceso de ingreso? \n
                         Esto cerrara la vacante y los otros candidatos asignados a este proceso se moveran a la bolsa de candidatos"     
                    } else {  #confirma normal
                         msg <- "¿Estas seguro de registrar la actualizacion al proceso de seleccion?"
                    }
                    
                    shinyalert(title = "Confirmar", 
                                   text =  msg,
                                   closeOnEsc = TRUE,
                                   closeOnClickOutside = FALSE,
                                   html = FALSE,
                                   type = "warning",
                                   timer = 0,
                                   animation = TRUE,
                                   showConfirmButton = TRUE,
                                   showCancelButton = TRUE,
                                   confirmButtonText = "Si, quiero registrarlo",
                                   confirmButtonCol = "#AEDEF4",
                                   cancelButtonText = "No, cancela registro",
                                   callbackR = function(x) if(x==T) modificar.proceso())
               }
          })
          
          modificar.proceso <- function(){
               proceso <- input$Cproceso
               fecha <- gsub("-","",ymd(input$CFecha))
               
               ren <- input$tabla.seguimiento_rows_selected
               id_candidato <- row.names(tabla.de.seguimiento[ren,])
               id_vacante <- tabla.de.seguimiento[ren,]$id_vacante
               id_proceso <- c_procesos[c_procesos$nombre == proceso,]$id
               comentarios <- "sin comentarios"
               if (id_proceso==5) {
                    id_razon_rechazo <- c_rechazo[c_rechazo$nombre== input$rechazo,]$id }
               else {id_razon_rechazo <- 0 }
               
               #solo actualiza procreso
               qinsert <- paste0("INSERT INTO vacantes_following
                                 (id_candidato, id_vacante, id_proceso, fecha, comentarios, id_razon_rechazo) ",
                                 "VALUES (",
                                 id_candidato, ",", 
                                 id_vacante, ",", 
                                 id_proceso , ",", 
                                 fecha, ",'",
                                 comentarios, "',",
                                 id_razon_rechazo, ")")
               con <- conectar()
               dbExecute(con,qinsert)

               
               #si ingreso, dar de baja la vacante tambien
               if (id_proceso==4) {
                    qupdate <- paste0("UPDATE vacantes SET id_status = 2 WHERE id= ", id_vacante)
                    dbExecute(con,qupdate)
               }
               dbDisconnect(con)
               
               #actualiza la tabla tambien
               output$tabla.seguimiento <- DT::renderDataTable({
                    tabla.de.seguimiento <<- cargar.seguimiento()
                    
                    tabla.print <- tabla.de.seguimiento%>%
                         select(everything(), -id_candidato, -id_vacante)
                    
                    #formatear columnas
                    tabla.print[,1:4] <- lapply(tabla.print[,1:4], factor)
                    
                    datatable(data = tabla.print,
                              rownames = T,
                              selection ='single', 
                              filter = "top",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 'ft',
                                             scrollX = TRUE,
                                             scrollY = 400,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
          }      
          
          #ASIGNACION DE CANDIDATOS A VACANTES ---------------------------------------------
          cargar.bolsa.candidatos <- function(){
               db.bolsa.candidatos <- q.bolsa.vacantes()
               
               #filtrar rechazados 
               if(!is.null(input$ver.rechazados)) if(input$ver.rechazados==F) {
                    db.bolsa.candidatos <- db.bolsa.candidatos%>%filter(id_proceso != 5)     
               }
               
               db.bolsa.candidatos <- db.bolsa.candidatos%>%
                    mutate_if(is.character, as.factor)%>%
                    arrange(nombre)%>%
                    tibble::column_to_rownames("id")%>%
                    select(-id_proceso)
               
               return(db.bolsa.candidatos)
          }
          
          output$tabla.bolsa.candidatos <- DT::renderDataTable({
               forzar <- input$cnf.guardar.proceso  #forza actualizacion al cerrar vacante
               db.bolsa.candidatos<<-cargar.bolsa.candidatos()
               
               datatable(data = db.bolsa.candidatos,
                         rownames = T,
                         selection ='single', 
                         filter = "top",
                         autoHideNavigation = T,
                         extensions = 'Scroller',
                         options = list(dom = 't',
                                        scrollX = TRUE,
                                        scrollY = 300,
                                        scroller = TRUE,
                                        fixedHeader = TRUE))
          })
          
          cargar.bolsa.vacantes <- function(){
               
               db.bolsa.vacantes <- q.vacantes(id_status = 1,id_usuario = id_user, all = F)%>%
                    mutate_if(is.character, as.factor)%>%
                    arrange(cliente)%>%
                    tibble::column_to_rownames("id")%>%
                    select(cliente, vacante, codigo_postal, fecha)
               return(db.bolsa.vacantes)
          }
          
          output$tabla.bolsa.vacantes <- DT::renderDataTable({
               forzar <- input$cnf.guardar.proceso  #forza actualizacion al cerrar vacante
               db.bolsa.vacantes <<- cargar.bolsa.vacantes()

               datatable(data = db.bolsa.vacantes,
                         rownames = T,
                         selection ='single', 
                         filter = "top",
                         autoHideNavigation = T,
                         extensions = 'Scroller',
                         options = list(dom = 't',
                                        scrollX = TRUE,
                                        scrollY = 200,
                                        scroller = TRUE,
                                        fixedHeader = TRUE))
               
          })
          
          observeEvent(c(input$tabla.bolsa.vacantes_rows_selected,
                         input$tabla.bolsa.candidatos_rows_selected),{
          
               ren.vac <- input$tabla.bolsa.vacantes_rows_selected
               id.vac <- row.names(db.bolsa.vacantes)[ren.vac]
               ren.cand <- input$tabla.bolsa.candidatos_rows_selected
               id.cand <- row.names(db.bolsa.candidatos)[ren.cand]
               
               output$msg.asignacion <- renderText({
                    paste("<br>Asignando a la vacante <b>", db.bolsa.vacantes[ren.vac,]$cliente, 
                    "</b>los siguientes candidatos <br><b>", 
                    paste(db.bolsa.candidatos[ren.cand,]$nombre, collapse = "<br>"),"</b>")
                    
               })
                              
          })
          
          observeEvent(input$guardar.asignacion, {
               ren.vac <- input$tabla.bolsa.vacantes_rows_selected
               id.vac <- row.names(db.bolsa.vacantes)[ren.vac]
               if(is.null(ren.vac)) return(NULL)
               
               ren.cand <- input$tabla.bolsa.candidatos_rows_selected
               id.cand <- row.names(db.bolsa.candidatos)[ren.cand]
               if(is.null(ren.cand)) return(NULL)
               
               #preguntar uno por uno, desde donde comenzar nuevamente el proceso y copiar
               #los procesos ya realizado, con fecha de hoy (ayudará a su indicador)
               
               query <- paste0("SELECT cand.nombre, vf.*, vp.orden, vp.nombre proceso
                         FROM vacantes_following vf
                         LEFT JOIN vacantes_procesos vp ON vp.id = vf.`id_proceso`
                         LEFT JOIN candidatos cand ON cand.id = vf.`id_candidato`
                         WHERE vf.`id_candidato` IN (", paste0(id.cand, collapse=",") , ")")
               con <- conectar()
               datos.asignacion <<- dbGetQuery(con, query)
               dbDisconnect(con)
               
               if(nrow(datos.asignacion[datos.asignacion$id_candidato== id.cand,])>1) { #llevo varios procesos
                    todos <- datos.asignacion%>%filter(id_candidato== id.cand)%>%arrange(orden)%>%select(proceso)
                    
                    showModal(selectinputModal(nombre = unique(datos.asignacion[datos.asignacion$id_candidato== id.cand,]$nombre),
                                               todos.procesos = todos), session)
                    
               } else { fun.guardar.asignacion(datos = datos.asignacion) }
               
          })
          
          observeEvent(input$ok.asignar,{
               removeModal(session)
               cuales.dejar <- c_procesos%>%
                    filter(orden <= max(c_procesos[c_procesos$nombre ==input$que.proceso,]$orden))%>%
                    select(id)
               datos.asignacion <- datos.asignacion%>%
                    filter(id_proceso == cuales.dejar$id)
               fun.guardar.asignacion(datos = datos.asignacion)
          })
          
          fun.guardar.asignacion <- function(datos = NULL){
               #vacante nueva
               ren.vac <- input$tabla.bolsa.vacantes_rows_selected
               id.vac <- row.names(db.bolsa.vacantes)[ren.vac]
               
               #solo actualiza procreso con fecha de hoy
               num.cand <- length(datos$id_proceso)
               datos<- datos%>%mutate("nva.vacante" = id.vac, "fecha.nva" = gsub("-","",ymd(fecha.hoy)))
               valores <- paste(do.call(paste, 
                                        c(datos%>%
                                               select(id_candidato, nva.vacante, id_proceso, fecha.nva), 
                    sep = ",")),collapse = "),(")
               cat(paste("Registrando asignacion", valores ,"\n"))
               
               qinsert <- paste0("INSERT INTO vacantes_following
                                 (id_candidato, id_vacante, id_proceso, fecha) ",
                                 "VALUES (", valores, ")")
               con <- conectar()
               dbExecute(con,qinsert)
               
               #marcar resgistro anterior como cambio de vacante
               qupdate <- paste0("UPDATE vacantes_following
                                 SET cambio_vacante = 1
                                 WHERE id IN (", paste0(datos$id, collapse = ",") ,")")
               dbExecute(con, qupdate)
               
               dbDisconnect(con)
               
               #actualiza tabla de candidatos
               output$tabla.bolsa.candidatos <- DT::renderDataTable({
                    forzar <- input$cnf.guardar.proceso  #forza actualizacion al cerrar vacante
                    db.bolsa.candidatos<<-cargar.bolsa.candidatos()
                    
                    datatable(data = db.bolsa.candidatos,
                              rownames = T,
                              selection ='single', 
                              filter = "top",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 't',
                                             scrollX = TRUE,
                                             scrollY = 300,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
          }
          
          selectinputModal <- function(failed = FALSE, nombre = NULL, 
                                       todos.procesos = NULL) {
               modalDialog(
                    selectInput("que.proceso", HTML(
                                paste("<b>", nombre,  "</b><br>llego hasta el proceso de <b>", tail(todos.procesos,1)$proceso , 
                                      "</b><br>¿Cual es la ultima etapa del proceso anterior que reutilizaras?")),
                              choices = todos.procesos$proceso, multiple = F, selected = tail(todos.procesos,1)$proceso),
                    footer = tagList(
                         modalButton("Cancelar"),
                         actionButton("ok.asignar", "Asignar")
                    )
               )
          }
          #TERMINA ASIGNACION DE CANDIDATOS A VACANTES----------------------------------------
          
          #ABC CANDIDATOS ---------------------------------------------------------------------
          cargar.candidatos <- function(){
               con <- conectar()
               db.tabla.candidatos <- q.candidatos(mis.candidatos=T)%>%
                    mutate_if(is.character, as.factor)
               dbDisconnect(con)
               
               #row.names(db.tabla.candidatos) <- db.tabla.candidatos$id
               db.tabla.candidatos$fecha_nacimiento <- ymd(db.tabla.candidatos$fecha_nacimiento)
               db.tabla.candidatos <- db.tabla.candidatos%>%
                    arrange(nombre)%>%
                    tibble::column_to_rownames("id")
               
               updatePickerInput(session, "Csexo", choices = c_sexo$nombre)
               updatePickerInput(session, "Cescolaridad", choices = c_escolaridad$nombre)
               updatePickerInput(session, "Cmedio", choices = c_medio$nombre)
 
               return(db.tabla.candidatos)
          }
          
          output$tabla.candidatos <- DT::renderDataTable({
               db.tabla.candidatos <<- cargar.candidatos()
               
               datatable(data = db.tabla.candidatos,
                         rownames = T,
                         selection = list(mode='single',
                              target = 'row'),
                         filter = "top",
                         autoHideNavigation = T,
                         extensions = 'Scroller',
                         options = list(dom = 't',
                                        scrollX = TRUE,
                                        scrollY = 400,
                                        scroller = TRUE,
                                        fixedHeader = TRUE))
          })

          
          #detalle del candidato y seguimiento de procesos
          observeEvent(input$tabla.candidatos_rows_selected, {
               ren <- input$tabla.candidatos_rows_selected
               if(is.null(ren)) return(NULL)
               id_cand <- row.names(db.tabla.candidatos[ren,])
               consulta <- db.tabla.candidatos[ren,]
               
               #cargar en controles para editar, borrar o nuevo
               updateTextInput(session, "Tid", value = id_cand)               
               updateTextInput(session, "Tnombre", value = consulta$nombre)
               updateTextInput(session, "Tdireccion", value = consulta$direccion)
               updateTextInput(session, "Tcp", value = consulta$cp)
               updateDateInput(session, "Tnacimiento", value = ymd(consulta$fecha_nacimiento))
               updatePickerInput(session, "Cescolaridad", selected = consulta$escolaridad)
               updatePickerInput(session, "Csexo", selected = consulta$sexo)
               updatePickerInput(session, "Cmedio", selected = consulta$medio)
               updatePickerInput(session, "Cvacantes", selected = paste(tabla.de.seguimiento[ren,]$cliente,"-",tabla.de.seguimiento[ren,]$vacante))
               
               #quitar combos de asignacion de candidatos
               output$ui.cvacantes <- renderUI({
                    return(NULL)
               })
               output$ui.ccliente <- renderUI({
                    return(NULL)
               })

          })

          observeEvent(input$cmd.nuevo.candidato,{  #reset de candidatos (nuevo candidato)
               req(input$cmd.nuevo.candidato)
               
               updateTextInput(session, "Tid", value = "")
               updateTextInput(session, "Tnombre", placeholder = "Nombre del candidato", value = "")
               updateTextInput(session, "Tdireccion", placeholder = "Calle, No. Colonia, Ciudad, Estado, Pais", value = "")
               updateTextInput(session, "Tcp", placeholder = "CP 5 digitos", value = "")
               updateDateInput(session, "Tnacimiento", value = fecha.hoy)
               
               proxy = dataTableProxy('tabla.candidatos', session)
               selectRows(proxy, selected = NULL)
               
               #muestra combos de asignacion a vacante
               
               output$ui.cvacantes <- renderUI({
                    vacantes <- q.vacantes(id_status = 1,id_usuario = id_user, all = F)
                    pickerInput("Cvacantes","Vacantes disponibles",
                                choices = unique(vacantes$vacante),
                                options = list('dropupAuto' = T, 'mobile'=T))
               })
               
          })
          
          observeEvent(c(input$Cvacantes,input$cmd.nuevo.candidato),{
               if(is.null(input$Cvacantes)) return(NULL)
               
               vacantes <- q.vacantes(id_status = 1,id_usuario = id_user, all = F)%>%
                    filter(vacante == input$Cvacantes)
               
               output$ui.ccliente <- renderUI({
                    pickerInput("Cclientes","Clientes",
                                choices = unique(vacantes$cliente),
                                options = list('dropupAuto' = T, 'mobile'=T))
               })
          })
          
          observeEvent(input$cmd.borrar.candidato, {
               req(input$cmd.borrar.candidato)
               id <- input$Tid
               cat(paste("Borrando candidato:", id), "\n")
               if(id !=""){

                    shinyalert(title = "Confirmar", 
                               text =  "¿Estas seguro de borrar este candidato?",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               html = FALSE,
                               type = "warning",
                               timer = 0,
                               animation = TRUE,
                               showConfirmButton = TRUE,
                               showCancelButton = TRUE,
                               confirmButtonText = "Si, eliminar candidato",
                               confirmButtonCol = "#AEDEF4",
                               cancelButtonText = "No, cancela eliminar",
                               callbackR = function(x) if(x==T) eliminar.candidato())
               }
          })
          
          eliminar.candidato <- function(){
               id <- input$Tid
               qupdate <- paste0("UPDATE candidatos SET baja = 1 WHERE id = " , id)
               
               con <- conectar()
               dbExecute(con,qupdate)
               dbDisconnect(con) 
               
               #quita seleccion
               proxy = dataTableProxy('tabla.seguimiento', session)
               selectRows(proxy, selected = NULL)
               
                #actualiza tabla                   
               db.tabla.candidatos <<- cargar.candidatos()
               
               output$tabla.candidatos <- DT::renderDataTable({
                    datatable(data = db.tabla.candidatos,
                              rownames = T,
                              selection = list(mode='single',
                                               target = 'row'),
                              filter = "top",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 't',
                                             scrollX = TRUE,
                                             scrollY = 400,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
               
               output$tabla.seguimiento <- DT::renderDataTable({
                    tabla.de.seguimiento <<- cargar.seguimiento()
                    
                    tabla.print <- tabla.de.seguimiento%>%
                         select(everything(), -id_candidato, -id_vacante)
                    
                    #formatear columnas
                    tabla.print[,1:4] <- lapply(tabla.print[,1:4], factor)
                    
                    datatable(data = tabla.print,
                              rownames = T,
                              selection ='single', 
                              filter = "top",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 't',
                                             scrollX = TRUE,
                                             scrollY = 400,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
          }

          observeEvent(input$cmd.guardar.candidato,{  #boton guardar
               req(input$cmd.guardar.candidato)
               id <- input$Tid
               
               msg <- ""
               if((fecha.hoy - input$Tnacimiento)/365 < 15) msg = "El candidato tiene menos de 15 años, favor de verificar"
               if(grepl("[^0-9]", input$Tcp) | nchar(input$Tcp)!=5) msg = "El codigo postal debe ser un numero de 5 digitos"
               if(input$Tnombre =="" | input$Tdireccion=="") msg = "Todos los campos deben ser llenados para registrar un candidato"
               
               if(msg!=""){
                    shinyalert("Error", msg,type = "error")
                    return(NULL)
               }
               
               if(id==""){ #si es nuevo - se confirma que se agregará el paso inicial como realizado
                    msg = "¿Estás seguro de querer guardar el registro de este candidato nuevo?"

               } else {
                    msg = "¿Estas seguro de guardar los cambios realizados?"
               }
               
               shinyalert(title = "Confirmar", 
                          text =  msg,
                          closeOnEsc = TRUE,
                          closeOnClickOutside = FALSE,
                          html = FALSE,
                          type = "warning",
                          timer = 0,
                          animation = TRUE,
                          showConfirmButton = TRUE,
                          showCancelButton = TRUE,
                          confirmButtonText = "Si, guardar",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela",
                          callbackR = function(x) if(x==T) guardar.candidatos())
               
               #actualiza tabla de candidato
               if(id==""){
                    output$tabla.seguimiento <- DT::renderDataTable({
                         tabla.de.seguimiento <<- cargar.seguimiento()
                         
                         tabla.print <- tabla.de.seguimiento%>%
                              select(everything(), -id_candidato, -id_vacante)
                         
                         #formatear columnas
                         tabla.print[,1:4] <- lapply(tabla.print[,1:4], factor)
                         
                         datatable(data = tabla.print,
                                   rownames = T,
                                   selection ='single', 
                                   filter = "top",
                                   autoHideNavigation = T,
                                   extensions = 'Scroller',
                                   options = list(dom = 't',
                                                  scrollX = TRUE,
                                                  scrollY = 400,
                                                  scroller = TRUE,
                                                  fixedHeader = TRUE))
                    })
               }    
          })
          
          guardar.candidatos <- function(id){ #confirmar guardar
               id <- input$Tid
               cat(paste("Candidatos a modificar:", id), "\n")
               
               id_sexo <- c_sexo[c_sexo$nombre == input$Csexo,1]
               id_escolaridad <- c_escolaridad[c_escolaridad$nombre == input$Cescolaridad,1]
               id_medio <- c_medio[c_medio$nombre == input$Cmedio, 1]
               
               #mayusculas, quita acentos y signos de puntuacion
               nombre.candidato <- gsub("[[:punct:]]","", stri_replace_all_regex(toupper(input$Tnombre), 
                                                                                 c('À','È','Í','Ó','Í','Ñ'),
                                                                                 c('A','E','I','O','U','N'), 
                                                                                vectorize_all = F))
               con <- conectar()              
               if(id== ""){  #nuevo registro
                    if(is.null(input$Cvacantes)) { #no se presionó agregar nuevo y no se puede seleccionar vacante
                         shinyalert("Error", "Presiona el boton (+) para agregar un nuevo candidato", type = "error")
                         return(NULL)
                    }
                         
                    #insertar candidato y crear proceso inicial con fecha de hoy
                    qinsert <- paste0("INSERT INTO candidatos
                                      (nombre, direccion, cp, fecha_nacimiento, id_sexo, id_escolaridad,
                                        id_medio) ",
                                      "VALUES ('",
                                      nombre.candidato, "', '",toupper(input$Tdireccion), "', ", input$Tcp, ", ",
                                      gsub("-","", ymd(input$Tnacimiento)), ", ", id_sexo, ", ",
                                      id_escolaridad, ", ", id_medio, ")")
                    dbExecute(con,qinsert)
                    
                    #id de ultimo candidato agregado
                    id.candidato <- dbGetQuery(con,"SELECT id, nombre FROM candidatos ORDER BY id DESC LIMIT 1")
                    
                    query = paste0("SELECT vac.id
                                             FROM vacantes vac
                                   LEFT JOIN vacantes_nombre vn ON vn.id = vac.id_nombre_vacante
                                   LEFT JOIN clientes cl ON cl.id = vac.id_cliente
                                   WHERE cl.nombre = '", input$Cclientes,"' AND
                                   vn.nombre = '", input$Cvacantes , "' AND
                                   vac.id_status = 1 AND
                                   vac.id_usuario = ", id_user)
                    id_vacante <- dbGetQuery(con, query)
                    
                    qinsert <- paste0("INSERT INTO vacantes_following
                                     (id_candidato, id_vacante, id_proceso, fecha)
                                     VALUES (",
                                     id.candidato$id, ",", id_vacante$id, ",", 1, ",", gsub("-","", ymd(fecha.hoy)),")")
                    
                    dbExecute(con,qinsert)
                    
               } else {  #actualizar registro
                    qupdate <- paste0("UPDATE candidatos 
                                      SET nombre = '", input$Tnombre,
                                      "' ,direccion = '" , input$Tdireccion,
                                      "' ,cp = ", input$Tcp, 
                                      " ,fecha_nacimiento = ", gsub("-","", ymd(input$Tnacimiento)),
                                      " ,id_sexo = ", id_sexo,
                                      " ,id_escolaridad = ", id_escolaridad,
                                      " ,id_medio = " , id_medio,
                                      " WHERE id = " , id)
 
                    dbExecute(con,qupdate)
               }
                dbDisconnect(con) 
                
                #actualiza tabla                   
                db.tabla.candidatos <<- cargar.candidatos()
                
                output$tabla.candidatos <- DT::renderDataTable({
                     datatable(data = db.tabla.candidatos,
                               rownames = T,
                               selection = list(mode='single',
                                                target = 'row'),
                               filter = "top",
                               autoHideNavigation = T,
                               extensions = 'Scroller',
                               options = list(dom = 't',
                                              scrollX = TRUE,
                                              scrollY = 400,
                                              scroller = TRUE,
                                              fixedHeader = TRUE))
                })
                
          }
          #Termina ABC candidatos---------------------------------------------------------------
          
          #GASTOS ------------------------------------------------------------------------------
          
          cargar.gastos <- function(){
               if(is.null(input$fechas.filtros.gastos)) return(NULL)
               fechas <- tiempos(input$fechas.filtros.gastos)
               
               
               gastos <- q.gastos(id_usuario = id_user, date.inicio = fechas)
               gastos$fecha <- ymd(gastos$fecha)
               row.names(gastos)<- gastos$id
               
               #cargar combos - medios y conceptos de gastos
               updatePickerInput(session,"gastos.conceptos",
                                 choices = c_concepto.gastos$nombre)
               updatePickerInput(session,"gastos.medios",
                                 choices =c_medio$nombre)
               return(gastos)
          }
          

          output$tabla.gastos <- renderDataTable({
               gastos <- cargar.gastos()
               if(is.null(gastos)) return(NULL)
               
               db.tabla.gastos <<- gastos%>%
                    mutate("prorrateo" = ifelse(id_comun ==0,"","SI"))%>%
                    select(id,concepto, "medio.asignado" = medio_asignado, fecha, monto, prorrateo)%>%
                    mutate_if(is.character, as.factor)%>%
                    tibble::column_to_rownames('id')
               
               datatable(data = db.tabla.gastos,
                         rownames = T,
                         selection = list(mode='single',
                                          target = 'row'),
                         filter = "top",
                         autoHideNavigation = T,
                         extensions = 'Scroller',
                         options = list(dom = 't',
                                        scrollX = TRUE,
                                        scrollY = 400,
                                        scroller = TRUE,
                                        fixedHeader = TRUE))
          })
          
          observeEvent(input$tabla.gastos_rows_selected, {
               
               ren <- input$tabla.gastos_rows_selected
               if(is.null(ren)) return(NULL)
               id_gasto <- row.names(db.tabla.gastos[ren,])
               cat(paste("Seleccionado gastos id:", id_gasto , "\n"))
               
               #llenar datos arriba               
               updateTextInput(session, "Gid", value = id_gasto)
               updatePickerInput(session,"gastos.conceptos",selected = gastos[gastos$id == id_gasto,]$concepto)
               updateDateInput(session, inputId = "fecha.gasto", value = gastos[gastos$id == id_gasto,]$fecha)
          
               #si prorrateado
               if(gastos[gastos$id == id_gasto,]$id_comun==0){  #sin prorrateo
                    seleccion <- gastos[gastos$id == id_gasto,]$medio_asignado
                    monto <- gastos[gastos$id== id_gasto,]$monto
               } else {
                    seleccion <- gastos[gastos$id_comun == gastos[gastos$id == id_gasto,]$id_comun,]$medio_asignado
                    monto <- gastos[gastos$id== id_gasto,]$monto*length(seleccion)
               }
               
               updatePickerInput(session,"gastos.medios", selected = seleccion)
               updateTextInput(session, "gasto.monto", value = monto)
               
          })
          
          observeEvent(input$cmd.nuevo.gasto,{
               #llenar datos arriba
               updateTextInput(session, "Gid", value = "")
               updateTextInput(session, "gasto.monto", value = "")
               updatePickerInput(session,"gastos.conceptos",selected = NULL)
               updateDateInput(session, "fecha.gasto",value = fecha.hoy)
               updatePickerInput(session,"gastos.medios",selected = "")
               
               #quita renglon seleccionado
               proxy = dataTableProxy('tabla.gastos', session)
               selectRows(proxy, selected = NULL)
          })
          
          observeEvent(input$cmd.borrar.gasto, {
               req(input$cmd.borrar.gasto)
               id <- input$Gid
               cat(paste("Borrando gasto:", id), "\n")
               if(id !=""){
                    
                    #si prorrateado
                    id_comun <- gastos[gastos$id == id,]$id_comun
                    texto <- "¿Estas seguro de borrar este gasto?"
                    if(id_comun!=0){  #sin prorrateo
                         id <- paste(paste(as.integer(gastos[gastos$id_comun == id_comun,]$id), collapse = ","))
                         texto <- "Esta gasto fue prorrateado en varios medios, se borraran todos estos prorrateos.\n
                         ¿Estas seguiro de borrar este gasto?"
                    }
                    
                    shinyalert(title = "Confirmar", 
                               text =  texto,
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               html = FALSE,
                               type = "warning",
                               timer = 0,
                               animation = TRUE,
                               showConfirmButton = TRUE,
                               showCancelButton = TRUE,
                               confirmButtonText = "Si, eliminar gasto",
                               confirmButtonCol = "#AEDEF4",
                               cancelButtonText = "No, cancela eliminar",
                               callbackR = function(x) if(x==T) eliminar.gasto())
               }
          })
          
          eliminar.gasto <- function(){  #revisar si requiere nivel para hacer esto
               id <- input$Gid

               #si prorrateado
               id_comun <- gastos[gastos$id == id,]$id_comun
               if(id_comun!=0){  #sin prorrateo
                    id <- paste(paste(as.integer(gastos[gastos$id_comun == id_comun,]$id), collapse = ","))
               }
               
               qupdate <- paste0("UPDATE gastos SET baja = 1 WHERE id IN (" , id, ")")
               
               con <- conectar()
               dbExecute(con,qupdate)
               dbDisconnect(con) 
               
               #quita seleccion
               proxy = dataTableProxy('tabla.gastos', session)
               selectRows(proxy, selected = NULL)
               updateTextInput(session, "Gid", value = "")
               
               #actualiza tabla                   
               output$tabla.gastos <- renderDataTable({
                    gastos <<- cargar.gastos()
                    if(is.null(gastos)) return(NULL)
                    
                    db.tabla.gastos <<- gastos%>%
                         mutate("prorrateo" = ifelse(id_comun ==0,"","SI"))%>%
                         select(id,concepto, "medio.asignado" = medio_asignado, fecha, monto, prorrateo)%>%
                         mutate_if(is.character, as.factor)%>%
                         tibble::column_to_rownames('id')
                    
                    datatable(data = db.tabla.gastos,
                              rownames = T,
                              selection = list(mode='single',
                                               target = 'row'),
                              filter = "top",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 't',
                                             scrollX = TRUE,
                                             scrollY = 400,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
          }
          
          observeEvent(input$cmd.guardar.gasto,{  #boton guardar
               req(input$cmd.guardar.gasto)
               id <- input$Gid
               
               if(id==""){ #si es nuevo - se confirma que se agregará el paso inicial como realizado
                    msg = "¿Estás seguro de querer guardar este gasto nuevo?"
                    
               } else {
                    msg = "¿Estas seguro de guardar los cambios realizados?"
               }
               
               shinyalert(title = "Confirmar", 
                          text =  msg,
                          closeOnEsc = TRUE,
                          closeOnClickOutside = FALSE,
                          html = FALSE,
                          type = "warning",
                          timer = 0,
                          animation = TRUE,
                          showConfirmButton = TRUE,
                          showCancelButton = TRUE,
                          confirmButtonText = "Si, guardar",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela",
                          callbackR = function(x) if(x==T) guardar.gastos())
               
          })
          
          guardar.gastos <- function(){ #confirmar guardar
               id <- input$Gid
               cat(paste("Candidatos a modificar:", id), "\n")
               
               monto <- as.numeric(gsub("[^0-9\\.]","",input$gasto.monto))  #quita simbolos, deja solo el punto
               id_medio <- as.integer(c_medio[c_medio$nombre %in% input$gastos.medios, 1])
               num.medios <- length(id_medio)
               id_concepto <- as.integer(c_concepto.gastos[c_concepto.gastos$nombre == input$gastos.conceptos,1])

               datos <- paste(do.call(paste, c(data.frame(
                    id_cliente = rep(0,num.medios),
                    id_concepto = rep(id_concepto,num.medios),
                    id_medio = id_medio,
                    id_asesor = rep(as.integer(id_user),num.medios),
                    id_comun = ifelse(num.medios==1, 
                                      rep(0,num.medios),
                                      rep(format(Sys.time(), "%H%M%S"),num.medios)),
                    fecha = rep(gsub("-","",input$fecha.gasto),num.medios),
                    monto = rep(round(monto/num.medios,2),num.medios)), 
                    sep = ",")),collapse = "),(")
                    
               con<- conectar()  
               
               if(id!= ""){  #modificando, no nuevo - borra y agrega nuevo
                    
                    msg <- ""
                    if(input$gasto.monto =="") msg = "Todos los campos deben ser llenados para registrar un gasto"
                    if(msg!=""){
                         sendSweetAlert(session, "Error", msg , type = "error")
                         dbDisconnect(con)
                         return(NULL)
                    }
                    
                    #si prorrateado debe borrar varios renglones
                    id_comun <- gastos[gastos$id == id,]$id_comun
                    if(id_comun!=0){  #sin prorrateo
                         id <- paste(paste(as.integer(gastos[gastos$id_comun == id_comun,]$id), collapse = ","))
                    }
                    
                    #borrar todos y guardar como nuevos (es mucho más sencillo que actualizar)
                    qdelete <- paste0("DELETE FROM gastos WHERE id IN (" , id, ")")
                    dbExecute(con,qdelete)
               }
               
               #insertar candidato y crear proceso inicial con fecha de hoy
               qinsert <- paste0("INSERT INTO gastos
                                 (id_cliente, id_concepto, id_medio, id_asesor, id_comun, fecha, monto) ",
                                 "VALUES (",
                                 datos,
                                 ")")
               dbExecute(con,qinsert)
               dbDisconnect(con) 
               
               #actualiza tabla                   
               output$tabla.gastos <- renderDataTable({
                    gastos <<- cargar.gastos()
                    if(is.null(gastos)) return(NULL)
                    
                    db.tabla.gastos <<- gastos%>%
                         mutate("prorrateo" = ifelse(id_comun ==0,"","SI"))%>%
                         select(id,concepto, "medio.asignado" = medio_asignado, fecha, monto, prorrateo)%>%
                         mutate_if(is.character, as.factor)%>%
                         tibble::column_to_rownames('id')
                    
                    datatable(data = db.tabla.gastos,
                              rownames = T,
                              selection = list(mode='single',
                                               target = 'row'),
                              filter = "top",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 't',
                                             scrollX = TRUE,
                                             scrollY = 400,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
               
          }
          
          #termina gastos--------------------------------------------------------------------
          
          #catalogos ---------------------------------------------------------------------
          cargar.catalogos <- function(){
               cat(paste("Seleccionado", input$cb.catalogo, "\n"))
               nombre.tabla <- c_catalogos[c_catalogos$descripcion==input$cb.catalogo,2]
               
               con <- conectar()
               db.tabla.catalogo <<- dbGetQuery(con, paste0("SELECT * FROM ", nombre.tabla, " WHERE BAJA = 0"))%>%
                    arrange(nombre)%>%
                    tibble::column_to_rownames('id')%>%
                    select("Descripcion" = nombre)
               dbDisconnect(con)
               
               return(db.tabla.catalogo)
               
          }
          output$tabla.catalogos <- renderDataTable({
               db.tabla.catalogo <- cargar.catalogos()
               
               datatable(data = db.tabla.catalogo,
                         rownames = T,
                         selection = list(mode='single',
                                          target = 'row'),
                         filter = "none",
                         autoHideNavigation = T,
                         extensions = 'Scroller',
                         options = list(dom = 't',
                                        scrollX = TRUE,
                                        scrollY = 400,
                                        scroller = TRUE,
                                        fixedHeader = TRUE))
          })
          
          observeEvent(input$tabla.catalogos_rows_selected, {
               ren <- input$tabla.catalogos_rows_selected
               if(is.null(ren)) return(NULL)
               id_valor <- row.names(db.tabla.catalogo)[ren]
               cat(paste("Seleccionado catalogo id:", id_valor , "\n"))
               
               #llenar datos arriba               
               updateTextInput(session, "Cid", value = id_valor)
               updateTextInput(session, "valor", value = db.tabla.catalogo[ren,1])
               
          })
          
          observeEvent(input$cmd.nuevo.valor,{
               updateTextInput(session, "Cid", value = "")
               updateTextInput(session, "valor", value = "")
               
               #quita renglon seleccionado
               proxy = dataTableProxy('tabla.catalogos', session)
               selectRows(proxy, selected = NULL)
          })
          
          observeEvent(input$cmd.borrar.valor,{
               req(input$cmd.borrar.valor)
               id <- input$Cid
               nombre.tabla <- c_catalogos[c_catalogos$descripcion==input$cb.catalogo,2]
               
               cat(paste("Borrando valor:", id), "del catalogo de ", nombre.tabla, "\n")
               if(id !=""){
                    
                    #si prorrateado
                    texto <- paste("¿Estas seguro de borrar este valor del catalogo de", input$cb.catalogo, "?")
                    shinyalert(title = "Confirmar", 
                               text =  texto,
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               html = FALSE,
                               type = "warning",
                               timer = 0,
                               animation = TRUE,
                               showConfirmButton = TRUE,
                               showCancelButton = TRUE,
                               confirmButtonText = "Si, eliminar valor",
                               confirmButtonCol = "#AEDEF4",
                               cancelButtonText = "No, cancela eliminar",
                               callbackR = function(x) if(x==T) eliminar.valor())
                    }
          })
          
          eliminar.valor <- function(){
               ren <- input$tabla.catalogos_rows_selected
               if(is.null(ren)) return(NULL)
               id_valor <- input$Cid
               nombre.tabla <- c_catalogos[c_catalogos$descripcion==input$cb.catalogo,2]
               
               qupdate <- paste0("UPDATE ", nombre.tabla, " SET baja = 1 WHERE id IN (" , id_valor, ")")
               
               con <- conectar()
               dbExecute(con,qupdate)
               dbDisconnect(con)
               
               updateTextInput(session, "Cid", value = "")
               updateTextInput(session, "valor", value = "")
               
               #actualizar la tabla
               output$tabla.catalogos <- renderDataTable({
                    db.tabla.catalogo <- cargar.catalogos()
                    
                    datatable(data = db.tabla.catalogo,
                              rownames = T,
                              selection = list(mode='single',
                                               target = 'row'),
                              filter = "none",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 't',
                                             scrollX = TRUE,
                                             scrollY = 400,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
               
          }
          
          observeEvent(input$cmd.guardar.valor,{  #boton guardar
               req(input$cmd.guardar.valor)
               id_valor <- input$Cid
               nombre.tabla <- c_catalogos[c_catalogos$descripcion==input$cb.catalogo,2]
               
               cat(paste("Modificando o agregando valor al id", id_valor, "del catalogo de ", nombre.tabla, "\n"))
               
               if (input$valor == ""){
                    sendSweetAlert(session, "Error", "El valor está vacio. Escriba un valor para el catalogo" , type = "error")
                    return(NULL)
               } 
               
               if(id_valor == ""){ #si es nuevo - se confirma que se agregará el paso inicial como realizado
                    msg = paste("¿Estás seguro de querer guardar este valor en el catalogo", input$cb.catalogo ,"?")
                    
               } else {
                    msg = paste("¿Estas seguro de guardar los cambios realizados al catalogo", input$cb.catalogo ,"?")
               }
               
               shinyalert(title = "Confirmar", 
                          text =  msg,
                          closeOnEsc = TRUE,
                          closeOnClickOutside = FALSE,
                          html = FALSE,
                          type = "warning",
                          timer = 0,
                          animation = TRUE,
                          showConfirmButton = TRUE,
                          showCancelButton = TRUE,
                          confirmButtonText = "Si, guardar",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela",
                          callbackR = function(x) if(x==T) guardar.catalogo())
               
          })
          
          guardar.catalogo <- function(){ #confirmar guardar
               id_valor <- input$Cid
               nombre.tabla <- c_catalogos[c_catalogos$descripcion==input$cb.catalogo,2]
               valor = toupper(input$valor)
               
               con <- conectar()
               if(id_valor == "") #nuevo registro
               {
                    #insertar candidato y crear proceso inicial con fecha de hoy
                    qinsert <- paste0("INSERT INTO ", nombre.tabla, "
                                      (nombre, baja) ",
                                      "VALUES ('", valor, "',0)")
                    dbExecute(con,qinsert)
               } else {
                    qupdate <- paste0("UPDATE ", nombre.tabla ,
                         " SET nombre = '", valor, "'", 
                         " WHERE id = " , id_valor)
                    
                    dbExecute(con,qupdate)  
               }

               dbDisconnect(con) 
               
               #actualizar la tabla
               output$tabla.catalogos <- renderDataTable({
                    db.tabla.catalogo <- cargar.catalogos()
                    
                    datatable(data = db.tabla.catalogo,
                              rownames = T,
                              selection = list(mode='single',
                                               target = 'row'),
                              filter = "none",
                              autoHideNavigation = T,
                              extensions = 'Scroller',
                              options = list(dom = 't',
                                             scrollX = TRUE,
                                             scrollY = 400,
                                             scroller = TRUE,
                                             fixedHeader = TRUE))
               })
          }
          
          #termina catalogos ------------------------------------------------------------------
          
          #GENERALES - LOGIN Y MENUS ----------------------------------------------------------
          #usuarios
          output$tabla.usuarios <- DT::renderDataTable({
               con <- conectar()
               logins <- dbGetQuery(con, 'SELECT * FROM users')
               dbDisconnect(con)
               
               datos <- logins%>%select("NOMBRE" = nombre,
                                        "USUARIO" = user,
                                        "CONTRASEÑA" = pass,
                                        "NIVEL" = level)
               datatable(data = datos,
                         rownames = F,
                         selection ='none',
                         autoHideNavigation = T,
                         options = list(dom = 't'))
          })
          
          output$tabla.vacantes <- DT::renderDataTable({
               if (level == "super") {
                    vacantes <- q.vacantes(all = T)%>%
                         mutate_if(is.character, as.factor)
               } else {
                    vacantes <- q.vacantes(id_status = 1,id_usuario = id_user, all = F)%>%
                         mutate_if(is.character, as.factor)
               }
               
               row.names(vacantes)<- vacantes$id
               datos <- vacantes%>%
                    select(cliente, vacante, fecha, codigo_postal)
               
               datatable(data = datos,
                         rownames = T,
                         selection ='single', 
                         filter = "top",
                         autoHideNavigation = T,
                         extensions = 'Scroller',
                         options = list(dom = 'ft',
                                        scrollX = TRUE,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        fixedHeader = TRUE))

          })
          
          output$tabla.clientes <- renderDataTable({
               clientes <- q.clientes()
               
               if (input$clientes.activos){
                    clientes <- clientes%>%filter(con_vacantes == 1)
               }
               
               #formatear
               clientes <- clientes%>%
                    mutate("direccion" = paste(ifelse(is.na(calle),"",calle),
                                               ifelse(is.na(numero),"", numero),
                                               ifelse(is.na(colonia),"", paste("COL.", colonia))))%>%
                    select(nombre,direccion,ciudad,estado,pais,"CP" = codigo_postal)
               
               DT::datatable(data = clientes,
                             rownames = T,
                             selection ='none',
                             autoHideNavigation = T,
                             options = list(pageLength=10))
          })
          

     
     #exit login
     observeEvent(input$b.salir,{

          output$menu.login <- renderMenu({
               sidebarMenu(
                    menuItem("Ingresar al sistema", tabName = "portada", icon = icon('sign-in'),selected = T),
                    textInput("s.usuario","Usuario:",placeholder = "Usuario: demo"),
                    passwordInput("s.contra", "Contraseña:",placeholder = "Contraseña: a"),
                    actionBttn(inputId = "b.login", label = "Entrar", 
                               style = "jelly", color = "primary")
               )
          })
       
          output$menu.logged <- renderMenu({
               sidebarMenu()
          })
     })
     
     #combos generales
     observeEvent(input$frecuencia,{
          fechas.selected <<- input$frecuencia
     })
     
     #combo filtrar fechas de gastos, para no cargar todos
     output$ui.fechas.filtros.gastos <- renderUI({
          con <- conectar()
          query <- paste("SELECT * FROM frecuencias WHERE baja = 0")
          consulta <- dbGetQuery(con, query)
          fechas.selected <<- consulta[consulta$default == 1,]$nombre
          dbDisconnect(con)
          
          pickerInput("fechas.filtros.gastos",label = "Filtrar ", 
                      choices = consulta$nombre,
                      multiple = F,
                      options = list(style = "btn-primary"),
                      inline = T)
     })
     
     #fechas y frecuencias condicionales a menu de scorboards
     output$ui.fechas <- renderUI({
          if (is.null(input$Menu.reclut)) return(NULL)
          
          con <- conectar()
          query <- paste("SELECT * FROM frecuencias WHERE baja = 0")
          consulta <- dbGetQuery(con, query)
          fechas.selected <<- consulta[consulta$default == 1,]$nombre
          dbDisconnect(con)
          

          if (input$Menu.reclut %in% c('score',"kpis")){
               pickerInput("frecuencia",label = "Periodo", 
                           choices = consulta$nombre,
                           multiple = F,
                           options = list(style = "btn-primary"))
          } else { return(NULL) }
          
     })
     
     output$ui.ver.rechazados <- renderUI({
          if(cp$allow_rechazados==1){  #switch para ver rechazados (cp define si se ve)
               materialSwitch(inputId = "ver.rechazados", 
                              label = "Ver rechazados", value = F,
                              status = "primary")
          }
     })
     
     #si es rechazo
     output$razon.rechazo <- renderUI({
          if(input$Cproceso!="Rechazo") return(NULL)
          
          con <- conectar()
          c_rechazo <<- dbGetQuery(con, "SELECT * FROM razones_rechazo WHERE baja = 0")
          dbDisconnect(con)
          
          pickerInput("rechazo","Razon de rechazo",
                      choices = c_rechazo$nombre,multiple = F)
     })
     
     #caja con las fechas
     output$ui.fechas.filtros <- renderUI({
          if (is.null(input$Menu.reclut)) return(NULL)
          
          if (input$Menu.reclut %in% c('score',"kpis")){
               htmlOutput("fechas.filtros")
          }
     })
     
     #carga combo de fechas en menu
     output$ui.rango <- renderUI({
          if (is.null(input$Menu.reclut)) return(NULL)
          
          if (input$Menu.reclut == "score"){
               pickerInput("rango.score",label = "Agrupamiento", 
                           choices = c("Semanal","Mensual"),
                           multiple = F,
                           options = list(style = "btn-primary"))
          } else { return(NULL) }
          
     })
     
     output$ui.filtro.procesos <- renderUI({      
          selectInput('filtro.proceso',"Filtrar procesos",
                      multiple = T, choices = c_procesos$nombre,                                
                      selected = c("Solicitud","Ingreso","Rechazo"))
     })
     
     output$imagen.inicio <- renderImage({
          return(list(
               src = "www/logo.png",
               contentType = "image/png",
               heigth = '700px',
               width = '1000px',
               alt = "Bienvenido"
          ))
     }, deleteFile = FALSE)
     #Termina generales logins y menus ----------------------------------------------------  
     
     #cargar los menus --------------------------------------------------------------------
     cargar_menus <- function(){ #si login correcto
          
          #cargar catalogos
          con <- conectar()
          c_sexo <<- dbGetQuery(con, paste("SELECT * FROM c_sexo"))
          c_escolaridad <<- dbGetQuery(con, paste("SELECT * FROM c_escolaridad WHERE baja = 0"))
          c_medio <<- dbGetQuery(con, paste("SELECT * FROM medios WHERE baja = 0"))
          c_procesos <<- dbGetQuery(con, "SELECT * FROM vacantes_procesos WHERE baja = 0 ORDER BY orden")
          c_concepto.gastos <<- dbGetQuery(con, "SELECT *  FROM gastos_conceptos WHERE baja = 0")
          c_catalogos <<- dbGetQuery(con, "SELECT * FROM catalogos")
          c_reclutadores <<- dbGetQuery(con, "SELECT * FROM users WHERE level = 'reclutador' AND baja = 0")
          dbDisconnect(con) 
          
          updatePickerInput(session, "cb.catalogo", choices = c_catalogos$descripcion)
          
          output$menu.login <- renderMenu({
               sidebarMenu(
                    p(paste("Bienvenid@", nombre)),
                    actionBttn(inputId = "b.salir", label = "Salir", 
                               style = "bordered", color = "danger",size = "xs", icon = icon("sign-out"))
               )
          })

          if(level == "super"){
               output$menu.logged <- renderMenu({
                    sidebarMenu(
                         menuItem("SCOREBOARD", tabName = "score", icon = icon("tachometer")),
                         menuItem("KPIs", tabName = "kpis", icon = icon("line-chart")),
                         menuItem("USUARIOS", tabName = "abc-users", icon = icon("users"),selected = F),
                         menuItem("CLIENTES", tabName = "abc-clientes", icon = icon("industry")),
                         menuItem("VACANTES", tabName = "abc-vacantes", icon = icon("list")),
                         menuItem("METAS", tabName = "abc-metas", icon = icon("trophy")),
                         id = "Menu.super"
                    )
               })
               
               #combos de elegir reclutadores
               output$ui.score.reclut <- renderUI({
                    pickerInput("score.reclut", label = "", 
                                inline = T, multiple = F, choices = c_reclutadores$nombre, 
                                options = list('dropupAuto' = T, 'mobile'=T,
                                               container=  'body'))
               })
          }
          
          if(level == "reclutador"){
               output$menu.logged <- renderMenu({
                    sidebarMenu(
                         menuItem("SEGUIMIENTO VACANTES", tabName = "abc-registro", icon = icon("bullseye"), selected = T),
                         menuItem("CANDIDATOS", icon = icon("users"),
                              menuSubItem("MIS CANDIDATOS", "abc-candidatos", icon = icon("archive")),
                              menuSubItem("BOLSA DE CANDIDATOS", "abc-bolsa", icon = icon("archive"))),
                         #menuItem("CALENDARIO", tabName = "abc-calendario", icon = icon("calendar")),
                         menuItem("VACANTES", tabName = "abc-vacantes", icon = icon("list")),
                         menuItem("GASTOS", tabName = "abc-gastos", icon = icon("usd")),
                         menuItem("SCOREBOARD", tabName = "score", icon = icon("tachometer")),
                         menuItem("KPIs", tabName = "kpis", icon = icon("line-chart")),
                         uiOutput("ui.fechas"),
                         # uiOutput("ui.rango"),
                         uiOutput("ui.fechas.filtros"),
                         menuItem("CATALOGOS", tabName = "catalogos", icon = icon("table")),
                         id = "Menu.reclut"
                    )
               })
               
               output$ui.score.reclut <- renderUI({
                    #combos de elegir reclutadores
                    output$ui.score.reclut <- renderUI({
                         pickerInput("score.reclut", label = "", 
                                     inline = T, multiple = F, choices = nombre, 
                                     options = list('dropupAuto' = T, 'mobile'=T,
                                                    container=  'body'))
                    })
               })
          }
     }
     
     # #actualizar tabla de edicion
     # observeEvent(input$tabla.vacantes_cell_edit,{
     #      info = input$tabla.vacantes_cell_edit
     #      proxy = dataTableProxy('tabla.vacantes')
     #      cat(paste(info))
     #      r = info$row
     #      c = info$col
     #      v = info$value
     #      vacantes[r,c] <- coerceValue(v,vacantes[r,c])
     #      replaceData(proxy, vacantes, resetPaging = F)
     #      
     #      #actualizar renglones en el server
     #      
     # })
     
})
