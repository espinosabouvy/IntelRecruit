

shinyServer(function(input, output, session) {
     options(shiny.reactlog=TRUE)      
     options(shiny.sanitize.errors = FALSE)
     
     #VARIABLES PRINCIPALES -----------------------------------------------------------
     #suponer fecha, para que sys.date() con haga ridiculas las graficas
     #cambiar a sys.date() para el programa, fuera del demo
     demo = T
     if(demo){
          fecha.hoy <- ymd(20180222)  #registro follow 20180311
     } else {
          fecha.hoy = as_date(Sys.Date())
     }
     
     # values <- reactiveValues(sessionId = NULL)
     # values$sessionId <- as.integer(runif(1, 1, 100000))
     # #cat(paste0("Session id: ", values$sessionId))
     # session$onSessionEnded(function() {
     #      observe(cat(paste0("Ended: ", values$sessionId)))
     #      # #desconectar cuando se cierra la sesion
     #      # dbDisconnect(con)
     # })
     
     #cargar cp
     cp <- read.csv("www/cp.csv")
     
     #main variables
     b.logged <- FALSE   #global status login
     id_user <- reactiveVal(character(0))  #id_user para filtros por usuario logeado
     level <- character(0)   #nivel del usuario logeado
     nombre <- character(0)  #nombre del usuario

     fechas.selected <- character(0)  #una frecuencia por default al cargar kpis
     
     #tablas guardadas de consultas
     kpi.tiempo <- NULL
     seguimiento <-  NULL
     embudo <-  NULL
     tabla.de.seguimiento <- NULL
          new.seguimiento <- reactiveVal(0)
     vacantes.abiertas <- NULL
     vacantes.disponibles <- NULL
     db.tabla.candidatos <- NULL
          new.candidatos <- reactiveVal(0)
     db.tabla.gastos <- NULL
          new.gastos <- reactiveVal(0)
     db.bolsa.candidatos <- NULL
     db.bolsa.vacantes <- NULL
          new.vacantes <- reactiveVal(0)
     db.tabla.catalogo <- NULL
     db.tabla.clientes <- NULL
          new.clientes <- reactiveVal(0)
     db.tabla.metas <- NULL
          new.metas <- reactiveVal(0)
     db.tabla.users <- NULL
          new.users <- reactiveVal(0)
     db.vacantes <- NULL
     datos.asignacion <- NULL
     c_sexo <- NULL
     c_escolaridad <- NULL
     c_medio <- NULL
     c_procesos <- NULL
     c_rechazo <- NULL
     c_concepto.gastos <- NULL
     c_catalogos <- NULL
     c_reclutadores <- NULL
     c_clientes <- NULL
     c_vacantes <- NULL
     c_concepto.metas <- NULL

     #CONSULTAS -----------------------------
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
          query <- paste0("SELECT candidatos.id, candidatos.nombre, vacantes.vacante_original, vacantes.cliente_original, va.id_proceso, va.ultimo_proceso, va.razon_rechazo, cp, c_sexo.`nombre` AS 'sexo', 
                              c_escolaridad.`nombre`AS 'escolaridad', fecha_nacimiento, medios.`nombre` AS 'medio', direccion 
                          FROM candidatos
                          RIGHT JOIN (
                              SELECT vf.id_candidato, vf.`id_vacante`, vf.id_proceso, vp.`nombre` ultimo_proceso,
                                   vf.id_razon_rechazo, rr.`nombre` razon_rechazo, vac.id_status
                              FROM vacantes_following vf
                              LEFT JOIN vacantes_procesos vp ON vp.id = vf.id_proceso
                              LEFT JOIN razones_rechazo rr ON rr.`id` = vf.`id_razon_rechazo`
                              RIGHT JOIN (
                                   SELECT vf.id_candidato, MAX(vp.orden) orden /*orden max de vacante actual*/
                                   FROM vacantes_following vf
                                   LEFT JOIN vacantes_procesos vp ON vp.id = vf.id_proceso
                                   WHERE vf.cambio_vacante = 0
                                   GROUP BY id_candidato
                              ) ultimo ON ultimo.id_candidato = vf.id_candidato 
                              AND ultimo.orden = vp.orden
                              LEFT JOIN vacantes vac ON vac.id = vf.id_vacante
                              WHERE (vf.id_proceso = 5 AND vf.cambio_vacante = 0) 
                                   OR (vac.id_status = 2 AND (vf.id_proceso <> 4 AND vf.cambio_vacante = 0))
                              ORDER BY vf.id_candidato
                              )  
                          AS va ON va.id_candidato = candidatos.`id`
                          LEFT JOIN c_sexo ON candidatos.`id_sexo` = c_sexo.`id`
                          LEFT JOIN c_escolaridad ON candidatos.`id_escolaridad` = c_escolaridad.`id`
                          LEFT JOIN medios ON medios.`id` = candidatos.`id_medio`
                          LEFT JOIN (
                              SELECT vac.`id`, vn.`nombre` AS 'vacante_original', 
                                   clientes.`nombre` AS 'cliente_original', vac.`id_status`
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
  
     q.gastos <- function(id_usuario=id_user(), date.inicio = 20000101){
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
          
     #incluye en los indicadores, procesos que cambiaron de vacante, pueden estar dobles,
     #aparece que se hizo solicitud, aunque se reutilizo del proceso anterior y en 
     #realidad no se volvio a hacer (se puede corregir, agregando columna - se reutilizo, pero
     #no se realizara por el momento)
     q.kpi.tiempo.proceso <- function(id_status = c(1,2), id_usuario, date.inicio = 20000101, all=F){
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
     
     q.score.vacantes <- function(date.inicio = 20000101){
          con <- conectar()
          query <- paste0("SELECT vacantes.id_cliente, clientes.nombre, users.nombre 'reclutador',
	                     COUNT(vacantes.fecha) 'vacantes',
                          IF(vacantes.id_status = 1, 'A','C') 'status', vacantes.id_status
                          FROM vacantes
                          LEFT JOIN clientes on clientes.id = vacantes.id_cliente
                          LEFT JOIN users on users.id = vacantes.id_usuario
                          WHERE vacantes.baja = 0
               AND vacantes.fecha >= ", date.inicio, "
               GROUP BY vacantes.id_cliente, vacantes.id_status, vacantes.id_usuario")
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          consulta$vacantes <- as.integer(consulta$vacantes)
          return(consulta)
     }
     
     q.vacantes <- function(id_status=c(1,2), id_usuario= id_user(), all=F, date.inicio= 20000101){
          con <- conectar()
          query <- paste0("SELECT vacantes.id, clientes.`nombre` AS 'cliente', vacantes_nombre.`nombre` AS 'vacante',
	                         IFNULL(vf.candidatos,0) candidatos, vacantes.fecha, vacantes_status.`nombre` AS 'status', 
                              users.`nombre` AS 'asesor', clientes.`codigo_postal`
                          FROM vacantes
                          LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
                          LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
                          LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
                          LEFT JOIN users ON users.id = vacantes.`id_usuario` 
                          LEFT JOIN (SELECT vf.id_vacante, COUNT(DISTINCT(vf.id_candidato)) 'candidatos'
                              FROM vacantes_following vf 
                              LEFT JOIN vacantes_procesos vp ON vp.id = vf.id_proceso
                              LEFT JOIN candidatos can ON can.id = vf.id_candidato
                              WHERE vf.cambio_vacante = 0
                              AND vp.baja = 0 
                              AND can.baja = 0  
                              AND vf.id_candidato NOT IN ( 
                                   SELECT DISTINCT(id_candidato) id_candidato
                                   FROM vacantes_following vf
                                   WHERE id_proceso IN (SELECT id 
                                        FROM vacantes_procesos 
                                        WHERE cierra_proceso=1)
                                   AND cambio_vacante = 0)
                              GROUP BY vf.id_vacante) 
                          AS vf ON vf.id_vacante = vacantes.`id`
                          WHERE vacantes.`baja`= 0  
                          AND vacantes.id_status IN (", paste(id_status,collapse = ","),")
                          AND vacantes.fecha >= ", gsub("-","",ymd(date.inicio)))
          
          if (all == F){ #agrega filtros a la consulta
               query <- paste(query, " AND  vacantes.id_usuario= ", id_usuario)
          }
          
          query <- paste0(query, " GROUP BY id
                          ORDER BY cliente")
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          consulta$fecha <- ymd(consulta$fecha)
          return(consulta)
     }
     
     
     #usuarios y sus vacantes abiertas
     q.usuarios <- function(id_status = c(1,2)){
          con <- conectar()
          query <- paste0("SELECT users.*, IFNULL(abiertas,0) 'vacantes_abiertas'
                          FROM users
                          LEFT JOIN (
                              SELECT id_usuario, COUNT(id) 'abiertas'
                              FROM vacantes 
                              WHERE baja = 0 
                              AND id_status IN (",paste(id_status,collapse = ",") ,") 
                              GROUP BY id_usuario) 
                          va on va.id_usuario = users.id
                          WHERE users.baja = 0")
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          return(consulta)
     }
     
     #seguimiento a candidatos en proceso, vacantes abiertas y/o cerradas
     q.seguimiento <- function(id_status=1, id_usuario= id_user(), all=F){
          con <- conectar()
          query <- paste0("SELECT cand.id AS 'id_candidato', cand.nombre  AS 'candidato', cand.escolaridad, cand.sexo, cand.medio, vacantes_detalle.*, vp.orden AS 'orden_proceso',
                              vp.id AS 'id_proceso', vp.nombre AS 'proceso', 
                              vf.fecha, vf.comentarios 
                          FROM vacantes_following AS vf
                          LEFT JOIN 
                              (SELECT candidatos.id, candidatos.nombre, candidatos.baja, c_sexo.`nombre` AS 'sexo', esc.`nombre` AS 'escolaridad', medios.`nombre` AS 'medio' 
                              FROM candidatos
                              LEFT JOIN c_sexo ON c_sexo.`id` = candidatos.`id_sexo`
                              LEFT JOIN c_escolaridad AS esc ON esc.id = candidatos.`id_escolaridad`
                              LEFT JOIN medios ON medios.`id` = candidatos.`id_medio`) 
                          AS cand ON cand.`id` = vf.`id_candidato`
                          LEFT JOIN vacantes_procesos AS vp ON vp.id = vf.id_proceso
                          RIGHT JOIN(
                              SELECT vacantes.`id` AS 'id_vacante',clientes.`nombre` AS 'cliente', vacantes_nombre.`nombre` AS 'vacante',
                                   vacantes_status.`nombre` AS 'status', users.id AS 'id_user', users.`nombre` AS 'asesor' 
                              FROM vacantes
                              LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
                              LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
                              LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
                              LEFT JOIN users ON users.id = vacantes.`id_usuario`
                              WHERE vacantes.baja = 0 AND vacantes.id_status IN (", paste0(id_status, collapse = ","),")
                                   AND vacantes.id_usuario IN (", paste0(id_usuario, collapse = ",") , "))
                         AS vacantes_detalle ON vacantes_detalle.id_vacante = vf.`id_vacante`
                         WHERE vf.id_candidato NOT IN 
                              (SELECT DISTINCT(vacantes_following.id_candidato) 
                              FROM vacantes_following
                              LEFT JOIN vacantes_procesos ON vacantes_procesos.id = vacantes_following.id_proceso
                              WHERE (vacantes_procesos.cierra_proceso = 1 AND vacantes_following.cambio_vacante = 0)
                              ORDER BY id_candidato) 
                          AND cand.baja=0
                          AND vf.cambio_vacante=0
                          ORDER BY cliente, candidato")
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          consulta$fecha <- ymd(consulta$fecha)
          return(consulta)
     
     }
     
     q.candidatos <- function(id_candidatos = NULL, mis.candidatos=F){
          con <- conectar()
          query <- paste("SELECT candidatos.id, candidatos.nombre, telefono, celular, correo, cp, c_sexo.`nombre` AS 'sexo', 
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
                                        SELECT id id_vacante FROM vacantes WHERE id_usuario = ", id_user(), 
                                        " AND id_status= 1)
                    )")     
          }
          
          query <- paste0(query, " ORDER BY candidatos.id")
          
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          return(consulta)
     }
     
     q.costo.por.medio <- function(id_usuario= id_user(), date.inicio = 20000101){
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
     
     
     q.clientes <- function(solo.activos = T){
          con <- conectar()
          query <- paste0("SELECT id, nombre, vacantes_abiertas, codigo_postal, telefono, direccion FROM (
                               SELECT clientes.*, COUNT(vacantes_abiertas.id_cliente) AS 'vacantes_abiertas' 
                               FROM clientes 
                               LEFT JOIN 
                                   (SELECT id_cliente FROM vacantes WHERE id_status = 1) 
                               AS vacantes_abiertas ON vacantes_abiertas.id_cliente = clientes.`id` 
                               WHERE clientes.`baja`= 0 
                               GROUP BY clientes.`id`
                               ORDER BY nombre) as vac_mod")
          if(solo.activos){
               query <- paste0(query, " WHERE vacantes_abiertas > 0")
          }
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          return(consulta)
     }
     
     q.embudo <- function(id.status.vacantes = c(1,2), id_usuario= id_user(), all=F, date.inicio=20000101){
          con <- conectar()
          query <- paste0("
               SELECT vacantes.id, clientes.`nombre`, vacantes_nombre.`nombre` AS 'vacante', 
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
               			) 
                              AS cand ON cand.id= vacantes_following.`id_candidato`
               	          LEFT JOIN vacantes_procesos AS proc ON proc.id = vacantes_following.`id_proceso`
                WHERE vacantes_following.cambio_vacante=0)
                AS vf ON vf.id_vacante = vacantes.`id` 
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
     
     q.vac.iguales <- function(id_vacante = 0){  #vacantes iguales a otra
          con <- conectar()
          
          query <- paste0("SELECT vac.id id_vacante
                         FROM vacantes vac
                    RIGHT JOIN(SELECT id_cliente, id_nombre_vacante
                    FROM vacantes vac
                    WHERE vac.id = ", id_vacante, ") 
                    AS dvac ON dvac.id_cliente = vac.id_cliente AND dvac.id_nombre_vacante = vac.id_nombre_vacante
                    WHERE vac.id_status = 1 AND vac.id <> ", id_vacante , 
                    " ORDER BY fecha
                    LIMIT 1")
          
          consulta <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          return(consulta)
     }
     
     q.following <- function(id_vacante = 0, id_candidato = 0, rechazos = F){
          con <- conectar()
          
          query <- paste0("SELECT *
                          FROM vacantes_following vf
                          WHERE vf.id_vacante =  ", id_vacante,  
                          " AND id_candidato <> ", id_candidato)
          if(rechazos){
               query <- paste0(query, " AND id_proceso <> 5")
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
     
     #termina funciones de consultas---
     
     #LOGIN ---------------------------------------------------
     #carga pantalla para login   
     output$menu.login <- renderMenu({
          sidebarMenu(
               menuItem("Ingresar al sistema", tabName = "portada", icon = icon('sign-in'),selected = T),
               textInput("s.usuario","Usuario:",placeholder = "User: reclut o super"),
               passwordInput("s.contra", "Contraseña:",placeholder = "Pass: 1234"),
               actionBttn(inputId = "b.login", label = "Entrar", 
                          style = "jelly", color = "primary")
          )
     })
     
     #observa cuando se hace click en login
     observeEvent(input$b.login,{
          
          con <- conectar()
          #verifica reset de la contraseña
          query <- paste0("SELECT id, reset FROM users WHERE
                              user = '", input$s.usuario ,
                          "' AND baja = 0 AND reset = 1")
          reset <- dbGetQuery(con, query)
          dbDisconnect(con)
          
          if(nrow(reset)>0){ #reset contrasena
               shinyalert("Informacion","Solicitaste un cambio de contraseña,
                          la contraseña que escribiste sera registrada con tu nueva contraseña",
                          closeOnEsc = TRUE,
                          closeOnClickOutside = FALSE,
                          html = FALSE,
                          type = "warning",
                          timer = 0,
                          animation = TRUE,
                          showConfirmButton = TRUE,
                          showCancelButton = TRUE,
                          confirmButtonText = "Si, guarda la contrasena",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela",
                          callbackR = function(x) if(x==T) guarda.contra(id = reset$id))
          } else {
               con <- conectar()
               query <- paste0("SELECT id, user,nombre,level FROM users WHERE baja = 0 
                               AND user = '", input$s.usuario ,
                               "' AND pass = '",sha1(input$s.contra, 10),"'")
               logins <- dbGetQuery(con, query)
               dbDisconnect(con)
               
               if (nrow(logins)>0){
                    level <<- logins$level
                    nombre <<- logins$nombre
                    id_user(logins$id)
                    showNotification(paste("Bienvenido", nombre), type = "message",duration = 5)
                    cargar_menus()
               } else {
                    # sendSweetAlert(session, "Incorrecto","Usuario o contraseña incorrectos",
                    #                "error")
                    output$menu.logged <- renderMenu({
                         sidebarMenu(h5("Usuario o contraseña incorrectos"))
                    })
               }
          }
     })
     
     guarda.contra <- function(id = NULL){
          #guarda la nueva contra
          qupdate <- paste0("UPDATE users
                                 SET pass = '", sha1(input$s.contra, 10), "',
                                  reset = 0
                                  WHERE id = " , id)
          con <- conectar()
          dbExecute(con, qupdate)
          dbDisconnect(con)
          shinyalert("Informacion","Ahora puedes entrar con tu nueva contraseña", 
                     closeOnEsc = TRUE,
                     closeOnClickOutside = FALSE,
                     type = "info")
     }
     
     #KPI'S ----------------------------------------------------------
     fun.kpi.tiempos <- reactive({
          if(is.null(input$frecuencia)) return(NULL)
          fechas <- tiempos(input$frecuencia)
          
          #fechas superior
          output$fechas.filtros <- renderText({
               paste("Del ", ymd(fechas), "al", fecha.hoy)
          }) 
          
          kpi.tiempo <<- q.kpi.tiempo.proceso(id_usuario = id_user(), 
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
                              WHERE baja=0 AND id_meta = 1")%>%
               group_by(fecha_inicio)%>%
               summarise(valor = max(valor))%>%
               arrange(fecha_inicio)
          
          dbDisconnect(con)
          meta$fecha_inicio <- ymd(meta$fecha_inicio)
          #meta$semana <- paste(year(meta$fecha_inicio),"-",week(meta$fecha_inicio))
          
          #crear valores
          fechas.meta <- data.frame("FECHA" =seq(min(kpi.tiempo$fecha_vacante), 
                                                 max(fecha.hoy + 7), by = "week"))
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
               filter(id_proceso == 1)%>%
               mutate("SEMANA" = paste(year(fecha),"-",week(fecha)))%>%
               group_by(asesor, id_vacante, SEMANA)%>%
               summarise("CANDIDATOS"= n())%>%
               group_by(asesor, SEMANA)%>%
               summarise("POR.VACANTE" = mean(CANDIDATOS))%>%
               merge(meta.solicitudes, by = "SEMANA", all.x = T)%>%
               mutate("MAX" = max(c(POR.VACANTE, META),na.rm = T)+1)
          
          if(nrow(datos)==0)return(NULL)
          
          ggplot(datos, aes(SEMANA, POR.VACANTE, group = 1)) + 
               geom_point() +
               geom_smooth(stat = "smooth", method = "lm", se = F, linetype= "dashed",col = "purple") + 
               geom_area(aes(SEMANA, MAX), fill = "green4", alpha = 0.3) +
               geom_area(aes(SEMANA, META), fill = "red", alpha = 0.4) +
               geom_line(aes(SEMANA, META), group =2, col = "green4") +
               geom_line(col = "darkmagenta", lwd=1) + 
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
                             WHERE baja=0 AND id_meta = 2")%>%
               group_by(fecha_inicio)%>%
               summarise(valor = max(valor))%>%
               arrange(fecha_inicio)
          
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
               filter(id_status ==2 & id_proceso==4)%>%  #cerradas y proceso de ingreso
               mutate("SEMANA" = paste(year(fecha_vacante),"-",week(fecha_vacante)),
                      "INGRESO" = as_date(ifelse(id_proceso == 4, fecha, fecha.hoy)),
                      "DIAS.ABIERTA" = INGRESO-fecha_vacante)%>%
               group_by(asesor, SEMANA, id_vacante)%>%
               summarise("DIAS.VACANTE" = min(DIAS.ABIERTA))%>%
               group_by(asesor, SEMANA)%>%
               summarise("X.DIAS.VACANTE" = as.numeric(round(mean(DIAS.VACANTE),1)))%>%
               merge(meta.tiempo, by = "SEMANA", all.x = T)%>%
               mutate("MAX" = max(c(X.DIAS.VACANTE, META))+1)
          
          if(nrow(dias.prom.vacante)==0) return(NULL)
          
          ggplot(dias.prom.vacante, aes(SEMANA, X.DIAS.VACANTE, group = 1)) + 
               geom_point() +
               geom_smooth(stat = "smooth", method = "lm", se = F, linetype= "dashed", col = "purple") + 
               geom_area(aes(SEMANA, META), fill = "green3", alpha = 0.6) +
               geom_area(aes(SEMANA, MAX), fill = "red", alpha = 0.3) +
               geom_line(aes(SEMANA, META), group = 2, col = "green4") +
               geom_line(lwd=1, col = "darkmagenta") + 
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
                             WHERE id_meta = 3 AND BAJA = 0")%>%
               group_by(fecha_inicio)%>%
               summarise(valor = max(valor))%>%
               arrange(fecha_inicio)
          
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
               filter(id_proceso==4)%>%
               mutate("SEMANA" = paste(year(fecha_vacante),"-",week(fecha_vacante)))%>%
               group_by(asesor, SEMANA)%>%
               summarise("CERRADAS" = n_distinct(id_vacante))
          
          pct.cerradas <- merge(vacantes, cerradas, all.x = T)%>%
               replace(., is.na(.), 0)%>%
               mutate("SALDO" = VACANTES- CERRADAS)%>%
               mutate("ACUM.VACANTES" = cumsum(VACANTES),
                      "ACUM.CERRADAS" = cumsum(CERRADAS),
                      "PCT.CERRADO" = round(ACUM.CERRADAS/ACUM.VACANTES*100,0))%>%
               merge(meta.cerradas, by = "SEMANA", all.x=T)%>%
               mutate("MAX" = 100)
          
          
          if(nrow(pct.cerradas)==0) return(NULL)
          
          ggplot(pct.cerradas, aes(SEMANA, PCT.CERRADO, group = 1)) + 
               geom_point()+
               geom_smooth(stat = "smooth", method = "lm", se = F, linetype= "dashed", col = "purple") + 
               geom_area(aes(SEMANA, MAX), fill = "green4", alpha = 0.3) +
               geom_area(aes(SEMANA, META), fill = "red", alpha = 0.4) +
               geom_line(aes(SEMANA, META), col = 'green4') +
               geom_line(lwd=1, col = "darkmagenta") + 
               theme(legend.position = 'top') + 
               ylab("Porcentaje") + 
               xlab("Año-Semana") + 
               coord_cartesian(ylim = c(0, 100))
          
     })
     #termina graficos de kpis--
          
     #SCOREBOARD SUPERVISOR -------------------------------------------
          output$ss.vacantes.cliente <- renderPlot({
               new.vacantes()
               
               if(is.null(input$frecuencia)) return(NULL)
               fechas <- tiempos(input$frecuencia)
                    
               vacantes <<- q.score.vacantes(date.inicio = fechas)
               
               #total vacantes
               output$uis.total.vacantes <- renderValueBox({
                    valueBox(value = sum(vacantes$vacantes),
                             "Total",
                             icon = icon('tasks'),
                             color = "light-blue")
               })
               
               #total abiertas
               output$uis.vacantes.abiertas <- renderValueBox({
                    valueBox(value = sum(vacantes[vacantes$id_status==1,]$vacantes),
                             "Abiertas",
                             icon = icon('thumbs-up'),
                             color = "orange")
               })
               
               #abiertas promedio
               output$uis.abiertas.promedio <- renderValueBox({
                    valor <- paste0(round(sum(vacantes[vacantes$id_status==1,]$vacantes)/nrow(unique(vacantes$reclutador))*100,0),"%")
                    valueBox(value = valor ,
                             "Abiertas",
                             icon = icon('thumbs-up'),
                             color = "orange")
               })
               
               #% cerradas
               output$uis.vacantes.cerradas <- renderValueBox({
                    valueBox(value = paste(round(sum(vacantes[vacantes$id_status==2,]$vacantes)/sum(vacantes$vacantes)*100,0),"%"),
                             "Cerradas",
                             icon = icon('thumbs-up'),
                             color = "light-blue")
               })
               
               #vacantes por reclutador
               output$ss.vacantes.reclut <- renderPlot({
                    for.plot <- vacantes%>%
                         group_by(reclutador, status)%>%
                         summarise("vacantes" = sum(vacantes))%>%
                         group_by(reclutador)%>%
                         mutate("total" = sum(vacantes),
                                "pct.cerradas" = as.integer(round(vacantes/total*100,0)))%>%
                         mutate("pct.abiertas" = as.integer(100-pct.cerradas))
                    
                    for.plot$reclutador.ok <- gsub(" ","\n", for.plot$reclutador)
                    
                    ggplot(for.plot, aes(reorder(reclutador.ok, c(total)), vacantes, group = status, 
                                         fill = status)) +  
                         geom_col(position = "stack") + 
                         geom_text(aes(label = paste0(status,": ",pct.cerradas,"%")), size = 3, hjust = 1, position = "stack")+
                         xlab("") + 
                         ylab("") + 
                         theme(legend.position = "none", 
                               legend.title = element_blank(),
                               legend.text = element_text(size = 7),
                               legend.key.size = unit(.3, 'cm'),
                               axis.text.y = element_text(size = 7),
                               axis.text.x = element_text(size = 7)) + 
                         coord_flip()
               })
               
               for.plot <- vacantes%>%
                    group_by(id_cliente)%>%
                    mutate("total" = sum(vacantes),
                           "pct.cerradas" = as.integer(round(vacantes/total*100,0)))%>%
                    mutate("pct.abiertas" = as.integer(100-pct.cerradas))
               
               ggplot(for.plot, aes(reorder(nombre, c(total)), vacantes, group = status, 
                                    fill =status)) +  
                    geom_col(position = "stack") + 
                    geom_text(aes(label = paste0(status,": ",pct.cerradas,"%")), size = 3, hjust = 1, position = "stack")+
                    xlab("") + 
                    ylab("") + 
                    theme(legend.position = "none", 
                          legend.title = element_blank(),
                          legend.text = element_text(size = 7),
                          legend.key.size = unit(.3, 'cm'),
                          axis.text.y = element_text(size = 7),
                          axis.text.x = element_text(size = 7)) + 
                    coord_flip()
          })
     output$uis.tiempo.promedio <- renderValueBox({
          if(is.null(input$frecuencia)) return(NULL)
          fechas <- tiempos(input$frecuencia)
          
          kpi.tiempo.score <- q.kpi.tiempo.proceso(date.inicio = fechas, all = T)  
          
          tiempo <- kpi.tiempo.score%>%
               group_by(proceso)%>%
               summarise(dias = round(mean(fecha-fecha_vacante),1))
          
          valor <- tiempo[tiempo$id_proceso==4,]$dias
          if(nrow(tiempo)==0) valor = 0
          valueBox(valor ,"Dias proceso", 
                   icon = icon("clock-o"),
                   color = "light-blue"
          )
     })
     
     #termina score supervisor --
     
     #SCOREBOARD ------------------------------------------------------------------------------
     
     #grafico tiempos de proceso
     observeEvent(input$reclut,{
          if(is.null(input$reclut)) return(NULL)
          id_user(c_reclutadores[c_reclutadores$nombre==input$reclut,]$id)
          cat(paste("Cambiando a reclutador id:", id_user(),"\n"))
     })
     
     output$p.tiempos.proceso <- renderPlot({
          if(is.null(input$frecuencia)) return(NULL)
          fechas <- tiempos(input$frecuencia)

          #fechas superior
          output$fechas.filtros <- renderText({
               paste("Del ", ymd(fechas), "al",fecha.hoy)
          }) 
          
          kpi.tiempo <<- q.kpi.tiempo.proceso(id_usuario = id_user(), 
                                              date.inicio = fechas)  
          
          tiempo <- kpi.tiempo%>%
               group_by(asesor, id_proceso, proceso)%>%
               summarise(dias = round(mean(fecha-fecha_vacante),1))
          
          
          output$ui.tiempo.promedio <- renderValueBox({
               valor <- tiempo[tiempo$id_proceso==4,]$dias
               if(nrow(tiempo)==0) valor = 0
               valueBox(valor ,"Dias proceso", 
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
               
               valor <- round(mean(dias.vacantes$dias.vacantes),1)
               valueBox(valor, "Dias abiertas",
                             icon= icon("calendar"),
                             color = "orange"
               )
          })
          
          output$p.razones.rechazo <- renderPlot({
               if(nrow(kpi.tiempo)==0)return(NULL)
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
                    geom_text(aes(label = paste(" ",razon_rechazo,"-",pct,"% ")), hjust = "inward", size = 3, color = "black") +
                    #geom_text(aes(label = paste(pct,"%")), hjust = -0.05, size = 3, color = "black") +
                    ylab("") +
                    xlab("") +
                    theme(legend.position = "none", axis.text.y = element_blank(),
                          axis.text.x = element_blank())
          })
          if(nrow(kpi.tiempo)==0)return(NULL)
          
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
          
          consulta <- q.vacantes(id_status=c(1), id_usuario = id_user(), all = F)
               
          valueBox(
               nrow(consulta) , "Abiertas",  icon = icon("list"),
               color = "orange"
          )
     })
     
     output$ui.vacantes.cumplidas <- renderValueBox({          
          if(is.null(input$frecuencia)) return(NULL)
          fechas <- tiempos(input$frecuencia)
          
          consulta <- q.vacantes(id_status=c(1,2), 
                                 id_usuario = id_user(), 
                                 all = F,
                                 date.inicio = fechas)
          
          output$ui.total.vacantes <- renderValueBox({
               valueBox(
                    nrow(consulta) , "Total",  icon = icon("list"),
                    width = 2, color = "light-blue"
               ) 
          })    
          
          valor <- paste0(round(nrow(consulta[consulta$status=="Cerrada",])/nrow(consulta)*100,0),"%")
          if(nrow(consulta)==0) valor = "0%"
          valueBox(value = valor, 
               "Cerradas",  icon = icon("thumbs-up"),
               width = 6, color = "light-blue"
          ) 
     }) 

     #costo por medio
     output$p.costo.por.medio <- renderPlot({
          if(is.null(input$frecuencia)) return(NULL)
          fechas <- tiempos(input$frecuencia)
          
          costo.x.medio <- q.costo.por.medio(id_usuario = id_user(), date.inicio = fechas) 
          
          if(nrow(costo.x.medio)==0)return(NULL)
          
          ggplot(costo.x.medio, aes(medio, costo,label = paste0(medio,"- $",costo)), group = 1) + 
               geom_bar(stat = 'identity',fill = "turquoise3") + 
               coord_flip() +
               geom_text(size = 3, hjust = 'inward') + 
               xlab("") + 
               ylab("") +
               theme(legend.position = "none", axis.text.y = element_blank(),
                     axis.text.x = element_blank())
          
     })
     
     #candidatos en proceso
     output$p.en.proceso <- renderPlot({
          seguimiento <- q.seguimiento(id_status = 1, id_usuario = id_user())
          
          p <- seguimiento%>%
               group_by(id_candidato)%>%
               mutate("ultimo_proceso" = max(orden_proceso))%>%
               filter(orden_proceso== ultimo_proceso)%>%
               group_by(orden_proceso, proceso)%>%
               summarise("freq" = n())
          p$proceso <- factor(p$proceso, unique((p%>%arrange((orden_proceso))%>%select(proceso))$proceso))
          
          if(nrow(p)==0)return(NULL)
          
          ggplot(p, aes(proceso, 1, label = paste(proceso, "\n", freq), fill = proceso)) + 
               geom_bar(stat = "identity") + 
               geom_text(size=5, vjust = 2) + 
               theme_void() +
               theme(legend.position = "none", axis.text.y = element_blank(),
                     axis.text.x = element_blank(), 
                     plot.background = element_blank())
     })
     
     #embudo
     output$p.embudo <- renderPlot({
          if(is.null(input$frecuencia)) return(NULL)
          fechas <- tiempos(input$frecuencia)
          embudo <<- q.embudo(id_usuario = id_user(), date.inicio = fechas)   
          
          #solicitudes por medios
          output$p.medios <- renderPlot({
               if(nrow(embudo)==0) return(NULL)
               if(is.null(input$frecuencia)) return(NULL)
               
               p <- embudo%>%
                    filter(as.integer(id_proceso) %in% c(1,4))%>%
                    group_by(id_proceso, proceso, medio)%>%
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
                    geom_text(size = 4, hjust = "inward")
               
          })
          
          #solicitudes por vacante
          output$ui.solicitudes.vacante <- renderValueBox({
               if(is.null(input$frecuencia)) return(NULL)
               
               valor <- ifelse(nrow(embudo)==0, 0,
               round(length(unique(embudo$candidato))/length(unique(embudo$id_vacante)),1))
               valueBox(valor, 
                    "Solicitudes",  icon = icon("bullseye"),
                    width = 2, color = "light-blue"
               )
               
          })
          
          output$ui.costo.vacante <- renderValueBox({
               if(is.null(input$frecuencia)) return(NULL)
               
               gastado <- q.total.gastado(id_usuario = id_user(), date.inicio = fechas) 
               cerradas <- nrow(embudo%>%
                    filter(id_proceso == 4))
               
               valueBox(round(gastado/cerradas,0), "Costo medio", icon = icon("usd"),
                        width = 6, color = "red"
               )
          })
          
          if(nrow(embudo)==0) return(NULL)
          if(is.null(input$frecuencia)) return(NULL)
          
          pp <- embudo%>%
               filter(id_proceso != 5)%>%
               group_by(orden, proceso,sexo)%>%
               summarise("total" = n())%>%
               ungroup()%>%
               group_by(sexo)%>%
               mutate("PCT" = total/max(total)*100)
          
          pp$proceso <- factor(pp$proceso, unique((pp%>%arrange(desc(orden))%>%select(proceso))$proceso))
          
          #embudo de reclutamiento
          ggplot(pp, aes(proceso, 
                         ifelse(sexo=="FEMENINO",-PCT,PCT), fill = sexo, group = sexo, 
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
     #termina kpi y scoreboard ---
          
          
     #AVANCE DEL PROCESO DE RECLUTAMIENTO--------------------------------------------------
     cargar.seguimiento <- function(){
          
          #las vacantes pueden estar abiertas o cerradas, pero no puede perderse
          #un candidato
          seguimiento <<- q.seguimiento(id_status = c(1), id_usuario = id_user(), all = F)%>%
               mutate_if(is.character, as.factor)
          
          #formatear
          datos <- seguimiento%>%
               mutate("proceso" = paste0(orden_proceso,".",proceso))%>%
               select(id_candidato, candidato, cliente, id_vacante, vacante, proceso, fecha)%>%
               spread(proceso, fecha)%>%
               arrange(cliente, vacante, candidato)%>%
               tibble::column_to_rownames("id_candidato")
          return(datos)
          
     }
     
     output$tabla.seguimiento <- DT::renderDataTable({
          new.seguimiento()
          tabla.de.seguimiento <<- cargar.seguimiento()
          
          if(nrow(tabla.de.seguimiento)==0)return(NULL)
          
          tabla.print <- tabla.de.seguimiento%>%
               select(everything(), -id_vacante)
          
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
          id_vacante <- tabla.de.seguimiento[ren,]$id_vacante

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
          
          id_cand <- row.names(tabla.de.seguimiento[ren,])
          con <- conectar()
          consulta <- q.candidatos(id_candidatos = id_cand, mis.candidatos = T)
          dbDisconnect(con)
          
          #cargar en controles para editar, borrar o nuevo
          updateTextInput(session, "Tid", value = id_cand)               
          updateTextInput(session, "Tnombre", value = consulta$nombre)
          updateTextInput(session, "Tdireccion", value = consulta$direccion)
          updateTextInput(session, "Ttelefono", value = consulta$telefono)
          updateTextInput(session, "Tcelular", value = consulta$celular)
          updateTextInput(session, "Tcorreo", value = consulta$correo)
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
                    msg <- "Esto cerrara la vacante y los otros candidatos asignados a esta vacante se asignaran a otra vacante \n
                    del mismo cliente y posicion si existiera o a la bolsa de candidatos \n
                    ¿Estas seguro de registrar el proceso de ingreso?"     
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
               id_razon_rechazo <- c_rechazo[c_rechazo$nombre== input$rechazo,]$id
          } else { 
               id_razon_rechazo <- 0 
          }
          
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
          
          #si ingreso, dar de baja la vacante, reasignar otros solicitantes o enviar a bolsa de candidatos
          if (id_proceso==4) {
               #buscar si hay más vacantes de este cliente y puesto
               vac.iguales <- q.vac.iguales(id_vacante = id_vacante)
               
               
               if(nrow(vac.iguales)>0) { #crear registro nuevo para otros candidatos de esa misma vacante
                    registros <- q.following(id_vacante = id_vacante, id_candidato = id_candidato, rechazos = F)
                    
                    if(nrow(registros)>0) {  #hay registros por cambiar de vacante
                         fun.cambiar.vacante(reg.escribir = registros, reg.marcar = registros,
                                             fecha.nueva = NULL, vacante.nva = vac.iguales$id_vacante)
                    }
               }
               
               qupdate <- paste0("UPDATE vacantes SET id_status = 2 WHERE id= ", id_vacante)
               dbExecute(con,qupdate)
               new.vacantes(new.vacantes()+1)  #forzar actualizacion vacantes
          }
          
          dbDisconnect(con)
          #actualiza tabla seguimiento
          new.seguimiento(new.seguimiento()+1) 
          #actualiza las vacantes, si el proceso regitrado cierra el proceso
          if(c_procesos[c_procesos$nombre == proceso,]$cierra_proceso == 1) new.vacantes(new.vacantes()+1)
     }      
     
     #ABC CANDIDATOS ---------------------------------------------------------------------
     observeEvent(input$cmd.nuevo.candidato,{  #reset de candidatos (nuevo candidato)
          req(input$cmd.nuevo.candidato)
          
          updateTextInput(session, "Tid", value = "")
          updateTextInput(session, "Tnombre", placeholder = "Nombre del candidato", value = "")
          updateTextInput(session, "Tdireccion", placeholder = "Calle, No. Colonia, Ciudad, Estado, Pais", value = "")
          updateTextInput(session, "Ttelefono", placeholder = "(lada) telefono", value = "")
          updateTextInput(session, "Tcelular", placeholder = "(lada) telefono", value = "")
          updateTextInput(session, "Tcorreo", placeholder = "correo electronico", value = "")
          updateTextInput(session, "Tcp", placeholder = "CP 5 digitos", value = "")
          updateDateInput(session, "Tnacimiento", value = fecha.hoy)
          
          proxy = dataTableProxy('tabla.seguimiento', session)
          selectRows(proxy, selected = NULL)
          
          #muestra combos de asignacion a vacante
          output$ui.cvacantes <- renderUI({
               vacantes <- q.vacantes(id_status = 1,id_usuario = id_user(), all = F)
               pickerInput("Cvacantes","Vacantes disponibles",
                           choices = unique(vacantes$vacante),
                           options = list('dropupAuto' = T, 'mobile'=T))
          })
          
     })
     
     observeEvent(c(input$Cvacantes, input$cmd.nuevo.candidato),{
          if(is.null(input$Cvacantes)) return(NULL)
          
          vacantes <- q.vacantes(id_status = 1,id_usuario = id_user(), all = F)%>%
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
          
          if(id !=""){
               cat(paste("Borrando candidato:", id), "\n")
               
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
          new.seguimiento(new.seguimiento()+1)
     }
     
     observeEvent(input$cmd.guardar.candidato,{  #boton guardar
          req(input$cmd.guardar.candidato)
          id <- input$Tid
          
          msg <- ""
          if((fecha.hoy - input$Tnacimiento)/365 < 15) msg = "El candidato tiene menos de 15 años, favor de verificar"
          if(grepl("[^0-9]", input$Tcp) | nchar(input$Tcp)!=5) msg = "El codigo postal debe ser un numero de 5 digitos"
          if(input$Tnombre =="") msg = "El nombre del candidato es obligatorio"
          
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
                                 (nombre, direccion, 
                                 telefono, celular, correo,
                                 cp, fecha_nacimiento, id_sexo, 
                                 id_escolaridad, id_medio) ",
                                 "VALUES ('",
                                 nombre.candidato, "', 
                                 '",toupper(input$Tdireccion), "',
                                 '",input$Ttelefono, "',
                                 '",input$Tcelular, "',
                                 '",input$Tcorreo, "',
                                 ", input$Tcp, ", ",
                                 gsub("-","", ymd(input$Tnacimiento)), ", 
                                 ", id_sexo, ", ",
                                 id_escolaridad, ", 
                                 ", id_medio, ")")
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
                              vac.id_usuario = ", id_user())
               id_vacante <- dbGetQuery(con, query)
               
               qinsert <- paste0("INSERT INTO vacantes_following
                                 (id_candidato, id_vacante, id_proceso, fecha)
                                 VALUES (",
                                 id.candidato$id, ",", id_vacante$id, ",", 1, ",", gsub("-","", ymd(fecha.hoy)),")")
               
               dbExecute(con,qinsert)
               new.seguimiento(new.seguimiento()+1)  #actualiza tabla seguimiento
               
          } else {  #actualizar registro
               qupdate <- paste0("UPDATE candidatos 
                                 SET nombre = '", input$Tnombre,
                                 "' ,direccion = '" , toupper(input$Tdireccion),
                                 "', telefono = '" , input$Ttelefono,
                                 "', celular = '" , input$Tcelular,
                                 "', correo = '" , input$Tcorreo,
                                 "' ,cp = ", input$Tcp, 
                                 " ,fecha_nacimiento = ", gsub("-","", ymd(input$Tnacimiento)),
                                 " ,id_sexo = ", id_sexo,
                                 " ,id_escolaridad = ", id_escolaridad,
                                 " ,id_medio = " , id_medio,
                                 " WHERE id = " , id)
               
               dbExecute(con,qupdate)
          }
          dbDisconnect(con) 
     }
     
     #Termina ABC candidatos---
     #termina Registro de avances del proceso---
     
     #ASIGNACION DE CANDIDATOS A VACANTES ---------------------------------------------
     cargar.bolsa.candidatos <- function(){
          db.bolsa.candidatos <- q.bolsa.vacantes()
          
          #filtrar rechazados 
          if(is.null(input$ver.rechazados)){
               db.bolsa.candidatos <- db.bolsa.candidatos%>%filter(id_proceso != 5)
          } else { if(input$ver.rechazados == "Quitar rechazados") {
               db.bolsa.candidatos <- db.bolsa.candidatos%>%filter(id_proceso != 5)
          }}
          
          db.bolsa.candidatos <- db.bolsa.candidatos%>%
               mutate_if(is.character, as.factor)%>%
               arrange(nombre)%>%
               tibble::column_to_rownames("id")%>%
               select(-id_proceso)
          
          return(db.bolsa.candidatos)
     }
     
     output$tabla.bolsa.candidatos <- DT::renderDataTable({
          new.candidatos()
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
     
     cargar.bolsa.vacantes <- function(id_status = 1,id_usuario = id_user(), all = F){
          
          db.bolsa.vacantes <- q.vacantes(id_status = id_status,
                                          id_usuario = id_usuario, all = all)%>%
               mutate_if(is.character, as.factor)%>%
               arrange(cliente)%>%
               tibble::column_to_rownames("id")%>%
               select(cliente, vacante, candidatos, codigo_postal, fecha)
          return(db.bolsa.vacantes)
     }
     
     output$tabla.bolsa.vacantes <- DT::renderDataTable({
          new.vacantes()
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
          
          msg  = "¿Estas seguro que deseas asignar este candidato a la vacante seleccionada?"
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
                     confirmButtonText = "Si, quiero asignarlo",
                     confirmButtonCol = "#AEDEF4",
                     cancelButtonText = "No, cancela asignacion",
                     callbackR = function(x) if(x==T) asignar.a.vacante())
     })
          
     asignar.a.vacante <- function(){
          ren.vac <- input$tabla.bolsa.vacantes_rows_selected
          id.vac <- row.names(db.bolsa.vacantes)[ren.vac]
          if(is.null(ren.vac)) return(NULL)
          
          ren.cand <- input$tabla.bolsa.candidatos_rows_selected
          id.cand <- row.names(db.bolsa.candidatos)[ren.cand]
          if(is.null(ren.cand)) return(NULL)

          #preguntar uno por uno, desde donde comenzar nuevamente el proceso y copiar
          #los procesos ya realizado, con fecha de hoy (ayudará a su indicador)
          query <- paste0("SELECT cand.nombre, vf.*, vp.orden, vp.nombre proceso, vp.cierra_proceso
                          FROM vacantes_following vf
                          LEFT JOIN vacantes_procesos vp ON vp.id = vf.`id_proceso`
                          LEFT JOIN candidatos cand ON cand.id = vf.`id_candidato`
                          WHERE vf.cambio_vacante = 0
                          AND vf.`id_candidato` IN (", paste0(id.cand, collapse=",") , ") 
                          ORDER BY orden DESC")
          con <- conectar()
          datos.asignacion <<- dbGetQuery(con, query)
          dbDisconnect(con)
          
          if(nrow(datos.asignacion%>%filter(cierra_proceso!=1))>1) { #llevo varios procesos de reclutamiento
               todos <- datos.asignacion[datos.asignacion$cierra_proceso==0,]$proceso
               
               nombre = unique(datos.asignacion[datos.asignacion$cierra_proceso== 0,]$nombre)
               mensaje = paste("<b>", nombre,  "</b><br>llego hasta el proceso de <strong>", todos[1] , 
                               "</strong><br>¿Cual es la ultima etapa del proceso anterior que reutilizaras?")
               showModal(selectinputModal(etiqueta = mensaje, opciones.seleccionar = todos,
                                          nombre.selinput = "que.proceso",
                                          nombre.ok = "ok.asignar",label.boton = "Asignar"), session)
               
          } else { 
               fun.cambiar.vacante(reg.escribir = datos.asignacion%>%filter(cierra_proceso!=1), 
                                   reg.marcar = datos.asignacion,
                                   fecha.nueva = fecha.hoy, 
                                   vacante.nva = id.vac)
               #actualiza tablas
               new.vacantes(new.vacantes()+1)
               new.candidatos(new.candidatos()+1)
               
               showNotification("Candidato asignado correctamente", type = "message",duration = 10)
          }
          

     }
     
     #crea un selectinput modal, personalizado
     selectinputModal <- function(failed = FALSE, etiqueta = NULL ,
                                  opciones.seleccionar = NULL, nombre.selinput = "que.proceso",
                                  nombre.ok = "ok.asignar", label.boton = "Asignar") {
          modalDialog(
               selectInput(nombre.selinput, HTML(etiqueta),
                           choices = opciones.seleccionar, multiple = F),
               footer = tagList(
                    modalButton("Cancelar"),
                    actionButton(inputId = nombre.ok, label = label.boton)
               )
          )
     }
     
     observeEvent(input$ok.asignar,{
          removeModal(session)
          cuales.dejar <- c_procesos%>%
               filter(orden <= max(c_procesos[c_procesos$nombre ==input$que.proceso,]$orden))%>%
               select(id)
          datos.dejar <- datos.asignacion%>%
               filter(id_proceso == cuales.dejar$id)
          
          #vacante nueva
          ren.vac <- input$tabla.bolsa.vacantes_rows_selected
          id.vac <- row.names(db.bolsa.vacantes)[ren.vac]
          
          fun.cambiar.vacante(reg.escribir = datos.dejar, reg.marcar = datos.asignacion,
                              fecha.nueva = fecha.hoy, 
                              vacante.nva = id.vac)
          
          #actualiza tablas
          new.vacantes(new.vacantes()+1)
          new.candidatos(new.candidatos()+1)
     })
     
     
     fun.cambiar.vacante <- function(reg.escribir = NULL, reg.marcar = NULL,
                                     fecha.nueva = fecha.hoy,
                                     vacante.nva = NULL){

          # num.cand <- length(datos$id_proceso)
          datos<- reg.escribir%>%
               mutate("nva.vacante" = vacante.nva,
                      "fecha.nva" = ifelse(is.null(fecha.nueva),as.integer(fecha), gsub("-","",ymd(fecha.nueva))))
                      
          valores <- paste(do.call(paste, 
                                   c(datos%>%
                                          select(id_candidato, nva.vacante, id_proceso, fecha.nva, comentarios, 
                                                 id_razon_rechazo)%>%
                                          mutate(comentarios = paste0("'", ifelse(is.na(comentarios),"",comentarios) , "'"),
                                                 id_razon_rechazo = ifelse(is.na(id_razon_rechazo),0,id_razon_rechazo)), 
               sep = ",")),collapse = "),(")
          
          cat(paste("Copiando registros del candidato", unique(datos$id_candidato) , "de vacante", unique(datos$id_vacante), "a nva vacante", unique(datos$nva.vacante) ,"\n"))
          qinsert <- paste0("INSERT INTO vacantes_following
                            (id_candidato, id_vacante, id_proceso, fecha, comentarios, id_razon_rechazo) ",
                            "VALUES (", valores, ")")
          con <- conectar()
          dbExecute(con,qinsert)
          
          #marcar resgistro anterior como cambio de vacante
          cat(paste("Marcando registros de vacante following", unique(reg.marcar$id), "como cambio vacante \n"))
          qupdate <- paste0("UPDATE vacantes_following
                            SET cambio_vacante = 1
                            WHERE id IN (", paste0(reg.marcar$id, collapse = ",") ,")")
          dbExecute(con, qupdate)
          dbDisconnect(con)
          
          new.candidatos(new.candidatos() +1)  #forzar actualizacion candidatos
          new.seguimiento(new.seguimiento()+1)  #forzar actualizacion a seguimiento de vacantes
     }
     
     #TERMINA ASIGNACION DE CANDIDATOS A VACANTES--
     
     
     #GASTOS ------------------------------------------------------------------------------
     
     cargar.gastos <- function(){
          if(is.null(input$fechas.filtros.gastos)) return(NULL)
          fechas <- tiempos(input$fechas.filtros.gastos)
          
          
          gastos <- q.gastos(id_usuario = id_user(), date.inicio = fechas)
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
          new.gastos()
          gastos.raw <<- cargar.gastos()
          if(is.null(gastos.raw)) return(NULL)
          
          db.tabla.gastos <<- gastos.raw%>%
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
          id_gasto <- as.integer(row.names(db.tabla.gastos[ren,]))
          cat(paste("Seleccionado gastos id:", id_gasto , "\n"))
          
          #llenar datos arriba
          updateTextInput(session, "Gid", value = id_gasto)
          updatePickerInput(session,"gastos.conceptos",selected = gastos.raw[gastos.raw$id == id_gasto,]$concepto)
          updateDateInput(session, inputId = "fecha.gasto", value = gastos.raw[gastos.raw$id == id_gasto,]$fecha)
     
          #si prorrateado
          if(gastos.raw[gastos.raw$id == id_gasto,]$id_comun==0){  #sin prorrateo
               seleccion <- gastos.raw[gastos.raw$id == id_gasto,]$medio_asignado
               monto <- gastos.raw[gastos.raw$id== id_gasto,]$monto
          } else {
               seleccion <- gastos.raw[gastos.raw$id_comun == gastos.raw[gastos.raw$id == id_gasto,]$id_comun,]$medio_asignado
               monto <- gastos.raw[gastos.raw$id== id_gasto,]$monto*length(seleccion)
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
               id_comun <- gastos.raw[gastos.raw$id == id,]$id_comun
               texto <- "¿Estas seguro de borrar este gasto?"
               if(id_comun!=0){  #sin prorrateo
                    id <- paste(paste(as.integer(gastos.raw[gastos.raw$id_comun == id_comun,]$id), collapse = ","))
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
          id_comun <- gastos.raw[gastos.raw$id == id,]$id_comun
          if(id_comun!=0){  #sin prorrateo
               id <- paste(paste(as.integer(gastos.raw[gastos.raw$id_comun == id_comun,]$id), collapse = ","))
          }
          
          qupdate <- paste0("UPDATE gastos SET baja = 1 WHERE id IN (" , id, ")")
          
          con <- conectar()
          dbExecute(con,qupdate)
          dbDisconnect(con) 
          
          #quita seleccion
          proxy = dataTableProxy('tabla.gastos', session)
          selectRows(proxy, selected = NULL)
          updateTextInput(session, "Gid", value = "")
          new.gastos(new.gastos()+1)
                         
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
               id_asesor = rep(as.integer(id_user()),num.medios),
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
               
                    #borrar todos y guardar como nuevos (es mucho más sencillo que actualizar)
                    qdelete <- paste0("DELETE FROM gastos WHERE id IN (" , id, ")")
                    dbExecute(con,qdelete)
               }
          }
          
          #insertar candidato y crear proceso inicial con fecha de hoy
          qinsert <- paste0("INSERT INTO gastos
                            (id_cliente, id_concepto, id_medio, id_asesor, id_comun, fecha, monto) ",
                            "VALUES (",
                            datos,
                            ")")
          dbExecute(con,qinsert)
          dbDisconnect(con) 
          new.gastos(new.gastos()+1)
     }
     
     #termina gastos--
     
     #ABC CATALOGOS ---------------------------------------------------------------------
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
     
     #termina catalogos ---
     
     #ABC METAS ---------------------------------------------------------------------

     output$tabla.metas <- renderDataTable({
          new.metas()
          
          con <- conectar()
          c_concepto.metas <<- dbGetQuery(con, "SELECT * FROM metas_kpi WHERE baja = 0")
          dbDisconnect(con)
          
          db.tabla.metas <<- c_concepto.metas%>%
               mutate("fecha.inicio" = ymd(fecha_inicio))%>%
               tibble::column_to_rownames('id')
          
          #cargar combo de posibles kpis
          updatePickerInput(session, "Mdescripcion", choices = unique(c_concepto.metas$descripcion))
          
          datatable(data = db.tabla.metas%>%
                         select(descripcion, valor, fecha.inicio),
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
     
     observeEvent(input$tabla.metas_rows_selected, {
          ren <- input$tabla.metas_rows_selected
          if(is.null(ren)) return(NULL)
          id <- row.names(db.tabla.metas)[ren]
          cat(paste("Seleccionada meta id:", id , "\n"))
          
          #llenar datos arriba               
          updateTextInput(session, "Mid", value = id)
          updatePickerInput(session, "Mdescripcion", selected = db.tabla.metas[ren,]$descripcion)
          updateTextInput(session, "Mmeta", value = db.tabla.metas[ren,]$valor)
          updateDateInput(session, "Mfecha", value = ymd(db.tabla.metas[ren,]$fecha.inicio))
     })
     
     observeEvent(input$cmd.nuevo.meta,{
          updateTextInput(session, "Mid", value = "")
          updateTextInput(session, "Mmeta", value = "")
          updateDateInput(session, "Mfecha", value = ymd(fecha.hoy))

          #quita renglon seleccionado
          proxy = dataTableProxy('tabla.metas', session)
          selectRows(proxy, selected = NULL)
     })

     observeEvent(input$cmd.borrar.meta,{
          req(input$cmd.borrar.meta)
          id <- input$Mid

          cat(paste("Borrando meta:", id, "\n"))
          if(id !=""){

               texto <- paste("¿Estas seguro de eliminar esta meta?")
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
                          confirmButtonText = "Si, eliminar meta",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela eliminar",
                          callbackR = function(x) if(x==T) eliminar.meta())
          }
     })

     eliminar.meta <- function(){
          id <- input$Mid
          
          qupdate <- paste0("UPDATE metas_kpi
                            SET baja = 1 WHERE id IN (" , id, ")")

          con <- conectar()
          dbExecute(con,qupdate)
          dbDisconnect(con)

          updateTextInput(session, "Mid", value = "")
          updateTextInput(session, "Mmeta", value = "")
          updateDateInput(session, "Mfecha", value = ymd(fecha.hoy))
          
          new.metas(new.metas()+1)
     }

     observeEvent(input$cmd.guardar.meta,{  #boton guardar
          req(input$cmd.guardar.meta)
          id <- input$Mid
          
          
          cat(paste("Modificando o agregando meta al id", id, "\n"))
          
          if(grepl("[^0-9//.]", input$Mmeta) | is.na(as.double(input$Mmeta))){
                    shinyalert("Error", "La meta deber ser un numero mayor a cero" , type = "error")
                    return(NULL)
          }

          if(id == ""){ #si es nuevo - se confirma que se agregará el paso inicial como realizado
               msg = paste("¿Estás seguro de querer guardar esta meta?")
          } else {
               msg = paste("¿Estas seguro de guardar los cambios realizados a la meta?")
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
                     callbackR = function(x) if(x==T) guardar.meta())

     })

     guardar.meta <- function(){ #confirmar guardar
          id_valor <- input$Mid
          id_meta = as.integer(c_concepto.metas[c_concepto.metas$descripcion==input$Mdescripcion,]$id_meta)
          descripcion <- input$Mdescripcion
          valor <- input$Mmeta
          fecha <- gsub("-","",input$Mfecha)

          con <- conectar()
          if(id_valor == "") #nuevo registro
          {
               #insertar candidato y crear proceso inicial con fecha de hoy
               qinsert <- paste0("INSERT INTO metas_kpi
                                 (id_meta, descripcion, valor, fecha_inicio, baja)
                                 VALUES (", id_meta,
                                 ",'" , descripcion, 
                                 "' ,", valor, 
                                 ", ", fecha, ",0)")
               dbExecute(con,qinsert)
          } else {
               qupdate <- paste0("UPDATE metas_kpi
                                 SET id_meta = ", id_meta, 
                                 ", descripcion = '", descripcion ,
                                 "', valor = ", valor,
                                 ", fecha_inicio = ", fecha,
                                 " WHERE id = " , id_valor)

               dbExecute(con,qupdate)
          }

          dbDisconnect(con)
          new.metas(new.metas()+1)
     }

     #termina metas ---
     
     #ABC USUARIOS -----------------------------------------------------------------
     output$tabla.usuarios <- DT::renderDataTable({
          new.users()
          
          con <- conectar()
          logins <- q.usuarios(id_status = 1) #solo vacs abiertas
          dbDisconnect(con)
          
          db.tabla.users <<- logins%>%
               tibble::column_to_rownames("id")%>%
               select(nombre, vacantes_abiertas, "nivel" = level, "usuario" = user, "contraseña" = pass)
          
          datatable(data = db.tabla.users,
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
     
     observeEvent(input$tabla.usuarios_rows_selected, {
          ren <- input$tabla.usuarios_rows_selected
          if(is.null(ren)) return(NULL)
          id <- row.names(db.tabla.users)[ren]
          cat(paste("Seleccionado usuario id:", id , "\n"))
          
          #llenar datos arriba               
          updateTextInput(session, "Uid", value = id)
          updateTextInput(session, "Uvac",value = db.tabla.users[ren,]$vacantes_abiertas)
          updateTextInput(session, "Unombre",value = db.tabla.users[ren,]$nombre)
          updateTextInput(session, "Uuser",value = db.tabla.users[ren,]$usuario)
          updatePickerInput(session, "Ulevel",selected = db.tabla.users[ren,]$nivel)
     })
     
     observeEvent(input$cmd.nuevo.user,{
          updateTextInput(session, "Uid", value = "")
          updateTextInput(session, "Uvac", value = "")
          updateTextInput(session, "Unombre",value = "")
          updateTextInput(session, "Uuser",value = "")

          #quita renglon seleccionado
          proxy = dataTableProxy('tabla.usuarios', session)
          selectRows(proxy, selected = NULL)
     })
     
     observeEvent(input$cmd.borrar.user,{
          id <- input$Uid
          
          cat(paste("Borrando usuario:", id, "\n"))
          if(id !=""){
               texto <- paste("Se te pedira que asignes a otro reclutador para sus vacantes abiertas.
                              ¿Estas seguro de borrar este usuario?")
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
                          confirmButtonText = "Si, eliminar usuario",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela eliminar",
                          callbackR = function(x) if(x==T) eliminar.usuario())
          }
     })

     eliminar.usuario <- function(){
          id <- input$Uid
          nombre <- input$Unombre
          num.vacantes <- input$Uvac
          
          if(num.vacantes>0){
          #a quien asignar sus vacantes
          mensaje = paste(nombre, "tiene", num.vacantes, "abiertas. <br>
                          Elige el reclutador al que seras asignadas sus vacantes")
          opciones <- c_reclutadores[c_reclutadores$nombre != nombre & c_reclutadores$level == "reclutador",]$nombre
          
          showModal(selectinputModal(etiqueta = mensaje, opciones.seleccionar = opciones,
                                     nombre.selinput = "que.reclut",
                                     nombre.ok = "ok.reasignar.vacantes",label.boton = "Asignar"), session)
          } else {
               qupdate <- paste0("UPDATE users
                                 SET baja = 1 WHERE id = " , id)
               con <- conectar()
               dbExecute(con,qupdate)
               dbDisconnect(con)
     
               updateTextInput(session, "Uid", value = "id")
               updateTextInput(session, "Uvac", value = "id")
               updateTextInput(session, "Unombre",value = "")
               updateTextInput(session, "Uuser",value = "")

               new.users(new.users()+1)
          }
     }
     
     observeEvent(input$ok.reasignar.vacantes, {
          removeModal(session)
          id_reclut_actual <- input$Uid
          id_reclut_nvo <- as.integer(c_reclutadores[c_reclutadores$nombre == input$que.reclut,]$id)
          
          qupdate <- paste0("UPDATE vacantes
                            SET id_usuario =", id_reclut_nvo,
                            " WHERE id_usuario = ", id_reclut_actual)
          
          con <- conectar()
          dbExecute(con,qupdate)
          
          #baja del usuario
          qupdate.user <- paste0("UPDATE users
                                 SET baja = 1 WHERE id = " , id_reclut_actual)
          dbExecute(con,qupdate.user)
          dbDisconnect(con)
          
          updateTextInput(session, "Uid", value = "id")
          updateTextInput(session, "Uvac", value = "id")
          updateTextInput(session, "Unombre",value = "")
          updateTextInput(session, "Uuser",value = "")
          
          new.users(new.users()+1)
          
     })
     
     observeEvent(input$cmd.guardar.user,{  #boton guardar
          id <- input$Uid
          s.nombre <- input$Unombre
          s.usuario <- input$Uuser
          s.level <- input$Ulevel
          
          cat(paste("Modificando o agregando user", id, "\n"))
          
          errores = 0
          if(s.nombre =="") errores = errores + 1
          if(s.usuario =="") errores = errores + 1
          
          if(grepl("[^a-zA-Z0-9]", s.usuario)){
               shinyalert("Error", "El usuario debe contener solo letras o numeros, sin puntuacion y sin espacios" , type = "error")
               return(NULL)
          }
               
          if(errores>0){
               shinyalert("Error", "Se deben completar todos los campos" , type = "error")
               return(NULL)
          }

          if(id == ""){ #si es nuevo - se confirma que se agregará el paso inicial como realizado
               msg = paste("¿Estás seguro de querer guardar este usuario?")
          } else {
               msg = paste("¿Estas seguro de guardar los cambios realizados a este usuario?")
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
                     callbackR = function(x) if(x==T) guardar.usuario())

     })

     guardar.usuario <- function(){ #confirmar guardar
          id <- input$Uid
          s.nombre <- toupper(input$Unombre)
          s.usuario <- input$Uuser
          s.level <- input$Ulevel
          
          con <- conectar()
          if(id == "") { #nuevo registro
               #revisar que no exista el usuario
               otros <- dbGetQuery(con, paste0("SELECT id FROM users WHERE user = '", s.usuario, 
                                   "' AND baja = 0"))
               if(nrow(otros)>0) {
                    shinyalert("Informacion","Ese nombre de usuario ya existe, elija otro",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               html = FALSE,
                               type = "warning")
                    return(NULL)
               }
               
               #insertar candidato y crear proceso inicial con fecha de hoy
               qinsert <- paste0("INSERT INTO users
                                 (user, level, nombre, baja, reset)
                                 VALUES ('", s.usuario, 
                                 "','" , s.level,
                                 "','", s.nombre,
                                 "', 0, 1)")
               dbExecute(con,qinsert)
               
          } else {
               otros <- dbGetQuery(con, paste0("SELECT id FROM users WHERE user = '", s.usuario, 
                                               "' AND baja = 0 AND id <> ",id))
               if(nrow(otros)>0) {
                    shinyalert("Informacion","Ese nombre de usuario ya existe, elija otro",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = FALSE,
                               html = FALSE,
                               type = "warning")
                    return(NULL)
               }
               
               qupdate <- paste0("UPDATE users
                                 SET user = '", s.usuario,
                                 "', level = '", s.level ,
                                 "', nombre = '", s.nombre,
                                 "', baja = 0, reset = 0
                                  WHERE id = " , id)

               dbExecute(con,qupdate)
          }

          dbDisconnect(con)
          new.users(new.users()+1)
     }
     
     observeEvent(input$cmd.reset.user,{
          id <- input$Uid
          s.nombre <- toupper(input$Unombre)
          
          if(id == "") return(NULL) #si es nuevo - se confirma que se agregará el paso inicial como realizado
          
          msg = paste("¿Estás seguro de permitir una contraseña nueva para", s.nombre,"?\n
                      La proxima vez que se registre, se guardara su nueva contraseña")
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
                     confirmButtonText = "Si, resetear",
                     confirmButtonCol = "#AEDEF4",
                     cancelButtonText = "No, cancela",
                     callbackR = function(x) if(x==T) reset.contra(id= id))
          
     })
     
     reset.contra <- function(id = NULL){
          con <- conectar()
          qupdate <- paste0("UPDATE users
                                 SET reset = 1
                                  WHERE id = " , id)
          dbExecute(con, qupdate)
          
          dbDisconnect(con)
          shinyalert("Informacion","El usuario ya puede ingresar con una nueva contraseña",
                     closeOnEsc = TRUE,
                     closeOnClickOutside = FALSE,
                     html = FALSE,
                     type = "info")
     }
     
     #termina abc usuarios
     
     #ABC VACANTES ----------------------------------------------------------------------
     output$tabla.vacantes.solo <- DT::renderDataTable({
          new.vacantes()
          
          vacantes <- cargar.bolsa.vacantes(id_status = 1, id_usuario = id_user())
          
          datatable(data = vacantes,
                    rownames = T,
                    selection ='single', 
                    filter = "top",
                    autoHideNavigation = T,
                    extensions = 'Scroller',
                    options = list(dom = 't',
                                   scrollX = TRUE,
                                   scrollY = 600,
                                   scroller = TRUE,
                                   fixedHeader = TRUE))
     })
     
     output$tabla.vacantes <- DT::renderDataTable({
          new.vacantes()
          
          if(input$vacantes.cerradas =="Todas"){
               db.vacantes <<- q.vacantes(id_status = c(1,2), all = T)%>%
                    tibble::column_to_rownames("id")
          } else {
               db.vacantes <<- q.vacantes(id_status = c(1), all = T)%>%
                    tibble::column_to_rownames("id")
          }
          
          datatable(data = db.vacantes,
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
     observeEvent(c(input$tabla.vacantes_rows_selected),{
          
          ren <- input$tabla.vacantes_rows_selected
          id_vacante <- row.names(db.vacantes)[ren]
          
          #cargar en controles para editar, borrar o nuevo
          db.vacantes$id <- row.names(db.vacantes)
          updateTextInput(session, "Vid", value = id_vacante)               
          updatePickerInput(session, "Vcliente", selected = db.vacantes[db.vacantes$id== id_vacante,]$cliente)
          updatePickerInput(session, "Vvacante", selected = db.vacantes[db.vacantes$id== id_vacante,]$vacante)
          updatePickerInput(session, "Vreclut", selected = db.vacantes[db.vacantes$id== id_vacante,]$asesor)
          updateDateInput(session, "Vfecha", value = ymd(db.vacantes[db.vacantes$id== id_vacante,]$fecha))

     })
     
     observeEvent(input$cmd.nuevo.vacante,{
          #llenar datos arriba
          updateTextInput(session, "Vid", value = "")
          updateDateInput(session, "Vfecha",value = fecha.hoy)
          
          #quita renglon seleccionado
          proxy = dataTableProxy('tabla.vacantes', session)
          selectRows(proxy, selected = NULL)
     })
     
     observeEvent(input$cmd.borrar.vacante, {
          req(input$cmd.borrar.vacante)
          id <- input$Vid
          cat(paste("Borrando vacante:", id), "\n")
          if(id !=""){
               #si prorrateado
               texto <- "Los candidatos asignados a esta vacante se asignaran a otra vacante del mismo cliente y posicion si existiera o a la bolsa de candidatos \n
               ¿Estas seguro de borrar la vacante?"  

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
                          confirmButtonText = "Si, eliminar vacante",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela eliminar",
                          callbackR = function(x) if(x==T) eliminar.vacante())
               }
     })
     
     
     eliminar.vacante <- function(){  #revisar si requiere nivel para hacer esto
          id_vacante <- input$Vid
          
          #buscar si hay más vacantes de este cliente y puesto
          vac.iguales <- q.vac.iguales(id_vacante = id_vacante)
          
          if(nrow(vac.iguales)>0) { #crear registro nuevo para otros candidatos de esa misma vacante
               registros <- q.following(id_vacante = id_vacante, id_candidato = 0, rechazos = F)
               
               if(nrow(registros)>0) {  #hay registros por cambiar de vacante (cambia fecha de procesos a hoy)
                    fun.cambiar.vacante(reg.escribir = registros, reg.marcar = registros, fecha.nueva = fecha.hoy, vacante.nva = vac.iguales$id_vacante)
               }
          }
          
          con <- conectar()
          qupdate <- paste0("UPDATE vacantes SET baja = 1 WHERE id= ", id_vacante)
          dbExecute(con,qupdate)
          dbDisconnect(con)
          
          new.vacantes(new.vacantes()+1)  #forzar actualizacion vacantes

     }

     observeEvent(input$cmd.guardar.vacante,{  #boton guardar
          req(input$cmd.guardar.vacante)
          id <- input$Vid

          if(id==""){ #si es nuevo - se confirma que se agregará el paso inicial como realizado
               msg = "¿Estás seguro de querer guardar esta vacante nueva?"

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
                     callbackR = function(x) if(x==T) guardar.vacante())

     })

     guardar.vacante <- function(){ #confirmar guardar
          id <- input$Vid
          cat(paste("Vacante a modificar:", id), "\n")
          
          id_cliente <- as.integer(c_clientes[c_clientes$nombre==input$Vcliente,]$id)
          id_vacante <- as.integer(c_vacantes[c_vacantes$nombre==input$Vvacante,]$id)
          id_reclut <- as.integer(c_reclutadores[c_reclutadores$nombre == input$Vreclut,]$id)
          fecha <- gsub("-","", input$Vfecha)
          
          msg <- ""
          if(input$Vvacante =="Nuevo puesto...") msg = "Elija una vacante valida"
          
          if(msg!=""){
               sendSweetAlert(session, "Error", msg , type = "error")
               dbDisconnect(con)
               return(NULL)
          }
          
          con<- conectar()
          if(id== ""){  #modificando, no nuevo - borra y agrega nuevo
               qinsert <- paste0("INSERT INTO vacantes
                                 (id_cliente, id_nombre_vacante, fecha, id_status, id_usuario, baja) ",
                                 "VALUES (", id_cliente, "," ,id_vacante ,
                                 ",", fecha, ",1 ,", id_reclut,  "0)")
               dbExecute(con, qinsert)
          } else {
               qupdate <- paste0("UPDATE vacantes
                                 SET id_cliente = ", id_cliente , 
                                 ", id_nombre_vacante ) " , id_vacante,
                                 ", fecha = " , fecha ,
                                 ", id_usuario = ", id_reclut ,
                                 " WHERE id = " , id)
               
               dbExecute(con, qupdate)
          }

          #insertar candidato y crear proceso inicial con fecha de hoy

          dbDisconnect(con)
          new.gastos(new.gastos()+1)
     }
     #terminar abc vacantes ---
     
     #ABC CLIENTES --------------------------------------------------------------------
     output$tabla.clientes <- renderDataTable({
          new.clientes()
          
          if (input$clientes.activos =="Todos"){
               db.tabla.clientes <<- q.clientes(solo.activos = F)%>%
                    tibble::column_to_rownames('id')
          } else {
               db.tabla.clientes <<- q.clientes(solo.activos = T)%>%
                    tibble::column_to_rownames('id')
          }

          datatable(data = db.tabla.clientes,
                    rownames = T,
                    selection ='single', 
                    filter = "top",
                    autoHideNavigation = T,
                    extensions = 'Scroller',
                    options = list(dom = 't',
                                   scrollX = TRUE,
                                   scrollY = 600,
                                   scroller = TRUE,
                                   fixedHeader = TRUE))
     })
     observeEvent(input$tabla.clientes_rows_selected, {
          ren <- input$tabla.clientes_rows_selected
          if(is.null(ren)) return(NULL)
          id_cte <- row.names(db.tabla.clientes)[ren]
          cat(paste("Seleccionado cliente id:", id_cte , "\n"))
          
          #llenar datos arriba               
          updateTextInput(session, "Ctid", value = id_cte)
          updateTextInput(session, "Ctcliente", value = db.tabla.clientes[ren,]$nombre)
          updateTextInput(session, "Ctdireccion", value = db.tabla.clientes[ren,]$direccion)
          updateTextInput(session, "Cttelefono", value = db.tabla.clientes[ren,]$telefono)
          updateTextInput(session, "Ctcp", value = db.tabla.clientes[ren,]$codigo_postal)
          
     })
     
     observeEvent(input$cmd.nuevo.cliente,{
          updateTextInput(session, "Ctid", value = "")
          updateTextInput(session, "Ctcliente", value = "")
          updateTextInput(session, "Ctdireccion", value = "")
          updateTextInput(session, "Cttelefono", value = "")
          updateTextInput(session, "Ctcp", value = "", placeholder = "Numero de 5 digitos")
          
          #quita renglon seleccionado
          proxy = dataTableProxy('tabla.clientes', session)
          selectRows(proxy, selected = NULL)
     })
     
     observeEvent(input$cmd.borrar.cliente,{
          req(input$cmd.borrar.cliente)
          ren <- input$tabla.clientes_rows_selected
          if(is.null(ren)) return(NULL)
          id <- input$Ctid
          
          if(db.tabla.clientes[ren,]$vacantes_abiertas>0){
               shinyalert("Informacion",
                              "No se puede eliminar un cliente con vacantes abiertas, cierre o borre todas las vacantes de este cliente para poder continuar",
                              type = "info",closeOnEsc = T,closeOnClickOutside = T)
               return(NULL)
          }
               
          cat(paste("Borrando cliente:", id, "\n"))
          if(id !=""){

               #si prorrateado
               texto <- paste("¿Estas seguro de borrar este cliente?")
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
                          confirmButtonText = "Si, eliminar cliente",
                          confirmButtonCol = "#AEDEF4",
                          cancelButtonText = "No, cancela eliminar",
                          callbackR = function(x) if(x==T) eliminar.cliente())
          }
     })

     eliminar.cliente <- function(){
          ren <- input$tabla.clientes_rows_selected
          if(is.null(ren)) return(NULL)
          id_cte <- input$Ctid

          qupdate <- paste0("UPDATE clientes SET baja = 1 WHERE id IN (" , id_cte, ")")

          con <- conectar()
          dbExecute(con,qupdate)
          dbDisconnect(con)

          updateTextInput(session, "Ctid", value = "")
          updateTextInput(session, "Ctcliente", value = "")
          updateTextInput(session, "Ctdireccion", value = "")
          updateTextInput(session, "Cttelefono", value = "")
          updateTextInput(session, "Ctcp", value = "")

          new.clientes(new.clientes()+1)

     }

     observeEvent(input$cmd.guardar.cliente,{  #boton guardar
          req(input$cmd.guardar.cliente)
          id <- input$Ctid
          cat(paste("Modificando o agregando cliente id", id))
          
          errores = 0  #validar completos
          msg <- "El nombre y el codigo postal del cliente son obligatorios"
          if(input$Ctcliente=="") errores = errores + 1
          if(input$Ctcp=="") errores = errrores  + 1
          if(grepl("[^0-9]", input$Ctcp) | nchar(input$Ctcp)!=5){
               errores = errores + 1
               msg = "El codigo postal debe ser un numero de 5 digitos"
          } 
          
          if (errores>0){
               shinyalert("Error", msg , type = "error", 
                          closeOnEsc = T,closeOnClickOutside = T)
               return(NULL)
          }
          
          if(id==""){
               msg = "¿Estas seguro de guardar este nuevo cliente?"
          } else {
               msg = "¿Estas seguro de guardar las modificaciones realizadas al cliente?"
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
                     confirmButtonText = "Si, guarda el cliente",
                     confirmButtonCol = "#AEDEF4",
                     cancelButtonText = "No, cancela",
                     callbackR = function(x) if(x==T) guardar.cliente())

     })

     guardar.cliente <- function(){ #confirmar guardar
          id <- input$Ctid
          nombre.cte = toupper(input$Ctcliente)
          direccion.cte = ifelse(input$Ctdireccion=="","-",toupper(input$Ctdireccion))
          cp.cte = input$Ctcp
          tel.cte = ifelse(input$Cttelefono=="","(00) 0000 0000",input$Cttelefono)

          con <- conectar()
          if(id == "") #nuevo registro
          {
               #insertar candidato y crear proceso inicial con fecha de hoy
               qinsert <- paste0("INSERT INTO clientes
                                 (nombre, direccion, codigo_postal, telefono, baja)
                                 VALUES ('", nombre.cte, "'
                                 ,'", direccion.cte, "'
                                 ,", cp.cte, "
                                 ,'" , tel.cte, "',0
                                 )")
               dbExecute(con,qinsert)
          } else {
               qupdate <- paste0("UPDATE clientes
                                 SET nombre = '", nombre.cte, "',
                                 direccion = '" , direccion.cte, "',
                                 codigo_postal = ", cp.cte, ",
                                 telefono = '", tel.cte,
                                 "' WHERE id = " , id)

               dbExecute(con,qupdate)
          }

          dbDisconnect(con)
          new.clientes(new.clientes()+1)
     }
     #termina abc clientes---
     
     #EXIT LOGIN -----------------------------------------------------------------------
     observeEvent(input$b.salir,{

          output$menu.login <- renderMenu({
               sidebarMenu(
                    menuItem("Ingresar al sistema", tabName = "portada", icon = icon('sign-in'),selected = T),
                    textInput("s.usuario","Usuario:",placeholder = "User: reclut o super"),
                    passwordInput("s.contra", "Contraseña:",placeholder = "Pass: 1234"),
                    actionBttn(inputId = "b.login", label = "Entrar", 
                               style = "jelly", color = "primary")
               )
          })
       
          output$menu.logged <- renderMenu({
               sidebarMenu()
          })
          
          output$menu.reclut <- renderMenu({
               sidebarMenu()
          })
          
          #reset variables
          new.seguimiento(new.seguimiento()+1)
          new.candidatos(new.candidatos()+1)
          new.gastos(new.gastos()+1)
          new.vacantes(new.vacantes()+1)
          new.clientes(new.clientes()+1)
          new.users(new.users()+1)
          
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
     
     #fechas y frecuencias condicionales a menu de scoreboards
     output$ui.fechas <- renderUI({
          new.seguimiento()
          if (!is.null(input$Menu.reclut)) {
               if (input$Menu.reclut %in% c('score',"kpis","supervision")){
                    con <- conectar()
                    query <- paste("SELECT * FROM frecuencias WHERE baja = 0")
                    consulta <- dbGetQuery(con, query)
                    fechas.selected <<- consulta[consulta$default == 1,]$nombre
                    dbDisconnect(con)
                    
                    pickerInput("frecuencia",label = "Periodo", 
                                choices = consulta$nombre,
                                multiple = F,
                                options = list(style = "btn-primary"))
               } else { return(NULL) }
          } else {
               if (!is.null(input$Menu.super)) {
                    if (input$Menu.super %in% c('score',"kpis","supervision")){
                         con <- conectar()
                         query <- paste("SELECT * FROM frecuencias WHERE baja = 0")
                         consulta <- dbGetQuery(con, query)
                         dbDisconnect(con)
                         
                         fechas.selected <<- head(consulta[consulta$default == 1,]$nombre,1)
                        
                         pickerInput("frecuencia",label = "Periodo", 
                                     choices = consulta$nombre,
                                     multiple = F,
                                     options = list(style = "btn-primary"))
                    } else { return(NULL) }  
               } else {return(NULL)}
          }
     })
     
     #caja con las fechas
     output$ui.fechas.filtros <- renderUI({
          if (is.null(input$Menu.reclut)) return(NULL)
          
          if (input$Menu.reclut %in% c('score',"kpis")){
               htmlOutput("fechas.filtros")
          }
     })
     #caja con las fechas
     output$ui.fechas.filtros.super <- renderUI({
          if (is.null(input$Menu.super)) return(NULL)
          
          if (input$Menu.super %in% c('score',"kpis")){
               htmlOutput("fechas.filtros")
          }
     })
     
     output$ui.ver.rechazados <- renderUI({
          if(cp$allow_rechazados==1){  #switch para ver rechazados (cp define si se ve)
               radioGroupButtons(inputId = "ver.rechazados", label = NULL, choices = c("Todos", "Quitar rechazados"), 
                                 status = "danger", selected = "Quitar rechazados",
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")))
          }
     })
     
     #si es rechazo
     output$razon.rechazo <- renderUI({
          if(input$Cproceso == "") return(NULL)
          if(as.integer(c_procesos[c_procesos$nombre == input$Cproceso,]$id) != 5) return(NULL)
          
          con <- conectar()
          c_rechazo <<- dbGetQuery(con, "SELECT * FROM razones_rechazo WHERE baja = 0")
          dbDisconnect(con)
          
          pickerInput("rechazo","Razon de rechazo",
                      choices = c_rechazo$nombre,multiple = F)
     })
     
     #carga combo de fechas en menu
     output$ui.rango <- renderUI({
          #if (is.null(input$Menu.reclut)) return(NULL)
          
          if (input$Menu.reclut == "score"){
               pickerInput("rango.score",label = "Agrupamiento", 
                           choices = c("Semanal","Mensual"),
                           multiple = F,
                           options = list(style = "btn-primary"))
          } else { return(NULL) }
          
     })
     
     # output$ui.filtro.procesos <- renderUI({      
     #      selectInput('filtro.proceso',"Filtrar procesos",
     #                  multiple = T, choices = c_procesos$nombre,                                
     #                  selected = c("Solicitud","Ingreso","Rechazo"))
     # })
     
     output$imagen.inicio <- renderImage({
          return(list(
               src = "www/logo.png",
               contentType = "image/png",
               heigth = '700px',
               width = '1000px',
               alt = "Bienvenido"
          ))
     }, deleteFile = FALSE)
     #Termina generales logins y menus ---  
     
     #CARGAR MENUS --------------------------------------------------------------------
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
          c_clientes <<- q.clientes()
          c_vacantes <<- dbGetQuery(con, "SELECT * FROM vacantes_nombre WHERE baja = 0")
          dbDisconnect(con) 
          
          updatePickerInput(session, "cb.catalogo", choices = c_catalogos$descripcion)
          updatePickerInput(session, "Cescolaridad", choices = c_escolaridad$nombre)
          updatePickerInput(session, "Csexo", choices = c_sexo$nombre)
          updatePickerInput(session, "Cmedio", choices = c_medio$nombre)
          updatePickerInput(session, "Vcliente", choices = c_clientes$nombre)
          updatePickerInput(session, "Vvacante", choices = c_vacantes$nombre)
          updatePickerInput(session, "Vreclut", choices = c_reclutadores$nombre)
          
          observeEvent(new.users(), {
               a <- isolate(new.users())
               updatePickerInput(session, "Vreclut", choices = c_reclutadores$nombre)
          })
          
          output$menu.login <- renderMenu({
               sidebarMenu(
                    p(paste("Bienvenid@", nombre)),
                    actionBttn(inputId = "b.salir", label = "Salir", 
                               style = "bordered", color = "danger",size = "xs", icon = icon("sign-out"))
               )
          })

          if(level == "supervisor"){
               
               output$menu.reclut <- renderMenu({
                    sidebarMenu(
                         uiOutput("ui.reclut")
               )
               })

               output$menu.logged <- renderMenu({
                    sidebarMenu(
                         uiOutput("ui.fechas"),
                         uiOutput("ui.fechas.filtros.super"),
                         menuItem("SCOREBOARD", tabName = "score", icon = icon("tachometer")),
                         menuItem("KPIs", tabName = "kpis", icon = icon("line-chart")),
                         #menuItem("SUPERVISION", tabName = "supervision", icon = icon("stethoscope"), selected = T),
                         menuItem("CLIENTES", tabName = "abc-clientes", icon = icon("industry")),
                         menuItem("VACANTES", tabName = "abc-vacantes", icon = icon("list")),
                         menuItem("METAS", tabName = "abc-metas", icon = icon("trophy")),
                         menuItem("USUARIOS", tabName = "abc-users", icon = icon("users")),
                         menuItem("CATALOGOS", tabName = "catalogos", icon = icon("table")),
                         id = "Menu.super"
                    )
               })
               
               #combos de elegir reclutadores
               output$ui.reclut <- renderUI({
                    new.users()
                    pickerInput("reclut", label = "Datos de", 
                                inline = F, multiple = F, choices = c_reclutadores$nombre, 
                                options = list('dropupAuto' = T, 'mobile'=T,
                                               container=  'body',
                                               style = "btn-primary"))
               })
               
          }
          
          if(level == "reclutador"){
               output$menu.reclut <- renderMenu({
                    return(NULL)
               })

               output$menu.logged <- renderMenu({
                    sidebarMenu(
                         menuItem("SEGUIMIENTO VACANTES", tabName = "abc-registro", icon = icon("bullseye"), selected = T),
                         menuSubItem("BOLSA DE CANDIDATOS", "abc-bolsa", icon = icon("archive")),
                         menuItem("VACANTES", tabName = "solo-vacantes", icon = icon("list")),
                         menuItem("GASTOS", tabName = "abc-gastos", icon = icon("usd")),
                         menuItem("SCOREBOARD", tabName = "score", icon = icon("tachometer")),
                         menuItem("KPIs", tabName = "kpis", icon = icon("line-chart")),
                         uiOutput("ui.fechas"),
                         # uiOutput("ui.rango"),  #por semana, mes
                         uiOutput("ui.fechas.filtros"),
                         id = "Menu.reclut"
                    )
               })
               
               #combos de elegir reclutadores
               output$ui.reclut <- renderUI({
                    pickerInput("reclut", label = "", 
                                inline = T, multiple = F, choices = nombre, 
                                options = list('dropupAuto' = T, 'mobile'=T,
                                               container=  'body'))
               })
          }
     }

})
