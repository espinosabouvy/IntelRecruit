/*candidatos 				q.candidatos() */
SELECT candidatos.id, candidatos.nombre, cp, c_sexo.`nombre` AS 'sexo', c_escolaridad.`nombre`AS 'escolaridad', fecha_nacimiento, medios.`nombre` AS 'medio', direccion 
FROM candidatos
LEFT JOIN c_sexo ON candidatos.`id_sexo` = c_sexo.`id`
LEFT JOIN c_escolaridad ON candidatos.`id_escolaridad` = c_escolaridad.`id`
LEFT JOIN medios ON medios.`id` = candidatos.`id_medio`
WHERE candidatos.`baja`= 0 
AND candidatos.`id` IN (  /*condicional a mis candidatos*/
	SELECT DISTINCT(id_candidato) 
	FROM vacantes_following vf
	WHERE id_vacante IN (
		SELECT id id_vacante FROM vacantes WHERE id_usuario = 5 AND id_status=1)
		)
/*AND candidatos.`id`=517  /*condicional en R a un solo candidato*/
ORDER BY candidatos.id

/*vacantes 						q.vacantes()*/
SELECT vacantes.id, clientes.`nombre` AS 'cliente', vacantes_nombre.`nombre` AS 'vacante', fecha, vacantes_status.`nombre` AS 'status', 
users.`user` AS 'asesor', clientes.`codigo_postal`
FROM vacantes
LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
LEFT JOIN users ON users.id = vacantes.`id_usuario`
WHERE vacantes.`baja`=0
AND vacantes.`id_status` IN (1,2) AND vacantes.`fecha` BETWEEN 20180101 AND 20180201  /*condicional en R*/


/*clientes + con vacantes   						q.clientes()*/
SELECT clientes.*, IF(COUNT(vacantes_abiertas.id_cliente)>0,1,0) AS 'con_vacantes' 
FROM clientes
LEFT JOIN (
	SELECT id_cliente FROM vacantes WHERE id_status = 1) 
AS vacantes_abiertas ON vacantes_abiertas.id_cliente = clientes.`id`
WHERE clientes.`baja`=0
GROUP BY clientes.`id`


/*seguimiento de candidatos - 							q.seguimiento*/
SELECT cand.id AS 'id_candidato', cand.nombre  AS 'candidato', cand.escolaridad, cand.sexo, cand.medio, vacantes_detalle.*, vp.orden AS 'orden_proceso',
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
	WHERE vacantes.id_status= 1 AND vacantes.id_usuario= 5)
	AS vacantes_detalle ON vacantes_detalle.id_vacante = vf.`id_vacante`
WHERE vf.id_candidato NOT IN 
	(SELECT DISTINCT(vacantes_following.id_candidato) FROM vacantes_following
	LEFT JOIN vacantes_procesos ON vacantes_procesos.id = vacantes_following.id_proceso
	WHERE vacantes_procesos.cierra_vacante = 1) 
	AND cand.baja=0
	


/*kpi vacantes 				q.kpi.tiempo.proceso()*/
SELECT va.asesor, va.id AS 'id_vacante', va.fecha_vacante, va.id_status, vp.nombre AS 'proceso', vf.fecha, rc.nombre AS 'razon rechazo'
FROM vacantes_following AS vf
LEFT JOIN vacantes_procesos AS vp ON vp.id = vf.id_proceso 
LEFT JOIN razones_rechazo rc ON rc.`id` = vf.id_razon_rechazo
RIGHT JOIN 
	(SELECT vacantes.id, vacantes.`baja`, clientes.`nombre`, vacantes_nombre.`nombre` AS 'vacante', fecha AS 'fecha_vacante', vacantes.`id_status` AS 'id_status', users.id AS 'id_usuario' ,
	users.`user` AS 'asesor' 
	FROM vacantes
	LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
	LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
	LEFT JOIN users ON users.id = vacantes.`id_usuario`
	WHERE vacantes.`id_status` IN (1,2))
AS va ON va.id = vf.id_vacante
WHERE va.baja = 0
AND va.id_usuario = 4 AND vf.fecha BETWEEN 20000101 AND 20181231



/*embudo - todas las vacantes, como se comportaron los candidatos        q.embudo()*/
SELECT vacantes.id, clientes.`nombre`, vacantes_nombre.`nombre` AS 'vacante', vacantes.fecha AS 'fecha_vacante', vacantes_status.`nombre` AS 'status', 
vacantes.`id_usuario`, users.`user` AS 'asesor' , vf.* 
FROM vacantes
LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
LEFT JOIN users ON users.id = vacantes.`id_usuario`
RIGHT JOIN (
	SELECT  vacantes_following.id_vacante, cand.candidato, cand.sexo, cand.escolaridad, cand.id_medio, cand.medio, proc.nombre AS 'proceso', vacantes_following.`fecha`
	FROM vacantes_following 
	LEFT JOIN (SELECT cand.id, cand.nombre AS 'candidato', c_sexo.`nombre` AS 'sexo', c_escolaridad.`nombre` AS 'escolaridad', cand.`id_medio`, medios.`nombre` AS 'medio'
			FROM candidatos AS cand
			LEFT JOIN c_sexo ON c_sexo.`id` = cand.id_sexo
			LEFT JOIN c_escolaridad ON c_escolaridad.id = cand.id_escolaridad
			LEFT JOIN medios ON medios.`id` = cand.id_medio	 
			) AS cand ON cand.id= vacantes_following.`id_candidato`
	LEFT JOIN vacantes_procesos AS proc ON proc.id = vacantes_following.`id_proceso`
 )AS vf ON vf.id_vacante = vacantes.`id`
WHERE vacantes.`baja`=0 
AND vacantes.`id_status` IN (1,2) AND vacantes.`id_usuario`=4 AND vacantes.fecha BETWEEN 20171221 AND 20180321 AND vacantes_status.`nombre` ="Cerrada"


/*candidatos disponibles - aparece multiples veces, si fue asignado más de una vez a una vacante y que no tengan proceso cerrado                      q.bolsa.vacantes()*/
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
	WHERE vf.id_proceso = 5 OR (vac.id_status = 2 AND vf.id_proceso <> 4) /*rechazado o (vacante cerrada y el no fue ingresado)*/
	AND vf.`cambio_vacante` = 0
	)  
AS va ON va.id_candidato = candidatos.`id`
LEFT JOIN c_sexo ON candidatos.`id_sexo` = c_sexo.`id`
LEFT JOIN c_escolaridad ON candidatos.`id_escolaridad` = c_escolaridad.`id`
LEFT JOIN medios ON medios.`id` = candidatos.`id_medio`
LEFT JOIN (SELECT vac.`id`, vn.`nombre` AS 'vacante_original', clientes.`nombre` AS 'cliente_original', vac.`id_status`
		FROM vacantes AS vac
		LEFT JOIN vacantes_nombre AS vn ON vn.`id`= vac.`id_nombre_vacante`
		LEFT JOIN clientes ON clientes.`id`= vac.`id_cliente`) 
AS vacantes ON vacantes.`id` = va.id_vacante
WHERE candidatos.`baja`=0
ORDER BY id_candidato

/*indicador de gastos por asesor*/
SELECT SUM(monto) gastado 
FROM gastos
WHERE baja = 0 
AND id_asesor = 5
AND fecha >= 20180116

/*gastos por medio y asesor              q.gastos*/
SELECT id_asesor, gc.nombre, medios.id id_medio, medios.`nombre`, monto 
FROM gastos
LEFT JOIN gastos_conceptos gc ON gc.id = gastos.`id_concepto`
LEFT JOIN medios ON medios.id = gastos.`id_medio`
WHERE gastos.`baja` = 0
AND gastos.fecha >= 20180116
AND gastos.`id_asesor`= 5

`catalogos`
SELECT *
FROM vacantes
LEFT JOIN (
	SELECT *
	FROM vacantes_following vf
	WHERE vf.`id_proceso` = 4
AS vf ON vf.id_vacante = vacantes.id
WHERE vacantes.`fecha`>= 20180101
AND id_usuario = 5

/*mis candidatos*/
SELECT DISTINCT(id_candidato) 
FROM vacantes_following vf
WHERE id_vacante IN (
	SELECT id id_vacante FROM vacantes WHERE id_usuario = 5 AND id_status=1)

/*ultimo proceso por candidato*/
SELECT vf.id_candidato, vf.id_proceso, vf.id_razon_rechazo, vac.id_status
FROM vacantes_following vf
LEFT JOIN vacantes_procesos vp ON vp.id = vf.id_proceso
RIGHT JOIN (
	SELECT vf.id_candidato, MAX(vp.orden) orden
	FROM vacantes_following vf
	LEFT JOIN vacantes_procesos vp ON vp.id = vf.id_proceso
	GROUP BY id_candidato
	) ultimo ON ultimo.id_candidato = vf.id_candidato AND 
			 ultimo.orden = vp.orden
 LEFT JOIN vacantes vac ON vac.id = vf.id_vacante
 WHERE vf.id_proceso = 5 OR (vac.id_status = 2 AND vf.id_proceso <> 4)

/*seguimiento de vacantes*/
SELECT vacantes.*, candidatos.`nombre` AS 'candidato', vacantes_procesos.`nombre` AS 'proceso', vacantes_following.fecha, vacantes_following.comentarios FROM vacantes_following
LEFT JOIN candidatos ON candidatos.`id` = vacantes_following.`id_candidato`
LEFT JOIN (
	SELECT vacantes.`id`, clientes.`nombre`, vacantes_nombre.`nombre` AS 'vacante', vacantes_status.`nombre` AS 'status', users.`nombre` AS 'asesor' 
	FROM vacantes
	LEFT JOIN clientes ON clientes.id = vacantes.`id_cliente`
	LEFT JOIN vacantes_nombre ON vacantes_nombre.id = vacantes.`id_nombre_vacante`
	LEFT JOIN vacantes_status ON vacantes_status.id = vacantes.`id_status`
	LEFT JOIN users ON users.id = vacantes.`id_usuario`) AS vacantes ON vacantes.`id` = vacantes_following.`id_vacante`
LEFT JOIN vacantes_procesos ON vacantes_procesos.`id` = vacantes_following.`id_proceso`

/*gastado*/
SELECT SUM(monto) gastado 
FROM gastos
WHERE baja = 0 
AND fecha >=  20180101
AND id_asesor = 5

/*gasto por vacantes cerradas*/
SELECT me.nombre medio, cerradas, IF(gastado IS NULL, 0, gastado) gastado, ROUND(IF(gastado/cerradas IS NULL, 0, gastado/cerradas),0) costo
FROM(
	SELECT ca.id_medio, COUNT(ca.id) cerradas
	FROM candidatos ca
	LEFT JOIN vacantes_following vf ON vf.`id_candidato` = ca.id
	LEFT JOIN vacantes va ON va.id = vf.`id_vacante`
	WHERE vf.`id_proceso` = 4 AND va.fecha >= 20180101 AND va.id_usuario = 5
	GROUP BY (ca.id_medio)) AS cerradas
LEFT JOIN (
	SELECT ga.id_medio, SUM(monto) gastado
	FROM gastos ga
	WHERE baja = 0 
	AND ga.fecha >= 20180101
	GROUP BY (id_medio)) AS gasto ON gasto.id_medio = cerradas.id_medio
LEFT JOIN medios me ON me.id = cerradas.id_medio

/*gastado*/
SELECT *
FROM(
	SELECT ca.id_medio, COUNT(ca.id) cerradas
	FROM candidatos ca
	LEFT JOIN vacantes_following vf ON vf.`id_candidato` = ca.id
	LEFT JOIN vacantes va ON va.id = vf.`id_vacante`
	WHERE vf.`id_proceso` = 4 AND va.fecha >= 20180101 AND va.id_usuario = 5
	GROUP BY (ca.id_medio)) AS cerradas
RIGHT JOIN (
	SELECT id_medio, SUM(monto) gastado
	FROM gastos ga
	WHERE baja = 0 
	AND ga.fecha >= 20180101
	GROUP BY (id_medio)) AS gastado ON gastado.id_medio = cerradas.id_medio
	
/*gastos 			q.gastos()*/
SELECT ga.id, gc.id id_concepto, gc.nombre concepto, me.id id_medio, IF(me.nombre IS NULL, 'NINGUNO', me.nombre) medio_asignado, 
fecha, monto, id_comun
FROM gastos ga
LEFT JOIN gastos_conceptos gc ON gc.id = ga.id_concepto
LEFT JOIN medios me ON me.id = ga.id_medio
WHERE ga.baja = 0
AND fecha >= 20180116
AND ga.id_asesor = 5


/*procesos realizados por candidato*/
SELECT cand.nombre, vf.*, vp.orden, vp.nombre proceso
FROM vacantes_following vf
LEFT JOIN vacantes_procesos vp ON vp.id = vf.`id_proceso`
LEFT JOIN candidatos cand ON cand.id = vf.`id_candidato`
WHERE vf.`id_candidato` IN (530)