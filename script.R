#crear follow random
candidatos <- q.candidatos()
vacantes <- q.vacantes(all=T)
follow <- data.frame("id_candidato" = candidatos$id)
rens <- nrow(follow)
follow$id_vacante <- sample(vacantes$id,size = rens,replace = T)
follow <- merge(follow, vacantes%>%
                     mutate("fecha.vacante" = ymd(vacantes$fecha))%>%
                     select("id_vacante" = id, fecha.vacante), 
                by = "id_vacante", all.x = T)

follow$uno <- follow$fecha.vacante + abs(round(rnorm(rens,1.5,3),0))
follow$dos <- ifelse(sample(c(0,1),rens, replace = T, prob = c(0.6,0.4))==0,
                     NA, follow$uno + abs(round(rnorm(rens,2.5,1),0)))
follow$dos <- as_date(follow$dos)
follow$tres <- ifelse(is.na(follow$dos),NA, 
                      ifelse(sample(c(0,1),rens, replace = T, prob = c(0.7,0.3))==0,
                     NA, follow$dos + abs(round(rnorm(rens,2.5,1),0))))
follow$tres <- as_date(follow$tres)
follow$cuatro <- ifelse(is.na(follow$tres),NA,
                        follow$tres + abs(round(rnorm(rens,1,2))))
follow$cuatro <- as_date(follow$cuatro)
follow$cinco <- ifelse(!is.na(follow$cuatro),NA, 
                       ifelse(sample(c(0,1),rens, replace = T, prob = c(0.7,0.3))==0,
                              NA, follow$dos + abs(round(rnorm(rens,2.5,1),0))))
follow$cinco <- as_date(follow$cinco)
names(follow)<- c("id_vacante","id_candidato","fecha.vacante","1","2","3","4","5")

follow <- follow%>%select(-fecha.vacante)
vacantes_following <- gather(follow, "id_proceso","fecha", 3:ncol(follow))
vacantes_following <- vacantes_following[!is.na(vacantes_following$fecha),]
vacantes_following$fecha <- gsub("-","", vacantes_following$fecha)
vacantes_following <- vacantes_following%>%
     mutate("id_razones_rechazo" = ifelse(id_proceso!=5,"",sample(c(1:4),nrow(vacantes_following),T)))
