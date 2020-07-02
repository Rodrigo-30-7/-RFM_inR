############################# RFM ##################################

library(tidyr)
library(data.table)

setwd("C:/Users/FelipeHernandez/Desktop/Predictable Media/Fork - RFM/2020")
basebase=fread("consolidado_transacciones_2020_b.csv", sep=";", header = T)

dim(basebase)
View(basebase)
  
base <- subset(basebase, iduser != "")
dim(base)
#class(base$iduser)
  
#View(cliente_unico)
#View(base)
  
base2 <- base  %>%
   separate(orderdate, c("fecha","fecha2"), sep = " ")

cliente_unico <- with( base2, data.frame( iduser = sort(unique(iduser)) ) )
base2$fecha <- as.Date(base2$fecha)

#cliente_unico <- cliente_unico %>%  # separate(iduser,paste0("iduser",1:2),sep = "-")
  
#cliente_unico <- unite(cliente_unico,iduser, c(1,2), sep = "")
  
cliente_unico <- cbind(cliente_unico,ultima_compra = aggregate( round( as.numeric( difftime( Sys.Date(), base2$fecha, units="days")) ), list(base2$iduser), min )$x)
cliente_unico <- cbind(cliente_unico,total_dias = with( base2, as.numeric( by( fecha, iduser, function(x) length(unique(x)) ) ) ))

freq <- rep(1,nrow(base2))
base2 <- cbind(base2,freq)

cliente_unico <- cbind(cliente_unico,total_gastado = with( base2, as.numeric(by(unitprice,iduser,sum) )))

rfm_2 <- quantile(cliente_unico[,2], prob=c(0.25,0.5,0.75,1))
rfm_3 <- quantile(cliente_unico[,3], prob=c(0.25,0.5,0.75,1))
rfm_4 <- quantile(cliente_unico[,4], prob=c(0.25,0.5,0.75,1))

cliente_unico$ultima_compra_q[cliente_unico$ultima_compra <= rfm_2[1]] <- 4
cliente_unico$ultima_compra_q[cliente_unico$ultima_compra > rfm_2[1] & cliente_unico$ultima_compra <=rfm_2[2]] <- 3
cliente_unico$ultima_compra_q[cliente_unico$ultima_compra > rfm_2[2] & cliente_unico$ultima_compra <=rfm_2[3]] <- 2
cliente_unico$ultima_compra_q[cliente_unico$ultima_compra > rfm_2[3] & cliente_unico$ultima_compra <=rfm_2[4]] <- 1

cliente_unico$total_dias_q[cliente_unico$total_dias <= rfm_3[1]] <- 1
cliente_unico$total_dias_q[cliente_unico$total_dias > rfm_3[1] & cliente_unico$total_dias <=rfm_3[2]] <- 2
cliente_unico$total_dias_q[cliente_unico$total_dias > rfm_3[2] & cliente_unico$total_dias <=rfm_3[3]] <- 3
cliente_unico$total_dias_q[cliente_unico$total_dias > rfm_3[3] & cliente_unico$total_dias <=rfm_3[4]] <- 4

cliente_unico$total_gastado_q[cliente_unico$total_gastado <= rfm_5[1]] <- 1
cliente_unico$total_gastado_q[cliente_unico$total_gastado > rfm_4[1] & cliente_unico$total_gastado <=rfm_4[2]] <- 2
cliente_unico$total_gastado_q[cliente_unico$total_gastado > rfm_4[2] & cliente_unico$total_gastado <=rfm_4[3]] <- 3
cliente_unico$total_gastado_q[cliente_unico$total_gastado > rfm_4[3] & cliente_unico$total_gastado <=rfm_4[4]] <- 4

head(cliente_unico)

Modelo_RFM <- unite(cliente_unico, RFM, c(5,6,7),sep="")

head(Modelo_RFM)

RFM_FORK = Modelo_RFM[,c(1,5)]

head(RFM_FORK)
#View(RFM_FORK)

write.csv(RFM_FORK, "RFM_FORK_2020.csv")


