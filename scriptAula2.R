## Código de aula: Representação dos dados de sobrevivência
## Autor: Paulo Cerqueira dos Santos Junior
## Data: 

#--- Limpando R:

rm(list=ls(all=TRUE))

#--- Pacotes:

require(survival)

#--- Exemplo de dados:

data(package="survival")

#--- Dados de cancer no ovário:

dados <- survival::ovarian

#--- Usando a função surv:

objsurv <- Surv(dados$futime,  dados$fustat)

#--- EKM:

ekm <- survfit(objsurv ~ 1)

ekm <- survfit(Surv(dados$futime,  dados$fustat)~1)
summary(ekm)
plot(ekm, xlab="Tempo", ylab="S(t) estimada", conf.int = FALSE)

plot(ekm, xlab="Tempo", 
     ylab="H(t) estimada", mark.time=TRUE, 
     cumhaz = TRUE )

#-- NA:

ENA <- survfit(coxph(Surv(dados$futime,  dados$fustat)~1))
plot(ENA, xlab="Tempo", ylab="S(t) estimada NA",mark.time=TRUE)
plot(ENA, xlab="Tempo", ylab="S(t) estimada NA",
     mark.time=TRUE, cumhaz = TRUE)


## Estratificado:

ekm_strat <- survfit(Surv(dados$futime,  dados$fustat)~dados$resid.ds)
ekm_strat

plot(ekm_strat , xlab="Tempo", ylab="S(t) estimada", mark.time=TRUE, lty=c(1,2))
legend("bottomleft", c("Não", "Sim"), lty=c(1,2), bty="n")

## Teste Log-rank:

survdiff(Surv(dados$futime,  dados$fustat)~dados$resid.ds,rho=0)
survdiff(Surv(dados$futime,  dados$fustat)~dados$resid.ds,rho=1)




















