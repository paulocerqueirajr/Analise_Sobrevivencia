# Dados covid19

# Limpando o R:

rm(list=ls(all=TRUE))


# Carregando os dados:


dados <- read.table("DadosCovid.txt", h=TRUE, sep="")
dados$IdAlta <- ifelse(dados$desfecho==2, 1, 0)
dados$IdObito <- ifelse(dados$desfecho==1, 1, 0)

  
ekm <- survfit(Surv(tempo, IdAlta)~1, 
               data=dados)
ekm
plot(ekm)

# Comparação por genero:

ekmS <- survfit(Surv(tempo, IdAlta)~sexo, 
               data=dados)
ekmS
plot(ekmS, mark.time=TRUE, 
     lty=c(1,2))

# Comparação por genero - Trocando o evento para obito:

ekmS_obt <- survfit(Surv(tempo, IdObito)~sexo, 
                data=dados)
ekmS_obt
plot(ekmS_obt, mark.time=TRUE, 
     lty=c(1,2))

# Comparação por comorbidades:

ekmCom <- survfit(Surv(tempo, IdAlta)~comorbidades, 
                    data=dados)
ekmCom
plot(ekmCom, mark.time=TRUE, 
     lty=c(1,2))

survdiff(Surv(tempo, IdAlta)~comorbidades, 
        data=dados, rho = 0)

# Comparação por faixa de idade:

ekmId <- survfit(Surv(tempo, IdAlta)~idade, 
                  data=dados)
ekmId
plot(ekmId,lty=c(1,2, 3))
legend("topright", c("0-2","2-11", "11-20"),lty=c(1,2, 3))
survdiff(Surv(tempo, IdAlta)~idade, 
         data=dados, rho = 0)


