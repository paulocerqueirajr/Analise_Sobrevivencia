##################################################
## Código Implementação verossimilhança em AS  ##
## Autor Paulo Junior                          ##
## Data 17092025                             ##
##################################################

##- limpando r

rm(list = ls(all=TRUE))


##- Funções

logvero - function(par=par, tempos=tempos, delta=delta){
  
  # Parametros
  
  alpha.par - par[1]
  gama.par  - par[2]
  
  # Função densidade
  f - gama.par(alpha.par^gama.par)(tempos^(gama.par-1))exp(-(alpha.partempos)^gama.par) 
  
  # Função sobrevivência
  s - exp(-(alpha.partempos)^gama.par) 
  
  # Função taxa de falha
  h - fs
  
  # Função de verossimilhança

  #lvero - sum(deltalog(f) + (1-delta)s)
  lvero - sum(deltalog(h) + log(s))
  
  return(-lvero)  
}

##- Dados simulados



set.seed(123456789)
n - 1000
alpha.val - 2
gama.val  - 1.5
sigma.val - 1alpha.val
#tempo.e   - (-log(1-runif(n))^(1gama.val))alpha.val
tempo.e   - rweibull(n = n, shape = gama.val, scale = sigma.val)
tempo.c   - rexp(n, rate = 0.1)
tempo.obs - pmin(tempo.e, tempo.c)
delta.obs     - ifelse(tempo.e= tempo.c, 1, 0)

ini.optim - c(1,1)
ajuste - optim(par = c(1,1) , fn = logvero, gr = NULL, 
                method = BFGS, hessian = TRUE, 
                tempos=tempo.obs, delta=delta.obs)
ajuste


## Função de sobrevivência

ekm - survivalsurvfit(Surv(tempo.obs, delta.obs)~1)
tempo - seq(0.0001, 2, length.out=1000)
a - ajuste$par[1]
gama - ajuste$par[2]
stWeib - exp(-(atempo)^gama)

plot(ekm)
lines(tempo, stWeib, col=red)
legend(topright, c(EKM, WEIB.), 
       col=c(black, red), lty=1, bty=n)











