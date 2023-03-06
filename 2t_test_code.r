research question: 
Null Hypothesis: Average profit generated from Non-tipping(NT) model is less or equal to average profit generated from Conventional-tipping(CT) model
Alternative Hypothesis: Average profit generated from NT model is greater than average profit generated from CT model

H0: μNT - μCT ≤ 0
Ha: μNT - μCT > 0

t.test(x=dat[Group=='Treatment',Customer],y=dat[Group=='Control',Customer],mu=0,alternative = 'greater')

library(data.table)
library(DT)

analyze.experiment = function(the.dat){
  require(data.table)
  setDT(the.dat)
  
  the.test = t.test(x = the.dat[Group == 'Treatment',Customerflow],y = the.dat[Group == 'Control',Customerflow], alternative = 'greater')
  the.effect = the.test$estimate[1] - the.test$estimate[2]
  lower.bound = the.test$conf.int[1]
  upper.bound = the.test$conf.int[2]
  p = the.test$p.value
  
  result = data.table(effect = the.effect,lower_ci = lower.bound,upper_ci = upper.bound, p = p)
  return(result)
}

B = 1000
n = 30
RNGversion(vstr=3.6)
set.seed(330)
Experiment = 1:B
Group=c(rep.int(x='Treatment',times=n/2),rep.int(x='Control',times=n/2))

sim.dat = as.data.table(expand.grid(Experiment = Experiment,Group = Group))
setorderv(x = sim.dat, cols = c('Experiment','Group'), order = c(1,1))
sim.dat[Group == 'Control', Customerflow:=round(x=rnorm(n=.N,mean=12000,sd=1500),digits=1)]
sim.dat[Group == 'Treatment', Customerflow:=round(x=rnorm(n=.N,mean=12000,sd=1500),digits=1)]

exp.results = sim.dat[,analyze.experiment(the.dat = .SD), keyby = 'Experiment']
DT::datatable(data=round(x=exp.results[,],digits = 3),rownames=F)
