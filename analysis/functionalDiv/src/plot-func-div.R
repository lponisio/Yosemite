## ************************************************************
## plotting
## ************************************************************ 
load(file="saved/traits/spec.Rdata")
load(file="saved/traits/nestLoc.Rdata")
load(file="saved/traits/nestBuild.Rdata")
load(file="saved/traits/social.Rdata")
load(file="saved/traits/fireKill.Rdata")
load(file="saved/traits/wood.Rdata")

c.traits <- c('Specialization')
div.traits <- c('Nest Construction Diversity',
                'Nest Location Diversity',
                'Sociality Diversity',
                'Fire survival',
                'Nest material diversity')

plot.func(list(specialization.bees$null.func,
               nestBuild$null.func,
               nestLoc$null.func,
               social$null.func,
               fire$null.func,
               wood$null.func),
          list(specialization.bees$obs$dats,
               nestBuild$obs$dats,
               nestLoc$obs$dats,
               social$obs$dats,
               fire$obs$dats,
               wood$obs$dats),
          traits=c(c.traits, div.traits),
          sub='bee', occ='occ')

plot.box(list(specialization.bees$obs$traits,
              nestBuild$obs$traits,
              nestLoc$obs$traits,
              social$obs$traits,
              fire$obs$traits,
              wood$obs$traits),
         unlist(comm.bee$status),
         traits=c(c.traits, div.traits),
         sub='bee', occ='occ')

plot.quantile(list(specialization.bees$quantile,
                 nestBuild$quantile,
                 nestLoc$quantile,
                 social$quantile,
                   fire$quantile,
                   wood$quantile),
              unlist(comm.bee$status),
         cases=c(c.traits, div.traits),
         sub='bee', occ='occ') 


specialization.bees$pvals
nestBuild$pvals
nestLoc$pvals
social$pvals
fire$pvals
wood$pvals
