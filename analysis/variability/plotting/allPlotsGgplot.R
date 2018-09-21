
days.pi.pols
dats.type
ys <- aggregate(list(y=dats.type[,"dist"]),
                list(sp=dats.type[, "species"],
                     #x=dats.type[,"site"],
                     days=dats.type[,"median.days"]),
                mean, na.rm=TRUE)

ggplot(days.pi.pols, aes(x = median.days, y = dist)) + 
  geom_point() +
  geom_line(size = 2) + 
  geom_ribbon(aes(ymax = days.pi.pols$phi, 
                  ymin = days.pi.pols$plo), 
              alpha = 0.5) +
  geom_point(data=ys, aes(x = days, y = y))



plot.ggplot <- function(data, fake.data){
ys <- aggregate(list(y=data[,"dist"]),
                list(sp=data[, "species"],
                     days=data[,"median.days"]),
                mean, na.rm=TRUE)

ggplot(fake.data, aes(x = median.days, y = dist)) + 
  geom_point() +
  geom_line(size = 2) + 
  geom_ribbon(aes(ymax = fake.data$phi, 
                  ymin = fake.data$plo), 
              alpha = 0.5) +
  geom_point(data=ys, aes(x = days, y = y))
}

plot.ggplot(dats.type, days.pi.pols)
