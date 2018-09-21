## setwd('~/Dropbox/speciesRoles')
rm(list=ls())
setwd('analysis/variability')
source('src/initialize.R')
f.path <- 'figures'
type <- "all"

pol.traits <- traits.ab.phen[traits.ab.phen$speciesType == 
"pollinator",c("GenusSpecies", "occ.date", "occ.date.minusBaci")]

load(file='saved/speciesTurnover/chao_TRUE_abund_pols.Rdata') #Partner varialbility
load(file="~/Dropbox/speciesRoles_saved/pcaScores.Rdata") #role variability
load(file="~/Dropbox/speciesRoles_saved/contrNodf.Rdata") #contribution to nodf variabiity

## getting only the varibles I am interested
partner <- dats.type[,c("species", "dist")]
role <- pol.pca.scores$pca.var[,c("GenusSpecies", "var.pca1")]
role <- role[!is.na(role$var.pca1),]
contr.nodf <- traits.contr.nodf[traits.contr.nodf$speciesType == "pollinator",
                                c("GenusSpecies", "contr.nodf")]
## finction to calculate z
calc.z <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

## getting the mean per species
partner <- partner %>%
  plyr::rename(c("species"="GenusSpecies")) %>%
  group_by(GenusSpecies)  %>%
  summarize(var.dist = mean(dist))
            
partner$z.dist <- calc.z(partner$var.dist)

role <- role %>%
  group_by(GenusSpecies)  %>%
  summarize(var.pca1 = mean(var.pca1))
role$z.pc1 <- calc.z(role$var.pca1)

contr.nodf <- contr.nodf %>%
  group_by(GenusSpecies)  %>%
  summarize(var.cnodf = mean(contr.nodf))
contr.nodf$z.cnodf <- calc.z(contr.nodf$var.cnodf)

## merging the results 
all.zs <- merge(partner, role, by="GenusSpecies")
all.zs <- merge(all.zs, contr.nodf, by="GenusSpecies")
all.zs$z.all <- rowMeans(all.zs[, c("z.dist", "z.pc1", "z.cnodf")])

all.zs$GenusSpecies <- factor(all.zs$GenusSpecies, levels = all.zs$GenusSpecies[order(all.zs$z.all)])

all.zs <- merge(all.zs, pol.traits, by="GenusSpecies")
all.zs.original <- all.zs

write.csv(all.zs, "~/Dropbox/speciesRoles_saved/zPersist.csv")

cor.test(all.zs.original$occ.date, all.zs.original$var.dist)
cor.test(all.zs.original$occ.date, all.zs.original$var.pca1)
cor.test(all.zs.original$occ.date, all.zs.original$var.cnodf)
cor.test(all.zs.original$occ.date, all.zs.original$z.all)

#testing the significance without the most persistent ones
all.zs.less <- all.zs.original[all.zs.original$occ.date<0.3,]
cor.test(all.zs.less$occ.date, all.zs.less$var.dist)
cor.test(all.zs.less$occ.date, all.zs.less$var.pca1)
cor.test(all.zs.less$occ.date, all.zs.less$var.cnodf)
cor.test(all.zs.less$occ.date, all.zs.less$z.all)

#testing the significance with occ.minus Baci 
cor.test(all.zs.original$occ.date.minusBaci, all.zs.original$var.dist)
cor.test(all.zs.original$occ.date.minusBaci, all.zs.original$var.pca1)
cor.test(all.zs.original$occ.date.minusBaci, all.zs.original$var.cnodf)
cor.test(all.zs.original$occ.date.minusBaci, all.zs.original$z.all)

#testing the significance without the most persistent ones with occ.minus Baci 
all.zs.original$occ.date.minusBaci[order(all.zs.original$occ.date.minusBaci)]
all.zs.less <- all.zs.original[all.zs.original$occ.date.minusBaci<0.3,]
cor.test(all.zs.less$occ.date.minusBaci, all.zs.less$var.dist)
cor.test(all.zs.less$occ.date.minusBaci, all.zs.less$var.pca1)
cor.test(all.zs.less$occ.date.minusBaci, all.zs.less$var.cnodf)
cor.test(all.zs.less$occ.date.minusBaci, all.zs.less$z.all)

all.zs <- melt(all.zs, id.vars = c("GenusSpecies", "occ.date.minusBaci"))

## Plotting 
#function to rename the labels
labeli <- function(variable, value){
  names_li <- list("var.dist"="Partner", 
                   "z.dist"="Partner",
                   "var.pca1"="Role", 
                   "z.pc1"="Role",
                   "var.cnodf"="Structural", 
                   "z.cnodf"="Structural",
                   "z.all"="Interaction")
  return(names_li[value])
}

### all values per species
ggplot(all.zs[grep(pattern = "z", all.zs$variable),], 
       aes(x=GenusSpecies, y=value, group=GenusSpecies)) + theme_bw() +
  geom_point(aes(color=variable, shape=variable), size=2.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, face="italic", size=12, family="Times"),
        axis.text.y = element_text(size=16, family="Times"),
        axis.title=element_text(size=16, family="Times"),
        panel.grid = element_blank(), legend.position="bottom", 
        legend.text=element_text(size=16, family="Times"))+
  scale_color_manual(name="",
                     labels = c("Partner", "Role", "Structural", "Flexibility"), 
                     values = c("#68829E", "#AEBD38", "#598234", "grey26")) +
  scale_shape_manual(name="",
                     labels = c("Partner", "Role", "Structural", "Flexibility"), 
                     values = c(16, 17, 18, 8)) +
  xlab("") + ylab("Interaction flexibility")

ggsave(filename = file.path(f.path, sprintf("%s.pdf", "meanFlex")),
       width=11, height=8)

### Plotting the zs separately 
dat <- all.zs[grep(pattern = "z", all.zs$variable),]

ggplot(dat, aes(x=occ.date.minusBaci, y=value)) + theme_bw() +
  geom_point(size=1.2) + 
  facet_grid(variable~., labeller=labeli)+ 
  theme(axis.text.x = element_text(size=14, family="Times"),
        axis.text.y = element_text(size=14, family="Times"),
        axis.title=element_text(size=16, family="Times"),
        panel.grid = element_blank(),
        strip.text.y = element_text(size = 14, family="Times"),
        legend.position="none") +
  stat_smooth(data=dat[dat$variable=="z.dist",], na.rm=TRUE,
              method = "lm", size = 1, alpha = 0.35) +
  stat_smooth(data=dat[dat$variable=="z.pc1",], na.rm=TRUE,
              method = "lm", size = 1, alpha = 0.35) +
  stat_smooth(data=dat[dat$variable=="z.all",], na.rm=TRUE,
              method = "lm", size = 1, alpha = 0.35) +
  stat_smooth(data=dat[dat$variable=="z.cnodf",], na.rm=TRUE,
              method = "lm", size = 1, alpha = 0.35) +
  ylab("Variability") + xlab("Persistence") 

ggsave(filename = file.path(f.path, sprintf("%s.pdf", "allZs")), 
       width=5, height=8)

### Plotting the original values  
dat <- all.zs[grep(pattern = "var", all.zs$variable),]
ggplot(dat, aes(x=occ.date.minusBaci, y=value)) + theme_bw() +
  geom_point(size=1.5) + 
  theme(panel.grid = element_blank()) + 
  facet_grid(variable~., scales = "free", 
             labeller=labeli) +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=14),
        panel.grid = element_blank(),
        strip.text.x = element_text(size = 20)) +
  stat_smooth(data=dat[dat$variable=="var.dist",], na.rm=TRUE,
              method = "lm", size = 1, alpha = 0.35) +
  stat_smooth(data=dat[dat$variable=="var.pca1",], na.rm=TRUE,
              method = "lm", size = 1, alpha = 0.35) +
  stat_smooth(data=dat[dat$variable=="var.cnodf",], na.rm=TRUE,
              method = "lm", size = 1, alpha = 0.35) +
  ylab("Variability") + xlab("Persistence")
ggsave(filename = file.path(f.path, sprintf("%s.pdf", "rawValsVar")),
       width=5, height=6)
####################################################
## creating table for the suplementary 
colnames(all.zs.original)
all.zs.table <- all.zs.original[,c("GenusSpecies","occ.date.minusBaci", "var.dist", 
                                   "var.pca1", "var.cnodf", "z.all")]
colnames(all.zs.table) <- c("Species", "Persistence", "Partner var.", "Role var.", "Structural var.", "Interaction flex.")
all.zs.table[,2:6] <- round(all.zs.table[,2:6], 4)
write.csv(x=all.zs.table, file = file.path(f.path, sprintf("%s.csv", "varValues")))
all.zs.table[,1:2]

## Creating a table where spp are ordered by the variable
sppPartner <- as.character(all.zs.table$Species[order(all.zs.table$`Partner var.`)])
sppRole <- as.character(all.zs.table$Species[order(all.zs.table$`Role var.`)])
sppStruct <- as.character(all.zs.table$Species[order(all.zs.table$`Structural var.`)])
sppMean <- as.character(all.zs.table$Species[order(all.zs.table$`Interaction flex.`)])
sppPersist <- as.character(all.zs.table$Species[order(all.zs.table$Persistence)])
spp.order <- as.data.frame(cbind(sppPartner, sppRole, sppStruct, sppMean))

