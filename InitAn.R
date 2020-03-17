# Trying out PMI w/ lex. spec. contexts for C&S2018

setwd('~/Google Drive/UniPotsdam/Research/Features/spec_context/')
mat <- read.csv('../CunningsSturtMaterials.csv')
pmi <- read.csv('./specPMI_100cutoff.csv')
#pmi <- droplevels(subset(pmi, Gov %in% mat$verb))
#pmi <- droplevels(subset(pmi, Dep %in% mat$distractor | Dep %in% mat$target))
#pmi <- droplevels(subset(pmi, 'obj-' %in% Rel.Gov))
pmi <- pmi[grepl('obj-', pmi$Rel.Gov),]

#mat$pmi_verb_target <- mat$pmi_verb_distractor <- NA
mat$pmi_verb_target <- mat$pmi_verb_distractor <- 100000.0
for (i in c(1:nrow(mat))) {
  v <- as.character(mat[i,]$verb)
  objv <- paste('obj-', v, sep='')
  t <- as.character(mat[i,]$target)
  d <- as.character(mat[i,]$distractor)
  #if (v %in% pmi$Gov) {
  if (objv %in% pmi$Rel.Gov) {
    if (t %in% pmi[pmi$Rel.Gov == objv,]$Dep) {
      mat[i,]$pmi_verb_target <- pmi[pmi$Rel.Gov == objv & pmi$Dep == t,]$PMI
    } 
    if (d %in% pmi[pmi$Rel.Gov == objv,]$Dep) {
      mat[i,]$pmi_verb_distractor <- pmi[pmi$Rel.Gov == objv & pmi$Dep == d,]$PMI
    }
  }
}

mat[mat$pmi_verb_distractor == 100000,]$pmi_verb_distractor <- NA
mat[mat$pmi_verb_target == 100000,]$pmi_verb_target <- NA
mean(is.na(mat$pmi_verb_target))
mean(is.na(mat$pmi_verb_distractor))

matvalid <- mat[complete.cases(mat),]
matvalid$diff <- matvalid$pmi_verb_distractor - matvalid$pmi_verb_target
mat$diff <- mat$pmi_verb_distractor - mat$pmi_verb_target
library(ggplot2)
ggplot(matvalid, aes(x=tplaus, y=diff, col=dplaus)) + geom_point() #stat_summary(fun.data=mean_cl_boot)
ggplot(mat, aes(x=item, y=diff, col=dplaus)) + geom_point() + facet_grid(tplaus~.)
ggplot(mat, aes(x=pmi_verb_target, y=pmi_verb_distractor, col=diff, shape=tplaus)) + geom_point()
ggplot(mat, aes(x=item, y=pmi_verb_target, col=tplaus)) + geom_point()
ggplot(mat, aes(x=item, y=pmi_verb_distractor, col=tplaus)) + geom_point()

