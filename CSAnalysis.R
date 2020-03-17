# Trying basic analyses w/ updated word/dependency context embeddings

library(ggplot2)
library(brms)
library(bayesplot)

cossim <- function(x, y) {
  x <- unname(unlist(x))
  y <- unname(unlist(y))
  return((x%*%y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))))
}

setwd('~/Google Drive/UniPotsdam/Research/Features/spec_context/')
csmat <- read.csv('../CunningsSturtMaterialsCORRECTED.csv')
# Unclear results/no effect of distr_adv w/ 100 SVD dims w/ minct=100
# Weak effects in opposite direction for 300 SVD dims w/ minct=100
wvecs <- read.csv('./WordVecs100SVD300.csv')
cvecs <- read.csv('./ContextVecs100SVD300.csv')

csmat$targ_sim <- csmat$distr_sim <- NA
# NOTE: some of the verbs don't have DO entries in their past-tense forms, but the do in the
# present tense. Consider using?
for (i in 1:nrow(csmat)) {
  ctx <- paste('obj-', csmat[i,]$verb, sep='')
  csmat[i,]$targ_sim <- cossim(wvecs[csmat[i,]$target, -1], cvecs[cvecs$Rel.Gov==ctx, -1])
  csmat[i,]$distr_sim <- cossim(wvecs[csmat[i,]$distractor, -1], cvecs[cvecs$Rel.Gov==ctx, -1])
}
csmat$dff <- csmat$distr_sim - csmat$targ_sim
csmat$distr_adv <- scale(csmat$dff, center=T, scale=T)

ggplot(csmat, aes(x=tplaus, y=dff, col=dplaus)) + geom_point() + facet_grid(dplaus~.)
ggplot(csmat, aes(x=tplaus, y=distr_adv, col=dplaus)) + geom_boxplot(notch=T)# + facet_grid(dplaus~.)
ggplot(csmat, aes(x=tplaus, y=distr_adv, col=dplaus)) + geom_point() + geom_smooth() + facet_grid(dplaus~.)
csmat[csmat$distr_adv > 2,]
csmat[csmat$item == 19,]
# Item 25: obj(played, piano/stage//bread/kettle); ACTUALLY A TYPO IN MATERIALS!!!!! Corrected

# Not quite as clear a pattern as C&S found in their materials, but there is an outlier that might be throwing it off...
Rmisc::summarySE(csmat, measurevar='targ_sim', groupvars='tplaus', na.rm=T)
#Rmisc::summarySE(csmat[csmat$item!=19,], measurevar='targ_sim', groupvars='tplaus', na.rm=T)
Rmisc::summarySE(csmat, measurevar='distr_sim', groupvars='dplaus', na.rm=T)
#Rmisc::summarySE(csmat[csmat$item!=19,], measurevar='distr_sim', groupvars='dplaus', na.rm=T)
aggregate(distr_sim ~ dplaus, data=csmat, FUN=function(x){return(c(median(x), 2*sd(x)/sqrt(length(x))))})
Rmisc::summarySE(csmat, measurevar='dff', groupvars=c('tplaus', 'dplaus'), na.rm=T)

## Trying it out on CS2018 reading time data
csmat$condition <- with(csmat, ifelse(tplaus == 'plaus' & dplaus == 'plaus', 'a',
                                  ifelse(tplaus == 'plaus' & dplaus == 'implaus', 'b',
                                         ifelse(tplaus == 'implaus' & dplaus == 'plaus', 'c', 'd'))))

# Column names: 'tplaus' = target plausibility; 'dplaus' = distractor plausibility
# 'within_item_diff' = cue_distr_sim - cue_targ _sim
# The larger the difference, the more of a plausibility advantage the distractor has over the target

# Getting C&S data into usable form
csdataexp1 <- read.table('../smith_features/data/CunningsE1.txt')
colnames(csdataexp1) = c("subject", "item", "region", "measure", "condition", "rt")
csdataexp1$subject <- csdataexp1$subject + 100  # making it clear they are from exp.1
csdataexp1$region <- factor(ifelse(csdataexp1$region == 2, "verb", "spillover"))
csdataexp1$condition <- ifelse(csdataexp1$condition == 1, "a", ifelse(csdataexp1$condition == 2, "b", ifelse(csdataexp1$condition == 3, "c", "d")))
csdataexp1$target <- factor(ifelse(csdataexp1$condition == "a" | csdataexp1$condition == "b", "plaus", "implaus"))
csdataexp1$distractor <- factor(ifelse(csdataexp1$condition == "a" | csdataexp1$condition == "c", "plaus", "implaus"))
csdataexp1$exp <- 1.0  # Sum coding
csdataexp1$nozero <- ifelse(csdataexp1$rt == 0, NA, csdataexp1$rt)

csdataexp2 <- read.table('../smith_features/data/CunningsE2.txt')
colnames(csdataexp2) = c("subject", "item", "region", "measure", "condition", "rt")
csdataexp2$subject <- csdataexp2$subject + 200  # making it clear they are from exp.2
csdataexp2$region <- factor(ifelse(csdataexp2$region == 2, "verb", "spillover"))
csdataexp2$condition <- ifelse(csdataexp2$condition == 1, "a", ifelse(csdataexp2$condition == 2, "b", ifelse(csdataexp2$condition == 3, "c", "d")))
csdataexp2$target <- factor(ifelse(csdataexp2$condition == "a" | csdataexp2$condition == "b", "plaus", "implaus"))
csdataexp2$distractor <- factor(ifelse(csdataexp2$condition == "a" | csdataexp2$condition == "c", "plaus", "implaus"))
csdataexp2$exp <- -1.0
# COPY-PASTE ERROR IN FIRST SUBMISSION
#csdataexp2$nozero <- ifelse(csdataexp1$rt == 0, NA, csdataexp2$rt)
csdataexp2$nozero <- ifelse(csdataexp2$rt == 0, NA, csdataexp2$rt)

csdata <- rbind(csdataexp1, csdataexp2)
# Log transform
csdata$logrt <- log(csdata$nozero)
                 
csverb <- droplevels(subset(csdata, region=='verb' & rt != 0 & measure=='tt'))

# Getting similarity differences onto RT dataframe
csverb <- merge(csverb, csmat[,c('item', 'condition', 'targ_sim', 'distr_sim', 'dff', 'distr_adv')])

ggplot(csverb, aes(x=distr_adv, y=logrt, col=as.factor(exp))) + facet_grid(target~., labeller=label_both) +
  geom_point(alpha=0.25) + geom_smooth(method='lm')
#ggplot(csverb[csverb$item!=19,], aes(x=distr_adv, y=logrt, col=as.factor(exp))) + facet_grid(target~., labeller=label_both) +
#  geom_point(alpha=0.25) + geom_smooth(method='lm')

# Setting up nested contrasts
csverb$target <- ifelse(csverb$target == 'plaus', 1.0, -1.0)
csverb$distractor <- ifelse(csverb$distractor == 'plaus', 1.0, -1.0)
csverb$PlausDistrAdv <- ifelse(csverb$target==1, csverb$distr_adv, 0)
csverb$ImplausDistrAdv <- ifelse(csverb$target==-1, csverb$distr_adv, 0)

# Verb analysis of total reading times
cspriors <- c(set_prior('normal(6, 1)', class='Intercept'),
              set_prior('normal(0, 0.5)', class='b'),
              set_prior('normal(0, 0.5)', class='sd'),  # SD of ranef
              set_prior('normal(0, 0.5)', class='sigma'),
              set_prior('lkj(2)', class='cor'))
nda1 <- brm(logrt ~ exp*(target + PlausDistrAdv + ImplausDistrAdv)
            + (1 + target + PlausDistrAdv + ImplausDistrAdv | subject)
            + (1 + exp*(target + PlausDistrAdv + ImplausDistrAdv) | item),
            data=csverb, prior=cspriors, cores=4)
nda1
mcmc_areas(nda1, pars=c('b_exp', 'b_target', 'b_PlausDistrAdv', 'b_ImplausDistrAdv', 'b_exp:target', 'b_exp:PlausDistrAdv', 'b_exp:ImplausDistrAdv'), prob=0.8, prob_outer=0.95) + theme_classic()

# Something seems odd about these results. ImplausDistrAdv should have the effect, not 

