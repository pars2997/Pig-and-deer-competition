################################################################################
###############  Analyze isotope data for all species ##########################
############## And detailed analysis of pig and deer isotope overlap ###########
############### Mitchell Parsons ###############################################
############### February 23, 2024 ##############################################

#### Load packages ####
library(SIBER)
library(viridis)
library(coda)
library(tidyverse)


#### Load data ####
# read in raw isotope data
# remove failed samples and add groups for broad species categories
dat <- read.csv("../RawData/UNM_isotopes2.csv")
# dat <- dat[!is.na(dat$N_isotope),]
dat[dat$Species == "ODHE","Species"] <- "Deer"
dat[dat$Species == "SUSC","Species"] <- "Pig"
head(dat)

# plot bioplot of C and N isotopes with colors denoting different species groups
par(mar=c(5,5,2,2))
plot(d15N ~ d13C,pch = 19,data = dat[dat$Species == "Deer",]
     ,xlim =c(-28,-20),ylim=c(0,9),
     cex.lab = 1.75, cex.axis = 1.25,xlab = "C isotope",ylab = "N isotope")
points(d15N ~ d13C,pch = 19,data = dat[dat$Species == "Pig",],col = "red")

legend(x = "topleft",legend = c("Deer","Pig"),
       cex = 1, pch = 19, col = c("black","red"))

# calculate mean and standard deviation for each group for carbon
Nmeans <- tapply(dat$d15N,dat$Species,mean)
N_sds <- tapply(dat$d15N,dat$Species,sd)
N_ses <- N_sds/sqrt(table(dat$Species))

# calculate mean and standard deviation for each group for nitrogen
Cmeans <- tapply(dat$d13C,dat$Species,mean)
C_sds <- tapply(dat$d13C,dat$Species,sd)
C_ses <- C_sds/sqrt(table(dat$Species))

# create dataframe of means and sds for each 
meansdf <- data.frame(cbind(Nmeans,N_ses,Cmeans,C_ses))
meansdf$color <- c("black","red")

# Make plot of means plus or minus standard errors for each species
plot(Nmeans ~ Cmeans,pch = 19, cex = 2, xlim = c(-26, -20),ylim = c(0,8),col = color, data = meansdf,
     cex.lab = 1.75, cex.axis = 1.25,xlab = "C isotope",ylab = "N isotope")
arrows(x0 = meansdf$Cmeans, y0 = meansdf$Nmeans - meansdf$N_ses, 
       x1 = meansdf$Cmeans, y1 = meansdf$Nmeans + meansdf$N_ses,
       angle = 90, length = 0.15, code = 3)
arrows(x0 = meansdf$Cmeans - meansdf$C_ses, y0 = meansdf$Nmeans, 
       x1 = meansdf$Cmeans + meansdf$C_ses, y1 = meansdf$Nmeans,
       angle = 90, length = 0.15, code = 3)
legend(x = "topright",legend = c("Deer","Pig"),
       cex = 1.5, pch = 19, col = c("black","red"))

#### Overlap analysis of deer and pigs ####
palette(viridis(2, option = "G", begin = 0.8, end = 0.2))
# set that all samples come from the same community
dat$community <- "one"
# dat <- dat %>% 
#   mutate(community = case_when(
#     Year == "2021" ~ "one",
#     Year == "2022" ~ "two",
#     Year == "2023" ~ "three"
#   ))

# format data for analysis with SIBER package
# iso 1 is C, iso 2 is N, group is species
# filtered out a few outliers that are likely poorly prepped samples
siberdat <- dat[dat$Species %in% c("Deer","Pig"),c("d13C","d15N","Species","community")]
# siberdat <- dat[dat$Year == 2023,c("d13C","d15N","Species","community")]

colnames(siberdat) <- c("iso1","iso2","group","community")

# create object for siber analysis
siber.data <- createSiberObject(siberdat)

# set plotting arguments
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hulls.args <- list(lty = 2, col = "grey20")

# plot raw data with 95% elipses for each species
# red is pig, black is deer
par(mfrow = c(1,1))
plotSiberObject(siber.data,
                ax.pad = 2,
                pch = 19,
                hulls = F, community.hulls.args = community.hulls.args,
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = F,group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                ylab = expression(paste(delta^(15),"N (\u2030)")),
                xlab = expression(paste(delta^(13),"C (\u2030)"))
                )
legend(x = "topright", legend = c("deer","pig"), pch = 16, 
       col = c("#43BBADFF", "#35264CFF"))

# Plot without ellipses
plotSiberObject(siber.data,
                ax.pad = 2,
                hulls = F,
                ellipses = F,
                group.hulls = F,
                bty = "L",
                iso.order = c(1,2),
                ylab = expression(paste(delta^(15),"N (\u2030)")),
                xlab = expression(paste(delta^(13),"C (\u2030)")),
                cex = 0.5
                )

# calculate total hull area, ellipse area, and small sample size corrected ellipse area
group.ML <- groupMetricsML(siber.data)
group.ML

png("../Figures/isotopeplot.png",res = 300,
     height = 5.5, width = 8, units = "in")
par(mar = c(5,6,2,2))
par(bg = "#F3FBF5")
plot(siberdat$iso1,siberdat$iso2, col = as.factor(siberdat$group), 
     pch = 19,
     ylab = expression(paste(delta^(15),"N (\u2030)")),
     xlab = expression(paste(delta^(13),"C (\u2030)")),
     xlim = c(-28,-20),
     ylim = c(-2,10),
     cex.lab = 2.25,
     cex.axis = 1.75,
     cex = 2
     )

legend(x = "topright", legend = c("deer","pig"), pch = 19,
       col = c("#43BBADFF", "#35264CFF"),cex = 1.75)

plotGroupEllipses(siber.data, n = 100, p.interval = 0.95, lty = 1, lwd = 2)
# plotGroupEllipses(siber.data, n = 100, p.interval = 0.95, ci.mean = T, lty = 1, lwd = 2)

dev.off()
# Calcualte diet overlap based on 95% ellipse and mean ellipse
ellipse1 <- "one.Deer"
ellipse2 <- "one.Pig"
# ellipse3 <- "two.Deer"
# ellipse4 <- "two.Pig"
# ellipse5 <- "three.Deer"
# ellipse6 <- "three.Pig"

sea.overlap <- maxLikOverlap(ellipse1,ellipse2,siber.data,
                             p.interval = NULL,n = 100)

ellipse95.overlap <- maxLikOverlap(ellipse1, ellipse2,siber.data,
                                   p.interval = 0.95, n = 100)

# #2021
# ellipse95.overlap <- maxLikOverlap(ellipse1, ellipse2,siber.data,
#                                    p.interval = 0.95, n = 100)
# 
# ellipse95.overlap[3]/ellipse95.overlap[1]
# 
# #2022
# ellipse95.overlap <- maxLikOverlap(ellipse3, ellipse4,siber.data,
#                                    p.interval = 0.95, n = 100)
# 
# ellipse95.overlap[3]/ellipse95.overlap[1]
# 
# #2023
# ellipse95.overlap <- maxLikOverlap(ellipse5, ellipse6,siber.data,
#                                    p.interval = 0.95, n = 100)
# 
# ellipse95.overlap[3]/ellipse95.overlap[1]

#### run bayesian modeling of diet overlap ####
# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 3        # run this many chains

parms$save.output = TRUE
parms$save.dir = "../ProcessedData"

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber.data, parms, priors)

# Calculate overlap of bayesian modeling for 100 samples

bayes95.overlap <- bayesianOverlap(ellipse1, ellipse2, ellipses.posterior,
                                   draws = 5700, p.interval = 0.95, n = 100)

bayes.prop.95.over <- (bayes95.overlap[,3] / (bayes95.overlap[,1]))

# histogram of proportion overlapping
hist(bayes.prop.95.over, 10)
quantile(bayes.prop.95.over, probs = c(0.025, 0.975))
mean(bayes.prop.95.over)

# #2021
# bayes95.overlap.2021 <- bayesianOverlap(ellipse1, ellipse2, ellipses.posterior,
#                                    draws = 5700, p.interval = 0.95, n = 100)
# 
# bayes.prop.95.over.2021 <- (bayes95.overlap.2021[,3] / (bayes95.overlap.2021[,1]))
# 
# # histogram of proportion overlapping
# hist(bayes.prop.95.over.2021, 10)
# quantile(bayes.prop.95.over.2021, probs = c(0.025, 0.975))
# mean(bayes.prop.95.over.2021)
# 
# #2022
# bayes95.overlap.2022 <- bayesianOverlap(ellipse3, ellipse4, ellipses.posterior,
#                                         draws = 5700, p.interval = 0.95, n = 100)
# 
# bayes.prop.95.over.2022 <- (bayes95.overlap.2022[,3] / (bayes95.overlap.2022[,1]))
# 
# # histogram of proportion overlapping
# hist(bayes.prop.95.over.2022, 10)
# quantile(bayes.prop.95.over.2022, probs = c(0.025, 0.975))
# mean(bayes.prop.95.over.2022)
# 
# #2023
# bayes95.overlap.2023 <- bayesianOverlap(ellipse5, ellipse6, ellipses.posterior,
#                                         draws = 5700, p.interval = 0.95, n = 100)
# bayes.prop.95.over.2023 <- (bayes95.overlap.2023[,3] / (bayes95.overlap.2023[,1]))
# 
# # histogram of proportion overlapping
# hist(bayes.prop.95.over.2023, 10)
# quantile(bayes.prop.95.over.2023, probs = c(0.025, 0.975))
# mean(bayes.prop.95.over.2023)


# plot historgram of overlaps
hist(bayes95.overlap[,3],10)

# calcualte proportion of deer overlapped by pig
bayes.prop.95.over <- (bayes95.overlap[,3] / (bayes95.overlap[,1]))

# histogram of proportion overlapping
hist(bayes.prop.95.over, 10)
summary(bayes.prop.95.over)

quantile(bayes.prop.95.over, probs = c(0.025, 0.975))
mean(bayes.prop.95.over)

# check diagnostics
all.files = dir(parms$save.dir, full.names = TRUE)

model.files = all.files[grep("jags_output",all.files)]
do.this <- 2
load(model.files[do.this])
gelman.diag(output,multivariate = FALSE)
gelman.plot(output, auto.layout = FALSE)

