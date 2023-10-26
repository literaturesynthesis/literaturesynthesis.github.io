# formation méta-analyse
# Auteur : Beillouin Damien
# Cours : Statistiques Forest plot
# Objectif du TD : Explorer et visualiser les données à l'aide de R et de forest plot.

# On charge les packages généraux
library(tidyverse) # needed for 'glimpse'
library(dmetar)

#--------------------------------------------------------------
###  Exemple 1: With metafor
#--------------------------------------------------------------

# On va se baser sur les données de Curtis et al. 
# install.packages('metafor')
library(metafor)
library(metadat)
dat <- metadat::dat.curtis1998
str(dat)

## SMD
SMD <- escalc(measure = "SMD", n1i = dat$n1i, n2i = dat$n2i, m1i = dat$m1i, 
              m2i = dat$m2i, sd1i = dat$sd1i, sd2i = dat$sd2i)
head(SMD)

## ln(RR)
lnRR <- escalc(measure = "ROM", n1i = dat$n1i, n2i = dat$n2i, m1i = dat$m1i, 
               m2 = dat$m2i, sd1i = dat$sd1i, sd2i = dat$sd2i)
head(lnRR)

dat <- cbind(dat, data.frame(lnRR))
# we should see yi (effec size) and vi (sampling variance) are added
str(dat)

forest(dat$yi, dat$vi)

# zoom sur les 12 premières études
forest(dat$yi[1:12], dat$vi[1:12])

### running a fixed effect-model 
common_m <- rma(yi = yi, vi = vi, method = "FE", data = dat)
summary(common_m)
forest(common_m)

## running a mixed effect model
random_m <- rma(yi = yi, vi = vi, method = "REML", data = dat)
summary(random_m)
forest(random_m)

## Meta-régression :
metareg <- rma(yi = yi, vi = vi, mod = ~time + method + fungrp, method = "REML", 
               data = dat)
summary(metareg)
forest(metareg)

### Check for publication biais

funnel(random_m)
regtest(random_m)

# Run trim-and-fill method
tf_m <- trimfill(random_m)
funnel(tf_m)

## run multi-level MA

multilevel_m <- rma.mv(yi = yi, V = vi, random = list(~1 | paper, ~1 | id), 
                       method = "REML", data = dat)
summary(multilevel_m)

#--------------------------------------------------------------
#### GRAPHIQUE 2:  Other example avec le package metafor
#--------------------------------------------------------------

library(metafor)
### create the dataset 
#from Goldenberg et al., 2021; http://dx.doi.org/10.1136/bmj.m4743
#Efficacy and safety of low and very low carbohydrate diets for type 2 
#diabetes remission: systematic review and meta-analysis of published and 
#unpublished randomized trial data

dat <- data.frame(author = c("Dyson", "Jönsson", "Morris", "Saslow", "Saslow", "Sato", "Tay", "Yamada"),
                  year   = c(2010, 2009, 2019, 2014, 2017, 2017, 2014, 2014),
                  ai     = c(3, 6, 11, 8, 6, 4, 36, 2), ##Nb event experimental group
                  n1i    = c(6, 6, 21, 9, 11, 22, 46, 12), ## Total experimental group
                  ci     = c(1, 3, 0, 5, 0, 0, 30, 2),  ## Nb events control group
                  n2i    = c(6, 6, 12, 13, 8, 27, 47, 12))  ## Total control group 

### calculate risk differences and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RD", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat,
              slab=paste(" ", author, year), addyi=FALSE)
dat

### fit random-effects model (using the DL estimator)
res <- rma(yi, vi, data=dat, method="DL")
res

### colors to be used in the plot
colp <- "#6b58a6"
coll <- "#a7a9ac"

### total number of studies
k <- nrow(dat)

### generate point sizes
psize <- weights(res)
psize <- 1.2 + (psize - min(psize)) / (max(psize) - min(psize))

### get the weights and format them as will be used in the forest plot
weights <- round(weights(res), 1)

### adjust the margins
par(mar=c(2.7,3.2,2.3,1.3), mgp=c(3,0,0), tcl=0.15)

### forest plot with extra annotations
sav <- forest(dat$yi, dat$vi, xlim=c(-3.4,2.1), ylim=c(-0.5,k+3), alim=c(-1,1), cex=0.88,
              pch=18, psize=psize, efac=0, refline=NA, lty=c(1,0), xlab="",
              ilab=cbind(paste(dat$ai, "/", dat$n1i), paste(dat$ci, "/", dat$n2i), weights),
              ilab.xpos=c(-1.9,-1.3,1.2), annosym=c(" (", " to ", ")"),
              rowadj=-.07)

### add the vertical reference line at 0
segments(0, -1, 0, k+1.6, col=coll)

### add the vertical reference line at the pooled estimate
segments(coef(res), 0, coef(res), k, col=colp, lty="33", lwd=0.8)

### redraw the CI lines and points in the chosen color
segments(summary(dat)$ci.lb, k:1, summary(dat)$ci.ub, k:1, col=colp, lwd=1.5)
points(dat$yi, k:1, pch=18, cex=psize*1.15, col="white")
points(dat$yi, k:1, pch=18, cex=psize, col=colp)

### add the summary polygon
addpoly(res, row=0, mlab="Total (95% CI)", efac=2, col=colp, border=colp)

### add the horizontal line at the top
abline(h=k+1.6, col=coll)

### redraw the x-axis in the chosen color
axis(side=1, at=seq(-1,1,by=0.5), col=coll, labels=FALSE)

### now we add a bunch of text; since some of the text falls outside of the
### plot region, we set xpd=NA so nothing gets clipped
par(xpd=NA)

### adjust cex as used in the forest plot and use a bold font
par(cex=sav$cex, font=2)

### add headings
text(sav$xlim[1], k+2.5, pos=4, "Study or\nsubgroup")
text(mean(sav$ilab.xpos[1:2]), k+3.4, "No of events / total")
text(0, k+2.7, "Risk difference, IV,\nrandom (95% CI)")
text(c(sav$ilab.xpos[3],sav$xlim[2]-0.35), k+2.7, c("Weight\n(%)","Risk difference, IV,\nrandom (95% CI)"))

### add 'Favours control'/'Favours experimental' text below the x-axis
text(c(-1,1), -2.5, c("Favors control","Favors experimental"), pos=c(4,2), offset=-0.3)

### use a non-bold font for the rest of the text
par(cex=sav$cex, font=1)

### add text with heterogeneity statistics
text(sav$xlim[1], -1, pos=4, bquote(paste("Test for heterogeneity: ",
                                          tau^2, "=", .(round(res$tau2, digits=2)), "; ",
                                          chi^2, "=", .(round(res$QE, digits=2)),
                                          ", df=", .(res$k - res$p), ", ",
                                         # .(fmtp(res$QEp, digits=2, pname="P", add0=TRUE, equal=TRUE)), "; ",
                                          I^2, "=", .(round(res$I2)), "%")))

### add text for test of overall effect
text(sav$xlim[1], -2, pos=4, bquote(paste("Test for overall effect: ")))



#--------------------------------------------------------------
#### GRAPHIQUE 1:  Avec le package meta
#--------------------------------------------------------------

library(meta)

# on charge les données:
data(ThirdWave)
glimpse(ThirdWave)

# On fait la méta-analyse
m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")

# on fait le forest plot.
forest.meta(m.gen, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))


#--------------------------------------------------------------
#### GRAPHIQUE 3:  Pour faire des orchards plot.
#--------------------------------------------------------------

devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)
data(eklof)
eklof<-metafor::escalc(measure="ROM", n1i=N_control, sd1i=SD_control,
                       m1i=mean_control, n2i=N_treatment, sd2i=SD_treatment, m2i=mean_treatment,
                       data=eklof)
# Add the unit level predictor
eklof$Datapoint<-as.factor(seq(1, dim(eklof)[1], 1))
# fit a MLMR - accounting for some non-independence
eklof_MR<-metafor::rma.mv(yi=yi, V=vi, mods=~ Grazer.type-1,
                          random=list(~1|ExptID, ~1|Datapoint), data=eklof)
results <- mod_results(eklof_MR, mod = "Grazer.type", group = "ExptID")
orchard_plot(results, mod = "Grazer.type",
             group = "ExptID", xlab = "log(Response ratio) (lnRR)")
# or
orchard_plot(eklof_MR, mod = "Grazer.type", group = "ExptID",
             xlab = "log(Response ratio) (lnRR)")

#--------------------------------------------------------------
#### GRAPHIQUE 4:  Pour faire une reactable des résultats
#--------------------------------------------------------------


# Choisir le fichier CSV nommé TAB_TD_reactable.csv dans votre ordinateur.
csv_file <- file.choose()

# Chargement des données à partir du fichier CSV choisi 
DATA <- read.csv(csv_file)


GROUP <- dplyr::group_by(DATA, `Land_use`) %>%
  dplyr::summarize(Number = dplyr::n())

reactable(
  GROUP,
  details = function(index) {
    sales <- filter(DATA, `Land_use` == GROUP$`Land_use`[index]) %>% select(-`Land_use`)
    tbl <- reactable(sales,  groupBy = "Sub_Cat_intervention",outlined = TRUE, highlight = TRUE, fullWidth = TRUE,
                     columns = list(
                       estimate = colDef(
                         cell = function(value) {
                           if (value >= 0) paste0("+", value) else value
                         },
                         style = function(value) {
                           color <- if (value > 0) {
                             "#008000"
                           } else if (value < 0) {
                             "#e00000"
                           }
                           list(fontWeight = 600, color = color)
                         }
                       )
                     ),
                     rowStyle = function(index) {
                       if (is.na(sales[index, "details"])) {
                         list(background = "rgba(0, 0, 0, 0.05)")
                       }
                     }
    )
    htmltools::div(style = list(margin = "12px 45px"), tbl)
  },
  onClick = "expand",
  rowStyle = list(cursor = "pointer")
)

#--------------------------------------------------------------
#### GRAPHIQUE 5:  Forest avec ggplot
#--------------------------------------------------------------

# Choisir le fichier CSV nommé TAB2_TD_forestplot.csv dans votre ordinateur.
csv_file <- file.choose()
# Chargement des données à partir du fichier CSV choisi 
TAB2 <- read.csv(csv_file)

# Choisir le fichier CSV nommé TAB1_TD_forestplot.csv dans votre ordinateur.
csv_file <- file.choose()

# Chargement des données à partir du fichier CSV choisi 
INd_effects <- read.csv(csv_file)

# Crée un graphique ggplot
p <- ggplot2::ggplot(TAB2, aes(estimate, reorder(Sub_Cat_intervention, estimate))) +
  # Ajoute des étoiles avec certaines caractéristiques esthétiques
  ggstar::geom_star(starshape = 12, size = 4, starstroke = 1.1) +
  # Ajoute des points en utilisant des données de INd_effects
  ggplot2::geom_point(
    data = INd_effects, 
    aes(group = Sub_Cat_intervention, size = as.numeric(N_paired_data)),
    shape = 21, alpha = 0.8, fill = "#ff9d02",
    position = position_dodge(width = 0.6)
  ) +
  # Ajoute des barres d'erreur en utilisant conf.low et conf.high
  ggplot2::geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high), color = "black",
    size = 0.2, width = 0.01
  ) +
  # Ajoute une ligne verticale à l'axe x à l'endroit où xintercept est égal à 0
  ggplot2::geom_vline(aes(xintercept = 0), linetype = 2, color = "gray") +
  # Applique un thème de ggpubr pour le graphique
  ggpubr::theme_pubr() +
  # Définit les limites de l'axe x
  xlim(c(-10, 140)) +
  # Supprime la légende du graphique
  theme(legend.position = "none") +
  # Définit les étiquettes des axes y et x
  labs(y = "", x = "SOC change (percent)")
# Affiche le graphique
p





