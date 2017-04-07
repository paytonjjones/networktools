## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
setwd("~/R/networktools package/networktools/vignettes")
devtools::document()
require(qgraph)

## ----initial social------------------------------------------------------
socialq <- IsingFit::IsingFit(social, plot=FALSE, progressbar=FALSE)
plot(socialq, layout="circle")

## ----social impact 1, echo=FALSE-----------------------------------------
impsocial<-impact(social, binary.data=TRUE)
plot(impsocial$Edge, nodes="Kim", title=c("Kim absent", "Kim present"))

## ----social impact 2-----------------------------------------------------
kim_global <- global.impact(social, nodes="Kim", binary.data=TRUE)
kim_global$impact

## ------------------------------------------------------------------------
plot(impsocial$Edge, nodes="Rod")

## ------------------------------------------------------------------------
rod_structure <- structure.impact(social, nodes="Rod", binary.data=TRUE)
rod_structure$impact

## ------------------------------------------------------------------------
rod_edge <- edge.impact(social, nodes="Rod", binary.data=TRUE)
rod_edge$impact$Rod["Pat","Pam"]

## ------------------------------------------------------------------------
social_impact <- impact(social, binary.data=TRUE)

## ------------------------------------------------------------------------
plot(social_impact)

## ------------------------------------------------------------------------
names(depression)
qgraph(cor(depression))

## ------------------------------------------------------------------------
impact_depression <- impact(depression)
plot(impact_depression)

## ------------------------------------------------------------------------
NCT_depression <- impact.NCT(depression, it=25, nodes=c("psychomotor_retardation", "sleep_disturbance"), progressbar=FALSE, test.edges=TRUE, edges=list(c(5,6)))

## ------------------------------------------------------------------------
#Global strength impact of psychomotor retardation
NCT_depression$psychomotor_retardation$glstrinv.pval
#Network structure impact of psychomotor retardation
NCT_depression$psychomotor_retardation$nwinv.pval
#Global strength impact of concentration problems
NCT_depression$sleep_disturbance$glstrinv.pval
#Network structure impact of psychomotor retardation
NCT_depression$sleep_disturbance$nwinv.pval
#Edge impact of concentration problems on fatigue--worthlessness
NCT_depression$sleep_disturbance$einv.pvals

## ----eval=FALSE----------------------------------------------------------
#  ?plot.all.impact
#  ?plot.global.impact
#  ?plot.structure.impact
#  ?plot.edge.impact

## ------------------------------------------------------------------------
plot(impact_depression, order="value", zscores=TRUE)


## ------------------------------------------------------------------------
plot(impact_depression$Edge, nodes="sleep_disturbance", type="contrast")

## ------------------------------------------------------------------------

plot(impact_depression$Edge, nodes="sleep_disturbance", type="single", title="Single impact graph: Edge impact visualized as edges")

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
qgraph(impact_depression$Edge$hi$psychomotor_retardation, title="High Psychomotor Retardation", layout="spring", color="lightblue")
qgraph(impact_depression$Edge$lo$psychomotor_retardation, title="Low Psychomotor Retardation", layout="spring", color="lightgreen")

## ------------------------------------------------------------------------
#: Sample size difference after split is >10% of total sample}

