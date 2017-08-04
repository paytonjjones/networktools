## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(networktools)
require(qgraph)

## ----social impact 0, echo=FALSE, fig.width = 4, fig.height = 4----------
example <- IsingFit::IsingFit(social[,1:5], plot=FALSE, progressbar=FALSE)
par(pin=c(1,1))
plot(example, layout="circle")

## ----initial social------------------------------------------------------
require(IsingFit)
socialq <- IsingFit(social, plot=FALSE, progressbar=FALSE)
plot(socialq, layout="circle")

## ----social impact 1, echo=FALSE-----------------------------------------
impsocial<-impact(social, binary.data=TRUE)
plot(impsocial$Edge, nodes="Kim", title=c("Kim participated", "Kim didn't particpate"))

## ----social impact 2-----------------------------------------------------
kim_global <- global.impact(input=social, nodes="Kim", binary.data=TRUE)
kim_global$impact

## ---- echo=FALSE---------------------------------------------------------
plot(impsocial$Edge, nodes="Rod", title=c("Rod absent", "Rod present"))

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
require(qgraph)
associationnet <- cor(depression)
qgraph(associationnet)
names(depression)

## ------------------------------------------------------------------------
impact_depression <- impact(depression)
plot(impact_depression)

## ------------------------------------------------------------------------
set.seed(1)
NCT_depression <- impact.NCT(depression, it=25, 
                             nodes=c("psychomotor_retardation", "sleep_disturbance"), 
                             progressbar=FALSE, test.edges=TRUE, edges=list(c(5,6)))

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
plot(impact_depression$Global.Strength, order="value", abs_val=TRUE)

## ------------------------------------------------------------------------
plot(impact_depression$Edge, nodes="sleep_disturbance", type="contrast")

## ------------------------------------------------------------------------

plot(impact_depression$Edge, nodes="sleep_disturbance", type="single", 
     title="Single impact graph: Edge impact visualized as edges")

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
qgraph(impact_depression$Edge$hi$psychomotor_retardation, 
       title="High Psychomotor Retardation", layout="spring", color="lightblue")
qgraph(impact_depression$Edge$lo$psychomotor_retardation, 
       title="Low Psychomotor Retardation", layout="spring", color="lightgreen")

## ------------------------------------------------------------------------
#: Sample size difference after split is >10% of total sample

