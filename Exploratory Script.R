#====================Regression Models Course Project===========================

#===========================Context============================================

# You work for Motor Trend, a magazine about the automobile industry. Looking at
#   a data set of a collection of cars, they are interested in exploring the 
#   relationship between a set of variables and miles per gallon (MPG) (outcome).
#   They are particularly interested in the following two questions:
    
#   1. "Is an automatic or manual transmission better for MPG"
#   2. "Quantify the MPG difference between automatic and manual transmissions"

#===============================Question========================================

# Take the mtcars data set and write up an analysis to answer their question 
#   using regression models and exploratory data analyses.

# Your report must be:
    
#   1. Written as a PDF printout of a compiled (using knitr) R markdown document.
#   2. Brief. Roughly the equivalent of 2 pages or less for the main text. 
#       Supporting figures in an appendix can be included up to 5 total pages 
#       including the 2 for the main report. The appendix can only include figures.
#   3. Include a first paragraph executive summary.

#==================================Rubric Checklist=============================

#    y   Interpret coefficients correctly
#    y   Some exploratory data analyses
#    y   Fit multiple models and detail strategy for model selection
#    y  Answer questions of interest
#    y   Residual plot and some diagnostics
#    y   quantify uncertainty of conclusions and/or perform an inference correctly
#    y   Report about 2 pgs (5 with supporting appendix)
#    y   Include executive summary
#    y   Done in knitr

#========================Load and Pre-Process Data===============================

    data(mtcars)
# Replace automatic/manual factor with descriptive terms
    mtcars$am[which(mtcars$am==0)]<-"automatic"
    mtcars$am[which(mtcars$am==1)]<-"manual"
    mtcars$am<-as.factor(mtcars$am)

# Replace "vs" factor with descriptive terms
    mtcars$vs[which(mtcars$vs==0)]<-"V-type-engine"
    mtcars$vs[which(mtcars$vs==1)]<-"Straight-engine"
    mtcars$vs<-as.factor(mtcars$vs)

# Convert cylinders, # of gears, and # of carburators to factors
    mtcars$cyl<-as.factor(mtcars$cyl)
    mtcars$gear<-as.factor(mtcars$gear)
    mtcars$carb<-as.factor(mtcars$carb)

#=============================Exploratory Analysis==============================

    dim(mtcars)
    summary(mtcars)

# Compare mpg by tranmition
    boxplot(mpg~am, data=mtcars)

    library(lattice)
    bwplot(mpg~am, data=mtcars)
    bwplot(mpg~am|hp, data=mtcars)

# Compare mpg to hp
    plot(mpg~hp, data=mtcars)
    abline(lm(mpg~hp, data=mtcars))
    cor(mtcars$mpg, mtcars$hp)

    summary(lm(mpg~hp, data=mtcars))

# Compare mpg to weight
    plot(mpg~wt, data=mtcars)
    abline(lm(mpg~wt, data=mtcars))
    cor(mtcars$mpg, mtcars$wt)

    summary(lm(mpg~wt, data=mtcars))

# Compare all variables
    pairs(mtcars)

# Find the correlations of all variables with mpg
    for (i in 2:11) {
        print(paste0(names(mtcars)[i], ":", cor(mtcars$mpg, as.numeric(mtcars[,i]))))
    }


    boxplot(mpg~vs, data=mtcars)

    plot(mpg~disp, data=mtcars)
    plot(disp~hp, data=mtcars)

    bwplot(mpg~am|vs, data=mtcars)
    bwplot(mpg~am|cyl, data=mtcars)
    bwplot(mpg~am|cyl+vs, data=mtcars)
    bwplot(mpg~am|gear, data=mtcars)
    bwplot(mpg~am|carb, data=mtcars)  # Appears to be the same relationship at all "common" levels

    bwplot(mpg~am|cyl+gear, data=mtcars)
    bwplot(mpg~am|cut(mtcars$hp, 4), data=mtcars)
    bwplot(mpg~am|cut(mtcars$wt, 4), data=mtcars)

    cut(mtcars$hp, 4)
#==============================Linear Models====================================

# Start with just mpg and trasmission
    lm.simple<-lm(mpg~am, data=mtcars)
    summary(lm.simple)
# Compare with model with all variables
    lm.all<-lm(mpg~., data=mtcars)
    summary(lm.all)

# Control for the variables that seem to have an effect
    lm.hp.wt<-lm(mpg~am+hp+wt, data=mtcars)
    summary(lm.hp.wt)

    par(mfrow=c(2,2))
    plot(lm.hp.wt)
    par(mfrow=c(1,1))


    lm.hp.wt.cyl.vs<-lm(mpg~am+hp+wt+cyl+vs, data=mtcars)
    summary(lm.hp.wt.cyl.vs)

    par(mfrow=c(2,2))
    plot(lm.hp.wt.cyl.vs)
    par(mfrow=c(1,1))

    lm.hp.cyl.vs<-lm(mpg~am+hp+cyl+vs, data=mtcars)
    summary(lm.hp.cyl.vs)
    
    par(mfrow=c(2,2))
    plot(lm.hp.cyl.vs)
    par(mfrow=c(1,1))

    lm.hp.wt.cyl<-lm(mpg~am+hp+wt+cyl, data=mtcars)
    summary(lm.hp.wt.cyl)
    
    par(mfrow=c(2,2))
    plot(lm.hp.wt.cyl)
    par(mfrow=c(1,1))

    lm.cyl<-lm(mpg~am+cyl, data=mtcars)
    summary(lm.cyl)
    
    par(mfrow=c(2,2))
    plot(lm.cyl)
    par(mfrow=c(1,1))

    lm.cyl.vs<-lm(mpg~am+cyl+vs, data=mtcars)
    summary(lm.cyl.vs)
    
    par(mfrow=c(2,2))
    plot(lm.cyl.vs)
    par(mfrow=c(1,1))

    lm.hp.cyl.vs<-lm(mpg~am+hp+cyl+vs, data=mtcars)
    summary(lm.hp.cyl.vs)
    
    par(mfrow=c(2,2))
    plot(lm.hp.cyl.vs)
    par(mfrow=c(1,1))

    lm.wt.cyl.vs<-lm(mpg~am+wt+cyl+vs, data=mtcars)
    summary(lm.wt.cyl.vs)

    confint(lm.hp.cyl.vs)[2, , drop=FALSE]
