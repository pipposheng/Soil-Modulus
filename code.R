library(alr3)          # Applied Linear Regression Package 3rd Edition

all=read.table("/alr3data/All.txt",sep="\t",header=T)     # Read table from file
# Divide Table into groups
all2=subset(all,Soil.Type=="A-2-4")         # A-2-4 Soils                   
all3=subset(all,Soil.Type=="A-3")           # A-3 Soils

# Analysis on A-3 Soils. A-2-4 Soils is similar
attach(all3)
scatterplotMatrix(~mr+fine+cc+cu+lbr+mc+duw,smoother=F)    # Scatterplot Matrix for Variables

# Linear Regression Models and Analysis of Variance
m0=lm(mr~fine+cc+cu+lbr+mc+duw)                     # First Linear Regression Model
invResPlot(m0)
summary(powerTransform(m0))                              # Check the required transformation
m0=lm(log(mr)~cc+cu+mc)
summary(m0)
anova(m0)
summary(powerTransform(cbind(cc,cu,mc)~1))      # Find the reqruied transformation for variables
cc1=cc^(-0.5)
cu1=log(cu)
mc1=mc^2
m1=lm(log(mr)~cc1+cu1+mc1)
summary(m1)
anova(m1)

# Compare the predicted values with the test values in figures.
par(mfrow=c(2,2))
plot(m1)
plot(predict(m1),log(mr),xlim=c(4.0,5.5),ylim=c(4.0,5.5),xlab="Predicted",ylab="Measured")
x=c(4,4.5,5,5.5)
# Acceptable range of results
y=x
y1=x+log(1.2)
y2=x+log(0.8)
lines(x,y,col="red",lwd=2)
lines(x,y2,col="blue",lwd=2)
lines(x,y1,col="blue",lwd=2)
text(x=5,y=5.3,label="+20%")
text(x=5.2,y=4.8,label="-20%")
