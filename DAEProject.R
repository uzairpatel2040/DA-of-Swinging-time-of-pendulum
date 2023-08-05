Time= matrix(c(6.92,7.07,8.31,8.44,7.76,7.59,9.28,9.06,
                     8.73,8.81,10.11,9.95,9.32,9.51,11.22,11.18,6.88,
                     6.66,8.41,8.35,7.85,7.98,9.42,9.63,8.63,8.71,
                     10.08,10.14,9.23,9.1,11.05,11.17),byrow=T,ncol=2)
dimnames(Time) = list(c("(1)","a","b","ab","c","ac","bc","abc",
                              "d","ad", "bd","abd","cd","acd","bcd","abcd"),
                            c("time_1","time_2"))
total = apply(Time,1,sum)
Time_average = matrix(c(total))
dimnames(Time_average) = list(c("(1)","a","b","ab","c","ac","bc","abc",
                                "d","ad", "bd","abd","cd","acd","bcd","abcd"),
                              c("totaltime__"))
Time_average
A = rep(c(-1,1),8)
B =rep(c(-1,-1,1,1),4)
C= c(rep(-1,4),rep(1,4),rep(-1,4),rep(1,4))
D=c(rep(-1,8),rep(1,8))

cbind(A,B,C,D,Time,total)
data.time=data.frame(A,B,C,D,Time,total)##The given data
data.time
cbind(A,B,C,D,Time_average)
data.time_average=data.frame(A,B,C,D,Time_average)##The given data
data.time_average
######################################
I=c(rep(1,16))
AB = A*B
AC = A*C
BC = B*C
ABC = A*B*C
AD=A*D
BD=B*D
ABD=A*B*D
CD=C*D
ACD=A*C*D
BCD=B*C*D
ABCD=A*B*C*D
Design.matrix=cbind(I, A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,
                    CD,ACD,BCD,ABCD,Time_average)
Design.matrix
################## Interaction and Main effects  ########################
n = 2 ##Replication
Feff = t(Time_average) %*% cbind(A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD)/(8*n)
Ieff=t(Time_average) %*% cbind(I)/(16*n)
eff=cbind(Ieff,Feff)
eff
Summary = rbind( cbind(I,A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD),eff)
dimnames(Summary)[[1]] = c(dimnames(Time_average)[[1]],"Effect")
Summary
#################
library(unrepx)
G=Design.matrix[,17]
pilotEff = yates(G, labels = c("A","B","C", "D")) 
pilotEff
hnplot(pilotEff,ID=0)
################################
lm.rate=lm(Time_average ~ A*B*C, data=data.time_average)
summary(lm.rate)
library(DoE.base)
library(FrF2)
MEPlot(lm.rate)
IAPlot(lm.rate)
#################################  Response Surface #######################
##########################  REgression ###########################
###########################################################################
mod=lm(Time_average ~ A+C+B, data=data.time_average)
summary(mod)
residuals=mod$res
residuals
qqnorm(mod$res,ylab="Ordinary Residuals")
qqline(mod$res)
##  PRESS ############################################
x=model.matrix(mod)
PRESS_res=summary(mod)$res/(1-hat(x))
print(PRESS_res)
##############  Plotting of PRESS Residuals #########################
par(mfrow=c(1,1))
plot(PRESS_res,ylab="PRESS residual")
PRESS=sum(PRESS_res^2)
################  R^2 Prediction #######################
PRESS
SS_T= sum(anova(mod)$"Sum Sq")
pred.r.squared = 1 - PRESS/(SS_T)
pred.r.squared
###########################################################
##########################  Design Projection #############################
#################################################################################
### Reorganize the data as give replication wise ##############################
##################################################################################
time__= matrix(c(6.92,7.07,6.88,6.66,8.31,8.44,8.41,8.35,7.76,7.59,7.85,7.98,9.28,9.06,9.42,9.63,8.73,8.81,8.63,8.71,10.11,9.95,10.08,10.14,9.32,9.51,9.23,9.10,11.22,11.18,11.05,11.17),byrow=T,ncol=4)
dimnames(time__) = list(c("(1)","a","c","ac","d","ad","cd","acd"),
                     c("Rep1","Rep2","Rep3","Rep4"))
time__
A= rep(c(-1,1),4)
B =rep(c(-1,-1,1,1),2)
C= c(rep(-1,4),rep(1,4))
Total = apply(time__,1,sum)
data.time2=data.frame(A,B,C,Total)##The given data
data.time2
######################################
I=c(rep(1,8))
AC = A*C
AD=A*D
CD=C*D
ACD=A*C*D
Design.matrix2=cbind(I, A,C, AC,D,AD,CD,ACD,Total)
Design.matrix2
########################  ANOVA Model ##############
Frate= c(t(time__))
Af= rep(as.factor(A),rep(4,8))
Af
Cf= rep(as.factor(C),rep(4,8))
Bf= rep(as.factor(B),rep(4,8))
data.mat=data.frame(Af,Cf,Df, Frate)
data.mat
time__.av=aov(Frate ~ Af*Bf*Cf, data=data.mat)
summary(time__.av)

library(unrepx)
G=Design.matrix2[,9]
pilotEff2 = yates(G, labels = c("A","B","C")) 
pilotEff2
hnplot(pilotEff2,ID=0)

