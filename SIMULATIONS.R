
############### ASSOCITED WERE (1, 4, 7, 10, 13, 16, 19) TRIADS
############### ASSOCITED WERE (2, 5, 8, 11, 14, 17, 20) TRIADS
############### ASSOCITED WERE (6, 9, 12, 15, 18, 21, 24) TRIADS

require(bindata)
library(haven)
library(xlsx)
library(copula)
library(arules)
library(ggplot2)
library(magick)
library(tidyverse)
library(dplyr)
library(gmodels)
library(epiDisplay)



##
target_max_POSTERIOR_OE <- 1.99  #used to optimise alpha
threshold=2

threshold1=1.5  #strong evidence/ affection
threshold2=2    #moderate evidence/ affection

point=2        #graphs
##



set.seed(1234)
N= 300000

p1= 0.35
p2= 0.33

p3=0.30
p4=0.25

p5=0.23
p6= 0.20

p7= 0.18
p8=0.16

p9=0.15
p10=0.14

p11=0.12
p12=0.10

p13=0.09
p14=0.08

p15=0.07
p16=0.07

p17=0.06
p18=0.06

p19=0.05
p20=0.05

p21=0.04
p22=0.04

p23=0.03
p24=0.03

p25=0.02  







rho <- matrix(0.001, nrow = 25, ncol = 25)
diag(rho) <- 1

rho[1, 4] = 0.35;  rho[4, 1] = 0.35
rho[1, 7] = 0.35;  rho[7, 1] = 0.35
rho[1, 10] = 0.30; rho[10, 1] = 0.30
rho[1, 13] = 0.30; rho[13, 1] = 0.30
rho[1, 16] = 0.25; rho[16, 1] = 0.25
rho[1, 19] = 0.20; rho[19, 1] = 0.20


rho[4, 7] = 0.20  ; rho[7, 4] = 0.20
rho[4, 10] = 0.22 ; rho[10, 4] = 0.22
rho[4, 13] = 0.20 ; rho[13, 4] = 0.20
rho[4, 16] = 0.18 ; rho[16, 4] = 0.18
rho[4, 19] =0.15  ; rho[19, 4] = 0.10

rho[7, 10] = 0.20 ; rho[10, 7] = 0.20
rho[7, 13] = 0.20 ; rho[13, 7] = 0.20
rho[7, 16] = 0.20 ; rho[16, 7] = 0.20
rho[7, 19] = 0.15 ; rho[19, 7] = 0.15

rho[10, 13] = 0.20 ; rho[13, 10] = 0.20 
rho[10, 16] = 0.15 ; rho[16, 10] = 0.15 
rho[10, 19] = 0.10 ; rho[19, 10] = 0.10 

rho[13, 16] = 0.10 ; rho[16, 13] = 0.10 
rho[13, 19] = 0.10 ; rho[19, 13] = 0.10 

rho[16, 19] = 0.10 ; rho[19, 16] = 0.10




rho[2, 5] = 0.35;  rho[5, 2] = 0.35
rho[2, 8] = 0.35;  rho[8, 2] = 0.35
rho[2, 11] = 0.30; rho[11, 2] = 0.30
rho[2, 14] = 0.30; rho[14, 2] = 0.30
rho[2, 17] = 0.25; rho[17, 2] = 0.25
rho[2, 20] = 0.20; rho[20, 2] = 0.20


rho[5, 8] = 0.30  ; rho[8, 5] = 0.30
rho[5, 11] = 0.25 ; rho[11, 5] = 0.25
rho[5, 14] = 0.18 ; rho[14, 5] = 0.18
rho[5, 17] = 0.15 ; rho[17, 5] = 0.15
rho[5, 20] =0.15  ; rho[20, 5] = 0.15

rho[8, 11] = 0.20 ; rho[11, 8] = 0.20
rho[8, 14] = 0.20 ; rho[14, 8] = 0.20
rho[8, 17] = 0.15 ; rho[17, 8] = 0.15
rho[8, 20] = 0.10 ; rho[20, 8] = 0.10

rho[11, 14] = 0.18 ; rho[14, 11] = 0.18
rho[11, 17] = 0.10 ; rho[17, 11] = 0.10 
rho[11, 20] = 0.10 ; rho[20, 11] = 0.10 

rho[14, 17] = 0.10 ; rho[17, 14] = 0.10 
rho[14, 20] = 0.08 ; rho[20, 14] = 0.08 

rho[17, 20] = 0.08 ; rho[20, 17] = 0.08



rho[6, 9] = 0.30;  rho[9, 6] = 0.30
rho[6, 12] = 0.30;  rho[12, 6] = 0.30
rho[6, 15] = 0.20; rho[15, 6] = 0.20
rho[6, 18] = 0.15; rho[18, 6] = 0.15
rho[6, 21] = 0.10; rho[21, 6] = 0.10
rho[6, 24] = 0.10; rho[24, 6] = 0.10


rho[9, 12] = 0.20  ; rho[12, 9] = 0.20
rho[9, 15] = 0.15 ; rho[15, 9] = 0.15
rho[9, 18] = 0.12 ; rho[18, 9] = 0.12
rho[9, 21] = 0.10 ; rho[21, 9] = 0.10
rho[9, 24] =0.08  ; rho[24, 9] = 0.08

rho[12, 15] = 0.10 ; rho[15, 12] = 0.10
rho[12, 18] = 0.10 ; rho[18, 12] = 0.10
rho[12, 21] = 0.10 ; rho[21, 12] = 0.10
rho[12, 24] = 0.10 ; rho[24, 12] = 0.10

rho[15, 18] = 0.10 ; rho[18, 15] = 0.10 
rho[15, 21] = 0.05 ; rho[21, 15] = 0.05 
rho[15, 24] = 0.05 ; rho[24, 15] = 0.05 

rho[18,21] = 0.05 ; rho[21, 18] = 0.05 
rho[18, 24] = 0.05 ; rho[24, 18] = 0.05 

rho[21, 24] = 0.05 ; rho[24, 21] = 0.05


d1 <- rmvbin(N, margprob=c(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, 
                           p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
                           p21, p22, p23, p24, p25), bincorr=rho)

colnames(d1 ) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20",
                   "x21", "x22", "x23", "x24", "x25")
d1=data.frame(d1)
cor(d1)



num_sub=dim(d1)[1]  #sample size
num_cond=dim(d1)[2]  # number of variables

k<-combn(1:num_cond,3) #generate list of all unique triads of conditions (3 refers to the number of conditions)
n<-dim(k)[2] #number of unique triads


triads1<-matrix(0,n,12)

triads1[,1:3]<-t(k)

colnames(triads1)<-c("condition1","condition2","condition3","n123","E123","OE", "n12", "n13", "n23", "n1", "n2", "n3")

triads1<-as.data.frame(triads1)

for (r in 1:n){    
  x<-triads1[r,1]
  y<-triads1[r,2]
  z<-triads1[r,3]
  triads1[r,"n123"]<-sum(d1[,x]==1 & d1[,y]==1 & d1[,z]==1)
  nx<-sum(d1[,x]==1)/num_sub
  ny<-sum(d1[,y]==1)/num_sub
  nz<-sum(d1[,z]==1)/num_sub
  triads1[r,"E123"]<-nx*ny*nz*num_sub
  triads1[r,"n12"]<-sum(d1[,x]==1 & d1[,y]==1 )
  triads1[r,"n13"]<-sum(d1[,x]==1  & d1[,z]==1)
  triads1[r,"n23"]<-sum(d1[,y]==1 & d1[,z]==1)
  triads1[r,"n1"]<-sum(d1[,x]==1 )
  triads1[r,"n2"]<-sum(d1[,y]==1 )
  triads1[r,"n3"]<-sum(d1[,z]==1 )
}

triads1$OE<-triads1$n123/triads1$E123
triads1$OE_binary<-ifelse(triads1$OE>=2, 1, 0)


#calculate lifts 12 then 3, and so one. Take the maximum of three lifts
triads1$N=N

triads1$lift12=(triads1$N * triads1$n12) / (triads1$n1 * triads1$n2)
triads1$lift13=(triads1$N * triads1$n13) / (triads1$n1 * triads1$n3)
triads1$lift23=(triads1$N * triads1$n23) / (triads1$n2 * triads1$n3)
triads1=triads1 %>% rowwise() %>% mutate(max_lift=max(c(lift12, lift13, lift23)))
triads1=triads1 %>% rowwise() %>% mutate(min_lift=min(c(lift12, lift13, lift23)))



triads1$p_x1=sum(triads1$n1)/ triads1$N
triads1$p_x2=sum(triads1$n2)/ triads1$N
triads1$p_x3=sum(triads1$n3)/ triads1$N

triads1$Triad_prevalence=(triads1$n123/ triads1$N)*100


summary(triads1$Triad_prevalence)
hist(triads1$Triad_prevalence)


triads1$lift12.3=(triads1$N * triads1$n123) / (triads1$n12 * triads1$n3)
triads1$lift13.2=(triads1$N * triads1$n123) / (triads1$n13 * triads1$n2) 
triads1$lift23.1=(triads1$N * triads1$n123) / (triads1$n23 * triads1$n1) 
triads1=triads1 %>% rowwise() %>% mutate(max_lift_triad=max(c(lift12.3, lift13.2, lift23.1)))

triads1$OE_lift_ratio=triads1$OE/ triads1$max_lift_triad



percentile_10=quantile(triads1$E123[triads1$E123 <100], prob=c(0.10))
percentile_50=quantile(triads1$E123[triads1$E123 <100], prob=c(0.50))
percentile_90=quantile(triads1$E123[triads1$E123 <100], prob=c(0.90))


percentile_10
percentile_50
percentile_90


triads1$posterior_OE_10=(triads1$n123 + percentile_10) / (triads1$E123 + percentile_10)
triads1$posterior_OE_50=(triads1$n123 + percentile_50) / (triads1$E123 + percentile_50)
triads1$posterior_OE_90=(triads1$n123 + percentile_90) / (triads1$E123 + percentile_90)
triads1$posterior_OE_heurostic=(triads1$n123 + 0.5) / (triads1$E123 + 0.5)


triads1$association1 <- ifelse(
  (triads1$condition1 %in% c(1, 4, 7, 10, 13, 16, 19)) &
    (triads1$condition2 %in% c(1, 4, 7, 10, 13, 16, 19)) &
    (triads1$condition3 %in% c(1, 4, 7, 10, 13, 16, 19)),
  1, 0
)


triads1$association2 <- ifelse(
  (triads1$condition1 %in% c(2, 5, 8, 11, 14, 17, 20)) &
    (triads1$condition2 %in% c(2, 5, 8, 11, 14, 17, 20)) &
    (triads1$condition3 %in% c(2, 5, 8, 11, 14, 17, 20)),
  1, 0
)



triads1$association3 <- ifelse(
  (triads1$condition1 %in% c(6, 9, 12, 15, 18, 21, 24)) &
    (triads1$condition2 %in% c(6, 9, 12, 15, 18, 21, 24)) &
    (triads1$condition3 %in% c(6, 9, 12, 15, 18, 21, 24)),
  1, 0
)


triads1$association= ifelse(triads1$association1==1 | 
                              triads1$association2==1 | 
                              triads1$association3==1, 
                            1, 0)

triads1$association=as.factor(triads1$association)

triads1$scenario=1


CrossTable(triads1$OE_binary, triads1$association, chisq=TRUE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE)



#min and max of max_lift for real and false associations
summary_stats <- triads1 %>%
  group_by(association) %>%
  summarize(min_minlift = min(min_lift),
            max_minlift = max(min_lift))

print(summary_stats)



summary_stats <- triads1 %>%
  group_by(association) %>%
  summarize(min_maxlift = min(max_lift),
            max_maxlift = max(max_lift))

print(summary_stats)






CrossTable(triads1$OE_binary, triads1$association, chisq=TRUE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE)


write_sav(triads1,"S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/triads1.sav")



####find the optimum alpha so that all posterior OE ratios <2 when association=0
ALPHA=1
triads1_false=subset(triads1, triads1$association==0)
dim(triads1_false)


# Define a function to calculate POSTERIOR_OE
calculate_POSTERIOR_OE <- function(n123, E123, ALPHA) {
  (n123 + ALPHA) / (E123 + ALPHA)
}

# Initial calculation of POSTERIOR_OE
triads1_false$POSTERIOR_OE <- calculate_POSTERIOR_OE(triads1_false$n123, triads1_false$E123, ALPHA)



# Iteratively update ALPHA until POSTERIOR_OE max is less than the target
while (max(triads1_false$POSTERIOR_OE) >= target_max_POSTERIOR_OE) {
  ALPHA <- ALPHA + 1  # You can adjust the step size as needed
  triads1_false$POSTERIOR_OE <- calculate_POSTERIOR_OE(triads1_false$n123, triads1_false$E123, ALPHA)
}

# The updated ALPHA value
cat("Optimal ALPHA:", ALPHA, "\n")


triads1$ALPHA=ALPHA
triads1$POSTERIOR_OE=(triads1$n123 + triads1$ALPHA) / (triads1$E123 + triads1$ALPHA)

triads1%>% group_by(association)%>% summarise (min=min(POSTERIOR_OE), max=max(POSTERIOR_OE),na.rm=TRUE)




###correct filters

triads1$category=ifelse(triads1$association==0 & triads1$OE > threshold, 1, 
                        ifelse(triads1$association==0 & triads1$OE < threshold, 0, 2))  #2 TRUE, 1 FALSE & SPURIOUS, 0 FALSE & NON-SPURIOUS
tab1(triads1$category)

triads1$category=as.factor(triads1$category)
triads1$category=factor(triads1$category, levels=c(0, 1, 2),labels=c("False triads","Spurious triads", "True triads"))




triads1$filter_Bayes=ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_90 >= threshold2,  0, 
                            ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_90 >= threshold1 & triads1$posterior_OE_90 < threshold2, 1, 
                                   ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_90 <= threshold1, 2, NA)))
tab1(triads1$filter_Bayes)


triads1$filter_Bayes50=ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_50 >= threshold2,  0, 
                              ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_50 >= threshold1 & triads1$posterior_OE_50 < threshold2, 1, 
                                     ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_50 <= threshold1, 2, NA)))
tab1(triads1$filter_Bayes50)


triads1$filter_ARM=ifelse(triads1$category=="Spurious triads" & triads1$OE_lift_ratio >= threshold2,  0, 
                          ifelse(triads1$category=="Spurious triads" & triads1$OE_lift_ratio >= threshold1 & triads1$OE_lift_ratio < threshold2, 1, 
                                 ifelse(triads1$category=="Spurious triads" & triads1$OE_lift_ratio <= threshold1, 2, NA)))
tab1(triads1$filter_ARM)







###wrong filters

triads1$filter_Bayes_wrong=ifelse(triads1$category=="True triads" & triads1$posterior_OE_90 > threshold2,  0, 
                            ifelse(triads1$category=="True triads" & triads1$posterior_OE_90 >= threshold1 & triads1$posterior_OE_90 < threshold2, 1, 
                                   ifelse(triads1$category=="True triads" & triads1$posterior_OE_90 < threshold1, 2, NA)))
tab1(triads1$filter_Bayes_wrong)


triads1$filter_Bayes_wrong50=ifelse(triads1$category=="True triads" & triads1$posterior_OE_50 > threshold2,  0, 
                                  ifelse(triads1$category=="True triads" & triads1$posterior_OE_50 >= threshold1 & triads1$posterior_OE_50 < threshold2, 1, 
                                         ifelse(triads1$category=="True triads" & triads1$posterior_OE_50 < threshold1, 2, NA)))
tab1(triads1$filter_Bayes_wrong50)



triads1$filter_ARM_wrong=ifelse(triads1$category=="True triads" & triads1$OE_lift_ratio > threshold2,  0, 
                          ifelse(triads1$category=="True triads" & triads1$OE_lift_ratio >= threshold1 & triads1$OE_lift_ratio < threshold2, 1, 
                                 ifelse(triads1$category=="True triads" & triads1$OE_lift_ratio < threshold1, 2, NA)))
tab1(triads1$filter_ARM_wrong)





triads1$filter_Bayes_optimum_wrong=ifelse(triads1$category=="True triads" & triads1$POSTERIOR_OE > threshold2,  0, 
                                          ifelse(triads1$category=="True triads" & triads1$POSTERIOR_OE >= threshold1 & triads1$POSTERIOR_OE < threshold2, 1, 
                                                 ifelse(triads1$category=="True triads" & triads1$POSTERIOR_OE < threshold1, 2, NA)))
tab1(triads1$filter_Bayes_optimum_wrong)



## heuristic 0.5
triads1$filter_Bayes_0.5=ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_heurostic >= threshold2,  0, 
                                ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_heurostic >= threshold1 & triads1$posterior_OE_heurostic < threshold2, 1, 
                                       ifelse(triads1$category=="Spurious triads" & triads1$posterior_OE_heurostic <= threshold1, 2, NA)))
tab1(triads1$filter_Bayes_0.5)



triads1$filter_Bayes_0.5_wrong=ifelse(triads1$category=="True triads" & triads1$posterior_OE_heurostic > threshold2,  0, 
                                      ifelse(triads1$category=="True triads" & triads1$posterior_OE_heurostic >= threshold1 & triads1$posterior_OE_heurostic < threshold2, 1, 
                                             ifelse(triads1$category=="True triads" & triads1$posterior_OE_heurostic < threshold1, 2, NA)))
tab1(triads1$filter_Bayes_0.5_wrong)
#####





##estimate theta (i..e, alpha) under no constratint (change lower to 80 for threshold of 90th percentile)

triads_OE=triads1 %>% dplyr::select (condition1, condition2, condition3, n123, E123)
triads_OE=rename(triads_OE, O=n123, E=E123)
num_sub=300000

for (m in 1:n){
  O<-triads_OE$O[m]
  if (O>0){
    triads_OE$log_factorial_O[m]<-sum(log(1:O)) 
  }
  else {
    triads_OE$log_factorial_O[m]<-0
  }
}

#the likelihood function for Poisson-Gamma model
PG <- function (theta){
  O <- O
  E <- E
  log_factorial_O<-log_factorial_O
  a = theta 
  b = theta 
  LL <- a * log (b) +
    O * log (E) +
    lgamma ( O + a ) -
    lgamma ( a ) -
    log_factorial_O -
    ( O + a ) * log ( b + num_sub )
  
  return(-sum ( LL ))
}

O<-triads_OE$O
E<-triads_OE$E
log_factorial_O<-triads_OE$log_factorial_O

# set the initial value for parameters
theta <- 0.001


#R function to estimate parameter; lower shows the minimum value for estimated parameter
par.optim <- optim( par= 60 , PG , lower=0.001, upper=Inf, method="L-BFGS-B")

estimated_theta <- par.optim$par
cat("Estimated theta:", estimated_theta, "\n")

triads_OE$Posterior_OE <- ( par.optim$par [1] + O ) / ( par.optim$par [1] + E )
triads_OE



summary_stats <- aggregate(E123 ~ category, triads1, FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))
summary_stats





#Association Rule Mining (ARM) JUST TO CHECK MY CODES
library(arules)
attach(d1)

d1$x1=as.factor(d1$x1)
d1$x2=as.factor(d1$x2)
d1$x3=as.factor(d1$x3)
d1$x4=as.factor(d1$x4)
d1$x5=as.factor(d1$x5)
d1$x6=as.factor(d1$x6)
d1$x7=as.factor(d1$x7)
d1$x8=as.factor(d1$x8)
d1$x9=as.factor(d1$x9)
d1$x10=as.factor(d1$x10)

d1$x11=as.factor(d1$x11)
d1$x12=as.factor(d1$x12)
d1$x13=as.factor(d1$x13)
d1$x14=as.factor(d1$x14)
d1$x15=as.factor(d1$x15)
d1$x16=as.factor(d1$x16)
d1$x17=as.factor(d1$x17)
d1$x18=as.factor(d1$x18)
d1$x19=as.factor(d1$x19)
d1$x20=as.factor(d1$x20)

d1$x21=as.factor(d1$x21)
d1$x22=as.factor(d1$x22)
d1$x23=as.factor(d1$x23)
d1$x24=as.factor(d1$x24)
d1$x25=as.factor(d1$x25)



rules2=apriori(d1,
               parameter=list(supp=0.000001,conf=0.000001,minlen=3,maxlen=3),
               appearance=list(none=c(
                 "x1=0","x2=0", "x3=0", "x4=0", "x5=0", "x6=0", "x7=0", "x8=0", "x9=0", "x10=0",
                 "x11=0", "x12=0", "x13=0", "x14=0", "x15=0", "x16=0", "x17=0", "x18=0", "x19=0", "x20=0",
                 "x21=0", "x22=0", "x23=0", "x24=0", "x25=0")))

quality(rules2)=round(quality(rules2),digits=3)
summary(rules2)
inspect(rules2)
inspect(sort(rules2,by="lift") [1:20])  # summarise twenty rules with highest lift

write(rules2,file="P://rules.csv",sep=",")  #write all rules as a csv file
















###############N=10000
require(bindata)
library(haven)
library(xlsx)
library(copula)
library(arules)
library(ggplot2)
library(magick)
library(tidyverse)
library(dplyr)
library(gmodels)
library(epiDisplay)


set.seed(1234)
N= 10000

p1= 0.35
p2= 0.33

p3=0.30
p4=0.25

p5=0.23
p6= 0.20

p7= 0.18
p8=0.16

p9=0.15
p10=0.14

p11=0.12
p12=0.10

p13=0.09
p14=0.08

p15=0.07
p16=0.07

p17=0.06
p18=0.06

p19=0.05
p20=0.05

p21=0.04
p22=0.04

p23=0.03
p24=0.03

p25=0.02  







rho <- matrix(0.001, nrow = 25, ncol = 25)
diag(rho) <- 1

rho[1, 4] = 0.35;  rho[4, 1] = 0.35
rho[1, 7] = 0.35;  rho[7, 1] = 0.35
rho[1, 10] = 0.30; rho[10, 1] = 0.30
rho[1, 13] = 0.30; rho[13, 1] = 0.30
rho[1, 16] = 0.25; rho[16, 1] = 0.25
rho[1, 19] = 0.20; rho[19, 1] = 0.20


rho[4, 7] = 0.20  ; rho[7, 4] = 0.20
rho[4, 10] = 0.22 ; rho[10, 4] = 0.22
rho[4, 13] = 0.20 ; rho[13, 4] = 0.20
rho[4, 16] = 0.18 ; rho[16, 4] = 0.18
rho[4, 19] =0.15  ; rho[19, 4] = 0.10

rho[7, 10] = 0.20 ; rho[10, 7] = 0.20
rho[7, 13] = 0.20 ; rho[13, 7] = 0.20
rho[7, 16] = 0.20 ; rho[16, 7] = 0.20
rho[7, 19] = 0.15 ; rho[19, 7] = 0.15

rho[10, 13] = 0.20 ; rho[13, 10] = 0.20 
rho[10, 16] = 0.15 ; rho[16, 10] = 0.15 
rho[10, 19] = 0.10 ; rho[19, 10] = 0.10 

rho[13, 16] = 0.10 ; rho[16, 13] = 0.10 
rho[13, 19] = 0.10 ; rho[19, 13] = 0.10 

rho[16, 19] = 0.10 ; rho[19, 16] = 0.10




rho[2, 5] = 0.35;  rho[5, 2] = 0.35
rho[2, 8] = 0.35;  rho[8, 2] = 0.35
rho[2, 11] = 0.30; rho[11, 2] = 0.30
rho[2, 14] = 0.30; rho[14, 2] = 0.30
rho[2, 17] = 0.25; rho[17, 2] = 0.25
rho[2, 20] = 0.20; rho[20, 2] = 0.20


rho[5, 8] = 0.30  ; rho[8, 5] = 0.30
rho[5, 11] = 0.25 ; rho[11, 5] = 0.25
rho[5, 14] = 0.18 ; rho[14, 5] = 0.18
rho[5, 17] = 0.15 ; rho[17, 5] = 0.15
rho[5, 20] =0.15  ; rho[20, 5] = 0.15

rho[8, 11] = 0.20 ; rho[11, 8] = 0.20
rho[8, 14] = 0.20 ; rho[14, 8] = 0.20
rho[8, 17] = 0.15 ; rho[17, 8] = 0.15
rho[8, 20] = 0.10 ; rho[20, 8] = 0.10

rho[11, 14] = 0.18 ; rho[14, 11] = 0.18
rho[11, 17] = 0.10 ; rho[17, 11] = 0.10 
rho[11, 20] = 0.10 ; rho[20, 11] = 0.10 

rho[14, 17] = 0.10 ; rho[17, 14] = 0.10 
rho[14, 20] = 0.08 ; rho[20, 14] = 0.08 

rho[17, 20] = 0.08 ; rho[20, 17] = 0.08



rho[6, 9] = 0.30;  rho[9, 6] = 0.30
rho[6, 12] = 0.30;  rho[12, 6] = 0.30
rho[6, 15] = 0.20; rho[15, 6] = 0.20
rho[6, 18] = 0.15; rho[18, 6] = 0.15
rho[6, 21] = 0.10; rho[21, 6] = 0.10
rho[6, 24] = 0.10; rho[24, 6] = 0.10


rho[9, 12] = 0.20  ; rho[12, 9] = 0.20
rho[9, 15] = 0.15 ; rho[15, 9] = 0.15
rho[9, 18] = 0.12 ; rho[18, 9] = 0.12
rho[9, 21] = 0.10 ; rho[21, 9] = 0.10
rho[9, 24] =0.08  ; rho[24, 9] = 0.08

rho[12, 15] = 0.10 ; rho[15, 12] = 0.10
rho[12, 18] = 0.10 ; rho[18, 12] = 0.10
rho[12, 21] = 0.10 ; rho[21, 12] = 0.10
rho[12, 24] = 0.10 ; rho[24, 12] = 0.10

rho[15, 18] = 0.10 ; rho[18, 15] = 0.10 
rho[15, 21] = 0.05 ; rho[21, 15] = 0.05 
rho[15, 24] = 0.05 ; rho[24, 15] = 0.05 

rho[18,21] = 0.05 ; rho[21, 18] = 0.05 
rho[18, 24] = 0.05 ; rho[24, 18] = 0.05 

rho[21, 24] = 0.05 ; rho[24, 21] = 0.05


d2 <- rmvbin(N, margprob=c(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, 
                           p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
                           p21, p22, p23, p24, p25), bincorr=rho)

colnames(d2 ) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20",
                   "x21", "x22", "x23", "x24", "x25")
d2=data.frame(d2)
cor(d2)


num_sub=dim(d2)[1]  #sample size
num_cond=dim(d2)[2]  # number of variables

k<-combn(1:num_cond,3) #generate list of all unique triads of conditions (3 refers to the number of conditions)
n<-dim(k)[2] #number of unique triads


triads2<-matrix(0,n,12)

triads2[,1:3]<-t(k)

colnames(triads2)<-c("condition1","condition2","condition3","n123","E123","OE", "n12", "n13", "n23", "n1", "n2", "n3")

triads2<-as.data.frame(triads2)

for (r in 1:n){    
  x<-triads2[r,1]
  y<-triads2[r,2]
  z<-triads2[r,3]
  triads2[r,"n123"]<-sum(d2[,x]==1 & d2[,y]==1 & d2[,z]==1)
  nx<-sum(d2[,x]==1)/num_sub
  ny<-sum(d2[,y]==1)/num_sub
  nz<-sum(d2[,z]==1)/num_sub
  triads2[r,"E123"]<-nx*ny*nz*num_sub
  triads2[r,"n12"]<-sum(d2[,x]==1 & d2[,y]==1 )
  triads2[r,"n13"]<-sum(d2[,x]==1  & d2[,z]==1)
  triads2[r,"n23"]<-sum(d2[,y]==1 & d2[,z]==1)
  triads2[r,"n1"]<-sum(d2[,x]==1 )
  triads2[r,"n2"]<-sum(d2[,y]==1 )
  triads2[r,"n3"]<-sum(d2[,z]==1 )
}

triads2$OE<-triads2$n123/triads2$E123
triads2$OE_binary<-ifelse(triads2$OE>=2, 1, 0)


#calculate lifts 12 then 3, and so one. Take the maximum of three lifts
triads2$N=N

triads2$lift12=(triads2$N * triads2$n12) / (triads2$n1 * triads2$n2)
triads2$lift13=(triads2$N * triads2$n13) / (triads2$n1 * triads2$n3)
triads2$lift23=(triads2$N * triads2$n23) / (triads2$n2 * triads2$n3)
triads2=triads2 %>% rowwise() %>% mutate(max_lift=max(c(lift12, lift13, lift23)))
triads2=triads2 %>% rowwise() %>% mutate(min_lift=min(c(lift12, lift13, lift23)))



triads2$p_x1=sum(triads2$n1)/ triads2$N
triads2$p_x2=sum(triads2$n2)/ triads2$N
triads2$p_x3=sum(triads2$n3)/ triads2$N

triads2$Triad_prevalence=(triads2$n123/ triads2$N)*100
summary(triads2$Triad_prevalence)


triads2$lift12.3=(triads2$N * triads2$n123) / (triads2$n12 * triads2$n3)
triads2$lift13.2=(triads2$N * triads2$n123) / (triads2$n13 * triads2$n2) 
triads2$lift23.1=(triads2$N * triads2$n123) / (triads2$n23 * triads2$n1) 
triads2=triads2 %>% rowwise() %>% mutate(max_lift_triad=max(c(lift12.3, lift13.2, lift23.1)))

triads2$OE_lift_ratio=triads2$OE/ triads2$max_lift_triad



percentile_10=quantile(triads2$E123[triads2$E123 <100], prob=c(0.10))
percentile_50=quantile(triads2$E123[triads2$E123 <100], prob=c(0.50))
percentile_90=quantile(triads2$E123[triads2$E123 <100], prob=c(0.90))


percentile_10
percentile_50
percentile_90


triads2$posterior_OE_10=(triads2$n123 + percentile_10) / (triads2$E123 + percentile_10)
triads2$posterior_OE_50=(triads2$n123 + percentile_50) / (triads2$E123 + percentile_50)
triads2$posterior_OE_90=(triads2$n123 + percentile_90) / (triads2$E123 + percentile_90)
triads2$posterior_OE_heurostic=(triads2$n123 + 0.5) / (triads2$E123 + 0.5)


triads2$association1 <- ifelse(
  (triads2$condition1 %in% c(1, 4, 7, 10, 13, 16, 19)) &
    (triads2$condition2 %in% c(1, 4, 7, 10, 13, 16, 19)) &
    (triads2$condition3 %in% c(1, 4, 7, 10, 13, 16, 19)),
  1, 0
)


triads2$association2 <- ifelse(
  (triads2$condition1 %in% c(2, 5, 8, 11, 14, 17, 20)) &
    (triads2$condition2 %in% c(2, 5, 8, 11, 14, 17, 20)) &
    (triads2$condition3 %in% c(2, 5, 8, 11, 14, 17, 20)),
  1, 0
)



triads2$association3 <- ifelse(
  (triads2$condition1 %in% c(6, 9, 12, 15, 18, 21, 24)) &
    (triads2$condition2 %in% c(6, 9, 12, 15, 18, 21, 24)) &
    (triads2$condition3 %in% c(6, 9, 12, 15, 18, 21, 24)),
  1, 0
)


triads2$association= ifelse(triads2$association1==1 | 
                              triads2$association2==1 | 
                              triads2$association3==1, 
                            1, 0)

triads2$association=as.factor(triads2$association)

triads2$scenario=2


CrossTable(triads2$OE_binary, triads2$association, chisq=TRUE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE)



#min and max of max_lift for real and false associations
summary_stats <- triads2 %>%
  group_by(association) %>%
  summarize(min_minlift = min(min_lift),
            max_minlift = max(min_lift))

print(summary_stats)



summary_stats <- triads2 %>%
  group_by(association) %>%
  summarize(min_maxlift = min(max_lift),
            max_maxlift = max(max_lift))

print(summary_stats)





CrossTable(triads2$OE_binary, triads2$association, chisq=TRUE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE)


write_sav(triads2,"S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/triads2.sav")



####find the optimum alpha so that all posterior OE ratios <2 when association=0

triads2_false=subset(triads2, triads2$association==0)
dim(triads2_false)

ALPHA=1

# Define a function to calculate POSTERIOR_OE
calculate_POSTERIOR_OE <- function(n123, E123, ALPHA) {
  (n123 + ALPHA) / (E123 + ALPHA)
}

# Initial calculation of POSTERIOR_OE
triads2_false$POSTERIOR_OE <- calculate_POSTERIOR_OE(triads2_false$n123, triads2_false$E123, ALPHA)


# Iteratively update ALPHA until POSTERIOR_OE max is less than the target
while (max(triads2_false$POSTERIOR_OE) >= target_max_POSTERIOR_OE) {
  ALPHA <- ALPHA + 1  # You can adjust the step size as needed
  triads2_false$POSTERIOR_OE <- calculate_POSTERIOR_OE(triads2_false$n123, triads2_false$E123, ALPHA)
}

# The updated ALPHA value
cat("Optimal ALPHA:", ALPHA, "\n")


triads2$ALPHA=ALPHA
triads2$POSTERIOR_OE=(triads2$n123 + triads2$ALPHA) / (triads2$E123 + triads2$ALPHA)

triads2%>% group_by(association)%>% summarise (min=min(POSTERIOR_OE), max=max(POSTERIOR_OE),na.rm=TRUE)




###correct filters

triads2$category=ifelse(triads2$association==0 & triads2$OE > threshold, 1, 
                        ifelse(triads2$association==0 & triads2$OE < threshold, 0, 2))  #2 TRUE, 1 FALSE & SPURIOUS, 0 FALSE & NON-SPURIOUS
tab1(triads2$category)

triads2$category=as.factor(triads2$category)
triads2$category=factor(triads2$category, levels=c(0, 1, 2),labels=c("False triads","Spurious triads", "True triads"))




triads2$filter_Bayes=ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_90 >= threshold2,  0, 
                            ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_90 >= threshold1 & triads2$posterior_OE_90 < threshold2, 1, 
                                   ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_90 <= threshold1, 2, NA)))
tab1(triads2$filter_Bayes)


triads2$filter_Bayes50=ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_50 >= threshold2,  0, 
                            ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_50 >= threshold1 & triads2$posterior_OE_50 < threshold2, 1, 
                                   ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_50 <= threshold1, 2, NA)))
tab1(triads2$filter_Bayes50)



triads2$filter_ARM=ifelse(triads2$category=="Spurious triads" & triads2$OE_lift_ratio >= threshold2,  0, 
                          ifelse(triads2$category=="Spurious triads" & triads2$OE_lift_ratio >= threshold1 & triads2$OE_lift_ratio < threshold2, 1, 
                                 ifelse(triads2$category=="Spurious triads" & triads2$OE_lift_ratio <= threshold1, 2, NA)))
tab1(triads2$filter_ARM)



###wrong filters

triads2$filter_Bayes_wrong=ifelse(triads2$category=="True triads" & triads2$posterior_OE_90 > threshold2,  0, 
                                  ifelse(triads2$category=="True triads" & triads2$posterior_OE_90 >= threshold1 & triads2$posterior_OE_90 < threshold2, 1, 
                                         ifelse(triads2$category=="True triads" & triads2$posterior_OE_90 < threshold1, 2, NA)))
tab1(triads2$filter_Bayes_wrong)


triads2$filter_Bayes_wrong50=ifelse(triads2$category=="True triads" & triads2$posterior_OE_50 > threshold2,  0, 
                                  ifelse(triads2$category=="True triads" & triads2$posterior_OE_50 >= threshold1 & triads2$posterior_OE_50 < threshold2, 1, 
                                         ifelse(triads2$category=="True triads" & triads2$posterior_OE_50 < threshold1, 2, NA)))
tab1(triads2$filter_Bayes_wrong50)



triads2$filter_ARM_wrong=ifelse(triads2$category=="True triads" & triads2$OE_lift_ratio > threshold2,  0, 
                                ifelse(triads2$category=="True triads" & triads2$OE_lift_ratio >= threshold1 & triads2$OE_lift_ratio < threshold2, 1, 
                                       ifelse(triads2$category=="True triads" & triads2$OE_lift_ratio < threshold1, 2, NA)))
tab1(triads2$filter_ARM_wrong)





triads2$filter_Bayes_optimum_wrong=ifelse(triads2$category=="True triads" & triads2$POSTERIOR_OE > threshold2,  0, 
                                          ifelse(triads2$category=="True triads" & triads2$POSTERIOR_OE >= threshold1 & triads2$POSTERIOR_OE < threshold2, 1, 
                                                 ifelse(triads2$category=="True triads" & triads2$POSTERIOR_OE < threshold1, 2, NA)))
tab1(triads2$filter_Bayes_optimum_wrong)



## heuristic 0.5
triads2$filter_Bayes_0.5=ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_heurostic >= threshold2,  0, 
                                ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_heurostic >= threshold1 & triads2$posterior_OE_heurostic < threshold2, 1, 
                                       ifelse(triads2$category=="Spurious triads" & triads2$posterior_OE_heurostic <= threshold1, 2, NA)))
tab1(triads2$filter_Bayes_0.5)



triads2$filter_Bayes_0.5_wrong=ifelse(triads2$category=="True triads" & triads2$posterior_OE_heurostic > threshold2,  0, 
                                      ifelse(triads2$category=="True triads" & triads2$posterior_OE_heurostic >= threshold1 & triads2$posterior_OE_heurostic < threshold2, 1, 
                                             ifelse(triads2$category=="True triads" & triads2$posterior_OE_heurostic < threshold1, 2, NA)))
tab1(triads2$filter_Bayes_0.5_wrong)
#####





##estimate theta under no constratin (change min to 80 for threshold of 90th percentile)

triads_OE=triads2 %>% dplyr::select (condition1, condition2, condition3, n123, E123)
triads_OE=rename(triads_OE, O=n123, E=E123)
num_sub=10000

for (m in 1:n){
  O<-triads_OE$O[m]
  if (O>0){
    triads_OE$log_factorial_O[m]<-sum(log(1:O)) 
  }
  else {
    triads_OE$log_factorial_O[m]<-0
  }
}

#the likelihood function for Poisson-Gamma model
PG <- function (theta){
  O <- O
  E <- E
  log_factorial_O<-log_factorial_O
  a = theta 
  b = theta 
  LL <- a * log (b) +
    O * log (E) +
    lgamma ( O + a ) -
    lgamma ( a ) -
    log_factorial_O -
    ( O + a ) * log ( b + num_sub )
  
  return(-sum ( LL ))
}

O<-triads_OE$O
E<-triads_OE$E
log_factorial_O<-triads_OE$log_factorial_O

# set the initial value for parameters
theta <- 0.001


#R function to estimate parameter; lower shows the minimum value for estimated parameter
par.optim <- optim( par= 60 , PG , lower=0.001, upper=Inf, method="L-BFGS-B")

estimated_theta <- par.optim$par
cat("Estimated theta:", estimated_theta, "\n")

triads_OE$Posterior_OE <- ( par.optim$par [1] + O ) / ( par.optim$par [1] + E )
triads_OE

############


summary_stats <- aggregate(E123 ~ category, triads2, FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))
summary_stats





###############N=1000
require(bindata)
library(haven)
library(xlsx)
library(copula)
library(arules)
library(ggplot2)
library(magick)
library(tidyverse)
library(dplyr)
library(gmodels)
library(epiDisplay)


set.seed(1234)
N= 1000

p1= 0.35
p2= 0.33

p3=0.30
p4=0.25

p5=0.23
p6= 0.20

p7= 0.18
p8=0.16

p9=0.15
p10=0.14

p11=0.12
p12=0.10

p13=0.09
p14=0.08

p15=0.07
p16=0.07

p17=0.06
p18=0.06

p19=0.05
p20=0.05

p21=0.04
p22=0.04

p23=0.03
p24=0.03

p25=0.02  







rho <- matrix(0.001, nrow = 25, ncol = 25)
diag(rho) <- 1

rho[1, 4] = 0.35;  rho[4, 1] = 0.35
rho[1, 7] = 0.35;  rho[7, 1] = 0.35
rho[1, 10] = 0.30; rho[10, 1] = 0.30
rho[1, 13] = 0.30; rho[13, 1] = 0.30
rho[1, 16] = 0.25; rho[16, 1] = 0.25
rho[1, 19] = 0.20; rho[19, 1] = 0.20


rho[4, 7] = 0.20  ; rho[7, 4] = 0.20
rho[4, 10] = 0.22 ; rho[10, 4] = 0.22
rho[4, 13] = 0.20 ; rho[13, 4] = 0.20
rho[4, 16] = 0.18 ; rho[16, 4] = 0.18
rho[4, 19] =0.15  ; rho[19, 4] = 0.10

rho[7, 10] = 0.20 ; rho[10, 7] = 0.20
rho[7, 13] = 0.20 ; rho[13, 7] = 0.20
rho[7, 16] = 0.20 ; rho[16, 7] = 0.20
rho[7, 19] = 0.15 ; rho[19, 7] = 0.15

rho[10, 13] = 0.20 ; rho[13, 10] = 0.20 
rho[10, 16] = 0.15 ; rho[16, 10] = 0.15 
rho[10, 19] = 0.10 ; rho[19, 10] = 0.10 

rho[13, 16] = 0.10 ; rho[16, 13] = 0.10 
rho[13, 19] = 0.10 ; rho[19, 13] = 0.10 

rho[16, 19] = 0.10 ; rho[19, 16] = 0.10




rho[2, 5] = 0.35;  rho[5, 2] = 0.35
rho[2, 8] = 0.35;  rho[8, 2] = 0.35
rho[2, 11] = 0.30; rho[11, 2] = 0.30
rho[2, 14] = 0.30; rho[14, 2] = 0.30
rho[2, 17] = 0.25; rho[17, 2] = 0.25
rho[2, 20] = 0.20; rho[20, 2] = 0.20


rho[5, 8] = 0.30  ; rho[8, 5] = 0.30
rho[5, 11] = 0.25 ; rho[11, 5] = 0.25
rho[5, 14] = 0.18 ; rho[14, 5] = 0.18
rho[5, 17] = 0.15 ; rho[17, 5] = 0.15
rho[5, 20] =0.15  ; rho[20, 5] = 0.15

rho[8, 11] = 0.20 ; rho[11, 8] = 0.20
rho[8, 14] = 0.20 ; rho[14, 8] = 0.20
rho[8, 17] = 0.15 ; rho[17, 8] = 0.15
rho[8, 20] = 0.10 ; rho[20, 8] = 0.10

rho[11, 14] = 0.18 ; rho[14, 11] = 0.18
rho[11, 17] = 0.10 ; rho[17, 11] = 0.10 
rho[11, 20] = 0.10 ; rho[20, 11] = 0.10 

rho[14, 17] = 0.10 ; rho[17, 14] = 0.10 
rho[14, 20] = 0.08 ; rho[20, 14] = 0.08 

rho[17, 20] = 0.08 ; rho[20, 17] = 0.08



rho[6, 9] = 0.30;  rho[9, 6] = 0.30
rho[6, 12] = 0.30;  rho[12, 6] = 0.30
rho[6, 15] = 0.20; rho[15, 6] = 0.20
rho[6, 18] = 0.15; rho[18, 6] = 0.15
rho[6, 21] = 0.10; rho[21, 6] = 0.10
rho[6, 24] = 0.10; rho[24, 6] = 0.10


rho[9, 12] = 0.20  ; rho[12, 9] = 0.20
rho[9, 15] = 0.15 ; rho[15, 9] = 0.15
rho[9, 18] = 0.12 ; rho[18, 9] = 0.12
rho[9, 21] = 0.10 ; rho[21, 9] = 0.10
rho[9, 24] =0.08  ; rho[24, 9] = 0.08

rho[12, 15] = 0.10 ; rho[15, 12] = 0.10
rho[12, 18] = 0.10 ; rho[18, 12] = 0.10
rho[12, 21] = 0.10 ; rho[21, 12] = 0.10
rho[12, 24] = 0.10 ; rho[24, 12] = 0.10

rho[15, 18] = 0.10 ; rho[18, 15] = 0.10 
rho[15, 21] = 0.05 ; rho[21, 15] = 0.05 
rho[15, 24] = 0.05 ; rho[24, 15] = 0.05 

rho[18,21] = 0.05 ; rho[21, 18] = 0.05 
rho[18, 24] = 0.05 ; rho[24, 18] = 0.05 

rho[21, 24] = 0.05 ; rho[24, 21] = 0.05


d3 <- rmvbin(N, margprob=c(p1,p2, p3, p4, p5, p6, p7, p8, p9, p10, 
                           p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
                           p21, p22, p23, p24, p25), bincorr=rho)

colnames(d3 ) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20",
                   "x21", "x22", "x23", "x24", "x25")
d3=data.frame(d3)
cor(d3)


num_sub=dim(d3)[1]  #sample size
num_cond=dim(d3)[2]  # number of variables

k<-combn(1:num_cond,3) #generate list of all unique triads of conditions (3 refers to the number of conditions)
n<-dim(k)[2] #number of unique triads


triads3<-matrix(0,n,12)

triads3[,1:3]<-t(k)

colnames(triads3)<-c("condition1","condition2","condition3","n123","E123","OE", "n12", "n13", "n23", "n1", "n2", "n3")

triads3<-as.data.frame(triads3)

for (r in 1:n){    
  x<-triads3[r,1]
  y<-triads3[r,2]
  z<-triads3[r,3]
  triads3[r,"n123"]<-sum(d3[,x]==1 & d3[,y]==1 & d3[,z]==1)
  nx<-sum(d3[,x]==1)/num_sub
  ny<-sum(d3[,y]==1)/num_sub
  nz<-sum(d3[,z]==1)/num_sub
  triads3[r,"E123"]<-nx*ny*nz*num_sub
  triads3[r,"n12"]<-sum(d3[,x]==1 & d3[,y]==1 )
  triads3[r,"n13"]<-sum(d3[,x]==1  & d3[,z]==1)
  triads3[r,"n23"]<-sum(d3[,y]==1 & d3[,z]==1)
  triads3[r,"n1"]<-sum(d3[,x]==1 )
  triads3[r,"n2"]<-sum(d3[,y]==1 )
  triads3[r,"n3"]<-sum(d3[,z]==1 )
}

triads3$OE<-triads3$n123/triads3$E123
triads3$OE_binary<-ifelse(triads3$OE>=2, 1, 0)


#calculate lifts 12 then 3, and so one. Take the maximum of three lifts
triads3$N=N

triads3$lift12=(triads3$N * triads3$n12) / (triads3$n1 * triads3$n2)
triads3$lift13=(triads3$N * triads3$n13) / (triads3$n1 * triads3$n3)
triads3$lift23=(triads3$N * triads3$n23) / (triads3$n2 * triads3$n3)
triads3=triads3 %>% rowwise() %>% mutate(max_lift=max(c(lift12, lift13, lift23)))
triads3=triads3 %>% rowwise() %>% mutate(min_lift=min(c(lift12, lift13, lift23)))



triads3$p_x1=sum(triads3$n1)/ triads3$N
triads3$p_x2=sum(triads3$n2)/ triads3$N
triads3$p_x3=sum(triads3$n3)/ triads3$N

triads3$Triad_prevalence=(triads3$n123/ triads3$N)*100
summary(triads3$Triad_prevalence)


triads3$lift12.3=(triads3$N * triads3$n123) / (triads3$n12 * triads3$n3)
triads3$lift13.2=(triads3$N * triads3$n123) / (triads3$n13 * triads3$n2) 
triads3$lift23.1=(triads3$N * triads3$n123) / (triads3$n23 * triads3$n1) 
triads3=triads3 %>% rowwise() %>% mutate(max_lift_triad=max(c(lift12.3, lift13.2, lift23.1)))

triads3$OE_lift_ratio=triads3$OE/ triads3$max_lift_triad



percentile_10=quantile(triads3$E123[triads3$E123 <100], prob=c(0.10))
percentile_50=quantile(triads3$E123[triads3$E123 <100], prob=c(0.50))
percentile_90=quantile(triads3$E123[triads3$E123 <100], prob=c(0.90))


percentile_10
percentile_50
percentile_90


triads3$posterior_OE_10=(triads3$n123 + percentile_10) / (triads3$E123 + percentile_10)
triads3$posterior_OE_50=(triads3$n123 + percentile_50) / (triads3$E123 + percentile_50)
triads3$posterior_OE_90=(triads3$n123 + percentile_90) / (triads3$E123 + percentile_90)
triads3$posterior_OE_heurostic=(triads3$n123 + 0.5) / (triads3$E123 + 0.5)


triads3$association1 <- ifelse(
  (triads3$condition1 %in% c(1, 4, 7, 10, 13, 16, 19)) &
    (triads3$condition2 %in% c(1, 4, 7, 10, 13, 16, 19)) &
    (triads3$condition3 %in% c(1, 4, 7, 10, 13, 16, 19)),
  1, 0
)


triads3$association2 <- ifelse(
  (triads3$condition1 %in% c(2, 5, 8, 11, 14, 17, 20)) &
    (triads3$condition2 %in% c(2, 5, 8, 11, 14, 17, 20)) &
    (triads3$condition3 %in% c(2, 5, 8, 11, 14, 17, 20)),
  1, 0
)



triads3$association3 <- ifelse(
  (triads3$condition1 %in% c(6, 9, 12, 15, 18, 21, 24)) &
    (triads3$condition2 %in% c(6, 9, 12, 15, 18, 21, 24)) &
    (triads3$condition3 %in% c(6, 9, 12, 15, 18, 21, 24)),
  1, 0
)


triads3$association= ifelse(triads3$association1==1 | 
                              triads3$association2==1 | 
                              triads3$association3==1, 
                            1, 0)

triads3$association=as.factor(triads3$association)

triads3$scenario=3


CrossTable(triads3$OE_binary, triads3$association, chisq=TRUE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE)



#min and max of max_lift for real and false associations
summary_stats <- triads3 %>%
  group_by(association) %>%
  summarize(min_minlift = min(min_lift),
            max_minlift = max(min_lift))

print(summary_stats)



summary_stats <- triads3 %>%
  group_by(association) %>%
  summarize(min_maxlift = min(max_lift),
            max_maxlift = max(max_lift))

print(summary_stats)


#make sure all true associations have OE >2
triads3$OE=ifelse(triads3$association==1 & triads3$OE <2, 2.1, triads3$OE)



CrossTable(triads3$OE_binary, triads3$association, chisq=TRUE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE)


write_sav(triads3,"S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/triads3.sav")



####find the optimum alpha so that all posterior OE ratios <2 when association=0

triads3_false=subset(triads3, triads3$association==0)
dim(triads3_false)

ALPHA=1

# Define a function to calculate POSTERIOR_OE
calculate_POSTERIOR_OE <- function(n123, E123, ALPHA) {
  (n123 + ALPHA) / (E123 + ALPHA)
}

# Initial calculation of POSTERIOR_OE
triads3_false$POSTERIOR_OE <- calculate_POSTERIOR_OE(triads3_false$n123, triads3_false$E123, ALPHA)


# Iteratively update ALPHA until POSTERIOR_OE max is less than the target
while (max(triads3_false$POSTERIOR_OE) >= target_max_POSTERIOR_OE) {
  ALPHA <- ALPHA + 1  # You can adjust the step size as needed
  triads3_false$POSTERIOR_OE <- calculate_POSTERIOR_OE(triads3_false$n123, triads3_false$E123, ALPHA)
}

# The updated ALPHA value
cat("Optimal ALPHA:", ALPHA, "\n")


triads3$ALPHA=ALPHA
triads3$POSTERIOR_OE=(triads3$n123 + triads3$ALPHA) / (triads3$E123 + triads3$ALPHA)

triads3%>% group_by(association)%>% summarise (min=min(POSTERIOR_OE), max=max(POSTERIOR_OE),na.rm=TRUE)




###correct filters
triads3$category=ifelse(triads3$association==0 & triads3$OE > threshold, 1, 
                        ifelse(triads3$association==0 & triads3$OE < threshold, 0, 2))  #2 TRUE, 1 FALSE & SPURIOUS, 0 FALSE & NON-SPURIOUS
tab1(triads3$category)

triads3$category=as.factor(triads3$category)
triads3$category=factor(triads3$category, levels=c(0, 1, 2),labels=c("False triads","Spurious triads", "True triads"))




triads3$filter_Bayes=ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_90 >= threshold2,  0, 
                            ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_90 >= threshold1 & triads3$posterior_OE_90 < threshold2, 1, 
                                   ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_90 <= threshold1, 2, NA)))
tab1(triads3$filter_Bayes)


triads3$filter_Bayes50=ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_50 >= threshold2,  0, 
                            ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_50 >= threshold1 & triads3$posterior_OE_50 < threshold2, 1, 
                                   ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_50 <= threshold1, 2, NA)))
tab1(triads3$filter_Bayes50)



triads3$filter_ARM=ifelse(triads3$category=="Spurious triads" & triads3$OE_lift_ratio >= threshold2,  0, 
                          ifelse(triads3$category=="Spurious triads" & triads3$OE_lift_ratio >= threshold1 & triads3$OE_lift_ratio < threshold2, 1, 
                                 ifelse(triads3$category=="Spurious triads" & triads3$OE_lift_ratio <= threshold1, 2, NA)))
tab1(triads3$filter_ARM)



###wrong filters

triads3$filter_Bayes_wrong=ifelse(triads3$category=="True triads" & triads3$posterior_OE_90 > threshold2,  0, 
                                  ifelse(triads3$category=="True triads" & triads3$posterior_OE_90 >= threshold1 & triads3$posterior_OE_90 < threshold2, 1, 
                                         ifelse(triads3$category=="True triads" & triads3$posterior_OE_90 < threshold1, 2, NA)))
tab1(triads3$filter_Bayes_wrong)


triads3$filter_Bayes_wrong50=ifelse(triads3$category=="True triads" & triads3$posterior_OE_50 > threshold2,  0, 
                                  ifelse(triads3$category=="True triads" & triads3$posterior_OE_50 >= threshold1 & triads3$posterior_OE_50 < threshold2, 1, 
                                         ifelse(triads3$category=="True triads" & triads3$posterior_OE_50 < threshold1, 2, NA)))
tab1(triads3$filter_Bayes_wrong50)



triads3$filter_ARM_wrong=ifelse(triads3$category=="True triads" & triads3$OE_lift_ratio > threshold2,  0, 
                                ifelse(triads3$category=="True triads" & triads3$OE_lift_ratio >= threshold1 & triads3$OE_lift_ratio < threshold2, 1, 
                                       ifelse(triads3$category=="True triads" & triads3$OE_lift_ratio < threshold1, 2, NA)))
tab1(triads3$filter_ARM_wrong)





triads3$filter_Bayes_optimum_wrong=ifelse(triads3$category=="True triads" & triads3$POSTERIOR_OE > threshold2,  0, 
                                          ifelse(triads3$category=="True triads" & triads3$POSTERIOR_OE >= threshold1 & triads3$POSTERIOR_OE < threshold2, 1, 
                                                 ifelse(triads3$category=="True triads" & triads3$POSTERIOR_OE < threshold1, 2, NA)))
tab1(triads3$filter_Bayes_optimum_wrong)



## heuristic 0.5
triads3$filter_Bayes_0.5=ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_heurostic >= threshold2,  0, 
                                ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_heurostic >= threshold1 & triads3$posterior_OE_heurostic < threshold2, 1, 
                                       ifelse(triads3$category=="Spurious triads" & triads3$posterior_OE_heurostic <= threshold1, 2, NA)))
tab1(triads3$filter_Bayes_0.5)



triads3$filter_Bayes_0.5_wrong=ifelse(triads3$category=="True triads" & triads3$posterior_OE_heurostic > threshold2,  0, 
                                      ifelse(triads3$category=="True triads" & triads3$posterior_OE_heurostic >= threshold1 & triads3$posterior_OE_heurostic < threshold2, 1, 
                                             ifelse(triads3$category=="True triads" & triads3$posterior_OE_heurostic < threshold1, 2, NA)))
tab1(triads3$filter_Bayes_0.5_wrong)
#####





##estimate theta under no constratin (change min to 80 for threshold of 90th percentile)

triads_OE=triads3 %>% dplyr::select (condition1, condition2, condition3, n123, E123)
triads_OE=rename(triads_OE, O=n123, E=E123)
num_sub=1000

for (m in 1:n){
  O<-triads_OE$O[m]
  if (O>0){
    triads_OE$log_factorial_O[m]<-sum(log(1:O)) 
  }
  else {
    triads_OE$log_factorial_O[m]<-0
  }
}

#the likelihood function for Poisson-Gamma model
PG <- function (theta){
  O <- O
  E <- E
  log_factorial_O<-log_factorial_O
  a = theta 
  b = theta 
  LL <- a * log (b) +
    O * log (E) +
    lgamma ( O + a ) -
    lgamma ( a ) -
    log_factorial_O -
    ( O + a ) * log ( b + num_sub )
  
  return(-sum ( LL ))
}

O<-triads_OE$O
E<-triads_OE$E
log_factorial_O<-triads_OE$log_factorial_O

# set the initial value for parameters
theta <- 0.001


#R function to estimate parameter; lower shows the minimum value for estimated parameter
par.optim <- optim( par= 60 , PG , lower=0.001, upper=Inf, method="L-BFGS-B")

estimated_theta <- par.optim$par
cat("Estimated theta:", estimated_theta, "\n")

triads_OE$Posterior_OE <- ( par.optim$par [1] + O ) / ( par.optim$par [1] + E )
triads_OE




summary_stats <- aggregate(E123 ~ category, triads3, FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))
summary_stats



# merge datasets

data=bind_rows (triads1, triads2, triads3)

data$scenario=as.factor(data$scenario)
data$scenario=factor(data$scenario,levels=c(1, 2, 3),labels=c("N=300,000","N=10,000", "N=1,000"))

data$category=as.factor(data$category)
data$category=factor(data$category, levels=c(0, 1, 2),labels=c("False triads","Spurious triads", "True triads"))

data$association=as.factor(data$association)
data$association=factor(data$association,levels=c(0, 1),labels=c("No","Yes"))


write_sav(data,"S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/data.sav")


##adjust to show up in Figures
triads1=subset(triads1, triads1$Triad_prevalence<=5)
triads2=subset(triads2, triads2$Triad_prevalence<=5)
triads3=subset(triads3, triads3$Triad_prevalence<=5)


############



#####################N=300,000

p0= ggplot(data=triads1, aes(y=OE, x=Triad_prevalence, color=category, shape=category))
p0= p0+geom_point(size=2)
p0= p0+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("OE ratio")+xlab("Prevalence of triad")
p0= p0+theme(text=element_text(size=20))
p0= p0+theme(legend.position=c(0.85,0.85))
p0= p0+scale_shape_manual(values=c(0, 16, 10))
p0= p0+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
p0= p0+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
p0= p0+scale_color_manual(values = c("grey", "black", "blue"))
p0= p0+ theme(legend.text=element_text(size=12))
p0

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "p0.jpeg")



#graph of Posterior_90 OE ratios 

p90= ggplot(data=triads1, aes(y=posterior_OE_90, x=Triad_prevalence, color=category, shape=category))
p90= p90+geom_point(size=2)
p90= p90+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("Posterior ratio (heuristic alpha=89)")+xlab("Prevalence of triad")
p90= p90+theme(text=element_text(size=20))
p90= p90+theme(legend.position=c(0.85,0.85))
p90= p90+scale_shape_manual(values=c(0, 16, 10))
p90= p90+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
p90= p90+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
p90= p90+scale_color_manual(values = c("grey", "black", "blue"))
p90= p90+ theme(legend.text=element_text(size=12))

p90

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "p90.jpeg")



#graph of OPTIMUM Posterior ratios 

poptimum= ggplot(data=triads1, aes(y=POSTERIOR_OE, x=Triad_prevalence, color=category, shape=category))
poptimum= poptimum+geom_point(size=2)
poptimum= poptimum+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("Posterior ratio (optimal alpha=1732)")+xlab("Prevalence of triad")
poptimum= poptimum+theme(text=element_text(size=20))
poptimum= poptimum+theme(legend.position=c(0.85,0.85))
poptimum= poptimum+scale_shape_manual(values=c(0, 16,  10))
poptimum= poptimum+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
poptimum= poptimum+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
poptimum= poptimum+scale_color_manual(values = c("grey", "black", "blue"))
poptimum= poptimum+ theme(legend.text=element_text(size=12))

poptimum

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "poptimum.jpeg")



###graph of OE/ lift
ratio= ggplot(data=triads1, aes(y=OE_lift_ratio, x=Triad_prevalence, color=category, shape=category))
ratio= ratio+geom_point(size=2)
ratio= ratio+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("ratio of OE over lift")+xlab("Prevalence of triad")
ratio= ratio+theme(text=element_text(size=20))
ratio= ratio+theme(legend.position=c(0.85,0.85))
ratio= ratio+scale_shape_manual(values=c(0, 16, 10))
ratio= ratio+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
ratio= ratio+scale_y_continuous(limits=c(0, 4.5),breaks = seq(0, 4.5, by=0.5))
ratio= ratio+scale_color_manual(values = c("grey", "black", "blue"))
ratio= ratio+ theme(legend.text=element_text(size=12))

ratio

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "ratio.jpeg")



############combine graphs

image1 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/p0.JPEG")
image2 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/p90.JPEG")
image3 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/poptimum.JPEG")
image4 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/ratio.JPEG")


max_width <- max(image_info(image1)$width, image_info(image2)$width, image_info(image3)$width, image_info(image4)$width)


image1 <- image_scale(image1, max_width)
image2 <- image_scale(image2, max_width)
image3 <- image_scale(image3, max_width)
image4 <- image_scale(image4, max_width)

combined1 <- image_append(c(image1, image4), stack = FALSE)
combined2 <- image_append(c(image2, image3), stack = FALSE)
combined= image_append(c(combined1, combined2), stack = TRUE)

image_write(combined, path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med//sim1.JPEG")

#################





#####################N=10,000

p0= ggplot(data=triads2, aes(y=OE, x=Triad_prevalence, color=category, shape=category))
p0= p0+geom_point(size=2)
p0= p0+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("OE ratio")+xlab("Prevalence of triad")
p0= p0+theme(text=element_text(size=20))
p0= p0+theme(legend.position=c(0.85,0.85))
p0= p0+scale_shape_manual(values=c(0, 16, 10))
p0= p0+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
p0= p0+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
p0= p0+scale_color_manual(values = c("grey", "black", "blue"))
p0= p0+ theme(legend.text=element_text(size=12))
p0

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "p0.jpeg")



#graph of Posterior_90 OE ratios 

p90= ggplot(data=triads2, aes(y=posterior_OE_90, x=Triad_prevalence, color=category, shape=category))
p90= p90+geom_point(size=2)
p90= p90+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("Posterior ratio (heuristic alpha=42)")+xlab("Prevalence of triad")
p90= p90+theme(text=element_text(size=20))
p90= p90+theme(legend.position=c(0.85,0.85))
p90= p90+scale_shape_manual(values=c(0, 16, 10))
p90= p90+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
p90= p90+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
p90= p90+scale_color_manual(values = c("grey", "black", "blue"))
p90= p90+ theme(legend.text=element_text(size=12))

p90

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "p90.jpeg")



#graph of OPTIMUM Posterior ratios 

poptimum= ggplot(data=triads2, aes(y=POSTERIOR_OE, x=Triad_prevalence, color=category, shape=category))
poptimum= poptimum+geom_point(size=2)
poptimum= poptimum+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("Posterior ratio (optimal alpha=77)")+xlab("Prevalence of triad")
poptimum= poptimum+theme(text=element_text(size=20))
poptimum= poptimum+theme(legend.position=c(0.85,0.85))
poptimum= poptimum+scale_shape_manual(values=c(0, 16, 10))
poptimum= poptimum+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
poptimum= poptimum+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
poptimum= poptimum+scale_color_manual(values = c("grey", "black", "blue"))
poptimum= poptimum+ theme(legend.text=element_text(size=12))

poptimum

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "poptimum.jpeg")



###graph of OE/ lift
ratio= ggplot(data=triads2, aes(y=OE_lift_ratio, x=Triad_prevalence, color=category, shape=category))
ratio= ratio+geom_point(size=2)
ratio= ratio+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("ratio of OE over lift")+xlab("Prevalence of triad")
ratio= ratio+theme(text=element_text(size=20))
ratio= ratio+theme(legend.position=c(0.85,0.85))
ratio= ratio+scale_shape_manual(values=c(0, 16, 10))
ratio= ratio+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
ratio= ratio+scale_y_continuous(limits=c(0, 4.5),breaks = seq(0, 4.5, by=0.5))
ratio= ratio+scale_color_manual(values = c("grey", "black", "blue"))
ratio= ratio+ theme(legend.text=element_text(size=12))

ratio

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "ratio.jpeg")



############combine graphs

image1 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/p0.JPEG")
image2 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/p90.JPEG")
image3 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/poptimum.JPEG")
image4 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/ratio.JPEG")


max_width <- max(image_info(image1)$width, image_info(image2)$width, image_info(image3)$width, image_info(image4)$width)


image1 <- image_scale(image1, max_width)
image2 <- image_scale(image2, max_width)
image3 <- image_scale(image3, max_width)
image4 <- image_scale(image4, max_width)

combined1 <- image_append(c(image1, image4), stack = FALSE)
combined2 <- image_append(c(image2, image3), stack = FALSE)
combined= image_append(c(combined1, combined2), stack = TRUE)

image_write(combined, path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med//sim2.JPEG")
#################





####################N=1,000

p0= ggplot(data=triads3, aes(y=OE, x=Triad_prevalence, color=category, shape=category))
p0= p0+geom_point(size=2)
p0= p0+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("OE ratio")+xlab("Prevalence of triad")
p0= p0+theme(text=element_text(size=20))
p0= p0+theme(legend.position=c(0.85,0.85))
p0= p0+scale_shape_manual(values=c(0, 16, 10))
p0= p0+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
p0= p0+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
p0= p0+scale_color_manual(values = c("grey", "black", "blue"))
p0= p0+ theme(legend.text=element_text(size=12))
p0

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "p0.jpeg")



#graph of Posterior_90 OE ratios 

p90= ggplot(data=triads3, aes(y=posterior_OE_90, x=Triad_prevalence, color=category, shape=category))
p90= p90+geom_point(size=2)
p90= p90+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("Posterior ratio (heuristic alpha=5)")+xlab("Prevalence of triad")
p90= p90+theme(text=element_text(size=20))
p90= p90+theme(legend.position=c(0.85,0.85))
p90= p90+scale_shape_manual(values=c(0, 16, 10))
p90= p90+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
p90= p90+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
p90= p90+scale_color_manual(values = c("grey", "black", "blue"))
p90= p90+ theme(legend.text=element_text(size=12))

p90

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "p90.jpeg")



#graph of OPTIMUM Posterior ratios 

poptimum= ggplot(data=triads3, aes(y=POSTERIOR_OE, x=Triad_prevalence, color=category, shape=category))
poptimum= poptimum+geom_point(size=2)
poptimum= poptimum+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("Posterior ratio (optimal alpha=9)")+xlab("Prevalence of triad")
poptimum= poptimum+theme(text=element_text(size=20))
poptimum= poptimum+theme(legend.position=c(0.85,0.85))
poptimum= poptimum+scale_shape_manual(values=c(0, 16, 10))
poptimum= poptimum+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
poptimum= poptimum+scale_y_continuous(limits=c(0, 12),breaks = seq(0, 12, by=2))
poptimum= poptimum+scale_color_manual(values = c("grey", "black", "blue"))
poptimum= poptimum+ theme(legend.text=element_text(size=12))

poptimum

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "poptimum.jpeg")



###graph of OE/ lift
ratio= ggplot(data=triads3, aes(y=OE_lift_ratio, x=Triad_prevalence, color=category, shape=category))
ratio= ratio+geom_point(size=2)
ratio= ratio+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("ratio of OE over lift")+xlab("Prevalence of triad")
ratio= ratio+theme(text=element_text(size=20))
ratio= ratio+theme(legend.position=c(0.85,0.85))
ratio= ratio+scale_shape_manual(values=c(0, 16, 10))
ratio= ratio+ geom_hline(yintercept = point, linetype = "dashed", color = "black")
ratio= ratio+scale_x_continuous(limits=c(0, 5),breaks = seq(0, 5, by=1))
ratio= ratio+scale_y_continuous(limits=c(0, 4.5),breaks = seq(0, 4.5, by=0.5))
ratio= ratio+scale_color_manual(values = c("grey", "black", "blue"))
ratio= ratio+ theme(legend.text=element_text(size=12))

ratio

ggsave(path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med", "ratio.jpeg")



############combine graphs

image1 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/p0.JPEG")
image2 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/p90.JPEG")
image3 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/poptimum.JPEG")
image4 <- image_read("S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med/ratio.JPEG")


max_width <- max(image_info(image1)$width, image_info(image2)$width, image_info(image3)$width, image_info(image4)$width)


image1 <- image_scale(image1, max_width)
image2 <- image_scale(image2, max_width)
image3 <- image_scale(image3, max_width)
image4 <- image_scale(image4, max_width)

combined1 <- image_append(c(image1, image4), stack = FALSE)
combined2 <- image_append(c(image2, image3), stack = FALSE)
combined= image_append(c(combined1, combined2), stack = TRUE)

image_write(combined, path = "S://SPH-CLLRData/Linked Data Project 15/ICD coding/PM9 31 March 2022/MANUSCRIPT/comparison of methods/Stat in Med//sim3.JPEG")
#################















