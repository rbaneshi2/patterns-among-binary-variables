

##packages to rransfer the data into R and to manipulate the data

library(xlsx)
library(readxl)
library(tidyverse)
library(dplyr)
library(haven) # to read SPSS files
library(ggplot2)
library(devtools)




##ASSOCIATION RULES MINING (ARM) TO CALCULATE THE LIFT STATISTIC
library(arules)

# READ THE DATA. MY DATA WAS AN spss FILE NAMED f3. 
# THE DATA SET SHOULD ONLY INCLUDE THE BINARY VARIABLES INDICATING THE PRESENCE OR ABSENCE OF EACH CONDITION FOR EACH SUBJECT
f3 <- read_sav("FOLDER YOU SAVED YOUR DATA/PM9.sav")  
attach(f3)


f3$x1=as.factor(f3$x1) #make sure each variable is known as a factor variable

##ARM is based on the concept of support, confidence, and lift. For a rule of A→B :
##support shows proportion of data used to assess each rule. For pairs, it shows number of subjects who have both conditions. 
##Confidence describes how likely disease B to occur given that disease A is occurred
## If the lift of A→B is four, condition B is four times more likely to occur with condition A than if pairs of conditions were independent 

#The main algorithm to extract triads of conditions is A-Priori algorithm. 
#The idea behind the A-Priori algorithm is to reduce the number of combinations to be examined, by assuming that if condition A is infrequent, 
#then  joint frequency of condition A with other conditions must be equally or more infrequent. 
#The main parameters to reduce the number of combinations to be examined are support and confidence. 
#In this work, to extract all possible combinations, the minimum support and minimum confidence were set at 0.000001 and 0.000001. 
# the argument maxlen controls the length of rules. For example maxlen of two only calculates the lift for pairs
# The appearance argument restricts the rules. We applied a restriction so as to calculate the lifts when conditions are present.

rules2=apriori(f3,
               parameter=list(supp=0.0001,conf=0.001,minlen=2,maxlen=2),
               appearance=list(none=c(
                 "x1=0","x2=0",....,"x27=0"
               )))

quality(rules2)=round(quality(rules2),digits=3)
summary(rules2)
inspect(rules2)
inspect(sort(rules2,by="lift") [1:20])  # summarise twenty rules with highest lift

write(rules2,file="P://rules.csv",sep=",")  #write all rules as a csv file







### CALCULATION OF NORMALISED JOINT FREQUENCIES

# prepare the adjacency matrix. In our data, it was a 27 x 27 matrix where entries in each cell showed the joint frequency of the corresponding pair
# values on the main diagonal should be zero

adj.mat=read.csv("FOLDER YOU SAVED THE NODE LIST/adjmat.csv") # read the adjacency matrix file
adj.mat=as.matrix(adj.mat)
row_targets <- c(1,1,...,1) # set the row margins for all 27 conditions to be one
column_targets <- c(1,1,...,1) # set the column margins for all 27 conditions to be one
adj.mat.norm<- ipu_matrix(adj.mat, row_targets, column_targets)
write.csv(adj.mat.norm,"P:adj.mat.norm.csv") # save the normalised joint frequencies. These will be used as weight in the network analysis





##Network analysis using OR as weight 

#prepare two csv files known as node list and edge list
# the node list has two columns named ID (which can takes values 1 to the number of conditions) and Label (which shows the name of the conditions).
#The dge file has three columns named Source (which shows first condition), Target (which shows second condition), and Weight (which shows strength of association)

library(igraph)

NL=read.csv("FOLDER YOU SAVED THE NODE LIST/node.csv") # read node list
ELOR=read.csv("FOLDER YOU SAVED THE EDGE LIST/OR_edge.csv") #read edge list


gor=graph_from_data_frame(d=ELOR,vertices=NL,directed=FALSE)
gor=set_edge_attr(gor, "weight", value= ELOR$Weight)
is_weighted(gor)

cor=cluster_leiden(gor,objective_function = "modularity",n_iterations = 3,resolution_parameter = 1)
plot(cor,gor,vertex.size=5*degree(gor),layout=layout_with_fr,vertex.size=25,edge.arrow.size=5)

node_cols=c("yellow","yellow","red", "yellow", "white","yellow","green","yellow")[membership(cor)] #color of nodes
poly_cols=c("white","white","white","white", "white","white","white","white")                # color of polygens
plot(cor,gor,col=node_cols,mark.border="black",mark.col=poly_cols)


degree(gor)
strength(gor,weights=ELFN$Weight) #weighted degree
closeness(gor,normalized=TRUE) #when true, it calculates the inverse average distance to all reachable vertices
closeness(gor,normalized=FALSE) #when false, it calculates the inverse of sum of distance to all reachable vertices
betweenness(gor,normalized=TRUE)




##ESTIMATION OF POSTERIOR RATIOS BY BAYESIAN POISSON-GAMMA MODEL

#defining the likelihood function

PG <- function (theta){
  O <- O
  E <- E
  a = theta 
  b = theta 
  LL <- a * log (b) +
    O * log (E) +
    lgamma ( O + a ) -
    lgamma ( a ) -
    log ( factorial ( O ) ) -
    ( O + a ) * log ( b + N )
  
  return(-sum ( LL ))
}

#O and E are the input vectors
O <- c ( 10 ,3, 11, 10, 25, 10, 18, 5, 13, 30 )
E <- c ( 10, 1, 5,  10, 15, 4,   3, 1, 2, 16)

# set the initial value for parameters
theta <- 1

#R function to estimate parameter; lower: the minimum value for estimated parameter
par.optim <- optim( par= 60 , PG ,lower=10, upper=Inf,method="L-BFGS-B")

OE <-  O / E 
OE

Posterior_OE <- ( par.optim$par [1] + O ) / ( par.optim$par [1] + E )
Posterior_OE

data <- cbind (O= O , E= E, OE= OE , Posterior_OE= Posterior_OE)
data






##Scatter plot of OE ratios
#prepare a data set with the following columns: O, E, Ratio, Posterior_Ratio, Triad_prevalence, and Expected_frequency (which takes 1 when E>90th percentile (say 100), 0 otherwise). 
ratios=read_sav("FOLDER YOU SAVED THE EDGE LIST/OE graph.sav")
ratios$Expected_frequency=factor(ratios$Expected_frequency,levels=c(0,1),labels=c(">=100","<100"))
p2=ggplot(ratios,aes(y=Ratio,x=Triad_prevalence,color=Expected_frequency,shape=Expected_frequency))
p2=p2+geom_point()
p2=p2+theme_bw()+ theme(panel.grid.minor = element_blank()) + ylab("OE ratio")+xlab("Prevalence of triad")
p2=p2+theme(text=element_text(size=10))
p2=p2+theme(legend.position=c(0.85,0.85)) # to put the legend in the top right corner of the graph

p2
