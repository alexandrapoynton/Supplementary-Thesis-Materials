#import my data 
stateyr <- read.csv("C:\\Users\\User\\Documents\\Honours\\Thesis\\Datasets\\Ready to Go\\noeu\\stateyearnoeu.csv")
dyadyr <- read.csv("C:\\Users\\User\\Documents\\Honours\\Thesis\\Datasets\\Ready to Go\\noeu\\dyadyearnoeu.csv")

#packages
library(RSiena)

#clean up dataframes and set parameters
colnames(stateyr)[1] = "actor"
colnames(dyadyr)[1] = "actora"
dyadyr$minpolity <- do.call(pmin,c(dyadyr[18:19],na.rm = TRUE))
actors <- stateyr[stateyr$year == '1995',]
years <- stateyr[stateyr$actor == 'AAB',]
actors <- actors$actora
years <- years$year
years27 <- head(years, -1)

###DEFINE FUNCTIONS###
#define networks#
#note that: columns are actor a, rows actor b - dyadyr file MUST not be rearranged 
#disputes
disputes <- array(c(dyadyr$aresp_tdpr), dim = c(150,150,28),dimnames = list(actors,actors,years))
disputes <- sienaDependent(disputes, allowOnly = TRUE)

#third party - respondent ties
tpty <- array(c(dyadyr$tptyb_respa), dim = c(150,150,28),dimnames = list(actors,actors,years))
tpty <- sienaDependent(tpty, allowOnly = TRUE)

#defining monadic variables
#gni with log transform
gni <- matrix(stateyr$gniam_log,nrow=length(actors),ncol=length(years),dimnames=list(actors,years))
gni <- varCovar(gni)

#gdp per capita
gdppc <- matrix(stateyr$gdppc_log,nrow=length(actors),ncol=length(years),dimnames=list(actors,years))
gdppc <- varCovar(gdppc)

#defining dyadic variables
#polity - lower of the two
polity <- head(dyadyr$minpolity, - 22500)
polity <- array(c(polity), dim = c(150,150,27),dimnames = list(actors,actors,years27))
polity <- varDyadCovar(polity)

#contiguity
contiguity <- head(dyadyr$contiguity, - 22500)
contiguity <- array(c(contiguity), dim = c(150,150,27),dimnames = list(actors,actors,years27))
contiguity <- varDyadCovar(contiguity)

#involvement in military dispute
inv_md <- head(dyadyr$inv_md, - 22500)
inv_md <- array(c(inv_md), dim = c(150,150,27),dimnames = list(actors,actors,years27))
inv_md <- varDyadCovar(inv_md)

#involvement in alliance
alliance <- head(dyadyr$alliance, - 22500)
alliance <- array(c(alliance), dim = c(150,150,27),dimnames = list(actors,actors,years27))
alliance <- varDyadCovar(alliance)

#exports from i to j
xps_log <- head(dyadyr$xps_btoa_log, - 22500)
xps_log <- array(c(xps_log), dim = c(150,150,27),dimnames = list(actors,actors,years27))
xps_log <- varDyadCovar(xps_log)


#create a Siena file
mydata <- sienaDataCreate(disputes, tpty, gni, gdppc, inv_md, alliance, xps_log, contiguity)

#define effects, documentation, outcome
myeff_1 <- getEffects(mydata)
print01Report(mydata)
effectsDocumentation(myeff_1)

#run model 1
model_1 <- sienaAlgorithmCreate(useStdInits = TRUE, projname = 'model1results')
model_1_results <- siena07(model_1, data=mydata, effects=myeff_1, batch = FALSE)
summary(model_1_results)

#export model 1 to html file
xtable(model_1_results, file = "model_1_results.htm", type = 'html')

#add effects as described to the model
myeff_2 <- getEffects(mydata)
myeff_2 <-includeEffects(myeff_2, outRateLog, type = 'rate')
myeff_2 <- includeEffects(myeff_2, gwespFF)
myeff_2 <- includeEffects(myeff_2, altX, interaction1 = "gni")
myeff_2 <- includeEffects(myeff_2, crprod, interaction1 = 'tpty')
model_2 <- sienaAlgorithmCreate(useStdInits = TRUE, projname = 'model2results', cond = FALSE)
#crashed so have removed allow only from disputes sienaDependent, still didn't work using unconditional estimation
model_2_results <- siena07(model_2, data=mydata, effects=myeff_2, batch = FALSE, returnDeps = TRUE)
summary(model_2_results)

#export model 2 results to html
xtable(model_2_results, file = "model_2_results.htm", type = 'html')

#model 2 had very high t scores, remove rate effect as was very poor fit - find another better outdegrees constraint
#model 3
myeff_3 <- getEffects(mydata)
myeff_3 <- includeEffects(myeff_3, gwespFF)
myeff_3 <- includeEffects(myeff_3, altX, interaction1 = "gni")
myeff_3 <- includeEffects(myeff_3, crprod, interaction1 = 'tpty')
model_3 <- sienaAlgorithmCreate(useStdInits = TRUE, projname = 'model3results', cond = FALSE)
model_3_results <- siena07(model_3, data=mydata, effects=myeff_3, batch = FALSE, returnDeps = TRUE)
summary(model_3_results)
#much better, will run one more time contingent on previous results to refine and get t value down

#rerun
model_3_results <- siena07(model_3, data=mydata, effects=myeff_3, batch = FALSE, returnDeps = TRUE, prevAns = model_3_results)

#export model 3 to html
xtable(model_3_results, file = "model_3_results.htm", type = 'html')

#model 4
#remove gwesp effect - perhaps not representative of state behavior
myeff_4 <- getEffects(mydata)
myeff_4 <- includeEffects(myeff_3, altX, interaction1 = "gni")
myeff_4 <- includeEffects(myeff_3, crprod, interaction1 = 'tpty')
model_4 <- sienaAlgorithmCreate(useStdInits = TRUE, projname = 'model4results', cond = FALSE)
model_4_results <- siena07(model_4, data=mydata, effects=myeff_4, batch = FALSE, returnDeps = TRUE)
summary(model_4_results)

#print model 4 results
xtable(model_4_results, file = "model_4_results.htm", type = 'html')

#model 3 did better at this stage - run gof testing
gofi1 <- sienaGOF(model_3_results, OutdegreeDistribution, verbose=TRUE, join=TRUE,
                  varName="disputes")
gofi2 <- sienaGOF(model_3_results, IndegreeDistribution, verbose=TRUE, join=TRUE,
                  varName="disputes")
gofi1
gofi2
plot(gofi2)

#need some treatment of outdegrees
myeff_5 <- getEffects(mydata)
myeff_5 <- includeEffects(myeff_5, gwespFF)
myeff_5 <- includeEffects(myeff_5, altX, interaction1 = "gni")
myeff_5 <- includeEffects(myeff_5, egoX, interaction1 = "gdppc")
myeff_5 <- includeEffects(myeff_5, crprod, interaction1 = 'tpty')
model_5 <- sienaAlgorithmCreate(useStdInits = TRUE, projname = 'model5results', cond = FALSE)
model_5_results <- siena07(model_5, data=mydata, effects=myeff_5, batch = FALSE, returnDeps = TRUE)
summary(model_5_results)

#model 5 adequate, run gofi testing
gofi3 <- sienaGOF(model_5_results, OutdegreeDistribution, verbose=TRUE, join=TRUE,
                  varName="disputes")
gofi4 <- sienaGOF(model_5_results, IndegreeDistribution, verbose=TRUE, join=TRUE,
                  varName="disputes")
gofi3
gofi4
plot (gofi4)
#GREAT for indegree - suggests that the model does a good job of establishing who likely targets are, but not necessarily complainants

#print model 5 results
xtable(model_5_results, file = "model_5_results.htm", type = 'html')

#run more testing on model 5
Multipar.RSiena(model_5_results,33)
#Wald test shows that effect of tpty is significant

#try one more model for outdegrees
#model 6 - includes xps_log interaction to model likelihood outdegrees go to more vital export partners
myeff_6 <- getEffects(mydata)
myeff_6 <- includeEffects(myeff_6, gwespFF)
myeff_6 <- includeEffects(myeff_6, altX, interaction1 = "gni")
myeff_6 <- includeEffects(myeff_6, egoX, interaction1 = "gdppc")
myeff_6 <- includeEffects(myeff_6, egoX, interaction1 = "xps_log")
myeff_6 <- includeEffects(myeff_6, crprod, interaction1 = 'tpty')
model_6 <- sienaAlgorithmCreate(useStdInits = TRUE, projname = 'model6results', cond = FALSE)
model_6_results <- siena07(model_6, data=mydata, effects=myeff_6, batch = FALSE, returnDeps = TRUE)

#model 6 gofi
gofi5 <- sienaGOF(model_6_results, OutdegreeDistribution, verbose=TRUE, join=TRUE,
                  varName="disputes")
gofi6 <- sienaGOF(model_6_results, IndegreeDistribution, verbose=TRUE, join=TRUE,
                  varName="disputes")
summary(model_6_results)

gofi5
gofi6
plot(gofi6)

#model 6 VERY good predictor of indegrees - tpty is statistically significant

#print model 5 results
xtable(model_6_results, file = "model_6_results.htm", type = 'html')

#conduct wald-type test model 6
Multipar.RSiena(model_6_results, 33)

#sim model 6 *come back to this maybe if i run out of things to say*
sim1model_6 <- sienaAlgorithmCreate(useStdInits = TRUE, projname = 'sim1model6results', cond = FALSE, nsub = 0, simOnly = TRUE)
sim1model_6_results <- siena07(sim1model_6, data=mydata, effects=myeff_6)


#run a time test *maybe also talk about this if i run out of stuff to say*
timetest <- sienaTimeTest(model_6_results, effects=NULL, excludedEffects=NULL, condition=FALSE)
plot(timetest)
