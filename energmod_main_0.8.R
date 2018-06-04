# Generic NEM Energy Model - Nuclear, Wind, Solar, Gas, v0.8
remove(list = ls()); options(scipen=999); source('energmod_fn_1.0.r'); library(parallel) #parallel computing
setwd('/Users/ben/Documents/PhD Chapters/Ch 3 Optimisation Modelling/FINAL DATA FRAMES AND CODE')

## User select options ##
inputs <- 'input_wsg.csv' # choose input file for energy generator list and input parameters
c.price <- 100 # $ carbon price per tCO2
overbuild <- T # if overbuild == T: wind or solar are overbuilt IF first in dispatch order
opt.sel <- F # if opt.sel == T: optimisation code runs; otherwise stacked by predefined order
opt <- 100 # Search grid for wind-solar mix (100 for 1-99 [fast/rough], 1000 for 0.1-99.9 [slow/precise])
grn.incr <- 0.1 # GW increment for addition of gas or renewables

## Load input data (data files for energy options and demand/supply traces)
gen.df <- read.csv(inputs) # energy source list & attributes
trace <- read.csv("nem_trace3.csv") # Demand = 2050, wind & solar = 2003-2012

## Initialize and convert data for modelling ##
trace<-trace[,6:8]; trace$ngw<-trace$d.nem/1000; trace[,ncol(trace)+1]<-1  # Clip & convert to GW
gen.df$fuel.cost <- gen.df$fuel.cost*(3.6/gen.df$therm.effic) + gen.df$vom # 3.6 GJ per MWh heat
gen.df$lcoe <- gen.df$lcoe - gen.df$fuel.cost; rownames(gen.df)<-gen.df$X; gen.df<-gen.df[,-1]
first.en <- row.names(gen.df)[1]; names(trace) <- c('demand','wind','solar','demGW',first.en)
hr<-nrow(trace); dem<-sum(trace$demGW); max.nem<-max(trace$demGW); min.nem<-min(trace$demGW)

if(overbuild == T){ # if wind or solar is first in dispatch order, calculate capacity overbuild
  overbuild <- max(1, ifelse(row.names(gen.df)[1]=='wind', 1/as.numeric(quantile(trace$wind))[2],
                ifelse(row.names(gen.df)[1]=='solar', 1/as.numeric(quantile(trace$solar))[2],1)))
  iter <- floor(max.nem*overbuild/grn.incr)
}   else iter <- floor(max.nem/grn.incr)

# create dataframe to send all  information to main function
var.sel <- data.frame(c.price=c.price, grn.incr=grn.incr, opt=opt, hr=hr, dem=dem, 
                      max.nem=max.nem, min.nem=min.nem, overbuild=overbuild, iter=iter)

## Model Execution - call function, run parallel processing ##
ptm <- proc.time(); if(nrow(gen.df) < 4) opt.sel <- F # if number of gen<4, optimisation not invoked
cl <- makeCluster((detectCores()-1), type = "SOCK") ## make a cluster of cores
clusterExport(cl, varlist = c('fn_renopt')) 
##### --------------------Main Model----------------------------#####
result <- fn_energmod(fn.var = var.sel, fn.gen = gen.df, 
          fn.sel = opt.sel, fn.tr = trace, cl = cl) ## Call model run
##### --------------------Main Model----------------------------#####
stopCluster(cl); proc.time() - ptm # Original at opt=1000 = 1248 sec, parallel function = 556 sec

## Results - many other options possible ##
plot(result[,grep(paste0(first.en," Prop"),colnames(result))],result$av.lcoe,ylab="Av.LCOE",xlab=first.en) # $ per MWh
result[which(result$av.lcoe==min(result$av.lcoe)),] # Lowest cost mix

write.csv(result,paste0("results_100_carbon",inputs)) # save results table with filename to match scenario
