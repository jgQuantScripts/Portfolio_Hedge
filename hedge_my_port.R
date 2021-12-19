require("quantmod");require("PerformanceAnalytics"); require("lubridate")

# read in vector of (%) returns:
port = read.csv("RETS_20211214.csv",header=TRUE,sep=",")
# convert to xts
port = xts(as.numeric(port$PORTFOLIO), order.by = as.Date(port$Date,"%m/%d/%y"))
colnames(port) = "PORT"
port[is.na(port)] <- 0
# starting day
START = index(port)[1]
# plot Portfolio - as a reference
charts.PerformanceSummary(port, geometric = FALSE)
# copy portfolio to hedge
toHedge = port
# *****************************************************************
#               what are we trying to hedge against?
# *****************************************************************
# Systematic Risk (Risk from the "System") : SPY, QQQ, DIA
# Sector Specific (Financials, Energy, etc): XLF, XLE, XLC, XLY
# *****************************************************************
# get data for tickers
e <- new.env()
tickers = c("SPY","QQQ","DIA","XLC","XLY","XLP","XLE","XLF","XLV","XLI","XLB","XLK","XLU","XLRE")
# get data from Yahoo Finance
getSymbols(tickers, from= (START - days(1)), env = e)
# combine Adjusted Closes
rsk = do.call(merge,eapply(e,Ad))
colnames(rsk) = gsub(".Adjusted","",names(rsk))
# get Returns
rsk = na.omit(ROC(rsk, type = "discrete"))
# combine assets with portfolio
rsk = na.omit(merge(toHedge,rsk))
# correlations to the assets
View(cor(coredata(rsk)))
# hist(rsk$PORT,breaks = 10)
# **************************************************************************************************
#                                             Find Beta Specific Hedge
# **************************************************************************************************
# Test Idea First: Only want to hedge the downside hence BETA bear
hedgeSPY = CAPM.beta.bear(Ra=rsk$PORT, Rb=rsk$SPY, Rf = 0)
# Rolling average of returns the last 3-days
toHedge$AVG = rollmean(toHedge$PORT, k=3)
# Only want to hedge when my portfolio is down over the last 3 days
toHedge$sig = Lag(ifelse(toHedge$AVG < 0, -1, 0))
# add Benchmark
toHedge = na.omit(merge(toHedge,rsk$SPY))
# add hedged portfolio
toHedge$hedgedPORT= ifelse(toHedge$sig == -1,                 # if signal == -1
                           (toHedge$sig*toHedge$SPY*hedgeSPY)+
                             toHedge$PORT,                    # short SPY (BETA weighted)
                           toHedge$PORT)                      # Otherwise unhedge
# chart Performance
charts.PerformanceSummary(merge(toHedge$PORT, toHedge$hedgedPORT),geometric = FALSE)

# could it be improved by using a rolling (20-day) BETA?
toHedge$rollBETA <- rollapply(data=rsk$PORT, FUN=CAPM.beta.bear, Rb= rsk$SPY, Rf = 0, 
                            width = 20, by = 1, align = "right", by.column=TRUE)
toHedge = na.omit(toHedge)
# add hedged portfolio
toHedge$roll_hedged= ifelse(toHedge$sig == -1,                 # if signal == -1
                            (toHedge$sig*toHedge$SPY*toHedge$rollBETA)+
                              toHedge$PORT,                    # short SPY (BETA weighted)
                            toHedge$PORT)                      # Otherwise unhedge
# chart Performance
charts.PerformanceSummary(merge(toHedge$PORT, toHedge$hedgedPORT, toHedge$roll_hedged),
                          geometric = FALSE)
# trouble with unrestricted BETA -> insinuates leverage > 2x portfolio
subset(toHedge, toHedge$rollBETA > 1 & toHedge$sig== -1)
# add static BETA @ 50% portfolio value
toHedge$static_hedged= ifelse(toHedge$sig == -1,                 # if signal == -1
                              (toHedge$sig*toHedge$SPY*0.50)+
                                toHedge$PORT,                    # short SPY (BETA weighted)
                              toHedge$PORT)   
# chart Performance
charts.PerformanceSummary(merge(toHedge$PORT, toHedge$hedgedPORT, 
                                toHedge$roll_hedged,toHedge$static_hedged),
                          geometric = FALSE)
# **************************************************************************************************
#                                Function to get hedge results
# **************************************************************************************************
hedgePortfolio = function(hedge,static_hedge,downNdays){
# Test Idea First: Only want to hedge the downside hence BETA bear
hedgeBETA = CAPM.beta.bear(Ra=rsk$PORT, Rb=rsk[,hedge], Rf = 0)
# Rolling average of returns the last N-days
toHedge$AVG = rollmean(toHedge$PORT, k=downNdays)
# Only want to hedge when my portfolio is down over the last N-days
toHedge$sig = Lag(ifelse(toHedge$AVG < 0, -1, 0))
# add Benchmark
toHedge = na.omit(merge(toHedge,rsk[,hedge]))
# add hedged portfolio
toHedge$hedgedPORT= ifelse(toHedge$sig == -1,                 # if signal == -1
                           (toHedge$sig*toHedge[,hedge]*hedgeBETA)+
                             toHedge$PORT,                    # short ASSET (BETA weighted)
                           toHedge$PORT)                      # Otherwise unhedge

# could it be improved by using a rolling (20-day) BETA?
toHedge$rollBETA <- rollapply(data=rsk$PORT, FUN=CAPM.beta.bear, Rb= rsk[,hedge], Rf = 0, 
                              width = 20, by = 1, align = "right", by.column=TRUE)
toHedge = na.omit(toHedge)
# add hedged portfolio
toHedge$roll_hedged= ifelse(toHedge$sig == -1,                 # if signal == -1
                            (toHedge$sig*toHedge[,hedge]*toHedge$rollBETA)+
                              toHedge$PORT,                    # short ASSET (BETA weighted)
                            toHedge$PORT)                      # Otherwise unhedge

# add static BETA @ 50% portfolio value
toHedge$static_hedged= ifelse(toHedge$sig == -1,                 # if signal == -1
                              (toHedge$sig*toHedge[,hedge]*static_hedge)+
                                toHedge$PORT,                    # short ASSET (BETA weighted)
                              toHedge$PORT)   
# return results
toHedge
}
# **************************************************************************************************
#                                Systematic Risk - Indices
# **************************************************************************************************
# test function / clear "toHedge"
toHedge <- port
pctHedge = 0.50
downDays = 2
spyRETS = hedgePortfolio(hedge="SPY",static_hedge = pctHedge, downNdays=downDays)

# chart Performance
charts.PerformanceSummary(merge(spyRETS$PORT, spyRETS$hedgedPORT, 
                                spyRETS$roll_hedged,spyRETS$static_hedged),
                          geometric = FALSE)

qqqRETS = hedgePortfolio(hedge="QQQ",static_hedge = pctHedge, downNdays=downDays)
# chart Performance
charts.PerformanceSummary(merge(qqqRETS$PORT, qqqRETS$hedgedPORT, 
                                qqqRETS$roll_hedged,qqqRETS$static_hedged),
                          geometric = FALSE)

diaRETS = hedgePortfolio(hedge="DIA",static_hedge = pctHedge, downNdays=downDays)
# chart Performance
charts.PerformanceSummary(merge(diaRETS$PORT, diaRETS$hedgedPORT, 
                                diaRETS$roll_hedged,diaRETS$static_hedged),
                          geometric = FALSE)

# combine portfolio and different index hedges
hedgeIdx = merge(spyRETS$PORT,spyRETS$static_hedged,qqqRETS$static_hedged,diaRETS$static_hedged)
colnames(hedgeIdx) = c("Portfolio","SPY_hedge","QQQ_hedge","DIA_hedge")
charts.PerformanceSummary(hedgeIdx,geometric = FALSE)
charts.PerformanceSummary(hedgeIdx["2021"],geometric = FALSE)
# **************************************************************************************************
#                                Sector Specific Risk
# **************************************************************************************************
# test function / clear "toHedge"
pctHedge = 0.75
downDays = 3
xleRETS = hedgePortfolio(hedge="XLE",static_hedge = pctHedge, downNdays=downDays)

# chart Performance
charts.PerformanceSummary(merge(xleRETS$PORT, xleRETS$hedgedPORT, 
                                xleRETS$roll_hedged,xleRETS$static_hedged),
                          geometric = FALSE)
# test function / clear "toHedge"
xlfRETS = hedgePortfolio(hedge="XLF",static_hedge = pctHedge, downNdays=downDays)

# chart Performance
charts.PerformanceSummary(merge(xlfRETS$PORT, xlfRETS$hedgedPORT, 
                                xlfRETS$roll_hedged,xlfRETS$static_hedged),
                          geometric = FALSE)

# combine portfolio and different index hedges
hedgeIdxS = merge(xlfRETS$PORT,xlfRETS$static_hedged,xleRETS$static_hedged)
colnames(hedgeIdxS) = c("Portfolio","XLF_hedge","XLE_hedge")
charts.PerformanceSummary(hedgeIdxS,geometric = FALSE)
charts.PerformanceSummary(hedgeIdxS["2021"],geometric = FALSE)

# **************************************************************************************************
#                                Attempt to Optimize
# **************************************************************************************************
# we will split the data test/validation (walk-forward)
require("DEoptim")

# set upper and lower limits: 
LOWER = c(0.05,2)
UPPER = c(1.00,20)
# split the data to avoid overfitting
toHedge <- port
TEST = toHedge["::20201231"]
VAL  = toHedge["2021::"]
toHedge <- TEST
# Optimization Function
# optimize the percentage hedge & days to look back (for down days)
# will return the pair with the highest Sharpe
toOptim = function(n)
{
  # get results
  tmp = hedgePortfolio(hedge="QQQ",static_hedge = n[1], downNdays=n[2])
  # Sharpe Ratio 
  sharpeR = mean(tmp$static_hedged)/sd(tmp$static_hedged)
  # will seek to minimize, so flip the sign
  return(-sharpeR)
}
# function to round to whole numbers 
fnmap_f <- function(x) {
  n1 = c(round(x[1],2))
  n2 = c(round(x[2],0))
  c(n1,n2)
}
# Parallel - will use all available cores 
system.time(
  r <- DEoptim(toOptim,lower=LOWER,upper=UPPER,
               control = list(itermax=100,trace=TRUE),
               fnMap = fnmap_f)
)
# save.image("hedgePortfolio_Half.RData")
# save.image("hedgePortfolio_FULL.RData")
# load("hedgePortfolio.RData")
#    Tested 100 iterations
#      user     system elapsed 
#    3102.938   19.609 3245.247 


# reassign toHedge to get the full dataset
toHedge <- port
qqqBEST = hedgePortfolio(hedge="QQQ",static_hedge = as.numeric(r$optim$bestmem[1]),
                         downNdays=as.numeric(r$optim$bestmem[2]))
# chart Performance
toPLOT = merge(qqqBEST$PORT, qqqBEST$hedgedPORT,qqqBEST$roll_hedged,qqqBEST$static_hedged)
charts.PerformanceSummary(toPLOT["::20201231"],geometric = FALSE)
charts.PerformanceSummary(toPLOT["2021::"],geometric = FALSE)

# full
charts.PerformanceSummary(toPLOT,geometric = FALSE)





