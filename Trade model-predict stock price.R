############ Project to predict next day's closing price ############
############## for top performing Dow Jones 30 company ##############

################################################################
#Installing qunatmod package for conducting financial analysis #
################################################################
rm(list = ls())
install.packages("quantmod")
library(quantmod)

###########################################################
# Installing plotly package for interactive visualization #
###########################################################
install.packages("plotly")
library(plotly)

##################################################
# Installing dplyr package for certain functions #
##################################################
library(dplyr)
#Intial Exploration of ChartSeries

######################################
# An example of quantmod functioning #
######################################

# Loading Apple inc. data from Yahoo Finance
getSymbols("AAPL",src="yahoo", from = "2015-01-01", to = "2016-09-30")

#checking for the existance of the symbol
exists("AAPL")

#If you do not want to load the data into the curret environment
getSymbols("INTC",src="yahoo", from = "2015-01-01", to = "2016-09-30",auto.assign  = FALSE)
exists("INTC")

# Similarly other companies' data
getSymbols("NKE",src="yahoo", from = "2015-01-01", to = "2016-09-30")
getSymbols("CAT",src="google", from = "2015-01-01", to = "2016-09-30",auto.assign  = TRUE)

#Gives the list of symbols loaded into the current environment along with their source 
showSymbols(env=parent.frame())

#To remove the a specific symbol from the environment
removeSymbols(Symbols = "AAPL",env=parent.frame())

#To remove the all the symbols from the environment
removeSymbols(Symbols = NULL,env=parent.frame())

# To fetch top 5 records from company stock price data
head(NKE)
head(AAPL)

#######################################################
# To fetch closing prices of all Dow Jones 30 company #
#######################################################

#To eleminate the "e" incase of exponential data
options(scipen = 999999999)

# Loading company symbols into a list
DOWJO30=c("AAPL","AXP","BA","CAT","CSCO","CVX","KO","DD","XOM","GE","GS","HD",
          "IBM","INTC","JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG",
          "TRV","UNH","UTX","V","VZ","WMT","DIS")

# Fetching data of all Dow Jones 30 companies from Yahoo finance
getSymbols(DOWJO30,src="yahoo",from="2015-09-01",to="2016-09-30")

# Creating data frame with all the data
DOWJO30data=data.frame(merge.xts(AAPL,AXP,BA,CAT,CSCO,CVX,KO,DD,XOM,GE,GS,HD,
                                 IBM,INTC,JNJ,JPM,MCD,MMM,MRK,MSFT,NKE,PFE,PG,
                                 TRV,UNH,UTX,V,VZ,WMT,DIS))
View(DOWJO30data)

# Selecting closing price of all 30 companies
DOWJO30Close=select(DOWJO30data,contains("Close"))
View(DOWJO30Close)
DOWJO30CloseAll=rowSums(DOWJO30Close)
View(DOWJO30CloseAll)

# Creating an xts object for using in quantmod functions
DOWJO30Closexts <- xts(DOWJO30CloseAll,order.by=as.Date(rownames(DOWJO30data)))
View(DOWJO30Closexts)

############################################
# Identifying top company based on returns #
############################################

#Function for periodic returns(daily,monthly,weekly) for each company in the DOWJONES30
perReturns=function(DOWJO30data,DOWJO30,per)
{
  #create a numeric vector to store the weighted moving average of periodic returns of each company in the DOWJONES 30
  M=vector(mode="numeric")
  for (i in 1:30)
  {
    p=i
    q=i+5
    #Periodic return of One company.as.xts converts data frame in to xts object
    x=periodReturn(as.xts(DOWJO30data[,p:q]),period = per)
    #calculate weighted moving average of the company's daily return 
    z=WMA(x,n=nrow(x),wts = 1:nrow(x))
    y=subset(z,!is.na(z))
    M[i]=sum(y)
    p=q+1
  }
  #combine the weighted average with its respective symbol
  N=cbind(DOWJO30,M)
  return(N)
}

# Calculate daily, monthly and weekly returns
N=perReturns(DOWJO30data,DOWJO30,"daily")
P=perReturns(DOWJO30data,DOWJO30,"weekly")
Q=perReturns(DOWJO30data,DOWJO30,"monthly")

# converting to data frames for plotting using plotly functions
n1=data.frame(N)
n2=data.frame(Q)
n2

# plotting Monthly returns for all 30 companies to visually identify top 5 performing companies
q = plot_ly(n2, x = n2$DOWJO30, y = n2$M,
            text = ~paste("Company: ", n2$DOWJO30, 'Return:', n2$M),
            mode = "markers", type = "bar")  
x <- list(
  title = "Company",
  titlefont = F)
y <- list(
  title = "WMA",
  titlefont = F,dtick = 5)

q %>% 
  layout(title='Average Monthly Returns of DOWJONES 30 companies', 
         xaxis= x, yaxis=y)

#Obtain the top 5 companies in terms of Weighted moving average of daily returns
new=n1[order(n1$M,decreasing = T),]
top5=head(new,5)[1]
top5

#########################################################
# Building model to predict the next days closing price #
# of Intel Corporation (top performing company)         #
#########################################################

# Model built based on past 1 week's prices along with closing prices of all other 30 
# companies in terms of returns

# select the closing prices of all the companies from the all the OHLC data
DOWJO30Close=select(DOWJO30data,contains("Close"))

# Calculate the day wise sum of closing prices of all the 30 companies
DOWJO30CloseAll=rowSums(DOWJO30Close)
View(DOWJO30CloseAll)

# Convert the data fram into an xts object for modeling using quantmod package
DOWJO30Closexts <- xts(DOWJO30CloseAll,order.by=as.Date(rownames(DOWJO30data)))
View(DOWJO30Closexts)

# To formulate the next day closing price of INTC based the on past 7 days lag 
# and closing prices of all other companies
q.model1 = specifyModel(Next(Cl(INTC)) ~ lag(Cl(INTC),1:7)+DOWJO30Closexts)

# Build the model based on the above specified model for the training data
model = buildModel(q.model1,method='glm',training.per=c('2015-09-01','2016-09-01'))
model

# Fit the model for the entire datarange
x=fittedModel(model)

# Co-effecients of the model
coef(model)
vcov(fittedModel(model))

# Building trademodel
tradeModel(model,leverage = 2)

# Next days prediction formula from the coeffecients obtained
INTC_Close=-1.587504164+0.866563866*lag(Cl(INTC),1)-0.037520653*lag(Cl(INTC),2)-0.016840860 *lag(Cl(INTC),3)+0.045736388*lag(Cl(INTC),4)+0.089459435*lag(Cl(INTC),5)+0.057937349*lag(Cl(INTC),6)-0.155130184*lag(Cl(INTC),7)+0.002520283*DOWJO30Closexts

# Plotting current and next day's closing prices
lineChart(Cl(INTC),TA="addTA(INTC_Close,on=1)",theme = chartTheme("white"),up.col="red")

########################################################################################
##################################### Thank You ########################################
########################################################################################



