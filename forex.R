# Created by Kshitij Gupta:
# Description :
# I have specifically prepared this to show the dynamics of spread between Forex and correlation across asset classes. A lot of these relative value ideas are the starting points 
# to develop full blown heding strategies across asset classes includeing FOREX for risk management perspective

# 1. Reads in the multi-asset prices consisting of FOREX, Equity, Fixed Income, Commodities, Volatility from yahoo finance
# 2. Display the behavior of spread between these assets
# 3. Display the behavior of volatility across these assets
# 4. Display the correlation between assets

rm(list = ls())

packages = c("dplyr", "ggplot2",
             "tidyquant", "xts",
             "tidyr", "ggcorrplot")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

username <- "" #enter gmail address : xxx@gmail.com"
password <- "" #enter gmail password

# library(dplyr)
# library(ggplot2)
# library(tidyquant)
# library(tidyr)
# library(ggcorrplot)

plotCumSpread <- function(symbol1, symbol2)
{
  multassetlist <- c("JPYUSD=X",
                     "GBPUSD=X",
                     "AUDUSD=X",
                     "CADUSD=X",
                     "CHFUSD=X",
                     "EURUSD=X",
                     "JPYEUR=X",
                     "GLD",
                     "^VIX",
                     "^GSPC",
                     "AGG"
  )
  
  etfnameslist <- c(
    "JPY/USD",
    "GBP/USD",
    "AUD/USD",
    "CAD/USD",
    "CHF/USD",
    "EUR/USD",
    "Gold",
    "Vix Index",
    "SP500 Index",
    "iShares Core U.S. Aggregate Bond ETF"
  )
  
  myetfnameslist <- etfnameslist
  
  price_data <- tq_get(multassetlist,
                       from = '2003-01-01',
                       to = '2021-04-02',
                       get = 'stock.prices')
  
  # browser()
  
  instruments <- tibble(data.frame(cbind(multassetlist, myetfnameslist), stringsAsFactors = FALSE))
  colnames(instruments) <- c("symbol", "description")
  
  ###--------adjusted closeprices-------------------
  tmpPrices <- price_data %>% select(c(date, symbol, adjusted)) %>% tidyr::spread(symbol, value = adjusted)
  
  ###--------analysis---------------------
  
  # symbol1 <- "SPYG_E"
  # symbol2 <- "SPYV_E"
  
  # select the relevant columns from the tibble
  # tmpPrices %>% select(SPYV_E, SPYG_E) 
  tmpPrices_Select <- tmpPrices %>% select(date, symbol1, symbol2) %>% tidyr::drop_na()
  
  calcLogReturns <- function(x)
  {
    return(100*(log(x) - log(lag(x))))
  }
  
  tmpReturns_Select <- tmpPrices_Select %>% mutate(across(c(symbol1,symbol2), calcLogReturns))
  
  # https://stackoverflow.com/questions/29678435/how-to-pass-dynamic-column-names-in-dplyr-into-custom-function
  
  tmpReturns_Select <- tmpReturns_Select %>% mutate(Spread = (!!as.name(symbol1)) - (!!as.name(symbol2)))
  
  tmpReturns_Select <- tmpReturns_Select %>% mutate(Roll_Sum_5 = zoo::rollapply(Spread, 5, sum, fill=NA, align = "right", partial=F)) %>% mutate(Roll_Sum_20 = zoo::rollapply(Spread, 20, sum, fill=NA, align = "right", partial=F)) %>% mutate(Roll_Sum_250 = zoo::rollapply(Spread, 250, sum, fill=NA, align = "right", partial=F))%>% mutate(Roll_Sum_500 = zoo::rollapply(Spread, 500, sum, fill=NA, align = "right", partial=F))
  
  ###----plot-------------------------------------
  dorollingplot <- FALSE
  if (dorollingplot)
  {
    ggplot(tail(tmpReturns_Select,500), aes(x=date)) + 
      geom_line(aes(y = Roll_Sum_5), color = "darkred") + 
      geom_line(aes(y = Roll_Sum_20), color="steelblue", linetype="twodash")+
      geom_line(aes(y = Roll_Sum_250), color="darkgreen") +
      geom_line(aes(y = Roll_Sum_500), color="black")
  }
  
  
  #-----cumsum-----------------------------------
  tmpReturns_Select <- tmpReturns_Select %>% tidyr::replace_na(list(Spread = 0)) %>% mutate(cumSpread = cumsum(Spread))
  
  g1 <- ggplot(tmpReturns_Select, aes(x=date)) + 
    geom_line(aes(y = cumSpread), color = "darkred") +
    ggtitle(paste0("Spread of ", symbol1, " - ", symbol2)) + 
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year",
                 date_labels = "%Y")
  
  
  #-----Sum of Spread By Year-----------------------
  tmp <- tmpReturns_Select
  tmp$date <- lubridate::floor_date(tmp$date, "year")
  tmp <- tmp %>% group_by(date) %>% summarize(Spread = sum(Spread))
  g2 <- ggplot(data = tmp, aes(x = date, y = Spread)) + geom_bar(stat = "identity")+ ggtitle(paste0("Spread of ", symbol1, " - ", symbol2)) + scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + ggtitle(paste0("Spread of ", symbol1, " - ", symbol2))
  
  
  #-----Sum of individual performance By Year-----------
  # browser()
  tmp <- tmpReturns_Select
  tmp$date <- lubridate::floor_date(tmp$date, "year")
  tmp <- tmp %>% group_by(date) %>% summarize(symbol1 = sum(!!as.name(symbol1)), symbol2 = sum(!!as.name(symbol2)), Spread = sum(Spread))
  colnames(tmp) <- c("date", symbol1, symbol2, "Spread")
  
  # https://community.rstudio.com/t/about-creating-multi-variable-bar-chart-using-ggplot/18859/6
  # https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
  df <- tmp %>% tidyr::gather(keys, values, as.name(symbol1):as.name(symbol2))
  g3 <- ggplot(df, aes(fill = keys, x = date, y = values)) +
    geom_bar(position = "dodge", stat = "identity") + scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + ggtitle(paste0("Spread of ", symbol1, " - ", symbol2)) + theme(legend.position="bottom")
  # g1
  # g2
  
  return(list("plots" = list(g1, g2, g3), "tablez" = tmp, "instruments" = instruments))
}

plotCorrelation <- function()
{
  multassetlist <- c("JPYUSD=X",
                     "GBPUSD=X",
                     "AUDUSD=X",
                     "CADUSD=X",
                     "CHFUSD=X",
                     "EURUSD=X",
                     "JPYEUR=X",
                     "GLD",
                     "^VIX",
                     "^GSPC",
                     "AGG"
  )
  
  etfnameslist <- c(
    "JPY/USD",
    "GBP/USD",
    "AUD/USD",
    "CAD/USD",
    "CHF/USD",
    "EUR/USD",
    "Gold",
    "Vix Index",
    "SP500 Index",
    "iShares Core U.S. Aggregate Bond ETF"
  )
  
  myetfnameslist <- etfnameslist
  
  price_data <- tq_get(multassetlist,
                       from = '2003-01-01',
                       to = '2021-04-02',
                       get = 'stock.prices')
  
  
  
  instruments <- tibble(data.frame(cbind(multassetlist, myetfnameslist), stringsAsFactors = FALSE))
  colnames(instruments) <- c("symbol", "description")
  
  
  ###--------adjusted closeprices-------------------
  tmpPrices <- price_data %>% select(c(date, symbol, adjusted)) %>% tidyr::spread(symbol, value = adjusted)
  
  ###--------analysis---------------------
  
  tmpPrices_Select <- tmpPrices %>% tidyr::drop_na()
  
  calcLogReturns <- function(x)
  {
    return(100*(log(x) - log(lag(x))))
  }
  
  tmpReturns_Select <- tmpPrices_Select %>% mutate(across(colnames(tmpPrices_Select)[2:length(colnames(tmpPrices_Select))], calcLogReturns))
  tmpVol <- tmpReturns_Select %>% mutate(across(c(colnames(tmpReturns_Select)[2:length(colnames(tmpReturns_Select))]), function(x) zoo::rollapply(x, 20, sd, fill=NA, align = "right", partial=F)))
  
  
  # browser()
  
  g1 <- ggcorrplot::ggcorrplot(cor(tmpReturns_Select[,colnames(tmpPrices_Select)[2:length(colnames(tmpPrices_Select))]], use = "pairwise.complete.obs")) + ggtitle(paste0("Correlation Across Assets"))
  
  return(list("plots" = list(g1)))
}


corrplot <- plotCorrelation()

spreadList<- list(list("JPYUSD=X", "EURUSD=X"),
                  list("JPYUSD=X", "GBPUSD=X"),
                  list("JPYUSD=X", "CADUSD=X"),
                  list("JPYUSD=X", "AUDUSD=X"),
                  list("JPYUSD=X", "CHFUSD=X")
                  # list("EEM_E", "SPY_E"),
                  # list("EFA_E", "SPY_E"),
                  # list("SPY_E", "AGG_E"),
                  # list("EEM_E", "EFA_E"),
                  # list("GLD_E", "SPY_E"),
                  # list("GLD_E", "AGG_E"),
                  # list("IWC_E", "IWM_E"),
                  # list("QQQ_E", "SPY_E"),
                  # list("QQQ_E", "IWO_E"),
                  # list("QQQ_E", "SPYG_E"),
                  # list("VNQ_E", "AGG_E"),
                  # list("BLV_E", "AGG_E"),
                  # list("HYG_E", "AGG_E")
                  )
# browser()
glist <- list()
ll <- list()
for (l in c(1:length(spreadList)))
{
  ll[[l]] <- plotCumSpread(spreadList[[l]][[1]],spreadList[[l]][[2]])
  glist[[l]] <- ll[[l]][["plots"]]
}

tlist <- list()
for (l in c(1:length(spreadList)))
{
  tlist[[l]] <- ll[[l]][["tablez"]]
}

# browser()

##save al the plots ---------------------------
# https://stackoverflow.com/questions/12234248/printing-multiple-ggplots-into-a-single-pdf-multiple-plots-per-page

library(grid)
library(gridExtra)
p <- c(corrplot,glist)
pdf("./Results/forexplots.pdf", onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[[i]])  
}
dev.off()

t <- tlist
pdf("./Results/assetsdesciption.pdf", onefile = TRUE)
tt <- ttheme_default(base_size = 3)
grid.table(ll[[l]][["instruments"]], rows = NULL, theme = tt)
# g <- tableGrob(ll[[l]][["instruments"]])
# grid.arrange(g)
grid.newpage()
for (i in seq(length(t))) {
  # do.call("grid.table", t[[i]])
  
  # g <- tableGrob(t[[i]])
  grid.table(t[[i]], rows = NULL)
  # grid.arrange(g)
  grid.newpage()
}
dev.off()

# browser()
plots <- paste0("./Results/",paste0("forexplots"),".pdf")
tables <- paste0("./Results/",paste0("assetsdesciption"),".pdf")
plotvec <- c(plots, tables)

if (username != "")
{
  ctrMail <- 0
  emailname <- paste0("Multi Asset Comparision Report : ")
  sender <- username
  recipients <- username
  
  # browser()
  
  # bodylist <- knitr::kable(plotdatalist[[1]], format = "html") %>% kableExtra::column_spec(1:4, border_right = TRUE, border_left = TRUE) %>%
  #   # kableExtra::row_spec(1, extra_css = "border-bottom: 1px solid") %>%
  #   kableExtra::kable_styling(full_width = FALSE)
  
  bodylist <- "FOREX Comparison Report"
  
  
    
  subject <- paste0(as.character(ctrMail <<- ctrMail + 1),"..","", emailname, " ", Sys.time())
  mailR::send.mail(from = sender,
                   to = recipients,
                   subject = subject,
                   # body = knitr::kable(plotdatalist[[1]], format = "html"), html = TRUE,
                   body = bodylist, html = TRUE,
                   smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = username,
                               passwd = password, ssl = TRUE),
                   attach.files = plotvec,
                   authenticate = TRUE,
                   send = TRUE)
  
}else
{
  print("Reports are in result folder. Enter Username and Password in the script to email reports to self!")
}