# 20210310 : This is an attempt to read in the multiasset numbers and plot them in gg plot format
rm(list = ls())
library(dplyr)
library(ggplot2)

plotCumSpread <- function(symbol1 = "SPYG_E", symbol2 = "SPYV_E")
{
  multassetlist <- c("IWS_E",
                     "IWP_E",
                     "IWR_E",
                     "IWB_E",
                     "ACWI_E",
                     "QQQ_E",
                     "IWC_E",
                     "IWM_E",
                     "IWV_E",
                     "SMMD_E",
                     "IWL_E",
                     "IWN_E",
                     "IWO_E",
                     "IWY_E",
                     "IWX_E",
                     "IWF_E",
                     "IWD_E",
                     "SPY_E",
                     "SPYV_E",
                     "SPYG_E",
                     "AGG_E",
                     "EEM_E",
                     "EFA_E",
                     "DJP_E",
                     "GSG_E",
                     "VNQ_E",
                     "HYG_E",
                     "TIP_E",
                     "EMB_E",
                     "BNDX_E",
                     "BLV_E",
                     "BSV_E",
                     "FXI_E",
                     "ECNS_E",
                     "GLD_E",
                     "DIA_E"
  )
  
  etfnameslist <- c(
    "iShares Russell Mid-Cap Value ETF",
    "iShares Russell Mid-Cap Growth ETF",
    "iShares Russell Mid-Cap ETF",
    "iShares Russell 1000 ETF",
    "iShares MSCI ACWI ETF",
    "Invesco QQQ Trust",
    "iShares Micro-Cap ETF",
    "iShares Russell 2000 ETF",
    "iShares Russell 3000 ETF",
    "iShares Russell 2500 ETF",
    "iShares Russell Top 200 ETF",
    "iShares Russell 2000 Value ETF",
    "iShares Russell 2000 Growth ETF",
    "iShares Russell Top 200 Growth ETF",
    "iShares Russell Top 200 Value ETF",
    "iShares Russell 1000 Growth ETF",
    "iShares Russell 1000 Value ETF",
    "SPDR S&P 500",
    "SPDR Portfolio S&P 500 Value ETF",
    "SPDR Portfolio S&P 500 Growth ETF",
    "iShares Core U.S. Aggregate Bond ETF",
    "iShares MSCI Emerging Markets ETF",
    "iShares MSCI EAFE ETF",
    "iPath Bloomberg Commodity Index Total Return(SM) ETN",
    "iShares S&P GSCI Commodity-Indexed Trust",
    "Vanguard Real Estate Index Fund ETF Shares",
    "iShares iBoxx $ High Yield Corporate Bond ETF",
    "iShares TIPS Bond ETF",
    "iShares J.P. Morgan USD Emerging Markets Bond ETF",
    "Vanguard Total International Bond Index Fund ETF Shares",
    "Vanguard Long-Term Bond Index Fund ETF Shares",
    "Vanguard Short-Term Bond Index Fund ETF Shares",
    "iShares China Large-Cap ETF",
    "iShares MSCI China Small-Cap ETF",
    "SPDR Gold Shares",
    "SPDR Dow Jones Industrial Average ETF Trust"
  )
  
  myetfnameslist <- c(
    "Russell Mid-Cap Value",
    "Russell Mid-Cap Growth",
    "Russell Mid-Cap",
    "Russell 1000",
    "MSCI ACWI",
    "NASDAQ QQQ",
    "Micro-Cap",
    "Russell 2000",
    "Russell 3000",
    "Russell 2500",
    "Russell Top 200",
    "Russell 2000 Value",
    "Russell 2000 Growth",
    "Russell Top 200 Growth",
    "Russell Top 200 Value",
    "Russell 1000 Growth",
    "Russell 1000 Value",
    "S&P 500",
    "S&P 500 Value",
    "S&P 500 Growth",
    "Core U.S. Aggregate Bond",
    "MSCI Emerging Markets",
    "MSCI EAFE",
    "Bloomberg Commodity Index Total Return",
    "S&P GSCI Commodity-Indexed",
    "Real Estate Index Fund",
    "High Yield Corporate Bond",
    "TIPS Bond",
    "J.P. Morgan USD Emerging Markets Bond",
    "Total International Bond",
    "Long-Term Bond",
    "Short-Term Bond",
    "China Large-Cap",
    "MSCI China Small-Cap",
    "SPDR Gold",
    "SPDR Dow Jones Index"
  )
  
  instruments <- tibble(data.frame(cbind(multassetlist, myetfnameslist), stringsAsFactors = FALSE))
  colnames(instruments) <- c("symbol", "description")
  
  symdat <- list()
  for (i in multassetlist)
  {
    tmpdat <- chopin::loaddaily(i,path = c("C:/Norgate/Binary/Equities","C:/Norgate/Binary/Futures/"))
    symdat[[i]] <- tidyr::as_tibble(tmpdat)
  }
  
  ###-------percent returns---------------
  browser()
  tmp <- symdat %>% purrr::map(select, c(tradedate, percentReturn)) %>% purrr::reduce(left_join, by = "tradedate")
  colnames(tmp) <- c("tradedate", multassetlist)
  
  tmp <- tmp %>% mutate(tradedate = as.Date(as.character(tradedate), "%Y%m%d"))
  
  ###--------closeprices-------------------
  tmpPrices <- symdat %>% purrr::map(select, c(tradedate, close)) %>% purrr::reduce(left_join, by = "tradedate")
  colnames(tmpPrices) <- c("tradedate", multassetlist)
  
  tmpPrices <- tmpPrices %>% mutate(tradedate = as.Date(as.character(tradedate), "%Y%m%d"))
  
  ###--------analysis---------------------
  
  # symbol1 <- "SPYG_E"
  # symbol2 <- "SPYV_E"
  
  # select the relevant columns from the tibble
  # tmpPrices %>% select(SPYV_E, SPYG_E) 
  tmpPrices_Select <- tmpPrices %>% select(tradedate, symbol1, symbol2) %>% tidyr::drop_na()
  
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
    ggplot(tail(tmpReturns_Select,500), aes(x=tradedate)) + 
      geom_line(aes(y = Roll_Sum_5), color = "darkred") + 
      geom_line(aes(y = Roll_Sum_20), color="steelblue", linetype="twodash")+
      geom_line(aes(y = Roll_Sum_250), color="darkgreen") +
      geom_line(aes(y = Roll_Sum_500), color="black")
  }
  
  
  #-----cumsum-----------------------------------
  tmpReturns_Select <- tmpReturns_Select %>% tidyr::replace_na(list(Spread = 0)) %>% mutate(cumSpread = cumsum(Spread))
  
  g1 <- ggplot(tmpReturns_Select, aes(x=tradedate)) + 
    geom_line(aes(y = cumSpread), color = "darkred") +
    ggtitle(paste0("Spread of ", symbol1, " - ", symbol2)) + 
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year",
                 date_labels = "%Y")
  
  
  #-----Sum of Spread By Year-----------------------
  tmp <- tmpReturns_Select
  tmp$tradedate <- lubridate::floor_date(tmp$tradedate, "year")
  tmp <- tmp %>% group_by(tradedate) %>% summarize(Spread = sum(Spread))
  g2 <- ggplot(data = tmp, aes(x = tradedate, y = Spread)) + geom_bar(stat = "identity")+ ggtitle(paste0("Spread of ", symbol1, " - ", symbol2)) + scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + ggtitle(paste0("Spread of ", symbol1, " - ", symbol2))
  
  
  #-----Sum of individual performance By Year-----------
  # browser()
  tmp <- tmpReturns_Select
  tmp$tradedate <- lubridate::floor_date(tmp$tradedate, "year")
  tmp <- tmp %>% group_by(tradedate) %>% summarize(symbol1 = sum(!!as.name(symbol1)), symbol2 = sum(!!as.name(symbol2)), Spread = sum(Spread))
  colnames(tmp) <- c("tradedate", symbol1, symbol2, "Spread")
  
  # https://community.rstudio.com/t/about-creating-multi-variable-bar-chart-using-ggplot/18859/6
  # https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
  df <- tmp %>% tidyr::gather(keys, values, as.name(symbol1):as.name(symbol2))
  g3 <- ggplot(df, aes(fill = keys, x = tradedate, y = values)) +
    geom_bar(position = "dodge", stat = "identity") + scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + ggtitle(paste0("Spread of ", symbol1, " - ", symbol2)) + theme(legend.position="bottom")
  # g1
  # g2
  
  return(list("plots" = list(g1, g2, g3), "tablez" = tmp, "instruments" = instruments))
}

spreadList<- list(list("IWM_E", "SPY_E"),
                  list("IWM_E", "IWB_E"),
                  list("SPYG_E", "SPYV_E"),
                  list("IWO_E", "IWN_E"),
                  list("EEM_E", "SPY_E"),
                  list("EFA_E", "SPY_E"),
                  list("SPY_E", "AGG_E"),
                  list("EEM_E", "EFA_E"),
                  list("GLD_E", "SPY_E"),
                  list("GLD_E", "AGG_E"),
                  list("IWC_E", "IWM_E"),
                  list("QQQ_E", "SPY_E"),
                  list("QQQ_E", "IWO_E"),
                  list("QQQ_E", "SPYG_E"),
                  list("VNQ_E", "AGG_E"),
                  list("BLV_E", "AGG_E"),
                  list("HYG_E", "AGG_E"))
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
p <- glist
pdf("~/R/emailplots/plots.pdf", onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[[i]])  
}
dev.off()

t <- tlist
pdf("~/R/emailplots/tables.pdf", onefile = TRUE)
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
plots <- paste0("~/R/emailplots/",paste0("plots"),".pdf")
tables <- paste0("~/R/emailplots/",paste0("tables"),".pdf")
plotvec <- c(plots, tables)

ctrMail <- 0
emailname <- paste0("Multi Asset Comparision Report : ")
sender <- "botgupta1234@gmail.com"
recipients <- "botgupta1234@gmail.com"

# browser()

# bodylist <- knitr::kable(plotdatalist[[1]], format = "html") %>% kableExtra::column_spec(1:4, border_right = TRUE, border_left = TRUE) %>%
#   # kableExtra::row_spec(1, extra_css = "border-bottom: 1px solid") %>%
#   kableExtra::kable_styling(full_width = FALSE)

bodylist <- "Multi Asset Comparison Report"
username <- "" #enter gmail address : xxx@gmail.com"
password <- "" #enter gmail password

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


# glist1 <- plotCumSpread("IWM_E", "SPY_E") # Russell 2000 vs SP500 # Small vs Large
# glist2 <- plotCumSpread("IWM_E", "IWB_E") # Russell 2000 vs Russell 1000 # Small vs Large
# glist3 <- plotCumSpread("SPYG_E", "SPYV_E") # SP500 Growth vs SP500 Value # Large Growth vs Large Value
# glist4 <- plotCumSpread("IWO_E", "IWN_E") # russell 2000 growth vs russell 2000 value # Small Growth vs Small Value
# glist5 <- plotCumSpread("EEM_E", "SPY_E") # 
# glist6 <- plotCumSpread("EFA_E", "SPY_E") #
# glist7 <- plotCumSpread("SPY_E", "AGG_E") #
# glist8 <- plotCumSpread("EEM_E", "EFA_E") #
# glist9 <- plotCumSpread("GLD_E", "SPY_E") #
# glist10 <- plotCumSpread("GLD_E", "AGG_E") #
# glist11 <- plotCumSpread("IWC_E", "IWM_E") #
# glist12 <- plotCumSpread("QQQ_E", "SPY_E") #
# glist13 <- plotCumSpread("QQQ_E", "IWO_E") #
# glist14 <- plotCumSpread("QQQ_E", "SPYG_E") #
# glist15 <- plotCumSpread("VNQ_E", "AGG_E") #
# glist16 <- plotCumSpread("BLV_E", "AGG_E") #
# glist17 <- plotCumSpread("HYG_E", "AGG_E") #