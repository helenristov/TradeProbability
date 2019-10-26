## @knitr preparation

library(ggplot2)
library(knitr)
library(plyr)

start.date  <- as.Date("2015-02-01")
end.date    <- as.Date("2015-03-15")
instruments <- c("CLU5.Z5", "EDU5.M6", "LCOU5.Z5", "LGOU5.Z5")
trades      <- data.frame()


## @knitr calculations

while(start.date <= end.date){
  message(paste("Working on", start.date))
  
  for(instrument in instruments){
    file.name <- paste0("/data/tick/", 
                        instrument, "/", 
                        format(start.date, "%Y.%m.%d"), ".", 
                        instrument, ".RData")
    
    if(file.exists(file.name)) {
      load(file.name)
      
      temp <- na.omit(get(instrument))
      
      if(nrow(temp) > 0) {
        temp                 <- data.frame(DateTime = index(temp), coredata(temp))
        
        temp$Last.Px         <- c(NA, head(temp$Price, -1))
        temp$Time.Since.Last <- c(NA, difftime(tail(temp$DateTime, -1), head(temp$DateTime, -1)))
        temp$Last.Px.Delayed <- na.locf(ifelse(temp$Time.Since.Last > 0.01, temp$Last.Px, NA), na.rm = FALSE) # 10ms delay
        temp$Last.Bid.Size   <- c(NA, head(temp$Bid.Size, -1))
        temp$Last.Ask.Size   <- c(NA, head(temp$Ask.Size, -1))
        temp$Instrument      <- instrument
        
        trades               <- rbind(trades, na.omit(temp))
      }
    }
  }
  
  start.date <- start.date + 1
}

trades$Bid.Pct                <- trades$Bid.Size / (trades$Bid.Size + trades$Ask.Size)
trades$Side                   <- ifelse(trades$Price == trades$Bid.Price, "BID", "ASK")
trades$Spread                 <- trades$Ask.Price - trades$Bid.Price
trades$Last.Px.Is.Bid         <- trades$Last.Px == trades$Bid.Price
trades$Last.Px.Is.Bid.Delayed <- trades$Last.Px.Delayed == trades$Bid.Price
trades$Inside.Size.Chg        <- (trades$Bid.Size - trades$Ask.Size) - (trades$Last.Bid.Size - trades$Last.Ask.Size)

rm(temp)


## @knitr data_visualization

trades.agg <- aggregate(Bid.Pct ~ Instrument + Side, data = trades, mean)

ggplot(trades, aes(x = Bid.Pct, fill = factor(Side))) + 
  geom_histogram(binwidth   = 0.01, 
                 alpha      = 0.5, 
                 position   = "identity", 
                 aes(y      = ..density..)) +
  facet_wrap(~ Instrument, 
             ncol           = 1, 
             scales         = "free_y") +
  geom_vline(data           = trades.agg, 
             aes(xintercept = Bid.Pct, 
                 color      = factor(Side)), 
             linetype       = "dashed", 
             size           = 1) +
  ylab("% of Trades") +
  labs(fill = "Trade Side")

# ggplot(trades, aes(x = Bid.Pct, y = Bid.Size + Ask.Size, color = factor(Side))) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~ Instrument, ncol = 1, scales = "free_y") +
#   ylab("Sqrt(Inside Size)") +
#   labs(color = "Trade Side") +
#   scale_y_sqrt()


## @knitr regression_models

train.end <- as.POSIXct("2015-02-28")

for (instrument in instruments) {
  mdl.1 <- glm(Side == "BID" ~ Bid.Pct, 
               data = trades[with(trades, Instrument == instrument & DateTime <= train.end),], 
               family = binomial())
  
  mdl.2 <- glm(Side == "BID" ~ Bid.Pct + Last.Px.Is.Bid.Delayed, 
               data = trades[with(trades, Instrument == instrument & DateTime <= train.end),], 
               family = binomial())
  
  mdl.3 <- glm(Side == "BID" ~ Bid.Pct + Last.Px.Is.Bid, 
               data = trades[with(trades, Instrument == instrument & DateTime <= train.end),], 
               family = binomial())
  
  trades$Bid.Pred.1[with(trades, DateTime > train.end & Instrument == instrument)] <- 
    predict(mdl.1, subset(trades, Instrument == instrument & DateTime > train.end), type = "response")
  
  trades$Bid.Pred.2[with(trades, DateTime > train.end & Instrument == instrument)] <- 
    predict(mdl.2, subset(trades, Instrument == instrument & DateTime > train.end), type = "response")
  
  trades$Bid.Pred.3[with(trades, DateTime > train.end & Instrument == instrument)] <- 
    predict(mdl.3, subset(trades, Instrument == instrument & DateTime > train.end), type = "response")
  
  message(paste("ANOVA Results for", instrument))
  print(anova(mdl.1, mdl.2, mdl.3, test = "Chisq"))
}

ddply(na.omit(trades), .(Instrument), summarize, 
      "Pct.Of.Volume.0"   = sum(Volume * (Time.Since.Last == 0)) / sum(Volume),
      "Pct.Of.Volume.0.1" = sum(Volume * (Time.Since.Last > 0 & Time.Since.Last < 1)) / sum(Volume),
      "Pct.Of.Volume.1"   = sum(Volume * (Time.Since.Last >= 1)) / sum(Volume))

mdl.accuracy <- ddply(na.omit(trades), 
                      .(Instrument), 
                      summarize, 
                      "WMP" = sum((Bid.Pred.1 > 0.5 & Side == "BID") | (Bid.Pred.1 < 0.5 & Side == "ASK")) / 
                        length(Side),
                      "WMP + Last Px Delayed" = sum((Bid.Pred.2 > 0.5 & Side == "BID") | (Bid.Pred.2 < 0.5 & Side == "ASK")) / 
                        length(Side),
                      "WMP + Last Px" = sum((Bid.Pred.3 > 0.5 & Side == "BID") | (Bid.Pred.3 < 0.5 & Side == "ASK")) / 
                        length(Side))

kable(mdl.accuracy, digits = 4, caption = "Out-of-Sample Prediction Accuracy")

#sum(trades$Time.Since.Last <= 0.01) / nrow(trades)

qplot(x = Inside.Size.Chg, y = Bid.Pct, data = trades, geom = "point")

# does a change in bid/ask qty mean anything?
