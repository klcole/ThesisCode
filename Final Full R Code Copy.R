library(data.table)
library(dplyr)
library(ggplot2)
library(hms)
library(readr)



#order volume
stockcodes <-
  c(11234, 12059, 12534, 1769, 2050, 2602, 3161, 3459, 3879, 5836)
days <- c(17, 18, 19, 20, 21, 24, 25, 26, 27, 28)
for (stockcode in stockcodes) {
  for (day in days) {
    #Read in data files
    Bid <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Bid_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    Offer <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Offer_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    HFTRuns <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/StrategicRunsAlternative/HFTRuns_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          RunEnd = col_time(format = "%H:%M:%OS"),
          RunStart = col_time(format = "%H:%M:%OS"),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    #Bid and Offer Volumes
    BidVolume <- nrow(Bid)
    OfferVolume <- nrow(Offer)
    
    #Create Subsets
    Bid.HFT <- subset(Bid, OrderCode %in% HFTRuns$OrderCode)
    
    Offer.HFT <- subset(Offer, OrderCode %in% HFTRuns$OrderCode)
    
    Bid.NT <- subset(Bid,!OrderCode %in% HFTRuns$OrderCode)
    
    Offer.NT <- subset(Offer,!OrderCode %in% HFTRuns$OrderCode)
    
    #Join HFT and NT orders
    HFTOrders <-
      full_join(
        Bid.HFT,
        Offer.HFT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    NTOrders <-
      full_join(
        Bid.NT,
        Offer.NT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    #Total Order Volume by Trader Type
    OrderVolumeHFT <- nrow(HFTOrders)
    OrderVolumeNT <- nrow(NTOrders)
    TotalVolume <- OrderVolumeHFT + OrderVolumeNT
    
    #Binding Values by Column
    order.volume <-
      cbind(OrderVolumeHFT,
            OrderVolumeNT,
            BidVolume,
            OfferVolume,
            TotalVolume)
    
    #save to csv
    write.table(
      order.volume,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Order Volume/OrderVolume_",
        stockcode,
        "_",
        day,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE,
      col.names = FALSE
    )
  }
}

for (stockcode in stockcodes) {
  #List file paths for Order Volume files
  all.ov.files <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Order Volume",
        sep = ""
      ),
      full = TRUE
    )
  
  #Read in all Files
  all.ov.data <- lapply(all.ov.files,  read.csv, header = FALSE)
  
  #Combine all files by row
  df <- do.call("rbind", all.ov.data)
  
  #Calculate ten-day totals
  totalHFT <- sum(df[, 1])
  totalNT <- sum(df[, 2])
  totalBid <- sum(df[, 3])
  totalOffer <- sum(df[, 4])
  total <- sum(df[, 5])
  
  #combine ten-day totals into vector
  Total <- c(totalHFT, totalNT, totalBid, totalOffer, total)
  
  #combine by row totals to full daily data frame
  df <- rbind(df,  Total)
  
  #assign row names
  rownames(df) <-
    c(
      "Nov. 17, 2014",
      "Nov. 18, 2014",
      "Nov. 19, 2014",
      "Nov. 20, 2014",
      "Nov. 21, 2014",
      "Nov. 24, 2014",
      "Nov. 25, 2014",
      "Nov. 26, 2014",
      "Nov. 27, 2014",
      "Nov. 28, 2014",
      "Total"
    )
  
  #assign column names
  colnames(df) <-
    c("Order Volume HFT",
      "Order Volume NT",
      "Bid Volume",
      "Offer Volume",
      "Total Volume")
  
  #write table as csv file
  write.table(
    df,
    file = paste(
      "~/Documents/Thesis/Thesis Results/",
      stockcode,
      "/OrderVolume_",
      stockcode,
      ".csv",
      sep = ""
    ),
    sep = ",",
    row.names = TRUE
  )
  
}





#Calculate Ratios for all stocks
stockcodes <-
  c(11234, 12059, 12534, 1769, 2050, 2602, 3161, 3459, 3879, 5836)
days <- c(17, 18, 19, 20, 21, 24, 25, 26, 27, 28)
for (stockcode in stockcodes) {
  for (day in days) {
    #read in bid, offer, and hft runs files
    Bid <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Bid_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    Offer <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Offer_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    HFTRuns <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/StrategicRunsAlternative/HFTRuns_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          RunEnd = col_time(format = "%H:%M:%OS"),
          RunStart = col_time(format = "%H:%M:%OS"),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    
    #Filter out zero prices
    Bid <- Bid %>%
      filter(Price != 0)
    
    Offer <- Offer %>%
      filter(Price != 0)
    
    
    
    #Create Subsets
    Bid.HFT <- subset(Bid, OrderCode %in% HFTRuns$OrderCode)
    
    Offer.HFT <- subset(Offer, OrderCode %in% HFTRuns$OrderCode)
    
    Bid.NT <- subset(Bid,!OrderCode %in% HFTRuns$OrderCode)
    
    Offer.NT <- subset(Offer,!OrderCode %in% HFTRuns$OrderCode)
    
    
    
    #Merge HFT Bid/Offer and NT Bid/Offer
    HFTOrders <-
      full_join(
        Bid.HFT,
        Offer.HFT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    NTOrders <-
      full_join(
        Bid.NT,
        Offer.NT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    #Order Submission Ratio
    SubmittedOrdersHFT <-
      nrow(HFTOrders) / (nrow(HFTOrders) + nrow(NTOrders))
    
    SubmittedOrdersNT <-
      nrow(NTOrders) / (nrow(HFTOrders) + nrow(NTOrders))
    
    #Order-to-Trade Ratio: q/t
    q.HFT <- nrow(HFTOrders)
    t.HFT <-
      nrow(subset(HFTOrders, OrderActionType == "M" |
                    OrderActionType == "P"))
    
    OrderToTradeHFT <- q.HFT / t.HFT
    
    q.NT <- nrow(NTOrders)
    t.NT <-
      nrow(subset(NTOrders, OrderActionType == "M" |
                    OrderActionType == "P"))
    
    OrderToTradeNT <- q.NT / t.NT
    
    #Calculate Duration
    MedianDurationHFT <-
      median(as.numeric(
        difftime(time1 = HFTOrders$TimeAction, time2 = HFTOrders$TimeAdded)))
    MeanDurationHFT <-
      mean(as.numeric(
        difftime(time1 = HFTOrders$TimeAction, time2 = HFTOrders$TimeAdded)))
    StDevDurationHFT <-
      sd(as.numeric(
        difftime(time1 = HFTOrders$TimeAction, time2 = HFTOrders$TimeAdded)))
    
    MedianDurationNT <-
      median(as.numeric(
        difftime(time1 = NTOrders$TimeAction, time2 = NTOrders$TimeAdded)))
    MeanDurationNT <-
      mean(as.numeric(
        difftime(time1 = NTOrders$TimeAction, time2 = NTOrders$TimeAdded)))
    StDevDurationNT <-
      sd(as.numeric(
        difftime(time1 = NTOrders$TimeAction, time2 = NTOrders$TimeAdded)))
    
    #Filter Matched orders
    MatchedHFT <- HFTOrders %>%
      filter(OrderActionType == "M")
    
    
    MatchedNT <- NTOrders %>%
      filter(OrderActionType == "M")
    
    #Percent of HFT resp. NT Best Price Orders that are matched
    MatchRateHFT <- (nrow(MatchedHFT) / nrow(HFTOrders)) * 100
    
    MatchRateNT <- (nrow(MatchedNT) / nrow(NTOrders)) * 100
    
    #Match duration
    MatchMeanDurationHFT <-
      mean(as.numeric(
        difftime(time1 = MatchedHFT$TimeAction, time2 = MatchedHFT$TimeAdded)
      ))
    MatchMedianDurationHFT <-
      median(as.numeric(
        difftime(time1 = MatchedHFT$TimeAction, time2 = MatchedHFT$TimeAdded)
      ))
    StDevMatchDurationHFT <-
      sd(as.numeric(
        difftime(time1 = MatchedHFT$TimeAction, time2 = MatchedHFT$TimeAdded)
      ))
    
    MatchMeanDurationNT <-
      mean(as.numeric(
        difftime(time1 = MatchedNT$TimeAction, time2 = MatchedNT$TimeAdded)
      ))
    MatchMedianDurationNT <-
      median(as.numeric(
        difftime(time1 = MatchedNT$TimeAction, time2 = MatchedNT$TimeAdded)
      ))
    StDevMatchDurationNT <-
      sd(as.numeric(
        difftime(time1 = MatchedNT$TimeAction, time2 = MatchedNT$TimeAdded)
      ))
    
    
    #Filter Order subsets for deleted orders (OrderActionType == "D")
    DeletedHFT <- HFTOrders %>%
      filter(OrderActionType == "D")
    
    
    DeletedNT <- NTOrders %>%
      filter(OrderActionType == "D")
    
    
    #Percent of Orders Deleted
    DeleteRateHFT <- (nrow(DeletedHFT) / nrow(HFTOrders))
    
    DeleteRateNT <- (nrow(DeletedNT) / nrow(NTOrders))
    
    #Delete Duration
    DeleteMeanDurationHFT <-
      mean(as.numeric(
        difftime(time1 = DeletedHFT$TimeAction, time2 = DeletedHFT$TimeAdded)
      ))
    DeleteMedianDurationHFT <-
      median(as.numeric(
        difftime(time1 = DeletedHFT$TimeAction, time2 = DeletedHFT$TimeAdded)
      ))
    StDevDeleteDurationHFT <-
      sd(as.numeric(
        difftime(time1 = DeletedHFT$TimeAction, time2 = DeletedHFT$TimeAdded)
      ))
    
    DeleteMeanDurationNT <-
      mean(as.numeric(
        difftime(time1 = DeletedNT$TimeAction, time2 = DeletedNT$TimeAdded)
      ))
    DeleteMedianDurationNT <-
      median(as.numeric(
        difftime(time1 = DeletedNT$TimeAction, time2 = DeletedNT$TimeAdded)
      ))
    StDevDeleteDurationNT <-
      sd(as.numeric(
        difftime(time1 = DeletedNT$TimeAction, time2 = DeletedNT$TimeAdded)
      ))
    
    
    #Combine all results
    Results <-
      rbind(
        SubmittedOrdersHFT,
        SubmittedOrdersNT,
        OrderToTradeHFT,
        OrderToTradeNT,
        MedianDurationHFT,
        MeanDurationHFT,
        StDevDurationHFT,
        MedianDurationNT,
        MeanDurationNT,
        StDevDurationNT,
        MatchRateHFT,
        MatchRateNT,
        MatchMedianDurationHFT,
        MatchMeanDurationHFT,
        StDevMatchDurationHFT,
        MatchMedianDurationNT,
        MatchMeanDurationNT,
        StDevMatchDurationNT,
        DeleteRateHFT,
        DeleteRateNT,
        DeleteMedianDurationHFT,
        DeleteMeanDurationHFT,
        StDevDeleteDurationHFT,
        DeleteMedianDurationNT,
        DeleteMeanDurationNT,
        StDevDeleteDurationNT
      )
    
    #Create New File
    write.table(
      Results,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Ratio Results/DailyRatioTable_",
        stockcode,
        "_",
        day,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE,
      col.names = paste("Nov. ", day, ", 2014", sep = "")
    )
    
  }
}


#Combine Daily Results Table for each stock and write to file
#stockcodes <- c(11234, 12059, 12534, 1769, 2050, 2602, 3161, 3459, 3879, 5836)
for (stockcode in stockcodes) {
  all.the.files <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Ratio Results",
        sep = ""
      ),
      full = TRUE
    )
  
  all.the.data <- lapply(all.the.files,  read.csv, header = TRUE)
  
  df <- do.call("cbind", all.the.data)
  
  colnames(df) <-
    c(
      "Nov. 17, 2014",
      "Nov. 18, 2014",
      "Nov. 19, 2014",
      "Nov. 20, 2014",
      "Nov. 21, 2014",
      "Nov. 24, 2014",
      "Nov. 25, 2014",
      "Nov. 26, 2014",
      "Nov. 27, 2014",
      "Nov. 28, 2014"
    )
  
  write.csv(
    df,
    file = paste(
      "~/Documents/Thesis/Thesis Results/",
      stockcode,
      "/ResultsTable",
      stockcode,
      ".csv",
      sep = ""
    ),
    row.names = c(
      "SubmittedOrdersHFT",
      "SubmittedOrdersNT",
      "OrderToTradeHFT",
      "OrderToTradeNT",
      "MedianDurationHFT",
      "MeanDurationHFT",
      "StDevDurationHFT",
      "MedianDurationNT",
      "MeanDurationNT",
      "StDevDurationNT",
      "MatchPercentageHFT",
      "MatchPercentageNT",
      "MatchMedianDurationHFT",
      "MatchMeanDurationHFT",
      "StDevMatchDurationHFT",
      "MatchMedianDurationNT",
      "MatchMeanDurationNT",
      "StDevMatchDurationNT",
      "DeletePercentageHFT",
      "DeletePercentageNT",
      "DeleteMedianDurationHFT",
      "DeleteMeanDurationHFT",
      "StDevDeleteDurationHFT",
      "DeleteMedianDurationNT",
      "DeleteMeanDurationNT",
      "StDevDeleteDurationNT"
    )
  )
  
}



#Plot Order Prices for HFT and NT
#stockcodes <- c(11234, 12059, 12534, 1769, 2050, 2602, 3161, 3459, 3879, 5836)
for (stockcode in stockcodes) {
  #subset, filter and save data files for plots
  for (i in c(17:21, 24:28)) {
    #read in data files
    Bid <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Bid_",
          stockcode,
          "_",
          i,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    Offer <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Offer_",
          stockcode,
          "_",
          i,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    HFTRuns <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/StrategicRunsAlternative/HFTRuns_",
          stockcode,
          "_",
          i,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          RunEnd = col_time(format = "%H:%M:%OS"),
          RunStart = col_time(format = "%H:%M:%OS"),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    #Filter out missing data (zero prices)
    Bid <- Bid %>%
      filter(Price != 0)
    
    Offer <- Offer %>%
      filter(Price != 0)
    
    
    #Create Subsets
    bid.HFT <- subset(Bid, OrderCode %in% HFTRuns$OrderCode)
    
    offer.HFT <- subset(Offer, OrderCode %in% HFTRuns$OrderCode)
    
    bid.NT <- subset(Bid,!OrderCode %in% HFTRuns$OrderCode)
    
    offer.NT <- subset(Offer,!OrderCode %in% HFTRuns$OrderCode)
    
    #Merge HFT Bid/Offer and NT Bid/Offer
    HFT.orders <-
      full_join(
        bid.HFT,
        offer.HFT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    NT.orders <-
      full_join(
        bid.NT,
        offer.NT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    #Change system to display milliseconds
    options(digits.secs = 3)
    
    
    #Format time columns to POSIXlt
    HFT.orders$TimeAdded <-
      as.POSIXlt(paste(paste(i, "11", "2014", sep = ""), HFT.orders$TimeAdded),
                 tz = "GMT" ,
                 format = "%d%m%Y %H:%M:%OS")
    HFT.orders$TimeAction <-
      as.POSIXlt(paste(paste(i, "11", "2014", sep = ""), HFT.orders$TimeAction),
                 tz = "GMT",
                 format = "%d%m%Y %H:%M:%OS")
    NT.orders$TimeAdded <-
      as.POSIXlt(paste(paste(i, "11", "2014", sep = ""), NT.orders$TimeAdded),
                 tz = "GMT",
                 format = "%d%m%Y %H:%M:%OS")
    NT.orders$TimeAction <-
      as.POSIXlt(paste(paste(i, "11", "2014", sep = ""), NT.orders$TimeAction),
                 tz = "GMT",
                 format = "%d%m%Y %H:%M:%OS")
    
    
    #Filter out orders placed during opening and closing call since these orders act as outliers
    HFT.orders <- HFT.orders[order(HFT.orders$TimeAction),]
    HFT.orders <-
      HFT.orders[HFT.orders$TimeAction$hour >= 8 &
                   HFT.orders$TimeAction$hour <= 16,]
    HFT.orders <-
      HFT.orders[!(HFT.orders$TimeAction$hour == 16 &
                     HFT.orders$TimeAction$min >= 30),]
    NT.orders <- NT.orders[order(NT.orders$TimeAction),]
    NT.orders <-
      NT.orders[NT.orders$TimeAction$hour >= 8 &
                  NT.orders$TimeAction$hour <= 16,]
    NT.orders <-
      NT.orders[!(NT.orders$TimeAction$hour == 16 &
                    NT.orders$TimeAction$min >= 30),]
    
    #For Plot Y-scale
    #Determine Quantiles and save to a vector
    hft.lower <-
      as.numeric(quantile(HFT.orders$Price, probs = 0.001, na.rm = TRUE))
    hft.upper <-
      as.numeric(quantile(HFT.orders$Price, probs = 0.999, na.rm = TRUE))
    nt.lower <-
      as.numeric(quantile(NT.orders$Price, probs = 0.001, na.rm = TRUE))
    nt.upper <-
      as.numeric(quantile(NT.orders$Price, probs = 0.999, na.rm = TRUE))
    
    #Filter Prices for HFT and NT by quantiles in order to clean up outlier orders
    #1% Quantile < Price < 99% Quantile
    HFT.orders.q <-
      HFT.orders[HFT.orders$Price >= hft.lower &
                   HFT.orders$Price <= hft.upper,]
    NT.orders.q <-
      NT.orders[NT.orders$Price >= nt.lower &
                  NT.orders$Price <= nt.upper,]
    
    #Write tables to file
    write.table(
      HFT.orders.q,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Plot Data/HFT_orders_",
        stockcode,
        "_",
        i,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE
    )
    
    write.table(
      NT.orders.q,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Plot Data/NT_orders_",
        stockcode,
        "_",
        i,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE
    )
    
    
  }
  #create and save plots
  
  #merge all trading days into one data table
  HFT.file.list <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Plot Data",
        sep = ""
      ),
      pattern = "^[H]",
      full.names = TRUE
    )
  plot.data.hft <-
    do.call("rbind", lapply(
      HFT.file.list,
      FUN = function(files) {
        read.table(files, header = TRUE, sep = ",")
      }
    ))
  
  NT.file.list <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Plot Data",
        sep = ""
      ),
      pattern = "^[N]",
      full.names = TRUE
    )
  plot.data.nt <-
    do.call("rbind", lapply(
      NT.file.list,
      FUN = function(files) {
        read.table(files, header = TRUE, sep = ",")
      }
    ))
  
  
  #reformat time columns to POSIXct
  plot.data.hft$TimeAdded <-
    as.POSIXct(as.character(plot.data.hft$TimeAdded),
               tz = "GMT",
               format = "%Y-%m-%d %H:%M:%OS")
  plot.data.hft$TimeAction <-
    as.POSIXct(as.character(plot.data.hft$TimeAction),
               tz = "GMT",
               format = "%Y-%m-%d %H:%M:%OS")
  plot.data.nt$TimeAdded <-
    as.POSIXct(as.character(plot.data.nt$TimeAdded),
               tz = "GMT",
               format = "%Y-%m-%d %H:%M:%OS")
  plot.data.nt$TimeAction <-
    as.POSIXct(as.character(plot.data.nt$TimeAction),
               tz = "GMT",
               format = "%Y-%m-%d %H:%M:%OS")
  
  #add new time column to plot data and reformat as hms(time)
  plot.data.hft$hms <-
    format(plot.data.hft$TimeAction, format = "%H:%M:%OS")
  plot.data.hft$hms <- as.hms(plot.data.hft$hms)
  plot.data.nt$hms <-
    format(plot.data.nt$TimeAction, format = "%H:%M:%OS")
  plot.data.nt$hms <- as.hms(plot.data.nt$hms)
  
  #add factor column for trading day in order to plot using facet
  plot.data.hft$Day <- as.factor(mday(plot.data.hft$TimeAction))
  plot.data.nt$Day <- as.factor(mday(plot.data.nt$TimeAction))
  
  #rename levels
  levels(plot.data.hft$Day)[levels(plot.data.hft$Day) == c("17", "18", "19", "20", "21", "24", "25", "26", "27", "28")] <-
    c(
      "Nov. 17, 2014",
      "Nov. 18, 2014",
      "Nov. 19, 2014",
      "Nov. 20, 2014",
      "Nov. 21, 2014",
      "Nov. 24, 2014",
      "Nov. 25, 2014",
      "Nov. 26, 2014",
      "Nov. 27, 2014",
      "Nov. 28, 2014"
    )
  levels(plot.data.nt$Day)[levels(plot.data.nt$Day) == c("17", "18", "19", "20", "21", "24", "25", "26", "27", "28")] <-
    c(
      "Nov. 17, 2014",
      "Nov. 18, 2014",
      "Nov. 19, 2014",
      "Nov. 20, 2014",
      "Nov. 21, 2014",
      "Nov. 24, 2014",
      "Nov. 25, 2014",
      "Nov. 26, 2014",
      "Nov. 27, 2014",
      "Nov. 28, 2014"
    )
  
  #plot orders
  #plot HFT orders and save to plot.HFT variable
  plot.HFT <- ggplot(plot.data.hft) +
    geom_point(aes(hms, Price, group = BuySellInd, colour = BuySellInd), size = 0.15) +
    ggtitle(paste(
      "Orders Placed by High Frequency Traders for Stock ",
      stockcode,
      sep = ""
    )) +
    xlab("Time Action") + ylab("Order Price") +
    scale_x_time(breaks = waiver()) +
    theme_light() +
    scale_color_manual(
      values = c("deepskyblue2", "maroon2"),
      name = "Buy Sell\nIndicator",
      labels = c("Buy\nOrder", "Sell\nOrder")
    ) +
    guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
    facet_wrap( ~ Day, ncol = 2, scales = "free_y")
  plot.HFT
  
  #plot NT orders and save to plot.NT variable
  plot.NT <- ggplot(plot.data.nt) +
    geom_point(aes(hms, Price, group = BuySellInd, colour = BuySellInd), size = 0.15) +
    ggtitle(paste("Orders Placed by Normal Traders for Stock ", stockcode, sep =
                    "")) +
    xlab("Time Action") + ylab("Order Price") +
    scale_x_time(breaks = waiver()) +
    theme_light() +
    scale_color_manual(
      values = c("darkolivegreen3", "mediumpurple3"),
      name = "Buy Sell\nIndicator",
      labels = c("Buy\nOrder", "Sell\nOrder")
    ) +
    guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
    facet_wrap( ~ Day, ncol = 2, scales = "free_y")
  plot.NT
  
  #save plots
  ggsave(
    paste("HFT_Plot_", stockcode, ".png", sep = ""),
    plot = plot.HFT,
    device = "png",
    path = paste("~/Documents/Thesis/Thesis Results/", stockcode, sep = ""),
    width = 11.00,
    height = 8.50,
    units = "in"
  )
  ggsave(
    paste("NT_Plot_", stockcode, ".png", sep = ""),
    plot = plot.NT,
    device = "png",
    path = paste("~/Documents/Thesis/Thesis Results/", stockcode, sep = ""),
    width = 11.00,
    height = 8.50,
    units = "in"
  )
  
}


#Order Location
for (stockcode in stockcodes) {
  for (k in c(17:21, 24:28)) {
    #read in bid, best bid, offer, best offer, spread, and hft runs files
    Bid <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Bid_",
          stockcode,
          "_",
          k,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    BestBid <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/BestBid_",
          stockcode,
          "_",
          k,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          Price = col_number(),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    Offer <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Offer_",
          stockcode,
          "_",
          k,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    BestOffer <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/BestOffer_",
          stockcode,
          "_",
          k,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          Price = col_number(),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    Spread <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Spread_",
          stockcode,
          "_",
          k,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          BidPrice = col_number(),
          Duration = col_number(),
          From = col_time(format = "%H:%M:%OS"),
          OfferPrice = col_number(),
          Spread = col_number(),
          To = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    HFTRuns <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/StrategicRunsAlternative/HFTRuns_",
          stockcode,
          "_",
          k,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          RunEnd = col_time(format = "%H:%M:%OS"),
          RunStart = col_time(format = "%H:%M:%OS"),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    
    
    #Adding BuySellInd column to BestBid and BestOffer Datasets
    BestBid <- mutate(BestBid, BuySellInd = "B")
    
    BestOffer <- mutate(BestOffer, BuySellInd = "S")
    
    
    #Joining BestBid and BestOffer Datasets
    AllBestOrders <-
      full_join(
        BestBid,
        BestOffer,
        by = c(
          "OrderCode",
          "TimeAdded",
          "TimeAction",
          "OrderActionType",
          "Price",
          "BuySellInd"
        )
      )
    
    #Changing zero prices to NA
    #BidPrice is column 3
    Spread[, 3][Spread[, 3] == 0] <- as.numeric(NA)
    #OfferPrice is column 5
    Spread[, 5][Spread[, 5] == 0] <- as.numeric(NA)
    
    
    
    #Determing Best Offer for All Offer Orders
    Offer$BestPrice <- as.numeric(NA)
    for (i in 1:nrow(Offer)) {
      time <- Offer$TimeAdded[i]
      best.offer <-
        Spread[Spread$From <= time & time <= Spread$To, "OfferPrice"]
      if (nrow(best.offer) == 1)
        Offer$BestPrice[i] <- best.offer
    }
    #change class of best price column to numeric
    class(Offer$BestPrice) = "numeric"
    
    #Determing Best Bid for All Bid Orders
    Bid$BestPrice <- as.numeric(NA)
    for (i in 1:nrow(Bid)) {
      time <- Bid$TimeAdded[i]
      best.bid <-
        Spread[Spread$From <= time & time <= Spread$To, "BidPrice"]
      if (nrow(best.bid) == 1)
        Bid$BestPrice[i] <- best.bid
    }
    #change class of Best Price column to numeric
    class(Bid$BestPrice) = "numeric"
    
    #Merging Bid and Offer Datasets
    AllOrders <-
      full_join(
        Bid,
        Offer,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize",
          "BestPrice"
        )
      )
    
    #Change zero prices to NA
    AllOrders[, 4][AllOrders[, 4] == 0] <- as.numeric(NA)
    
    
    #Determine if orders are at best bid or offer
    AllOrders$AtBestBidOrOffer <- as.numeric(NA)
    for (i in 1:nrow(AllOrders)) {
      price <- AllOrders$Price[i]
      best.price <- AllOrders$BestPrice[i]
      if (nrow(AllOrders.df))
        AllOrders$AtBestBidOrOffer[i] <- as.numeric(price == best.price)
    }
    
    
    
    #Orders that are Inside the Spread
    #Ask > Price > Bid
    #AllOrders.df$InsideTheSpread <- 0
    AllOrders$InsideTheSpread <- as.numeric(NA)
    for (i in 1:nrow(AllOrders)) {
      time <- AllOrders$TimeAdded[i]
      price <- AllOrders$Price[i]
      spread.time <-
        Spread[Spread$From <= time & time <= Spread$To,]
      #returns 1 for TRUE and 0 for FALSE
      if (nrow(spread.time))
        AllOrders$InsideTheSpread[i] <-
        as.numeric(spread.time$BidPrice < price &&
                     price < spread.time$OfferPrice)
    }
    
    
    #Orders that are Outside the Spread
    #Price > Ask or Price < Bid
    AllOrders$OutsideTheSpread <- as.numeric(NA)
    for (i in 1:nrow(AllOrders)) {
      time <- AllOrders$TimeAdded[i]
      price <- AllOrders$Price[i]
      order.type <- AllOrders$BuySellInd[i]
      spread.time <-
        Spread[Spread$From <= time & time <= Spread$To,]
      #returns 1 for TRUE and 0 for FALSE
      if (nrow(spread.time)) {
        if (order.type == "B") {
          AllOrders$OutsideTheSpread[i] <-
            as.numeric(spread.time$BidPrice > price)
        }
        else if (order.type == "S") {
          AllOrders$OutsideTheSpread[i] <-
            as.numeric(spread.time$OfferPrice < price)
        }
      }
    }
    
    #changing data frame to data table format
    all.orders <- as.data.table(AllOrders)
    spread <- as.data.table(Spread)
    
    
    #Add HFT label column
    all.orders$TraderType <- as.character(NA)
    for (i in 1:nrow(all.orders)) {
      if (all.orders$OrderCode[i] %in% HFTRuns$OrderCode) {
        all.orders$TraderType[i] <- "HFT"
      }
      else {
        all.orders$TraderType[i] <- "NT"
      }
    }
    
    #Create New Data Table
    OrderLocation <-
      data.table(
        OrderCode = all.orders$OrderCode,
        TraderType = all.orders$TraderType,
        TimeAdded = all.orders$TimeAdded,
        TimeAction = all.orders$TimeAction,
        BuySellInd = all.orders$BuySellInd,
        OrderPrice = all.orders$Price,
        OrderActionType = all.orders$OrderActionType,
        BestPrice = all.orders$BestPrice,
        AtBBO = all.orders$AtBestBidOrOffer,
        InsideSpread = all.orders$InsideTheSpread,
        OutsideSpread = all.orders$OutsideTheSpread
      )
    
    
    #Save LM file
    write.table(
      OrderLocation,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Order Location/OrderLocationTable_",
        stockcode,
        "_",
        k,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE,
      col.names = TRUE
    )
    
    
    #Subset HFT and NT
    hft.subset <- OrderLocation[OrderLocation$TraderType == "HFT",]
    nt.subset <- OrderLocation[OrderLocation$TraderType == "NT",]
    
    
    #Order location
    #Ratio of Orders placed at the Best Bid or Offer
    AtBBO.HFT <-
      hft.subset[, (sum(hft.subset$AtBBO, na.rm = TRUE) / length(OrderLocation$AtBBO)) *
                   100]
    AtBBO.NT <-
      nt.subset[, (sum(nt.subset$AtBBO, na.rm = TRUE) / length(OrderLocation$AtBBO)) *
                  100]
    
    #Ratio of Orders placed Inside the Spread
    Inside.HFT <-
      hft.subset[, (sum(hft.subset$InsideSpread, na.rm = TRUE) / length(OrderLocation$InsideSpread)) *
                   100]
    Inside.NT <-
      nt.subset[, (sum(nt.subset$InsideSpread, na.rm = TRUE) / length(OrderLocation$InsideSpread)) *
                  100]
    
    #Ratio of Orders placed Outside the Spread
    Outside.HFT <-
      hft.subset[, (
        sum(hft.subset$OutsideSpread, na.rm = TRUE) / length(OrderLocation$OutsideSpread)
      ) * 100]
    Outside.NT <-
      nt.subset[, (
        sum(nt.subset$OutsideSpread, na.rm = TRUE) / length(OrderLocation$OutsideSpread)
      ) * 100]
    
    #Combine all results
    order.location <-
      data.table(
        V1 = AtBBO.HFT,
        V2 = AtBBO.NT,
        V3 = Inside.HFT,
        V4 = Inside.NT,
        V5 = Outside.HFT,
        V6 = Outside.NT
      )
    
    #Set Column Names
    setnames(
      order.location,
      1:6,
      c(
        "AtBBO.HFT",
        "AtBBO.NT",
        "Inside.HFT",
        "Inside.NT",
        "Outside.HFT",
        "Outside.NT"
      )
    )
    
    #Create New File
    write.table(
      order.location,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Order Location/OrderLocationRatios_",
        stockcode,
        "_",
        k,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE
    )
    
  }
}



for (stockcode in stockcodes) {
  for (day in days) {
    Bid <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Bid_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    Offer <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Offer_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    HFTRuns <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/StrategicRunsAlternative/HFTRuns_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          RunEnd = col_time(format = "%H:%M:%OS"),
          RunStart = col_time(format = "%H:%M:%OS"),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    spread <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Spread_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          BidPrice = col_number(),
          Duration = col_number(),
          From = col_time(format = "%H:%M:%OS"),
          OfferPrice = col_number(),
          Spread = col_number(),
          To = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    
    hft.bid <- subset(Bid, OrderCode %in% HFTRuns$OrderCode)
    
    hft.offer <- subset(Offer, OrderCode %in% HFTRuns$OrderCode)
    
    nt.bid <- subset(Bid,!OrderCode %in% HFTRuns$OrderCode)
    
    nt.offer <- subset(Offer,!OrderCode %in% HFTRuns$OrderCode)
    
    hft.bid <- as.data.table(hft.bid)
    hft.offer <- as.data.table(hft.offer)
    nt.bid <- as.data.table(nt.bid)
    nt.offer <- as.data.table(nt.offer)
    
    hft.bid[, 4][hft.bid[, 4] == 0,] <- as.numeric(NA)
    hft.offer[, 4][hft.offer[, 4] == 0,] <- as.numeric(NA)
    nt.bid[, 4][nt.bid[, 4] == 0,] <- as.numeric(NA)
    nt.offer[, 4][nt.offer[, 4] == 0,] <- as.numeric(NA)
    spread[, 3][spread[, 3] == 0,] <- as.numeric(NA)
    spread[, 5][spread[, 5] == 0,] <- as.numeric(NA)
    
    hft.lower <- sd(hft.bid$Price, na.rm = TRUE)
    hft.upper <- sd(hft.offer$Price, na.rm = TRUE)
    nt.lower <- sd(nt.bid$Price, na.rm = TRUE)
    nt.upper <- sd(nt.offer$Price, na.rm = TRUE)
    
    
    
    hft.subset <-
      full_join(
        hft.bid,
        hft.offer,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    nt.subset <-
      full_join(
        nt.bid,
        nt.offer,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    order.date <- paste(day, "112014", sep = "")
    hft.subset$Date <- rep(order.date)
    nt.subset$Date <- rep(order.date)
    spread$Date <- rep(order.date)
    
    #Format time elements in the TimeAction column
    hft.subset$TimeAdded <-
      paste(hft.subset$Date, hft.subset$TimeAdded, sep = " ")
    hft.subset$TimeAction <-
      paste(hft.subset$Date, hft.subset$TimeAction, sep = " ")
    hft.subset$TimeAdded <-
      as.POSIXct(hft.subset$TimeAdded, tz = "GMT" , format = "%d%m%Y %H:%M:%OS")
    hft.subset$TimeAction <-
      as.POSIXct(hft.subset$TimeAction, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
    
    nt.subset$TimeAdded <-
      paste(nt.subset$Date, nt.subset$TimeAdded, sep = " ")
    nt.subset$TimeAction <-
      paste(nt.subset$Date, nt.subset$TimeAction, sep = " ")
    nt.subset$TimeAdded <-
      as.POSIXct(nt.subset$TimeAdded, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
    nt.subset$TimeAction <-
      as.POSIXct(nt.subset$TimeAction, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
    
    spread$From <- paste(spread$Date, spread$From, sep = " ")
    spread$To <- paste(spread$Date, spread$To, sep = " ")
    spread$From <-
      as.POSIXct(spread$From, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
    spread$To <-
      as.POSIXct(spread$To, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
    
    
    hft.subset <- as.data.table(hft.subset)
    nt.subset <- as.data.table(nt.subset)
    spread <- as.data.table(spread)
    
    
    
    hft.subset$OneStDevOutside <- as.numeric(NA)
    hft.subset$TwoStDevOutside <- as.numeric(NA)
    for (k in 1:nrow(hft.subset)) {
      time <- hft.subset$TimeAdded[k]
      price <- hft.subset$Price[k]
      order.type <- hft.subset$BuySellInd[k]
      spread.time <-
        spread[spread$From <= time & time <= spread$To,]
      lower.h <- spread.time$BidPrice - hft.lower
      two.lower.h <- spread.time$BidPrice - 2 * hft.lower
      upper.h <- spread.time$OfferPrice + hft.upper
      two.upper.h <- spread.time$OfferPrice + 2 * hft.upper
      #returns 1 for TRUE and 0 for FALSE
      if (nrow(spread.time)) {
        if (order.type == "B") {
          hft.subset$OneStDevOutside[k] <- as.numeric(lower.h > price)
          hft.subset$TwoStDevOutside[k] <-
            as.numeric(two.lower.h > price)
        }
        else if (order.type == "S") {
          hft.subset$OneStDevOutside[k] <- as.numeric(upper.h < price)
          hft.subset$TwoStDevOutside[k] <-
            as.numeric(two.upper.h < price)
        }
      }
    }
    
    
    nt.subset$OneStDevOutside <- as.numeric(NA)
    nt.subset$TwoStDevOutside <- as.numeric(NA)
    for (k in 1:nrow(nt.subset)) {
      time <- nt.subset$TimeAdded[k]
      price <- nt.subset$Price[k]
      order.type <- nt.subset$BuySellInd[k]
      spread.time <-
        spread[spread$From <= time & time <= spread$To,]
      lower.n <- spread.time$BidPrice - nt.lower
      two.lower.n <- spread.time$BidPrice - 2 * nt.lower
      upper.n <- spread.time$OfferPrice + nt.upper
      two.upper.n <- spread.time$OfferPrice + 2 * nt.upper
      #returns 1 for TRUE and 0 for FALSE
      if (nrow(spread.time)) {
        if (order.type == "B") {
          nt.subset$OneStDevOutside[k] <- as.numeric(lower.n > price)
          nt.subset$TwoStDevOutside[k] <-
            as.numeric(two.lower.n > price)
        }
        else if (order.type == "S") {
          nt.subset$OneStDevOutside[k] <- as.numeric(upper.n < price)
          nt.subset$TwoStDevOutside[k] <-
            as.numeric(two.upper.n < price)
        }
      }
    }
    
    hft.onestdevoutside <-
      hft.subset[, (sum(hft.subset$OneStDevOutside, na.rm = TRUE) / nrow(hft.subset))]
    hft.twostdevoutside <-
      hft.subset[, (sum(hft.subset$TwoStDevOutside, na.rm = TRUE) / nrow(hft.subset))]
    nt.onestdevoutside <-
      nt.subset[, (sum(nt.subset$OneStDevOutside, na.rm = TRUE) / nrow(nt.subset))]
    nt.twostdevoutside <-
      nt.subset[, (sum(nt.subset$TwoStDevOutside, na.rm = TRUE) / nrow(nt.subset))]
    
    
    PriceStDev <-
      data.table(
        Date = order.date,
        HFT.OneStDevOut = hft.onestdevoutside,
        NT.OneStDevOut = nt.onestdevoutside,
        HFT.TwoStDevOut = hft.twostdevoutside,
        NT.TwoStDevOut = nt.twostdevoutside
      )
    
    write.table(
      PriceStDev,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/St Dev Tables/StDevTable_",
        stockcode,
        "_",
        day,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE,
      col.names = FALSE
    )
    
    
  }
}


for (stockcode in stockcodes) {
  all.stdev.files <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/St Dev Tables",
        sep = ""
      ),
      full = TRUE
    )
  
  all.stdev.data <- lapply(all.stdev.files,  read.csv, header = FALSE)
  
  df.stdev <- do.call("rbind", all.stdev.data)
  
  
  write.table(
    df.stdev,
    file = paste(
      "~/Documents/Thesis/Thesis Results/",
      stockcode,
      "/StDevTable",
      stockcode,
      ".csv",
      sep = ""
    ),
    sep = ",",
    row.names = FALSE,
    col.names = c(
      "Date",
      "HFT One St. Dev. Outside",
      "NT One St. Dev. Outside",
      "HFT Two St. Dev. Outside",
      "NT Two St. Dev. Outside"
    )
  )
  
}



#combine all trading day spread file into one file for each stock
for (stockcode in stockcodes) {
  for (day in days) {
    Spread.File <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Spread_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          BidPrice = col_number(),
          Duration = col_number(),
          From = col_time(format = "%H:%M:%OS"),
          OfferPrice = col_number(),
          Spread = col_number(),
          To = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    order.date <- paste(day, "112014", sep = "")
    
    Spread.File$Date <- rep(order.date)
    
    write.table(
      Spread.File,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Spread Files/SpreadFile_",
        stockcode,
        "_",
        day,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE,
      col.names = TRUE
    )
    
  }
  
  all.spread.files <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/Spread Files",
        sep = ""
      ),
      full = TRUE
    )
  
  all.spread.data <-
    lapply(all.spread.files,  read.csv, header = TRUE)
  
  df.spread <- do.call("rbind", all.spread.data)
  
  write.table(
    df.spread,
    file = paste(
      "~/Documents/Thesis/Thesis Results/Full Spread Files/FullSpread_",
      stockcode,
      ".csv",
      sep = ""
    ),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
  
  
}






#Quoted Spread

#Create empty data table for results to follow
QuotedSpreadFull.dt <-
  data.table(
    Stockcodes = stockcodes,
    QuotedSpread = as.numeric(NA),
    PercentQuotedSpread = as.numeric(NA),
    AvgMidpoint = as.numeric(NA),
    StDevMidpoint = as.numeric(NA)
  )


for (stockcode in stockcodes) {
  #read in full spread files
  full.spread <-
    read_delim(
      paste(
        "~/Documents/Thesis/Thesis Results/Full Spread Files/FullSpread_",
        stockcode,
        ".csv",
        sep = ""
      ),
      ",",
      escape_double = FALSE,
      col_types = cols(
        BidPrice = col_number(),
        Duration = col_number(),
        From = col_time(format = "%H:%M:%OS"),
        OfferPrice = col_number(),
        Spread = col_number(),
        To = col_time(format = "%H:%M:%OS")
      ),
      trim_ws = TRUE
    )
  
  
  
  #Change zero prices to NA
  #BidPrice is column 3
  full.spread[, 3][full.spread[, 3] == 0] <- as.numeric(NA)
  #OfferPrice is column 5
  full.spread[, 5][full.spread[, 5] == 0] <- as.numeric(NA)
  
  
  #add Midpoint, QuotedSpread and PercentQuotedSpread columns to full.spread data frame
  full.spread$Midpoint <- as.numeric(NA)
  full.spread$QuotedSpread <- as.numeric(NA)
  full.spread$PercentQuotedSpread <- as.numeric(NA)
  #calculate midpoint, quoted spread and percent quoted spread
  for (i in 1:nrow(full.spread)) {
    full.spread$Midpoint[i] <-
      (full.spread$OfferPrice[i] + full.spread$BidPrice[i]) / 2
    full.spread$QuotedSpread[i] <-
      full.spread$OfferPrice[i] - full.spread$BidPrice[i]
    full.spread$PercentQuotedSpread[i] <-
      full.spread$QuotedSpread[i] / full.spread$Midpoint[i]
    
  }
  
  
  #print the results to the QuotedSpreadFull.dt data table based on matching stockcodes
  for (k in 1:nrow(QuotedSpreadFull.dt)) {
    if (QuotedSpreadFull.dt$Stockcodes[k] == stockcode) {
      QuotedSpreadFull.dt$QuotedSpread[k] <-
        mean(full.spread$QuotedSpread, na.rm = TRUE)
      QuotedSpreadFull.dt$PercentQuotedSpread[k] <-
        mean(full.spread$PercentQuotedSpread, na.rm = TRUE)
      QuotedSpreadFull.dt$AvgMidpoint[k] <-
        mean(full.spread$Midpoint, na.rm = TRUE)
      QuotedSpreadFull.dt$StDevMidpoint[k] <-
        sd(full.spread$Midpoint, na.rm = TRUE)
      
    }
  }
  
  
}


#Write the data table as a csv file and save to computer
write.table(
  QuotedSpreadFull.dt,
  file = paste(
    "~/Documents/Thesis/Thesis Results/FullMidpoint_",
    stockcode,
    ".csv",
    sep = ""
  ),
  sep = ",",
  row.names = FALSE,
  col.names = c(
    "Stock Code",
    "Quoted Spread",
    "Percent Quoted Spread",
    "Average Midpoint",
    "Standard Deviation"
  )
)



for (stockcode in stockcodes) {
  Midpoint.dt <-
    data.table(
      Dates = stockdates,
      AvgMidpoint = as.numeric(NA),
      StDevMidpoint = as.numeric(NA)
    )
  
  for (stockdate in stockdates) {
    spread.df <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Spread_",
          stockcode,
          "_",
          stockdate,
          ".csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          BidPrice = col_number(),
          Duration = col_number(),
          From = col_time(format = "%H:%M:%OS"),
          OfferPrice = col_number(),
          Spread = col_number(),
          To = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    spread.df[, 3][spread.df[, 3] == 0] <- as.numeric(NA)
    #OfferPrice is column 5
    spread.df[, 5][spread.df[, 5] == 0] <- as.numeric(NA)
    
    
    spread.df$Midpoint <- as.numeric(NA)
    for (i in 1:nrow(spread.df)) {
      spread.df$Midpoint[i] <-
        (spread.df$OfferPrice[i] + spread.df$BidPrice[i]) / 2
    }
    
    for (k in 1:nrow(Midpoint.dt)) {
      if (Midpoint.dt$Dates[k] == stockdate) {
        Midpoint.dt$AvgMidpoint[k] <- mean(spread.df$Midpoint, na.rm = TRUE)
        Midpoint.dt$StDevMidpoint[k] <-
          sd(spread.df$Midpoint, na.rm = TRUE)
        
      }
    }
  }
  
  write.table(
    Midpoint.dt,
    file = paste(
      "~/Documents/Thesis/Thesis Results/",
      stockcode,
      "/Midpoint_",
      stockcode,
      ".csv",
      sep = ""
    ),
    sep = ",",
    row.names = FALSE,
    col.names = c(
      "Stock Code",
      "Quoted Spread",
      "Percent Quoted Spread",
      "Average Midpoint",
      "Standard Deviation"
    )
  )
  
  
}














#Create and save subsets for HFT and NT orders for simpler manipulation in later sections


stockcodes <-
  c(11234, 12059, 12534, 1769, 2050, 2602, 3161, 3459, 3879, 5836)
days <- c(17, 18, 19, 20, 21, 24, 25, 26, 27, 28)
for (stockcode in stockcodes) {
  for (day in days) {
    Bid <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Bid_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    Offer <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/BestPrices/Offer_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    HFTRuns <-
      read_delim(
        paste(
          "~/Documents/Thesis/Best Prices/",
          stockcode,
          "/StrategicRunsAlternative/HFTRuns_",
          stockcode,
          "_",
          day,
          "112014.csv",
          sep = ""
        ),
        ";",
        escape_double = FALSE,
        col_types = cols(
          RunEnd = col_time(format = "%H:%M:%OS"),
          RunStart = col_time(format = "%H:%M:%OS"),
          TimeAction = col_time(format = "%H:%M:%OS"),
          TimeAdded = col_time(format = "%H:%M:%OS")
        ),
        trim_ws = TRUE
      )
    
    #Create Subsets
    Bid.HFT <- subset(Bid, OrderCode %in% HFTRuns$OrderCode)
    
    Offer.HFT <- subset(Offer, OrderCode %in% HFTRuns$OrderCode)
    
    Bid.NT <- subset(Bid,!OrderCode %in% HFTRuns$OrderCode)
    
    Offer.NT <- subset(Offer,!OrderCode %in% HFTRuns$OrderCode)
    
    
    hft.subset <-
      full_join(
        Bid.HFT,
        Offer.HFT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    nt.subset <-
      full_join(
        Bid.NT,
        Offer.NT,
        by = c(
          "OrderCode",
          "ParticipantCode",
          "TimeAdded",
          "Price",
          "BuySellInd",
          "AggrSize",
          "StockCode",
          "MarketSectorCode",
          "MessageSequenceNumber",
          "TimeAction",
          "OrderActionType",
          "MarketMechanismType",
          "TradeSize",
          "AggregateSize"
        )
      )
    
    order.date <- paste(day, "112014", sep = "")
    
    hft.subset$Date <- rep(order.date)
    nt.subset$Date <- rep(order.date)
    
    #save to csv
    write.table(
      hft.subset,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/HFT Subset/HFTSubset_",
        stockcode,
        "_",
        day,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE,
      col.names = TRUE
    )
    #save to csv
    write.table(
      nt.subset,
      file = paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/NT Subset/NTSubset_",
        stockcode,
        "_",
        day,
        "112014.csv",
        sep = ""
      ),
      sep = ",",
      row.names = FALSE,
      col.names = TRUE
    )
    
  }
  
  #for HFT
  all.hft.subsets <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/HFT Subset",
        sep = ""
      ),
      full = TRUE
    )
  
  all.hft.data <- lapply(all.hft.subsets,  read.csv, header = TRUE)
  
  df.hft <- do.call("rbind", all.hft.data)
  
  write.table(
    df.hft,
    file = paste(
      "~/Documents/Thesis/Thesis Results/Full Subsets/FullHFTSubset_",
      stockcode,
      sep = ""
    ),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
  
  
  #for NT
  all.nt.subsets <-
    list.files(
      paste(
        "~/Documents/Thesis/Thesis Results/",
        stockcode,
        "/NT Subset",
        sep = ""
      ),
      full = TRUE
    )
  
  all.nt.data <- lapply(all.nt.subsets,  read.csv, header = TRUE)
  
  df.nt <- do.call("rbind", all.nt.data)
  
  write.table(
    df.nt,
    file = paste(
      "~/Documents/Thesis/Thesis Results/Full Subsets/FullNTSubset_",
      stockcode,
      sep = ""
    ),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
  
}



#Plot intraday deletion rates using 5 minute time intervals

for (stockcode in stockcodes) {
  hft.subset <-
    read_delim(
      paste(
        "~/Documents/Thesis/Thesis Results/Full Subsets/FullHFTSubset_",
        stockcode,
        sep = ""
      ),
      ",",
      escape_double = FALSE,
      col_types = cols(
        TimeAction = col_time(format = "%H:%M:%OS"),
        TimeAdded = col_time(format = "%H:%M:%OS")
      ),
      trim_ws = TRUE
    )
  
  nt.subset <-
    read_delim(
      paste(
        "~/Documents/Thesis/Thesis Results/Full Subsets/FullNTSubset_",
        stockcode,
        sep = ""
      ),
      ",",
      escape_double = FALSE,
      col_types = cols(
        TimeAction = col_time(format = "%H:%M:%OS"),
        TimeAdded = col_time(format = "%H:%M:%OS")
      ),
      trim_ws = TRUE
    )
  
  #Format time elements in the TimeAction column
  hft.subset$TimeAdded <-
    paste(hft.subset$Date, hft.subset$TimeAdded, sep = " ")
  hft.subset$TimeAction <-
    paste(hft.subset$Date, hft.subset$TimeAction, sep = " ")
  hft.subset$TimeAdded <-
    as.POSIXct(hft.subset$TimeAdded, tz = "GMT" , format = "%d%m%Y %H:%M:%OS")
  hft.subset$TimeAction <-
    as.POSIXct(hft.subset$TimeAction, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
  
  nt.subset$TimeAdded <-
    paste(nt.subset$Date, nt.subset$TimeAdded, sep = " ")
  nt.subset$TimeAction <-
    paste(nt.subset$Date, nt.subset$TimeAction, sep = " ")
  nt.subset$TimeAdded <-
    as.POSIXct(nt.subset$TimeAdded, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
  nt.subset$TimeAction <-
    as.POSIXct(nt.subset$TimeAction, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
  
  #days <- c(17, 18, 19, 20, 21, 24, 25, 26, 27, 28)
  for (day in days) {
    start.time <-
      paste(paste(day, "112014", sep = ""), "08:00:00.000", sep = " ")
    end.time <-
      paste(paste(day, "112014", sep = ""), "16:30:00.000", sep = " ")
    
    start.time <-
      as.POSIXct(start.time, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
    end.time <-
      as.POSIXct(end.time, tz = "GMT", format = "%d%m%Y %H:%M:%OS")
    
    intervals.df <- as.data.table(seq(start.time, end.time, by = 300))
    colnames(intervals.df) <- "interval"
    
    intervals <- intervals.df$interval
    
    hft.subset <- as.data.table(hft.subset)
    nt.subset <- as.data.table(nt.subset)
    
    
    intervals.df$hft.deleted.rate <- as.numeric(NA)
    intervals.df$nt.deleted.rate <- as.numeric(NA)
    intervals.df$order.imbalance <- as.numeric(NA)
    
    for (i in 1:102) {
      interval <- c(intervals[i], intervals[i + 1])
      rows.in.interval.hft <-
        hft.subset[hft.subset$TimeAction >= interval[1] &
                     interval[2] >= hft.subset$TimeAction,]
      deleted.rows.hft <-
        rows.in.interval.hft[rows.in.interval.hft$OrderActionType == "D",]
      intervals.df$hft.deleted.rate[i + 1] <-
        nrow(deleted.rows.hft) / nrow(rows.in.interval.hft)
      
      rows.in.interval.nt <-
        nt.subset[nt.subset$TimeAction >= interval[1] &
                    interval[2] >= nt.subset$TimeAction,]
      deleted.rows.nt <-
        rows.in.interval.nt[rows.in.interval.nt$OrderActionType == "D",]
      intervals.df$nt.deleted.rate[i + 1] <-
        nrow(deleted.rows.nt) / nrow(rows.in.interval.nt)
      
      bid.num.hft <-
        rows.in.interval.hft[rows.in.interval.hft$BuySellInd == "B",]
      offer.num.hft <-
        rows.in.interval.hft[rows.in.interval.hft$BuySellInd == "S",]
      bid.num.nt <-
        rows.in.interval.nt[rows.in.interval.nt$BuySellInd == "B",]
      offer.num.nt <-
        rows.in.interval.nt[rows.in.interval.nt$BuySellInd == "S",]
      bid.total <- nrow(bid.num.hft) + nrow(bid.num.nt)
      offer.total <- nrow(offer.num.hft) + nrow(offer.num.nt)
      intervals.df$order.imbalance[i + 1] <-
        (bid.total - offer.total) / (bid.total + offer.total)
      
    }
    
    delete.plot <- ggplot(intervals.df) +
      geom_line(aes(interval, order.imbalance + 0.7), colour = "grey") +
      geom_line(aes(interval, hft.deleted.rate), colour = "maroon2") +
      geom_line(aes(interval, nt.deleted.rate), colour = "deepskyblue") +
      scale_y_continuous(sec.axis = sec_axis( ~ . - 0.7, name = "Order Imbalance")) +
      theme_light() +
      ggtitle(paste(
        "Intraday Deleted Order Ratio for HFT and NT for Nov. ",
        day,
        ", 2014",
        sep = ""
      )) +
      xlab("Time Action") +
      ylab("Deleted Order Ratio") +
      scale_color_manual(
        values = c("maroon2", "deepskyblue", "grey"),
        labels = c("HFT", "NT", "Order Imbalance")
      ) +
      guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
      theme(axis.text.x = element_text(angle = 50, vjust = 0.5),
            legend.title = element_blank())
    
    
    
    
    #Save Plots to file
    ggsave(
      paste("DeleteRatePlot_", stockcode, "_", day, "112014.png", sep = ""),
      plot = delete.plot,
      device = "png",
      path = paste("~/Documents/Thesis/Thesis Results/", stockcode, sep = ""),
      width = 11.00,
      height = 8.50,
      units = "in"
    )
    
  }
  
}
