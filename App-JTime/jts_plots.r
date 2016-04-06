
#############################################################################################
# Function 1: Subset data
#############################################################################################

f_GetLinkData <- function(jts,SA_LinkID) {
  
  
  ################## Initialize ##################
  
  # Change hyphen to underscore
  SA_LinkID  = gsub("-","_",SA_LinkID)
  
  # Other parameters
  V_Periods <- c("NT","AM","IP","PM","EVE")
  
  ################## Subset - by link ID ##################
  
  jts_sub <- jts[jts$SA_LinkID == SA_LinkID,]
  
  ################## Check if there is some data ##################
  
  if(nrow(jts_sub) == 0) {
    return(NA)
  }
  
  ################## Extract list of TM links with associated order ##################
  
  # If duplicates they will be deleted
  
  # Get list of unique TM_LinkID + Order + Distance
  T_TMLinks <- unique(jts_sub[,c("TM_LinkID","TMLinkOrder","Dist")])
  
  # Sort by order
  T_TMLinks <- T_TMLinks[order(T_TMLinks$TMLinkOrder),]
  
  # Remove duplicates links
  # remove only consecutive duplicates: keeps the last one of the series
  T_TMLinks <- T_TMLinks[cumsum(rle(as.character(T_TMLinks$TM_LinkID))$lengths),]
  # remove all duplicates
  # T_TMLinks <- T_TMLinks[!(duplicated(T_TMLinks[,c("TM_LinkID","Dist")])),]
  
  # Create new order
  N_Links <- nrow(T_TMLinks)
  T_TMLinks$TMLinkOrder <- seq(1,N_Links,1)
  
  # Add cumulative distance
  T_TMLinks$CumDist <- cumsum(T_TMLinks$Dist)
  
  
  ################## Generates data table for the plot ##################
  
  # Initializes
  T_Plot <- as.data.frame(matrix(NA, nrow = 5 * N_Links, ncol = 5))
  names(T_Plot) <- c("Period","TM_LinkID","TMLinkOrder","Dist","CumDist" )
  
  # Period column
  T_Plot$Period <- c(rep("NT",N_Links),rep("AM",N_Links),
                     rep("IP",N_Links),rep("PM",N_Links),
                     rep("EVE",N_Links))
  T_Plot$Period <- factor(T_Plot$Period , ordered = TRUE, levels = V_Periods)
  
  # 4 other columns
  T_Plot$TM_LinkID <- rep(T_TMLinks$TM_LinkID,5)
  T_Plot$TMLinkOrder <- rep(T_TMLinks$TMLinkOrder,5)
  T_Plot$Dist <- rep(T_TMLinks$Dist,5)
  T_Plot$CumDist <- rep(T_TMLinks$CumDist,5)
  
  # Merge with jts_sub to get JT_Mean and JT_Median
  T_Plot <- merge(T_Plot,
                  jts_sub[,c("Period","TM_LinkID","JTMean","JTMedian")],
                  by = c("Period","TM_LinkID"),
                  all.x=TRUE)
  
  # Clean after merge: remove duplicates and sort
  T_Plot <- unique(T_Plot)
  T_Plot <- T_Plot[order(T_Plot$Period,T_Plot$TMLinkOrder),]
  
  # Get Cumulative means and median
  # The loop is useful here to deal with NA values
  T_Plot$CumJTMean <- NA
  T_Plot$CumJTMedian <- NA
  
  N_TPlot <- nrow(T_Plot)
  
  for (i in 1:N_TPlot) {
    
    if (T_Plot$TMLinkOrder[i] == 1) {
      
      if (is.na(T_Plot$JTMean[i])) {
        T_Plot$CumJTMean[i] <- 0
      } else {
        T_Plot$CumJTMean[i] <- T_Plot$JTMean[i]
      }
      
      if (is.na(T_Plot$JTMean[i])) {
        T_Plot$CumJTMedian[i] <- 0
      } else {
        T_Plot$CumJTMedian[i] <- T_Plot$JTMean[i]
      }
      
    } else {
      
      Add_JTMean <- ifelse(is.na(T_Plot$JTMean[i]),0,T_Plot$JTMean[i])
      Add_JTMedian <- ifelse(is.na(T_Plot$JTMedian[i]),0,T_Plot$JTMedian[i])
      
      T_Plot$CumJTMean[i] <- T_Plot$CumJTMean[i-1] + Add_JTMean
      T_Plot$CumJTMedian[i] <- T_Plot$CumJTMedian[i-1] + Add_JTMedian
    }
    
  }
  
  
  ################## Return the data table to plot ##################
  
  return(T_Plot)
  
}




#############################################################################################
# Function 2: Plot data
#############################################################################################


f_GetPlot <- function(jts_link,SA_LinkID,Periods,Quantity) {
  
  # Subset
  Data_Plot <- jts_link[jts_link$Period %in% Periods,]
  
  # Variable to plot
  Data_Plot$Var <- NA
  
  if (Quantity == "Mean") {
    Data_Plot$Var <- Data_Plot$CumJTMean
  } else if (Quantity == "Median") {
    Data_Plot$Var <- Data_Plot$CumJTMedian
  } else {
    Data_Plot$Var <- 0
  }
  
  # Parameters Plot
  PlotTitle <- paste(Quantity,"Observed Journey Time on link",gsub("_","-",SA_LinkID))
  xlabel <- "Cumulative Distance (m)"
  ylabel <- "Cumulative Journey Time (minutes)"
  SizeTitle <- 22
  SizeAxes <- 16
  SizeLegend <- 16
  
  xmin <- 0
  xmax <- ceiling(max(Data_Plot$CumDist))
  xmax <- xmax + 100 - (xmax %% 100)
  
  if (xmax <= 5000) {
    incr_x <- 500
  } else if (xmax <= 10000) {
    incr_x <- 1000
  } else if (xmax <= 20000) {
    incr_x <- 2000
  } else {
    incr_x <- 5000
  }
  
  ymin <- 0
  ymax <- ceiling(max(Data_Plot$Var))
  
  if (ymax <= 5) {
    incr_y <- 0.5
  } else if (ymax <= 10) {
    incr_y <- 1
  } else if (ymax <= 20) {
    incr_y <- 2
  } else {
    incr_y <- 5
  }
  
  out_plot <- ggplot(Data_Plot, 
                     aes(x = CumDist,
                         y = Var, 
                         shape = Period,
                         color = Period)) +
    # what to plot
    geom_point(size = 4) +
    geom_line(linetype = "solid", size = 1) +
    # background color
    theme(panel.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white")) +
    # axis and grid colors
    theme(axis.line.x = element_line(colour = 'black', size = 1),
          axis.line.y = element_line(colour = 'black', size = 1),
          panel.grid.major = element_line(colour = "darkgray", size = 0.5)) +
    # shapes and colours
    scale_colour_manual(values = c("NT" = "red", "AM" = "darkblue", 
                                   "IP" = "magenta", "PM" = "brown", "EVE" = "darkgreen"))  +
    scale_shape_manual(values = c("NT" = 7, "AM" = 17, "IP" = 15, "PM" = 19, "EVE" = 3))  +
    # title and axes labels
    ggtitle(PlotTitle) +
    xlab(xlabel) +
    ylab(ylabel) +
    # layout
    theme(plot.title = element_text(size = SizeTitle, face="bold", margin = margin(t = 10, b = 20))) +
    theme(axis.title.x = element_text(size = SizeAxes, face="bold", margin = margin(t = 10, b = 20)), 
          axis.title.y = element_text(size = SizeAxes, face="bold", margin = margin(l = 10, r = 20))) +
    theme(axis.text.x = element_text(size = SizeAxes), 
          axis.text.y = element_text(size = SizeAxes)) +
    theme(legend.position = "right", legend.key.height = unit(1,"line")) +
    theme(legend.text = element_text(size = SizeLegend), 
          legend.title = element_text(size = SizeLegend, face="bold")) +
    # coordinates
    coord_cartesian(xlim = c(xmin,xmax),ylim = c(ymin,ymax)) +
    scale_x_continuous(breaks = seq(xmin,xmax,incr_x), labels = seq(xmin,xmax,incr_x),limits = c(xmin,xmax)) +
    scale_y_continuous(breaks = seq(ymin,ymax,incr_y), labels = seq(ymin,ymax,incr_y),limits = c(ymin,ymax))
    
  # return the plot
  out_plot
  
}



#############################################################################################
# Function 3: Table of what has been plotted
#############################################################################################


f_GetTable <- function(jts_link,SA_LinkID,Periods,Quantity) {
  
  # Parameters
  V_Periods <- c("NT","AM","IP","PM","EVE")
  
  # Subset
  Table_Out <- jts_link[jts_link$Period %in% Periods,]
  
  # Variable to plot
  Table_Out$Var <- NA
  Table_Out$CumVar <- NA
  
  if (Quantity == "Mean") {
    Table_Out$Var <- Table_Out$JTMean
    Table_Out$CumVar <- Table_Out$CumJTMean
  } else if (Quantity == "Median") {
    Table_Out$Var <- Table_Out$JTMedian
    Table_Out$CumVar <- Table_Out$CumJTMedian
  } else {
    Table_Out$Var <- 0
    Table_Out$CumVar <- 0
  }
  
  # Re-order columns
  Col_Order <- c("Period","TMLinkOrder","TM_LinkID","Dist","CumDist","Var","CumVar")
  Table_Out <- Table_Out[,Col_Order]
  
  
  
  # Formatting
  Table_Out$Period <- factor(Table_Out$Period,ordered = TRUE,V_Periods)
  Table_Out$Dist <- round(Table_Out$Dist)
  Table_Out$CumDist <- round(Table_Out$CumDist)
  Table_Out$Var <- round(Table_Out$Var,digits = 2)
  Table_Out$CumVar <- round(Table_Out$CumVar,digits = 2)
  
  # Sort
  Table_Out <- Table_Out[order(Table_Out$Period,Table_Out$TMLinkOrder),]
  
  # Friendly names
  Friendly_Names <- c("Period","Link Order","Link ID","Distance (m)","Cumulative Distance (m)",
                      paste(Quantity,"JT (min)"),
                      paste("Cumulative",Quantity,"JT (min)"))
  names(Table_Out) <- Friendly_Names
  
  # Returns
  return(Table_Out)
}


