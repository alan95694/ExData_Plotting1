plot3 <- function() {
    
    fnamePlot = "plot3.png"
    
    # specify dates of interest, use data from and between below dates
    date_n1 <- as.Date("2007-02-01", "%Y-%m-%d")
    date_n2 <- as.Date("2007-02-02", "%Y-%m-%d")

    # data folder to load and save into
    dataFolder      <- "E:/CourseRA/DataScienceSpecialization/Class4_ExploratorAnalysis/week_1_project/ExData_Plotting1/"    
    # file to save subseted data into, speeds rerunning
    date_SubFile    <- paste0("subData_", as.character(date_n1), 
                              "_to_", as.character(date_n2), ".csv")

    if (!file.exists(paste0(dataFolder, date_SubFile))) {
        # --- Check local computer has sufficent ram ---
        # file dimensions are known quantities
        reqRAM_bytes    <- (2075259 * 9) * 8
        reqRAM_MB       <- reqRAM_bytes / (2^20)
        if (reqRAM_MB > memory.size()){
            stop("insufficent memory on local computer to load data")
        }
    
        print("Start of data importation")
        data_raw    <- read.table( 
                        paste0(dataFolder, "/household_power_consumption.txt"), 
                        na.strings = "?", sep = ";", header = TRUE)
        print("Data imported")
        
        # Remove bad points
        data_raw <- data_raw[complete.cases(data_raw), ]

        # make reclass date 
        data_raw$Date <- as.Date(data_raw$Date, format="%d/%m/%Y")
        
        # --- Subset data to dates of interest ---
        data_Rng    <- subset(data_raw, 
            data_raw$Date >= date_n1 & data_raw$Date <= date_n2)

        # Check length of data 
        days <- 2
        day2Hr <- 24
        Hr2Min <- 60
        print(paste0("Two days worth of data points are: ", 
                     as.character(days * day2Hr * Hr2Min), " long."))
        print(paste0("length of subseted data ", 
                     as.character(NROW(data_Rng$Date))))
        
        # vv <- c(1,2,3,4)
        # print(paste(data_Rng$Date[vv], data_Rng$Time[vv]))
        # vv <- c(2877, 2878, 2879, 2880)
        # print(paste(data_Rng$Date[vv], data_Rng$Time[vv]))

        # --- save subsetted data ---
        write.csv(data_Rng, file = paste0(dataFolder, date_SubFile))
    }
    else {
        data_Rng <- read.csv(file = paste0(dataFolder, date_SubFile) )
    }

    print( names(data_Rng) )

    # --- plot power data ---
    data_Rng$tmPOSIXlt <- as.POSIXlt( paste(data_Rng$Date, data_Rng$Time), 
                  format = "%Y-%m-%d %H:%M:%S")

    
    with(data_Rng, plot(tmPOSIXlt, Sub_metering_1, 
         main = "", type = "n",
         xlab = "", 
         ylab = "Energy sub metering"))
    
    with(data_Rng, lines(tmPOSIXlt, Sub_metering_1, 
            col = "black"))
    with(data_Rng, lines(tmPOSIXlt, Sub_metering_2, 
            col = "red"))
    with(data_Rng, lines(tmPOSIXlt, Sub_metering_3, 
            col = "blue"))
    legend("topright", 
           lty=c(1,1,1), 
           col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
            
    # --- save to file ---
    dev.copy(png, paste0(dataFolder, fnamePlot))
    dev.off()
    
    
    return(TRUE)
}

















