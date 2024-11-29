# Loading the dataset
data=read.table("./ExData_Plotting1/exdata_data_household_power_consumption/household_power_consumption.txt",header=TRUE,sep=";",na.strings="?")
View(data)

# Subsetting the dataset
data=data[data$Date %in% c("1/2/2007","2/2/2007"),]

# Combining Date and Time into a single Date-Time object
data$DateTime=as.POSIXct(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")
View(data)

# Plot 1: Global Active Power Histogram
hist(as.numeric(as.character(data$Global_active_power)),col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")
dev.copy(png,filename="plot1.png")
dev.off()
