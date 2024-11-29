# Loading the dataset
data=read.table("./ExData_Plotting1/exdata_data_household_power_consumption/household_power_consumption.txt",header=TRUE,sep=";",na.strings="?")
View(data)

# Subsetting the dataset
data=data[data$Date %in% c("1/2/2007","2/2/2007"), ]

# Combining Date and Time into a single Date-Time object
data$DateTime=as.POSIXct(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")

# Plot 2: Global Active Power Vs Time
plot(data$DateTime,as.numeric(as.character(data$Global_active_power)),type="l",xlab="",ylab="Global Active Power(kilowatts)",xaxt="n")
axis.POSIXct(1,at=seq(min(data$DateTime),max(data$DateTime)+86400,by="day"),format="%a")
dev.copy(png, filename="plot2.png")
dev.off()
