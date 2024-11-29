# Loading the dataset
data=read.table("./ExData_Plotting1/exdata_data_household_power_consumption/household_power_consumption.txt",header=TRUE,sep=";",na.strings="?")
View(data)

# Subsetting the dataset
data=data[data$Date %in% c("1/2/2007","2/2/2007"),]

# Combining Date and Time into a single Date-Time object
data$DateTime=as.POSIXct(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")
View(data)

par(mfrow=c(2,2))

# Plot 4: Combined plots

with(data,{
  # Plot 1: Global Active Power Vs Time
  plot(data$DateTime,as.numeric(as.character(data$Global_active_power)),type="l",xlab="",ylab="Global Active Power(kilowatts)",xaxt="n")
  axis.POSIXct(1,at=seq(min(data$DateTime),max(data$DateTime)+86400,by="day"),format="%a")
  
  # Plot 2: Voltage over time
  plot(DateTime,as.numeric(as.character(Voltage)),type="l",xlab="datetime",ylab="Voltage",xaxt="n")
  axis.POSIXct(1,at=seq(min(data$DateTime),max(data$DateTime)+86400,by="day"),format="%a")
  
  # Plot 3: Energy sub-metering
  plot(data$DateTime,data$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering",xaxt="n")
  with(data,lines(DateTime,as.numeric(as.character(Sub_metering_1))))
  with(data,lines(DateTime,as.numeric(as.character(Sub_metering_2)),col="red"))
  with(data,lines(DateTime,as.numeric(as.character(Sub_metering_3)),col="blue"))
  axis.POSIXct(1,at=seq(min(data$DateTime),max(data$DateTime)+86400,by="day"),format="%a")
  legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=1)
  
  # Plot 4: Global Reactive Power over time
  plot(DateTime,as.numeric(as.character(Global_reactive_power)),type="l",xlab="datetime",ylab="Global Reactive Power",xaxt="n")
  axis.POSIXct(1,at=seq(min(data$DateTime),max(data$DateTime)+86400,by="day"),format="%a")
})
dev.copy(png, filename="plot4.png")
dev.off()