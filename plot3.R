# Loading the dataset
data=read.table("./ExData_Plotting1/exdata_data_household_power_consumption/household_power_consumption.txt",header=TRUE,sep=";",na.strings="?")
View(data)

# Subsetting the dataset
data=data[data$Date %in% c("1/2/2007","2/2/2007"),]

# Combining Date and Time into a single Date-Time object
data$DateTime=as.POSIXct(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")

# Plot 3: Energy sub metering
plot(data$DateTime,data$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering",xaxt="n")
with(data,lines(DateTime,as.numeric(as.character(Sub_metering_1))))
with(data,lines(DateTime,as.numeric(as.character(Sub_metering_2)),col="red"))
with(data,lines(DateTime,as.numeric(as.character(Sub_metering_3)),col="blue"))
axis.POSIXct(1,at=seq(min(data$DateTime),max(data$DateTime)+86400,by="day"),format="%a")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=1)
dev.copy(png,filename="plot3.png")
dev.off()
