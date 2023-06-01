readPodCSVDataPower = function(data_dir,file_name) {  
        #data=read.csv(paste(data_dir,file_name,sep=""),header=TRUE,na.strings="NULL", colClasses=c("character","numeric","numeric",rep("character",7),"numeric","character",rep("numeric",2),"character"),na.strings='null')
        data=read.csv(paste(data_dir,file_name,sep=""),header=FALSE,na.strings="NULL", colClasses=c(rep("character",15),na.strings='null'))
        colnames(data)=c("timestamp","hour_key","date_key","datacenter","superpod","pod","hostname","role","model","rack_number","rack_upos","room_number","power1","power2","source_tstamp")
        
        data$timestamp = as.POSIXct(as.numeric(data$timestamp)/1000,tz="UTC",origin="1970-01-01")
        #Round to the next minute
        data$timestamp = as.POSIXct(60*round(as.numeric(data$timestamp)/(60)),tz="UTC",origin="1970-01-01")
        data$power1 = as.numeric(as.character(data$power1))
        data$power2 = as.numeric(as.character(data$power2))
        
        data = data[ order(data[,"hostname"], data[,"timestamp"]), ]
        
        non_narows=which(!is.na(data$power1) & !is.na(data$power2))
        data = data[non_narows,]        
                
        ##Need to remove duplicates!!
        deriv_time= data$timestamp[2:length(data$timestamp)]-data$timestamp[1:(length(data$timestamp)-1)]
        data = data[which(deriv_time>0 & (data$hostname[2:length(data$hostname)] == data$hostname[1:(length(data$hostname)-1)])),] 
        
        data$total_power = data$power1+ data$power2
        data= data[,c("timestamp","datacenter","superpod","pod","hostname","role","model","rack_number","rack_upos","room_number","total_power")]
             
        return(data)            
}