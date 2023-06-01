readHostCSVDataCpu = function(data_dir,file_name) {  
        #data=read.csv(paste(data_dir,file_name,sep=""),header=TRUE,na.strings="NULL", colClasses=c("character","numeric","numeric",rep("character",7),"numeric","character",rep("numeric",2),"character"),na.strings='null')
        data=read.csv(paste(data_dir,file_name,sep=""),header=FALSE,na.strings="NULL", colClasses=c(rep("character",8)))#,na.strings='null'
        colnames(data)=c("date_key","hour_key","minute_key","pod","superpod","datacenter","hostname","cpu")
        
        data$timestamp = as.POSIXct(paste(data$date_key," ",data$hour_key,":",data$minute_key,sep=""), format="%Y%m%d %H:%M",tz="UTC",origin="1970-01-01")
        data$cpu = as.numeric(as.character(data$cpu))
        data=data[,c("timestamp","pod","superpod","datacenter","hostname","cpu")]
        
        data = data[ order(data[,"hostname"], data[,"timestamp"]), ]
        
        non_narows=which(!is.na(data$cpu))
        data = data[non_narows,]        
                
        ##Need to remove duplicates!!
        deriv_time= data$timestamp[2:length(data$timestamp)]-data$timestamp[1:(length(data$timestamp)-1)]
        data = data[which(deriv_time>0 & (data$hostname[2:length(data$hostname)] == data$hostname[1:(length(data$hostname)-1)])),]    
        
        #Remove values >100%
        data=data[data$cpu<=100,]
          
        return(data)            
}