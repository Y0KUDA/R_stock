###########################################
#######Stock Historical Analyzer###########
###########################################
#--MENU--
#Data format: StockHistoricalData
#Function download Historical Data from Google and initialize for format : LoadStockDatFromGoogle
#Function Load Historical Data from Local HDD that downloaded(from Stooq) and initialize for format : LoadStockDatFromStooq
#
#Useful Functions
#	*StockMap : higher-order function for stock data.Mapping function takes reference to Stock and Ordinal number.
#	*StockBasePlot : Plot Stock chart simply. It takes only Stock data.
#	*StockChartPlot : Plot something technical chart data. Use after using StockBasePlot function.
#	*StockWriteDate : Write DATE to chart. It takes a number that how many you want write.
#
#
#
#
#



##stock historical data class
setClass (
  "StockHistoricalData",
  representation (
    StockExchange = "character",
    TickerSymbol="character",
    Interval="integer",
    From="integer",
    To="integer",
    Period="integer",
    Data="data.frame" #date/close/high/low/open/volume
  ),
  prototype = list(
    StockExchange = as.character(NULL),
    TickerSymbol=as.character(NULL),
    Interval=as.integer(NULL),
    From=as.integer(NULL),
    To=as.integer(NULL),
    Period=as.integer(NULL),
    Data=as.data.frame(list(NULL))
  )
)
######################################
####google finance api################
######################################
#this system has 3 process.
#1. download raw data from google finance
#2. preprocess the data
#3. using two functions and create new StockHistoricalData object...
library(RCurl)##############################load Curl#####
DownloadStockData=function(StockExchange,TickerSymbol,Interval,Period){
	src <- paste("http://finance.google.com/finance/getprices?x=",StockExchange,
				"&q=",TickerSymbol,
				"&i=",Interval,
				"&p=",Period,"d",sep="")
	return (getURL(src))
}
FormatData=function(rawdata){
	i=regexpr("TIMEZONE_OFFSET",rawdata)[1]
	if(i==-1){
		return(NULL)
	}
	data=substr(rawdata,i+1,nchar(rawdata))
	i=regexpr("\n",data)[1]
	data=substr(data,i+1,nchar(data))
	data=read.csv(text=data,header=FALSE,col.names=c("DATE","CLOSE","HIGH","LOW","OPEN","VOLUME"),stringsAsFactors=FALSE)
	return (data)
}
LoadStockDatFromGoogle=function(StockExchange,TickerSymbol,Interval,Period){
	rawdata=DownloadStockData(StockExchange,TickerSymbol,Interval,Period)
	dat=FormatData(rawdata)
	if(is.null(dat)){
		return(NULL)
	}
	base=0
	for(i in 1:length(dat$DATE)){##Rewirte weird Unix Date...
		if(!is.na(charmatch("a",dat$DATE[i]))){
			dat$DATE[i]=sub("a","",dat$DATE[i])
			base=as.integer(dat$DATE[i])
		}else{
			dat$DATE[i]=base+as.integer(dat$DATE[i])*as.integer(Interval)
		}
	}
	
	ret=new("StockHistoricalData",
			StockExchange=StockExchange,
			TickerSymbol=TickerSymbol,
			Interval=as.integer(Interval),
			#From=as.integer(dat$DATE[1]),
			#To=as.integer(dat$DATE[length(dat$DATE)]),
			#Period=To-From
			Data=dat
	)
	ret@From=as.integer(dat$DATE[1])
	ret@To=as.integer(dat$DATE[length(dat$DATE)])
	ret@Period=ret@To-ret@From
	return (ret)
}
##stc=DLStockDat("NASDAQ","AAPL",60,1)


#check the opning days
#plot(stc@Data$DATE,1:length(stc@Data$DATE)*0)

#check the sampled days
#for(i in stc@Data$DATE) print(as.POSIXct(as.integer(i), origin="1970-01-01"))

#cat(rawdata) show string with linefeed




######################################################
####Downloaded Stooq Stock Historical Data api########
######################################################

#Some DATEs are missing. These must be Completed.
LoadStockDatFromStooq=function(StockExchange,TickerSymbol,Interval,Period){ ##Interval has one of (300,3600,86400)
	country=""
	if(tolower(StockExchange)=="nyse"||tolower(StockExchange)=="nasdaq"){
		country="us"
	}else if(tolower(StockExchange)=="tse"){
		country="jp"
	}
	path="./Stooq"
	if(Interval==300){path=paste(path,"/5 min",sep="")}
	else if(Interval==3600){path=paste(path,"/hourly",sep="")}
	else if(Interval==86400){path=paste(path,"/daily",sep="")}
	path1=paste(path,"/",country,
				"/",tolower(StockExchange),
				" stocks","/1/",
				sub("\\.","-",tolower(TickerSymbol)),".",country,".txt",sep="")
	path2=paste(path,"/",country,
				"/",tolower(StockExchange),
				" stocks","/2/",
				sub("\\.","-",tolower(TickerSymbol)),".",country,".txt",sep="")
	path3=paste(path,"/",country,
				"/",tolower(StockExchange),
				" stocks",
				"/",sub("\\.","-",tolower(TickerSymbol)),".",country,".txt",sep="")
	if(file.exists(path1)){dat=read.csv(path1,header=TRUE,stringsAsFactors=FALSE)}
	else if(file.exists(path2)){dat=read.csv(path2,header=TRUE,stringsAsFactors=FALSE)}
	else if(file.exists(path3)){dat=read.csv(path3,header=TRUE,stringsAsFactors=FALSE)}
	else{return (NULL)}
	
	colnames(dat)=toupper(colnames(dat))
	
	HasTime=FALSE
	if(is.na(charmatch("TIME",colnames(dat)))){}
	else{HasTime=TRUE}
	if(is.null(dat)){
		return(NULL)
	}
	
	for(i in 1:length(dat$DATE)){##Rewirte weird Unix Date...
		if(HasTime){dat[i,"DATE"]=as.numeric(as.POSIXct(paste(dat[i,"DATE"]," ",dat[i,"TIME"],sep=""), format="%Y-%m-%d %H:%M:%S"))}
		else{dat[i,"DATE"]=as.numeric(as.POSIXct(paste(dat[i,"DATE"]), format="%Y%m%d"))   }
	}
	
	dat=dat[setdiff(colnames(dat), "TIME")]
	dat=dat[setdiff(colnames(dat), "OPENINT")]

	ret=new("StockHistoricalData",
			StockExchange=StockExchange,
			TickerSymbol=TickerSymbol,
			Interval=as.integer(Interval),
			#From=as.integer(dat$DATE[1]),
			#To=as.integer(dat$DATE[length(dat$DATE)]),
			#Period=To-From
			Data=dat
	)
	ret@From=as.integer(dat$DATE[1])
	ret@To=as.integer(dat$DATE[length(dat$DATE)])
	ret@Period=ret@To-ret@From
	return (ret)
}

##stc=LoadStockDatFromStooq("NASDAQ","AAPL",300,100)
##stc=LoadStockDatFromStooq("NYSE","LGF-A",86400,100)
##stc=LoadStockDatFromStooq("NYSE","BRK-A",86400,3650)

StockMap=function(Stock,f){
	ret=NULL
	for(i in 1:length(Stock@Data$DATE))ret[i]=f(Stock,i)
	return (ret)
}

StockBasePlot=function(Stock,from=1,to=0,HIGH=TRUE,LOW=TRUE,HIGHCOL="BLUE",LOWCOL="RED"){
	if(to==0)to=length(Stock@Data$DATE)
	top=max(Stock@Data$HIGH[from:to])
	bottom=min(Stock@Data$LOW[from:to])
	if(HIGH){
		plot(Stock@Data$DATE[from:to],Stock@Data$HIGH[from:to],
				xlim=c(Stock@Data$DATE[from],Stock@Data$DATE[to]),ylim=c(bottom,top),
				type='l',col=HIGHCOL,
				xlab="DATE",ylab="VALUE",
				cex.lab=0.8,xaxt="n") ##write date by another function.
		}
	if(LOW){par(new=T)
		plot(Stock@Data$DATE[from:to],Stock@Data$LOW[from:to],
				xlim=c(Stock@Data$DATE[from],Stock@Data$DATE[to]),ylim=c(bottom,top),
				type='l',col=LOWCOL,
				xlab=ifelse(HIGH,"","DATE"),ylab=ifelse(HIGH,"","VALUE"),
				cex.lab=0.8,xaxt="n",yaxt=ifelse(HIGH,"n","s"))}
}

#StockBasePlot(stc,3000,4000,TRUE,TRUE)


StockWriteDate=function(Stock,from=1,to=0,split=2,las=3){ ##DATE has nonuniform. Some DATE data is missing.
	if(to==0)to=length(Stock@Data$DATE)
	points=as.integer(((0:(split))/split *(to-from))+from)
	axis(1,at=Stock@Data$DATE[points],labels=as.POSIXct(Stock@Data$DATE[points], origin="1970-01-01"),las=3,cex.axis=0.8)
}

#StockWriteDate(stc,3000,4000,10)

StockChartPlot=function(Stock,Chart,from=1,to=0,COL="BLACK"){
	if(to==0)to=length(Stock@Data$DATE)
	top=max(Stock@Data$HIGH[from:to])
	bottom=min(Stock@Data$LOW[from:to])
	plot(Stock@Data$DATE[from:to],Chart[1:(to-from+1)],
			xlim=c(Stock@Data$DATE[from],Stock@Data$DATE[to]),ylim=c(bottom,top),
			type='l',col=COL,
			xlab="",ylab="",
			xaxt="n",yaxt="n")
}





