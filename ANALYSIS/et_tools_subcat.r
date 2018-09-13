readFixationReport <- function(filename, screenSize=c(1024,768), 
                               X="CURRENT_FIX_X", Y="CURRENT_FIX_Y") {
	#this function reads in a fixation report specified by filename
	# and does some clean-up
	#optional input argument screenSize is used for excluding off-screen fixations
	#optional input argument TargetLoc identifies the column name for the location of the target image
	#optional input argument CompLoc identifies the column name for the location of the competitor image

	gaze<-read.table(filename,header=TRUE,sep="\t",na.strings=".")

	#mark off screen fixations as NA
	# gaze[,X][gaze[,X] > screenSize[1]] <- NA
	# gaze[,X][gaze[,X] < 0] <- NA
	# gaze[,Y][gaze[,Y] > screenSize[2]] <- NA
	# gaze[,Y][gaze[,Y] < 0] <- NA
	#measure calibration goodness by computing: 
	#	proportion non-fixation time
	#	proportion of off-screen fixations
	temp <- ddply(gaze,.(SUBJECT,TARGET), summarize, fixTime = sum(CURRENT_FIX_DURATION),totalTime = max(CURRENT_FIX_END))
	calibGoodness <- ddply(temp, .(SUBJECT), summarize, nonFixTime = mean(1 - (fixTime/totalTime)))
	
	nSubjects<-calibGoodness$SUBJECT
	oob_prop <- matrix(nrow=length(nSubjects), ncol=1)
	for (i in 1:length(nSubjects)){
		temp <- subset(gaze,SUBJECT==calibGoodness$SUBJECT[i])
		oob = (temp[,X] > screenSize[1] | temp[,X] < 0 | temp[,Y] > screenSize[2] | temp[,Y] < 0)
		oob_prop[i] = sum(temp$CURRENT_FIX_DURATION[oob])/sum(temp$CURRENT_FIX_DURATION)		
	}
	calibGoodness <- data.frame(calibGoodness, oob_prop = oob_prop)
	print(calibGoodness)

	return(gaze)
}

behavioralAnalysis <- function(gaze,by="subjects"){
	#culls the important behavioral data from the fixation report
	#returns subject-x-condition means or item means with condition info (if by="items")

	#extract just the relevant columns
	dataRaw<-subset(gaze,select=c("SUBJECT","COND","TARGET","ACC","RT"))
	dataFull<-unique(dataRaw) #drop repeat rows (since there are multiple fixations per trial)

	if (by == "subjects") {
		x<-ddply(dataFull,.(SUBJECT, COND), summarize, 
		  RT = mean(RT[ACC==1]), ACC = mean(ACC))
	} else {
		x<-ddply(dataFull,.(TARGET, COND), summarize, 
			RT = mean(RT[ACC==1]), ACC = mean(ACC))
	}

	return(x)
}

assignIA <- function(gaze) {
	# create a column for each type of object
	gaze$T <- gaze$FIXATED_OBJECT == "trg"
	gaze$D <- gaze$FIXATED_OBJECT == "dis"
	gaze$C <- gaze$FIXATED_OBJECT == "coh"
	# gaze$X <- gaze$FIXATED_OBJECT == "ctr"
  # gaze$O <- gaze$FIXATED_OBJECT == "other"
  gaze$O <- gaze$FIXATED_OBJECT %in% c("other","ctr")

	return(gaze)
}

expandFixList <- function(d, binSize=20){
  #convenience function called by binify fixations
  timeBin<-(ceiling(d$CURRENT_FIX_START/binSize):ceiling(d$FixEnd/binSize))
  data.frame(timeBin=timeBin,FixationID=d$FixationID)
}

binifyFixations <- function(gaze, binSize=20, keepCols=c("SUBJECT","TRIAL","TARGET","T"), maxTime=NULL){
	# convert a list of fixations to bins
	# binSize determines the size of each bin in ms
	# keepCols determines which columns from the original data frame will show up in the output
	#	will no longer need fixation start and duration, nor fixation location coordinates
	#
	# maxTime can be used to cut down trial length
	#

	#need to know when fixations end
	if ("CURRENT_FIX_END" %in% names(gaze)) { 
		gaze$FixEnd <- gaze$CURRENT_FIX_END
	} else {
		#compute end of fixation from start and duration
		gaze$FixEnd <- gaze$CURRENT_FIX_START + gaze$CURRENT_FIX_DURATION
	}
	#if maxTime is defined, do some trimming
	if (!is.null(maxTime)) {
		#drop all fixations that start after the maxTime
		gaze<-subset(gaze,CURRENT_FIX_START < maxTime)
		#trim fixation end times to be less than maxTime
		gaze$FixEnd[gaze$FixEnd>maxTime]<-maxTime
	}

	#make a fixation ID variable that is just the fixation number in the overall data frame
	gaze$FixationID <- 1:nrow(gaze)

  data <- ddply(idata.frame(gaze), .(FixationID), expandFixList, binSize=binSize)

	#there is a border case in which two redundant bins can be generated
	#clean them up by keeping the second one
	data<-subset(data,timeBin[2:length(timeBin)]!=timeBin[1:(length(timeBin)-1)])

	#combine data
	dataFull <- merge(data,gaze[,c(keepCols,"FixationID")],by="FixationID")

	#add a variable with actual time instead of time bin
	dataFull$Time <- dataFull$timeBin*binSize

	return(dataFull)
}

computeFixProp <- function(gazeBins, by="SUBJECT"){
  #take binified raw data and convert to mean fixation proportions
  #meanFix uses total number of trials (i.e., padding shorter trials with 0)
  #meanFixBob uses number of ongoing trials, as done by Bob McMurray
  #output also includes sumFix and N columns for using logistic regression
  
  #default to doing by subjects
  
  #melt the data frame into a "long" format
#   gazeM<-melt(gazeBins, id=c("Time","SUBJECT","COND","TARGET","TRIAL"), 
#               measure=c("T","D","C","X","O"), variable_name = "FIX_OBJECT", na.rm=T) 
  gazeM<-melt(gazeBins, id=c("Time","SUBJECT","COND","TARGET","TRIAL"), 
              measure=c("T","D","C","O"), variable_name = "FIX_OBJECT", na.rm=T)
  #rename the value column and make it a number
  gazeM$Fix<-as.numeric(gazeM$value) 
  
  if (by=="SUBJECT"){
    #compute number of trials for each subject-by-condition combination
    nTrials <- ddply(gazeBins,.(SUBJECT, COND), summarize, N = length(unique(TRIAL)))
    #merge fixation and number-of-trials data
    gazeM2<-merge(gazeM,nTrials)
    # double N for distractors
    gazeM2$N[gazeM2$FIX_OBJECT=="D"] <- gazeM2$N[gazeM2$FIX_OBJECT=="D"]*2
    #compute subject means over trials (SE calculation added by Monica)
    gazeS<-ddply(gazeM2,.(SUBJECT, COND, FIX_OBJECT, Time), summarize, 
                 meanFix=sum(Fix)/unique(N), 
                 seFix=sd(c(rep(1,each=sum(Fix)),rep(0,each=unique(N)-sum(Fix))))/sqrt(unique(N)), 
                 sumFix=sum(Fix), 
                 N=unique(N)) 
  } else { #by items version
    #compute number of trials for each target (condition is between-items)
    nTrials <- ddply(gazeBins,.(TARGET, COND), summarize, N = length(unique(interaction(SUBJECT,TRIAL))))
    #merge fixation and number-of-trials data
    gazeM2<-merge(gazeM,nTrials)
    # double N for distractors
    gazeM2$N[gazeM2$FIX_OBJECT=="D"] <- gazeM2$N[gazeM2$FIX_OBJECT=="D"]*2
    #compute target means over trials (SE calculation added by Monica)
    gazeS<-ddply(gazeM2,.(TARGET, COND, FIX_OBJECT, Time), summarize, 
                 meanFix=sum(Fix)/unique(N), 
                 seFix=sd(c(rep(1,each=sum(Fix)),rep(0,each=unique(N)-sum(Fix))))/sqrt(unique(N)), 
                 sumFix=sum(Fix), 
                 N=unique(N))
  }
  return(gazeS)
}