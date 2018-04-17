library(moments) #for skewness and kurtosis calculation
library(zoo) #for rollapply
library(psd) #for spectrum calculation

raw_label_data <- read.csv("storks_obs_train.csv", header = FALSE)
#This dataset arrange raw accelerometer data with behavior labels at the last 
#column. Each row represent one equal segementation of 4 sec data with 
#sampling of 10Hz. Each row with X,Y,Z,X,Y,Z,... sequence.

freq <- 10
#sampling frequency is 10 Hz

testdata <- c(1:dim(raw_label_data)[1])
testdata <- data.frame(testdata)

testdata$MeanX <- 0; testdata$MeanY <- 0; testdata$MeanZ <- 0
#define mean of each axis


testdata$stdX <- 0; testdata$stdY <- 0; testdata$stdZ <- 0
#standard deviation of each axis


testdata$varX <- 0; testdata$varY <- 0; testdata$varZ <- 0
#variance of each axis


testdata$icvX <- 0; testdata$icvY <- 0; testdata$icvZ <- 0
#inverse coefficient of variation. {not sure why some researchers use this 
#feature rather than coefficient of variation(cv). And another problem here is that 
#sometimes mean(X/Y/Z) is negative which is not the case with cv}


testdata$skX <- 0; testdata$skY <- 0; testdata$skZ <- 0
#skewness of axis


testdata$kurX <- 0; testdata$kurY <- 0; testdata$kurZ <- 0
#kurtosis of axis


testdata$maxX <- 0; testdata$maxY <- 0; testdata$maxZ <- 0
#maximum of axis


testdata$minX <- 0; testdata$minY <- 0; testdata$minZ <- 0
#minimum of axis


testdata$rangeX <- 0; testdata$rangeY <- 0; testdata$rangeZ <- 0
#range of axis


testdata$normX <- 0; testdata$normY <- 0; testdata$normZ <- 0
#Euclidean norm of axis. {If would like to use this feature, all samples should
#have same sampling frequency and length}


testdata$covXY <- 0; testdata$covXZ <- 0; testdata$covYZ <- 0
#covariance between axis


testdata$corXY <- 0; testdata$corXZ <- 0; testdata$corYZ <- 0
#pearson correlation between axis


testdata$meandiffXY <- 0; testdata$meandiffXZ <- 0; testdata$meandiffYZ <- 0
#average difference between two axis


testdata$stddiffXY <- 0; testdata$stddiffXZ <- 0; testdata$stddiffYZ <- 0
#standard deviation of the defference between two axis


testdata$varsbaX <- 0; testdata$varsbaY <- 0; testdata$varsbaZ <- 0
#variantion of static body acceleration. {made this up by myself, not sure if it 
#is useful}


testdata$vardbaX <- 0; testdata$vardbaY <- 0; testdata$vardbaZ <- 0
#variation of dynamic body acceleration. {also made up by myself, probably 
#correlated with variation of each axis}


testdata$odbaX <- 0; testdata$odbaY <- 0; testdata$odbaZ <- 0
#overall dynamic body acceleration of each axis. I use the average of dba
#(dynamic body acceleration) here because this will be independent with the 
#sample length.


testdata$ODBA <- 0
#Overall Dynamic Body Acceleration


testdata$mdbaX <- 0; testdata$mdbaY <- 0; testdata$mdbaZ <- 0
#maximum dynamic body acceleration of each axis


testdata$pitch <- 0
#pitch angle of the device. Need to be careful with the attachment type - 
#backpack and neckcollar would have very different pitch value.


testdata$roll <- 0
#roll angle of the device. Similar situation with pitch as described above.


testdata$meandlX <- 0; testdata$meandlY <- 0; testdata$meandlZ <- 0
#mean difference between to continuous points


testdata$vardlX <- 0; testdata$vardlY <- 0; testdata$vardlZ <- 0
#variation of difference between to continous points


testdata$maxfftX <- 0; testdata$maxfftY <- 0; testdata$maxfftZ <- 0
#dominant frequency of each axis


testdata$maxspecX <- 0; testdata$maxspecY <- 0; testdata$maxspecZ <- 0
#maximum of power spectrum of each axis


testdata$qt25X <- 0; testdata$qt25Y <- 0; testdata$qt25Z <- 0; 
#25% quantile of each axis

testdata$qt50X <- 0; testdata$qt50Y <- 0; testdata$qt50Z <- 0; 
#50% quantile of each axis

testdata$qt75X <- 0; testdata$qt75Y <- 0; testdata$qt75Z <- 0; 
#75% quantile of each axis


testdata$label <- "0"

# a simple way to test efficiency of pspectrum function in library(psd)
# t <- seq(0,0.25,0.001)
# x <- sin(2*pi*50*t) + 2*sin(2*pi*120*t)
# y <- x + 0.01*rnorm(length(t))
# spec <- pspectrum(y, x.frqsamp=1000, plot=TRUE)
# max(spec$spec)
# spec$freq[which(spec$spec >= max(spec$spec))]


for(i in 1:dim(testdata)[1])
{
  XXX <- raw_label_data[i,seq(1,length(raw_label_data[i,])-1,3)]
  YYY <- raw_label_data[i,seq(2,length(raw_label_data[i,])-1,3)]
  ZZZ <- raw_label_data[i,seq(3,length(raw_label_data[i,])-1,3)]
  
  XXX <- as.numeric(XXX)/10
  YYY <- as.numeric(YYY)/10
  ZZZ <- as.numeric(ZZZ)/10
  #transform this dataset into unit of g (gravity, 1g = 9.8 m/s2)
  
  testdata$MeanX[i] <- mean(XXX)
  testdata$MeanY[i] <- mean(YYY)
  testdata$MeanZ[i] <- mean(ZZZ)
  
  testdata$stdX[i] <- sd(XXX)
  testdata$stdY[i] <- sd(YYY)
  testdata$stdZ[i] <- sd(ZZZ)
  
  testdata$varX[i] <- testdata$stdX[i]^2
  testdata$varY[i] <- testdata$stdY[i]^2
  testdata$varZ[i] <- testdata$stdZ[i]^2
  
  testdata$icvX[i] <- sd(XXX)/mean(XXX)
  testdata$icvY[i] <- sd(YYY)/mean(YYY)
  testdata$icvZ[i] <- sd(ZZZ)/mean(ZZZ)
  
  testdata$skX[i] <- skewness(XXX)
  testdata$skY[i] <- skewness(YYY)
  testdata$skZ[i] <- skewness(ZZZ)
  
  testdata$kurX[i] <- kurtosis(XXX)
  testdata$kurY[i] <- kurtosis(YYY)
  testdata$kurZ[i] <- kurtosis(ZZZ)
  
  testdata$maxX[i] <- max(XXX)
  testdata$maxY[i] <- max(YYY)
  testdata$maxZ[i] <- max(ZZZ)
  
  testdata$minX[i] <- min(XXX)
  testdata$minY[i] <- min(YYY)
  testdata$minZ[i] <- min(ZZZ)
  
  testdata$rangeX[i] <- testdata$maxX[i] - testdata$minX[i]
  testdata$rangeY[i] <- testdata$maxY[i] - testdata$minY[i]
  testdata$rangeZ[i] <- testdata$maxZ[i] - testdata$minZ[i]
  
  testdata$normX[i] <- sqrt(sum(XXX^2))
  testdata$normY[i] <- sqrt(sum(YYY^2))
  testdata$normZ[i] <- sqrt(sum(ZZZ^2))
  
  testdata$covXY[i] <- cov(XXX, YYY)
  testdata$covXZ[i] <- cov(XXX, ZZZ)
  testdata$covYZ[i] <- cov(YYY, ZZZ)
  
  testdata$corXY[i] <- cor(XXX, YYY, method = "pearson")
  testdata$corXZ[i] <- cor(XXX, ZZZ, method = "pearson")
  testdata$corYZ[i] <- cor(YYY, ZZZ, method = "pearson")
  
  testdata$meandiffXY[i] <- mean(XXX-YYY)
  testdata$meandiffXZ[i] <- mean(XXX-ZZZ)
  testdata$meandiffYZ[i] <- mean(YYY-ZZZ)
  
  testdata$stddiffXY[i] <- sd(XXX-YYY)
  testdata$stddiffXZ[i] <- sd(XXX-ZZZ)
  testdata$stddiffYZ[i] <- sd(YYY-ZZZ)
  
  sbaX <- rollapply(XXX, 11, mean, fill = NA, align = c("center"))
  sbaY <- rollapply(YYY, 11, mean, fill = NA, align = c("center"))
  sbaZ <- rollapply(ZZZ, 11, mean, fill = NA, align = c("center"))
  #sliding window length for white stork dataset is 11, namely 1 sec data
  
  dbaX <- abs(XXX-sbaX)
  dbaY <- abs(YYY-sbaY)
  dbaZ <- abs(ZZZ-sbaZ)
  
  testdata$varsbaX[i] <- sd(sbaX-mean(sbaX, na.rm = T), na.rm = T)
  testdata$varsbaY[i] <- sd(sbaY-mean(sbaY, na.rm = T), na.rm = T)
  testdata$varsbaZ[i] <- sd(sbaZ-mean(sbaZ, na.rm = T), na.rm = T)
  
  testdata$vardbaX[i] <- sd(dbaX-mean(dbaX, na.rm = T), na.rm = T)
  testdata$vardbaY[i] <- sd(dbaY-mean(dbaY, na.rm = T), na.rm = T)
  testdata$vardbaZ[i] <- sd(dbaZ-mean(dbaZ, na.rm = T), na.rm = T)
  
  testdata$odbaX[i] <- mean(dbaX, na.rm = T)
  testdata$odbaY[i] <- mean(dbaY, na.rm = T)
  testdata$odbaZ[i] <- mean(dbaZ, na.rm = T)
  
  testdata$ODBA[i] <- testdata$odbaX[i] + testdata$odbaY[i] + testdata$odbaZ[i]
  
  testdata$mdbaX[i] <- max(dbaX, na.rm = T)
  testdata$mdbaY[i] <- max(dbaY, na.rm = T)
  testdata$mdbaZ[i] <- max(dbaZ, na.rm = T)
  
  
  sign <- ifelse(testdata$MeanZ[i] > 0, 1, -1)
  testdata$roll[i] <- atan(testdata$MeanY[i]/(sign*sqrt(testdata$MeanZ[i]^2+0.001*testdata$MeanX[i]^2)))
  #When accY and accZ are both very small, miu = 0.001 and value of accX are used to compromise.
  
  testdata$pitch[i] <- atan(-testdata$MeanX[i]/sqrt(testdata$MeanY[i]^2+testdata$MeanZ[i]^2))
  
  testdata$meandlX[i] <- mean(abs(diff(XXX)))
  testdata$meandlY[i] <- mean(abs(diff(YYY)))
  testdata$meandlZ[i] <- mean(abs(diff(ZZZ)))
  
  testdata$vardlX[i] <- sd(abs(diff(XXX)))^2
  testdata$vardlY[i] <- sd(abs(diff(YYY)))^2
  testdata$vardlZ[i] <- sd(abs(diff(ZZZ)))^2
  
  spectrX <- pspectrum(XXX, x.frqsamp = freq)
  spectrY <- pspectrum(YYY, x.frqsamp = freq)
  spectrZ <- pspectrum(ZZZ, x.frqsamp = freq)
  
  testdata$maxfftX[i] <- max(spectrX$spec)
  testdata$maxspecX[i] <- spectrX$freq[which(spectrX$spec >= testdata$maxfftX[i])]
  testdata$maxfftY[i] <- max(spectrY$spec)
  testdata$maxspecY[i] <- spectrY$freq[which(spectrY$spec >= testdata$maxfftY[i])]
  testdata$maxfftZ[i] <- max(spectrZ$spec)
  testdata$maxspecZ[i] <- spectrZ$freq[which(spectrZ$spec >= testdata$maxfftZ[i])]
  
  testdata$qt25X[i] <- quantile(XXX)[2]
  testdata$qt25Y[i] <- quantile(YYY)[2]
  testdata$qt25Z[i] <- quantile(ZZZ)[2]
  
  testdata$qt50X[i] <- quantile(XXX)[3]
  testdata$qt50Y[i] <- quantile(YYY)[3]
  testdata$qt50Z[i] <- quantile(ZZZ)[3]
  
  testdata$qt75X[i] <- quantile(XXX)[4]
  testdata$qt75Y[i] <- quantile(YYY)[4]
  testdata$qt75Z[i] <- quantile(ZZZ)[4]
  
  testdata$label[i] <- as.character(raw_label_data[i,length(raw_label_data[i,])])
}

testdata$label <- as.factor(testdata$label)
