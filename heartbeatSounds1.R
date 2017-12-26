# followed example for using tuneR
#http://samcarcagno.altervista.org/blog/basic-sound-processing-r/

library(tuneR)
library(readr)

# Functions for analysis and plotting are between the hastags
###################################################################################################
plot_sqrd_wave <- function(w,x) {
  s1 <- (w@left)^2
  s1 <- s1 / 2^(w@bit -1)
  timeArray <- (0:(length(s1) -1)) / w@samp.rate
  p <- plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = as.character(x)) 
  return(p)
}
plot_all_sqrd_waves <- function(x) {
  for(i in 1:length(x$fname)) {
    wave <- readWave(x$fname[i])
    plot_sqrd_wave(wave,x$fname[i])
    rm(wave)
    #print(i)
  }
}
plot_wave <- function(w,x) {
  s1 <- w@left
  s1 <- s1 / 2^(w@bit -1)
  timeArray <- (0:(length(s1) -1)) / w@samp.rate
  p <- plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude', main = as.character(x)) 
  return(p)
}
plot_all_waves <- function(x) {
  for(i in 1:length(x$fname)) {
    wave <- readWave(x$fname[i])
    plot_wave(wave,x$fname[i])
    rm(wave)
    #print(i)
  }
}
waveData <- function(x) {
  for(i in 1:length(x$fname)) {
    wave <- readWave(x$fname[i])
    x$numOfSamples[i] <- length(wave@left)
    x$sampleRate[i] <- wave@samp.rate
    x$bit[i] <- wave@bit
    rm(wave)
    # print(i)
  }
  return(x)
}
###################################################################################################

setwd("~/Desktop/R/HeartbeatSounds/data")

# change the working directory to the data folder for this
# project.
seta_df <- read_csv("~/Desktop/R/HeartbeatSounds/data/set_a.csv")

trainA_df <- seta_df[1:124,]


trainA_df$numOfSamples <- c(1:length(trainA_df$fname))
trainA_df$sampleRate <- c(1:length(trainA_df$fname))
trainA_df$bit <- c(1:length(trainA_df$fname))


# new dataset
data <- waveData(trainA_df)

# Printing all of the normal plots to a pdf
pdf("HeartbeatSoundsPlots.pdf")
plot_all_waves(trainA_df)
dev.off()

# Printing all of the squared  plots to a pdf
pdf("HeartbeatSoundsPlotsSquared.pdf")
plot_all_sqrd_waves(trainA_df)
dev.off()


snd <- readWave(seta_df$fname[93])
s1 <- snd@left
s1 <- s1 / 2^(snd@bit -1)

timeArray <- (0:(length(s1) -1)) / snd@samp.rate

timeArray1 = timeArray*10000
#scale to milliseconds
#timeArray <- timeArray * 1000 

plot(timeArray, s1^2, type='l', col='black', xlab='Time (ms)', ylab='Amplitude') 

