# title: qPCR analysis
# author: Orly
# date: 7/30/19


# load data and packages into document
rm(list=ls())
library(dplyr)
rm(list=ls())
path = "/Users/Orly/Desktop/Coding"
HEL <- read.csv(file =paste0(path,"/HEL_Card9_2019-05-21.txt"), sep = "\t")

HEL$Cells <- paste0("HEL", c(rep(1, 48), rep(2, 48)))


# organize columns of data by individual values
samples <- as.character(unique(HEL$Sample))
primers <- as.character(unique(HEL$Target))
Cts <- as.character(unique(HEL$Ct))
wells <- as.character(unique(HEL$Well))
cells <- as.character(unique(HEL$Cells))
treatments <- as.character(unique(HEL$Treatment))

# name all primers, samples, and treatments
ISG15 <- filter(HEL, Target=="ISG15")
GAPDH <- filter(HEL, Target=="GAPDH")
IFIT3 <- filter(HEL, Target=="IFIT3")
ISG54 <- filter(HEL, Target=="ISG54")

HELCTUT1 <- filter(HEL, Sample=="HEL CT UT1")
HELCTIFNalfa1 <- filter(HEL, Sample=="HEL CT IFNa 1")
HELCARD9UT1 <- filter(HEL, Sample=="HEL CARD9 UT1")
HELCARD9IFNalfa1 <- filter(HEL, Sample=="HEL CARD9 IFN alfa 1")
HELCTUT2 <- filter(HEL, Sample=="HEL CT UT2")
HELCTalfa2 <- filter(HEL, Sample=="HEL CT alfa 2")
HELCARD9UT2 <- filter(HEL, Sample=="HEL CARD9 UT2")
HELCARD9IFNalfa2 <- filter(HEL, Sample=="HEL Card9 IFN alfa 2")

# create new columns
HEL$Mean <- c("Mean")
HEL$StDev <- c("StDev")
HEL$dCt <- c("dCt")
HEL$ddCt <- c("ddCt")
HEL$rQ <- c("rQ")

# calculate mean for each triplicate
for (i in primers) {
  working_df <- filter(HEL, Target==i)
    for (j in samples) {
        cur_working_df <- filter(working_df, Sample==j)
        working_cts <- mean(cur_working_df$Ct)
        HEL[HEL$Target==i & HEL$Sample==j,]$Mean=working_cts
    }
}

# calculate standard deviation for each triplicate
for (i in primers) {
  working2_df <- filter(HEL, Target==i)
    for (j in samples) {
      cur_working2_df <- filter(working2_df, Sample==j)
      working2_cts <- sd(cur_working2_df$Ct)
      HEL[HEL$Target==i & HEL$Sample==j,]$StDev=working2_cts
    }
}

# calculate dCt value for each triplicate
for (i in samples) {
  for (j in primers) {
    b_mean <- as.numeric(HEL[HEL$Target=="GAPDH" & HEL$Sample==i,]$Mean)[1]
    c_mean <- as.numeric(HEL[HEL$Target==j & HEL$Sample==i,]$Mean)[1]
    HEL[HEL$Sample==i & HEL$Target==j,]$dCt= (c_mean)-(b_mean)
  }
}

# calculate ddCt value for each triplicate
for (i in cells) {
   for (j in primers) {
     no_dct <- as.numeric(HEL[HEL$Cells==i & HEL$Target==j & HEL$Treatment=="No",]$dCt)
       for (k in samples) {
         yes_dct <- as.numeric(HEL[HEL$Cells==i & HEL$Target==j & HEL$Sample==k,]$dCt)
         HEL[HEL$Cells==i & HEL$Target==j & HEL$Sample==k,]$ddCt= yes_dct-no_dct
    }
  }
}

# calculate rQ value for each triplicate
HEL$rQ= 2**-as.numeric(HEL$ddCt)

