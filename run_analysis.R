## script for analysis of samsung wearable data



xtrain <- read.table("train/X_train.txt")

ytrain <- read.table("train/y_train.txt")


xtest <- read.table("test/X_test.txt")

ytest <- read.table("test/y_test.txt")

actlabs <- read.table("activity_labels.txt")

features <- read.table("features.txt")

subtest <- read.table("test/subject_test.txt")

subtrain <- read.table("train/subject_train.txt")

xnames <- as.vector(features[,2])

names(xtest) <- xnames

names(xtrain) <- xnames

names(ytest) <- "activity"

names(ytrain) <- "activity"

ftest <- cbind(ytest,xtest)

ftrain <- cbind(ytrain,xtrain)

mayb <- rbind(ftest,ftrain)

eh <- rbind(subtest,subtrain)

names(eh) <- "subject"

mayb <- cbind(eh,mayb)

names(mayb) <- gsub("meanFreq","manFreq",names(mayb)) 

means <- grep("mean",names(mayb))

me <- names(mayb)[means]

stds <- grep("std",names(mayb))

st <- names(mayb)[stds]

stuff <- c("subject","activity", me, st)

hurr <- mayb[,stuff]

hurrs <- hurr

# hurr$activity <- gsub(1,"WALKING",hurr$activity)
# hurr$activity <- gsub(2,"WALKING_UPSTAIRS",hurr$activity)
# hurr$activity <- gsub(3,"WALKING_DOWNSTAIRS",hurr$activity)
# hurr$activity <- gsub(4,"SITTING",hurr$activity)
# hurr$activity <- gsub(5,"STANDING",hurr$activity)
# hurr$activity <- gsub(6,"LAYING",hurr$activity)
# 
# 
# hurr$activity <- as.factor(hurr$activity)


meantastic <- data.frame(matrix(0,ncol=68,nrow = 1))
names(meantastic) <- names(hurrs)

for(i in 1:length(unique(hurrs$subject))){
    chop <- hurrs[hurrs$subject == i,]
    for(j in 1:length(unique(hurrs$activity))){
        bit <- chop[chop$activity == j,]
        
        acmean <- colMeans(bit)
        acmean <- as.vector(acmean)
        acmeans <- as.data.frame(acmean,colnames = "names")
        
        ifelse(i==1 & j==1 ,meantastic[1,] <- acmean, meantastic <- rbind(meantastic,acmean))
    }
    
}

meanNames <- paste("MRSA",names(meantastic),sep = " ")

names(meantastic) <- meanNames

names(meantastic)[1:2] <- c("subject", "activity")

meantastic$activity <- gsub(1,"WALKING",meantastic$activity)
meantastic$activity <- gsub(2,"WALKING_UPSTAIRS",meantastic$activity)
meantastic$activity <- gsub(3,"WALKING_DOWNSTAIRS",meantastic$activity)
meantastic$activity <- gsub(4,"SITTING",meantastic$activity)
meantastic$activity <- gsub(5,"STANDING",meantastic$activity)
meantastic$activity <- gsub(6,"LAYING",meantastic$activity)


meantastic$activity <- as.factor(meantastic$activity)

# write.table(meantastic,"projectMeans.txt",row.names = FALSE)
