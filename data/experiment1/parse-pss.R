# Parse all the PSS file scripts

library(rprime)

files <- dir()
logs <- grep("Implicit Decision", files)

pss <- NULL

for (file in files[logs]) {
  lines <- read_eprime(file)
  print(file)
  df <- to_data_frame(FrameList(lines))
  df$Subject <- df$Subject[1]
  df$Session <- df$Session[1]
  if (is.null(pss)) {
    pss <- df
  } else {
    pss <- merge(pss, df, all=T)
  }
}

train <- subset(pss, pss$Procedure == "Trainb")
lengths <- aggregate(train$Subject, list(Trials = train$Subject), length)
names(lengths) <- c("Subject", "Length")

test <- subset(pss, !is.na(pss$StimulusPresentation2.ACC))
test$StimulusPresentation2.ACC <- as.numeric(test$StimulusPresentation2.ACC)

choose_trials <- c("AC", "AD", "AE", "AF", "CA", "DA", "EA", "FA")
avoid_trials <- c("BC", "BD", "BE", "BF", "CB", "DB", "EB", "FB")

test_choose <- subset(test, test$TrialType %in% choose_trials)
test_avoid <- subset(test, test$TrialType %in% avoid_trials)

# Inter-session reliability
test_choose1 <- subset(test, test$TrialType %in% choose_trials[1:4])
test_choose2 <- subset(test, test$TrialType %in% choose_trials[5:8])
test_avoid1 <- subset(test, test$TrialType %in% avoid_trials[1:4])
test_avoid2 <- subset(test, test$TrialType %in% avoid_trials[5:8])

# Main session aggregates
choose <- aggregate(test_choose$StimulusPresentation2.ACC,
                    list(Subject=test_choose$Subject, Session=test_choose$Session), mean)
names(choose)[3] <- "Choose"
avoid <- aggregate(test_avoid$StimulusPresentation2.ACC,
                   list(Subject=test_avoid$Subject, Session=test_avoid$Session), 
                   mean)
names(avoid)[3] <- "Avoid"

# Split trials reliability

choose1 <- aggregate(test_choose1$StimulusPresentation2.ACC,
                     list(Subject=test_choose1$Subject, Session=test_choose1$Session), mean)
names(choose1)[3] <- "Choose1"
choose2 <- aggregate(test_choose2$StimulusPresentation2.ACC,
                     list(Subject=test_choose2$Subject, Session=test_choose2$Session), mean)
names(choose2)[3] <- "Choose2"
avoid1 <- aggregate(test_avoid1$StimulusPresentation2.ACC,
                    list(Subject=test_avoid1$Subject, Session=test_avoid1$Session), 
                    mean)
names(avoid1)[3] <- "Avoid1"
avoid2 <- aggregate(test_avoid2$StimulusPresentation2.ACC,
                    list(Subject=test_avoid2$Subject, Session=test_avoid2$Session), 
                    mean)
names(avoid2)[3] <- "Avoid2"

final <- merge(choose, avoid, all=T)
final <- merge(final, lengths, all=T)
# The split trails
final <- merge(final, choose1, all=T)
final <- merge(final, choose2, all=T)
final <- merge(final, avoid1, all=T)
final <- merge(final, avoid2, all=T)

write.table(final, "pss-aggregated2.txt", quote=F, col.names=T, row.names=F, sep="\t")