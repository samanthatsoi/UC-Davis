hwdata1 <- readRDS("~/Desktop/college_scorecard_2013.rds")

dim(hwdata1)
nrow(hwdata1)
sum(hwdata1$main)
ncol(hwdata1)
var_class<-sapply(hwdata1, class)

table(var_class)
sum(var_class=="factor")
sum(var_class=="factor")
sum(var_class=="numeric")
sum(var_class=="integer")
names(var_class[var_class=="character"])
sum(is.na(hwdata1))


which.max(colSums(is.na(hwdata1)))
which.min(colSums(is.na(hwdata1)))

sum(is.na(hwdata1$avg_sat))

#length(colSums(is.na(hwdata)))

#length(sapply(as.data.frame(is.na(hwdata)), sum))

which.max(sapply(as.data.frame(is.na(hwdata1)), sum))

which.max(apply(as.matrix(is.na(hwdata1)),1, sum))
table(hwdata1$ownership)
degree_dat<-hwdata1[c("primary_degree","highest_degree","ownership")]

degree_dat_tab<-table(degree_dat)
dimnames(degree_dat_tab)

prop.table(degree_dat_tab,margin=1)
prop.table(degree_dat_tab,margin=2)
highAndOwn <-hwdata1[c("highest_degree","ownership")]
highestDeg <-prop.table(table(highAndOwn))
highestDeg
#mosaicplot(prop.table(degree_dat_tab,margin=1),color=TRUE, shade=TRUE)

barplot(highestDeg, legend=T, main = "Proportion of highest degree rewarded for each type of college")
mean(hwdata1$undergrad_pop, na.rm=TRUE)
median(hwdata1$undergrad_pop, na.rm=TRUE)
boxplot(hwdata1$undergrad_pop, na.rm=TRUE, main="Undergraduate population of colleges nationally with outliers")
quantile(hwdata1$undergrad_pop, na.rm=TRUE)
hwdata1$name[hwdata1$undergrad_pop == 166816]
boxplot(hwdata1$undergrad_pop, na.rm=TRUE,outline=FALSE, main="Undergraduate population of colleges nationally without outliers")
meantut <- mean(hwdata1$tuition, na.rm = TRUE)
tuitionCA <- hwdata1$tuition[hwdata1$state %in% "CA"]
tuitionNY <- hwdata1$tuition[hwdata1$state %in% "NY"]
tuitionFL <- hwdata1$tuition[hwdata1$state %in% "FL"]
tuitionPA <- hwdata1$tuition[hwdata1$state %in% "PA"]
tuitionTX <- hwdata1$tuition[hwdata1$state %in% "TX"]
medTX <- median(tuitionTX, na.rm=TRUE)
medPA <- median(tuitionPA, na.rm=TRUE)
boxplot(tuitionCA,tuitionNY,tuitionFL,tuitionPA,tuitionTX, names = c("CA","NY","FL","PA","TX"), xlab= "State",ylab="Tuition", main="Tuition in 5 most populous states")

spendingPub <- hwdata1$spend_per_student[hwdata1$ownership %in% "Public"]
spendingPub2 <- spendingPub[!is.na(spendingPub)]
spendingNon <- hwdata1$spend_per_student[hwdata1$ownership %in% "Nonprofit"]
spendingNon2 <- spendingNon[!is.na(spendingNon)]
spendingFor <- hwdata1$spend_per_student[hwdata1$ownership %in% "For Profit"]
spendingFor2 <- spendingFor[!is.na(spendingFor)]
earningPub <- hwdata1$avg_10yr_salary[hwdata1$ownership %in% "Public"]
earningPub2 <- earningPub[!is.na(earningPub)]
earningNon <- hwdata1$avg_10yr_salary[hwdata1$ownership %in% "Nonprofit"]
earningNon2 <- earningNon[!is.na(earningNon)]
earningFor <- hwdata1$avg_10yr_salary[hwdata1$ownership %in% "For Profit"]
earningFor2 <- earningPub[!is.na(earningFor)]
plot(spendingPub,earningPub, main = "spending vs. earning for students who attended public colleges", xlab = "spending per student in USD", ylab ="10 year earning for students in USD")
plot(spendingNon,earningNon, main = "spending vs. earning for students who attended nonprofit colleges", xlab = "spending per student in USD", ylab ="10 year earning for students in USD")
plot(spendingFor,earningFor, main = "spending vs. earning for students who attended for-profit colleges", xlab = "spending per student in USD", ylab ="10 year earning for students in USD")
hwdata1$best <- hwdata1$avg_10yr_salary/hwdata1$cost
hwdata1$name[hwdata1$avg_10yr_salary]
hwdata1$best
best <- sort(hwdata1$best,decreasing=T)[1:10]

hwdata1$best[hwdata1$name == "Auburn University at Montgomery"]
hwdata1$name[best]
hwdata1$ownership[hwdata1$name %in% c("United States Merchant Marine Academy", "Augusta University", "South Texas College", "University of Connecticut-Avery Point", "University of Connecticut-Stamford", "University of Connecticut-Tri-Campus", "Indian River State College", "Palm Beach State College")]
idealDiv <- 1/7

hwdata1$diversity <- abs((hwdata1$race_white - idealDiv)/idealDiv) + abs((hwdata1$race_black - idealDiv)/idealDiv) + abs((hwdata1$race_hispanic - idealDiv)/idealDiv) + abs((hwdata1$race_asian - idealDiv)/idealDiv) + abs((hwdata1$race_native - idealDiv)/idealDiv) + abs((hwdata1$race_pacific - idealDiv)/idealDiv) + abs((hwdata1$race_other - idealDiv)/idealDiv)
top10scores <- sort(hwdata1$diversity,decreasing=F)[1:10]
mostDivColleges <- hwdata1$name[hwdata1$diversity %in% top10scores]
bot10scores <- sort(hwdata1$diversity,decreasing=T)[1]
botDivColleges <- hwdata1$name[hwdata1$diversity %in% bot10scores]
mostDivColleges
top10scores
hwdata1$tuition[hwdata1$name %in% "University of California-Davis"]
boxplot(hwdata1$tuition,outline=FALSE, main = "Tuitions in colleges nationally and that of UC Davis", ylab = "Tuition in USD")
points(hwdata1$tuition[hwdata1$name %in% "University of California-Davis"])
boxplot(hwdata1$tuition[hwdata1$ownership %in% "Public"],outline=FALSE, main = "Tuitions in public colleges nationally and that of UC Davis", ylab = "Tuition in USD")
points(hwdata1$tuition[hwdata1$name %in% "University of California-Davis"])

boxplot(hwdata1$first_gen,outline=FALSE, main="Proportion of first generations in colleges nationally and that of UC Davis",ylab="Proportion of first gen")
points(hwdata1$first_gen[hwdata1$name %in% "University of California-Davis"])

boxplot(hwdata1$race_black,outline=FALSE, main="Proportion of blacks in colleges nationally and that of UC Davis", ylab = "Proportion of blacks")
points(hwdata1$race_black[hwdata1$name %in% "University of California-Davis"])