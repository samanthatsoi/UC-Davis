library(corrplot)
###Q1

##convert columns to appropriate data types and allow users to specify which file they want to load.
read_digits <- function(infile) 
{ 
  dataname <- read.table(infile, header=FALSE) 
  dataname[] <- lapply(dataname,function(x) as.numeric(x))
  
  return (dataname)
}

#load test and train data
test <-read_digits("~/Desktop/Downloads/digits/test.txt")
train <- read_digits("~/Desktop/Downloads/digits/train.txt")

### Q2

#displays one observation (one digit) from the data set as a grayscale image
view_digit = function(vals)
{
  m<-matrix(vals,16,16,byrow=TRUE)
  colors = rgb((255:0)/255, (255:0)/255, (255:0)/255)
  m = t(m)  # transpose the image
  m = m[,nrow(m):1]  # turn up-side-down
  image(m, col = colors, xaxt = "n", yaxt = "n")
}

#subsetting, the first column is the digit itself
test2 <- test[,(2:257)]
train2 <- train[,(2:257)]

###Q3a

digit_list<-split(train,train$V1)

#finding the average for each item
avg_digits<-lapply(1:length(digit_list),function(x)apply(digit_list[[x]],2,mean))
avg_digitsdf <- data.frame(avg_digits)
names(avg_digitsdf) <- avg_digitsdf["V1",] #change the names of the column to each digit
temp <- avg_digitsdf[2:257,] #subset it so the digits are not included

#scaling images to fit to page
par(mfrow=c(1,4), mar=c(10,1,10,2)) 

#display the average of each digit
for(i in 1:length(digit_list)){
  view_digit(temp[[i]])
}
par(mfrow=c(1,1))


###Q3b

#cleaning up
digit_correlations<-cor(do.call(cbind,avg_digits))
rownames(digit_correlations)<-0:9
colnames(digit_correlations)<-0:9

#correlation plot
corrplot(digit_correlations,method="number",type="upper")

#finding variance, using tail to find highest variance
pixel_var<-apply(train2, 2,var)
tail(sort(pixel_var), n=5)
#using head to find smallest variance
head(sort(pixel_var), n=5)
### Q4
full<-rbind(test,train)
distmat<-as.matrix(dist(full))

#need to decide which nearest neighbor, because there could be ties for max freq
decide_NN<-function(NN_set){
  NN_table<-table(NN_set) #put into table, has its frequencies of the digits
  max_digits<-which(NN_table==max(NN_table)) #finds the digit that appears most in the table. if there's a tie, which will get both the elements
  best_digits<-names(NN_table)[max_digits] #need to be done for ties
  knn_pred<-sample(best_digits,1) #if it's a tie, then randomly choose one
  return(knn_pred)
}

#uses k-nearest neighbors to predict the label for a point or collection of points.
predict_knn<-function(train,test,distance,k){
  print(paste0("k=",k))
  distance<-distance[1:nrow(test),-c(1:nrow(test))]
  ordmat<-apply(distance,1,order) #find the distance and order it by indices
  k_NN<-matrix(train[unlist(ordmat[1:k,]),1],nrow(test),k,byrow=TRUE) #put the matrix into vector and then back to the vector again
  knn_preds<-apply(k_NN,1,decide_NN)
  return(knn_preds)
}
### Q5
### to figure out the error 

#creating fold, so it doesn't have do it numerous times
single_cv<-function(folds,i,k,distance_mat){
  distance<-distance_mat[folds==i,folds!=i]
  test2<-full[folds==i,]
  train2<-full[folds!=i,]
  knn_pred<-predict_knn(train2,test2,distance,k)
  return(knn_pred)
}


#using single_cv
full_cv<-function(n,k,distance_mat){
  folds<-sort(rep_len(1:n,nrow(full)))
  full_predictions<-unlist(sapply(1:n,function(i)single_cv(folds,i,k,distance_mat)))
  return(full_predictions)
}

# uses 10-fold cross-validation to estimate the error rate for k-nearest neighbors
cv_error_knn<-function(folds,k,distance_mat){
  print(paste0("k=",k))
  temp_cv<-full_cv(folds,k,distance_mat)
  error<-(1-sum(diag(table(full$V1,temp_cv)))/nrow(full))*100
  return(error)
}


### Q6 

#using different distance metric
distmat2 <- as.matrix(dist(full, method = "minkowski"))
distmat3 <- as.matrix(dist(full, method = "manhattan"))
dist_mats<-list(euclidean=distmat,minkowski=distmat2,manhattan=distmat3)

#finding 10-fold CV error rates for 1 to 15
ten_fold_rates<-mapply(function(i,j)cv_error_knn(10,i,dist_mats[[j]]),rep(1:15),rep(names(dist_mats),each=15))

#subsetting, cleaning up data
euclid<-ten_fold_rates[1:15]
minkow<-ten_fold_rates[16:30]
manhat<-ten_fold_rates[31:45]
fold_rates<-data.frame(euclid,minkow,manhat)

#plotting 10-fold CV error rates for k = 1, . . . , 15
plot(fold_rates$euclid,col="black",type="l",lty=4,ylim=c(1.2,4.1),xlab="k-nearest neighbor",ylab= "Error Rate",main="Cross-Validation Error Rates")
lines(fold_rates$minkow,col="red",lty=3)
lines(fold_rates$manhat,col="green",lty=2)
legend("bottomright",legend=c("Euclidean","Minkowski","Manhattan"),col=c("black","red","green"),lty=c(4,3,2))

###7
best1<-full_cv(10,1,dist_mats$euclidean)
table(full$V1,best1)
best2<-full_cv(10,3,dist_mats$euclidean)
table(full$V1,best2)
best3<-full_cv(10,3,dist_mats$minkowski)
table(full$V1,best3)
###8
which(full$V1!=best3)
numOfBad <- sum(which(full$V1!=best3))

view_digit2 <- function(rowToConvert)
{
  mat <- matrix(as.numeric(rowToConvert), ncol = sqrt(length(rowToConvert)), byrow= TRUE)
  heatmap(mat,Rowv=NA,Colv=NA,col=paste("gray",1:99,sep=""))
}
view_digit2(train2[18,])
mis18 <- best3[18]
mis18
view_digit2(train2[280,])
mis280 <- best3[280]
mis280
view_digit2(train2[5046,])
mis5046 <- best3[5046]
mis5046

###9
test_error<-function(preds){
  conf_mat<-table(test$V1,preds)
  error_rate<-(1-sum(diag(conf_mat))/nrow(test))*100
  return(error_rate)
}

test_set_preds<-mapply(function(i,j)predict_knn(train,test,dist_mats[[j]],i),rep(1:15,3),rep(names(dist_mats),each=15))

test_error_rates<-apply(test_set_preds,2,test_error)

test_error_df<-data.frame(method=rep(names(dist_mats),each=15),k=rep(1:15,3),error=test_error_rates)

ts_euclid<-test_error_rates[1:15]
ts_minkow<-test_error_rates[16:30]
ts_manhat<-test_error_rates[31:45]
ts_error_rates<-data.frame(ts_euclid,ts_minkow,ts_manhat)
plot(ts_error_rates$ts_euclid,col="black",type="l",lty=4,ylim=c(2.9,6),xlab="k-nearest neighbor",ylab= "Error Rate",main="Test-Set Error Rates")
lines(ts_error_rates$ts_minkow,col="red",lty=3)
lines(ts_error_rates$ts_manhat,col="green",lty=2)
legend("bottomright",legend=c("Euclidean","Minkowski","Manhattan"),col=c("black","red","green"),lty=c(4,3,2))