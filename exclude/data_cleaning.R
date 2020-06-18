rm(list = ls())

col_names <- c("countries_en",
               "additives_n",
               "pnns_groups_2",
               "nutriscore_score",
               "nutriscore_grade",
               "nova_group",
               "fat_100g",
               "carbohydrates_100g",
               "sugars_100g",
               "proteins_100g",
               "salt_100g",
               "fiber_100g"
)

nutrition <- c(
  "fat_100g",
  "carbohydrates_100g",
  "sugars_100g",
  "proteins_100g",
  "salt_100g",
  "fiber_100g"
)

files_list <-list.files("data",pattern=".csv")
for (i in 1:length(files_list)) 
  {

  filename <- sprintf("data/%s",files_list[i])
  if (i==1){
    row_dataset <- read.csv2(filename,header=TRUE)

    colindex <-which(names(row_dataset) %in% col_names)
    row_dataset <-row_dataset[ , (names(row_dataset) %in% col_names)]
    col_names <- colnames(row_dataset)  
  } else {
    row_dataset <- read.csv2(filename,header=FALSE)
    row_dataset <-row_dataset[ , (names(row_dataset) %in% sprintf("V%d",colindex))]
    colnames(row_dataset) <- col_names
  }
  row_dataset[row_dataset==""]<-NA
  row_dataset[row_dataset=="unknown"]<-NA  
  row_dataset <- row_dataset[which(!is.na(row_dataset$nutriscore_score)),]
  row_dataset <- data.frame(row_dataset)

  if (i==1){
    mydataset <- row_dataset
  } else {
    mydataset <-rbind(mydataset,row_dataset)
  }
  
}
write.csv(mydataset,"complete_data.csv", row.names = FALSE)
rm(row_dataset,i,files_list,filename,colindex)

row_dataset <- read.csv("complete_data.csv",header=TRUE)
mydataset <- data.frame(row_dataset)
mydataset[nutrition] <- apply(mydataset[nutrition],2,as.numeric)

errata <- !is.na(mydataset[nutrition])&(mydataset[nutrition]>100)
mydataset[nutrition][errata] <- mydataset[nutrition][errata]/1000
mydataset[nutrition][is.na(mydataset[nutrition])] <- 0.0
mydataset[nutrition][mydataset[nutrition]>100] <- NA
rm(errata)

## Take columns with more than 80% not NA 
mydataset <- mydataset[which(rowMeans(!is.na(mydataset)) > 0.8),]

##################### A PARTIR DE AQUI ES COPY PASTE######################
###########Outliers with Cooks distance
mod1 <- lm(Score ~ ., data=mydataset)
cooksd1 <- cooks.distance(mod1)
plot(cooksd1, pch=".", cex=2, main="score")  # plot cook's distance
abline(h = 4*mean(cooksd1, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>4*mean(cooksd1, na.rm=T),names(cooksd1),""), col="red")  # add labels

######### check with mahalanobis distance ########
X <- mydataset

# define a function to find extreme outliers
FindExtremeOutliers <- function(data) {
  q1 = quantile(data)[2]
  q3 = quantile(data)[4]
  iqr = q3 - q1 #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 3) + q3
  extreme.threshold.lower = q1 - (iqr * 3)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}
#chech the missing values
mydataset[!complete.cases(mydataset),]

#imput missing values

imp = mice(X, method = "norm", m = 1, print = FALSE)
complete(imp)[!complete.cases(X),]

# mahalanobis

O = Moutlier(X, quantile = 0.95)
S = (O$md >= O$cutoff); cbind(dataset[S,], distance = O$md[S])





library(chemometrics)

outlier = Moutlier(mydataset[,1:27], quantile = 0.975, plot = TRUE)

outlier
to_cut <- outlier$rd[outlier$rd>75]
t <- row.names(as.matrix(to_cut))
Boobies_without <- Boobs[-which(rownames(Boobs) %in% t),]
par(mfrow=c(1,1))
boxplot(Boobies_without)






