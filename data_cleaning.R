row_dataset <- read.table("xaa11.csv",sep="\t",header=TRUE, fill=TRUE)
drops <- c("image_url",
           "image_small_url",
           "image_ingredients_url",
           "image_ingredients_small_url",
           "image_nutrition_url",
           "image_nutrition_small_url",
           "X.butyric.acid_100g",
           "X.caproic.acid_100g",                       
           "X.caprylic.acid_100g",
           "X.capric.acid_100g",
           "X.lauric.acid_100g",
           "X.myristic.acid_100g",                     
           "X.palmitic.acid_100g",
           "X.stearic.acid_100g",                       
           "X.arachidic.acid_100g",
           "X.behenic.acid_100g",                   
           "X.lignoceric.acid_100g",
           "X.cerotic.acid_100g",                     
           "X.montanic.acid_100g",
           "X.melissic.acid_100g",                      
           "monounsaturated.fat_100g",
           "polyunsaturated.fat_100g",                 
           "omega.3.fat_100g",
           "X.alpha.linolenic.acid_100g",            
           "X.eicosapentaenoic.acid_100g",
           "X.docosahexaenoic.acid_100g"
           )
row_dataset <-row_dataset[ , !(names(row_dataset) %in% drops)]
mydataset <- data.frame(row_dataset)

## Remove columns with more than 50% NA 
#which(rowMeans(!is.na(mydataset)) > 0.3), 
mydataset <- mydataset[which(colMeans(!is.na(mydataset)) > 0.9)]



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






