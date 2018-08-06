install.packages("knitr")
install.packages("caret")
install.packages("gmodels")
install.packages("lattice")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("Kmisc")
install.packages("ROCR")
install.packages("corrplot")


#Read in csv files
weather_data <- read.csv("/Users/paotingkung/Desktop/weather.csv", header = TRUE)
##Looking at the data structure, we discover the dataset includes a mix of numerical and categorical variables
str(weather_data)
n <- nrow(weather_data)
##Which spans the following timeline
c(as.character(weather_data$Date[1]), as.character(weather_data$Date[n]))
##We further notice that RISK_MM relation with the RainTomorrow variable is the following.
all.equal(weather_data$RISK_MM > 1, weather_data$RainTomorrow == "Yes")
##The Rainfall variable and the RainToday are equivalent according to the following relationship (as anticipated by the Rainfall description).
all.equal(weather_data$Rainfall > 1, weather_data$RainToday == "Yes")
##To make it more challenging, we decide to take RISK_MM, RainFall and RainToday out, and determine a new dataset as herein depicted.
weather_data2 <- subset(weather_data, select = -c(Date, Location, RISK_MM, Rainfall, RainToday))
colnames(weather_data2)


(cols_withNa <- apply(weather_data2, 2, function(x) sum(is.na(x))))


weather_data3 <- weather_data2[complete.cases(weather_data2),]
##Categorical Variable Analysis
factor_vars <- names(which(sapply(weather_data3, class) == "factor"))
factor_vars <- setdiff(factor_vars, "RainTomorrow")
chisq_test_res <- lapply(factor_vars, function(x) { 
  chisq.test(weather_data3[,x], weather_data3[, "RainTomorrow"], simulate.p.value = TRUE)
})
names(chisq_test_res) <- factor_vars
chisq_test_res$WindGustDir

install.packages("CrossTable")
barchart_res <- lapply(factor_vars, function(x) { 
  title <- colnames(weather_data3[,x, drop=FALSE])
  wgd <- CrossTable(weather_data3[,x], weather_data3$RainTomorrow, prop.chisq=F)
  barchart(wgd$prop.row, stack=F, auto.key=list(rectangles = TRUE, space = "top", title = title))
})
names(barchart_res) <- factor_vars
barchart_res$WindGustDir








weather_data4 <- subset(weather_data2, select = -c(WindDir9am, WindDir3pm))
weather_data5 <- weather_data4[complete.cases(weather_data4),]
colnames(weather_data5)




#######
factor_vars <- names(which(sapply(weather_data5, class) == "factor"))
numeric_vars <- setdiff(colnames(weather_data5), factor_vars)
numeric_vars <- setdiff(numeric_vars, "RainTomorrow")
numeric_vars
numeric_vars_mat <- as.matrix(weather_data5[, numeric_vars, drop=FALSE])
numeric_vars_cor <- cor(numeric_vars_mat)
corrplot(numeric_vars_cor)



pairs(weather_data5[,numeric_vars], col=weather_data5$RainTomorrow)


grob_plots <- invisible(lapply(chunk(1, length(gp), 4), function(x) {marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
#####
install.packages("chunk")
install.packages("CrossTable")
gp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=weather_data5, aes(x=eval(parse(text=x)), col = RainTomorrow)) + geom_density() + xlab(x) + ggtitle(paste(x, "density", sep= " "))}))
grob_plots <- invisible(lapply(chunk(1, length(gp), 4), function(x) {
  marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
grob_plots


#predict
?lapply
install.packages("chunk")
numeric_vars <- setdiff(colnames(weather_data5), factor_vars)
gp <- invisible(lapply(numeric_vars, function(x) { 
  ggplot(data=weather_data5, aes(x=eval(parse(text=x)), col = RainTomorrow)) + geom_density() + xlab(x) + ggtitle(paste(x, "density", sep= " "))}))
grob_plots <- invisible(lapply(chunk(1, length(gp), 4), function(x) {
  marrangeGrob(grobs=lapply(gp[x], ggplotGrob), nrow=2, ncol=2)}))
grob_plots
library(devtools)
install_github("leandroroser/chunkR")
