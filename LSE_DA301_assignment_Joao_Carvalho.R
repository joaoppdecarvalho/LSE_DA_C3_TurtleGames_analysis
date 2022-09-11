## LSE Data Analytics Online Career Accelerator 

# Main Assignment - João carvalho

###############################################################################
################################################################################

# Week 4 assignment: EDA using R

###############################################################################

# 1. Load and explore the data

# Import Tidyverse.
library(tidyverse)

# Import the data set.
df <- read.csv(file.choose('turtle_sales.csv'), header=T)

# Print the data frame.
View(df)

# View the descriptive statistics all dataset to explore categorical variables
DataExplorer::create_report(df)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
df_sales <-subset(df, select=-c(Platform, Ranking, Year, Genre, Publisher))

# View the data frame.
view(df_sales)

# View statistics
summary(df_sales)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
qplot(NA_Sales, Global_Sales, data=df_sales)
qplot(EU_Sales, Global_Sales, data=df_sales)
qplot(EU_Sales, NA_Sales, data=df_sales)

## 2b) Histograms
plot(hist(df_sales$Global_Sales))
plot(hist(df_sales$NA_Sales))
plot(hist(df_sales$EU_Sales))

## 2c) Boxplots
qplot(NA_Sales, data=df_sales, geom='boxplot')
qplot(EU_Sales, data=df_sales, geom='boxplot')
qplot(Global_Sales, data=df_sales, geom='boxplot')

## Extra) Barplot of sales

# Create column for Other region
df_sales$other <- df_sales$Global_Sales - df_sales$NA_Sales - df_sales$EU_Sales

# Reorder to Global_sales be the last column
df_sales <- df_sales[, c(1,3,4,2,5)]

# Plot
x <- barplot(colSums(df_sales[,2:5]), col = c("blue", "gray", "gray", "gray"),
        main="Total Sales per region (£ Millions)",
        ylim=range(pretty(c(0, 2500))))
y<-as.matrix(colSums(df_sales[,2:5]))
text(x,y+100,labels=as.character(y),pos = 3, cex = 1)

# Drop other_sales column as it will not be necessary
df_sales = subset(df_sales, select = -c(5))
        
################################################################################

# 3. Observations and insights

# Europe and North America represents approx. 80% of Global Sales
# Global, Europe and NA Sales have some higher outliers
# From DataExplorer, we observe that 107 and 525 are the leading products

###############################################################################
###############################################################################

# Week 5 assignment: Cleaning and manipulating data using R

###############################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
view(df_sales)

# Check output: Determine the min, max, and mean values.
summary(df_sales)

# View the descriptive statistics.
DataExplorer::create_report(df_sales)

###############################################################################

# 2. Determine the impact on sales per product.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
group_product <- df_sales %>% group_by(Product) %>% summarise_all(sum)

# View the data frame.
view(group_product)

# Explore the data frame.
summary(group_product)
dim(group_product)

## 2b) Determine which plot is the best to compare game sales.
# Scatterplot NA - Global Sales.
ggplot(group_product,
       mapping=aes(x=NA_Sales, y=Global_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method=lm, se=FALSE) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 70, 5), "North America") +
  scale_y_continuous(breaks=seq(0, 70, 5), "Global") +
  # Add a title and subtitle.
  labs(title="Sales relationship between North America and Global (£ Millions)",
       subtitle="by product")

# Scatterplot EU - Global Sales.
ggplot(group_product,
       mapping=aes(x=EU_Sales, y=Global_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method=lm, se=FALSE) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 70, 5), "Europe") +
  scale_y_continuous(breaks=seq(0, 70, 5), "Global") +
  # Add a title and subtitle.
  labs(title="Sales relationship between Europe and Global (£ Millions)",
       subtitle="by product")

# Scatterplot EU - NA.
ggplot(group_product,
       mapping=aes(x=EU_Sales, y=NA_Sales)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method=lm, se=FALSE) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 40, 5), "Europe") +
  scale_y_continuous(breaks=seq(0, 40, 5), "North America") +
  # Add a title and subtitle.
  labs(title="Sales relationship between Europe and North America (£ Millions)",
       subtitle="by product")

# Remove outliers (>15).
EU_NA_no_outliers <- filter(group_product, EU_Sales<15, NA_Sales<15)
dim(EU_NA_no_outliers)

ggplot(EU_NA_no_outliers,
       mapping=aes(x=EU_Sales, y=NA_Sales)) +
  geom_point(color='purple',
             alpha=0.75,
             size=2.5) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 15, 5), "Europe") +
  scale_y_continuous(breaks=seq(0, 15, 5), "North America") +
  # Add a title and subtitle.
  labs(title="Relationship between sales in Europe and NA, upper limit values 15 (£ Millions)",
       subtitle="by product")

# Create histograms.
group_product %>% 
  pivot_longer(cols = 2:4) %>% 
  ggplot(aes(value)) + 
  scale_y_continuous("Number of products")+
  scale_x_continuous("Sum of Sales per product")+
  geom_histogram(bins = 10) + 
  facet_wrap(~name)

# Create boxplots from higher to lower.
boxplot(group_product$EU_Sales, group_product$NA_Sales, 
        group_product$Global_Sales, 
        horizontal=TRUE,
        names = c("Europe", "North America", "Global"),
        las = 1,
        main = "Sales (£ Millions) per region",
        xlab="")

###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
qqnorm(group_product$EU_Sales, main = "Normal Q-Q Plot (EU)")
qqline(group_product$EU_Sales, col='red')

qqnorm(group_product$NA_Sales, main = "Normal Q-Q Plot (NA)")
qqline(group_product$NA_Sales, col='red')

qqnorm(group_product$Global_Sales,  main = "Normal Q-Q Plot (GLOBAL)")
qqline(group_product$Global_Sales, col='red')

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(group_product$Global_Sales)
shapiro.test(group_product$EU_Sales)
shapiro.test(group_product$NA_Sales)

# All p-values well below 0.05, so the dataset does not follow normal distribution.


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(group_product$Global_Sales)
skewness(group_product$EU_Sales)
skewness(group_product$NA_Sales)
  # Our output suggest a positive skewness.
kurtosis(group_product$Global_Sales)
kurtosis(group_product$EU_Sales)
kurtosis(group_product$NA_Sales)
  # Our output suggest very high values for kurtosis


## 3d) Determine correlation

# Check correlation between variables using Pearson's correlation.
cor(group_product$EU_Sales, group_product$NA_Sales)
cor(group_product$NA_Sales, group_product$Global_Sales)
cor(group_product$EU_Sales, group_product$Global_Sales)

###############################################################################

# 4. Plot the data
ggplot(group_product, aes(x=EU_Sales)) +
  # Add fill colour to the function.
  geom_density(fill='blue', bw=1) + 
  # Specify the title.
  labs(title="Europe Sales per product", subtitle="(£ Millions)")

ggplot(group_product, aes(x=NA_Sales)) +
  # Add fill colour to the function.
  geom_density(fill='blue', bw=1) + 
  # Specify the title.
  labs(title="North America Sales per product", subtitle="(£ Millions)")


# Density lines jointly

# Change product category and plot linegraph jointly for sales
group_product$Product = as.factor(group_product$Product)

library(data.table)
group_product.plot <- melt(group_product) 

p <- ggplot(aes(x=value, colour=variable), data=group_product.plot)
p + geom_density() +  labs(title="Density lines of Sales", 
                           subtitle="(£ Millions)")

# Top 10 products with highest value of global_sales
top10_global<-group_product[order(-group_product$Global_Sales),]
df_top10_global <-head(top10_global,10)
view(df_top10_global)

# Plot Top 10 products by Global_Sales
ggplot(df_top10_global, aes(x = reorder(Product,-Global_Sales), y = Global_Sales))+
  geom_col()+
  ggtitle("Top10 products: Global Sales (£ Millions)")

###############################################################################

# 5. Observations and insights

# Right skewed and very high kurtosis on all Sales variables
# QQ-plots and Shapiro Wilk tests shows that sales variables does not follow normal distribution



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
view(group_product)

# Determine a summary of the data frame.
summary(group_product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
EU_NA <- lm(EU_Sales ~ NA_Sales,
             data=df_sales)

Global_EU <- lm(Global_Sales ~ EU_Sales,
            data=df_sales)

Global_NA <- lm(Global_Sales ~ NA_Sales,
                data=df_sales)

# View the result.
summary(EU_NA)
summary(Global_EU)
summary(Global_NA)


## 2b) Create plots to view linear regression using original data
qplot(NA_Sales, Global_Sales, data=df_sales)
qplot(EU_Sales, Global_Sales, data=df_sales)
qplot(EU_Sales, NA_Sales, data=df_sales)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
MLR = lm(Global_Sales~NA_Sales+EU_Sales, data=df_sales)

  ## Use sales without grouping per product 
  ## as we will compare with observed data not aggregated

# Multiple linear regression model.
summary(MLR)

# Plot residuals
plot(MLR$residuals)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

EU_Sales <- c(23.8, 1.56,0.65,0.97,0.52)
NA_Sales <- c(34.02,3.93,2.73,2.26,22.08)
# Combine vectors into a data frame.
test <- data.frame(NA_Sales, EU_Sales)

# Create a new object and specify the predict function.
predictTest = predict(MLR, newdata=test,
                      interval='confidence')

# Convert results to a dataframe.
test_results_df<- as.data.frame(predictTest)

# Add observed results from the original dataset to the dataframe
test_results_df['observed'] <- c(67.85,6.04,4.32,3.53,23.21)

# View results and compare
view(test_results_df)

# Creating plot to visualise if it is in the confidence interval
library("ggplot2")
p <- ggplot(test_results_df, aes(x=fit))+
  geom_linerange(aes(ymin=lwr,ymax=upr),linetype=2,color="blue")+
  geom_point(aes(y=observed),size=2,color="red")+
  geom_point(aes(y=upr),size=2,color="blue")+
  geom_point(aes(y=lwr),size=2,color="blue")+
  facet_grid(cols = vars(observed))+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

p + facet_grid(observed ~ ., scales = "free_y") + labs(
  x = "", y="", title = "Global Sales",
  subtitle = "Estimated Confidence interval in blue, observed values in red")

####### Extra: Explore MLR with Publisher Nintendo Factor

library(plotly)

# Global vs EU
plot_ly(df,
        x = ~Global_Sales,
        y = ~EU_Sales,
        type = 'scatter',
        mode = 'markers',
        color = ~factor(Publisher),
        symbols = c('circle', 'x', 'o'),
        size = 2,
        alpha = 1)

# Global vs NA
plot_ly(df,
        x = ~Global_Sales,
        y = ~NA_Sales,
        type = 'scatter',
        mode = 'markers',
        color = ~factor(Publisher),
        symbols = c('circle', 'x', 'o'),
        size = 2,
        alpha = 1)


# Create MLR using original dataset without group by product
MLR_no_group = lm(Global_Sales~NA_Sales+EU_Sales+I(Publisher == "Nintendo"),
                  data=df)
summary(MLR_no_group)

Publisher <- c(TRUE,TRUE,FALSE,TRUE,FALSE)
test1 <- data.frame(NA_Sales, EU_Sales,Publisher)

# Create a new object and specify the predict function.
predictTest1 = predict(MLR_no_group, newdata=test1,
                      interval='confidence')

# Convert results to a dataframe.
test_results_df1<- as.data.frame(predictTest1)

# Add observed results from the original dataset to the dataframe
test_results_df1['observed'] <- c(67.85,6.04,4.32,3.53,23.21)

# View results and compare
view(test_results_df1)


# Creating plot to visualise if it is in the confidence interval
p <- ggplot(test_results_df1, aes(x=fit))+
  geom_linerange(aes(ymin=lwr,ymax=upr),linetype=2,color="blue")+
  geom_point(aes(y=observed),size=2,color="red")+
  geom_point(aes(y=upr),size=2,color="blue")+
  geom_point(aes(y=lwr),size=2,color="blue")+
  facet_grid(cols = vars(observed))+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

p + facet_grid(observed ~ ., scales = "free_y") + labs(
  x = "", y="", title = "Global Sales",
  subtitle = "Estimated Confidence interval in blue, observed values in red")

###############################################################################

# 5. Observations and insights

# Very strong R2 Single linear regression when analysing Global using EU or NA
# Very strong Adj-R2 in MLR analysing Global, using EU and NA
# Highest-values all from Nintendo Publisher
# Very strong Adj-R2 in MLR when added Nintendo Publisher TRUE/FALSE variable



###############################################################################
###############################################################################




