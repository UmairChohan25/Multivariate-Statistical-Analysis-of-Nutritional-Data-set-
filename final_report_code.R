

nutritional<-read.csv("C:/Users/UmairChohan/Downloads/PS1/PS1/data/nutritional.txt", sep="")

# overview of data set


head(nutritional)


# To equalize out the different types of servings of each food, first divide each variable by weight ofthe food item (which leaves us with 6 variables). Next, because of the wide variations in the different variables, standardize each variable. Perform Principal Component Analysis on the transformed data.


# create a new data frame with transformed variables
nutritional_transf <- data.frame(
  fat_per_gram = nutritional$fat / nutritional$weight,
  energy_per_gram = nutritional$food.energy / nutritional$weight,
  carbs_per_gram = nutritional$carbohydrates / nutritional$weight,
  protein_per_gram = nutritional$protein / nutritional$weight,
  cholesterol_per_gram = nutritional$cholesterol / nutritional$weight,
  saturated_fat_per_gram = nutritional$saturated.fat / nutritional$weight
)


## Very important to scale the data befor to use PCA



# standardize the variables
nutritional_transf_std <- scale(nutritional_transf)






# perform PCA
pca <- prcomp(nutritional_transf_std, center = TRUE, scale. = TRUE)

# print the summary of the PCA
summary(pca)




# plot PCA
plot(pca)





# Cumulative proportion of variance explained
cumsum(pca$sdev^2/sum(pca$sdev^2))
## [1] 0.4414322 0.6631213 0.8331422 0.9464871 0.9909079 1.0000000
prop_var <- pca$sdev^2 / sum(pca$sdev^2)
cum_prop_var <- cumsum(prop_var)
# determine the number of components to retain
prop_var_cutoff <- 0.80 # set the cutoff for proportion of variance explained
num_components <- length(which(cum_prop_var < prop_var_cutoff)) + 1
cat("Number of components to retain:", num_components)
 
## Method To reatin PCA (Scree plot)



# plot the scree plot
plot(pca, type = "l", main = "Scree Plot")

# 3. Give an interpretation to the first two principal components


loadings <- pca$rotation[, 1:2]
colnames(loadings) <- c("PC1", "PC2")
rownames(loadings) <- colnames(nutritional_transf_std)
loadings



# 4. Identify univariate outliers with respect to the first three principal components, up to 3 per component. These points correspond to foods that are very high or very low in what variable (up to 2 variables per observation)?


#Extract the scores for each observation on the first three principal components.
pc_scores <- predict(pca, type = "scores")[, 1:3]


#Calculate the Mahalanobis distance for each observation based on its scores on the first three principal components. You can do this using the mahalanobis function:
mahal_dist <- mahalanobis(pc_scores, colMeans(pc_scores), cov = cov(pc_scores))
#Identify outliers based on the Mahalanobis distances. we can use a cutoff value based on the chi-square distribution with degrees of freedom equal to the number of principal components used. For example, to identify outliers with a significance level of 0.01 (i.e., a cutoff based on the 99th percentile of the chi-square distribution), you can use
cutoff <- qchisq(0.99, df = 3)
outliers <- which(mahal_dist > cutoff)
# Compute PCA
pca <- prcomp(nutritional)

# Calculate the scores for the first three principal components
scores <- pca$x[, 1:3]

# Identify univariate outliers for each principal component
outliers <- list()
for (i in 1:3) {
  outliers[[i]] <- boxplot.stats(scores[, i])$out
}

# Plot univariate outliers with respect to the first three principal components
par(mfrow = c(1, 3))
for (i in 1:3) {
  boxplot(scores[, i], main = paste0("PC", i), ylim = c(min(scores[, i]), max(scores[, i]) * 1.1))
  points(rep(i, length(outliers[[i]])), outliers[[i]], col = "red", pch = 19)
}



# 5. Make a 3-d scatter plot with the first three principal components, while color coding these outliers


library(scatterplot3d)
# Standardize the variables by dividing each one by its standard deviation
nutritional_scaled <- apply(nutritional[,1:6], 2, function(x) (x - mean(x)) / sd(x))

# Add back the saturated fat variable
nutritional_scaled <- cbind(nutritional_scaled, nutritional$saturated.fat)

# Rename the columns
colnames(nutritional_scaled) <- c("fat", "food.energy", "carbohydrates", "protein", "cholesterol", "weight", "saturated.fat")
mahal_dist <- mahalanobis(nutritional_scaled, center = colMeans(nutritional_scaled), cov = cov(nutritional_scaled))
threshold <- 3
outlier_inds <- which(mahal_dist > threshold)
library(plotly)



# Create a data frame with the first three principal components
pca_df <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], PC3 = pca$x[,3])

# Add a column to the data frame to indicate whether an observation is an outlier
pca_df$outlier <- ifelse(mahal_dist > threshold, "Outlier", "Non-outlier")

# Create a 3D scatter plot with color coded outliers using plotly
plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~outlier, 
        colors = c("Non-outlier" = "blue", "Outlier" = "red"), 
        marker = list(size = 2)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "PC1"), 
                      yaxis = list(title = "PC2"), 
                      zaxis = list(title = "PC3"), 
                      camera = list(up = list(x = 0, y = 0, z = 1),
                                    eye = list(x = -1.8, y = -1.8, z = 0.6),
                                    center = list(x = 0, y = 0, z = 0))))

# 6. Investigate multivariate normality through the first three principal components.



pcs <- prcomp(nutritional, scale. = TRUE)$x[, 1:3]
qqnorm(pcs)
qqline(pcs)



# 7. Find multivariate outliers through the first three principal components, up to 5 in total. Are they the most extreme observations with respect to the 6 original variables?


# Perform PCA
pca <- prcomp(nutritional, scale = TRUE)
loadings <- pca$rotation

# Calculate Mahalanobis distance using the first three principal components
pca_scores <- as.matrix(nutritional) %*% loadings[,1:3]
cov_matrix <- cov(pca_scores)
inv_cov_matrix <- solve(cov_matrix)
mahalanobis_dist <- apply(pca_scores, 1, function(x) sqrt(t(x) %*% inv_cov_matrix %*% x))

# Identify the top 5 observations with the largest Mahalanobis distances
multivar_outliers <- order(mahalanobis_dist, decreasing = TRUE)[1:5]
multivar_outliers



# we can also check that is there any multivariate outliers in the most extreme observations


extreme_obs <- apply(nutritional, 2, function(x) order(x, decreasing = TRUE)[1:5])
extreme_obs
# Check if any of the multivariate outliers are among the most extreme observations
any(multivar_outliers %in% as.vector(extreme_obs))





# State Data Set



st<- as.data.frame(state.x77)


## overview of data set HEAD(ST)


head(st)



## changed the varaibles name to avoid spaces



names(st)[4] = "Life.Exp"
names(st)[6] = "HS.Grad"
st[,9] = st$Population * 1000 / st$Area
colnames(st)[9] = "Density"


# Checking Missing Values in the dataset


sum(is.na(st))





# Data Cleaning and Preprocessing



# 1. Compute the correlation matrix and comment on the most relevant relationships among variables (upto 10).


corr <- (cor(st[,1:9]))
corr





## 2. Find univariate outliers, up to 3 per variable, up to 10 in total.

# Loop through each variable in the dataset
for (col in colnames(st)) {
  # Calculate quartiles and interquartile range
  q1 <- quantile(st[[col]], 0.25)
  q3 <- quantile(st[[col]], 0.75)
  iqr <- q3 - q1
  
  # Find outliers
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  outliers <- which(st[[col]] < lower | st[[col]] > upper)
  
  # Print up to 3 outliers for each variable
  if (length(outliers) > 0) {
    cat("Variable:", col, "\n")
    cat("Outliers:", paste(outliers[1:min(3, length(outliers))], collapse = ", "), "\n\n")
  }
}



## 3. Make a boxplot of any variable plotting the corresponding outliers, if any, found in point 2 in red.



# Choose a variable to plot (e.g. Murder)
col <- "Murder"

# Calculate quartiles and interquartile range
q1 <- quantile(st[[col]], 0.25)
q3 <- quantile(st[[col]], 0.75)
iqr <- q3 - q1

# Find outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
outliers <- which(st[[col]] < lower | st[[col]] > upper)

# Create boxplot with outliers in red
bp <- boxplot(st[[col]], main = col, horizontal = TRUE,
        ylim = c(0, max(st[[col]])), col = "lightblue",
        boxwex = 0.5, outline = TRUE)
if (length(outliers) > 0) {
  points(x = rep(1, length(outliers)), y = st[[col]][outliers], pch = "*", col = "red")
}






# Create histograms, density plots, and Q-Q plots of each variable
par(mfrow = c(3, 3))
for (col in colnames(st)) {
  hist(st[[col]], main = col, col = "lightblue", xlab = "", ylab = "")
  lines(density(st[[col]]), col = "darkblue")
  qqnorm(st[[col]], main = col, col = "darkblue")
  qqline(st[[col]])
}






# Loop through each variable in the dataset
for (col in colnames(st)) {
  # Perform Shapiro-Wilk test
  sw_test <- shapiro.test(st[[col]])
  
  # Print results
  cat("Variable:", col, "\n")
  cat("p-value:", sw_test$p.value, "\n")
  
  # Determine normality based on p-value
  if (sw_test$p.value < 0.05) {
    cat("Normality: Not supported\n\n")
  } else {
    cat("Normality: Supported\n\n")
  }
}


## 5. Make a scatter plot of Area vs Population, colour-coding the outliers found in point 2 with a different colours. Choose among the following colour names. Can they be considered bivariate outliers? lookup<-c("darkgreen", "brown", "lightblue", "magenta", "purple","blue", "red", "lightgreen", "orange", "cyan")


# Create a vector of colors for the outliers
lookup <- c("darkgreen", "brown", "lightblue", "magenta", "purple",
            "blue", "red", "lightgreen", "orange", "cyan")

# Identify the outliers of Area and Population
area_outliers <- which(st$Area < quantile(st$Area, 0.25) - 1.5 * IQR(st$Area) | 
                         st$Area > quantile(st$Area, 0.75) + 1.5 * IQR(st$Area))
pop_outliers <- which(st$Population < quantile(st$Population, 0.25) - 1.5 * IQR(st$Population) | 
                         st$Population > quantile(st$Population, 0.75) + 1.5 * IQR(st$Population))

# Combine the outlier indices into one vector
outliers <- unique(c(area_outliers, pop_outliers))

# Create a color vector for each point
colors <- rep("black", nrow(st))
colors[outliers] <- lookup[1:length(outliers)]

# Create the scatter plot
plot(st$Area, st$Population, col = colors, xlab = "Area", ylab = "Population")






# Fit a linear regression model to Population vs Area
model <- lm(Population ~ Area, data = st)

# Calculate Cook's distance for each observation
cooksd <- cooks.distance(model)

# Identify observations with high Cook's distances (e.g. above a certain threshold)
threshold <- 4 / nrow(st)  # set threshold to 4/n, where n is the number of observations
outliers <- which(cooksd > threshold)

# Plot the scatter plot with outliers highlighted in red
plot(Population ~ Area, data = st, col = ifelse(1:nrow(st) %in% outliers, "red", "black"))








cov.st <- cov(st[, 1:9])
center.st <- apply(st[, 1:9], 2, mean)
mah.dist <- mahalanobis(st[, 1:9], center.st, cov.st)^2

# Calculate the expected quantiles of the chi-square distribution
df <- length(center.st)
qchisq <- qchisq((1:length(mah.dist))/(length(mah.dist)+1), df)

# Plot the chi-square Q-Q plot of the squared Mahalanobis distances
qqplot(qchisq, mah.dist, xlab = "Expected quantiles of chi-square distribution",
       ylab = "Observed squared Mahalanobis distances")
abline(0, 1, col = "red")


## 7. Identify multivariate outliers, if any, and compare with the univariate outliers previously found

 

# Calculate squared Mahalanobis distances
cov_mat <- cov(st[, -9])
mean_vec <- apply(st[, -9], 2, mean)
md <- mahalanobis(st[, -9], mean_vec, cov_mat, inverted = TRUE)
sq_md <- md^2



# Set cutoff value for chi-square distribution
cutoff <- qchisq(0.99, df = ncol(st) - 1)

# Identify multivariate outliers
mult_outliers <- which(sq_md > cutoff)

mult_outliers



# Univariate Outliers

# Loop through each variable in the dataset
for (col in colnames(st)) {
  # Calculate quartiles and interquartile range
  q1 <- quantile(st[[col]], 0.25)
  q3 <- quantile(st[[col]], 0.75)
  iqr <- q3 - q1
  
  # Find outliers
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  outliers <- which(st[[col]] < lower | st[[col]] > upper)
  
  # Print up to 3 outliers for each variable
  if (length(outliers) > 0) {
    cat("Variable:", col, "\n")
    cat("Outliers:", paste(outliers[1:min(3, length(outliers))], collapse = ", "), "\n\n")
  }
}







