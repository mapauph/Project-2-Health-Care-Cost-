# CHRONIC KIDNEY DISEASE PROJECT
## Installing packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("viridisLite")
install.packages("unikn")
install.packages("caret")
install.packages("lattice")
install.packages("class")
install.packages("zoo")
install.packages("fastDummies")
install.packages("randomForest")
install.packages("e1071")
install.packages("xgboost")
install.packages("ipred")
install.packages("rpart")
# Library
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(viridisLite)
library(unikn)
library(readr)
library(caret)
library(class)
library(zoo)
library(fastDummies)
library(randomForest)
library(e1071)
library(xgboost)
library(ipred)
library(rpart)
# Uploading data
data1 <- read_csv("R DOCUMENTS/kidney.csv")
colnames(data1) <- c('ID', 'age', 'blood_pressure', 'specific_gravity', 'albumin', 'sugar', 'red_blood_cells', 'pus_cell',
                     'pus_cell_clumps', 'bacteria', 'blood_glucose_random', 'blood_urea', 'serum_creatinine', 'sodium',
                     'potassium', 'haemoglobin', 'packed_cell_volume', 'white_blood_cell_count', 'red_blood_cell_count',
                     'hypertension', 'diabetes_mellitus', 'coronary_artery_disease', 'appetite', 'peda_edema',
                     'anemia', 'class')

data <- as.data.frame(data1)

# Summary
summary(data)

## Change variables from objects to numerical
data$white_blood_cell_count <- as.numeric(data$white_blood_cell_count)
data$packed_cell_volume <- as.numeric(data$packed_cell_volume)
data$ID <- as.factor(data$ID)

data <- na.omit(data)

# Replace values in the 'class' column
data$class <- ifelse(data$class == 'ckd', 0, 1)
head(data)

## Extracting categorical and numerical columns
cat_cols <- colnames(data)[sapply(data, is.character)]
num_cols <- colnames(data)[sapply(data, is.numeric)]

## Replace incorrect values
data <- data %>% 
  mutate(diabetes_mellitus = ifelse(diabetes_mellitus %in% c('yes', 'no'), diabetes_mellitus, NA))

data$class <- as.numeric(data$class)

# Checking numerical features distribution
ggplot(data = gather(data, key = "Feature", value = "Value", num_cols), aes(x = Value, fill = Feature)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Feature, scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = cm.colors(length(num_cols))) +
  labs(title = "Numeric variables distribution", x = "Value", y = "Density")

# Looking at categorical columns

# Define the color palette
my_colors <- hcl.colors(3, palette = "Cold")

# Define the categorical columns to plot
cat_cols_to_plot <- c("red_blood_cells", "pus_cell", "pus_cell_clumps", "bacteria",
                      "hypertension", "diabetes_mellitus", "coronary_artery_disease",
                      "appetite", "peda_edema", "anemia")

# Create a list to store the plots
plot_list <- list()

# Generate the plots and store them in the list
for (column in cat_cols_to_plot) {
  # Filter values excluding NaN and empty values
  filtered_data <- data %>%
    filter(!is.na(!!sym(column)) & !!sym(column) != "") %>%
    select("class", column)
  
  p <- ggplot(filtered_data, aes_string(x = column, fill = column)) +
    geom_bar() +
    theme_minimal() +
    labs(title = column) +
    theme(legend.position = "none") +
    scale_fill_manual(values = my_colors)  # Color palette
  plot_list[[column]] <- p
}

# Combine the plots into a single grid and display them in RStudio
combined_plot <- grid.arrange(grobs = plot_list, ncol = 4)
print(combined_plot)

# Heatmap of data

numeric_data <- data[sapply(data, is.numeric)]

##Correlation matrix calculation
heatmap_data <- cor(numeric_data, use = "complete.obs")

ggplot(data = melt(heatmap_data), aes(Var2, Var1, fill = value)) +
  geom_tile() +
  theme_minimal() +
  scale_fill_viridis_c(option = "viridis") +  # Palette
  geom_text(aes(label = round(value, 2)), vjust = 1) +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Vertical titles

# EXPLORATORY DATA ANALYSIS
# AGE
# Density plot for the "age" variable differentiating between CKD and NotCKD
ggplot(data, aes(x = age, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("CKD", "NotCKD")) +
  labs(title = "Distribution of Age by CKD Status", x = "Age", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Violin plot for the "age" variable differentiating between CKD and NotCKD
ggplot(data, aes(x = factor(class), y = age, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#FF6F61","#6F8A91"), labels = c("CKD", "NotCKD" )) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Age by CKD Status", x = "CKD Status", y = "Age") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2), 
        panel.grid.minor = element_blank())

## Density plot for the "Albumin" variable differentiating between CKD and NotCKD
ggplot(data, aes(x = albumin, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("CKD", "NotCKD"))  +
  labs(title = "Distribution of Albumin by CKD Status", x = "Albumin", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Violin plot with boxplot for the "Albumin" variable differentiating between CKD and NotCKD
ggplot(data, aes(x = factor(class), y = albumin, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#FF6F61","#6F8A91"), labels = c("CKD", "NotCKD" )) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Albumin by CKD Status", x = "CKD Status", y = "Albumin") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2), 
        panel.grid.minor = element_blank())

# Blood Pressure
# Density plot for the "blood_pressure" variable differentiating between CKD and NotCKD
ggplot(data, aes(x = blood_pressure, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("CKD", "NotCKD"))  +
  labs(title = "Distribution of Blood Pressure by CKD Status", x = "Blood Pressure", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Violin plot with boxplot for the "blood_pressure" variable differentiating between CKD and NotCKD
ggplot(data, aes(x = factor(class), y = blood_pressure, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#FF6F61","#6F8A91"), labels = c("CKD", "NotCKD" )) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Blood Pressure by CKD Status", x = "CKD Status", y = "Blood Pressure") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2), 
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "blood_glucose_random" variable
data_cleaned_blood_glucose <- data[!is.na(data$blood_glucose_random), ]

# Gráfico de densidad para la variable "blood_glucose_random" sin valores en blanco, diferenciando entre CKD y NotCKD
ggplot(data_cleaned_blood_glucose, aes(x = blood_glucose_random, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("CKD", "NotCKD"))  +
  labs(title = "Distribution of Blood Glucose (Random) by CKD Status", x = "Blood Glucose (Random)", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "blood_glucose_random" variable
data_cleaned_blood_glucose <- data[!is.na(data$blood_glucose_random), ]

# Violin plot with box plot for the "blood_glucose_random" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_blood_glucose, aes(x = factor(class), y = blood_glucose_random, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#FF6F61","#6F8A91"), labels = c("CKD", "NotCKD" )) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Blood Glucose (Random) by CKD Status", x = "CKD Status", y = "Blood Glucose (Random)") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "blood_urea" variable
data_cleaned_blood_urea <- data[!is.na(data$blood_urea), ]
# Density plot for the "blood_urea" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_blood_urea, aes(x = blood_urea, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("CKD", "NotCKD")) +
  labs(title = "Distribution of Blood Urea by CKD Status", x = "Blood Urea", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "blood_urea" variable
data_cleaned_blood_urea <- data[!is.na(data$blood_urea), ]

# Violin plot with inner box plot for the "blood_urea" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_blood_urea, aes(x = factor(class), y = blood_urea, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#FF6F61","#6F8A91"), labels = c("CKD", "NotCKD" )) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Blood Urea by CKD Status", x = "CKD Status", y = "Blood Urea") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "haemoglobin" variable
data_cleaned_haemoglobin <- data[!is.na(data$haemoglobin), ]
# Density plot for the "haemoglobin" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_haemoglobin, aes(x = haemoglobin, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("CKD", "NotCKD"))  +
  labs(title = "Distribution of Hemoglobin by CKD Status", x = "Haemoglobin", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Violin plot with inner box plot for the "haemoglobin" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_haemoglobin, aes(x = factor(class), y = haemoglobin, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#FF6F61","#6F8A91"), labels = c("CKD", "NotCKD" )) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Hemoglobin by CKD Status", x = "CKD Status", y = "Haemoglobin") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "serum_creatinine" variable
data_cleaned_serum_creatinine <- data[!is.na(data$serum_creatinine), ]
# Density plot for the "serum_creatinine" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_serum_creatinine, aes(x = serum_creatinine, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("CKD", "NotCKD")) +
  labs(title = "Distribution of Serum Creatinine by CKD Status", x = "Serum Creatinine", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.min =  element_blank())
# Violin plot with inner box plot for the "serum_creatinine" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_serum_creatinine, aes(x = factor(class), y = serum_creatinine, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#FF6F61", "#6F8A91"), labels = c("CKD", "NotCKD")) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Serum Creatinine by CKD Status", x = "CKD Status", y = "Serum Creatinine") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "specific_gravity" variable
data_cleaned_specific_gravity <- data[!is.na(data$specific_gravity), ]
# Density plot for the "specific_gravity" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_specific_gravity, aes(x = specific_gravity, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), labels = c("NotCKD", "CKD")) +
  labs(title = "Distribution of Specific Gravity by CKD Status", x = "Specific Gravity", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Violin plot with inner box plot for the "specific_gravity" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_specific_gravity, aes(x = factor(class), y = specific_gravity, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#6F8A91", "#FF6F61"), labels = c("NotCKD", "CKD")) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of Specific Gravity by CKD Status", x = "CKD Status", y = "Specific Gravity") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Remove rows with missing values in the "white_blood_cell_count" variable
data_cleaned_white_blood_cell_count <- data[!is.na(data$white_blood_cell_count), ]

# Density plot for the "white_blood_cell_count" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_white_blood_cell_count, aes(x = white_blood_cell_count, fill = factor(class))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), labels = c("NotCKD", "CKD")) +
  labs(title = "Distribution of White Blood Cell Count by CKD Status", x = "White Blood Cell Count", y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

# Violin plot with inner box plot for the "white_blood_cell_count" variable differentiating between CKD and NotCKD
ggplot(data_cleaned_white_blood_cell_count, aes(x = factor(class), y = white_blood_cell_count, fill = factor(class))) +
  geom_violin(trim = FALSE, size = 0.5, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = c("#6F8A91", "#FF6F61"), labels = c("NotCKD", "CKD")) +
  scale_x_discrete(labels = c("CKD" = "CKD", "NotCKD" = "NotCKD")) +
  labs(title = "Distribution of White Blood Cell Count by CKD Status", x = "CKD Status", y = "White Blood Cell Count") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_blank())

#categorical variables preprocesing 
cat_cols




#DATA PRE PROCESSING
data1 <- read_csv("R DOCUMENTS/kidney.csv")
colnames(data1) <- c('ID', 'age', 'blood_pressure', 'specific_gravity', 'albumin', 'sugar', 'red_blood_cells', 'pus_cell',
                     'pus_cell_clumps', 'bacteria', 'blood_glucose_random', 'blood_urea', 'serum_creatinine', 'sodium',
                     'potassium', 'haemoglobin', 'packed_cell_volume', 'white_blood_cell_count', 'red_blood_cell_count',
                     'hypertension', 'diabetes_mellitus', 'coronary_artery_disease', 'appetite', 'peda_edema',
                     'anemia', 'class')

data <- as.data.frame(data1)

data$white_blood_cell_count <- as.numeric(data$white_blood_cell_count)
data$packed_cell_volume <- as.numeric(data$packed_cell_volume)
data$red_blood_cell_count <-as.numeric(data$red_blood_cell_count)
data$ID <- as.factor(data$ID)


# Supongamos que tu DataFrame se llama "data"
data_numeric <- data %>% select_if(is.numeric)  # Seleccionar variables numéricas
data_categorical <- data %>% select_if(is.character)  # Seleccionar variables categóricas



#PREPROSESING NUMERIC DATA

# Calcula la media de cada columna
column_means <- colMeans(data_numeric, na.rm = TRUE)

# Encuentra las posiciones de las celdas NA
nas <- is.na(data_numeric)

# Imputa los valores NA con las medias respectivas
data_numeric[nas] <- column_means[rep(1, sum(nas))]


#PREPROCESSING CATEGORICAL DATA

data_categorical <- data_categorical %>% 
  mutate(
    red_blood_cells = if_else(red_blood_cells == "normal", 0, 1),
    pus_cell = if_else(pus_cell == "normal", 0, 1),
    pus_cell_clumps = if_else(pus_cell_clumps == "notpresent", 0, 1),
    bacteria = if_else(bacteria == "notpresent", 0, 1),
    hypertension = if_else(hypertension == "no", 0, 1),
    diabetes_mellitus = if_else(diabetes_mellitus == "no", 0, 1),
    coronary_artery_disease = if_else(coronary_artery_disease == "no", 0, 1),
    appetite = if_else(appetite == "good", 0, 1),
    peda_edema = if_else(peda_edema == "no", 0, 1),
    anemia = if_else(anemia == "no", 0, 1),
    class = if_else(class == "notckd", 0, 1)
  )

# Reemplazar NA por la moda (0 o 1)
data_categorical <- data_categorical %>%
  mutate_all(~ ifelse(is.na(.), ifelse(sum(. == 0, na.rm = TRUE) >= sum(. == 1, na.rm = TRUE), 0, 1), .))

# Lista de nombres de las columnas binarias
column_names <- c(
  "red_blood_cells", "pus_cell", "pus_cell_clumps", "bacteria",
  "hypertension", "diabetes_mellitus", "coronary_artery_disease",
  "appetite", "peda_edema", "anemia", "class"
)

# Convertir las columnas numéricas en categóricas
data_categorical[column_names] <- lapply(data_categorical[column_names], as.factor)

summary(data_categorical)


# Crear variables dummy para las columnas categóricas en data_categorical
data_dummy <- dummy_cols(data_categorical, select_columns = categorical_column_names)

# Seleccionar las primeras 11 variables del DataFrame data_dummy
data_dummy_subset <- data_dummy[, 1:11]
summary(data_combined)
# Combinar data_numeric y data_dummy_subset
data_combined <- cbind(data_numeric, data_dummy_subset)

#MODEL BUILDING


# Definir las variables independientes (características) y dependientes (etiquetas)
ind_col <- setdiff(names(data_combined), "class")
dep_col <- "class"

X <- data_combined[, ind_col]
y <- data_combined[, dep_col]

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(0) # Establecer una semilla aleatoria para reproducibilidad
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

# Entrenar el modelo KNN
knn_model <- knn(X_train, X_test, y_train, k = 5)
knn_acc <- mean(knn_model == y_test)

# Imprimir resultados del modelo KNN
cat("Test Accuracy of KNN is:", knn_acc, "\n")


#ARBOL DE DECISIONES
# Entrenar el modelo de árbol de decisión
dtc_model <- rpart(y_train ~ ., data = X_train)
dtc_pred <- predict(dtc_model, X_test, type = "class")
dtc_acc <- mean(dtc_pred == y_test)

# Imprimir resultados del modelo de árbol de decisión
cat("Test Accuracy of Decision Tree Classifier is:", dtc_acc, "\n")

#RANDOM FOREST CLASSIFIER

# Entrenar el modelo de bosque aleatorio
rf_model <- randomForest(X_train, as.factor(y_train))
rf_pred <- predict(rf_model, X_test)
rf_acc <- mean(rf_pred == y_test)

# Imprimir resultados del modelo de bosque aleatorio
cat("Test Accuracy of Random Forest Classifier is:", rf_acc, "\n")

# Entrenar el modelo AdaBoost
ada_model <- ada(dtc_model, data = X_train, iter = 50)
ada_pred <- predict(ada_model, X_test)
ada_acc <- mean(ada_pred == y_test)

# Imprimir resultados del modelo AdaBoost
cat("Test Accuracy of Ada Boost Classifier is:", ada_acc, "\n")

# Entrenar el modelo Gradient Boosting
library(xgboost)
gb_model <- xgboost(data = X_train, label = y_train, nrounds = 100)
gb_pred <- predict(gb_model, X_test)
gb_pred <- ifelse(gb_pred > 0.5, 1, 0)
gb_acc <- mean(gb_pred == y_test)

# Imprimir resultados del modelo Gradient Boosting
cat("Test Accuracy of Gradient Boosting Classifier is:", gb_acc, "\n")