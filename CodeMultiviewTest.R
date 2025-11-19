library(multiview)
library(gliomaData)

# 1. Charger et préparer les données
data(ge_cgh_locIGR)
blocks  <- ge_cgh_locIGR$multiblocks
y_raw   <- factor(ge_cgh_locIGR$y)
levels(y_raw) <- colnames(blocks$y)  # noms explicites des classes

X1 <- blocks$GE    # bloc 1 : GE
X2 <- blocks$CGH   # bloc 2 : CGH
y  <- y_raw        # cible factor

# 2. Choisir la classe "positive" (celle qu'on code 1)
target_class <- "midl"  
# 3. Construire y_bin = 1 si target_class, 0 sinon
y_bin <- as.integer(y == target_class)
n           <- length(y_bin)
train_idx   <- sample.int(n, size = floor(0.75 * n))
test_idx    <- setdiff(seq_len(n), train_idx)

X_train <- list(
  GE  = X1[train_idx, , drop = FALSE],
  CGH = X2[train_idx, , drop = FALSE]
)
X_test  <- list(
  GE  = X1[test_idx, , drop = FALSE],
  CGH = X2[test_idx, , drop = FALSE]
)
y_train <- y_bin[train_idx]
y_test  <- y_bin[test_idx]

multiview.control(itrace = 1)                 # pas de barre si on veut
multi <- multiview(
  x_list       = X_train,
  y            = y_train,
  family       = binomial(),
  rho          = 0.5,
  trace.it=1
)
## 6. Prédiction sur le jeu de test -------------------------------------------


lambdas <- multi$lambda

# Initialisation
accuracies <- numeric(length(lambdas))

# Boucle sur les lambdas
for (i in seq_along(lambdas)) {
  lambda_val <- lambdas[i]
  
  pred_test <- predict(
    multi,
    newx = X_test,
    s    = lambda_val,
    type = "class"
  )
  
  accuracies[i] <- mean(pred_test == y_test)
}

# Trouver le lambda optimal et sa précision
best_index <- which.max(accuracies)
best_lambda <- lambdas[best_index]
best_accuracy <- accuracies[best_index]
best_pred <- predict(
  multi,
  newx = X_test,
  s    = best_lambda,
  type = "class"
)
tp <- sum(best_pred == 1 & y_test == 1)  # True positives
tn <- sum(best_pred == 0 & y_test == 0)  # True negatives
fp <- sum(best_pred == 1 & y_test == 0)  # False positives
fn <- sum(best_pred == 0 & y_test == 1)  # False negatives

# Sensibilité = TP / (TP + FN)
sensitivity <- tp / (tp + fn)

# Spécificité = TN / (TN + FP)
specificity <- tn / (tn + fp)

# Affichage des résultats
cat("Meilleur lambda :", best_lambda, "\n")
cat("Précision maximale :", best_accuracy, "\n")
cat("Sensibilité :", round(sensitivity, 4), "\n")
cat("Spécificité :", round(specificity, 4), "\n")
cat("Meilleur lambda :", best_lambda, "\n")
cat("Précision maximale :", best_accuracy, "\n")
