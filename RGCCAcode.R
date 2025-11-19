library(gliomaData)
library(RGCCA)
library(caret)
# 3. Charger les données
data("ge_cgh_locIGR", package = "gliomaData")
# 4. Préparer les blocs
blocks <- ge_cgh_locIGR$multiblocks
Loc <- factor(ge_cgh_locIGR$y)
levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
blocks[[3]] <- Loc  # Le bloc 3 devient la variable de localisation (cible)

# 5. Vérifier le nombre de colonnes dans chaque bloc
vapply(blocks, NCOL, FUN.VALUE = 1L)
fit.sgcca <- rgcca(blocks = blocks, response = 3, ncomp = 2,sparsity = c(0.2, 0.2000, 1),verbose = FALSE,method='sgcca')
summary(fit.sgcca)
#fit_stab <- rgcca_stability(fit.sgcca,keep = vapply(fit.sgcca$a, function(x) mean(x != 0),FUN.VALUE = 1.0),n_boot = 100, verbose = TRUE, n_cores = 1)
set.seed(0)
in_train <- caret::createDataPartition(blocks[[3]], p = .75, list = FALSE)
training <- lapply(blocks, function(x) as.matrix(x)[in_train, , drop = FALSE])
testing <- lapply(blocks, function(x) as.matrix(x)[-in_train, , drop = FALSE])
cv_out <- rgcca_cv(blocks = training, response = 3,par_type = "sparsity",par_value = c(.2, .2, 0),
                        par_length = 10,
                        prediction_model = "lda",
                        validation = "kfold",
                        k = 7, n_run = 3, metric = "Balanced_Accuracy",
                        n_cores = 2)

fit <- rgcca(cv_out)
fit_stabi <- rgcca_stability(fit,
                            keep = vapply(
                              fit$a, function(x) mean(x != 0),
                             FUN.VALUE = 1.0
                              ),
                            n_boot = 100, verbose = TRUE, n_cores = 2)
boot_out <- rgcca_bootstrap(fit_stabi, n_boot = 500)
plot(boot_out, block = 1,
     display_order = FALSE,
     n_mark = 50, cex = 1.5, cex_sub = 17,
     show_star = TRUE)

pred <- rgcca_predict(fit, blocks_test = testing, prediction_model = "lda")
pred$confusion$test



library(ggplot2)

# Sélection de la variable d'intérêt
var_name <- "A_23_P205428"
df <- data.frame(
  Variable = blocks$GE[, var_name],
  Loc = blocks[[3]]
)

# Boxplot
ggplot(df, aes(x = Loc, y = Variable, fill = Loc)) +
  geom_boxplot() +
  labs(title = paste("Distribution de", var_name, "selon la localisation"),
       x = "Localisation", y = "Valeur")é
