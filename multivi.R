library(multiview)
library(gliomaData)
data(ge_cgh_locIGR)

# 2. Extraire les blocs et la cible
blocks <- ge_cgh_locIGR$multiblocks
y_raw   <- factor(ge_cgh_locIGR$y)
levels(y_raw) <- colnames(blocks$y)    # noms explicites des classes

X1 <- blocks$GE    # bloc 1 : GE
X2 <- blocks$CGH   # bloc 2 : CGH
y  <- y_raw        # cible factor

# 3. Choisir la classe à enlever
classe_remove <- "dipg"

# 4. Construire l’indice des observations à garder
keep_idx <- which(y != classe_remove)

# 5. Filtrer chaque bloc et la cible
X1_filt <- X1[keep_idx, , drop = FALSE]
X2_filt <- X2[keep_idx, , drop = FALSE]
y_filt  <- droplevels(y[keep_idx])

# 6. Vérifier qu’il ne reste que deux niveaux
table(y_filt)
X_list <- list(GE = X1_filt, CGH = X2_filt)
y_filt <- as.numeric(y_filt)-1

# 2. Entraînement avec validation croisée
set.seed(123)
multiview.control(itrace = 1)
cv_fit <- cv.multiview(X_list,
  y            = y_filt,
  family       = binomial(),     # modèle logistique binaire
  rho          = 0.25,            # poids d’accord entre vues (à ajuster)
  nfolds       = 5,
  trace.it     = 1,
  type.measure = "class" 
)

top_vars <- coef_ordered(cv_fit, s = "lambda.min")
plot(cv_fit)
