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
target_class <- "dipg"  
# 3. Construire y_bin = 1 si target_class, 0 sinon
y_bin <- as.integer(y == target_class)

# Vérifions bien qu'on a un vecteur 0/1
table(y_bin)
#   0    1 
# xxx   yyy 

# 4. Préparer la liste de matrices (toutes les observations conservées)
X_list <- list(
  GE  = scale(X1),    # on standardise si besoin
  CGH = scale(X2)
)

# 5. Lancer cv.multiview en binaire
multiview.control(itrace = 1)  # active la barre de progression
cv_fit <- cv.multiview(
  x_list       = X_list,
  y            = y_bin,
  family       = binomial(),    # logistique binaire
  rho          = 0.5,           
  trace.it     = 1,
  alpha=0.5,
  nfolds=4
)
plot(cv_fit)
