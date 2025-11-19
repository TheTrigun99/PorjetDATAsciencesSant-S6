library(multiview)
library(gliomaData)

data(ge_cgh_locIGR)
blocks  <- ge_cgh_locIGR$multiblocks
y_raw   <- factor(ge_cgh_locIGR$y)
levels(y_raw) <- colnames(blocks$y)  # noms explicites des classes

X1 <- blocks$GE    # bloc 1 : GE
X2 <- blocks$CGH   # bloc 2 : CGH
y  <- y_raw        # cible factor

target_class <- "dipg"  
#Construire y_bin = 1 si target_class, 0 sinon
y_bin <- as.integer(y == target_class)

# Vérifions bien qu'on a un vecteur 0/1
table(y_bin)

# Préparer la liste de matrices (toutes les observations conservées)
X_list <- list(
  GE  = scale(X1),    # on standardise si besoin
  CGH = scale(X2)
)

multiview.control(itrace = 1) 


cv_fit <- cv.multiview(
  x_list       = X_list,
  y            = y_bin,
  family       = binomial(),    
  rho          = 0.5,           
  trace.it     = 1,
  alpha=0.5,
  nfolds=5)

