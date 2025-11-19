library(glmnet)
library(gliomaData)
library(ggplot2)
data(CGH_annot)
data(GE_annot)
data("ge_cgh_locIGR", package = "gliomaData")
# Afficher les dimensions

set.seed(123)
blocks <- ge_cgh_locIGR$multiblocks
Loc <- factor(ge_cgh_locIGR$y)
levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
blocks[[3]] <- Loc
print(sapply(blocks, dim))

x= blocks[[1]]

y= blocks[[3]]
cv.lasso <- cv.glmnet(x, y, alpha = 1,type.measure = "class", family = "multinomial")
print(plot(cv.lasso))
pred <- predict(cv.lasso, newx = x, s = "lambda.min", type = "class")
pred <- as.factor(pred)

# Matrice de confusion
conf_df <- as.data.frame(table(Vraie = y, Predite = pred))

# Affichage graphique
print(ggplot(conf_df, aes(x = Vraie, y = Predite, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matrice de confusion LASSO logistique",
       x = "Classe réelle", y = "Classe prédite") +
  theme_minimal())
