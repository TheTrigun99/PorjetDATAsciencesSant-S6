Les principaux codes sont CodeMultiview.R et RGCCAcode.R qui sont les 2 différents modèles avec pipeline utilisés durant mon projet DATA SCIENCE en Santé pendant le S5.
Le Notebook a été fais par moi et un collègue, les messages de warning sont dû au fait qu'une des 3 classes est minoritaire. Ce problème provoque une instabilité pour le modèle afin de prédire cette classe, on le voit notamment quand je fais
du one vs all avec multiview.
Les fichiers .rds sont des modèles enregistrés et entraîné pour éviter de tout réentrainer à chaque fois (cela prend du temps les données étant conséquente et à très haute dimesion).
Des graphes concernant les résultats sont inclus ainsi que le rapport de projet.
