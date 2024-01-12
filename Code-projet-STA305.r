# Importation des données

data_projet <- read.csv("C:/Users/victo/Documents/ISPED/M2/STA305/Projet/data_projet.csv",sep=";",dec=".")
str(data_projet)
summary(data_projet)
sd(data_projet$LAD)
sd(data_projet$SAD)
summary(as.factor(data_projet$upper_abdomen))

groupeD <- subset(data_projet, data_projet$Fum_peak=="D")
summary(groupeD)
sd(groupeD$LAD)
sd(groupeD$SAD)

table(data_projet$Site_MRS)

### ANALYSE 1 (D VS U) ###

# Création variable à expliquer analyse 1

data_projet$Fum_peak_D<-ifelse(data_projet$Fum_peak=="D",1,ifelse(data_projet$Fum_peak=="U",0,NA))

# Recodage variables qualitatives explicatives

data_projet$Gender<-ifelse(data_projet$Gender=="M",1,0)
data_projet$upper_abdomen<-ifelse(data_projet$upper_abdomen=="Yes",1,0)
data_projet$Th_strategies<-ifelse(is.na(data_projet$Th_strategies),0,1)
data_projet$Primary_t<-ifelse(data_projet$Primary_t=="Yes",1,0)
data_projet$Lymph_node_metastasis<-ifelse(data_projet$Site_MRS=="Lymph node",1,0)

# Importation modèle JAGS dans R

library(rjags)

data_analyse_1 <- subset(data_projet, data_projet$Fum_peak_D!="NA")

jags_1 <- jags.model("C:/Users/victo/Documents/ISPED/M2/STA305/Projet/model_analyse_1.txt",data=list(y_obs=data_analyse_1$Fum_peak_D,N=length(data_analyse_1$Fum_peak_D),x1=data_analyse_1$Gender,x2=data_analyse_1$Age,x3=data_analyse_1$LAD,x4=data_analyse_1$SAD,x5=data_analyse_1$Voxel,x6=data_analyse_1$upper_abdomen,x7=data_analyse_1$Th_strategies,x8=data_analyse_1$Primary_t,x9=data_analyse_1$Lymph_node_metastasis))

# Echantillon de taille 50000 des distributions a posteriori des coefficients beta

res_1 <- coda.samples(model = jags_1, variable.names = c("logbeta1", "logbeta2", "logbeta3", "logbeta4", "logbeta5", "logbeta6", "logbeta7", "logbeta8", "logbeta9"), n.iter = 50000)

plot(res_1)
plot(res_1[[1]][,4])
plot(res_1[[1]][,1]) # plot pour logbeta1
plot(res_1[[1]][,2]) # plot pour logbeta2

# Aide pour sélectionner certains coefficients dans res_1

res_1[[1]][1,1] # premier coefficient de logbeta1 dans res_1
res_1[[1]][,1] # colonne logbeta1 dans res_1
res_1[[1]][1:5,2] # 5 premières lignes de logbeta2 dans res_1
head(res_1[[1]]) # 7 premières lignes de tous les coefficients logbeta dans res_1

# Estimateurs moyennes et médianes a posteriori pour chaque coefficient beta et intervalles de crédibilité à 95 %

resume_1 <- summary(res_1)
resume_1

resume_1[[1]][,1]
exp(resume_1[[1]][,1])

resume_1$statistics["logbeta1", "Mean"]
resume_1$statistics["logbeta2", "Mean"]
resume_1$statistics["logbeta3", "Mean"]
resume_1$statistics["logbeta4", "Mean"]
resume_1$statistics["logbeta5", "Mean"]
resume_1$statistics["logbeta6", "Mean"]
resume_1$statistics["logbeta7", "Mean"]
resume_1$statistics["logbeta8", "Mean"]
resume_1$statistics["logbeta9", "Mean"]

resume_1$quantiles["logbeta1", "50%"]
resume_1$quantiles["logbeta2", "50%"]
resume_1$quantiles["logbeta3", "50%"]
resume_1$quantiles["logbeta4", "50%"]
resume_1$quantiles["logbeta5", "50%"]
resume_1$quantiles["logbeta6", "50%"]
resume_1$quantiles["logbeta7", "50%"]
resume_1$quantiles["logbeta8", "50%"]
resume_1$quantiles["logbeta9", "50%"]

resume_1$quantiles["logbeta1", c(1,5)]
resume_1$quantiles["logbeta2", c(1,5)]
resume_1$quantiles["logbeta3", c(1,5)]
resume_1$quantiles["logbeta4", c(1,5)]
resume_1$quantiles["logbeta5", c(1,5)]
resume_1$quantiles["logbeta6", c(1,5)]
resume_1$quantiles["logbeta7", c(1,5)]
resume_1$quantiles["logbeta8", c(1,5)]
resume_1$quantiles["logbeta9", c(1,5)]

# Etude de la convergence de l'algorithme MCMC

library(coda)

jags2_1 <- jags.model("C:/Users/victo/Documents/ISPED/M2/STA305/Projet/model_analyse_1.txt", data=list(y_obs=data_analyse_1$Fum_peak_D,N=length(data_analyse_1$Fum_peak_D),x1=data_analyse_1$Gender,x2=data_analyse_1$Age,x3=data_analyse_1$LAD,x4=data_analyse_1$SAD,x5=data_analyse_1$Voxel,x6=data_analyse_1$upper_abdomen,x7=data_analyse_1$Th_strategies,x8=data_analyse_1$Primary_t,x9=data_analyse_1$Lymph_node_metastasis), n.chains = 3 )

res2_1 <- coda.samples(model=jags2_1, variable.names = c("logbeta1", "logbeta2", "logbeta3", "logbeta4", "logbeta5", "logbeta6", "logbeta7", "logbeta8", "logbeta9"), n.iter = 50000, inits=list(list(logbeta1=1,logbeta2=1,logbeta3=1,logbeta4=1,logbeta5=1,logbeta6=1,logbeta7=1,logbeta8=1,logbeta9=1),list(logbeta1=2,logbeta2=2,logbeta3=2,logbeta4=2,logbeta5=2,logbeta6=2,logbeta7=2,logbeta8=2,logbeta9=2),list(logbeta1=3,logbeta2=3,logbeta3=3,logbeta4=3,logbeta5=3,logbeta6=3,logbeta7=3,logbeta8=3,logbeta9=3)))

plot(res2_1)
plot(res2_1[[1]][,2])
gelman.plot(res2_1)

acfplot(res2_1)

par(mfrow = c(3, 2))
autocorr.plot(res2_1, ask = FALSE, auto.layout = FALSE,lag.max=500)
?autocorr.plot
# Un lag de 100 permettrait de diminuer l'autocorrélation, même s'il en resterait toujours un peu
par("mar")
par(mar=c(1,1,1,1))
setwd("C:/Users/victo/Documents/ISPED/M2/STA305/Projet")
dev.print(device=png, file="plot autocorr.png", width=600)

# évaluation de la convergence par quantiles cumulés
cumuplot(res2_1, ask=FALSE,auto.layout=FALSE)
# On a bien convergé vers une loi stationnaire

# Retirer la phase de chauffe des 30000 premières itérations afin d'atteindre la convergence de la chaîne de Markov vers sa loi stationnaire
res2_1_burnt <- window(res2_1,start=30000)
# résultats pour tableau 1
summary(res2_1)
res <- summary(res2_1_burnt)
res
res[[1]][,1]
exp(res[[1]][,1])

# affichage des traces et de la densité après avoir retiré la phase de chauffe
plot(res2_1_burnt)
# diagramme de Gelman Rubin
gelman.plot(res2_1_burnt)
# autocorrélation des paramètres
acfplot(res2_1_burnt)
# quantiles cumulés
par(mfrow=c(3,3))
cumuplot(res2_1_burnt,ask=F,auto.layout=F)

# corrélations entre les paramètres
par(mfrow=c(1,1))
crosscorr.plot(res2_1_burnt)

# Intervalles de crédibilité de plus haute densité a posteriori à 95% et comparaison avec ceux obtenus à partir des quantiles 2,5% et 97,5%

hdCI1 <- HDInterval::hdi(res2_1_burnt)
hdCI1

symCI1 <- summary(res2_1_burnt)$quantiles[, c(1, 5)]
symCI1

symCI1[,2]-symCI1[,1]
hdCI1[2,] - hdCI1[1,]
# Les intervalles de crédibilité dans hdCI sont les IC95% les plus étroits possible.

### ANALYSE 2 (TF VS D/U) ###

# Création variable à expliquer analyse 2

data_projet$Fum_peak_TF<-ifelse(data_projet$Fum_peak=="TF",1,0)

# Importation modèle JAGS dans R

library(rjags)
jags_2 <- jags.model("C:/Users/victo/Documents/ISPED/M2/STA305/Projet/model_analyse_2.txt",data=list(y_obs=data_projet$Fum_peak_TF,N=length(data_projet$Fum_peak_TF),x1=data_projet$Gender,x2=data_projet$Age,x3=data_projet$LAD,x4=data_projet$SAD,x5=data_projet$upper_abdomen,x6=data_projet$Th_strategies,x7=data_projet$Primary_t,x8=data_projet$Lymph_node_metastasis))

# Echantillon de taille 50000 des distributions a posteriori des coefficients beta

res_2 <- coda.samples(model = jags_2, variable.names = c("logbeta1", "logbeta2", "logbeta3", "logbeta4", "logbeta5", "logbeta6", "logbeta7", "logbeta8"), n.iter = 50000)

plot(res_2)
plot(res_2[[1]][,1]) # plot pour logbeta1
plot(res_2[[1]][,2]) # plot pour logbeta2

# Estimateurs moyennes et médianes a posteriori pour chaque coefficient beta et intervalles de crédibilité à 95 %

resume_2 <- summary(res_2)
resume_2

exp(resume_2[[1]][,1])

resume_2$statistics["logbeta1", "Mean"]
resume_2$statistics["logbeta2", "Mean"]
resume_2$statistics["logbeta3", "Mean"]
resume_2$statistics["logbeta4", "Mean"]
resume_2$statistics["logbeta5", "Mean"]
resume_2$statistics["logbeta6", "Mean"]
resume_2$statistics["logbeta7", "Mean"]
resume_2$statistics["logbeta8", "Mean"]

resume_2$quantiles["logbeta1", "50%"]
resume_2$quantiles["logbeta2", "50%"]
resume_2$quantiles["logbeta3", "50%"]
resume_2$quantiles["logbeta4", "50%"]
resume_2$quantiles["logbeta5", "50%"]
resume_2$quantiles["logbeta6", "50%"]
resume_2$quantiles["logbeta7", "50%"]
resume_2$quantiles["logbeta8", "50%"]

resume_2$quantiles["logbeta1", c(1,5)]
resume_2$quantiles["logbeta2", c(1,5)]
resume_2$quantiles["logbeta3", c(1,5)]
resume_2$quantiles["logbeta4", c(1,5)]
resume_2$quantiles["logbeta5", c(1,5)]
resume_2$quantiles["logbeta6", c(1,5)]
resume_2$quantiles["logbeta7", c(1,5)]
resume_2$quantiles["logbeta8", c(1,5)]

# Etude de la convergence de l'algorithme MCMC

library(coda)

jags2_2 <- jags.model("C:/Users/victo/Documents/ISPED/M2/STA305/Projet/model_analyse_2.txt", data=list(y_obs=data_projet$Fum_peak_TF,N=length(data_projet$Fum_peak_TF),x1=data_projet$Gender,x2=data_projet$Age,x3=data_projet$LAD,x4=data_projet$SAD,x5=data_projet$upper_abdomen,x6=data_projet$Th_strategies,x7=data_projet$Primary_t,x8=data_projet$Lymph_node_metastasis), n.chains = 3 )

res2_2 <- coda.samples(model=jags2_2, variable.names = c("logbeta1", "logbeta2", "logbeta3", "logbeta4", "logbeta5", "logbeta6", "logbeta7", "logbeta8"), n.iter = 50000, inits=list(list(logbeta1=1,logbeta2=1,logbeta3=1,logbeta4=1,logbeta5=1,logbeta6=1,logbeta7=1,logbeta8=1),list(logbeta1=2,logbeta2=2,logbeta3=2,logbeta4=2,logbeta5=2,logbeta6=2,logbeta7=2,logbeta8=2),list(logbeta1=3,logbeta2=3,logbeta3=3,logbeta4=3,logbeta5=3,logbeta6=3,logbeta7=3,logbeta8=3)))

plot(res2_2)
plot(res2_2[[1]][,2])
gelman.plot(res2_2)

acfplot(res2_2)

par(mfrow = c(3, 2))
autocorr.plot(res2_2, ask = FALSE, auto.layout = FALSE)
par("mar")
par(mar=c(1,1,1,1))
setwd("C:/Users/victo/Documents/ISPED/M2/STA305/Projet")
dev.print(device=png, file="plot autocorr.png", width=600)

cumuplot(res2_2, ask = FALSE, auto.layout = FALSE)
# On a bien convergé vers une loi stationnaire

# Retirer la phase de chauffe des 20000 premières itérations afin d'atteindre la convergence de la chaîne de Markov vers sa loi stationnaire
res2_2_burnt <- window(res2_2,start=20000)
# résultats pour tableau 3
res2 <- summary(res2_2_burnt)
res2
res2[[1]][,1]
exp(res2[[1]][,1])

# affichage des traces et de la densité après avoir retiré la phase de chauffe
plot(res2_2_burnt)
# diagramme de Gelman Rubin
gelman.plot(res2_2_burnt)
# autocorrélation des paramètres
acfplot(res2_2_burnt)
# quantiles cumulés
cumuplot(res2_2_burnt,ask=F,auto.layout=F)

# corrélations entre les paramètres
par(mfrow=c(1,1))
crosscorr.plot(res2_2_burnt)

# Intervalles de crédibilité de plus haute densité a posteriori à 95% et comparaison avec ceux obtenus à partir des quantiles 2,5% et 97,5%

hdCI2 <- HDInterval::hdi(res2_2_burnt)
hdCI2

symCI2 <- summary(res2_2_burnt)$quantiles[, c(1, 5)]
symCI2

symCI2[,2]-symCI2[,1]
hdCI2[2,] - hdCI2[1,]
# Les intervalles de crédibilité dans hdCI sont les IC95% les plus étroits possible.

# Analyse de sensibilité 1 : priors uniformes (-10,10)

jags_sens <- jags.model("C:/Users/victo/Documents/ISPED/M2/STA305/Projet/model_analyse_sensibilite.txt",data=list(y_obs=data_analyse_1$Fum_peak_D,N=length(data_analyse_1$Fum_peak_D),x1=data_analyse_1$Gender,x2=data_analyse_1$Age,x3=data_analyse_1$LAD,x4=data_analyse_1$SAD,x5=data_analyse_1$Voxel,x6=data_analyse_1$upper_abdomen,x7=data_analyse_1$Th_strategies,x8=data_analyse_1$Primary_t,x9=data_analyse_1$Lymph_node_metastasis))

# Echantillon de taille 50000 des distributions a posteriori des coefficients beta

res_sens <- coda.samples(model = jags_sens, variable.names = c("logbeta1", "logbeta2", "logbeta3", "logbeta4", "logbeta5", "logbeta6", "logbeta7", "logbeta8", "logbeta9"), n.iter = 50000)

plot(res_sens)

# Estimateurs moyennes et médianes a posteriori pour chaque coefficient beta et intervalles de crédibilité à 95 %

res_sens_burnt <- window(res_sens,start=30000)

resume_sens <- summary(res_sens_burnt)
resume_sens

resume_sens[[1]][,1]
exp(resume_sens[[1]][,1])

# Intervalles de crédibilité de plus haute densité a posteriori à 95% et comparaison avec ceux obtenus à partir des quantiles 2,5% et 97,5%

hdCI_sens <- HDInterval::hdi(res_sens)
hdCI_sens

symCI_sens <- summary(res_sens)$quantiles[, c(1, 5)]
symCI_sens

symCI_sens[,2]-symCI_sens[,1]
hdCI_sens[2,] - hdCI_sens[1,]
# Les intervalles de crédibilité dans hdCI sont les IC95% les plus étroits possible.

# Analyse de sensibilité 2 : prior gaussiens (0, 0.1) = variance de 10 (plus faible que analyse 1)

jags_sens_2 <- jags.model("C:/Users/victo/Documents/ISPED/M2/STA305/Projet/model_analyse_sensibilite_2.txt",data=list(y_obs=data_analyse_1$Fum_peak_D,N=length(data_analyse_1$Fum_peak_D),x1=data_analyse_1$Gender,x2=data_analyse_1$Age,x3=data_analyse_1$LAD,x4=data_analyse_1$SAD,x5=data_analyse_1$Voxel,x6=data_analyse_1$upper_abdomen,x7=data_analyse_1$Th_strategies,x8=data_analyse_1$Primary_t,x9=data_analyse_1$Lymph_node_metastasis))

# Echantillon de taille 50000 des distributions a posteriori des coefficients beta

res_sens_2 <- coda.samples(model = jags_sens_2, variable.names = c("logbeta1", "logbeta2", "logbeta3", "logbeta4", "logbeta5", "logbeta6", "logbeta7", "logbeta8", "logbeta9"), n.iter = 50000)

plot(res_sens_2)

# Estimateurs moyennes et médianes a posteriori pour chaque coefficient beta et intervalles de crédibilité à 95 %

resume_sens_2 <- summary(res_sens_2)
resume_sens_2

resume_sens_2[[1]][,1]
exp(resume_sens_2[[1]][,1])

# Intervalles de crédibilité de plus haute densité a posteriori à 95% et comparaison avec ceux obtenus à partir des quantiles 2,5% et 97,5%

hdCI_sens_2 <- HDInterval::hdi(res_sens_2)
hdCI_sens_2

symCI_sens_2 <- summary(res_sens_2)$quantiles[, c(1, 5)]
symCI_sens_2

symCI_sens_2[,2]-symCI_sens_2[,1]
hdCI_sens_2[2,] - hdCI_sens_2[1,]
# Les intervalles de crédibilité dans hdCI sont les IC95% les plus étroits possible.
