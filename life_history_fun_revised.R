#Explication: Ce script a pour but de calculer des variables de traits d'histoire de vie pour tous les individus nées à Charlevoix (1680 - 1944) et SLSJ (1842-1980), ex. age a la premiere reproduction ou longevité ou moyen d'intervalle entre naissances.
#Pour ce but, j'ai besoin des dates complètes (naissance, décés, et mariage) pour avoir des calculs précis de ces variables. La structure des jeux de données est indiqué ci-dessus.
#J'ai attaché un fichier sample.txt juste pour representer la structure des jeux de données déja utilisé aupravant pour le bon déroulement de ce script.


dat <- read.csv(file = "C:/Users/Crampton-Mawass/Downloads/sample.txt", header = TRUE, stringsAsFactors = FALSE,na.strings=c(""," ","NA")) #inputer le nom du fihcier contenant les jeux de données
  # charger un jeu de données "dat" concernant les individus nés au Charlevoix puis SLSJ (séparement), dont les 1eres colonnes sont dans l'ordre (peu importe les noms)
  #1. identifiant de l'individu
  #2. identifiant du pére
  #3. identifiant de la mére
  #4. sexe (1 pour les hommes et 2 pour les femmes)
  #5. année de naissance (si dates complétes ou mélange de dates et d'années, ca marche mais il faut installer la library lubridate avant de faire tourner)
  #6. année de décés (idem)
  #7 année de mariage (idem)

colnames(dat)<-c("ind","pere","mere","sexe","birthy","dated","datm")

#changer le format des colonnes de dates
dat$birthy <- as.Date(dat$birthy, format = "%m/%d/%Y")
dat$dated <- as.Date(dat$dated, format = "%m/%d/%Y")
dat$datm <- as.Date(dat$datm, format = "%m/%d/%Y")

#changer le format des colonnes contenant les variables
dat <- dat[is.na(dat$birthy) == FALSE,]
dat$ind <- as.numeric(dat$ind)
dat$mere <- as.numeric(dat$mere)
dat$pere <- as.numeric(dat$pere)
dat$sexe <- as.numeric(dat$sexe)

######
cohort_parent <- function(dat){
	dat$firstcohort <- rep(0,nrow(dat))
	class(dat$firstcohort) <- "Date"
	dat$lastcohort <- rep(0,nrow(dat))
	class(dat$lastcohort) <- "Date"
	for (i in 1:nrow(dat)) {
	if (dat$ind[i] %in% dat$pere) {
		birthys <- na.omit(dat[dat$pere == dat$ind[i],]$birthy)
		dat$firstcohort[i] <- min(birthys)
		dat$lastcohort[i] <- max(birthys)
	}
	else if (dat$ind[i] %in% dat$mere) {
		birthys <- na.omit(dat[dat$mere == dat$ind[i],]$birthy)
		dat$firstcohort[i] <- min(birthys)
		dat$lastcohort[i] <- max(birthys)
	}
	}
	dat[which(dat$firstcohort == 0),]$firstcohort <- NA
	dat[which(dat$lastcohort == 0),]$lastcohort <- NA
	return(dat)
}
#####
dat <- cohort_parent(dat) #calculate the first and last cohort to which an indivdiual contributed a child
#calculate longevity
dat$longevity <- ifelse(is.na(dat$dated) == T, NA, as.numeric(dat$dated-dat$birthy)/365) #calculate longevity for each ind with known date of death
#calculate age at first reproduction
dat$AFR <- ifelse(is.na(dat$firstcohort) == T, NA, as.numeric(dat$firstcohort-dat$birthy)/365) #calculate AFR for each ind with known reproduction
#calculate age at last reproduction
dat$ALR <- ifelse(is.na(dat$lastcohort), NA, as.numeric(dat$lastcohort-dat$birthy)/365) #calculate ALR for each ind with known reproduction
#calculate interval between marriage and first repro
dat$MFBI <- ifelse(is.na(dat$AFR) == T, NA, as.numeric(dat$firstcohort-dat$datm)) #calculate MFBI for each ind with known reproduction
#calculate twinning briths 0-no twin births 1- at least 1 twin birth
dat$TWIN <- rep(0,nrow(dat))

for (i in 1:nrow(dat)) {
  if (dat$ind[i] %in% dat$pere) {
    dup <- sum(duplicated(na.omit(dat[dat$pere == dat$ind[i],]$birthy)))
    dat$TWIN[i]<- dup
  } else if (dat$ind[i] %in% dat$mere) {
    dup <- sum(duplicated(na.omit(dat[dat$mere == dat$ind[i],]$birthy)))
    dat$TWIN[i]<-dup
  } else {
    dat$TWIN[i] <- 0
  }
}
dat$TWIN <- ifelse(dat$TWIN >= 1, 1,0)

#calculate mean interval between births
dat$MIBI <- rep(0,nrow(dat))

for (i in 1:nrow(dat)) {
  if (dat$ind[i] %in% dat$pere) {
    birthy <- sort(unique(na.omit(dat[dat$pere == dat$ind[i],]$birthy)))
    int <- mean(birthy[-1] - birthy[-length(birthy)])
    dat$MIBI[i] <- int[[1]] 
  } else if (dat$ind[i] %in% dat$mere) {
    birthy <- sort(unique(na.omit(dat[dat$mere == dat$ind[i],]$birthy)))
    int <- mean(birthy[-1] - birthy[-length(birthy)])
    dat$MIBI[i] <- int[[1]]
  } else {
    dat$MIBI[i] <- NA
  }
}

dat[which(is.nan(dat$MIBI) == T),]$MIBI <- 0

write.table(dat[,-c(2:7)],file="life_history.txt")

#Contactez moi par courriel walidmawass10@gmail.com pour toute question