#####data entry#####
load(file = "C:/Users/Crampton-Mawass/Desktop/Work/Selection_analysis/ped_saguenay.rda")

load(file = "C:/Users/Crampton-Mawass/Desktop/Work/Selection_analysis/ped_charlevoix.rda")
#life history data
char_lht <- read.table(file = "C:/Users/Crampton-Mawass/Desktop/Work/Selection_analysis/life_history_charlevoix.txt")
char_lht <- char_lht[,-c(2,3)]
colnames(char_lht)[1] <- "id"
char_lht$id <- as.factor(char_lht$id)

#fix issue with negative longevity due to civil act discrepancies
char_lht[which(char_lht$longevity < 0 & char_lht$longevity > -2),]$longevity <- 0

sag_lht <- read.table(file = "C:/Users/Crampton-Mawass/Desktop/Work/Selection_analysis/life_history_saguenay.txt")
sag_lht <- sag_lht[,-c(2,3)]
colnames(sag_lht)[1]<-"id"
sag_lht$id <- as.factor(sag_lht$id)

#pedigree dataframe
sag_ped <- ped_saguenay[,c(1,2,3)] #full pedigree of SLSJ

char_ped <- ped_charlevoix[,c(1,2,3)] #full pedigree of charlevoix
#individuals with know year of birth
sag <- ped_saguenay[which(is.na(ped_saguenay$yob) == F),c(1:6,11,16)] #choose only ind with known birth year

char <- ped_charlevoix[which(is.na(ped_charlevoix$yob) == F),c(1:6,11,16)]#choose only ind with known birth year

#calculate offspring mortality

char_lht$offmort <- as.numeric(rep(0, nrow(char_lht)))
char_lht$infmort <- as.numeric(rep(0, nrow(char_lht)))

for (i in 1:nrow(char_lht)) {
  if (char_lht$id[i] %in% ped_charlevoix$sire) {
    kids <- as.vector(ped_charlevoix[which(ped_charlevoix$sire == as.vector(char_lht$id[i])),]$id)
    longevity <- char_lht[which(char_lht$id %in% kids),]$longevity
    char_lht$offmort[i] <- sum(longevity < 15, na.rm = T)
    char_lht$infmort[i] <- sum(longevity < 1, na.rm = T)
  }
  if (char_lht$id[i] %in% ped_charlevoix$dam) {
    kids <- as.vector(ped_charlevoix[which(ped_charlevoix$dam == as.vector(char_lht$id[i])),]$id)
    longevity <- char_lht[which(char_lht$id %in% kids),]$longevity
    char_lht$offmort[i] <- sum(longevity < 15, na.rm = T)
    char_lht$infmort[i] <- sum(longevity < 1, na.rm = T)
  }
}

sag_lht$offmort <- as.numeric(rep(0, nrow(sag_lht)))
sag_lht$infmort <- as.numeric(rep(0, nrow(sag_lht)))

for (i in 1:nrow(sag_lht)) {
  if (sag_lht$id[i] %in% ped_saguenay$sire) {
    kids <- as.vector(ped_saguenay[which(ped_saguenay$sire == as.vector(sag_lht$id[i])),]$id)
    longevity <- sag_lht[which(sag_lht$id %in% kids),]$longevity
    sag_lht$offmort[i] <- sum(longevity < 15, na.rm = T)
    sag_lht$infmort[i] <- sum(longevity < 1, na.rm = T)
  }
  if (sag_lht$id[i] %in% ped_saguenay$dam) {
    kids <- as.vector(ped_saguenay[which(ped_saguenay$dam == as.vector(sag_lht$id[i])),]$id)
    longevity <- sag_lht[which(sag_lht$id %in% kids),]$longevity
    sag_lht$offmort[i] <- sum(longevity < 15, na.rm = T)
    sag_lht$infmort[i] <- sum(longevity < 1, na.rm = T)
  }
}


#########fitness data#############
sag_fit <- read.table("datFIT2_saguenay.txt")
char_fit <- read.table("datFIT2.txt")
colnames(sag_fit)[1]<-"id"
colnames(char_fit)[1] <- "id"

#Saguenay
sag <- merge(sag, sag_lht, by = "id") #merge data with known yob and lht data
sag$LRS <- sag$n_child-sag$offmort
write.table(sag, file = "sag_df.txt")
#keep female ind
sag_f <- sag[which(sag$sex == 1),]
sag_f$sex <- NULL
#add fitness data to each ind
sag_f <- merge(sag_f,sag_fit[,c(1:4)],by = "id")

#Charelvoix
char_lht$offmort <- ifelse(is.na(char_lht$offmort)==T, 0,char_lht$offmort)
char_lht$infmort <- ifelse(is.na(char_lht$infmort)==T, 0,char_lht$infmort)
char <- merge(char, char_lht, by = "id") #merge data with known yob and lht data

char$LRS <- char$n_child-char$offmort
write.table(char, file = "char_df.txt")
#keep female ind
char_f <- char[which(char$sex == 1),]
char_f$sex <- NULL

#add fitness data to each ind
char_f <- merge(char_f,char_fit[,c(1:4)],by = "id")

#fix na issue by confirming women had no children and assign fitness of 0
char_f$fit <- ifelse(is.na(char_f$fit)==T, 0,char_f$fit)
sag_f$fit <- ifelse(is.na(sag_f$fit)==T, 0,sag_f$fit)

char_f$fitmu <- ifelse(is.na(char_f$fitmu)==T, 0,char_f$fitmu)
sag_f$fitmu <- ifelse(is.na(sag_f$fitmu)==T, 0,sag_f$fitmu)
#fix cohort cahrelvoix
char_f$cohorte <- ifelse(char_f$yob %in% 1680:1696 ,1, char_f$cohorte)

n<-1697
for (i in 2:13) {
  char_f$cohorte <- ifelse(char_f$yob %in% n:(n+19) ,i, char_f$cohorte)
  n <- n+20
}

char_f$cohorte <- ifelse(char_f$yob %in% 1937:1943 ,14, char_f$cohorte)
#fix cohort saguenay
sag_f$cohorte <- ifelse(sag_f$yob %in% 1842:1847 ,1, sag_f$cohorte)

n<-1848
for (i in 2:7) {
  sag_f$cohorte <- ifelse(sag_f$yob %in% n:(n+19) ,i, sag_f$cohorte)
  n <- n+20
}

sag_f$cohorte <- ifelse(sag_f$yob %in% 1968:1972 ,8, sag_f$cohorte)

#keep women with known reproductive event
char_f <- char_f[which(char_f$n_child != 0),]
sag_f <- sag_f[which(sag_f$n_child != 0),]

#keep women before migration in event for Charlevoix, i.e. before 1837
char_f <- char_f[which(char_f$cohorte %in% 3:10),]
#keep women who completed their family, i.e. before 1948
sag_f <- sag_f[which(sag_f$cohorte %in% 3:8),]

