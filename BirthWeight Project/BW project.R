library("rigr")
library("ggplot2")
library("knitr")
library("ggpubr")

BW <- read.table("BirthsKingCounty2001-Biost514-517-2022.txt", header = T)
BW$sex <- ifelse(BW$sex == "M", 1, 0) # 1 represent male, otherwise female
BW$smoker <- ifelse(BW$smoker == "Y", 1, 0) # 1 represent smoke during pregnancy, otherwise don't
BW$drinker <- ifelse(BW$drinker == "Y", 1, 0) # 1 represent drink during pregnancy, otherwise don't

# Table 1
ds <- descrip(BW, strata = BW$firstep)
kable(ds[, 1:9], caption = "Descriptive Statistics Statified by firstep ")

# box plot for outliers
a <- ggplot(data = BW, aes(y = bwt)) + geom_boxplot()
b <- ggplot(data = BW, aes(y = age)) + geom_boxplot()
c <- ggplot(data = BW, aes(y = wpre)) + geom_boxplot()
d <- ggplot(data = BW, aes(y = wgain)) + geom_boxplot()
e <- ggplot(data = BW, aes(y = education)) + geom_boxplot()
f <- ggplot(data = BW, aes(y = gestation)) + geom_boxplot()
ggarrange(a, b, c, d, e, f, labels = c("birthWeight", "age", 
                                       "previousWeight", "weightGain", "education", "gestation"),
          ncol = 3, nrow = 2)

# histogram of birth weight
ggplot(data = BW, aes(bwt)) + geom_histogram(binwidth = 50) + 
  ggtitle("Graph 1: Histogram of Birth Weight")

# t-test for the difference between participants and non-participants
bw_diff <- mean(BW$bwt[BW$firstep == 1]) - mean(BW$bwt[BW$firstep == 0])
tt <- t.test(BW$bwt[BW$firstep == 1], BW$bwt[BW$firstep == 0], alternative = "less")
diff_tab <- matrix(c(bw_diff, tt$conf.int[1], tt$conf.int[2]), ncol = 3)
colnames(diff_tab) <- c("overall difference", "95% CI lower bound", "95% CI upper bound")

N <- c(sum(BW$firstep), 2500-sum(BW$firstep))
prop_single <- c(mean(BW$married[BW$firstep==1]), mean(BW$married[BW$firstep==0]))
prop_welfare <- c(mean(BW$welfare[BW$firstep==1]), mean(BW$welfare[BW$firstep==0]))
prop_smoker <- c(mean(BW$smoker[BW$firstep==1]), mean(BW$smoker[BW$firstep==0]))
prop_drinker <- c(mean(BW$drinker[BW$firstep==1]), mean(BW$drinker[BW$firstep==0]))
tab <- rbind(N, prop_single, prop_welfare, prop_smoker, prop_drinker)
colnames(tab) <- c("participants", "non-participants")

age_t <- t.test(BW$age[BW$firstep==1], BW$age[BW$firstep==0], 
                alternative = "greater") # participant in FS is not older than those who don't
edu_t <- t.test(BW$education[BW$firstep==1], BW$education[BW$firstep==0],
                alternative = "greater") # participants in FS are not more educated than those who don't
wgain_t <- t.test(BW$wgain[BW$firstep==1], BW$wgain[BW$firstep==0], 
                  alternative = "two.sided") # there is no difference between the mean wight gain between participant and       
# non-participant 
t_tab <- rbind(c(age_t$conf.int[1], age_t$conf.int[2], age_t$p.value),
               c(edu_t$conf.int[1], edu_t$conf.int[2], edu_t$p.value),
               c(wgain_t$conf.int[1], wgain_t$conf.int[2], wgain_t$p.value))
rownames(t_tab) <- c("age", "education", "weightGain")
colnames(t_tab) <- c("95% CI lower bound", "95% CI upper bound", "p-value")

race_tab <- table(BW$race, as.factor(BW$firstep))
race_tab <- cbind(race_tab[,1]/sum(BW$firstep==0), race_tab[,2]/sum(BW$firstep==1))
colnames(race_tab) <- c("non-participants", "participants")

# table 2 & 3
tab4 <- as.table(rbind(descrip(BW$bwt, strata = BW$drinker), 
                       descrip(BW$bwt, strata = BW$married), 
                       descrip(BW$bwt, strata = BW$smoker), 
                       descrip(BW$bwt, strata = BW$welfare))[, 1:9])
rownames(tab4) <- c("ALL", "non-drinker", "drinker", 
                    "ALL", "non-married", "married", 
                    "ALL", "non-smoker", "smoker", 
                    "ALL", "non-welfare", "welfare")
kable(tab4, caption = "Effect of Dichotomous Variables on Birth Weight")
drink <- t.test(BW$bwt[BW$drinker == 0], BW$bwt[BW$drinker == 1], alternative = "two.sided")
married <- t.test(BW$bwt[BW$married == 0], BW$bwt[BW$married == 1], alternative = "two.sided")
smoker <- t.test(BW$bwt[BW$smoker == 0], BW$bwt[BW$smoker == 1], alternative = "two.sided")
welfare <- t.test(BW$bwt[BW$welfare == 0], BW$bwt[BW$welfare == 1], alternative = "two.sided")
tt1 <- rbind(c(drink$p.value, drink$conf.int),
             c(married$p.value, married$conf.int), 
             c(smoker$p.value, smoker$conf.int),
             c(welfare$p.value, welfare$conf.int))
colnames(tt1) <- c("p-value", "95% CI lower bound", "95% CI upper bound")
rownames(tt1) <- c("drinker", "married", "smoker", "welfare")
kable(tt1, caption = "T-test for Potential Modifiable Dichotomous Variables")

# graph 2
g <- ggplot(data = BW, aes(as.character(firstep), bwt)) + geom_boxplot() + xlab("firstep") 
h <- ggplot(data = BW, aes(as.character(smoker), bwt)) + geom_boxplot() + xlab("smoker")
i <- ggplot(data = BW, aes(as.character(welfare), bwt)) + geom_boxplot() + xlab("welfare")
j <- ggplot(data = BW, aes(as.character(married), bwt)) + geom_boxplot() + xlab("married")
G <- ggarrange(g, h, i, j, ncol = 2, nrow = 2)
annotate_figure(G, top = text_grob("Graph 2", size = 14))

# t-test of participants and non-participants who smoke
sm <- t.test(BW$bwt[BW$smoker==1 & BW$firstep == 1], BW$bwt[BW$smoker==1 & BW$firstep == 0], 
             alternative = "two.sided")
nsm <- t.test(BW$bwt[BW$smoker==0 & BW$firstep == 1], BW$bwt[BW$smoker==0 & BW$firstep == 0], 
              alternative = "two.sided")

# make table 4
tabsm <- rbind(c(sm$p.value, sm$conf.int), c(nsm$p.value, nsm$conf.int))
colnames(tabsm) <- c("p-value", "95% CI lower bound", "95% CI upper bound")
rownames(tabsm) <- c("non_participants vs. participants(smoker)",
                     "non_participants vs. participants(non-smoker)")
kable(tabsm, caption = "Compare participants and non-participants(smoker)")

# table 5: distribution of races and effect of program in races
kable(race_tab, caption = "Distribution of Different Races")
ggplot(data = BW, aes(race, bwt, color = as.character(firstep))) + geom_boxplot() +
  labs(color = "firstep") + ggtitle("Graph 3: Effect of Firstep in Different Races")

# table 6
BW_fs <- subset(BW, BW$firstep==1)
fs_low <- sum(BW_fs$bwt < 2500)/nrow(BW_fs); fs_very_low <- sum(BW_fs$bwt < 1500)/nrow(BW_fs)
BW_nfs <- subset(BW, BW$firstep==0)
nfs_low <- sum(BW_nfs$bwt < 2500)/nrow(BW_nfs); nfs_very_low <- sum(BW_nfs$bwt < 1500)/nrow(BW_nfs)
bw_fs <- as.table(matrix(c(fs_low, fs_very_low, nfs_low, nfs_very_low), ncol=2))
rownames(bw_fs) <- c("low", "very low"); colnames(bw_fs) <- c("participant", "non-participants")
kable(bw_fs, caption = "Proportion of Low and Very Low Birth Weight")



