library("stringr")
library("dplyr")
library("ggplot2")
library("ggrepel")
library("rigr")
library("knitr")
library("ggpubr")

# Movie information
movie_all <- read.csv("IMDb_All_Genres_etf_clean1.csv", header = T)
## focus only on movies from 2017-2022
movie_partial <- subset(movie_all, movie_all$Year>2016)
## select the needed variables
movie_partial <- movie_partial %>% 
  select(Movie_Title, Year, Rating, main_genre, side_genre) %>% 
  arrange(Year) 

# Box Office of Movie
BO_2017 <- read.csv("2017.csv", header = T)
BO_2018 <- read.csv("2018.csv", header = T) 
BO_2019 <- read.csv("2019.csv", header = T)
BO_2020 <- read.csv("2020.csv", header = T)
BO_2021 <- read.csv("2021.csv", header = T)
BO_2022 <- read.csv("2022.csv", header = T)
BO <- rbind(BO_2017, BO_2018, BO_2019, BO_2020, BO_2021, BO_2022)
BO <- BO %>% rename(Movie_Title = "Release.Group", Dom_perc = "X.", Fore_perc = "X..1")

# Clean special characters
BO <- BO %>% mutate(Worldwide = as.numeric(gsub('[^[:alnum:] ]', "", Worldwide)), 
                    Domestic = as.numeric(gsub('[^[:alnum:] ]', "", Domestic)), 
                    Foreign = as.numeric(gsub('[^[:alnum:] ]', "", Foreign)),
                    Dom_perc = as.numeric(substr(Dom_perc, 1, nchar(Dom_perc)-1)),
                    Fore_perc = as.numeric(substr(Fore_perc, 1, nchar(Fore_perc)-1)))

#combine two data sets 
BO.new <- BO %>% right_join(movie_partial, by = "Movie_Title") 
BO.new %>%  group_by(Year) %>% summarise(n=n(), missing.D = sum(is.na(Domestic)),
                                         missing.F = sum(is.na(Foreign)), 
                                         missing.both = sum(is.na(Rank)))

BO.new  <- BO.new %>% filter(!is.na(Rank)) %>% 
  mutate(side_genre1 = substr(side_genre, 1, str_locate(pattern = ",", side_genre)-1),
         side_genre2 = substr(side_genre, str_locate(pattern = ",", side_genre)+1, nchar(side_genre)),
         covid = ifelse(Year > 2019, "after", "before")) %>% select(-side_genre)

# boxplot
world <- BO.new %>% 
  ggplot(aes(x = as.character(Year), Worldwide)) + geom_boxplot() 
domestic <- BO.new %>% 
  ggplot(aes(x = as.character(Year), Domestic)) + geom_boxplot() 
foreign <- BO.new %>% 
  ggplot(aes(x = as.character(Year), Foreign)) + geom_boxplot() 
overall_world <- BO.new %>% 
  ggplot(aes(covid, Worldwide)) + geom_boxplot()
overall_dom <- BO.new %>% 
  ggplot(aes(covid, Domestic)) + geom_boxplot()
overall_fore <- BO.new %>% 
  ggplot(aes(covid, Foreign)) + geom_boxplot()

plot <- ggarrange(world, overall_world, domestic, overall_dom, foreign, overall_fore,
                  labels = c("wolrd", "overall_world", "domestic", "overall_dom", 
                             "foreign", "overall_fore"),
                  ncol = 2, nrow = 3)
annotate_figure(plot, top = text_grob("Graph 1", 
                                      face = "bold", size = 12))

# t test 
tworld <- t.test(BO.new$Worldwide[BO.new$covid == "before"], 
                 BO.new$Worldwide[BO.new$covid == "after"], 
                 alternative = "greater")
tdom <- t.test(BO.new$Domestic[BO.new$covid == "before"], 
               BO.new$Domestic[BO.new$covid == "after"], 
               alternative = "greater")
tfore <- t.test(BO.new$Foreign[BO.new$covid == "before"], 
                BO.new$Foreign[BO.new$covid == "after"], 
                alternative = "greater")

t.covid.table <- rbind(c(tworld$p.value, tworld$conf.int), 
                       c(tdom$p.value, tdom$conf.int),
                       c(tfore$p.value, tfore$conf.int))
colnames(t.covid.table) <- c("p-value", "95% CI lower bound", "95% CI upper bound")
rownames(t.covid.table) <- c("Worldwide BO", "Domestic BO", "Foreign BO")
kable(t.covid.table, caption = "Effect of Covid on Box Office")

descrip(BO.new, strata = BO.new$covid)
dperc <- t.test(BO.new$Dom_perc[BO.new$covid == "before"], 
                BO.new$Dom_perc[BO.new$covid == "after"], alternative = "two.sided")
fperc <- t.test(BO.new$Fore_perc[BO.new$covid == "before"], 
                BO.new$Fore_perc[BO.new$covid == "after"], alternative = "two.sided")
perc_tab <- matrix(c(dperc$p.value, "not significant", 
                     fperc$p.value, "significant"), 2, 2, byrow = T)
rownames(perc_tab) <- c("p-value", "effect")
colnames(perc_tab) <- c("Domestic", "Foreign")
kable(perc_tab, caption = "Percnetage Difference in Domestic/Foreign Box Office")

kable(BO.new %>%  group_by(Year) %>% 
        summarise(n=n(), 
                  missing.D = sum(is.na(Domestic)),
                  missing.F = sum(is.na(Foreign))) %>% 
        mutate(missing.D.perc = missing.D/n), caption = "Number of Movie Didn't Release Domestically")

BO.high <- BO.new %>% filter(Rank < 6) 
t.test(BO.high$Worldwide[BO.high$covid == "before"], 
       BO.high$Worldwide[BO.high$covid == "after"],
       alternative = "two.sided")

BO.new %>% ggplot(aes(main_genre, Worldwide, color = covid)) + 
  geom_boxplot() + ggtitle("Graph 2")

BO.new %>% ggplot(aes(main_genre, Domestic, color = covid)) + 
  geom_boxplot() + ggtitle("Graph 3")

BO.new %>% ggplot(aes(main_genre, Foreign, color = covid)) + 
  geom_boxplot() + ggtitle("Graph 4")

a <- t.test(BO.new$Worldwide[BO.new$main_genre == "Biography" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Biography" & BO.new$covid == "after"],
            alternative = "two.sided")

b <- t.test(BO.new$Worldwide[BO.new$main_genre == "Crime" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Crime" & BO.new$covid == "after"],
            alternative = "two.sided")

c <- t.test(BO.new$Worldwide[BO.new$main_genre == "Drama" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Drama" & BO.new$covid == "after"],
            alternative = "two.sided")

d <- t.test(BO.new$Worldwide[BO.new$main_genre == "Comedy" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Comedy" & BO.new$covid == "after"],
            alternative = "two.sided")

e <- t.test(BO.new$Worldwide[BO.new$main_genre == "Adventure" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Adventure" & BO.new$covid == "after"],
            alternative = "two.sided")

f <- t.test(BO.new$Worldwide[BO.new$main_genre == "Action" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Action" & BO.new$covid == "after"],
            alternative = "two.sided")

g <- t.test(BO.new$Worldwide[BO.new$main_genre == "Animation" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Animation" & BO.new$covid == "after"],
            alternative = "two.sided")

h <- t.test(BO.new$Worldwide[BO.new$main_genre == "Horror" & BO.new$covid == "before"],
            BO.new$Worldwide[BO.new$main_genre == "Horror" & BO.new$covid == "after"],
            alternative = "two.sided")


p <- c(a$p.value, b$p.value, c$p.value, d$p.value, 
       e$p.value, f$p.value, g$p.value, h$p.value)
ci <- rbind(a$conf.int, b$conf.int, c$conf.int, d$conf.int,
            e$conf.int, f$conf.int, g$conf.int, h$conf.int)
genre_tab <- cbind(p, ci)
genre_tab <- cbind(genre_tab, 
                   ifelse(genre_tab[, 1] < 0.05, "significant", "not significant"))
rownames(genre_tab) <- c("Biography", "Crime", "Drama", "Comedy", "Adventure",
                         "Action", "Animation", "Horror")
colnames(genre_tab) <- c("p-value", "95% CI upper", "95% CI lower", "result")
kable(genre_tab, caption = "Effct of Covid on Movie by Genre")

BO.new %>% ggplot(aes(Rating, Worldwide)) + geom_point(aes(color = main_genre)) +
  geom_smooth(aes(color = main_genre), method = "lm", se = F) + 
  geom_smooth(color = "blue", method = "lm", se = F) +
  facet_grid(rows = vars(covid)) + ggtitle("Graph 5")

l <- c("Action", "Animation", "Comedy", "Drama")
rate_after <- lapply(l, function(l) summary(lm(Worldwide ~ Rating, 
                                               subset(BO.new, main_genre == l & covid == "after")))$coefficients[2, c(1,4)])
rate_before <- lapply(l, function(l) summary(lm(Worldwide ~ Rating, 
                                                subset(BO.new, main_genre == l & covid == "before")))$coefficients[2, c(1,4)])


rate_after <- matrix(unlist(rate_after), 4, 2, byrow = T)
rownames(rate_after) <- c("Action", "Animation", "Comedy", "Drama")
colnames(rate_after) <- c("effect", "p-value")
kable(rate_after, caption = "Effect of Rating on Movie by Genre after Covid")

rate_before <- matrix(unlist(rate_before), 4, 2, byrow = T)
rownames(rate_before) <- c("Action", "Animation", "Comedy", "Drama")
colnames(rate_before) <- c("effect", "p-value")
kable(rate_before, caption = "Effect of Rating on Movie by Genre before Covid")