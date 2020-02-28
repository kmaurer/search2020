### Working to Visualize Experiments on Adversarial Distance Searches for Overconfidence 

library(tidyverse)

setwd("C:\\Users\\maurerkt\\Google Drive\\AFRLSFFP\\Spring2019")


dat1 <- read.csv("resultsCatDog.csv") %>%
  mutate(dataset="Data:CatDog")
dat2 <- read.csv("resultsCelebA.csv") %>%
  mutate(dataset="Data:CelebA")
dat3 <- read.csv("resultsZappos.csv") %>%
  mutate(dataset="Data:Zappos")
dat <- rbind(dat1, dat2, dat3)

levels(dat$phi)
dat$phi <- factor(dat$phi, levels= c("adversarial", "logistic", "Bandits",
                                     "cluster_prior","lowConfidence", "random"),
                  labels=c("Adversarial","Facility Locations","Bandits",
                           "Coverage-Based","Most Uncertain","Random"))
head(dat)
summary(dat)

# Adversarial starts with very good SDR (smart way to initialize a search for overconfidence)
# but average SDR value drops (suggesting it is not learning how to discover new overconfident points)

# Whereas our method starts poorly (blind search to begin)
# but average SDR value increases (learns the attributes )
ggplot() + 
  geom_line(aes(x=b, y=SDR, color=phi, group=iteration), alpha=.1,
             data=dat)+
  stat_smooth(aes(x=b, y=SDR, color=phi), size=2, 
              method="lm", formula=y ~ poly(x, 3),
              se=FALSE, data=dat)+
  scale_y_continuous(limits=c(0,10)) +
  facet_grid(dataset~phi) +
  theme_bw() +
  labs(x="Query Steps (b)", 
       title="SDR Across Stages of Oracle Query Search for Overconfidence")
  


# Similar idea, but at discrete stages of search
stages <- dat %>%
  filter(b %in% seq(19,99,by=20)) %>%
  mutate(b_group = factor(b, label=seq(20,100,by=20)))
str(stages)

ggplot() + 
  geom_boxplot(aes(x=b_group, y=SDR, color=phi),
            data=stages) +
  # geom_hline(yintercept = 1) + 
  facet_grid(dataset~phi) +
  labs(x="Query Steps (b)")+
  theme_bw()+
  theme(legend.position = "none")
# ggsave(filename="AdversarialQueryResults.png",dpi=600,
#        width=10,height=5,units="in")


# same thing for spread stat
ggplot() + 
  geom_boxplot(aes(x=b_group, y=spread, color=phi),
               data=stages) +
  facet_grid(dataset~phi, scales="free_y") +
  labs(x="Query Steps (b)", 
       title="Spread Statistics Across Stages of Oracle Query Search for Overconfidence")+
  theme_bw()


