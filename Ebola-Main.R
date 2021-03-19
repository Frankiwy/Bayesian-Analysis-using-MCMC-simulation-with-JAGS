library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(R2jags)
library(bayesplot)
library(TeachingDemos)
library(kableExtra)
library(gridExtra)

dir.create("images", showWarnings = TRUE)

ebola_congo <- (read_csv("dataset/ebola_congo.csv",
                    col_types = cols(`publication_date` = col_skip(), `source` = col_skip(),
                                     `report_date` = col_skip(), `country` = col_skip(),
                                     `confirmed_cases` = col_skip(), `probable_cases` = col_skip(), 
                                     `confirmed_deaths` = col_skip(), `new_deaths` = col_skip(),
                                     `total_suspected_cases` = col_skip(), `new_cured` = col_skip(),
                                     `new_suspected_cases` = col_skip(), `old_suspected_cases` = col_skip(),
                                     `confirmed_cases_change` = col_skip(), `probable_cases_change` = col_skip(),
                                     `total_cases_change` = col_skip(), `confirmed_deaths_change` = col_skip(),
                                     `total_deaths_change` = col_skip(), `total_suspected_cases_change` = col_skip()
                                     ))[-1,]) # -1 is used for skip the first line
ebola_congo$province <- as.character(ebola_congo$province)
ebola_congo$total_cases <- as.numeric(as.character(ebola_congo$total_cases))
ebola_congo$total_deaths <- as.numeric(as.character(ebola_congo$total_deaths))
ebola_congo$total_cured <- as.numeric(as.character(ebola_congo$total_cured))


fix.NA <- function(dataset,first,second=NA){
  candidate <- which(is.na(dataset[first])) # get vector of positions
  delete <- c()
  if (!is.na(second)) { # if second column is passed
    for (elm in candidate){ # iterate over all the found values
      sec <- dataset[second][elm,]
      baseline <- dataset$total_cases[elm]
      if (!is.na(sec)){ # if the second element is not NA
        new_v <- round(abs(dataset$total_cases[elm] - dataset[second][elm,]),0)
        if ((!is.na(new_v)) & (baseline >= new_v )) dataset[first][elm,] <- new_v # substitute the new value
        else delete <- c(delete, elm) # elm to be deleted 
      }
      
      else { # if it is... than compute the value using the median
        get_prov <- dataset$province[elm] # find the prov to use as groupby value
        new_v <- round(mean(c(
          dataset[first][which(dataset$province==get_prov),]
          )[[1]],na.rm=T),0) # get the median value 
        if ((!is.na(baseline)) & (dataset$total_cases[elm] >= new_v )) dataset[first][elm,] <- new_v # substitute the new value
        else delete <- c(delete, elm) # elm to be deleted
      }
    }
    print(paste('Warning: ',(length(candidate)-length(delete)),' values have been approximated & ',
                length(delete),' have been candidated...'))
    dataset <- dataset[-delete,] # candidate values

  }
  else {
    dataset <- dataset[-candidate,] # candidate values 
    print(paste('Warning: ',length(candidate),' values have been deleted...'))
  }
  return(dataset)
}

ebola_congo <- fix.NA(ebola_congo,'total_deaths','total_cured')

congo <- ebola_congo %>% group_by(health_zone) %>% summarize(province=first(province),
                                                             total_cases=sum(total_cases),
                                                             total_deaths=sum(total_deaths),
                                                             total_cured=total_cases- total_deaths) # groupy by health_zones
#View(congo)
congo <- congo[c(-5,-7,-8,-22,-25,-27),]#delete strange health_zones 
congo <- congo[congo$total_deaths<=10000,]
#View(congo)
length(unique(congo$health_zone))
length(congo[congo$health_zone=='South Kivu',])
# Deaths Plots

summary(congo$total_deaths)

# malnutrition indexes

malnutrition <- read_excel("dataset/malnutrition-decembre-2020.xlsx", 
                           col_types = c("text", "skip", "text", 
                                         "skip", "text", "text", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "skip", "skip", "numeric", "skip", 
                                         "text", "skip", "skip", "skip", "numeric", 
                                         "text", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", 
                                         "skip", "skip", "skip"))


malnutrition1 <- malnutrition[malnutrition$`Zone de Santé` %in% unique(congo$health_zone),]

unique(malnutrition1$`Zone de Santé`)

unique(congo$health_zone)
View(malnutrition1)






#########################################
deaths_summary <- congo %>% ggplot(aes(x=total_deaths)) +
  geom_histogram(binwidth=500, fill= 'orchid',colour = 'purple', 
                 alpha=.6,boundary = 0, closed = "left") +
  ylim(0,7)+
  xlim(0, 5000) +
  geom_vline(xintercept = 241, linetype='dotted', lwd=.8) + #1st quantile
  geom_vline(xintercept = 685, linetype='dashed', lwd=.8, col='red') + # median
  geom_vline(xintercept = 1711, linetype='dotted', lwd=.8) + #3rd quantile
  scale_x_discrete(name="Deaths",limits= seq(0,4500,500)) +
  labs(y=' ')

cases_summary <- congo %>% ggplot(aes(x=total_cases)) +
  geom_histogram(binwidth=1000, fill= 'cyan4',colour = 'darkgreen', 
                 alpha=.6,boundary = 0, closed = "left") +
  ylim(0,7)+
  geom_vline(xintercept = 657, linetype='dotted', lwd=.8) + #1st quantile
  geom_vline(xintercept = 1209, linetype='dashed', lwd=.8, col='red') + # median
  geom_vline(xintercept = 5562, linetype='dotted', lwd=.8) + #3rd quantile
  scale_x_discrete(name="Cases",limits= seq(0,9000,1000)) +
  labs(y=' ')

dat <- data.frame('Var'=c('total cases','total deaths','total cured'),
                  'Min.'=c(34,0,1), 'Q1.'=c(657, 241, 147), 'Median'=c(1209, 685, 631),
                  'Mean'=c(2629,1215,1414),'Q3.'=c(5562,1711,1482), 'Max.'=c(8889,4403,7146))
dat_summary <- dat %>% kbl(caption='Summary table:') %>%
  kable_material_dark()
dat_summary

t1 <- tableGrob(dat, theme=ttheme_minimal(), rows=NULL)

summary_figure <- grid.arrange(deaths_summary, cases_summary,t1,nrow = 1)

summary_figure

group.colors <- c('Ituri' = "royalblue3", 
                  'North Kivu' = "tomato2", 'South Kivu' ="gold1")

deaths_h <- congo %>% ggplot(aes(x=total_deaths)) +
  geom_histogram(binwidth=500, fill= 'orchid',colour = 'purple', 
                 alpha=.6,boundary = 0, closed = "left") +
  ylim(0,7)+
  xlim(0, 5000) +
  scale_x_discrete(name=" ",limits= seq(0,4500,500)) +
  labs(y=' ') +
  theme(axis.text.y = element_text(colour='red'),
        axis.line.y = element_line(size = 1, colour = "red"))


deaths_prov <- congo %>% ggplot(aes(x=total_deaths, fill=province)) +
  geom_histogram(binwidth=500,colour = 'purple', 
                 alpha=.6,boundary = 0, closed = "right") +
  scale_fill_manual(values=group.colors) +
  ylim(0,7)+
  scale_x_discrete(name=" ",limits= seq(0,4500,500)) +
  labs(title = 'Total deaths',y=' ') +
  theme(legend.position = c(.88, 0.65),
        axis.text.y = element_text(colour='red'),
        axis.line.y = element_line(size = 1, colour = "red"),
        plot.title = element_text(hjust = 0.5, size = 14, color = 'purple'))

deaths_d <- congo %>% ggplot(aes(x=total_deaths)) +
  geom_histogram(aes(y=..density..), binwidth=500,
                 fill= 'orchid',colour = 'purple', 
                 alpha=.6,boundary = 0, closed = "left") +
  xlim(0, 5000) +
  ylim(0,10e-04)+
  geom_density(fill='red',alpha=.2,col='violet') +
  scale_x_discrete(name=" ",limits= seq(0,4500,500)) +
  labs(y='Densities') +
  theme(plot.title = element_text(hjust = 0.5, size = 14, color = 'purple'),
        axis.title.y = element_text(size=12,colour = 'black',face='bold'),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.text.y = element_text(colour='black'))

# Cases Plots

cases_h <- congo %>% ggplot(aes(x=total_cases)) +
  geom_histogram(binwidth=1000, fill= 'cyan4',colour = 'darkgreen', 
                 alpha=.6,boundary = 0, closed = "left") +
  ylim(0,7)+
  scale_x_discrete(name=" ",limits= seq(0,9000,1000)) +
  labs(y=' ') +
  theme(axis.text.y = element_text(colour='red'),
        axis.line.y = element_line(size = 1, colour = "red"))


cases_prov <- congo %>% ggplot(aes(x=total_cases, fill=province)) +
  geom_histogram(binwidth=1000,colour = 'darkgreen', 
                 alpha=.6,boundary = 0, closed = "right") +
  scale_fill_manual(values=group.colors) +
  scale_x_discrete(name=" ",limits= seq(0,9000,1000)) +
  ylim(0,7)+
  labs(title = 'Total cases',y=' ') +
  theme(legend.position = c(.88, 0.65),
        axis.text.y = element_text(colour='red'),
        axis.line.y = element_line(size = 1, colour = "red"),
        plot.title = element_text(hjust = 0.5, size = 14, color = 'darkgreen'))

cases_d <- congo %>% ggplot(aes(x=total_cases)) +
  geom_histogram(aes(y=..density..), binwidth=1000,
                 fill= 'cyan4',colour = 'darkgreen', 
                 alpha=.6,boundary = 0, closed = "left") +
  xlim(0, 5000) +
  ylim(0,10e-04)+
  geom_density(fill='green',alpha=.2,col='seagreen1') +
  scale_x_discrete(name=" ",limits= seq(0,9000,1000)) +
  labs(y=' ') +
  theme(plot.title = element_text(hjust = 0.5, size = 14, color = 'darkgreen'),
        axis.line.y = element_line(size = 1, colour = "black"),
        axis.text.y = element_text(colour='black'))


figure <- ggarrange(deaths_prov, cases_prov,
                    deaths_h, cases_h,
                    deaths_d, cases_d,
                    ncol = 2, nrow = 3, align = 'hv')
annotaded_fi <- annotate_figure(figure,
                bottom = text_grob("Data source: \n https://data.humdata.org/dataset/ebola-cases-and-deaths-drc-north-kivu", color = "blue",
                                   hjust = 1.01, x = 1, face = "italic", size = 15),
                left = text_grob("Frequencies", color = "red", size=12,
                                 rot = 90, face='bold', hjust = -.4, vjust = 2.3),
                fig.lab = " ", fig.lab.face = "bold")

saving <- function(name,the_figure,w,h){
  png(filename=name, width = w, height = h) # open image
  plot(the_figure)
  dev.off() # close and save image
}

saving('images/combo.jpg',annotaded_fi,w=900,h=556)










####################
alpha.star <- .5+sum(congo$total_deaths)
beta.star <-.5+sum(congo$total_cases) - sum(congo$total_deaths)

p.hat.c <- alpha.star/(alpha.star+beta.star) # point estimate


hist(congo$total_deaths,probability = T)
lines(density(congo$total_deaths))
curve(dbinom(x,size=N,prob = p.hat.c),add=T,col='red')


# Equal tails
cred=.95
p.ET.c <- c(qbeta((1-cred)/2, alpha.star, beta.star), 
             qbeta(1-(1-cred)/2, alpha.star, beta.star))
# HPD
p.HPD.c <- hpd(qbeta, conf=cred, 
                shape1=alpha.star,
                shape2=beta.star)

par(mfrow=c(3,6))
for (i in p.hat.jags){
  ggplot() +
    xlim(.45,.475) +
    geom_function(fun = dbeta,
                  args = list(shape1=alpha.star,shape2=beta.star), 
                  col='orchid', lwd=1.2) +
    geom_vline(xintercept = p.hat.c, col=2, lwd=.8) + # Point estimation
    geom_vline(xintercept = i, col=1, lwd=.8) + # jags point estimation
    geom_vline(xintercept = p.ET.c, col=3, lwd=1, lty=2) + # Equal tails
    geom_vline(xintercept = p.HPD.c, col=4, lwd=1, lty=2) + # HPD
    labs(title = 'Posterior Density',
         x='x', y=expression(paste(pi,"(", p, "|", r, n,")"))) +
    theme(plot.title = element_text(hjust = 0, size = 14, color = 'orchid'),
          axis.title.x = element_text(size=10,face='bold'),
          axis.title.y = element_text(size=10,face='bold'))
}

ggplot() +
  xlim(.45,.475) +
  geom_function(fun = dbeta,
                args = list(shape1=alpha.star,shape2=beta.star), 
                col='orchid', lwd=1.2) +
  geom_vline(xintercept = p.hat.c, col=2, lwd=.8) + # Point estimation
  geom_vline(xintercept = p.ET.c, col=3, lwd=1, lty=2) + # Equal tails
  geom_vline(xintercept = p.HPD.c, col=4, lwd=1, lty=2) + # HPD
  labs(title = 'Posterior Density',
       x='x', y=expression(paste(pi,"(", p, "|", r, n,")"))) +
  theme(plot.title = element_text(hjust = 0, size = 14, color = 'orchid'),
        axis.title.x = element_text(size=10,face='bold'),
        axis.title.y = element_text(size=10,face='bold'))




############### JAGS ###################

n <- congo$total_cases # tries 
r <- congo$total_deaths # number of success (unfortunately...)
N <- nrow(congo)
congo.jags <- list("r", "n", "N")

# Model
model <- function() {
  for(i in 1:N){
    p[i] ~ dbeta(1.0, 1.0) # Prior
    r[i] ~ dbinom(p[i], n[i]) # Model
  }
}

# Starting values
mod.inits = function(){
  list("p" = rep(0.1,N))
}
# Define parameters of interest
mod.params <- c("p")

# Run JAGS
set.seed(123)
mod.fit <- jags(data = congo.jags,                            
                model.file = model, inits = mod.inits,          
                parameters.to.save = mod.params,                  
                n.chains = 3, n.iter = 10000, n.burnin = 1000, n.thin=5)
mod.fit

# to do better diagnostic we can export the chain array:
chainArray <- mod.fit$BUGSoutput$sims.array

bayesplot::mcmc_combo(chainArray)
bayesplot::mcmc_acf(chainArray)

coda.fit <- as.mcmc(mod.fit)
coda::acfplot(coda.fit)
coda::geweke.plot(coda.fit)
coda::geweke.diag(coda.fit)
coda::gelman.plot(coda.fit)



chainMat <- mod.fit$BUGSoutput$sims.matrix
#point estimate
p.hat.jags <- colMeans(chainMat)
p.hat.jags

#intervals
cred <- 0.95
p.ET.jags <- apply(chainMat, 2, quantile, prob=c((1-cred)/2, 1-(1-cred)/2))

#HPD 
p.HPD.jags <- coda::HPDinterval(as.mcmc(chainMat))


p.hat.c
p.hat.jags

p.ET.c
p.ET.jags
 
p.HPD.c
p.HPD.jags

##### Assuming dependencies among areas


congo.jags2 <- list("r", "n", "N")
model2 <- function() {
  #likelihood
  for(i in 1:N){
    r[i] ~ dbinom(p[i], n[i]) #Model
    b[i] ~ dnorm(mu,tau) # pooling
    logit(p[i]) <- b[i] #link
  }
  mu ~ dnorm(0.0, 1e-6) # vague mean Prior --> abbiamo e-06 perché in jags il secondo valore della normale è la PRECISION che è l'inverso della var. lower the precision higher the sd
  tau ~ dgamma(0.001, 0.001) #vague tau(precision) prior 
  
  sigma <- 1 / sqrt(tau) # we return the sd that is the inverse sqared of the precision (tau)
  pop.mean <- exp(mu) / (1 + exp(mu))
}

# Starting values
mod.inits2 = function(){
  list(b = rep(0.1,N),
       tau = 1,
       mu = 0)
}

# Run JAGS
set.seed(123)
mod.fit2 <- jags(data = congo.jags2,                            
                 model.file = model2, inits = mod.inits2,          
                 parameters.to.save = c("p","sigma","mu","pop.mean"),                  
                 n.chains = 3, n.iter = 10000, n.burnin = 1000, n.thin=5)
mod.fit2


########## FREQUENTIST APPROACH ##########

congo$prob =  congo$total_deaths / congo$total_cases
congo

PROB_Matrix <- as.matrix(congo$prob)
n = nrow(congo)
B <- 10^4

mean.output <- rep(NA,B) # empty vector
sd.output <- rep(NA,B)

for (b in 1:B){
  new_sample <- PROB_Matrix[sample(n,n,replace=TRUE),]
  mean.output[b] <- mean(new_sample)
  sd.output[b] <- sqrt(var(new_sample))
}

paste("Mean Lower Bound:", round(quantile(mean.output, c(0.025)),3),
      "Mean Upper Bound:",round(quantile(mean.output, c(0.975)),3))

paste("SD Lower Bound:", round(quantile(sd.output, c(0.025)),3),
      "SD Upper Bound:", round(quantile(sd.output, c(0.975)),3))





congo
