library(readr)
library(dplyr)
library(R2jags)
#library(ggmcmc)
#library(mcmcse)
library(bayesplot)

master <- read_csv("dataset/master.csv",
                   col_types = cols(`HDI for year` = col_skip(),
                                    `gdp_for_year ($)` = col_skip(),
                                    sex = col_skip(),
                                    `suicides/100k pop` = col_skip(),
                                    `age` = col_skip(),
                                    `country-year` = col_skip(),
                                    `generation` = col_skip()
                                    ))

#View(master)
colnames(master) <-c("Country", "Year", "Suicides","Population","GDP_per_capita")
df <- master[master$Year == '2015',] %>%
  group_by(Country) %>% summarise(Year= max(Year),
                                  Population=round(sum(Population),0),
                                  Suicides = sum(Suicides),
                                  Average_GDP_per_capita = mean(GDP_per_capita))


hist(df$Population)
hist(df$Suicides)
unique(df$Country)
nrow(df)
df <- df[df$Suicides>=5000,]
df <- df[df$Country=='Italy',]
nrow(df)
df

par(mfrow=c(1,2))
hist(df$Population, 
     main = '', xlab='', ylab = '', yaxt="none", xaxt="none",
     col='orchid', border="purple", lwd=10)
mtext('Population distribution in 2015', side=3, cex=1.5, font=2, line=2, col='purple')
mtext("Population", side=1, line=3.5, font = 2, col='purple') # x-label
mtext("Frequency", side=2, line=3, font = 2, col='purple') # y-lable
axis(1, seq(0,3.5e+08,0.5e+08),las=2, font=1, cex.axis=0.8)
axis(2, seq(0,60,10),las=1)


hist(df$Suicides, 
     main = '', xlab='', ylab = '', yaxt="none", xaxt="none",
     col='#336666', border="#003333")
mtext('Suicides distribution in 2015', side=3, cex=1.5, font=2, line=2, col='#003300')
mtext("Suicides", side=1, line=3.5, font = 2, col='#003300') # x-label
mtext("Frequency", side=2, line=3, font = 2, col='#003300') # y-lable
axis(1, seq(0,46000,5000),las=2, font=1, cex.axis=0.8)
axis(2, seq(0,60,10),las=1)

par(mfrow=c(1,1))

hist(df$Suicides, density=20, breaks=20, prob=TRUE, 
     xlab="x-variable", xlim=c(0, 50000),ylim=c(0, 0.0004))
lines(density(df$Suicides), col='#FF3399')
curve(dpois(x,1)/10^5, 
      col="blue", lwd=2, add=TRUE, yaxt="n")

########################################

n <- df$Population
r <- df$Suicides
N <- nrow(df)
suicides.jags <- list("r", "n", "N")

model <- function() {
  for(i in 1:N){
    p[i] ~ dbeta(.5, .5) # Prior
    r[i] ~ dbinom(p[i], n[i]) # Model
  }
}

mod.params <- c('p')

# Starting values
mod.inits = function(){
  list("p" = rep(0.1, N))
}


# Run JAGS
set.seed(123)
mod.fit <- jags(data = suicides.jags,                            
                model.file = model, inits = mod.inits,          
                parameters.to.save = mod.params,                  
                n.chains = 3, n.iter = 10000, n.burnin = 1000, n.thin=5)
mod.fit

chainArray <- mod.fit$BUGSoutput$sims.array
mcmc_trace(chainArray)

# Diagnostic with coda

coda.fit <- coda::as.mcmc(mod.fit)
coda::acfplot(coda.fit)

coda::gelman.plot(coda.fit)

# point estimate
chainMat <- mod.fit$BUGSoutput$sims.matrix
colMeans(chainMat)





