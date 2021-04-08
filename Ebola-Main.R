

# (0) Libraries and Functions ---------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(R2jags)
library(bayesplot)
library(TeachingDemos)
library(kableExtra)
library(gridExtra)
library(reshape2)
library(viridis)
library(coda)
library(corrplot)
library(animation)


dir.create("images", showWarnings = TRUE) # directory to images
dir.create("images/model1", showWarnings = TRUE) # directory to images
dir.create("images/model2", showWarnings = TRUE) # directory to images

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


fix.NA.2 <- function(dataset,col.name,method='median'){
  col.idx <- grep(col.name, colnames(dataset)) # get index column to use to work only on it
  elm.idx <- which(is.na(dataset[col.name])) # get element indexes where there are NAs
  for (idx in elm.idx){
    province.name <- dataset$province[idx] # get province name to use for group-by
    if (method == 'median'){
      new.value <- median(unlist(dataset[dataset$province==province.name,col.idx], use.names=FALSE),na.rm = T) #get the mean
      dataset[idx,][col.name] = new.value # update dataset with new variable
      print(paste('Warning: ',length(elm.idx),' NAs have been substituted using median...'))
    }
    else{
      new.value <- mean(unlist(dataset[dataset$province==province.name,col.idx], use.names=FALSE),na.rm = T) #get the mean
      dataset[idx,][col.name] = new.value # update dataset with new variable
      print(paste('Warning: ',length(elm.idx),' NAs have been substituted using mean...'))
      
    }
  }
  return (dataset)
}


saving <- function(name,the.figure,w,h){
  png(filename=name, width = w, height = h) # open image
  plot(the.figure)
  dev.off() # close and save image
}

# (1) IMPORT DATA -------------------------------------------------------------


## (1) IMPORT Ebola DATASET
ebola_congo <- (read_csv("dataset/ebola_congo.csv",
                         col_types = cols(`publication_date` = col_skip(), `source` = col_skip(),
                                          `report_date` = col_skip(), `country` = col_skip(),
                                          `confirmed_cases` = col_skip(),
                                          `probable_cases` = col_skip(), 
                                          `confirmed_deaths` = col_skip(),
                                          `new_deaths` = col_skip(),
                                          `total_suspected_cases` = col_skip(), 
                                          `new_cured` = col_skip(),
                                          `new_suspected_cases` = col_skip(),
                                          `old_suspected_cases` = col_skip(),
                                          `confirmed_cases_change` = col_skip(),
                                          `probable_cases_change` = col_skip(),
                                          `total_cases_change` = col_skip(),
                                          `confirmed_deaths_change` = col_skip(),
                                          `total_deaths_change` = col_skip(),
                                          `total_suspected_cases_change` = col_skip(),
                                          `province` = col_character(), 
                                          `total_cases` = col_number(), 
                                          `total_deaths` = col_number(),
                                          `total_cured` = col_number() 
                         ))[-1,]) # -1 is used for skip the first line

ebola_congo <- fix.NA(ebola_congo,'total_deaths','total_cured')

### (1.1) grouping by health_zones
congo <- ebola_congo %>% group_by(health_zone) %>% 
  summarize(province=first(province), total_cases=sum(total_cases),
            total_deaths=sum(total_deaths), total_cured=total_cases-total_deaths) 

congo <- congo[c(-5,-7,-8,-22,-25,-27),] #delete strange health_zones 
congo <- congo[congo$total_deaths<=5000,] 

## (2) IMPORT Malnutrition DATASET
malnutrition <- read_excel("dataset/malnutrition.xlsx", 
                           col_types = c("text", "skip", "skip", 
                                         "skip", "text", "text", "numeric", 
                                         "skip", "numeric", "numeric", "numeric", 
                                         "skip", "skip", "numeric", "skip", 
                                         "skip", "skip", "skip", "skip", "numeric", 
                                         "skip", "skip", "skip", "skip", 
                                         "skip", "skip", "skip", 
                                         "skip", "skip", "skip"))

colnames(malnutrition) <- c("province", "health_zone", "postecode",
                            'population_estimate','MAS','MAM','GAM','stunted_growth',
                            'malnutrition_among_FeFAs')

malnutrition$health_zone[malnutrition$health_zone %in% c("Manguredjipa","Nyakunde") ] <- c("Mangurujipa","Nyankunde")

### (2.1) Doing intersection with the health_zones present in congo dataset too
cm_intersection <- intersect(unique(congo$health_zone), unique(malnutrition$health_zone))
malnutrition <- malnutrition[malnutrition$health_zone %in% cm_intersection,][-1]

congo <- left_join(congo, malnutrition, by = "health_zone")

### find NAs and infer them using group median:

col.numeric<- unlist(lapply(congo, is.numeric)) # get only numeric cols
# Getting the columns of A that have at least 1 NA is equivalent to get the rows that have at least NA for t(A).
col.names <- colnames(congo[col.numeric])[!complete.cases(t(congo[col.numeric]))] # complete.cases by definition (very efficient since it is just a call to C function) gives the rows without any missing value.
for (name in col.names) congo <- fix.NA.2(congo, name) # fix NAs


dat <- data.frame('Var'=c('total cases','total deaths','total cured', 'MAS',
                          'MAM', 'GAM', 'stunted_growth',
                          'malnutrition_among_FeFAs','population_estimate'),
                  'Min.'=c(34,0,1, 0.30, 2.40, 2.80, 47.10, 0.20,48003),
                  'Q1.'=c(657, 241, 147, 1.90, 2.70, 4.60, 49.60, 0.20,  126776), 
                  'Median'=c(1209, 685, 631, 1.90, 2.70, 4.60, 49.60, 0.20, 161232),
                  'Mean'=c(2629,1215,1414, 2.59, 4.00, 6.64, 53.51, 0.69,200370),
                  'Q3.'=c(5562,1711,1482, 3.40, 5.10, 10.20, 55.20, 1.30,264633),
                  'Max.'=c(8889,4403,7146, 6.10, 10.90, 14.30, 72.40, 1.30,462362))
dat_summary <- dat %>% kbl(
  caption='Table 1: Categorical Variables Summary Table:') %>%
  kable_paper(bootstrap_options = "striped", full_width = F,
              html_font = "Cambria") %>%
  row_spec(0, background = alpha("orchid",.2), bold=T, color = 'black')
dat_summary

# (2) SUMMARY PLOTS -----------------------------------------------------------

# (0) Set Colors
group.colors <- c('Ituri' = "royalblue3", 
                  'North Kivu' = "tomato2", 'South Kivu' ="gold1") 

# (1) DEATHS' HISTOGRAMS

## (1.1) Deaths Histogram based on "provinces"
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

## (1.2) Purple Histogram
deaths_h <- congo %>% ggplot(aes(x=total_deaths)) +
  geom_histogram(binwidth=500, fill= 'orchid',colour = 'purple', 
                 alpha=.6,boundary = 0, closed = "left") +
  ylim(0,7)+
  xlim(0, 5000) +
  scale_x_discrete(name=" ",limits= seq(0,4500,500)) +
  labs(y=' ') +
  theme(axis.text.y = element_text(colour='red'),
        axis.line.y = element_line(size = 1, colour = "red"))

## (1.3) Purple Histogram + Density
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

# (2) CASES' HISTOGRAMS

## (2.1) Cases Histogram based on "provinces"
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

## (2.2) Green Histogram
cases_h <- congo %>% ggplot(aes(x=total_cases)) +
  geom_histogram(binwidth=1000, fill= 'cyan4',colour = 'darkgreen', 
                 alpha=.6,boundary = 0, closed = "left") +
  ylim(0,7)+
  scale_x_discrete(name=" ",limits= seq(0,9000,1000)) +
  labs(y=' ') +
  theme(axis.text.y = element_text(colour='red'),
        axis.line.y = element_line(size = 1, colour = "red"))

## (2.3) Green Histogram + Density
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
                                bottom = text_grob("Data source: \n https://data.humdata.org/dataset/ebola-cases-and-deaths-drc-north-kivu",
                                                   color = "blue", hjust = 1.01, x = 1,
                                                   face = "italic", size = 10),
                                left = text_grob("Frequencies", color = "red", size=12,
                                                 rot = 90, face='bold', hjust = -.4,
                                                 vjust = 2.3),
                                fig.lab = " ", fig.lab.face = "bold")

annotaded_fi
saving('images/death_cases_histogram.jpg',annotaded_fi,w=900,h=556)


# (3) MODEL 1 (ASSUME INDEPENDECE) --------------------------------------------

n <- congo$total_cases # tries 
r <- congo$total_deaths # number of success (unfortunately...)
N <- nrow(congo)
congo.jags <- list("r", "n", "N")

model <- function() {
  for(i in 1:N){
    r[i] ~ dbinom(p, n[i]) #  Likelihood
  }
  p ~ dbeta(.5, .5) # Prior
}

# Starting values
mod.inits = function(){
  list("p" = rbeta(1, 1/2, 1/2))
}

# Run JAGS
set.seed(1618216)
mod.fit <- jags(data = congo.jags,                            
                model.file = model, inits = mod.inits,          
                parameters.to.save = c("p"),                  
                n.chains = 3, n.iter = 1e4, n.burnin = 1000, n.thin=5)

mod.fit

# (3.1) Rely on Conjugacy --------------------------------------------

# Jeffrey's prior
alpha <- 1/2
beta <- 1/2

# Conjugate inference

alpha.star <- alpha+sum(r)
beta.star <- alpha+sum(n)-sum(r)

# Point estimation
p.hat.c <- alpha.star/(alpha.star+beta.star)
cat(paste("`",'p: ',round(p.hat.c,3),"`"))


# (3.2) Diagnostic for Model 1  --------------------------------------------

chainArray <- mod.fit$BUGSoutput$sims.array # get the chain

# this is variable is used to change the aspect of some plots.
myfacets <-
  facet_bg(fill = "gray30", color = NA, ) +
  facet_text(face = "bold", color = "skyblue", size = 10) 

# (1) mcmc_trace
color_scheme_set("mix-brightblue-gray")
## (1.1) plot pi and deviance
mcmc_trace(chainArray, facet_args = list(labeller = ggplot2::label_parsed)) +
  myfacets

saving('images/model1/p_and_deviance.png',mcmc_trace(chainArray, facet_args = list(labeller = ggplot2::label_parsed)) +
         myfacets,w=1e3,h=423)

mcmc_trace(chainArray, pars = "p", window = c(300,500),
           facet_args = list(labeller = ggplot2::label_parsed))+
  myfacets
saving('images/model1/p_window.png',
       mcmc_trace(chainArray, pars = "p", window = c(300,500),
                  facet_args = list(labeller = ggplot2::label_parsed))+
         myfacets,w=1e3,h=423)

# (2) Gelamn & Rubin

coda.fit <- as.mcmc(mod.fit)
gelman.diag(coda.fit)

gelman.plot(coda.fit)

# (3) Autocorrelation

autocorr.diag(coda.fit)

# mcmc_acf
color_scheme_set("mix-teal-pink") # set color
mcmc_acf(chainArray, facet_args = list(labeller = ggplot2::label_parsed))+
  myfacets


saving('images/model1/autocorrelation_p_and_deviance.png',mcmc_acf(chainArray, facet_args = list(labeller = ggplot2::label_parsed))+
         myfacets,w=1e3,h=423)


# (4) Density
color_scheme_set("pink")

myfacets <-
  facet_bg(fill = "gray50", color = NA, ) +
  facet_text(face = "bold", color = 'white', size = 10)

mcmc_dens_overlay(chainArray, facet_args = list(labeller = ggplot2::label_parsed)) +
  myfacets

saving('images/model1/densityOverlay_p_and_deviance.png',mcmc_dens_overlay(chainArray, facet_args = list(labeller = ggplot2::label_parsed)) +
         myfacets,w=1e3,h=423)

# (5) Areas plot
color_scheme_set("green")
plot_title <- ggtitle("Posterior distributions",
                      "with Median & 90% Intervals")
chain_p <- mcmc_areas(chainArray, pars="p", prob = 0.9, point_est = 'median') + plot_title

chain_dev <- mcmc_areas(chainArray,
                        pars=c("deviance"),
                        prob = 0.9, point_est = 'median') + plot_title

ggarrange(chain_p, chain_dev,
          ncol = 2, nrow = 1)

saving('images/model1/densityANDintervals_p_and_deviance.png',ggarrange(chain_p, chain_dev,
                                                                        ncol = 2, nrow = 1)
       ,w=1e3,h=423)



# (3.3) Credibility Intervals Model 1  --------------------------------------------  

chainMatrix <- mod.fit$BUGSoutput$sims.matrix # join the deviance and pi for all the 3 chains

p.hat.jags <- colMeans(chainMatrix)
p.hat.jags[2]

cred <- 0.95
p.ET.jags <- apply(ChainMatrix, 2, quantile, prob=c((1-cred)/2, 1-(1-cred)/2))

p.HPD.jags <- HPDinterval(as.mcmc(ChainMatrix))

res.dat <- round(data.frame('ET2.5'=p.ET.jags[1,],
                            'ET97.5'=p.ET.jags[2,],
                            'HPD2.5'=p.HPD.jags[,1],
                            'HPD97.5'=p.HPD.jags[,2]),6)
row.names(res.dat)[2] <- "$p$"

my.colors <- c('white',rep('royalblue',2),rep('yellow',2)) # set colors

kbl(res.dat,col.names = NULL,
    caption="     95% Intervals:") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" ", "Lower" = 1, "Upper" = 1, "Lower" = 1, "Upper" = 1),
                   bold = T, background = alpha(my.colors, 0.05), color = 'red') %>%
  add_header_above(c(" ", "Equal Tail" = 2, "HPD" = 2),
                   background = alpha(my.colors[c(1:2,4)],0.05), color = 'black') %>%
  column_spec(c(2,3), background = alpha(my.colors[2], 0.05)) %>%
  column_spec(c(4,5), background = alpha(my.colors[4], 0.05)) 


# (4) Model 2  --------------------------------------------  


# (4.1) Code HEATMAP --------------------------------------------

Cor = cor(congo[unlist(lapply(congo, is.numeric))])
#change col and row names just for readability 
v.names <- c('tot.cases','tot.deaths','tot.cured','pop','MAS','MAM','GAM','STG','FeFA')
colnames(Cor) <- v.names
rownames(Cor) <- v.names
corrplot(Cor, type="upper", method="ellipse", tl.pos="d")
corrplot(Cor, type="lower", method="number", col="black", 
         add=TRUE, diag=FALSE, tl.pos="n", cl.pos="n")

# (4.2) JAGS Model 2 --------------------------------------------

n <- congo$total_cases # tries 
r <- congo$total_deaths # number of success (unfortunately...)
GAM <- congo$GAM
STG <- congo$stunted_growth
FeFA <- congo$malnutrition_among_FeFAs
N <- nrow(congo)

congo.jags2 <- list("r", "n", "N","GAM","STG","FeFA")
model2 <- function() {
  
  # (1) Likelihood
  for(i in 1:N){
    r[i] ~ dbinom(p[i], n[i]) #Model
    logit(p[i]) <- beta[1] + beta[2]*GAM[i] + beta[3]*STG[i] + beta[4]*FeFA[i] #link
  }
  
  # (2) Priors
  # betas start from 1 and not from 0 to be in concordance with JAGS notation
  for (j in 1:4){ beta[j]~dnorm( mu, tau) }  # pooling
  
  mu ~ dnorm(0.0, 1e-6) # vague mean Prior -->  there is e-06 since in JAGS it is necessary to pass the PRECISION (which is the inverse of the sd). Lower the Precision higher the SD
  tau ~ dgamma(1e-3, 1e-3) # vague tau (Precision)
  
  sigma <- 1 / sqrt(tau) # we return the sd that is the inverse squared of the Precision (tau)
  #pop.mean <- exp(mu) / (1 + exp(mu))
}

# Starting values
mod.inits2 = function(){
  list(tau = 1e3,
       mu = 0)
}

# Run JAGS
set.seed(1618216)
mod.fit2 <- jags(data = congo.jags2,                            
                 model.file = model2, inits = mod.inits2,          
                 parameters.to.save = c("p","sigma","mu","tau", "beta"),         
                 n.chains = 3, n.iter = 1e4, n.burnin = 1000, n.thin=5)

mod.fit2


# (4.3) Get p_{i} estimates from GLM --------------------------------------------


ChainMatrix2 <- mod.fit2$BUGSoutput$sims.matrix # extraxt chains
pm_coeff <- colMeans(chainMatrix2) #posterior mean of the coefficients betas

exp_comp <- function(x1,x2,x3) 1/(1+exp(-(-5.56819741+0.12126469*x1+0.08479912*x2+0.29248419*x3)))
est.probs <- rep(1,N)
for (n in 1:nrow(congo)) est.probs[n] <- exp_comp(congo$GAM[n],congo$stunted_growth[n],congo$malnutrition_among_FeFAs[n])

X = round(cbind('est.probs'=est.probs, 'JAGS.probs' = pm_coeff[7:23]),4)
X <- data.frame(X)
X <- data.frame(t(X))
colnames(X) <- paste("$p_{",seq(1:17),"}$",sep='')

X %>% kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = F) %>%
  row_spec(0, background = alpha("orchid",0.2), color = 'black') %>%
  column_spec(1, bold=TRUE) %>%
  scroll_box(width = "830px") 

# (4.4) Diagnostic Model 2 --------------------------------------------

chainArray2 <- mod.fit2$BUGSoutput$sims.array # get the chain

# this is variable is used to change the aspect of some plots.
myfacets <-
  facet_bg(fill = "gray30", color = NA, ) +
  facet_text(face = "bold", color = "skyblue", size = 10)

# (1) mcmc_trace
color_scheme_set("mix-brightblue-gray")
## (1.1) plot 4 betas
mcmc_trace(chainArray2,par=c("beta[1]","beta[2]","beta[3]","beta[4]"),
           facet_args = list(labeller = ggplot2::label_parsed)) +
  myfacets

saving('images/model2/traceplots_betas.png',
       mcmc_trace(chainArray2,par=c("beta[1]","beta[2]","beta[3]","beta[4]"),
                                              facet_args = list(labeller = ggplot2::label_parsed)) +
         myfacets, w=1e3,h=423)


mcmc_trace(chainArray2, pars = "beta[2]", window = c(300,500),
           facet_args = list(labeller = ggplot2::label_parsed))+
  myfacets

# (2) Gelamn & Rubin 

coda.fit2 <- as.mcmc(mod.fit2)
gelman.diag(coda.fit2[,c(1:4)])

gelman.plot(coda.fit2[,c(1:4)])


# (3) Autocorrelation

aut.dat <- round(data.frame(autocorr.diag(coda.fit2)),4)
colnames(aut.dat)[c(1:4,24)] <- c("$\\beta_{1}$","$\\beta_{2}$","$\\beta_{3}$","$\\beta_{4}$","$\\sigma$")
colnames(aut.dat)[c(7:23)] <- paste("$p_{",seq(1:17),"}$",sep='')

aut.dat[,c(1:5,7:23,25,24,6)] %>% kbl(caption='Autoccorelation Diagnostic:') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = F) %>%
  row_spec(0, background = alpha("orchid",0.2), color = 'black') %>%
  column_spec(1, bold=T,width = "80px") %>%
  scroll_box(width = "830px")

color_scheme_set("mix-teal-pink") # set color
mcmc_acf(chainArray2,par=c("beta[1]","beta[2]","beta[3]","beta[4]"),
         facet_args = list(labeller = ggplot2::label_parsed))+
  myfacets
saving('images/model2/autocorrelation_betas.png',
       mcmc_acf(chainArray2,par=c("beta[1]","beta[2]","beta[3]","beta[4]"),
                facet_args = list(labeller = ggplot2::label_parsed))+
         myfacets, w=1e3,h=423)

# (4) Density
color_scheme_set("pink")
## (4.1) plot 4 betas
myfacets <-
  facet_bg(fill = "gray50", color = NA, ) +
  facet_text(face = "bold", color = 'white', size = 10)

mcmc_dens_overlay(chainArray2,par=c("beta[1]","beta[2]","beta[3]","beta[4]"),
                  facet_args = list(labeller = ggplot2::label_parsed)) +
  myfacets

saving('images/model2/density_betas.png',mcmc_dens_overlay(chainArray2,par=c("beta[1]","beta[2]","beta[3]","beta[4]"),
                                                                   facet_args = list(labeller = ggplot2::label_parsed)) +
         myfacets, w=1e3,h=423)



## (4.2) plot deviance and sigma
mcmc_dens_overlay(chainArray2,par=c("deviance","sigma"),
                  facet_args = list(labeller = ggplot2::label_parsed)) +
  myfacets


saving('images/model2/densityOverlay_betas.png',mcmc_dens_overlay(chainArray2,par=c("deviance","sigma"),
                                                           facet_args = list(labeller = ggplot2::label_parsed)) +
         myfacets
       , w=1e3,h=423)

# (5) Areas plot

color_scheme_set("green")
b1 <- mcmc_areas(chainArray2,
                 pars=c("beta[1]"),
                 prob = 0.9, point_est = 'median')
b2 <- mcmc_areas(chainArray2,
                 pars=c("beta[2]"),
                 prob = 0.9, point_est = 'median')
b3 <- mcmc_areas(chainArray2,
                 pars=c("beta[3]"),
                 prob = 0.9, point_est = 'median')
b4 <- mcmc_areas(chainArray2,
                 pars=c("beta[4]"),
                 prob = 0.9, point_est = 'median') 
dev <- mcmc_areas(chainArray2,
                  pars=c("deviance"),
                  prob = 0.9, point_est = 'median') 
sig <- mcmc_areas(chainArray2,
                  pars=c("sigma"),
                  prob = 0.9, point_est = 'median') 
fig <- ggarrange(b1, b2,
                 b3, b4,
                 dev, sig,
                 ncol = 3, nrow = 2)
annotate_figure(fig, top = text_grob(
  "Posterior distributions with Medians & 90% Intervals", 
  color = "black",hjust = 1.01, x = 1,
  face = "italic", size = 18))
saving('images/model2/densityANDIntervals_betas_deviance_sigma.png',annotate_figure(fig, top = text_grob(
  "Posterior distributions with Medians & 90% Intervals", 
  color = "black",hjust = 1.01, x = 1,
  face = "italic", size = 18))
       , w=1e3,h=423)
# (4.5) Credibility Intervals Model 2 --------------------------------------------

chainMatrix2 <- mod.fit2$BUGSoutput$sims.matrix # join the deviance and pi for all the 3 chains

p.hat.jags2 <- colMeans(chainMatrix2)
p.hat.jags2

cred <- 0.95
p.ET.jags2 <- apply(chainMatrix2, 2, quantile, prob=c((1-cred)/2, 1-(1-cred)/2))
p.HPD.jags2 <- HPDinterval(as.mcmc(chainMatrix2))

res.dat2 <- data.frame('ET2.5'=p.ET.jags2[1,],
                       'ET97.5'=p.ET.jags2[2,],
                       'HPD2.5'=p.HPD.jags2[,1],
                       'HPD97.5'=p.HPD.jags2[,2])

row.names(res.dat2)[c(1:4,6,24,25)] <- c(paste("\\beta_{",seq(1:4),"}$",sep=''),"$\\mu","$\\sigma$","$\\tau")
row.names(res.dat2)[c(7:23)] <- paste("$p_{",seq(1:17),"}$",sep='')


my.colors <- c('white',rep('royalblue',2),rep('yellow',2)) # set colors

kbl(round(res.dat2[c(1:5,7:23,25,24,6),],4),col.names = NULL,
    caption="     95% Intervals:") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" ", "Lower" = 1, "Upper" = 1, "Lower" = 1, "Upper" = 1),
                   bold = T, background = alpha(my.colors, 0.05), color = 'red') %>%
  add_header_above(c(" ", "Equal Tail" = 2, "HPD" = 2),
                   background = alpha(my.colors[c(1:2,4)],0.05), color = 'black') %>%
  column_spec(c(2,3), background = alpha(my.colors[2], 0.05)) %>%
  column_spec(c(4,5), background = alpha(my.colors[4], 0.05)) %>%
  scroll_box(height = "400px")

# (6) Model Comparison


cat(
  paste("- `", "Model 1 DIC: ",round(mod.fit$BUGSoutput$DIC, 3), "`"),
  paste("- `", "Model 2 DIC: ",round(mod.fit2$BUGSoutput$DIC, 3), "`"),
  sep='\n'
)

cat(paste("`", "DIC Difference:", round(abs(mod.fit$BUGSoutput$DIC - mod.fit2$BUGSoutput$DIC),3),"`"))





# (5) Appendix B --------------------------------------------

# DENSITY PLOT GIF
color_scheme_set("pink")

saveGIF ({
  # (3) mcmc_trace
  for (m in 1:N){
    plot(mcmc_dens_overlay(chainArray2,par=c(paste("p[",m,"]",sep='')),
                           facet_args = list(labeller = ggplot2::label_parsed)) +
           xlab(paste('p',m))) 
    Sys.sleep(1)
  }
}, ani.height = 400, ani.width =750, movie.name = "pi_density.gif")

# GELMAN RUBIN GIF

saveGIF ({
  # (3) Gelman-Rubin
  for (m in 1:N){
    gelman.plot(coda.fit2[,paste("p[",m,"]",sep='')], main=paste('p',m))
    Sys.sleep(1)
  }
}, ani.height = 400, ani.width =750, movie.name = "Gelman_Rubin_plot.gif")


# TRACE PLOT GIF

color_scheme_set("mix-brightblue-gray")
saveGIF ({
  # (3) mcmc_trace
  for (m in 1:N){
    plot(mcmc_trace(chainArray2,par=c(paste("p[",m,"]",sep='')),
                    facet_args = list(labeller = ggplot2::label_parsed))+
           ylab(paste('p',m)) ) 
    Sys.sleep(1)
  }
}, ani.height = 400, ani.width =750, movie.name = "pi_traceplot.gif")




















# (6) Frequentest Approach ------------------------------------------------

#At first number of sampling was picked. 
#After that in the loop list of estimated probabilities that we calculated
#below was resampled with replacement. 
#Mean and Standard deviation of each sample were saved. 
#Using percentiles we can construct our Confidence Intervals.


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













par(mfrow=c(1,3))
hist(congo$GAM)
hist(congo$stunted_growth)
hist(congo$malnutrition_among_FeFAs)

plot(congo$GAM, congo$total_deaths)
plot(congo$stunted_growth, congo$total_deaths)
plot(congo$malnutrition_among_FeFAs, congo$total_deaths)





















coda.fit2.1 <- as.mcmc(mod.fit2.1)

aut.dat <- round(data.frame(autocorr.diag(coda.fit2.1)),4)
colnames(aut.dat)[c(1:4,24)] <- c("$\\beta_{1}$","$\\beta_{2}$","$\\beta_{3}$","$\\beta_{4}$","$\\sigma$")
colnames(aut.dat)[c(7:23)] <- paste("$p_{",seq(1:17),"}$",sep='')

aut.dat[,c(1:5,7:23,25,24,6)] %>% kbl(caption='Autoccorelation Diagnostic:') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = F) %>%
  row_spec(0, background = alpha("orchid",0.2), color = 'black') %>%
  column_spec(1, bold=T,width = "80px") %>%
  scroll_box(width = "830px")


res.dat2 <- data.frame('ET2.5'=p.ET.jags2[1,],
                       'ET97.5'=p.ET.jags2[2,],
                       'HPD2.5'=p.HPD.jags2[,1],
                       'HPD97.5'=p.HPD.jags2[,2])

row.names(res.dat2)[c(1:4,25)] <- c("$\\beta_{1}$","$\\beta_{2}$","$\\beta_{3}$","$\\beta_{4}$","$\\sigma$")
row.names(res.dat2)[c(7:23)] <- paste("$p_{",seq(1:17),"}$",sep='')
chainArray2 <- mod.fit2.1$BUGSoutput$sims.array

my.colors <- c('white',rep('royalblue',2),rep('yellow',2)) # set colors

kbl(round(res.dat2[c(1:5,7:23,25,24,6),],4),col.names = NULL,
    caption="     95% Intervals:") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" ", "Lower" = 1, "Upper" = 1, "Lower" = 1, "Upper" = 1),
                   bold = T, background = alpha(my.colors, 0.05), color = 'red') %>%
  add_header_above(c(" ", "Equal Tail" = 2, "HPD" = 2),
                   background = alpha(my.colors[c(1:2,4)],0.05), color = 'black') %>%
  column_spec(c(2,3), background = alpha(my.colors[2], 0.05)) %>%
  column_spec(c(4,5), background = alpha(my.colors[4], 0.05)) %>%
  scroll_box(height = "400px")

