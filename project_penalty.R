

##### THE MOTHERHOOD PENALTY
##### Team №13
##### Author: Tsogoeva Tatiana
##### Start: 04/10/22

#install.packages('haven')
#install.packages('kableExtra')
#install.packages('cowplot')
#install.packages("extrafont")
#install.packages("interplot")


library(haven)
library(dplyr)
library(tidyr) 
library(ggplot2) 
library(cowplot)
library(psych) 
library(kableExtra)
library(extrafont)
library(lmtest)   
library(sandwich)    
library(modelsummary)
library(stargazer)
library(margins)


#########################################
############## preparation ##############
#########################################

# Import data
data <- read_spss('RLMS.sav', col_select = c('ID_W',
                                            'H5',
                                            'J10',
                                            'J40',
                                            'J8',
                                            'J38',
                                            'J1',
                                            'J32',
                                            'MARST',
                                            'M3',
                                            'DIPLOM',
                                            'STATUS',
                                            'ID_I',
                                            'J72.171',
                                            'J72.172',
                                            'J72.173',
                                            'AGE'))
                                          

# Rename the columns
names(data) <- c('Wave',
                 'Id',
                 'Urban',
                 'Marital status',
                 'Education',
                 'Female',
                 'Age',
                 'Work status',
                 'Hours_1',
                 'Wage_1',
                 'Has second job',
                 'Hours_2',
                 'Wage_2',
                 'Children',
                 'Number of children',
                 'Number of children(under 18)',
                 'Health')


# Only observations 2021 
data <- subset(data, Wave == 30)
# Only women 
data <- subset(data, Female == 2)
# Only 'currently working'
data <- subset(data, `Work status` == 1)

data <- subset(data, Age >= 16 & Age < 60)


# Handle missing values
data[data == 99999997] = NA  #  DOES NOT KNOW
data[data == 99999998] = NA  #  REFUSES TO ANSWER
data[data == 99999999] = NA  #  NO ANSWER


# About the number of children asked only those,
# who answered positively to the question "HAS CHILDREN?"
data <- data %>% 
  mutate(`Number of children` = ifelse(Children == 2, 0, `Number of children`),
         `Number of children(under 18)` = ifelse(Children == 2, 0, `Number of children(under 18)`))


# About the second job asked only those,
# who answered positively to the question "HAS SECOND JOB?"
data <- data %>% 
  mutate(Hours_2 = ifelse(`Has second job` == 2, 0, Hours_2),
         Wage_2 = ifelse(`Has second job` == 2, 0, Wage_2))


# Cleaning
data <- na.omit(data)


# Urban, 1 - urban(oblastnoy center or town), 0 - another 
data <- data %>% 
  mutate(Urban = ifelse(Urban <= 2, 1, 0))


# Cleaning 
data <- data[,-c(1, 6, 8, 11)]
data <- data[,c('Id','Age', 'Wage_1', 'Hours_1','Wage_2','Hours_2','Children',
                'Number of children' ,'Number of children(under 18)', 
                'Education','Marital status', 'Urban', 'Health')]


# Reorder
data <- data %>% 
  mutate(Health = ifelse(Health == 5, 0,
                          ifelse(Health == 4, 1,
                                 ifelse(Health == 3, 2,
                                        ifelse(Health == 2, 3,
                                               ifelse(Health == 1, 4, NA))))))


data <- data %>% 
  mutate(Education = ifelse(Education == 1, 0, 
                            ifelse(Education == 2, 1, 
                                   ifelse(Education == 3, 2, 
                                          ifelse(Education == 4, 3, 
                                                 ifelse(Education == 5, 4,
                                                        ifelse(Education == 6, 5, NA)))))))

 
# Hourly wage
data <- data %>% 
  mutate(`Total wage` = Wage_1 + Wage_2,
         `Total hours` = Hours_1 + Hours_2)


# Total hourly wage
data <- data %>% 
  mutate(`Hourly total wage` = round(`Total wage` / `Total hours`,2))


# Cleaning (data with hourly total wage)
data <- data[,-c(3, 4, 5, 6, 14, 15)]


#########################################
######## descriptive statistics #########
#########################################


# All variables (table_1)
ds_1 = describe(select(data,
    c('Age','Number of children','Number of children(under 18)', 'Urban',
      'Education','Marital status','Health', 'Hourly total wage'))) 
ds_1 <- ds_1[,-c(1, 6, 7, 11, 12, 13)] 
ds_1$mean <- round(ds_1$mean, 2)
ds_1$sd <- round(ds_1$sd, 2)

ds_1 %>%
  kable(caption = 'Таблица описательных статистик переменных до удаления выбросов') %>%
  kable_styling(bootstrap_options = 'hover', position = "center")


########### Wage ###########

windowsFonts(A = windowsFont('Times New Roman'))

# Box plot
ggplot(data = data)+
  geom_boxplot(aes(y = `Hourly total wage`))+
  theme_light()+
  theme(text = element_text(family = 'A'))


# Removing outliers
Q <- quantile(data$`Hourly total wage`, probs = c(.25, .75))
IQR <- IQR(data$`Hourly total wage`)
up <- Q[2] + 1.5*IQR
low <- Q[1] - 1.5*IQR
data_after <- filter(data, (`Hourly total wage` > low) & (`Hourly total wage` < up))


# All variables (table_2)
ds_2 = describe(select(data_after,
                     c('Age','Number of children' ,'Number of children(under 18)', 'Urban',
                       'Education','Marital status','Health', 'Hourly total wage'))) 
ds_2 <- ds_2[,-c(1,6,7,11,12,13)] 
ds_2$mean <- round(ds_2$mean, 2)
ds_2$sd <- round(ds_2$sd, 2)

ds_2 %>%
  kable(caption = 'Таблица описательных статистик переменных после удаления выбросов') %>%
  kable_styling(bootstrap_options = 'hover', position = 'center')


#########################################
############### graphs ##################
#########################################


# Density
data <- data_after %>% 
  mutate(Children = ifelse(Children == 2, 'No children','Children')) 

data_after$Children <- haven::as_factor(data_after$Children)

data_after %>% 
    ggplot(aes(`Hourly total wage`, col = Children, fill = Children))+
    geom_density(alpha = 0.25)+
    xlab('Hourly total wage')+
    ylab('Density')+
    theme_light()+
    theme(text = element_text(family = 'A'))


########### wage + everything ###########


bp_we <- ggplot(data_after, aes(x = as.factor(Education), y = `Hourly total wage`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Education')+
  ylab('Hourly wage')+
  theme_light()+
  theme(text = element_text(family = 'A'))
  
bp_wu <- ggplot(data_after, aes(x = as.factor(Urban), y = `Hourly total wage`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Urban')+
  ylab('Hourly wage')+
  theme_light()+
  theme(text = element_text(family = 'A'))

bp_wm <- ggplot(data_after, aes(x = as.factor(`Marital status`), y = `Hourly total wage`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Marital status')+
  ylab('Hourly wage')+
  theme_light()+
  theme(text = element_text(family = 'A'))

bp_wh <- ggplot(data_after, aes(x = as.factor(Health), y = `Hourly total wage`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Health')+
  ylab('Hourly wage')+
  theme_light()+
  theme(text = element_text(family = 'A'))

plot_grid(bp_we, bp_wu, bp_wm, bp_wh, ncol = 2, nrow = 2)


############## wage + age ###############


wage_age <- ggplot(data_after, aes(x = Age, y =`Hourly total wage`)) +
  geom_point(col = '#104E8B', alpha = 0.4)+
  xlab('Age')+
  ylab('Hourly wage')+
  geom_smooth(method = loess, se = F, formula = y ~ x)+
  theme_light()+
  theme(text = element_text(family = 'A'))

plot_grid(wage_age, ncol = 1, nrow = 2)


############### children ###############


# Children hist
ch <- ggplot(data = data_after)+
  geom_histogram(aes(x = `Number of children`), bins = 20, fill = "#104E8B", )+
  scale_x_continuous(breaks = c(0:10))+
  ylim(0,1500)+
  ylab('Count')+
  theme_light()+
  theme(text = element_text(family = 'A'))

ch18 <- ggplot(data = data_after)+
  geom_histogram(aes(x = `Number of children(under 18)`), bins = 20, fill = "#104E8B", )+
  scale_x_continuous(breaks = c(0:10))+
  ylab('Count')+
  theme_light()+
  theme(text = element_text(family = 'A'))

pdf(file='children_density.pdf')
plot_grid(ch, ch18, ncol = 2, nrow = 2)
dev.off()


# Scattering diagram (wage + children)
wch <- ggplot(data_after, aes(x = `Number of children`, y =`Hourly total wage`)) +
  geom_jitter(width = 0.2, alpha = 0.5, col = '#104E8B') + 
  scale_x_continuous(breaks = c(0:8))+
  xlab('Number of children')+
  ylab('Hourly wage')+
  geom_smooth(method = 'lm', se=F, color = '#CD3333')+
  theme_light()+
  theme(text = element_text(family = 'A'))

wch18 <- ggplot(data_after, aes(x = `Number of children(under 18)`, y =`Hourly total wage`)) +
  geom_jitter(width = 0.2, alpha = 0.5, col = '#104E8B') + 
  scale_x_continuous(breaks = c(0:8))+
  xlab('Number of children (under 18)')+
  ylab('Hourly wage')+
  geom_smooth(method = 'lm', se=F, color = '#CD3333')+
  theme_light()+
  theme(text = element_text(family = 'A'))

plot_grid(wch, wch18, ncol = 2, nrow = 2)


############# children + age ############


age_ch <- ggplot(data_after, aes(y = `Hourly total wage`, x = as.factor(`Number of children`)))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Number of children')+
  ylab('Hourly total wage')+
  theme_light()+
  theme(text = element_text(family = 'A'))

age_ch18 <- ggplot(data_after, aes(y = `Hourly total wage`, x = as.factor(`Number of children(under 18)`)))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Number of children (under 18)')+
  ylab('Hourly total wage')+
  theme_light()+
  theme(text = element_text(family = 'A'))

plot_grid(age_ch, age_ch18, ncol = 2, nrow = 2)


######### children + everything ##########


che <- ggplot(data_after, aes(x = as.factor(Education), y = `Number of children`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Education')+
  ylab('Number of children')+
  theme_light()+
  theme(text = element_text(family = 'A'))

chu <- ggplot(data_after, aes(x = as.factor(Urban), y = `Number of children`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Urban')+
  ylab('Number of children')+
  theme_light()+
  theme(text = element_text(family = 'A'))

chm <- ggplot(data_after, aes(x = as.factor(`Marital status`), y = `Number of children`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Marital status')+
  ylab('Number of children')+
  theme_light()+
  theme(text = element_text(family = 'A'))

chh <- ggplot(data_after, aes(x = as.factor(Health), y = `Number of children`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Health')+
  ylab('Number of children')+
  theme_light()+
  theme(text = element_text(family = 'A'))

plot_grid(che, chu, chm, chh, ncol = 2, nrow = 2)


####### children(under 18) + everything ########


ch18e <- ggplot(data_after, aes(x = as.factor(Education), y = `Number of children(under 18)`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Education')+
  ylab('Number of children (under 18)')+
  theme_light()+
  theme(text = element_text(family = 'A'))

ch18u <- ggplot(data_after, aes(x = as.factor(Urban), y = `Number of children(under 18)`)) +
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Urban')+
  ylab('Number of children (under 18)')+
  theme_light()+
  theme(text = element_text(family = 'A'))

ch18m <- ggplot(data_after, aes(x = as.factor(`Marital status`), y = `Number of children(under 18)`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Marital status')+
  ylab('Number of children (under 18)')+
  theme_light()+
  theme(text = element_text(family = "A"))

ch18h <- ggplot(data_after, aes(x = as.factor(Health), y = `Number of children(under 18)`))+
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20)+
  xlab('Health')+
  ylab('Number of children (under 18)')+
  theme_light()+
  theme(text = element_text(family = 'A'))

plot_grid(ch18e, ch18u, ch18m, ch18h, ncol = 2, nrow = 2)


#########################################
############# regression ################
#########################################


############## Regression ###############


# Сonverting the variable: if the respondent has 3 
# or more children, then it has the value '3'
data_after <- data_after %>% 
  mutate(NOC = ifelse(`Number of children` == 0, 0,
                      ifelse(`Number of children` == 1, 1, 
                             ifelse( `Number of children` == 2, 2, 
                                     ifelse( `Number of children` >= 3, 3, NA)))))

data_after <- data_after %>% 
  mutate(NOC18 = ifelse(`Number of children(under 18)` == 0, 0,
                        ifelse(`Number of children(under 18)` == 1, 1, 
                               ifelse( `Number of children(under 18)` == 2, 2, 
                                       ifelse( `Number of children(under 18)` >= 3, 3, NA)))))


# Create age square 
data_after <- data_after %>% 
  mutate(`Age_sqr` = Age^2)


# Create log(wage)
data_after <- data_after %>% 
  mutate(`Hourly total wage (log)` = log(`Hourly total wage`))


# Create binary variables (children)
data_after <- data_after %>% 
  mutate(NOC_1 = ifelse(`Number of children` == 1, 1, 0),
         NOC_2 = ifelse(`Number of children` == 2, 1, 0),
         NOC_more3 = ifelse(`Number of children` >= 3, 1, 0))


# Create binary variables (children18)
data_after <- data_after %>% 
  mutate(NOC18_1 = ifelse(`Number of children(under 18)` == 1, 1, 0),
         NOC18_2 = ifelse(`Number of children(under 18)` == 2, 1, 0),
         NOC18_more3 = ifelse(`Number of children(under 18)` >= 3, 1, 0))


# Create binary variables (marital status)
data_after <- data_after %>% 
  mutate(Binary_marital_status = ifelse(`Marital status` == 2, 1,
                                        ifelse(`Marital status` == 3, 1, 0)))


# Regression(1) for children
rgr1 <- lm(`Hourly total wage (log)` ~ 1 + 
             NOC_1 +
             NOC_2 +
             NOC_more3 +
             Age + 
             Age_sqr +
             as.character(Education) +
             Binary_marital_status +
             as.character(Health) +
             Urban, 
             data = data_after)

cov1 <- vcovHC(rgr1, type = "HC0")
se1 <- sqrt(diag(cov1))


# Regression(1) for children under 18
rgr1_18 <- lm(`Hourly total wage (log)` ~ 1 + 
                NOC18_1 +
                NOC18_2 +
                NOC18_more3 +
                Age + 
                Age_sqr +
                as.character(Education) +
                Binary_marital_status +
                as.character(Health) +
                Urban, 
                data = data_after)
 
cov1_18 <- vcovHC(rgr1_18, type = "HC0")
se1_18 <- sqrt(diag(cov1_18))


# Regression(2) for children
rgr2 <- lm(`Hourly total wage (log)` ~ 1 + 
             NOC_1 +
             NOC_2 +
             NOC_more3 +
             Age + 
             Age_sqr +
             as.character(Education) +
             Binary_marital_status +
             as.character(Health) +
             Urban +
             NOC_1:Binary_marital_status +
             NOC_2:Binary_marital_status +
             NOC_more3:Binary_marital_status, 
             data = data_after)

cov2 <- vcovHC(rgr2, type = "HC0")
se2 <- sqrt(diag(cov2))


# Regression(2) for children under 18
rgr2_18 <- lm(`Hourly total wage (log)` ~ 1 + 
                NOC18_1 +
                NOC18_2 +
                NOC18_more3 +
                Age + 
                Age_sqr +
                as.character(Education) +
                Binary_marital_status +
                as.character(Health) +
                Urban +
                NOC18_1:Binary_marital_status +
                NOC18_2:Binary_marital_status +
                NOC18_more3:Binary_marital_status, 
                data = data_after)

cov2_18 <- vcovHC(rgr2_18, type = "HC0")
se2_18 <- sqrt(diag(cov2_18))


# Final table (children)
stargazer(rgr1,rgr2,
          se = list(se1, se2),               
          title = "Результаты оценки",                          
          keep.stat = "n",                                      
          notes = "В скобках даны робастные стандартные ошибки", 
          type = 'text')


# Final table (children under 18)
stargazer(rgr1_18,rgr2_18,
          se = list(se1_18, se2_18),               
          title = "Результаты оценки",                          
          keep.stat = "n",                                      
          notes = "В скобках даны робастные стандартные ошибки", 
          type = 'text')


################ T-test #################


# We need to test the hypothesis: Н0 is that the coefficient before
# 'Number of children' and 'Number of children(under 18) is 0


# For all children 
t1 <- coeftest(rgr1, df=Inf, vcov = vcovHC, type = "HC0")

tt1  <- t1[2,1] / t1[2,2]
pv1 <- pnorm(tt1) 

tt2  <- t1[3,1] / t1[3,2]
pv2 <- pnorm(tt2) 

tt3  <- t1[4,1] / t1[4,2]
pv3 <- pnorm(tt3) 


# For children under 18
t1_18 <- coeftest(rgr1_18, df=Inf, vcov = vcovHC, type = "HC0")

tt1_18  <- t1_18[2,1] / t1_18[2,2]
pv1_18 <- pnorm(tt1_18) 

tt2_18  <- t1_18[3,1] / t1_18[3,2]
pv2_18 <- pnorm(tt2_18) 

tt3_18  <- t1_18[4,1] / t1_18[4,2]
pv3_18 <- pnorm(tt3_18)


########### Marginal effects ############


# Сalculate the marginal effects 
mar1 <- rgr2 %>%  margins_summary(variables = 'NOC_1',
                                 at = list(Binary_marital_status = c(0, 1)),
                                 vcov = cov2)

mar2 <- rgr2 %>%  margins_summary(variables = 'NOC_2',
                                  at = list(Binary_marital_status = c(0, 1)),
                                  vcov = cov2)

mar3 <- rgr2 %>%  margins_summary(variables = 'NOC_more3',
                                  at = list(Binary_marital_status = c(0, 1)),
                                  vcov = cov2)

mar1_18 <- rgr2_18 %>%  margins_summary(variables = 'NOC18_1',
                                  at = list(Binary_marital_status = c(0, 1)),
                                  vcov = cov2_18)

mar2_18 <- rgr2_18 %>%  margins_summary(variables = 'NOC18_2',
                                  at = list(Binary_marital_status = c(0, 1)),
                                  vcov = cov2_18)

mar3_18 <- rgr2_18 %>%  margins_summary(variables = 'NOC18_more3',
                                  at = list(Binary_marital_status = c(0, 1)),
                                  vcov = cov2_18)


# Graph of marginal effects (alpha= 0.05)
ME1 <- mar1 %>% ggplot()+
  geom_point(aes (x = Binary_marital_status, y = AME)) +
  geom_errorbar(aes (x = Binary_marital_status, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  xlab('Marital status (binary)') +
  ylab('Average marginal effects') +
  theme(text = element_text(family = 'A')) +
  theme_light()

ME2 <- mar2 %>% ggplot()+
  geom_point(aes (x = Binary_marital_status, y = AME)) +
  geom_errorbar(aes (x = Binary_marital_status, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  xlab('Marital status (binary)') +
  ylab('Average marginal effects') +
  theme(text = element_text(family = 'A')) +
  theme_light()

ME3 <- mar3 %>% ggplot()+
  geom_point(aes (x = Binary_marital_status, y = AME)) +
  geom_errorbar(aes (x = Binary_marital_status, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  xlab('Marital status (binary)') +
  ylab('Average marginal effects') +
  theme(text = element_text(family = 'A')) +
  theme_light()

ME1_18 <- mar1_18 %>% ggplot()+
  geom_point(aes (x = Binary_marital_status, y = AME)) +
  geom_errorbar(aes (x = Binary_marital_status, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  xlab('Marital status (binary)') +
  ylab('Average marginal effects') +
  theme(text = element_text(family = 'A')) +
  theme_light()

ME2_18 <- mar2_18 %>% ggplot()+
  geom_point(aes (x = Binary_marital_status, y = AME)) +
  geom_errorbar(aes (x = Binary_marital_status, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  xlab('Marital status (binary)') +
  ylab('Average marginal effects') +
  theme(text = element_text(family = 'A')) +
  theme_light()

ME3_18 <- mar3_18 %>% ggplot()+
  geom_point(aes (x = Binary_marital_status, y = AME)) +
  geom_errorbar(aes (x = Binary_marital_status, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  xlab('Marital status (binary)') +
  ylab('Average marginal effects') +
  theme(text = element_text(family = 'A')) +
  theme_light()

plot_grid(ME1, ME2, ME3, ncol = 3, nrow = 2)
plot_grid(ME1_18, ME2_18, ME3_18, ncol = 3, nrow = 2)







