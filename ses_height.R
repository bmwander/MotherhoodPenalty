
##### SES & HEIGHT
##### Author: Tsogoeva Tatiana
##### Start: 08/03/23
##### Last: 16/03/23


library(haven)
library(dplyr)
library(tidyr) 
library(ggplot2) 
library(cowplot)
library(psych) 
library(kableExtra)
library(extrafont)
library(lmtest)   
library(MASS)
library(sandwich)    
library(modelsummary)
library(stargazer)


#########################################
############## preparation ##############
#########################################

# Import data
data <- read_spss('RLMS.sav', col_select = c('ID_W', # WAVE OF SURVEY=YEAR
                                             'ID_I',
                                             'REGION', # 
                                             # 'PSU', # PRIMARY SAMPLE UNIT
                                             'STATUS', # 
                                             'MARST', # MARITAL STATUS 
                                             'J72.171', # HAS children?
                                             'EDUC', # EDUCATION (DETAIL): OVER 14 YEARS
                                             'H5', # RESPONDENT GENDER
                                             'I4', # NATIONALITY CONSIDERS SELF
                                             'AGE', # NUMBER OF FULL YEARS 
                                             'J1', # CURRENT WORK STATUS1 
                                             'J2COD08', # ILO 2008 CODE--JOB 1 
                                             'J6', # SUBORDINATES?--JOB 1
                                             'J6.0', # NUMBER OF SUBORDINATES--JOB 1
                                             'J32', # CURRENTLY HAS SECOND JOB?
                                             'J33COD08', # ILO 2008 CODE--EXTRA JOB
                                            # 'J36', # SUBORDINATES?--JOB 2
                                             'J60', # TOTAL income IN LAST 30 DAYS
                                             'M2', # SELF-REPORTED HEIGHT
                                             'M3')) # health EVALUATION


# Rename the columns
names(data) <- c('wave',
                 'id',
                 'region',
               # 'Psu',
                 'status',
                 'marst',
                 'educ',
                 'male',
                 'age',
                 'russian',
                 'workst',
                 'codejob1',
                 'sub1',
                 'sub1_num',
                 'job2',
                 'codejob2',
                # 'Sub_2',
                 'income',
                 'children',
                 'height',
                 'health')

# 
data$age <- as.numeric(data$age)
data$sub1_num <- as.numeric(data$sub1_num)
data$income <- as.numeric(data$income)
data$height <- as.numeric(data$height)

# Handle missing values
data[data == 99999997] = NA  #  DOES NOT KNOW
data[data == 99999998] = NA  #  REFUSES TO ANSWER
data[data == 99999999] = NA  #  NO ANSWER

# Only observations 2021 
data <- subset(data, wave == 30)

# ISCO-08
data <- data %>% 
  mutate(codejob2 = sub("^(\\w).*$", "\\1", codejob2),
         codejob1 = sub("^(\\w).*$", "\\1", codejob1))

# Reorder
data <- data %>% 
  mutate(male = ifelse(male == 1, 1, 0),
         russian = ifelse(russian == 1, 1, 0),
         sub1 = ifelse(sub1 == 1, 1, 0),
         job2 = ifelse(job2 == 1, 1, 0),
         children = ifelse(children == 1, 1, 0))

data <- data %>% 
  mutate(health = ifelse(health == 5, 0,
                         ifelse(health == 4, 1,
                                ifelse(health == 3, 2,
                                       ifelse(health == 2, 3,
                                              ifelse(health == 1, 4, NA))))))

data <- data %>% 
  mutate(codejob1 = ifelse(codejob1 == 0, 10,
                         ifelse(codejob1 == 1, 9,
                                ifelse(codejob1 == 2, 8,
                                       ifelse(codejob1 == 3, 7,
                                              ifelse(codejob1 == 4, 6, 
                                                     ifelse(codejob1 == 5, 5, 
                                                            ifelse(codejob1 == 6, 4, 
                                                                   ifelse(codejob1 == 7, 3,
                                                                          ifelse(codejob1 == 8, 2, 
                                                                                 ifelse(codejob1 == 9, 1, NA)))))))))))

data <- data %>% 
  mutate(codejob2 = ifelse(codejob2 == 0, 10,
                           ifelse(codejob2 == 1, 9,
                                  ifelse(codejob2 == 2, 8,
                                         ifelse(codejob2 == 3, 7,
                                                ifelse(codejob2 == 4, 6, 
                                                       ifelse(codejob2 == 5, 5, 
                                                              ifelse(codejob2 == 6, 4, 
                                                                     ifelse(codejob2 == 7, 3,
                                                                            ifelse(codejob2 == 8, 2, 
                                                                                   ifelse(codejob2 == 9, 1, NA)))))))))))

# 
data <- data %>% 
  mutate(codejob1 = ifelse(workst == 5, 0, codejob1),
         sub1 = ifelse(workst == 5, '', sub1),
         sub1_num = ifelse(workst == 5, '', sub1_num),
         job2 = ifelse(workst == 5, '', job2),
         codejob2 = ifelse(workst == 5, 0, codejob2),
         sub1_num = ifelse(sub1 == 0, 0, sub1_num),
         codejob2 = ifelse(job2 == 0, 0, codejob2),
         gender = ifelse(male == 1, 'male', 'female'))

# Create binary variables (marital status)
data <- data %>% 
  mutate(marst = ifelse(marst == 2, 1,
                        ifelse(marst == 3, 1, 0)))

# Cleaning
data <- na.omit(data) #13246

data <- data[,-1]
data <- data[,c('id','age', 'male', 'gender', 'russian','region', 'status',
                'marst', 'health', 'children', 'height', 'educ', 'workst', 
                'codejob1', 'sub1', 'sub1_num','job2', 'codejob2', 'income')]

# Log
data$lincome <- log(data$income)


#########################################
######## descriptive statistics #########
######################################### 

# DS (table_1)
ds_1 = describe(select(data,
                       c('age', 'status', 'sub1', 'job2', 'height', 'children',
                         'educ','marst','health', 'income'))) 
ds_1 <- ds_1[,-c(1, 6, 7, 11, 12, 13)] 
ds_1$mean <- round(ds_1$mean, 2)
ds_1$sd <- round(ds_1$sd, 2)

ds_1 %>%
  kable(caption = 'Таблица описательных статистик до удаления выбросов в доходе') %>%
  kable_styling(bootstrap_options = 'hover', position = "center")

# Box plot
ggplot(data = data) +
  geom_boxplot(aes(y = lincome)) +
  ylab('Логарифм дохода') +
  theme_light()

# Removing outliers
Q <- quantile(data$lincome, probs = c(.25, .75))
IQR <- IQR(data$lincome)
up <- Q[2] + 1.5*IQR
low <- Q[1] - 1.5*IQR
data <- filter(data, (lincome > low) & (lincome < up)) #log 11787

# DS (table_1)
ds_2 = describe(select(data,
                       c('age', 'status', 'sub1', 'job2', 'height', 'children',
                         'educ','marst','health', 'income')))
ds_2 <- ds_2[,-c(1, 6, 7, 11, 12, 13)] 
ds_2$mean <- round(ds_2$mean, 2)
ds_2$sd <- round(ds_2$sd, 2)

ds_2 %>%
  kable(caption = 'Таблица описательных статистик после удаления выбросов в доходе') %>%
  kable_styling(bootstrap_options = 'hover', position = "center")

# Average height male/female (168.1012)
aggregate(x = c(data$height),    
          by = list(data$gender),      
          FUN = mean)


#########################################
############### graphs ##################
#########################################

ggplot(data, aes(x = as.factor(region), y = lincome)) +
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20) +
  xlab('Регион') +
  ylab('Логарифм дохода') +
  theme_light()

ggplot(data, aes(x = as.factor(region), y = Height)) +
  geom_boxplot(fill = '#4271AE', colour = '#1F3552', 
               alpha = 0.8,
               outlier.colour = '#1F3552', outlier.shape = 20) +
  xlab('Регион') +
  ylab('Рост, см') +
  theme_light()

ggplot(data,
       aes(x = as.numeric(height), y = as.numeric(lincome), color = gender)) +
  geom_point(alpha = 0.4) +
  labs(x = 'Рост, см', y = 'Логарифм дохода') +
  theme_light()


#########################################
############# regression ################
#########################################


############## Regression ###############

data$educ <- factor(data$educ)
data$codejob1 <- factor(data$codejob1)
data$sub1 <- factor(data$sub1)
data$sub1_num <- as.numeric(data$sub1_num)

# Regression for education
model_educ <- polr(educ ~ 1 + 
                   height +
                   age + 
                   male +
                 # russian +
                   as.factor(region) +
                   as.factor(status),
                   data = data)

# Regression for income
model_income <- lm(lincome ~ 1 + 
                     height +
                     age + 
                     male +
                   # russian +
                     marst +
                     as.factor(region) +
                     as.factor(status) +
                     as.factor(educ) +
                     as.factor(codejob1) +
                   # sub1_num +
                     as.factor(codejob2) +
                     as.factor(health) +
                     children,
                     data = data)

# Regression for job
model_codejob1 <- polr(codejob1 ~ 1 + 
                         height +
                         age + 
                         male +
                       # russian +
                       # marst +
                         as.factor(region) +
                         as.factor(status) +
                         as.factor(educ) +
                         as.factor(health),
                       # children,
                         data = data)

# пепа не воркинг
# model_codejob2 <- polr(codejob2 ~ 1 + 
#                          height +
#                          age + 
#                          male +
#                          russian +
#                          marst +
#                          as.factor(region) +
#                          as.factor(status) +
#                          as.factor(educ) +
#                          as.factor(health) +
#                          children,
#                          data = data)

# Regression for subordinates 
# model_sub <- glm(sub1 ~ 1 + 
#                   height +
#                   age + 
#                   male +
#                   russian +
#                   marst +
#                   as.factor(region) +
#                   as.factor(status) +
#                   as.factor(educ) +
#                   as.factor(codejob1) +
#                   as.factor(health) +
#                   children,
#                   data = data,
#                   family = binomial)
# 
# # Regression for number of subordinates 
# model_sub_num <- lm(sub1_num ~ 1 + 
#                      height +
#                      age + 
#                      male +
#                      russian +
#                      marst +
#                      as.factor(region) +
#                      as.factor(status) +
#                      as.factor(educ) +
#                      as.factor(codejob1) +
#                      as.factor(health) +
#                      children,
#                      data = data)

stargazer(list(model_educ, model_income, model_codejob1),
          title = "Результаты оценки",                          
          keep.stat = "n",                                      
          notes = "В скобках даны стандартные ошибки", 
          type = 'text')

# Heteroskedasticity?
bptest(model_income)


############# Male/Female ###############

datam <- subset(data, male == 1)
dataf <- subset(data, male == 0)

# Regression for education
model_educm <- polr(educ ~ 1 + 
                     height +
                     age + 
                   # russian +
                     as.factor(region) +
                     as.factor(status),
                     data = datam)

model_educf <- polr(educ ~ 1 + 
                     height +
                     age + 
                   # russian +
                     as.factor(region) +
                     as.factor(status),
                     data = dataf)

stargazer(list(model_educm, model_educf),
          title = "Результаты оценки",                          
          keep.stat = "n",                                      
          notes = "В скобках даны стандартные ошибки", 
          type = 'text')

# Regression for income
model_incomem <- lm(lincome ~ 1 + 
                     height +
                     age + 
                   # russian +
                     marst +
                     as.factor(region) +
                     as.factor(status) +
                     as.factor(educ) +
                     as.factor(codejob1) +
                   # sub1_num +
                     as.factor(codejob2) +
                     as.factor(health) +
                     children,
                     data = datam)

model_incomef <- lm(lincome ~ 1 + 
                     height +
                     age + 
                   # russian +
                     marst +
                     as.factor(region) +
                     as.factor(status) +
                     as.factor(educ) +
                     as.factor(codejob1) +
                   # sub1_num +
                     as.factor(codejob2) +
                     as.factor(health) +
                     children,
                     data = dataf)

stargazer(list(model_incomem, model_incomef),
          title = "Результаты оценки",                          
          keep.stat = "n",                                      
          notes = "В скобках даны стандартные ошибки", 
          type = 'text')

# Regression for job
model_codejob1m <- polr(codejob1 ~ 1 + 
                          height +
                          age + 
                        # russian +
                        # marst +
                          as.factor(region) +
                          as.factor(status) +
                          as.factor(educ) +
                          as.factor(health),
                        # children,
                          data = datam, Hess = T)
  
model_codejob1f <- polr(codejob1 ~ 1 + 
                         height +
                         age + 
                       # russian +
                       # marst +
                         as.factor(region) +
                         as.factor(status) +
                         as.factor(educ) +
                         as.factor(health),
                       # children,
                         data = dataf, Hess = T)

stargazer(list(model_codejob1m, model_codejob1f),
          title = "Результаты оценки",                          
          keep.stat = "n",                                      
          notes = "В скобках даны стандартные ошибки", 
          type = 'text')

stargazer(list(model_educm, model_educf, model_incomem, model_incomef, model_codejob1m, model_codejob1f),
          title = "Результаты оценки",                          
          keep.stat = "n",                                      
          notes = "В скобках даны стандартные ошибки", 
          type = 'text')























