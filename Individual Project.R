# Datasets 
dUsers <- read.csv('~/Desktop/Data_Project_1/users.csv')
dOrders <- read.csv('~/Desktop/Data_Project_1/orders.csv')

# Some Feature Engineering

# Dummy Variables for age
dUsers$age16to25 <- ifelse(dUsers$age == '16-25',1,0) 
dUsers$age26to35 <- ifelse(dUsers$age == '26-35',1,0) 
dUsers$age36to45 <- ifelse(dUsers$age == '36-45',1,0) 
dUsers$age46to55 <- ifelse(dUsers$age == '46-55',1,0) 
dUsers$ageGreaterOrEqualTo56 <- ifelse(dUsers$age == '>=56',1,0) 

# Dummy Variables for education
dUsers$edu1 <- ifelse(dUsers$education == 1,1,0) 
dUsers$edu2 <- ifelse(dUsers$education == 2,1,0) 
dUsers$edu3 <- ifelse(dUsers$education == 3,1,0) 
dUsers$edu4 <- ifelse(dUsers$education == 4,1,0) 

# Dummy Variables for gender
dUsers$gender_Female <- ifelse(dUsers$gender == 'F',1,0) 
dUsers$gender_Male <- ifelse(dUsers$gender == 'M',1,0) 

# Dummy Variables for marital status
dUsers$marital_status_single <- ifelse(dUsers$marital_status == 'S',1,0) 
dUsers$marital_status_married <- ifelse(dUsers$marital_status == 'M',1,0) 

# create a new table
library('sqldf')

# I use an INNER JOIN here so only records for users who placed orders will appear in the new data frame. 
# This way I avoid including customers who did not place orders 
# A LEFT OUTER JOIN would have included them and we don't want that
newdf <- sqldf(
  'SELECT    u.user_ID, sum(o.final_unit_price) "SUM_ORDERS", 
                          u.age, u.age16to25, u.age26to35, u.age36to45, u.age46to55, u.ageGreaterOrEqualTo56,
                          u.marital_status, u.education, u.edu1, u.edu2, u.edu3, u.edu4, u.gender_Female, u.gender_Male,
                          u.marital_status_single, u.marital_status_married
                
                FROM      dUsers u INNER JOIN dOrders o ON u.user_ID = o.user_ID
                
                WHERE     (u.age16to25 = 1 OR u.age26to35 = 1 OR u.age36to45 = 1 OR u.age46to55 = 1 OR u.ageGreaterOrEqualTo56 = 1)
                          AND (u.marital_status <> "U") AND (u.education <> -1) AND (u.gender <> "U") AND (o.quantity == 1)
                
                GROUP BY  u.user_ID, u.gender, u.age, u.age16to25, u.age26to35,
                          u.age36to45, u.age46to55, u.ageGreaterOrEqualTo56, u.marital_status, u.education, u.edu1,
                          u.edu2, u.edu3, u.edu4 
                
                ORDER BY  "SUM_ORDERS" DESC'
)


# I eliminated the remaining instances with UNKNOWN values in marital status, education, and gender.
nrow(newdf[newdf$marital_status == 'U',]) #0   FEATURE ENGINEERING IN SQL WHERE clause
nrow(newdf[newdf$education == -1,]) #0         FEATURE ENGINEERING IN SQL WHERE clause
nrow(newdf[newdf$gender == 'U',]) #0           FEATURE ENGINEERING IN SQL WHERE clause

# some of the orders are negative dollars so I correct for this by turning all negatives to 0.
newdf$SUM_ORDERS <- ifelse(newdf$SUM_ORDERS < 0, 0, newdf$SUM_ORDERS) 


ols <- lm(SUM_ORDERS ~  gender_Male + marital_status_married
          + edu2 + edu3 + edu4
          + age36to45 + age46to55 + ageGreaterOrEqualTo56, newdf)

summary(ols) 

# I turn my newdf data frame into a csv so I can use it in Tableau to create graphs and charts
write.csv(newdf, '~/Desktop/Newdf.csv', row.names=FALSE)


