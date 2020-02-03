
####Load Data####
library(tidyverse)
library(caret)
library(neuralnet)
library(randomForest)

setwd("/home/jacob_w_lyman/home")

train <- read_csv("my_train.csv") %>%
  select(-X1)

####Clean Data####
train <- train %>%
  mutate(channelGrouping = as.factor(channelGrouping)
         , date = date
         , fullVisitorId = as.factor(fullVisitorId)
         , visitId = as.factor(visitId)
         , visitNumber = as.factor(visitNumber)
         , visitStartTime = as.factor(visitStartTime)
         , browser = as.factor(browser)
         , operatingSystem = as.factor(operatingSystem)
         , isMobile = as.logical(isMobile)
         , deviceCategory = as.factor(deviceCategory)
         , continent = as.factor(continent)
         , subContinent = as.factor(subContinent)
         , country = as.factor(country)
         , region = as.factor(region)
         , metro = as.factor(metro)
         , city = as.factor(city)
         , networkDomain = as.factor(networkDomain)
         , hits1 = as.numeric(hits1)
         , pageviews = as.numeric(pageviews)
         , bounces = as.numeric(bounces)
         , newVisits = as.numeric(newVisits)
         , sessionQualityDim = as.numeric(sessionQualityDim)
         , timeOnSite = as.numeric(timeOnSite)
         , transactions = as.numeric(transactions)
         , transactionRevenue = as.numeric(transactionRevenue)
         , totalTransactionRevenue = as.numeric(totalTransactionRevenue)
         , campaign = as.factor(campaign)
         , source = source
         , medium = as.factor(medium)
         , keyword = as.factor(keyword)
         , referralPath = referralPath 
         , isTrueDirect = isTrueDirect
         , adContent = as.factor(adContent)
  )

train$isTrueDirect[is.na(train$isTrueDirect)] <- 'False'
train <- train %>%
  mutate(isTrueDirect = as.logical(isTrueDirect)
         , isTrueDirect = as.numeric(isTrueDirect)
         , isMobile = as.numeric(isMobile))

# Remove NAs
train$transactionRevenue[is.na(train$transactionRevenue)] <- 0
train$transactions[is.na(train$transactions)] <- 0
train$totalTransactionRevenue[is.na(train$totalTransactionRevenue)] <- 0

# NOTE: These next few lines are replacing NAs with 0 but should treated differently
train$timeOnSite[is.na(train$timeOnSite)] <- 0
train$newVisits[is.na(train$newVisits)] <- 0
train$bounces[is.na(train$bounces)] <- 0
train$pageviews[is.na(train$pageviews)] <- 0
train$sessionQualityDim[is.na(train$sessionQualityDim)] <- 0

#### Feature Engineering ####

train_continuous <- train %>%
  mutate(PredictedLogRevenue = log(transactionRevenue)) %>%
  select(PredictedLogRevenue 
         , hits1
         , pageviews
         , bounces
         , newVisits
         , sessionQualityDim
         , timeOnSite
         , transactions)

#Create dummy variables of important factor columns for matrix
newtrain <- train %>%
  select(channelGrouping
         , browser
         , operatingSystem)
newtrain <- model.matrix(~., data = newtrain)
newtrain <- as.data.frame(newtrain)

newtrain2 <- train %>%
  select(isMobile
         , deviceCategory
         , country)
newtrain2 <- model.matrix(~., data = newtrain2)
newtrain2 <- as.data.frame(newtrain2)

newtrain3 <- train %>%
  select(region
         , campaign)
newtrain3 <- model.matrix(~., data = newtrain3)
newtrain3 <- as.data.frame(newtrain3)

train <- cbind(newtrain[,-1], newtrain2[,-1], newtrain3[,-1])

rm(newtrain,newtrain2,newtrain3)

dummies <- names(train)

new_names <- c('x802','x801','x800','x799','x798','x797','x796','x795','x794','x793','x792','x791','x790','x789','x788','x787','x786','x785','x784','x783','x782','x781','x780','x779','x778','x777','x776','x775','x774','x773','x772','x771','x770','x769','x768','x767','x766','x765','x764','x763','x762','x761','x760','x759','x758','x757','x756','x755','x754','x753','x752','x751','x750','x749','x748','x747','x746','x745','x744','x743','x742','x741','x740','x739','x738','x737','x736','x735','x734','x733','x732','x731','x730','x729','x728','x727','x726','x725','x724','x723','x722','x721','x720'
               ,'x719','x718','x717','x716','x715','x714','x713','x712','x711','x710','x709','x708','x707','x706','x705','x704','x703','x702','x701','x700','x699','x698','x697','x696','x695','x694','x693','x692','x691','x690','x689','x688','x687','x686','x685','x684','x683','x682','x681','x680','x679','x678','x677','x676','x675','x674','x673','x672','x671','x670','x669','x668','x667','x666','x665','x664','x663','x662','x661','x660','x659','x658','x657','x656','x655','x654','x653','x652','x651','x650','x649','x648','x647','x646','x645','x644','x643','x642','x641','x640','x639','x638','x637','x636','x635','x634','x633','x632','x631','x630','x629','x628','x627','x626','x625','x624','x623','x622','x621','x620','x619','x618','x617','x616','x615','x614','x613','x612','x611','x610','x609','x608','x607','x606','x605','x604','x603','x602','x601','x600','x599','x598','x597','x596','x595','x594','x593','x592','x591','x590','x589','x588','x587','x586','x585','x584','x583','x582','x581','x580','x579','x578','x577','x576','x575','x574','x573','x572','x571','x570','x569','x568','x567','x566','x565','x564','x563','x562','x561','x560','x559','x558','x557','x556','x555','x554','x553','x552','x551','x550','x549','x548','x547','x546','x545','x544','x543','x542','x541','x540','x539','x538','x537','x536','x535','x534','x533','x532','x531','x530','x529','x528','x527','x526','x525','x524','x523','x522','x521','x520','x519','x518','x517','x516','x515','x514','x513','x512','x511','x510','x509','x508','x507','x506','x505','x504','x503','x502','x501','x500','x499','x498','x497','x496','x495','x494','x493','x492','x491','x490','x489','x488','x487','x486','x485','x484','x483','x482','x481','x480','x479','x478','x477','x476','x475','x474','x473','x472','x471','x470','x469','x468','x467','x466','x465','x464','x463','x462','x461','x460','x459','x458','x457','x456','x455','x454','x453','x452','x451','x450','x449','x448','x447','x446','x445','x444','x443','x442','x441','x440','x439','x438','x437','x436','x435','x434','x433','x432','x431','x430','x429','x428','x427','x426','x425','x424','x423','x422','x421','x420','x419','x418','x417','x416','x415','x414','x413','x412','x411','x410','x409','x408','x407','x406','x405','x404','x403','x402','x401','x400','x399','x398','x397','x396','x395','x394','x393','x392','x391','x390','x389','x388','x387','x386','x385','x384','x383','x382','x381','x380','x379','x378','x377','x376','x375','x374','x373','x372','x371','x370','x369','x368','x367','x366','x365','x364','x363','x362','x361','x360','x359','x358','x357','x356','x355','x354','x353','x352','x351','x350','x349','x348','x347','x346','x345','x344','x343','x342','x341','x340','x339','x338','x337','x336','x335','x334','x333','x332','x331','x330','x329','x328','x327','x326','x325','x324','x323','x322','x321','x320','x319','x318','x317','x316','x315','x314','x313','x312','x311','x310','x309','x308','x307','x306','x305','x304','x303','x302','x301','x300','x299','x298','x297','x296','x295','x294','x293','x292','x291','x290','x289','x288','x287','x286','x285','x284','x283','x282','x281','x280','x279','x278','x277','x276','x275','x274','x273','x272',
               'x271','x270','x269','x268','x267','x266','x265','x264','x263','x262','x261','x260','x259','x258','x257','x256','x255','x254','x253','x252','x251','x250','x249','x248','x247','x246','x245','x244','x243','x242','x241','x240','x239','x238','x237','x236','x235','x234','x233','x232','x231','x230','x229','x228','x227','x226','x225','x224','x223','x222','x221','x220','x219','x218','x217','x216','x215','x214','x213','x212','x211','x210','x209','x208','x207','x206','x205','x204','x203','x202','x201','x200','x199','x198','x197','x196','x195','x194','x193','x192','x191','x190','x189','x188','x187','x186','x185','x184','x183','x182','x181','x180'
               ,'x179','x178','x177','x176','x175','x174','x173','x172','x171','x170','x169','x168','x167','x166','x165','x164','x163','x162','x161','x160','x159','x158','x157','x156','x155','x154','x153','x152','x151','x150','x149','x148','x147','x146','x145','x144','x143','x142','x141','x140','x139','x138','x137','x136','x135','x134','x133','x132','x131','x130','x129','x128','x127','x126','x125','x124','x123','x122','x121','x120','x119','x118','x117','x116','x115','x114','x113','x112','x111','x110',
               'x109','x108','x107','x106','x105','x104','x103','x102','x101','x100','x99','x98','x97','x96','x95','x94','x93','x92','x91','x90','x89','x88','x87','x86','x85','x84','x83','x82','x81','x80','x79','x78','x77','x76','x75','x74','x73','x72','x71','x70','x69','x68','x67','x66','x65','x64','x63','x62','x61','x60','x59','x58','x57','x56','x55','x54','x53','x52','x51','x50','x49','x48','x47','x46','x45','x44','x43','x42','x41','x40','x39','x38','x37','x36','x35','x34','x33','x32','x31','x30','x29','x28','x27','x26','x25','x24','x23','x22','x21','x20','x19','x18','x17','x16','x15','x14','x13','x12','x11','x10','x9','x8','x7','x6','x5','x4','x3','x2','x1')
new_names <- as.character(new_names)
colnames(train) <- new_names

train <- cbind(train_continuous,train)
rm(train_continuous)

trainIndex <- caret::createDataPartition(train$transactions, p = 0.75, list = FALSE)
train_set <- train[trainIndex,]
validation_set <- train[-trainIndex,]

rm(train,trainIndex)

#### Models #####

## OLS model ##

my_OLS <- lm(transactions ~ .,data = train_set)

## Random Forest model ##
model <- randomForest(transactions ~ .,
                      data = validation_set, mtry = 5, ntree = 100, importance = TRUE)
model

#Predictions
y<- solo.test[,"winPlacePerc"]

rf.predictions <- predict(model,newdata=solo.test)

temp <- as.vector(y)
temp <- temp$winPlacePerc

mean(abs(temp-rf.predictions))

#### Final Test Predictions ####

test <- read_csv("my_test.csv") %>%
  select(-X1)

# Clean Test Data for Model Predictions
