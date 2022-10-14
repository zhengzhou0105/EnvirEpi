## MTJ2W Data Cleaning
## Author: Zheng Zhou
## Date: Feb 27 2022
## Update: Apr 28 2022

# Global Settings-------
seed <- 47405
source("MTJ2W Utility Functions.r")

# Input Data----------
# Input Raw Data: coach and client assessment
df_20to21_client <- read.csv("MTJ2W+Client+Assessment_February+23,+2022_08.44.csv")
df_20to21_coach <- read.csv("MTJ2W+Coach+Measures_February+23,+2022_08.44.csv")

# Input Raw Data from Katie
load("CleanMJ2WAssessment.rdata")
load("CleanData3.rdata")
load("CleanData6.rdata")
load("CleanDataInitial.rdata")
load("CleanDataPre.rdata")

# Input Clean Data
load("MTJ2W Clean Data.rdata")


# Clean Coach and Client dataset-------
# Remove Double Headers
# Qualtrics recorded two rows for headers. Keep only the first row.
df_20to21_client <- df_20to21_client %>% filter(
  !row_number() == 1      # first row auto read as header in R. Second row became row 1 in R.
)

df_20to21_coach <- df_20to21_coach %>% filter(
  !row_number() == 1    # first row auto read as header in R. Second row became row 1 in R.
)

# Rename Key Variables
df_20to21_client <- df_20to21_client %>% rename(
  Client = QID384_1,       # client names
  ID =  QID384_6,           # Cohort ID
  Coach = QID384_7,         # Coach
  Site = QID386,            # Site
  Time = QID385,             # Timing or type of visit
  # Motivation Scales
  BREQidtif1 = QID135687571_1,
  BREQamot1 = QID135687571_2,
  BREQintrin1 = QID135687571_3,
  BREQintroj1 = QID135687571_4,
  BREQintegr1 = QID135687571_5,
  BREQext1 = QID135687571_6,
  BREQidtif2 = QID135687574_1,
  BREQamot2 = QID135687574_2,
  BREQintrin2 = QID135687574_3,
  BREQintroj2 = QID135687574_4,
  BREQintegr2 = QID135687574_5,
  BREQext2 = QID135687574_6,
  BREQidtif3 = QID135687572_1,
  BREQamot3 = QID135687572_2,
  BREQintrin3 = QID135687572_3,
  BREQintroj3 = QID135687572_4,
  BREQintegr3 = QID135687572_5,
  BREQext3 = QID135687572_6,
  BREQidtif4 = QID135687575_1,
  BREQamot4 = QID135687575_2,
  BREQintrin4 = QID135687575_3,
  BREQintroj4 = QID135687575_4,
  BREQintegr4 = QID135687575_5,
  BREQext4 = QID135687575_6
)

df_20to21_coach <- df_20to21_coach %>% rename(
  Client = QID342_1,       # client names
  ID = QID342_5,            # Cohort ID
  Coach = QID342_4,         # Coach
  Site = QID343,            # Site
  Time = QID375            # Timing or type of visit
)

# Text Cleaning for String Variables

df_20to21_client <- df_20to21_client %>% mutate(
  Client = str_to_title(Client), # Names, capitalize first letter
  Coach = str_to_title(Coach), # Names, capitalize first letter
  ID = str_to_upper(ID)        # ID, all caps
)
df_20to21_coach <- df_20to21_coach %>% mutate(
  Client = str_to_title(Client), # Names, capitalize first letter
  Coach = str_to_title(Coach),   # Names, capitalize first letter
  ID = str_to_upper(ID)         # ID, all caps
)

# Client name correction
# "Lavalley" in client 95 = "Vicki Lavalley" 129, 
# "Margaret" in coach 139 = "Margaret Leichtnam" 132
df_20to21_client$Client[95] <- "Vicki Lavalley"
df_20to21_coach$Client[139] <- "Margaret Leichtnam"

# Coach name correction
# "Janie " in client 134, in coach 40,104,109
# "\nJanie" in client 83
df_20to21_client$Coach[c(134,83)] <- "Janie"
df_20to21_coach$Coach[c(40,104,109)] <- "Janie"


# Check Key Variables

# ID
unique(df_20to21_client$ID)
unique(df_20to21_coach$ID)


# Number of clients
unique(df_20to21_client$Client)
unique(df_20to21_coach$Client)

# Number of coaches
unique(df_20to21_client$Coach)
unique(df_20to21_coach$Coach)

# Check if Coach matches between two datasets
df_coachchk_1 <-  df_20to21_client[,c("Client","Coach","Time")]
df_coachchk_2 <-  df_20to21_coach[,c("Client","Coach","Time")]
df_coachchk <- full_join(df_coachchk_1,df_coachchk_2,"Client")
sum(df_coachchk$Coach.x != df_coachchk$Coach.y,na.rm = T)



# Concatenate Katie's files----
# Check Time across datasets
# CleanPre1 and CleanPre1 no Time variable. Check with initial data
table(CleanClient6month$Time)
table(CleanCoach6month$Time)
table(cleaninitial$Time)
table(ClientClean3month$Time)
table(CoachClean3month$Time)
table(MJ2WClientAssessment$Time)
table(MJ2WCoachAssessment$Time)

# Reorganize datasets by Time

# remove QUALTRICS info
QUALinfo <- c(1:17)

# data with unknown time
df_timeunknown_init <- cleaninitial %>% filter(
  is.na(Time)
) %>% select(-all_of(QUALinfo))

df_timeunknown_6mo <- CleanClient6month %>% filter(
  Time == ""
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = NA
)

df_unknown <- bind_rows(df_timeunknown_init,df_timeunknown_6mo)

# data at initial
df_init_initial <- cleaninitial %>% filter(
  Time ==  "Pre-Initial"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "Initial",
  data = "cleaninitial",
  .before = Time
)

df_init_clientass <- MJ2WClientAssessment %>% filter(
  Time ==  "Initial"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "Initial",
  data = "ClientAssessment",
  .before = Time
)

df_init_coachass <- MJ2WCoachAssessment %>% filter(
  Time == "Initial Coach"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "Initial",
  data = "CoachAssessment",
  .before = Time
)

df_init_pre1 <- CleanPre1 %>% mutate(
  Time = "Initial",
  data = "Pre1",
  .before = Time
) %>% select(-all_of(QUALinfo))
# two typo in ID: \nCU4C22  and  eienvn
# Fix CU4C22, drop eienvn
df_init_pre1$ID[which(df_init_pre1$ID == "\nCU4C22")] <- "CU4C22"
df_init_pre1 <- df_init_pre1 %>% filter(
  ID != "eienvn"
)

df_init_pre2 <- CleanPre2 %>% mutate(
  Time = "Initial",
  data = "Pre2",
  .before = Time
) %>% select(-all_of(QUALinfo)) %>% filter(
  ID != "Test"
)

# Sort by client and coach dataset

df_init_client <- bind_rows(
  df_init_clientass,
  df_init_initial,
) %>% arrange(ID)

unique(df_init_client$ID)

df_init_coach <- bind_rows(
  df_init_coachass,
  df_init_pre2
) %>% arrange(ID)

unique(df_init_coach$ID)

# join the data to estimate sample size
df_init_clean <- full_join(df_init_client,df_init_coach,
                           by = "ID") %>% arrange(ID)


# data at 3 month

df_3mo_initial <- cleaninitial %>% filter(
  Time == "Post-3 months"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "3 Month",
  data = "cleaninitial",
  .before = Time
)

df_3mo_client3mo <- ClientClean3month %>% filter(
  Time == "Post-3 months"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "3 Month",
  data = "client3mo",
  .before = Time
)

df_3mo_coach3mo <- CoachClean3month %>% filter(
  Time == "Post-3 months"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "3 Month",
  data = "coach3mo",
  .before = Time
)

df_3mo_coach6mo <- CleanCoach6month %>% filter(
  Time == "Post-3 months"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "3 Month",
  data = "coach6mo",
  .before = Time
)

df_3mo_clientass <- MJ2WClientAssessment %>% filter(
  Time == "3-months"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "3 Month",
  data = "clientAssessment",
  .before = Time
)

df_3mo_coachass <- MJ2WCoachAssessment %>% filter(
  Time == "3-month Coach"
) %>% select(-all_of(QUALinfo)) %>% mutate(
  Time = "3 Month",
  data = "coachAssessment",
  .before = Time
)

df_3mo_client <- bind_rows(
  df_3mo_initial,
  df_3mo_client3mo,
  df_3mo_clientass
) %>% arrange(ID)

unique(df_3mo_client$ID)    # 116,  one duplicate
df_3mo_client %>% group_by(ID) %>% summarise(n=n()) %>% pull(n)
df_3mo_client[which(df_3mo_client$ID == "CU6C06"),]  # row 64 drop

df_3mo_client <- df_3mo_client %>% slice(-64)

df_3mo_coach <- bind_rows(
  df_3mo_coach3mo,
  df_3mo_coach6mo,
  df_3mo_coachass
) %>% arrange(ID) %>% relocate(
  ID,data,
  .before = NULL
)
  
unique(df_3mo_coach$ID)

# Preliminary on Coach/client Assessment datasets-----
df_init_client <- MJ2WClientAssessment %>% filter(
  Time == "Initial"
)

df_3mo_client <- MJ2WClientAssessment %>% filter(
  Time == "3-months"
)

df_6mo_client <- MJ2WClientAssessment %>% filter(
  Time == "6-months"
)

df_1yr_client <- MJ2WClientAssessment %>% filter(
  Time == "1 year"
)

df_init_coach <- MJ2WCoachAssessment %>% filter(
  Time == "Initial Coach"
)

df_3mo_coach <- MJ2WCoachAssessment %>% filter(
  Time == "3-month Coach"
)

df_6mo_coach <- MJ2WCoachAssessment %>% filter(
  Time == "6-month Coach"
)

df_1yr_coach <- MJ2WCoachAssessment %>% filter(
  Time == "1-year Coach"
)

write.csv(df_init_client,
          "Preliminary_initial_client.csv",
          row.names = F)

write.csv(df_init_coach,
          "Preliminary_initial_coach.csv",
          row.names = F)

write.csv(df_3mo_client,
          "Preliminary_3month_client.csv",
          row.names = F)

write.csv(df_3mo_coach,
          "Preliminary_3month_coach.csv",
          row.names = F)

write.csv(df_1yr_client,
          "Preliminary_1year_client.csv",
          row.names = F)

write.csv(df_1yr_coach,
          "Preliminary_1year_coach.csv",
          row.names = F)

# Analysis on preliminary data

# PA
df_PA_prelim <- read.csv("MTJ2W preliminary health measures Apr 29 PA.csv")
t.test(df_PA_prelim$Initial,df_PA_prelim$X3month)
t.test(df_PA_prelim$Initial,df_PA_prelim$X1year)

# RAI
df_RAI_prelim <- read.csv("MTJ2W preliminary health measures Apr 29 RAI.csv")
t.test(df_RAI_prelim$Initial,df_RAI_prelim$X3month)
t.test(df_RAI_prelim$Initial,df_RAI_prelim$X1year,"less")

# Output Clean Data-------
df_20to21_client_clean <- df_20to21_client
df_20to21_coach_clean <- df_20to21_coach

save(
  # df_20to21_client_clean,df_20to21_coach_clean,
  df_init_client,df_init_coach,
  df_3mo_client,df_3mo_coach,
  file = "MTJ2W Clean Data.Rdata"
)

save(
  df_init_client,df_init_coach,
  df_3mo_client,df_3mo_coach,
  df_6mo_client,df_6mo_coach,
  df_1yr_client,df_1yr_coach,
  file = "MTJ2W Preliminary Data.Rdata"
)
