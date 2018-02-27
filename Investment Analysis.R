###############
## CODE COMPILED IN MAC OS  VERSION 10.13.1, R STUDIO Version 1.1.383, R VERSION: 
# platform       x86_64-apple-darwin15.6.0   
# arch           x86_64                      
# os             darwin15.6.0                
# system         x86_64, darwin15.6.0        
# status                                     
# major          3                           
# minor          4.2                         
# year           2017                        
# month          09                          
# day            28                          
# svn rev        73368                       
# language       R                           
# version.string R version 3.4.2 (2017-09-28)
# nickname       Short Summer
###################

install.packages("stringr")
install.packages("dplyr")
install.packages("tidyr")

library(stringr)
library(dplyr)
library(tidyr)

setwd("/Volumes/Data/DataSciences/Upgrad/Investment Analysis Assignment")

#####################
### CHECK POINT #1
#####################

#import companies, mappings, rounds tables in RStudio
companies <- read.csv(file = "/Volumes/Data/DataSciences/Upgrad/Investment Analysis Assignment/input/companies.txt",sep = '\t',stringsAsFactors = F)
inv_cmpn <- companies

inv_cmpn$category_list <- tolower(inv_cmpn$category_list)
round2 <- read.csv(file = "input/rounds2.csv",sep = ",",stringsAsFactors = F)
inv_r2 <- round2


# Table 1.1: Understand the Data Set 
# 1. How many unique companies are present in rounds2?	

# Data Description:
# Serveral Companies in the Permalink Column have Capital Case, it is now converted to the lower case.
# Convert the Permalink to lower case
# Separate Orgnaization Type from companies into separate columns
# Look for distint companies in the Investment Separated
# Count the number of Companies.

#Method:

inv_r2$company_permalink <- str_to_lower(inv_r2$company_permalink) # to convert lower case
inv_r2_uniq_comp <- inv_r2 %>% separate(company_permalink,into = c("col_sep","comp"),sep = "/organization/") %>% distinct(comp)
count(inv_r2_uniq_comp)



# 2. How many unique companies are present in companies?	       

# # Method:1
# 
# Convert the Permalink to lower case
# Separate the columns compnay column from Organization
# Identify the distinct colummns in comanies table
# Count the distinct the companies


inv_cmpn$permalink <- str_to_lower(inv_cmpn$permalink) # convert to lower case

inv_cmp_distinct <- inv_cmpn %>% 
    separate(col = permalink, into = c("x","comp"), sep = "/organization/") %>% 
  distinct(comp) # Distinct Companies seaprated from companies

count(inv_cmp_distinct)




# 3 .In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.	

# Answer: permalink can be used as unique Key. 

# Check if there are any duplicates in the Company File for Permalink
#inv_cmpn %>% distinct(... = inv_cmpn$name) %>% count()
#inv_cmpn$name[which(duplicated(x = inv_cmpn$name,incomparables = FALSE, fromLast = TRUE))]
colnames(inv_cmpn)[1]



# 4. Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N	 

# Doing an anti_join on the Companies Column Invest Round2 Unique and Distinct records in Companies Permalink file

inv_comp_merge <- anti_join(x = inv_r2_uniq_comp,y = inv_cmp_distinct, by = "comp")

# If there are companies in the merged file, print "Yes", else Print "No" merged files are of Zero length print "Yes" else print "No

if(length(inv_comp_merge$comp) > 0) 
{ paste("Yes")
  } else
  { paste("No")
    }


# 5. Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
# 5.1. Name the merged frame master_frame. How many observations are present in master_frame?
# Answer:

# ANSWER: rename the column of the companies dataframe from "permanlink" to "company_permanlink"

colnames(inv_cmpn)[colnames(inv_cmpn) == "permalink" ] <- "company_permalink"

# merge the columns on "company_permalink" which is the unique key

master_frame <- merge(x = inv_r2, y = inv_cmpn, by.x = "company_permalink", all.x = TRUE)

# count of the number of the records in the master_frame
nrow(master_frame)


##############
#### CODE Analysis for Table 2 in INVESTMENT Excel Spreadsheet
##############


# Table 2.1: Average Values of Investments for Each of these Funding Types 


master_data_grp_by_funding_type <- master_frame %>% group_by(funding_round_type) %>% summarise(avg_funding = mean(raised_amount_usd,na.rm = TRUE))

# Average funding amount of venture type

filter(master_data_grp_by_funding_type, funding_round_type == "venture")


# Average funding amount of angel type

filter(master_data_grp_by_funding_type, funding_round_type == "angel")

# Average funding amount of seed type	 

filter(master_data_grp_by_funding_type, funding_round_type == "seed")

# Average funding amount of private equity type	 

filter(master_data_grp_by_funding_type, funding_round_type == "private_equity")

# Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
# which investment type is the most suitable for it?
#   

# Filter by Raised_amount in the range $5M and $15M.
# Aggregate the date by the raised amount on the Fund_Type.

master_frame_5m_15m <- master_frame %>% 
  filter(between(raised_amount_usd,5000000,15000000)) %>% 
  group_by(fundType = funding_round_type) %>% 
  summarise(total_funding = sum(raised_amount_usd,na.rm = TRUE)) %>%
  arrange(desc(total_funding))


FT <- master_frame_5m_15m$fundType[1]
FT

#############################
### CODE Analysis for Table 3 in INVESTMENT Excel Spreadsheet INVESTMENT.XLSX
############################

#Identify the top three English-speaking countries in the data frame top9.

#MEHTOD: READ THE COUNTRIES FROM THE Countries PDF FILE, CONVERT THEM TO ISO CODES.
# codes of the english speaking countries , codes from ISO country codes
country_code <- c("ATG","BHS","BRB","BLZ","CAN","DMA","GRD","GUY","JAM","KNA","LCA","VCT","TTO","USA","BWA","CMR","ETH","ERI","GMB","GHA","KEN","LSO","LBR","MWI","MUS","NAM","NGA","RWA","SYC","SLE","ZAF","SSD","SDN","SWZ","TZA","UGA","ZMB","ZWE","IND","PAK","PHL","SGP","AUS","FJI","KIR","MHL","FSM","NRU","NZL","PLW","PNG","WSM","SLB","TON","TUV","VUT","IRL","MLT","GBR")

# Create a list of "English" language files
Official_Lang <- rep("English",length(country_code))

#create a data frame of english speaking countries from the list country_code and Official_lang
english_speaking_countries<- data.frame(country_code,Official_Lang,stringsAsFactors = F)

# create a new master frame by merging the countries
master_frame_english_language <- left_join(x=master_frame,y = english_speaking_countries, by.x ="country_code",all.x = "country_code")
head(master_frame_english_language)

# create list of countries funding country-wise
top9_country <- master_frame_english_language %>%  
  filter(FT =="venture") %>% 
  group_by(country_code = country_code) %>% 
  summarise(total_funding = sum(raised_amount_usd,na.rm = TRUE)) %>%
  arrange(desc(total_funding))

#View(top9)

#Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)
top9 <- top9_country[1:10,]
top9

# Removing the missing country, which is assumed as X, the Top9 Countries are:
top9_without_missing_values <- top9[-which(is.na(str_replace(top9$country_code,string = "" , replacement = "X"))),]
top9_without_missing_values <- as.data.frame(top9_without_missing_values)
top9_without_missing_values

#top 3 countries that recieved most of the funding
top_3_countries <- top9_without_missing_values %>% 
  merge(x = top9_without_missing_values,y = english_speaking_countries,by.x = "country_code",by.y = "country_code", all.x = TRUE) %>% 
  filter(Official_Lang == "English") %>% 
  arrange(desc(total_funding))

top_3_countries <- top_3_countries[1:3,1]
top_3_countries

###################################
##### ANALYSIS FOR TABLE 5
###################################

###################################
#  DATA CLEANING FOR SECTOR ANALYSIS
###################################


# Read data from the CSV file
mappings <- read.csv(file = "input/mapping.csv", sep = ",",stringsAsFactors = F,encoding = "UTF-8")
invst_mapping <- mappings

# converting to lower case
invst_mapping$category_list <- tolower(invst_mapping$category_list)

#Looking for Bad_data that contains 0 in the Categories
invst_mapping$category_list[which(str_detect(string = invst_mapping$category_list,pattern = "0"))]

#Replacing the 0 with "na"s
invst_mapping$category_list <- str_replace_all(string = invst_mapping$category_list,pattern = "0",replacement = "na")

#Checking to see if "0" exist
invst_mapping$category_list[which(str_detect(string = invst_mapping$category_list,pattern = "0"))]

# Replace the Enterprise 2.na with Enterprise 2.0
invst_mapping$category_list[which(str_detect(invst_mapping$category_list,pattern = "Enterprise 2.na"))] <- "Enterpise 2.0"


# Correcting the Column names of the mappings.csv which have "."
colnames(invst_mapping) <- str_replace_all(names(invst_mapping), '\\.', "")


# gather data from muliple columns to a single main_sector, remove the columns that do not have 1's
invst_mapping <- gather(invst_mapping,key = main_sector,value = my_val,... = AutomotiveSports:SocialFinanceAnalyticsAdvertising)
invst_mapping <- invst_mapping[!(invst_mapping$my_val == 0),] 
invst_mapping <- invst_mapping[,-length(invst_mapping)] # Remove the unwanted last column


# This step splits the PIPE characater to a dummy " /ORG/ "
# The " /ORG/ " is split upon using sepratate function

master_frame_english_language <- master_frame_english_language %>% mutate(category_list = str_replace_all(category_list, '\\|', " /ORG/ ")) %>% 
  separate(col = category_list,into = c("category_list"),sep = " /ORG/ ")


# merging the new category list with the mappings table containing the main_sector column is added to cat_list table.

master_frame_alldata <- merge(x = master_frame_english_language, y = invst_mapping,by.x = "category_list",all.x = TRUE) 
head(master_frame_alldata)




###################################
#Checkpoint 4: Sector Analysis 1
###################################


#When we say sector analysis, we refer to one of the eight main sectors.

#Also, you know the top three English speaking countries and the most suitable funding type for Spark Funds. 
#Let’s call the three countries 'Country 1', 'Country 2' and 'Country 3' and the funding type 'FT'.



# get the 8-main sectors
# Master_frame with Funding type = venture, and Country Code = USA, GBR, IND
master_frame_alldata_filtered <- master_frame_alldata %>% filter(country_code %in% top_3_countries, funding_round_type %in% FT) 

# get the sectors using groupby
invst_sectors <- master_frame_alldata_filtered %>%
  group_by(sectors = main_sector) %>% 
  summarise(total_funding = sum(raised_amount_usd,na.rm = TRUE))%>% 
  arrange(desc(total_funding))
#View(invst_sectors)

# convert the sectors to list 
invst_sectors <- as.list(invst_sectors)$sectors


# remove the blanks and NA ---- invst_sectors! = removes "Blanks" and is.na removes NAs
invst_sectors <- invst_sectors[invst_sectors!= c("Blanks")]
invst_sectors <- invst_sectors[!is.na(invst_sectors)]

# The eight investment sectors are:
paste(invst_sectors)


###################################
#Checkpoint 5: Sector Analysis 2
###################################

# 
# Now, the aim is to:
#   find out the most heavily invested main sectors in each of the three countries (for funding type FT and investments range of 5-15 M USD).
#   Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type FT falling within the 5-15 million USD range. 
# The three data frames should contain:
#   All the columns of the master_frame along with the primary sector and the main sector

master_frame_top3_8sectors_5m_15m <- master_frame_alldata %>% filter(country_code %in% top_3_countries, funding_round_type %in% FT, main_sector %in% invst_sectors, between(raised_amount_usd, 5000000,15000000) ) 


D1 <- master_frame_top3_8sectors_5m_15m %>% 
  filter(country_code %in% top_3_countries[1])
# #%>%
# group_by(Country = country_code) %>% 
# summarise(Total_no_of_investments = n(),Amount_of_Investment = sum(raised_amount_usd,na.rm = TRUE)) %>%
# arrange(desc(Total_no_of_investments,Amount_of_Investment))

#filter(country_code %in% top_3_countries[1]) %>%
#group_by(fundingcountry_code) %>% 
#summarise(Number_of_Investment = n(), Total_investments = sum(funding_round_code,na.rm = TRUE))


D2 <- master_frame_top3_8sectors_5m_15m %>% 
  filter(country_code %in% top_3_countries [2])

# D2 <- master_frame_alldata_filtered_sectors %>% 
#   filter(country_code %in% top_3_countries[2]) %>%
#   group_by(Country = country_code) %>% 
#   summarise(Total_no_of_investments = n(),Amount_of_Investment = sum(raised_amount_usd,na.rm = TRUE)) %>%
#   arrange(desc(Total_no_of_investments,Amount_of_Investment))


D3 <- master_frame_top3_8sectors_5m_15m %>% 
  filter(country_code %in% top_3_countries[3])



# D3 <- master_frame_alldata_filtered_sectors %>% 
#   filter(country_code %in% top_3_countries[3]) %>%
#   group_by(Country = country_code) %>% 
#   summarise(Total_no_of_investments = n(),Amount_of_Investment = sum(raised_amount_usd,na.rm = TRUE)) %>%
#   arrange(desc(Total_no_of_investments,Amount_of_Investment))

head(D1)  
head(D2)
head(D3)

# The total number (or count) of investments for each main sector in a separate column, by each country:

master_frame_top3_8sectors_5m_15m %>%
  group_by(sector = main_sector, country = country_code) %>% 
  summarise(Total_no_of_investments = n()) %>% 
  arrange(sector, desc(Total_no_of_investments), country )



# The total amount invested in each main sector in a separate column

master_frame_top3_8sectors_5m_15m %>%
  group_by(sector = main_sector) %>% 
  summarise(Total_no_of_investments = sum(raised_amount_usd,na.rm = TRUE)) %>% 
  arrange(desc(Total_no_of_investments),sector)



# Using the three data frames, you can calculate the total number and amount of investments in each main sector.

master_frame_top3_8sectors_5m_15m %>%
  group_by(sector = main_sector) %>% 
  summarise(Total_no_of_investments = n(),Amount_of_Investment = sum(raised_amount_usd,na.rm = TRUE)) %>%
  arrange(desc(Total_no_of_investments,Amount_of_Investment)) 








#####################################
# Questions	                                      C1	C2	C3
#####################################



# Total number of Investments (count)			


master_frame_top3_8sectors_5m_15m %>%
  group_by(country = country_code) %>% 
  summarise(Total_no_of_investments = n()) %>% 
  arrange(desc(Total_no_of_investments)) %>%
  t()



# Total amount of investment (USD)
master_frame_top3_8sectors_5m_15m %>%
  group_by(country = country_code) %>% 
  summarise(Amount_of_Investment = sum(raised_amount_usd,na.rm = TRUE)) %>%
  arrange(desc(Amount_of_Investment)) %>%
  t()

# Top Sector name (no. of investment-wise)	
# Second Sector name (no. of investment-wise)			
# Third Sector name (no. of investment-wise)

# Number of investments in top sector (3)			
# Number of investments in second sector (4)			
# Number of investments in third sector (5)			

# C1= USA
D1 %>%
  group_by(sector = main_sector ,country = country_code) %>% 
  summarise(Total_no_of_investments = n()) %>% 
  arrange(desc(Total_no_of_investments)) #%>%t()

# C2 = GBR
D2 %>%
  group_by(sector = main_sector ,country = country_code) %>% 
  summarise(Total_no_of_investments = n()) %>% 
  arrange(desc(Total_no_of_investments)) #%>%t()

# C3 = IND
D3 %>%
  group_by(sector = main_sector ,country = country_code) %>% 
  summarise(Total_no_of_investments = n()) %>% 
  arrange(desc(Total_no_of_investments)) #%>%t()




# For point 3 (top sector count-wise), which company received the highest investment?			
# For point 4 (second best sector count-wise), which company received the highest investment?			

# USA

D1 %>%
  filter(main_sector %in% c("Others","SocialFinanceAnalyticsAdvertising")) %>%
  group_by(company = company_permalink, sector = main_sector) %>% 
  summarise(Investments = sum(raised_amount_usd,na.rm = TRUE)) %>% 
  arrange(desc(Investments)) 

#GBR
D2 %>%
  filter(main_sector %in% c("Others","SocialFinanceAnalyticsAdvertising")) %>%
  group_by(company = name, sector = main_sector) %>% 
  summarise(Investments = sum(raised_amount_usd,na.rm = TRUE)) %>% #top_n(3 ,wt = company) %>% 
  arrange(desc(Investments),company,sector) 

#IND
options(scipen=999) # removes exponents

D3 %>%
  filter(main_sector %in% c("Others")) %>%
  group_by(company = name, sector = main_sector) %>% 
  summarise(Investments = sum(raised_amount_usd,na.rm = TRUE)) %>% 
  arrange(desc(Investments),company,sector)


D3 %>%
  filter(main_sector %in% c("SocialFinanceAnalyticsAdvertising")) %>%
  group_by(company = name, sector = main_sector) %>% 
  summarise(Investments = sum(raised_amount_usd,na.rm = TRUE)) %>% 
  arrange(desc(Investments),company,sector)




