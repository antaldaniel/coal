# RENEWABLES
# EUROBAROMETER
# Milos Popovic 26/2/2020

# CODEBOOK

#"country" = country numeric, 
#"iso3" = ISO3, 
#"nuts2cz" = NUTS2 for Czechia;
		     #1 = Praha (Prague)	
			 #2 = Stredni Cechy (Central Behemia)
			 #3 = Jihozapad (Southwest)
			 #4 = Severozapad (Northwest)
			 #5 = Severovychod (Northeast)
			 #6 = Jihovychod (Southeast)
			 #7 = Stredni Morava (Central Moravia)
			 #8 = Moravskoslezsko (Moravian-Silesian Region)
			 #99 = NA
#"nuts2hu" = NUTS2 for Hungary,
			 #1 = Kozep-Magyarorszag (Central Hungary)
			 #2 = Eszak-Magyarorszag (North Hungary)
			 #3 = Eszak-Alfold (North Great Plain)
			 #4 = Del-Alfold (South Great Plain)
			 #5 = Del-Dunantul (South Transdanubia)
			 #6 = Kozep-Dunantul (Central / North Transdanubia)
			 #7 = Nyugat-Dunantul (West Transdanubia)
			 #99 = NA
#"nuts2pl" = NUTS2 for Poland,
			 #1 = Dolnoslaskie (Lower Silesian)
			 #2 = Kujawsko-Pomorskie (Kuyavian-Pomerania)
			 #3 = Lubelskie (Lublin)
			 #4 = Lubuskie (Lubusz)
			 #5 = Lodzkie (Lodz)
			 #6 = Malopolskie (Malopolska)
			 #7 = Mazowieckie (Masovian)
			 #8 = Opolskie (Opole)
			 #9 = Podkarpackie
			 #10 = Podlaskie
			 #11 = Pomorskie (Pomeranian)
			 #12 = Slaskie (Silesian)
			 #13 = Swietokrzyskie
			 #14 = Warminsko-Mazurskie (Warmian-Masurian)
			 #15 = Wielkopolskie (Wielkopolska / Greater Poland)
			 #16 = Zachodniopomorskie (West Pomeranian)
			 #99 = NA
#"nuts2sk" = NUTS2 for Slovakia
             #1 = Bratislavsky kraj (Bratislava Region)
			 #2 = Zapadne Slovensko (Western Slovakia)
			 #3 = Stredne Slovensko (Central Slovakia)
			 #4 = Vychodne Slovensko (Eastern Slovakia)
			 #99 = NA
#"pubsupprenew" = More public financial  support should be given to  the transition to clean  energies even if it means  subsidies to fossil fuels should be reduced);
                  # 1 = Totally agree
				  # 2 = Tend to agree
				  # 3 = Tend to disagree
				  # 4 = Totally disagree
				  # 5 = DK 
#"renewby2030" = How important do you think it is that the (NATIONALITY) government sets ambitious targets to increase the amount of renewable energy used, such as wind or solar power, by 2030?;
				  # 1 = Very important
				  # 2 = Fairly important
				  # 3 = Not very important
				  # 4 = Not at all important
				  # 5 = DK 
#"age" = exact age (numeric), 
#"ideology3cat" = left/center/right; 1 = (1 - 4) Left, 2 = (5 - 6) Centre, 3 = (7 -10) Right, 9 = DK/Refusal
#"gender" = male (1) or female (2), 
#"eduage" = years of education, 0 = Refusal, 2 = 2 years, 97 = No full-time education, 98 = Still studying, 99 = DK
#"employed" = self-employed/employed/unemployed; 1 = Self-employed (5-9 in d15a), 2 = Employed (10-18 in d15a), 3 = Not working (1-4 in d15a)
#"communitysize = urban/rural area"; 1 = Rural area, 2 = Towns and suburbs/small urban area, 3 = Cities/large urban area
#"housesize" = household composition, number of individuals aged 15 or more (exact number), 
#"eu25weight" = weights for EU25

setwd("C:/Users/milos.agathon/Dropbox/Energy security project/Eurobarometer")

# SELECT VARS
# Eurobarometer 2019

eb19 <- read.csv("C:/Users/milos.agathon/Dropbox/Energy security project/Eurobarometer/eb91.3_ZA7572/v4_2019.csv")
eb19b <- eb19[,c("country", "isocntry", "nuts","qb4_4", "qb7", "d11", "d1r1", "d10", "d8", "d15a_r1", "d25", "d40a", "w14")]
names(eb19b) <- c("country", "iso3", "NUTS_ID", "pubsupprenew", "renewby2030", "age", "ideology3cat", "gender", "eduage", "employed", "communitysize", "housesize", "eu25weight")
write.csv(file="eb19.csv", eb19b)

# Eurobarometer 2018

eb18 <- read.csv("C:/Users/milos.agathon/Dropbox/Energy security project/Eurobarometer/eb90.2_ZA7488/v4_2018.csv")
eb18b <- eb18[,c("country", "isocntry", "nuts", "qb5_5", "d11", "d1r1", "d10", "d8", "d15a_r1", "d25", "d40a", "w14")]
names(eb18b) <- c("country", "iso3", "NUTS_ID", "pubsupprenew", "age", "ideology3cat", "gender", "eduage", "employed", "communitysize", "housesize", "eu25weight")
write.csv(file="eb18.csv", eb18b)

# Eurobarometer 2017

eb17 <- read.csv("C:/Users/milos.agathon/Dropbox/Energy security project/Eurobarometer/eb87.1_ZA6861/v4_2017.csv")
eb17b <- eb17[,c("country", "isocntry", "nuts","p7cz", "p7hu", "p7pl", "p7sk", "qc4_5", "qc7", "d11", "d1r1", "d10", "d8", "d15a_r1", "d25", "d40a", "w14")]
names(eb17b) <- c("country", "iso3", "NUTS_ID","nuts2cz", "nuts2hu","nuts2pl","nuts2sk", "pubsupprenew", "renewby2030", "age", "ideology3cat", "gender", "eduage", "employed", "communitysize", "housesize", "eu25weight")
write.csv(file="eb17.csv", eb17b)

# Eurobarometer 2015

eb15 <- read.csv("C:/Users/milos.agathon/Dropbox/Energy security project/Eurobarometer/eb83.4_ZA6595/v4_2015.csv")
eb15b <- eb15[,c("country", "isocntry","nuts", "qa7", "d11", "d1r1", "d10", "d8", "d15a_r1", "d25", "d40a", "w14")]
names(eb15b) <- c("country", "iso3","NUTS_ID", "renewby2030", "age", "ideology3cat", "gender", "eduage", "employed", "communitysize", "housesize", "eu25weight")
write.csv(file="eb15.csv", eb15b)

# Eurobarometer 2013

eb13 <- read.csv("C:/Users/milos.agathon/Dropbox/Energy security project/Eurobarometer/eb80.2_ZA5877/v4_2013.csv")
eb13b <- eb13[,c("country", "isocntry", "nuts", "qa7", "d11", "d10", "d8", "d15a_r1", "d25", "d40a", "w14")]
names(eb13b) <- c("country", "iso3", "NUTS_ID", "renewby2030", "age", "gender", "eduage", "employed", "communitysize", "housesize", "eu25weight")
write.csv(file="eb13.csv", eb13b)