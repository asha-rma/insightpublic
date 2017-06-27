t1 = Sys.time()
library(geosphere)

### Load visits data
setwd("C:/Asha/High/Insight/Project/intermediate_outputs/")
visits = read.csv("visits.csv")
names(visits)
visits$unique_id = as.character(visits$unique_id)
visits$Real_Date = as.Date(as.character(visits$Real_Date), format = "%Y-%m-%d")


### Load budget data
budget = read.csv("budget.csv")
budget$unique_id = as.character(budget$unique_id)
budget$Real_Date = as.Date(as.character(budget$Real_Date), format = "%Y-%m-%d")

### unique_id cleanup step 1 - remove trailing hyphens
visits$unique_id_2 = visits$unique_id
endhyphen = sapply(visits$unique_id, FUN = function(x){y = substr(x, nchar(x), nchar(x)); y == "-"})
length(endhyphen); length(which(endhyphen == T))
visits$unique_id_2[which(endhyphen == T)] = sapply(visits$unique_id[which(endhyphen == T)], 
                                                   FUN = function(x){y = substr(x, 1, (nchar(x) - 1))})


### Take out A2 (number of services received in quarter field from dataset to see
# which is actual first service received
visits_no_a2 = visits[-which(visits$field == "number_of_services_received_in_quarter"),]

### Number of rows by each SW
y = aggregate(field~unique_id_2, data = visits_no_a2, FUN = length)
summary(y$field)
table(y$field)
hist(y$field[which(y$field<=66)])

# See what's happening with SW who have very high number of rows
thresh = 49
sw_freq = y$unique_id_2[which(y$field == thresh)]
sw_freq
i = 1
sort(table(visits_no_a2$Real_Date[which(visits_no_a2$unique_id_2 == sw_freq[i])]), decreasing = T)
oddday = subset(visits_no_a2, visits_no_a2$unique_id_2 == sw_freq[i] 
                & Real_Date == as.Date("2016-07-18"))
oddday[order(oddday$field), c("field", "val", "peer_educator_id", "site", "district")]
table(as.character(oddday$peer_educator_id))

### See how many SW "visiting" more than one site per day
site_rep_byday = aggregate(site~unique_id_2 + Real_Date, data = visits_no_a2, FUN = function(x){length(unique(x))})
table(site_rep_byday$site)
thresh = 3
site_freq = unique(site_rep_byday$unique_id_2[which(site_rep_byday$site == thresh)])
site_freq
i = 1
site_rep_byday1 = subset(site_rep_byday, unique_id_2 == site_freq[i] & site > 1)
site_rep_byday2 = subset(site_rep_byday, unique_id_2 == site_freq[i])
nrow(site_rep_byday1);nrow(site_rep_byday2)
dts = site_rep_byday1$Real_Date[order(site_rep_byday1$site, decreasing = T)]
dts
oddday = subset(visits_no_a2, visits_no_a2$unique_id_2 == site_freq[i] 
                & Real_Date == dts[2])
oddday[order(oddday$field), c("field", "val", "peer_educator_id", "site", "district")]
table(as.character(oddday$peer_educator_id))
### For all SW with 3+ visits to different sites, reassign to 
# 1. most frequent site that day within X km of original site
# 2. (if above is NA) majority site across all visits

### Find distances between subdistricts (sites_mapped not good)
setwd("C:/Asha/High/Insight/Project/data_files/zenysis_data/")
site_long = aggregate(SiteLon~site, data = visits_no_a2, unique)
names(site_long)[2] = "longitude"
site_lat = aggregate(SiteLat~site, data = visits_no_a2, unique)
names(site_lat)[2] = "latitude"
site_coord = merge(site_long, site_lat, by = "site")
length(unique(as.character(visits_no_a2$site))); nrow(site_coord)
distmatrix = matrix(ncol = nrow(site_coord), nrow = nrow(site_coord))
for (i in 1:nrow(site_coord))
{
  for (j in 1:nrow(site_coord))
  {
    distance = distHaversine(site_coord[i, c("longitude", "latitude")], 
                             site_coord[j, c("longitude", "latitude")])
    distmatrix[i, j] = round(distance/1000, 0)
  }
}
distmatrix = as.data.frame(distmatrix)
names(distmatrix) = site_coord$site
row.names(distmatrix) = site_coord$site
head(distmatrix)
tail(distmatrix)
distthresh = 15
nearby = data.frame(homesite = character(), neighbor = character())
for(i in names(distmatrix))
{
  neighbors = try(row.names(distmatrix)[intersect(which(distmatrix[,i] >0), which(distmatrix[,i] < distthresh))])
  if (length(neighbors)>0)
  {
    for (j in neighbors)
    {
      nearby = rbind(nearby, data.frame(homesite = i, neighbor = j))  
    }
  }
}
nearby


### unique_id(_2) cleanup step 2: exclude unique_id_2 == "" or "23"
sw = sort(unique(visits$unique_id_2)); length(sw)
sw = sw[-which(sw %in% c("", "23"))]; length(sw)

### Clean up visits data with worst unique_id_2
dim(visits_no_a2)
visits_no_a2 = visits_no_a2[which(visits_no_a2$unique_id_2 %in% sw),]
dim(visits_no_a2)

### Clean up visits with other repeating sites
site_rep = aggregate(site~unique_id_2, data = visits_no_a2, FUN = function(x){length(unique(x))})
names(site_rep)[which(names(site_rep) == "site")] = "numsites" 
sum(table(site_rep$numsites)); length(unique(visits_no_a2$unique_id_2))
table(site_rep$numsites)
dim(site_rep)

sw_homesite = aggregate(site~unique_id_2, data = visits_no_a2, 
                        FUN = function(x){y1 = table(x);
                        y2 = sort(y1, decreasing = T);
                        if(length(y2) == 1)
                        {
                          y3 = names(y2)
                        } else
                        {
                          if (y2[1]>y2[2])
                          {
                            y3 = names(y2)[1]
                          } else
                          {
                            y3 = "No_clear_homesite"
                          }
                        }
                        })
names(sw_homesite)[which(names(sw_homesite) == "site")] = "homesite"
dim(sw_homesite)
site_rep = merge(site_rep, sw_homesite, by = "unique_id_2")
dim(site_rep); head(site_rep)
table(site_rep$numsites[which(site_rep$homesite == "No_clear_homesite")])
sw_nohome = site_rep$unique_id_2[which(sw_homesite$homesite == "No_clear_homesite")]
length(sw_nohome)

# Exclude SWs with no clear homesite
site_rep_havehome = site_rep[which(site_rep$unique_id_2 %in% sw_nohome == F),]
dim(site_rep); dim(site_rep_havehome)

visits_havehome = visits_no_a2[which(visits_no_a2$unique_id_2 %in% sw_nohome == F),]
dim(visits_no_a2); dim(visits_havehome)

dim(visits_havehome)
visits_havehome = merge(visits_havehome, site_rep_havehome, by = "unique_id_2", all.x = T, all.y = F)
dim(visits_havehome)


# Select SWs with just one associated site
visits_onesite = visits_havehome[which(visits_havehome$numsites == 1),]
dim(visits_no_a2); dim(visits_onesite)

# Select SWs with multiple associated sites
sw_manysites = unique(site_rep$unique_id_2[which(site_rep$numsites>1)])
table(site_rep_havehome$numsites[which(site_rep_havehome$unique_id_2 %in% sw_manysites)])
visits_manysites = visits_havehome[which(visits_havehome$numsites > 1),]
dim(visits_manysites); dim(visits_havehome)
visits_manysites$closetohome = NA
visits_manysites$closetohome[which(visits_manysites$site %in% nearby$homesite == F)] = "Far"
for (i in which(is.na(visits_manysites$closetohome)))
{
  homesite = visits_manysites$homesite[i]
  if (visits_manysites$site[i] %in% nearby$neighbor[which(nearby$homesite == homesite)])
  {
    visits_manysites$closetohome[i] = "Near"
  } else
  {
    visits_manysites$closetohome[i] = "Far"
  }
}
nrow(visits_manysites); length(which(visits_manysites$closetohome == "Far")); length(which(visits_manysites$closetohome == "Near"))

length(unique(as.character(visits_no_a2$site)))
length(unique(as.character(visits_manysites$site)))
length(unique(as.character(visits_manysites$homesite)))
unique(as.character(visits_manysites$site))
unique(as.character(nearby$homesite))       


# Find PE by Site
pe_site = aggregate(site~peer_educator_id, data = visits, FUN = function(x){y1 = table(x);
y2 = sort(y1, decreasing = T);
if(length(y2) == 1)
{
  y3 = names(y2)
} else
{
  if (y2[1]>y2[2])
  {
    y3 = names(y2)[1]
  } else
  {
    y3 = "No_clear_pe_site"
  }
}                                                                            
})
pe_site
pe_site$peer_educator_id[which(pe_site$site == "No_clear_pe_site")]


# Find PE by SW
pe_sw = aggregate(peer_educator_id~unique_id_2, data = visits, FUN = function(x){y1 = table(x);
y2 = sort(y1, decreasing = T);
if(length(y2) == 1)
{
  y3 = names(y2)
} else
{
  if (y2[1]>y2[2])
  {
    y3 = names(y2)[1]
  } else
  {
    y3 = "No_clear_pe"
  }
}                                                                            
})
head(pe_sw)
length(which(pe_sw$peer_educator_id == "No_clear_pe"))


### Remove duplicate visits (CHECK try to do by taking into account main PE)
# First for SWs associated only with single site: CHECK quick run with crude cleaning, see below
rm(r)
r = match(visits_onesite$unique_id_2, pe_sw$unique_id_2)
visits_onesite$main_pe = pe_sw$peer_educator_id[r]

visits_onesite$closetohome = NA
repord_onesite = order(visits_onesite$unique_id_2, visits_onesite$Real_Date, visits_onesite$field)
visits_onesite = visits_onesite[repord_onesite,]
visits_onesite[1:10, c("unique_id_2", "Real_Date", "field")]

# CHECK: may want to add step checking if peer educator is the main one
# CHECK don't have to include site here because all these are at the same site
# Will need to include site for visits_manysites
dup = duplicated(visits_onesite[, c("unique_id_2", "Real_Date", "field")])
visits_onesite$dup = dup
nrow(visits_onesite); length(dup); length(which(dup == T))
visits_one_nodup = visits_onesite[-which(dup == T),]
nrow(visits_one_nodup); nrow(visits_onesite)

# Find visit interval
interval_raw = append(as.numeric(diff(visits_one_nodup$Real_Date)), NA)
# But this includes intervals between visits by different SW too
# So set to NA all these spurious intervals
finddiffsw = rle(visits_one_nodup$unique_id_2)
index_diffsw = cumsum(finddiffsw$lengths)
diffsw = rep(F, nrow(visits_one_nodup))
diffsw[index_diffsw] = T
interval_act = interval_raw
interval_act[which(diffsw == T)] = NA
hist(as.numeric(interval_act))
summary(as.numeric(interval_act))

# Calculate proportion who do return
length(unique(visits_one_nodup$unique_id_2[which(interval_act>0)]))/length(unique(visits_one_nodup$unique_id_2))

visits_1site_int = cbind(visits_one_nodup, interval_act)
returns = visits_one_nodup$unique_id_2[which(interval_act>0)]
# First look at people who don't return
visits_1site_1visit = visits_1site_int[which(visits_1site_int$unique_id_2 %in% returns == F),]
visits_1site_1visit$censtime = 0
visits_1site_1visit$stime = max(visits_1site_int$Real_Date) - visits_1site_1visit$Real_Date
visits_1site_1visit = visits_1site_1visit[order(visits_1site_1visit$unique_id_2, visits_1site_1visit$Real_Date),]
dup4 = duplicated(visits_1site_1visit[, c("unique_id_2")])
visits_1site_1visit_unique = visits_1site_1visit[which(dup4 == F),]
dim(visits_1site_1visit); dim(visits_1site_1visit_unique)

# People who do return
visits_1site_mulvisit = visits_1site_int[which(visits_1site_int$unique_id_2 %in% returns == T),]
# Time from first to second visit
return_sw = aggregate(interval_act~unique_id_2, data = visits_1site_mulvisit, FUN = function(x){
  y1 = x[which(is.na(x) == F)]
  y2 = y1[which(y1>0)]
  y3 = y2[1]
})
dim(return_sw)

# Very crude trimming
dup2 = duplicated(visits_1site_mulvisit[, c("unique_id_2")])
visits_1site_mulvisit_unique = visits_1site_mulvisit[which(dup2 == F),]
visits_1site_mulvisit_unique$censtime = 1
rm(r)
r = match(visits_1site_mulvisit_unique$unique_id_2, return_sw$unique_id_2)
visits_1site_mulvisit_unique$stime = return_sw$interval_act[r]
rm(r)
dim(visits_1site_mulvisit); dim(visits_1site_mulvisit_unique)

visits_1site_unique = rbind(visits_1site_1visit_unique, visits_1site_mulvisit_unique)



### Make sure correct first date and quarter number
# Could also do via aggregate
# first = aggregate(Real_Date~unique_id_2, data = visits, FUN = min)
# Check that it's correct
temp = visits_havehome[order(visits_havehome$unique_id_2, visits_havehome$Real_Date),]
dup3 = duplicated(temp[, "unique_id_2"])
temp2 = temp[which(dup3 == F),]
dim(visits_havehome); dim(temp2)
temp3  = data.frame(unique_id_2 = temp2$unique_id_2, first_date = temp2$Real_Date, first_quart = temp2$qnum)

dim(visits_1site_unique)
visits_1site_unique = merge(visits_1site_unique, temp3, by = "unique_id_2", all.x = T, all.y = F)


### Extract birth date
# Find numerical values in unique id
temp_dt1 = strsplit(visits_1site_unique$unique_id_2, "[^0-9]+")
temp_dt1
# Check distribution of number lengths
numl = unlist(lapply(temp_dt1, FUN = nchar))
summary(numl); table(numl) # Most of the non-null values are 4 digits in length
hist(numl, 40)

# Get first year mentioned in unique id
temp_dt2 = lapply(temp_dt1, FUN = function(x){y1 = nchar(x);
y2 = which(y1 ==4);
y3 = x[y2[1]]; y3})
head(temp_dt2); length(temp_dt2); nrow(visits_1site_unique)
length(which(is.na(unlist(temp_dt2)))) #Number of NAs
sw_byear1 = as.numeric(unlist(temp_dt2))

# Get second year mentioned in unique id
temp_dt3 = lapply(temp_dt1, FUN = function(x){y1 = nchar(x);
y2 = which(y1 ==4);
y3 = x[y2[2]]; y3})              
head(temp_dt3); length(temp_dt3); nrow(visits_1site_unique)
length(which(is.na(unlist(temp_dt3)))) #Number of NAs
sw_byear2 = as.numeric(unlist(temp_dt3))

# If two years are mentioned, take average
sw_byear = sw_byear1
sw_byear[which(is.na(sw_byear2) == F)] = 
  (sw_byear1[which(is.na(sw_byear2) == F)] + 
     sw_byear2[which(is.na(sw_byear2) == F)])/2
length(sw_byear); nrow(visits_1site_unique)
summary(sw_byear, na.rm = T)
sw_byear[union(which(sw_byear < 1962), which(sw_byear>2005))] = NA
length(sw_byear); length(which(is.na(sw_byear))); length(which(is.na(sw_byear) == F))

visits_1site_unique$sw_birthyr = sw_byear

# Identify females 
temp_sx_fe1 = lapply(visits_1site_unique$unique_id, FUN =  function(x){
  y1 = grep("female", x);
  y2 = max(0, y1);
  y2})
# Check if above max function works in python
temp_sx_fe2 = unlist(temp_sx_fe1)
length(which(temp_sx_fe2 > 1))
table(temp_sx_fe2)
length(temp_sx_fe2)

# Identify males: females will also show up in grep("male", ..), so subtract female count
# from grep("male", ..) count to identify males
temp_sx_ma1 = lapply(visits_1site_unique$unique_id, FUN =  function(x){
  y1 = grep("male", x);
  y2 = max(0, y1);
  y2})
temp_sx_ma2 = unlist(temp_sx_ma1)
length(which(temp_sx_ma2 > 1))
temp_sx_ma3 = temp_sx_ma2 - temp_sx_fe2
table(temp_sx_ma2); table(temp_sx_ma3)

visits_1site_unique$sw_sex = "unknown"
visits_1site_unique$sw_sex[which(temp_sx_fe2 == 1)] = "female"
visits_1site_unique$sw_sex[which(temp_sx_ma3 == 1)] = "male"
table(visits_1site_unique$sw_sex)



### Ratio of SW/PE by site
swbysite = aggregate(unique_id_2~site, data = visits_1site_unique, FUN = function(x){y = length(unique(x))}) 
pebysite = aggregate(peer_educator_id~site, data = visits_1site_unique, FUN = function(x){y = length(unique(x))}) 
swpe = merge(swbysite, pebysite, by = "site")
swpe$swperat = swpe$unique_id_2/swpe$peer_educator_id
hist(swpe$swperat, 20)
summary(swpe$swperat)
names(swpe)
swpe = swpe[, names(swpe) %in% c("site", "swperat")]
visits_1site_unique = merge(visits_1site_unique, swpe, by = "site", all.x = T, all.y = F)

# Check also mean SW/PE ratio per day
swbysitedt = aggregate(unique_id_2~site + Real_Date, data = visits_1site_unique, FUN = function(x){y = length(unique(x))}) 
swperdaysite = aggregate(unique_id_2~site, data = swbysitedt, mean)
pebysitedt = aggregate(peer_educator_id~site + Real_Date, data = visits_1site_unique, FUN = function(x){y = length(unique(x))}) 
peperdaysite = aggregate(peer_educator_id~site, data = pebysitedt, mean)
swpe_perday = merge(swperdaysite, peperdaysite, by = "site")
swpe_perday$swperatpd = swpe_perday$unique_id_2/swpe_perday$peer_educator_id
hist(swpe_perday$swperatpd, 20, col = 2)
summary(swpe_perday$swperatpd)
names(swpe_perday)
swpe_perday = swpe_perday[, names(swpe_perday) %in% c("site", "swperatpd")]
visits_1site_unique = merge(visits_1site_unique, swpe_perday, by = "site", all.x = T, all.y = F)



### Budget
sort(unique(budget$field))
nrow(budget)
budget_b = budget[which(budget$field == "budget"),]
nrow(budget_b)
head(budget_b)

budget_subrec = aggregate(val~subrecipient, data = budget_b, sum)
budget_numdisp = aggregate(val~subrecipient, data = budget_b, length)
budget_subrec
budget_numdisp
names(budget_subrec)[2] = "totalbudget"
visits_1site_unique = merge(visits_1site_unique, budget_subrec, by = "subrecipient", all.x = T, all.y = F) 

# Get budget data for specific tests
budget_h = budget[which(budget$field == "budget_b_1_received_hts_and_know_results"),]
budget_hiv = aggregate(val~subrecipient, data = budget_h, sum)
names(budget_hiv)[2] = "hivtest_budg"

budget_s = budget[which(budget$field == "budget_c1_sws_screened_for_sexually_transmitted_infections_sti_2"),]
budget_std = aggregate(val~subrecipient, data = budget_s, sum)
names(budget_std)[2] = "stdtest_budg"

budget_t = budget[which(budget$field == "budget_d1_number_of_sws_screened_for_tb"),]
budget_tb = aggregate(val~subrecipient, data = budget_t, sum)
names(budget_tb)[2] = "tbtest_budg"

budget_tests = merge(budget_hiv, budget_std, by = "subrecipient")
budget_tests = merge(budget_tests, budget_tb, by = "subrecipient")
budget_tests
visits_1site_unique = merge(visits_1site_unique, budget_tests, by = "subrecipient", all.x = T, all.y = F) 


budget_tests_pl = merge(budget_tests, budget_subrec, by = "subrecipient")
budget_tests_pl[,2:5] = round(budget_tests_pl[,2:5]/10000,0)
budget_tests_pl


# Pop density data (district)
setwd("C:/Asha/High/Insight/Project/data_files/external_data/")
popbydist = read.csv("Statoids_2001_Pop.csv")
pop = as.numeric(gsub(",", "", as.character(popbydist$Population)))
area = as.numeric(gsub(",", "", as.character(popbydist$Area.km.?.)))
popbydist$density = pop/area
# Note Buffalo City separated from Amathole in 2011, so use Amathole as proxy
test = popbydist$density[popbydist$Municipality %in% c("Bojanala", "Amathole", "City of Cape Town",
                                                       "City of Johannesburg",  "City of Tshwane",
                                                       "eThekwini", "Ehlanzeni", "Ekurhuleni",
                                                       "Mopani", "Sedibeng", "Thabo Mofutsanyane",
                                                       "Vhembe", "West Rand")]
hist(test, 20)
match_dt = data.frame(Municipality = c("Bojanala Platinum", "Amathole", "City of Cape Town",
                                       "City of Johannesburg", "City of Tshwane",
                                       "eThekwini", "Ehlanzeni", "Ekurhuleni",
                                       "Mopani", "Sedibeng", "Thabo Mofutsanyane",
                                       "Vhembe", "West Rand"),
                      district = c("Bojanala", "Buffalo City", "City of Cape Town", 
                                   "City of Johannesburg", "City of Tshwane", 
                                   "EThekwini", "Ehlanzeni", "Ekurhuleni", 
                                   "Mopani", "Sedibeng", "Thabo Mofutsanyane", 
                                   "Vhembe", "West Rand"))
r = match(popbydist$Municipality, match_dt$Municipality)
popbydist$district = match_dt$district[r]
popbydist

visits_1site_unique =merge(visits_1site_unique, popbydist, by = "district", all.x = T, all.y = F)




### Minimum care package
hiv = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "b_1_received_hts_and_know_results")])
length(hiv)
sti = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "c1_sws_screened_for_sexually_transmitted_infections_sti_2")])
length(sti)
tb = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "d1_number_of_sws_screened_for_tb")])
length(tb)

length(unique(visits_one_nodup$unique_id_2))
hiv_tb = hiv[which(hiv%in%tb)]; round(100*length(hiv_tb)/length(unique(visits_one_nodup$unique_id_2)),0)
sti_tb = sti[which(sti%in%tb)]; round(100*length(sti_tb)/length(unique(visits_one_nodup$unique_id_2)),0)
hiv_tb_sti = sti[which(sti %in% hiv_tb)]; round(100*length(hiv_tb_sti)/length(unique(visits_one_nodup$unique_id_2)), 0)


### New HIV diagnoses
hiv_neg = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "b2_sw_tested_hiv_negative")])
hiv_pos = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "b_3_newly_diagnosed_hiv_positive_sw")])
hiv_test = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "b_1_received_hts_and_know_results")])
hiv_known = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "b_5_known_hiv_positive_status")])
hiv_ref = unique(visits_one_nodup$unique_id_2[which(visits_one_nodup$field == "b6_sws_who_refused_hiv_test")])
length(which(hiv_pos %in% hiv_test == F)) 
length(which(hiv_neg %in% hiv_test == F))
length(which(hiv_test %in% c(hiv_pos, hiv_neg) == F))
#All positives and negatives are recorded in test but not all test results are available

length(hiv_test)
length(hiv_known)
length(hiv_ref) #not many (only 291) refused
length(which(hiv_test %in% hiv_known)) #329 hiv tests in hiv known
length(which(hiv_pos %in% hiv_known)) #95 positives in hiv known 
length(which(hiv_neg %in% hiv_known)) #121 negatives in hiv known categ


### How many visits to complete full cycle?
mcp_sw = data.frame(unique_id_2 = unique(visits_one_nodup$unique_id_2))
datecol = data.frame(varnames = c("hiv_known_date", "hiv_test_date", "std_test_date", "tb_test_date"),
                     varfield = c("b_5_known_hiv_positive_status",
                                  "b_1_received_hts_and_know_results",
                                  "c1_sws_screened_for_sexually_transmitted_infections_sti_2",
                                  "d1_number_of_sws_screened_for_tb"))
for (i in 1:nrow(datecol))
{
  sub_var = subset(visits_one_nodup, field == as.character(datecol$varfield[i]))
  sub_var = sub_var[order(sub_var$unique_id_2),]
  a = rle(sub_var$unique_id_2); length(a$lengths[which(a$lengths > 1)])
  dup5 = duplicated(sub_var[, "unique_id_2"])
  sub_var = sub_var[which(dup5 == F),]
  
  sub_date_1 = sub_var[, c("unique_id_2", "Real_Date")]
  varname = as.character(datecol$varnames[i])
  names(sub_date_1)[2] = varname
  mcp_sw = merge(mcp_sw, sub_date_1, by = "unique_id_2", all.x = T)
  print(paste(length(which(is.na(mcp_sw[,varname]) == F)), varname))
}
nrow(mcp_sw)

# Any intersections between known HIV status and HIV test date: ~106 
hiv_both = length(intersect(which(is.na(mcp_sw$hiv_test_date) ==F), 
                            which(is.na(mcp_sw$hiv_known_date) == F)))
hiv_both
nrow(mcp_sw)
length(which(mcp_sw$hiv_known > mcp_sw$hiv_test_date))
length(which(mcp_sw$hiv_known < mcp_sw$hiv_test_date))
length(which(mcp_sw$hiv_known == mcp_sw$hiv_test_date))


### Typical length of time
# STD
mcp_sw$maxdate = as.Date(as.character(apply(mcp_sw[, c("hiv_test_date", "std_test_date", "tb_test_date")],
                                            1, FUN = function(x){
                                              y = max(x, na.rm = T)
                                            })))

mcp_sw$mindate = as.Date(as.character(apply(mcp_sw[, c("hiv_test_date", "std_test_date", "tb_test_date")],
                                            1, FUN = function(x){
                                              y = min(x, na.rm = T)
                                            })))
mcp_sw$testrange = mcp_sw$maxdate - mcp_sw$mindate
length(which(is.na(mcp_sw$testrange))); length(which(is.na(mcp_sw$testrange) == F))

# Identify what combination of tests each SW has had
# And how many
whichtest = function(x){
  a = seq(1,3)
  lookup = data.frame(a = a,
                      missing = c("two_std_tb", "two_hiv_tb", "two_hiv_std"),
                      only = c("one_hiv", "one_std", "one_tb"))
  # which test date fields have a value
  y1 = which(is.na(x) == F)
  if(length(y1) == 0)
  {
    y = "none"
  } else
  {
    if(length(y1) == 3)
    {
      y = "all"
    } else
    {
      if (length(y1) == 2)
      {
        y2 = a[which(a %in% y1 == F)]
        y = as.character(lookup$missing[which(lookup$a == y2)])
      } else
      {
        # length(y1) has to be 1
        y2 = y1
        y = as.character(lookup$only[which(lookup$a == y2)])
      }
    }
  }
  y
}
mcp_sw$alltests = NA
mcp_sw$alltests = apply(mcp_sw[, c("hiv_test_date", "std_test_date", "tb_test_date")], 1,
                        FUN = whichtest)
mcp_sw$numtests = NA
mcp_sw$numtests = sapply(mcp_sw$alltests, FUN = function(x)
{ 
  lookup = data.frame(y1 = c("none", "one", "two", "all"),
                      num = seq(0,3))
  y1 = strsplit(x, "_")[[1]][1]
  y = lookup$num[which(lookup$y1 == y1)]
  y
})
mcp_sw[1:20,]
table(mcp_sw$alltests) # For Venn diag
table(mcp_sw$numtests)

plot(mcp_sw$numtests~mcp_sw$testrange)
# Testrange is NA for zero tests
aggregate(testrange~numtests, data = mcp_sw, function(x){y = summary(as.numeric(x)); y })
# Majority of people are getting all their tests in one day, even with all three tests
# Confirmed by:
summary(as.numeric(mcp_sw$testrange))
plot(ecdf(as.numeric(mcp_sw$testrange[which(mcp_sw$numtests == 3)])), col = 3)
plot(ecdf(as.numeric(mcp_sw$testrange[which(mcp_sw$numtests == 2)])), col = 2, add = T)
plot(ecdf(as.numeric(mcp_sw$testrange[which(mcp_sw$numtests == 1)])), col = 4, add = T)

# So may not make sense to see what services provided before:
# OR divide into two groups: 1. one day folks (testrange = 0) - what services obtained
# 2. longer folks: what services obtained prior
# Number of visits by those who obtained at least one test versus those who 

### Does testrange starts at first visit or later
mcp_sw = merge(mcp_sw, temp3, by = "unique_id_2")
aggregate(numtests~first_quart, data = mcp_sw, summary)
aggregate(numtests~first_quart, data = mcp_sw, length)
# First quarter poor but 2-4 are about same
mcp_sw$testdelay = mcp_sw$mindate - mcp_sw$first_date
summary(as.numeric(mcp_sw$testdelay))
# Most of these happen during first test (makes sense because most people don't show up
# more than once)

# Do people who get tests visit more often?
temp4 = aggregate(Real_Date~unique_id_2, data = visits_one_nodup, function(x){y = length(unique(x))})
names(temp4)[2] = "numvisits"
mcp_sw = merge(mcp_sw, temp4, by = "unique_id_2")
plot(mcp_sw$numtests~mcp_sw$numvisits)
aggregate(numtests~numvisits, data = mcp_sw, summary)
# Fairly even for up to 7 visits, but after many visits, much more likely to have 2 or more tests
aggregate(numvisits~numtests, data = mcp_sw, summary)

# Do they get more services or kinds of services by completion of MCP
# (Consider only tests taken before/on mcp_sw$maxdate)
ignore = c("b_5_known_hiv_positive_status",
           "b_1_received_hts_and_know_results",
           "c1_sws_screened_for_sexually_transmitted_infections_sti_2",
           "d1_number_of_sws_screened_for_tb",
           "b2_sw_tested_hiv_negative",
           "b_3_newly_diagnosed_hiv_positive_sw",
           "b6_sws_who_refused_hiv_test")
visits_one_nodup_temp = merge(visits_one_nodup, mcp_sw, by = "unique_id_2")
dim(visits_one_nodup_temp)
visits_one_nodup_temp$bymax = visits_one_nodup_temp$Real_Date>=visits_one_nodup_temp$maxdate
visits_one_nodup_temp$fieldbymax = NA
visits_one_nodup_temp$fieldbymax[which(visits_one_nodup_temp$bymax == T)] = 
  as.character(visits_one_nodup_temp$field[which(visits_one_nodup_temp$bymax == T)])

# Number of distinct services
temp5 = aggregate(fieldbymax~unique_id_2, data = visits_one_nodup_temp, function(x){
  y1 = as.character(x);
  y2 = x[which(x %in% ignore == F)];
  y = length(unique(y2))})
names(temp5)[2] = "numdistinctservices"
mcp_sw = merge(mcp_sw, temp5, by = "unique_id_2", all.x = T, all.y = F)
mcp_sw$numdistinctservices[which(is.na(mcp_sw$numdistinctservices))] = 0
plot(mcp_sw$numtests~mcp_sw$numdistinctservices, main = "Everyone")
# More tests associated with more distinct services obtained
# True for both single visit people and multi visit people
plot(numtests~numdistinctservices, data = mcp_sw[which(mcp_sw$numvisits == 1),], 
     main = "One-timers")
plot(numtests~numdistinctservices, data = mcp_sw[which(mcp_sw$numvisits > 1),],
     main = "Returners")

# Number of services
temp6 = aggregate(fieldbymax~unique_id_2, data = visits_one_nodup_temp, function(x){
  y1 = as.character(x);
  y = length(which(x %in% ignore == F))})
names(temp6)[2] = "numservices"
mcp_sw = merge(mcp_sw, temp6, by = "unique_id_2", all.x = T, all.y = F)
mcp_sw$numservices[which(is.na(mcp_sw$numservices))] = 0
plot(mcp_sw$numtests~mcp_sw$numservices, main = "Everyone")
table(mcp_sw$numservices)
# For one-timers, more services associated with more tests
plot(numtests~numservices, data = mcp_sw[which(mcp_sw$numvisits == 1),], 
     main = "One-timers")
plot(numtests~numservices, data = mcp_sw[which(mcp_sw$numvisits > 1),],
     main = "Returners")
aggregate(numservices~numtests, data = mcp_sw, median)


temp7 = aggregate(field~unique_id_2, data = visits_one_nodup, function(x){
  y1 = sort(table(x), decreasing = T);
  y2 = names(y1)[1]
  y2
})
head(temp7)
names(temp7)[2] = "mostcommonserv"
mcp_sw_temp = merge(mcp_sw, temp7, by = "unique_id_2")
a = aggregate(numtests~mostcommonserv, data = mcp_sw_temp, mean)
a[order(a$numtests, decreasing = T),]
# Not much information

temp8 = aggregate(fieldbymax~unique_id_2, data = visits_one_nodup_temp, function(x){
  y = length(which(x %in% c("f_1_male_condoms_number_given", "female_condom",
                            "f3_lube", "goody_bags")))
  y
})
head(temp8)
names(temp8)[2] = "material"
mcp_sw = merge(mcp_sw, temp8, by = "unique_id_2", all.x = T, all.y = F)
mcp_sw$material[which(is.na(mcp_sw$material))] = 0
plot(mcp_sw$numtests~mcp_sw$material, main = "Everyone")
# No clear association between material and number of tests
plot(numtests~material, data = mcp_sw[which(mcp_sw$numvisits == 1),], 
     main = "One-timers")
plot(numtests~material, data = mcp_sw[which(mcp_sw$numvisits > 1),],
     main = "Returners")

temp9 = aggregate(fieldbymax~unique_id_2, data = visits_one_nodup_temp, function(x){
  y = length(which(x %in% c("e4_support_group_workshops", "risk_reduction_workshop")))
  y
})
names(temp9)[2] = "workshops"
mcp_sw = merge(mcp_sw, temp9, by = "unique_id_2", all.x = T, all.y = F)
mcp_sw$workshops[which(is.na(mcp_sw$workshops))] = 0
plot(mcp_sw$numtests~mcp_sw$workshops, main = "Everyone")
# No clear association between material and number of tests
plot(numtests~workshops, data = mcp_sw[which(mcp_sw$numvisits == 1),], 
     main = "One-timers")
plot(numtests~workshops, data = mcp_sw[which(mcp_sw$numvisits > 1),],
     main = "Returners")
aggregate(numtests~workshops, data = mcp_sw, median)
aggregate(workshops~numtests, data = mcp_sw, summary)
# Only at very high levels is there positive association

### Account for HIV known status
rm(r)
r = match(mcp_sw$unique_id_2, hiv_known)
r[which(is.na(r) == F)] = "yes"
r[which(is.na(r))] = "no"
mcp_sw$hiv_known = r
# Also check if HIV known before HIV test
hivknownbeforetest = mcp_sw$hiv_test_date - mcp_sw$hiv_known_date
summary(as.numeric(hivknownbeforetest))


### Combine minimum care package and other data
head(visits_1site_unique); dim(visits_1site_unique)
nrow(mcp_sw)
sw_1site = merge(mcp_sw, visits_1site_unique, by = "unique_id_2")
nrow(sw_1site)
library(plyr)
sw_1site$twoormore = mapvalues(sw_1site$numtests, from = c("0", "1", "2", "3"), to = c("0", "0", "1", "1"))


### Budget per person
budgetperperson = aggregate(totalbudget~subrecipient, data = sw_1site, FUN = function(x){
  y1 = as.numeric(as.character(x));
  y2 = unique(x)/length(x)
  y2
})
names(budgetperperson)[2] = "budgetperperson"
sw_1site = merge(sw_1site, budgetperperson, by = "subrecipient", all.x = T, all.y = F)

sw_1site$hiv_test = 0
sw_1site$hiv_test[which(is.na(sw_1site$hiv_test_date) == F)] = 1

sw_1site$std_test = 0
sw_1site$std_test[which(is.na(sw_1site$std_test_date) == F)] = 1

sw_1site$tb_test = 0
sw_1site$tb_test[which(is.na(sw_1site$tb_test_date) == F)] = 1


sort(names(sw_1site))
# Possibly important features
# SW level
c("first_quart.x", "hiv_known", 
  "main_pe", "numdistinctservices",
  "numservices", "numvisits", "sw_birthyr", "sw_sex",
  "testrange", "material", "workshops")

# Higher levels
c("subrecipient", "district", "homesite",
  "hivtest_budg",  "stdtest_budg", "tbtest_budg", 
  "totalbudget", "budgetperperson", 
  "density", "swperatpd")


# Model choice
# https://doc.dataiku.com/dss/latest/machine_learning/scikitlearn.html#regression-lasso-regression
# https://www.quora.com/Why-does-Gradient-boosting-work-so-well-for-so-many-Kaggle-problems


# Mixed effects logistic regression
# https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
# Took very long

### Three models (one for each test)
library(lme4)
seldata = sw_1site[, names(sw_1site) %in% c("hiv_test", "std_test", "tb_test", 
                                            # "material", "workshops",
                                            "hiv_known", 
                                            "sw_birthyr",
                                            "swperatpd", "density",
                                            "first_quart.x",
                                            "homesite", "district", "subrecipient", 
                                            "unique_id_2")]
seldata$material = 0
seldata$material[which(sw_1site$material > 0)] = 1
seldata$workshops = 0
seldata$workshops[which(sw_1site$workshops > 0)] = 1

# Change class of columns
numcols = c("sw_birthyr",
            # "material", "workshops", 
            "density", "swperatpd")
categcols = names(seldata)[which(names(seldata) %in% numcols == F)]
seldata[, numcols] = apply(seldata[, numcols], 2, FUN = function(x){y = as.numeric(as.character(x))})
class(seldata$sw_birthyr)
for (i in categcols)
{
  seldata[, i] = as.factor(seldata[, i])
}
lapply(names(seldata), FUN = function(x){print(paste(x, class(seldata[, x]), length(which(is.na(seldata[, x])))))})
#lapply(categcols, FUN = function(x){length(levels(seldata[, x]))})
# seldata$numservices[which(is.na(seldata$numservices))] = 0

seldata_nona = na.omit(seldata)
dim(seldata); dim(seldata_nona)

datascal = data.frame()
seldata2 = seldata_nona
for (i in numcols)
{
  mean_var = mean(seldata_nona[, i], na.rm = T)
  twosd_var = 2*sd(seldata_nona[, i], na.rm = T)
  seldata2[, i] = (seldata_nona[, i] - mean_var)/twosd_var
  if (i == numcols[1])
  {
    datascal = data.frame(numcol = i, mean = mean_var, twosd = twosd_var)
  } else
  {
    datascal = rbind(datascal, data.frame(numcol = i, mean = mean_var, twosd = twosd_var))
  }
}

ind <- sample(2,nrow(seldata2),replace=TRUE,prob=c(0.7,0.3))
traindata = seldata2[ind == 1,]
testdata = seldata2[ind == 2,]

testdata$district[which(testdata$district %in% traindata$district == F)]  =NA
testdata$site[which(testdata$site %in% traindata$site == F)] = NA
testdata$subrecipient[which(testdata$subrecipient %in% traindata$subrecipient == F)]=NA
testdata = na.omit(testdata)
alldata = rbind(traindata, testdata)

# Convergence issues: stopped when variables rescaled
# https://stats.stackexchange.com/questions/110004/how-scared-should-we-be-about-convergence-warnings-in-lme4
# https://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4
# https://stackoverflow.com/questions/21344555/convergence-error-for-development-version-of-lme4
# http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
# https://stackoverflow.com/questions/23478792/warning-messages-when-trying-to-run-glmer-in-r#23570617
# https://stackoverflow.com/questions/34550758/glmer-from-r-package-lme4-asking-to-scale-variables-even-though-variables-alread

# HIV Model
t1 = Sys.time()
mel_hiv = glmer(hiv_test ~ material
                + workshops + hiv_known 
                + sw_birthyr
                + swperatpd + density + first_quart.x 
                + (1|homesite) + (1|district),
                data = traindata, family = binomial)
t2 = Sys.time()
t2-t1 #1 - 3 minutes
summary(mel_hiv)

melpred_hiv = predict(mel_hiv, newdata = testdata, type = "response")
head(melpred_hiv)

library(ROCR)
perf_hiv = prediction(melpred_hiv, testdata$hiv_test)
auc_hiv = performance(perf_hiv, "auc")
auc_hiv
auc_val_hiv = auc_hiv@y.values; auc_val_hiv
pred3_hiv = performance(perf_hiv, "tpr","fpr")
plot(pred3_hiv,main="HIV Test: ROC Curve for Hierarchical Logistic Model",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


library(caret)
# Threshold of 0.5 seems acceptable
melpred_hiv_class = rep(0, length(melpred_hiv))
melpred_hiv_class[which(melpred_hiv>0.5)] = 1 
xtab = table(melpred_hiv_class, testdata$hiv_test)
conf_hiv = confusionMatrix(xtab)
conf_hiv

mat_hiv = conf_hiv$table
accuracy_hiv = sum(diag(mat_hiv))/sum(mat_hiv); accuracy_hiv
precision_hiv = diag(mat_hiv)/rowSums(mat_hiv); precision_hiv
recall_hiv = diag(mat_hiv)/colSums(mat_hiv); recall_hiv

pred4 = performance(perf_hiv, "rec")
plot(pred4,main="HIV Recall Curve",col=2,lwd=2, ylim = c(0,1))
abline(v = 0.5)

pred4 = performance(perf_hiv, "prec")
plot(pred4,main="HIV Precision Curve",col=2,lwd=2, ylim = c(0,1))
abline(v = 0.5)

pred4 = performance(perf_hiv, "prec", "rec")
plot(pred4,main="Precision - Recall Curve",col=2,lwd=2, ylim = c(0,1))
abline(v = 0.5)


# STD Model
t1 = Sys.time()
mel_std = glmer(std_test ~  material
                + workshops + hiv_known 
                + sw_birthyr
                + swperatpd + density + first_quart.x 
                + (1|homesite) + (1|district),
                data = traindata, family = binomial)
t2 = Sys.time()
t2-t1 #1 - 3 minutes
summary(mel_std)

# STD Predictions
melpred_std = predict(mel_std, newdata = testdata, type = "response")
head(melpred_std)

perf_std = prediction(melpred_std, testdata$std_test)
auc_std = performance(perf_std, "auc")
auc_std
auc_val_std = auc_std@y.values; auc_val_std
pred3_std = performance(perf_std, "tpr","fpr")
plot(pred3_std,main="STD Test: ROC Curve for Hierarchical Logistic Model",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


melpred_std_class = rep(0, length(melpred_std))
melpred_std_class[which(melpred_std>0.5)] = 1 
xtab = table(melpred_std_class, testdata$std_test)
conf_std = confusionMatrix(xtab)
conf_std

mat_std = conf_std$table
accuracy_std = sum(diag(mat_std))/sum(mat_std); accuracy_std
precision_std = diag(mat_std)/rowSums(mat_std); precision_std
recall_std = diag(mat_std)/colSums(mat_std); recall_std

pred4 = performance(perf_std, "rec")
plot(pred4,main="STD Recall Curve",col=2,lwd=2, ylim = c(0,1))
abline(v = 0.5)

pred4 = performance(perf_std, "prec")
plot(pred4,main="STD Precision Curve",col=2,lwd=2, ylim = c(0,1))
abline(v = 0.5)


# TB Model
t1 = Sys.time()
mel_tb = glmer(tb_test ~ material
               + workshops + hiv_known 
               + sw_birthyr
               + swperatpd  + density + first_quart.x 
               + (1|homesite) + (1|district),
               data = traindata, family = binomial)
t2 = Sys.time()
t2-t1 #1 - 3 minutes
summary(mel_tb)

melpred_tb = predict(mel_tb, newdata = testdata, type = "response")
head(melpred_tb)

perf_tb = prediction(melpred_tb, testdata$tb_test)
auc_tb = performance(perf_tb, "auc")
auc_tb
auc_val_tb = auc_tb@y.values; auc_val_tb
pred3_tb = performance(perf_tb, "tpr","fpr")
plot(pred3_tb,main="TB Test: ROC Curve for Hierarchical Logistic Model",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

pred4 = performance(perf_tb, "prec", "rec")
plot(pred4,main="Precision - Recall Curve",col=2,lwd=2, ylim = c(0,1))

pred4 = performance(perf_tb, "rec")
plot(pred4,main="Recall Curve",col=2,lwd=2, ylim = c(0,1))
abline(v = 0.5)

pred4 = performance(perf_tb, "prec")
plot(pred4,main="Precision Curve",col=2,lwd=2, ylim = c(0,1))
abline(v = 0.5)


melpred_tb_class = rep(0, length(melpred_tb))
melpred_tb_class[which(melpred_tb>0.5)] = 1 
xtab = table(melpred_tb_class, testdata$tb_test)
conf_tb = confusionMatrix(xtab)
conf_tb

mat_tb = conf_tb$table
accuracy_tb = sum(diag(mat_tb))/sum(mat_tb); accuracy_tb
precision_tb = diag(mat_tb)/rowSums(mat_tb); precision_tb
recall_tb = diag(mat_tb)/colSums(mat_tb); recall_tb



### HIV prevalence
setwd("C:/Asha/High/Insight/Project/data_files/external_data/")
hiv_prev = read.csv("NDH_2008_hiv_prev.csv")
head(hiv_prev)
a = sort(as.character(hiv_prev$District))
b = sort(as.character(unique(sw_1site$district)))
b[which(b %in% a)]
b[which(b %in% a == F)]
match_hiv_dt = data.frame(ndhdist = c("Amatole", "Metropol",
                                      "Tshwane", "Ethekwini",
                                      "Bojanala", "City of Johannesburg", "Ehlanzeni", "Ekurhuleni", 
                                      "Mopani", "Sedibeng", "Thabo Mofutsanyane", "Vhembe", "West Rand"),
                          district = c("Buffalo City", "City of Cape Town", 
                                       "City of Tshwane", "EThekwini",
                                       "Bojanala", "City of Johannesburg", "Ehlanzeni", "Ekurhuleni", 
                                       "Mopani", "Sedibeng", "Thabo Mofutsanyane", "Vhembe", "West Rand"
                          ))
dtstats = data.frame(district = b)
r = match(hiv_prev$District, match_hiv_dt$ndhdist)
hiv_prev$nacosadist = match_hiv_dt$district[r]
rm(r)
r = match(dtstats$district, hiv_prev$nacosadist)
dtstats$hiv_prev = hiv_prev$Prevalence_Pct_2007[r]
rm(r)
dtstats
sitestats = data.frame(unique(alldata[, c("homesite", "district")]))
sitestats = merge(sitestats, dtstats, by = "district")

prop_alltests = aggregate(alltests~site, sw_1site, FUN = function(x){
  y1 = as.character(x);
  num_alltests = length(y1[which(y1 == "all")]);
  num_sw = length(y1);
  y2 = round(100*num_alltests/num_sw, 1)
  y2
})
names(prop_alltests)[2] = "prop_three_tests"

prop_hivtests = aggregate(alltests~site, sw_1site, FUN = function(x){
  y1 = as.character(x);
  num_hivtests = length(grep("hiv", y1)) + length(y1[which(y1 == "all")]);
  num_sw = length(y1);
  y2 = round(100*num_hivtests/num_sw, 1)
  y2
})
names(prop_hivtests)[2] = "prop_hiv_tests"

prop_stdtests = aggregate(alltests~site, sw_1site, FUN = function(x){
  y1 = as.character(x);
  num_stdtests = length(grep("std", y1)) + length(y1[which(y1 == "all")]);
  num_sw = length(y1);
  y2 = round(100*num_stdtests/num_sw, 1)
  y2
})
names(prop_stdtests)[2] = "prop_std_tests"

prop_tbtests = aggregate(alltests~site, sw_1site, FUN = function(x){
  y1 = as.character(x);
  num_tbtests = length(grep("tb", y1)) + length(y1[which(y1 == "all")]);
  num_sw = length(y1);
  y2 = round(100*num_tbtests/num_sw, 1)
  y2
})
names(prop_tbtests)[2] = "prop_tb_tests"

numsw = aggregate(unique_id_2~site, sw_1site, FUN = function(x){length(unique(x))})
names(numsw)[2] = "num_sw"
numsw

budgetpp_site = aggregate(budgetperperson~site, sw_1site, FUN = unique)
budgetpp_site$budgetperperson = round(budgetpp_site$budgetperperson, 0)

sitestats = merge(sitestats, prop_alltests, by.x = "homesite", by.y = "site", all.x = T, all.y = F)
sitestats = merge(sitestats, prop_hivtests, by.x = "homesite", by.y = "site",  all.x = T, all.y = F)
sitestats = merge(sitestats, prop_stdtests, by.x = "homesite", by.y = "site",  all.x = T, all.y = F)
sitestats = merge(sitestats, prop_tbtests, by.x = "homesite", by.y = "site",  all.x = T, all.y = F)
sitestats = merge(sitestats, numsw, by.x = "homesite", by.y = "site",  all.x = T, all.y = F)
sitestats = merge(sitestats, budgetpp_site, by.x = "homesite", by.y = "site",  all.x = T, all.y = F)

sitestats
names(sitestats)
names(sitestats) = c("Home_site", "District", "HIV_Prevalence", 
                     "Proportion_All_Three", "Proportion_HIV_Tests", "Proportion_STD_Tests",
                     "Proportion_TB_Tests", "Number_of_Sex_Workers", "Budget_Per_Sex_Worker")


hiv_sel = unique(alldata$unique_id_2[which(alldata$hiv_test == 1)]); length(hiv_sel)
std_sel = unique(alldata$unique_id_2[which(alldata$std_test == 1)]); length(std_sel)
tb_sel = unique(alldata$unique_id_2[which(alldata$tb_test == 1)]); length(tb_sel)
nrow(alldata)
hiv_pct = round(100*length(hiv_sel)/nrow(alldata), 0)
std_pct = round(100*length(std_sel)/nrow(alldata), 0)
tb_pct = round(100*length(tb_sel)/nrow(alldata), 0)
teststaken = data.frame(Test = c("Tuberculosis", "Other STDs","HIV"),
                        Percent = c(tb_pct, std_pct, hiv_pct))

# ggplot(data=teststaken, aes(x=Test, y=Percent)) +
#   geom_bar(stat="identity", colour="blue", fill = "blue", width = 0.6) +
#   ylim(0, 100) +
#   ylab("Sex workers who have taken test (%)") +
#   theme_classic()

setwd("C:/Asha/High/Insight/Project/intermediate_outputs/")
jpeg("teststaken.jpeg", width = 6, height = 4.5, units = "in", res = 400)
par(mar = c(5.1, 10, 4.1, 2.1))
barplot2(height = teststaken$Percent, names.arg = teststaken$Test, 
         width = 0.25, ylim = c(0,2),
         horiz = T, xlim = c(0,100), xlab = "Sex workers who have taken test (%)",
         las = 2, col = "cornflowerblue", border = "cornflowerblue",
         cex.names = 1.2)
dev.off()

setwd("C:/Asha/High/Insight/Project/zenysis_app/")
# write.csv(sitestats, file = "sitestats.csv")
# save(mel_hiv, file = "mel_hiv")
# save(mel_std, file = "mel_std")
# save(mel_tb, file = "mel_tb")
# save(alldata, file = "alldata")
# save(datascal, file = "datascal")