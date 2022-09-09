
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#------------------------------------#

library(pacman)

p_load(tidyverse, fastDummies, rstan, rmdcev, furrr, dplyr,plotrix,viridis, wesanderson)

set.seed(12345)

model = "gamma"
n_classes = 3
n_workers = 5
# File names

file_name_model_selection = "output/mdcev_lc_model_selection.RData"
file_name_model_selection_summary = "output/mdcev_lc_model_selection_summary.csv"

file_name_nclass_selection = "output/mdcev_lc_nclassselection.RData"
file_name_nclass_selection_summary = "output/mdcev_lc_nclassselection_summary.csv"

file_name_lc_estimation = paste0("output/mdcev_lc_estimation_",model,n_classes,".RData")
file_name_lc_welfare = paste0("output/mdcev_lc_welfare_",model,n_classes,".RData")

source('code/helper_modified.R')


mdcev_formula <- ~ 0

mdcev_formula_lc <- formula(paste("~ 0", 
						 "| indig + imigrant + male + urban + web_sample + 
	university + college + ageindex + province"))

#------------------------------------#
# Load Data
#------------------------------------#

df_mdcev <- read_csv("Data/CanadaRecData.csv")

## removing instances where expenditure>income ... need to identify the individuals and remove their data
## e.g., row 326 is individual 20, so remove all their data. remove all rows where expenditure is greater than income, sorting by ID

df_mdcev2 <- df_mdcev %>% group_by(id, income) %>% summarise(xsum = sum(costs * quant))
removals <- df_mdcev2$id[df_mdcev2$xsum >= df_mdcev2$income]
df_mdcev <- filter(df_mdcev, !id %in% removals)

activity_names <- unique(df_mdcev$activity)


#------------------------------------#
# Model selection
#------------------------------------#
# For which model to use
start.time <- Sys.time()
source("code/EstimateMDCEV_modelselection.R", echo = TRUE)
end.time <- Sys.time()
time.taken1 <- end.time - start.time
time.taken1


#------------------------------------#
# LC class number selection
#------------------------------------#
# N class selection for LC model
# parallel test of all models
lc_list <- list(two = 2,
				three = 3,
				four = 4,
				five = 5)

start.time <- Sys.time()
source("code/EstimateMDCEV_nclassselection.R", echo = TRUE)
end.time <- Sys.time()
time.taken2 <- end.time - start.time
time.taken2

summary(output_lc[[1]])
summary(output_lc[[2]])
summary(output_lc[[3]])
summary(output_lc[[4]])



#------------------------------------#
# Return MVN draws for chosen model
#------------------------------------#
# In this case, looks like 3-class model is preferred as 4-class model 
# has a class with small probabilities

# Get starting values from previous model run
load(file_name_nclass_selection)
init <- output_lc[[2]][["stan_fit"]][["par"]]
init$theta <- NULL
init$sum_log_lik <- NULL

df_mdcev_temp <- mdcev.data(data = df_mdcev, 
							id.var = "id", 
							alt.var = "activity", 
							choice = "quant" )

output <- mdcev(formula = mdcev_formula_lc,
			   data = df_mdcev_temp,
			   initial.parameters = init,
			   gamma_ascs = 1,
				std_errors = "mvn",
				n_classes = n_classes,
				single_scale = 1,
				n_draws = 500,
			   psi_ascs = 1,
			   model = model,
			   algorithm = "MLE")

save(output, file = file_name_lc_estimation)

#------------------------------------#
# Simulate welfare for 3 classes
#------------------------------------#

load(file_name_lc_estimation)

summary(output)

nerrs = 15
nsims = 100
nalts <- 17
npols <- nalts+1
policy_names <- c(activity_names, "all")

policies<- CreateBlankPolicies(npols = npols, 
							   model = output, 
							   price_change_only = TRUE)

# Closing each site individual then all together
price_p <- cbind(0,diag(nalts)*10000000)
price_p <- rbind(price_p, c(0,rep(100000000,nalts)))# add all closures to price_list
policies$price_p <- split(price_p, seq(nrow(price_p)))

# simulate welfare for each class in parallel
class_list <- list(class="class1",
				   class="class2",
				   class="class3")

plan(multiprocess, workers = length(class_list))

welfare_out = future_map(class_list, function(class){
	df_sim <- PrepareSimulationData(output, nsims = nsims,
									policies, class=class) # how many simulations, and class argument = class"1"
	welfare <- mdcev.sim(df_sim$df_indiv, 
						 df_common = df_sim$df_common,
						 sim_options = df_sim$sim_options, 
						 cond_err = 1, 
						 nerrs = nerrs, 
						 sim_type = "welfare") # specifies welfare changes
	return(welfare)
},
.options = furrr_options(seed = TRUE))

closeAllConnections()

#------------------------------------#
# Clean up welfare simulations
#------------------------------------#

# retrieve class probabilities
theta <- as_tibble(output[["class_probabilities"]])

df_id <- df_mdcev %>%
	distinct(id, indig, imigrant, male, province) %>%
	bind_cols(theta)


plan(multiprocess, workers = 5)

welfare_out1 = future_map(welfare_out[[1]], function(x){
colnames(x) = policy_names
out = as_tibble(x) %>%
	mutate(sim_id = rep(1:nsims)) %>%
	pivot_longer(-c("sim_id"), names_to = "activity", values_to = "wtp1") 
	return(out)
})

welfare_out2 = future_map(welfare_out[[2]], function(x){
	colnames(x) = policy_names
	out = as_tibble(x) %>%
		mutate(sim_id = rep(1:nsims)) %>%
		pivot_longer(-c("sim_id"), names_to = "activity", values_to = "wtp2") 
	return(out)
})

welfare_out3 = future_map(welfare_out[[3]], function(x){
	colnames(x) = policy_names
	out = as_tibble(x) %>%
		mutate(sim_id = rep(1:nsims)) %>%
		pivot_longer(-c("sim_id"), names_to = "activity", values_to = "wtp3") 
	return(out)
})

closeAllConnections()

welfare_out1 = do.call(rbind,welfare_out1) %>%
	mutate(id = rep(df_id$id, each = nsims*npols)) 

welfare_out2 = do.call(rbind,welfare_out2) %>%
	mutate(id = rep(df_id$id, each = nsims*npols))

welfare_out3 = do.call(rbind,welfare_out3) %>%
	mutate(id = rep(df_id$id, each = nsims*npols))

welfare = welfare_out1 %>%
	left_join(welfare_out2) %>%
	left_join(welfare_out3) %>%
	left_join(df_id) %>%
	mutate(wtp = wtp1*class1 + wtp2*class2+wtp3*class3) 

save(welfare, file = file_name_lc_welfare)

#------------------------------------#
# Compare Class belonging probability
#------------------------------------#
im_C <- df_id %>%
  filter(imigrant == 1,
         indig == 0)
summary(im_C)  
ind_C <- df_id %>%
  filter(imigrant == 1,
         indig == 0)
summary(ind_C)
oth <- df_id %>%
  filter(imigrant == 0,
         indig == 0)
summary(oth_C)  

male_C <- df_id %>%
  filter(male == 1)
summary(male_C) 

fem_C <- df_id %>%
  filter(male == 0)
summary(fem_C)  


#mean(fem_C$class2)/std.error(fem_C$class2)
#mean(fem_C$class3)/std.error(fem_C$class3)
#------------------------------------#
# Summarize welfare
#------------------------------------#
load(file_name_lc_welfare)

##----- Welfare impacts by ethnicity ---

#welfare_summary = welfare%>%
#	group_by(sim_id, activity, indig, imigrant) %>%
#	summarise(wtp = mean(wtp)) %>%
#	ungroup(.) %>%
#	group_by(activity, indig, imigrant) %>%
#	summarise(wtp_mean = round(mean(wtp), 0), 
#			  wtp_sd = round(sd(wtp), 0),
#			  wtp_hi = round(quantile(wtp, .975),0),
#			  wtp_lo = round(quantile(wtp, .025),0)) %>%
#	mutate(subgroup = case_when(indig == 1 & imigrant == 0 ~ "Indigenous",
#								 indig == 0 & imigrant == 1 ~ "Immigrant",
#								 indig == 0 & imigrant == 0 ~ "Other",
#								 TRUE ~ NA_character_)) 


##### welfare impacts relative to neither
welfare_summary = welfare %>%
  group_by(sim_id, activity, indig, imigrant) %>%
  summarise(wtp = mean(wtp)) %>%
  mutate(subgroup = case_when(indig == 1 & imigrant == 0 ~ "Indigenous",
                              indig == 0 & imigrant == 1 ~ "Immigrant",
                              indig == 0 & imigrant == 0 ~ "Other",
                              TRUE ~ NA_character_))

welfare_summary = na.omit(welfare_summary)

welfare_ratio = welfare_summary %>%
  group_by(activity, sim_id)%>%
  mutate(ratio = wtp/wtp[subgroup=="Other"]-1)

welfare_ratio$ratio <- welfare_ratio$ratio*100

welfare_ratio = welfare_ratio %>%
  group_by(activity, subgroup) %>%
  summarise(wtp_mean = mean(ratio), 
            wtp_se = sd(ratio),
            wtp_hi = quantile(ratio, .975),
            wtp_lo = quantile(ratio, .025))

library(patchwork)

### plotting differences
labels = tibble(activity = unique(welfare_ratio$activity),
				activity_name = c('All', 'Birding','Camping','Cycling', 
								  "Fishing", "Gardening", "Golfing", "Hiking", 
								  "Bird hunting", "Hunting large animals", 
								  "Hunting other", "Hunting waterfowl", 
								  "Motorized vehicles", "Photography",  
								  "Cross-country ski", "Downhill ski", 
								  "Motorboat", "Beach"))

welfare_ratio  = welfare_ratio %>%
	filter(subgroup != "Other") %>%
	left_join(labels) %>%
	ungroup(.)

welfare_all  = welfare_ratio %>%
	filter(activity == "all")

plot1 = welfare_ratio %>%
	filter(subgroup == "Immigrant", activity != "all") %>%
	mutate(activity_name = fct_reorder(activity_name, desc(wtp_mean))) %>%
	ggplot(aes(y = wtp_mean, x = activity_name))+#, colour = as.factor(subgroup))) + 
	coord_flip() +
	#	ylim(-500, 0) +
	geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo))+
	ylab("") + 
	xlab("") + #labs(colour = "Ethnicity")+
  ggtitle("Immigrant subgroup")+
	scale_y_continuous(limits = c(-50, 300),
					   breaks = c(-50, 0, 50, 100, 200, 300))+
	# scale_x_discrete(labels = c('All', 'Birding','Camping','Cycling', "Fishing", "Gardening", "Golfing", "Hiking", "Bird hunting", "Hunting large animals", "Hunting other", "Hunting waterfowl", "Motorized vehicles", "Photography",  "Cross-country ski", "Downhill ski", "Motorboat", "Beach"))+
	geom_hline(yintercept = 0 )+
	geom_hline(yintercept = -7.63 , linetype = "dashed") +
	annotate(geom = "text",
			 label="Dashed line is for \n All Activities",
			 x = 15,
			 y = 200,
			 angle = 0, 
			 vjust = 1) +
	#  geom_text(x=17, y=110, label="+", color="black", size=10) +
	theme_bw() #+ 
#	theme(panel.grid.major = element_blank(),
#		  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot1


plot2 = welfare_ratio %>%
	filter(subgroup == "Indigenous", activity != "all") %>%
	mutate(activity_name = fct_reorder(activity_name, desc(wtp_mean))) %>%
	ggplot(aes(y = wtp_mean, x = activity_name))+#, colour = as.factor(subgroup))) + 
	coord_flip() +
	#	ylim(-500, 0) +
	geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo))+
	ylab("Difference in economic benefits relative to 'Neither' subgroup (%)") + 
	xlab("") + #labs(colour = "Ethnicity")+
  ggtitle("Indigenous subgroup")+
	scale_y_continuous(limits = c(-50, 300),
					   breaks = c(-50, 0, 50, 100, 200, 300))+
	geom_hline(yintercept = 0 )+
	geom_hline(yintercept = 63.4 , linetype = "dashed") +
#	annotate(geom = "text",
#			 label="Dasehd line is for \n All Activities",
#			 x = 4,
#			 y = 100,
#			 angle = 0, 
#			 vjust = 1) +
	#  geom_text(x=17, y=110, label="+", color="black", size=10) +
	theme_bw()# + 
#	theme(panel.grid.major = element_blank(),
#	panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
plot2

 p = plot1 / plot2
ggsave('subgroup_diff.png', p, height = 10, width  = 8)

p

welfare_summaryGEN = welfare %>%
	group_by(sim_id, activity, male) %>%
	summarise(wtp = mean(wtp)) %>%
	mutate(subgroup = case_when(male == 1 ~ "Man",
								male == 0 ~ "Woman",
								TRUE ~ NA_character_))


welfare_ratioG = welfare_summaryGEN %>%
	group_by(activity, sim_id)%>%
	mutate(ratio = wtp/wtp[subgroup=="Man"]-1)

welfare_ratioG$ratio <- welfare_ratioG$ratio*100

welfare_ratioG = welfare_ratioG %>%
	group_by(activity, subgroup) %>%
	summarise(wtp_mean = mean(ratio), 
			  wtp_se = sd(ratio),
			  wtp_hi = quantile(ratio, .975),
			  wtp_lo = quantile(ratio, .025))

### plotting differences

welfare_ratioG  = welfare_ratioG %>%
	filter(subgroup != "Man") %>%
	left_join(labels) %>%
	ungroup(.)

welfare_allG  = welfare_ratioG %>%
	filter(activity == "all")

plot1 = welfare_ratioG %>%
	filter(activity != "all") %>%
	mutate(activity_name = fct_reorder(activity_name, desc(wtp_mean))) %>%
	ggplot(aes(y = wtp_mean, x = activity_name))+#, colour = as.factor(subgroup))) + 
	coord_flip() +
	geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo))+
	ylab("Difference in economic benefits for women relative to men (%)") + 
	xlab("") + #labs(colour = "Ethnicity")+
	scale_y_continuous(limits = c(-100, 75),
					   breaks = c(-100,-75, -50, -25, 0, 25, 50, 75))+
	# scale_x_discrete(labels = c('All', 'Birding','Camping','Cycling', "Fishing", "Gardening", "Golfing", "Hiking", "Bird hunting", "Hunting large animals", "Hunting other", "Hunting waterfowl", "Motorized vehicles", "Photography",  "Cross-country ski", "Downhill ski", "Motorboat", "Beach"))+
	geom_hline(yintercept = 0 )+
	geom_hline(yintercept = -21.3 , linetype = "dashed") +
	annotate(geom = "text",
			 label="Dashed line is for \n All Activities",
			 x = 15,
			 y = 60,
			 angle = 0, 
			 vjust = 1) +
	#  geom_text(x=17, y=110, label="+", color="black", size=10) +
	theme_bw() #+ 
#	theme(panel.grid.major = element_blank(),
#		  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot1
ggsave('temp/subgroup_diff_gender.png', plot1, height = 8, width  = 8)




p2 <-welfare_ratioG %>%
	filter(subgroup != "Man") %>%
	ggplot(aes(y = wtp_mean, x = activity, colour = as.factor(subgroup))) + 
	coord_flip() +
	#	ylim(-500, 0) +
	geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo), position = "jitter")+
	ylab("Economic benefits relative to Men (%)") + xlab("Activity") + labs(colour = "Gender")+
	scale_x_discrete(labels = c('All', 'Birding','Camping','Cycling', "Fishing", "Gardening", "Golfing", "Hiking", "Bird hunting", "Hunting large animals", "Hunting other", "Hunting waterfowl", "Motorized vehicles", "Photography",  "Cross-country ski", "Downhill ski", "Motorboat", "Beach"))+
	geom_hline(yintercept = 100 )+
	geom_text(x=17.1, y=95, label="-", color="black", size=10)+
	geom_text(x=17, y=105, label="+", color="black", size=10) 

p2+scale_color_manual(values=wes_palette(n=1, name="Royal2"))
























p1 <-welfare_ratio %>%
	filter(subgroup != "Other") %>%
	group_by(subgroup) %>%
	mutate(activity = fct_reorder(activity, desc(wtp_mean))) %>%
	ggplot(aes(y = wtp_mean, x = activity))+#, colour = as.factor(subgroup))) + 
  coord_flip() +
	facet_wrap(~subgroup, ncol = 1) +
#	ylim(-500, 0) +
	geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo))+
  ylab("Economic benefits relative to 'Neither' subgroup (%)") + xlab("Activity") + labs(colour = "Ethnicity")+
 # scale_x_discrete(labels = c('All', 'Birding','Camping','Cycling', "Fishing", "Gardening", "Golfing", "Hiking", "Bird hunting", "Hunting large animals", "Hunting other", "Hunting waterfowl", "Motorized vehicles", "Photography",  "Cross-country ski", "Downhill ski", "Motorboat", "Beach"))+
geom_hline(yintercept = 0 )+
 # geom_text(x=17.1, y=90, label="-", color="black", size=10)+
#  geom_text(x=17, y=110, label="+", color="black", size=10) +
	theme_bw()
p1


p1+scale_color_manual(values=wes_palette(n=2, name="Moonrise2"))

p1

###---------------Welfare impacts by gender ------------------###

#welfare_summaryGen = welfare%>%
#  group_by(sim_id, activity, male) %>%
 # summarise(wtp = mean(wtp)) %>%
  #ungroup(.) %>%
#  group_by(activity, male) %>%
#  summarise(wtp_mean = round(mean(wtp), 0), 
#            wtp_sd = round(sd(wtp), 0),
#            wtp_hi = round(quantile(wtp, .975),0),
#            wtp_lo = round(quantile(wtp, .025),0)) %>%
#  mutate(subgroup = case_when(male == 1 ~ "Man",
#                              male == 0 ~ "Woman",
 #                             TRUE ~ NA_character_)) 


welfare_summaryGEN = welfare %>%
  group_by(sim_id, activity, male) %>%
  summarise(wtp = mean(wtp)) %>%
  mutate(subgroup = case_when(male == 1 ~ "Man",
                             male == 0 ~ "Woman",
                             TRUE ~ NA_character_))


welfare_ratioG = welfare_summaryGEN %>%
  group_by(activity, sim_id)%>%
  mutate(ratio = wtp/wtp[subgroup=="Man"])

welfare_ratioG$ratio <- welfare_ratioG$ratio*100

welfare_ratioG = welfare_ratioG %>%
  group_by(activity, subgroup) %>%
  summarise(wtp_mean = mean(ratio), 
            wtp_se = std.error(ratio),
            wtp_hi = quantile(ratio, .975),
            wtp_lo = quantile(ratio, .025))

### plotting differences

p2 <-welfare_ratioG %>%
  filter(subgroup != "Man") %>%
  ggplot(aes(y = wtp_mean, x = activity, colour = as.factor(subgroup))) + 
  coord_flip() +
  #	ylim(-500, 0) +
  geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo), position = "jitter")+
  ylab("Economic benefits relative to Men (%)") + xlab("Activity") + labs(colour = "Gender")+
  scale_x_discrete(labels = c('All', 'Birding','Camping','Cycling', "Fishing", "Gardening", "Golfing", "Hiking", "Bird hunting", "Hunting large animals", "Hunting other", "Hunting waterfowl", "Motorized vehicles", "Photography",  "Cross-country ski", "Downhill ski", "Motorboat", "Beach"))+
  geom_hline(yintercept = 100 )+
  geom_text(x=17.1, y=95, label="-", color="black", size=10)+
  geom_text(x=17, y=105, label="+", color="black", size=10) 

p2+scale_color_manual(values=wes_palette(n=1, name="Royal2"))


###----- Interacting gender and ehtnicity
welfare_summaryINT = welfare%>%
  group_by(sim_id, activity, male, indig, imigrant) %>%
  summarise(wtp = mean(wtp)) %>%
  ungroup(.) %>%
  group_by(activity, male, indig, imigrant) %>%
  summarise(wtp_mean = round(mean(wtp), 0), 
            wtp_sd = round(sd(wtp), 0),
            wtp_hi = round(quantile(wtp, .975),0),
            wtp_lo = round(quantile(wtp, .025),0)) %>%
  mutate(subgroup = case_when(male == 1 & indig==1 & imigrant==0 ~ "Indigenous Man",###fix
                              male == 0 & indig==1 & imigrant==0~ "Indigenous Woman",
                              male == 1 & indig==0 & imigrant==1 ~ "Immigrant Man",
                              male == 0 & indig==0 &imigrant==1 ~ "Immigrant Woman",
                              male == 1 & indig==0 & imigrant==0 ~ "'Other' Man",
                              male == 0 & indig==0 & imigrant==0 ~ "'Other' Woman",
                              TRUE ~ NA_character_)) 

## summarizing  welfare for each group
welfare_indigM=welfare_summaryINT %>%
  filter(indig==1,
         imigrant==0,
         male==1)

welfare_indigF=welfare_summaryINT %>%
  filter(indig==1,
         imigrant==0,
         male==0)

welfare_immigM=welfare_summaryINT %>%
  filter(indig==0,
         imigrant==1,
         male==1)
welfare_immigF=welfare_summaryINT %>%
  filter(indig==0,
         imigrant==1,
         male==0)

welfare_otherM=welfare_summaryINT %>%
  filter(indig==0,
         imigrant==0,
         male==1)
welfare_otherF=welfare_summaryINT %>%
  filter(indig==0,
         imigrant==0,
         male==0)

welfare_summaryINT %>%
  filter(activity != "all",
         !is.na(subgroup)) %>%
  ggplot(aes(y = wtp_mean, x = activity, colour = as.factor(subgroup))) + 
  geom_line() +
  coord_flip() +
  #	ylim(-500, 0) +
  geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo), position = "jitter")+
  ylab("Welfare impact (CAD$)") + xlab("Activity") + labs(colour = "Identity")+
  scale_x_discrete(labels = c('Birding','Camping','Cycling', "Fishing", "Gardening", "Golfing", "Hiking", 
                              "Bird hunting", "Hunting large animals", "Hunting other", "Hunting waterfowl", "Motorized vehicles", "Photography", 
                              "Cross-country ski", "Downhill ski", "Motorboat", "Beach"))

##----woman & ethnicity

welfare_summaryWOM = welfare%>%
  group_by(sim_id, activity, male, indig, imigrant) %>%
  summarise(wtp = mean(wtp)) %>%
  ungroup(.) %>%
  group_by(activity, male, indig, imigrant) %>%
  summarise(wtp_mean = round(mean(wtp), 0), 
            wtp_sd = round(sd(wtp), 0),
            wtp_hi = round(quantile(wtp, .975),0),
            wtp_lo = round(quantile(wtp, .025),0)) %>%
  mutate(subgroup = case_when(male == 0 & indig==1 & imigrant==0~ "Indigenous Woman",
                              male == 0 & indig==0 &imigrant==1 ~ "Immigrant Woman",
                              male == 0 & indig==0 & imigrant==0 ~ "'Other' Woman",
                              male == 1 & indig==0 & imigrant==0 ~ "'Other' Man",
                              TRUE ~ NA_character_)) 

welfare_summaryWOM %>%
  filter(activity != "all",
         !is.na(subgroup)) %>%
  ggplot(aes(y = wtp_mean, x = activity, colour = as.factor(subgroup))) + 
  geom_line() +
  coord_flip() +
  #	ylim(-500, 0) +
  geom_pointrange(aes(ymin=wtp_hi, ymax=wtp_lo), position = "jitter")+
  ylab("Welfare impact (CAD$)") + xlab("Activity") + labs(colour = "Identity")+
  scale_x_discrete(labels = c('Birding','Camping','Cycling', "Fishing", "Gardening", "Golfing", "Hiking", 
                              "Bird hunting", "Hunting large animals", "Hunting other", "Hunting waterfowl", "Motorized vehicles", "Photography", 
                              "Cross-country ski", "Downhill ski", "Motorboat", "Beach"))
