
## Randy's comments

# I like that you created a nicely organized repo and code.  This worked for me, and could for others which equals reproducibility.
# The comments are good.  I promised no quizzes, but I want to quiz you on them to see how much you could tell me about the functions :)
# I believe you copied my code for the chart which is fine-that is the beauty of R.  Now in the continuing spirit of things, make sure you share code and help someone else along the way.
# The only issue I had with the code was ``` at the very bottom which as a vestige of the code I shared.
# I have to get a few things arranged, but I'm still hoping for the Philly Cheesesteak with you.  Can we get one without cheese?  I know that's blasphemous, but I can't do dairy :()


#load libraries
library(tidyverse)
library(janitor)

#set seed
set.seed(123)

#read csv of bps_aoi_attributes into data frame in environment
bps_aoi_attributes <- read_csv("input_data/bps_aoi_attributes.csv")
#display first 6 rows of dataset
head(bps_aoi_attributes)
#display last 6 rows of dataset
tail(bps_aoi_attributes)


#read csv of bps_model_number_name into data frame in environment
bps_model_number_name <- read_csv("input_data/bps_model_number_name.csv")
#display first 6 rows of dataset
head(bps_model_number_name)
#display last 6 rows of the dataset
tail(bps_model_number_name)


#read csv of combine_raw into data frame 
combine_raw <- read_csv("input_data/combine_raw.csv")
#display first 6 rows of dataset
head(combine_raw)
#display last 6 rows of dataset
tail(combine_raw)

#read csv of LF16_BPS_200
LF16_BPS_200 <- read_csv("input_data/LF16_BPS_200.csv")
#display first 6 rows of dataset
head(LF16_BPS_200)
#display last 6 rows of dataset
tail(LF16_BPS_200)

#read csv of ref_con_modified
ref_con_modified <- read_csv("input_data/ref_con_modified.csv")
#display first 6 rows of dataset
head(ref_con_modified)
#display last 6 rows of dataset
tail(ref_con_modified)

#read csv of scls_aoi_attributes
scls_aoi_attributes <- read_csv("input_data/scls_aoi_attributes.csv")
#display first 6 rows of dataset
head(scls_aoi_attributes)
#display last 6 rows of dataset
tail(scls_aoi_attributes)

#Edit dataset ref_con_modified into new dataset "aoi_ref_con" so that "Model_Code" stays the way it is, while all other columns are pivoted. Unite function is used to combine the "Model_Code" and "reflabel" columns, and remove= false indicates that the original columns should be kept. left_join with the "bps_model_number_name" dataframe combines the data frames based on a common column and keeps all rows from the first datavframe while matching rows from the second.
aoi_ref_con <- ref_con_modified %>%
  pivot_longer(!Model_Code, names_to = "refLabel", values_to = "refPercent") %>%
  unite(model_label, c("Model_Code", "refLabel"), remove = FALSE) %>%
  left_join(bps_model_number_name)

#perform a left join with "combine_raw" and "scls_aoi_attributes" into a new dataframe called combine, while only selecting the second and fourth columns and joining them based on "Var2" in combine_raw and "value" in scls_aoi_attributes
combine <- left_join(combine_raw, 
                     scls_aoi_attributes %>%
                       dplyr::select(2, 4),  
                     by = c("Var2" = "VALUE"))


#similar to the function above, but this time selecting the first and fourth column and joining them based on "Var1" from "combine" and "value" in "LF16_BPS_200"
combine <- left_join(combine, 
                     LF16_BPS_200 %>%
                       dplyr::select(1:4),
                     by = c("Var1" = "VALUE"))


#Group the "combine" dataframe by "Var1" and "BPS_Model", mutate calculates the total count within each group, then calculates the percentage within each group, and then combining data in "BPS_Model" and "Label" into one column called "model_label"
combine <- combine %>%
group_by(Var1, BPS_MODEL) %>%
  mutate(total_count = sum(Freq)) %>%
  mutate(currentPercent = as.integer((Freq/total_count)*100)) %>%
  unite(model_label, c("BPS_MODEL", "LABEL"))



#Join the two data frames "aoi_ref_con" and "combine", drop NA values from refPercent, convert currentPercent into numeric data, remove total_count into numeric data, and then remove and NA values from total_count, and then renames the columns "Freq" to "count", "Var1" to "bps_value", "Var2" to "scl_value", and "BpS_Name" to "bps_name". clean_names() converts the column names to capitalize each word
aoi_ref_cur <- left_join(aoi_ref_con,
                         combine) %>%
  drop_na(refPercent) %>%
  mutate(currentPercent = as.numeric(currentPercent),
         currentPercent = ifelse(is.na(currentPercent), 0, currentPercent)) %>%
  mutate(total_count = as.numeric(total_count),
         total_count = ifelse(is.na(total_count), 0, total_count)) %>%
  select(-c(BPS_CODE, ZONE)) %>%
  select(c(Freq,
           Var1,
           Var2,
           BpS_Name,
           Model_Code,
           refLabel,
           model_label,
           refPercent,
           currentPercent,
           total_count)) %>%
  rename(count = Freq,
         bps_value = Var1,
         scl_value = Var2,
         bps_name = BpS_Name) %>%
  clean_names() 



#create a new dataframe "bps_scls_top" by the "model_code" column. It then checks if the total_count column is 0, in which case it is replaced with the max value of total_count to make sure that there are no 0 values in total_count.The data "total count" is then arranged in descending order using the arrange function. ungroup ensures that future operations in this code chunk apply to the entire dataframe. Then, the columns "bps_name", "ref_label", "current_percent" and "ref_percent" are selected and then the columns "ref_percent" and "current_percent" are pivoted and renamed to "ref_cur" and "percent". 
bps_scls_top <- aoi_ref_cur %>%
  group_by(model_code) %>%
  mutate(total_count = ifelse(total_count == 0, max(total_count), total_count)) %>%
  arrange(desc(total_count))  %>%
  ungroup() %>%
  dplyr::filter(dense_rank(desc(total_count)) < 4) %>%
  dplyr::select(c("bps_name", "ref_label",  "current_percent", "ref_percent")) %>%
  pivot_longer(
    cols = c(`ref_percent`, `current_percent`), 
    names_to = "ref_cur", 
    values_to = "percent"
  )


#convert the "ref_label" column in the "bps_scls_top" into categorical variables "Developed", "Agriculture", "UE", "UN", "E", "D", "C", "B", and "A" (aka the variables on the chart)
bps_scls_top$ref_label <- factor(bps_scls_top$ref_label, 
                                 levels = c(
                                   "Developed",
                                   "Agriculture",
                                   "UE",
                                   "UN",
                                   "E",
                                   "D",
                                   "C",
                                   "B",
                                   "A"))



#Make the chart
sclasplot <-
  ggplot(bps_scls_top, aes(fill = factor(ref_cur), y = percent, x = ref_label)) + 
  geom_col(width = 0.8, position = position_dodge()) +
  coord_flip() +
  facet_grid(. ~BpS) +
  scale_x_discrete(limits = (levels(bps_scls_top$ref_label))) +
  labs(
    title = "Succession Classes past and present",
    subtitle = "Top BpSs selected for illustration. Not all succession classes present in all BpSs",
    caption = "Data from landfire.gov.",
    x = "",
    y = "Percent") +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0, face = "italic"), 
        plot.title.position = "plot", 
        plot.caption.position =  "plot") +
  scale_fill_manual(values = c("#3d4740", "#32a852" ), 
                    name = " ", 
                    labels = c("Present",
                               "Past")) +
  facet_wrap(~bps_name, nrow(3),labeller = labeller(bps_name = label_wrap_gen())) +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

sclasplot
```

                                   


