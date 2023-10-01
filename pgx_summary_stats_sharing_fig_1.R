data <- read.csv("Desktop/Desktop - Feryal's MacBook Air/Ovid/2023_08_28_updated_summary_final_excluded_samples_removed.csv", header =T)
#Get years
article_info <- read.csv("Desktop/Desktop - Feryal's MacBook Air/Ovid/Graphs/article_info_import_to_R.csv", header =T)
article_info <- dplyr::select(article_info, PMID, DATE)
old_article_info <- read.csv("Desktop/Desktop - Feryal's MacBook Air/Ovid/Graphs/old_Article_info_import_to_R.csv", header =T)
old_article_info <- dplyr::select(old_article_info, PMID, DATE)
combined <- rbind(article_info, old_article_info)
combined$year <- combined$DATE
combined$year <- sub("\\-.*", "", combined$year)
combined <- unique(combined)
data$year <- combined$year[match(data$PMID, combined$PMID)]
#Categorize
data_to_graph <- data
data_to_graph$new_categories <- data_to_graph$final_category
data_to_graph <- data_to_graph %>% 
  dplyr::mutate(across(new_categories, str_replace, "link_provided_in_article", "Data available")) %>%
  dplyr::mutate(across(new_categories, str_replace, "public_link_provided_on_request", "Data available")) %>%
  dplyr::mutate(across(new_categories, str_replace, "directed_to_more_recent_summary_statistics", "Data available")) %>%
  dplyr::mutate(across(new_categories, str_replace, "data_provided_on_request", "Data available")) %>%
  dplyr::mutate(across(new_categories, str_replace, "provided_incomplete_data", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "willing_to_share_but_data_pending", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "referred_to_another_source", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "requested_more_information", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "requested_formal_collaboration", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "require_data_access_agreement", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "no_time_resources_or_unable_to_locate_data", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "unable_unwilling_to_share_data", "Data unavailable at time of publication")) %>%
  dplyr::mutate(across(new_categories, str_replace, "no_response", "No response from authors")) %>%
  dplyr::mutate(across(new_categories, str_replace, "email_issue", "No response from authors"))
#Graph
library(data.table)
library(tidyverse)
library(PNWColors)
# # https://stackoverflow.com/questions/45730991/how-can-i-get-my-area-plot-to-stack-using-ggplot
data_numbers <- data_to_graph %>% count(`year`, new_categories) %>%
  spread(new_categories, n, fill = 0) %>%
  mutate_at(vars("Data available", "Data unavailable at time of publication", "No response from authors"), cumsum) %>%
  gather(new_categories, N, -`year`, factor_key = T) %>%
  mutate(new_categories = ordered(new_categories, levels = rev(levels(new_categories))))
# Make palette
pal=PNWColors::pnw_palette("Bay",3, type = "discrete")
ggplot2::ggplot(data_numbers, aes(x=as.numeric(year), y=N, fill=new_categories)) + 
  geom_area(position = 'stack', alpha=0.9) +
  theme_bw() + xlab("Publication Year") +
  ylab("Number of Articles") + ggtitle("Data Availibility Status") +
  scale_fill_manual(values=pal) +
  theme(legend.title=element_blank(), legend.position="bottom", legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=10, face="bold"),
        axis.text.y = element_text(size=10, face="bold")) +
  scale_x_continuous(breaks=seq(2007, 2022, 5))
#Save
ggplot2::ggsave("Desktop/Desktop - Feryal's MacBook Air/Ovid/DAS_image_86mm.tiff", width=86, height=60, units="mm", device="tiff", dpi=300, scale=2)
