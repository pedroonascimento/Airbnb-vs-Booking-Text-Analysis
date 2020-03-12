library(rvest)
library(magrittr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(scales)
library(sentimentr)

# This R Script will compare the information of the first 30 pages from Airbnb and Booking.com, from Trustpilot reviews' website
# First, we get the two companies' url

list_airbnb_pages <- str_c('https://www.trustpilot.com/review/www.airbnb.com?page=', 1:30)    # Getting the first 20 pages of Airbnb

list_booking_pages <- str_c('https://www.trustpilot.com/review/www.booking.com?page=', 1:30)  # Getting the first 20 pages of Booking.com

# Creating a function to get the reviews' text

geting_table <- function(html, company_name){
  
  # Extract the Basic information from the HTML
  review <- read_html(html) %>% html_nodes('.review-content__text') %>% html_text()
  
  # Append into a tibble
  append_table <- tibble(review = review) 
  
  # Tag the individual data with the company name
  append_table %>% 
    mutate(company = company_name) %>% 
    select(company, review)
}

scrape_write_table <- function(list_company_url, company_name){
  
  # Apply the extraction and bind the individual results back into one table, 
  list_company_url %>% 
    # Apply to all URLs
    map(geting_table, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows()     
}

airbnb <- scrape_write_table(list_airbnb_pages, 'airbnb')
booking <- scrape_write_table(list_booking_pages, 'booking')

# Now that we have 2 dataframes, one for each company. Thus, it's important to do two processes: tokenization and perform an 
# anti join with the stop_words dataset.

airbnb_tokens_nostop <- airbnb %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>%
  count(company,word, sort = TRUE)

booking_tokens_nostop <- booking %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>%
  count(company,word, sort = TRUE)

# Airbnb: -38.88519 | Booking: -46.58112
 airbnb_reviews  <- airbnb[,2]
 booking_reviews <- booking[,2]
 airbnb_reviews  <- sentiment(airbnb_reviews$review)
 booking_reviews <- sentiment(booking_reviews$review)
 airbnb_reviews %>% summarise(company_name = 'airbnb', overall_sentiment = sum(sentiment))
 booking_reviews%>%  summarise(company_name = 'booking.com', overall_sentiment = sum(sentiment))
 
 airbnb_refund <- filter(airbnb, grepl('refund', review))
 booking_refund <- filter(booking, grepl('refund', review))
 airbnb_refund  <- sentiment(airbnb_refund$review)
 booking_refund <- sentiment(booking_refund$review)
 airbnb_refund %>% summarise(company_name = 'airbnb', overall_sentiment = sum(sentiment))
 booking_refund%>%  summarise(company_name = 'booking.com', overall_sentiment = sum(sentiment))
 
# A preliminary analysis showed that most of experiences with 'refund' are bad. Basically, customers
# are not getting their money refunded. Therefore, the sentiment is gonna be changed to 'negative',
# and consider only the token 'refunded' as 'positive'.

filter(airbnb, grepl('refund', review)) %>% count()
filter(booking, grepl('refund', review)) %>% count()

# Finally, the main goals are: 
# 1) Count the frequency of the remaining tokens.
# 2) Perform an inner join with 2 sentiment libraries: afinn and bing.
# 3) Perfom some calculations and plot some graphics to generate business insights. The calculations are: summing the the 
# overall sentiment over the two companies', according to Trustpilot's reviews, and a count of the tokens per feeling. Then, 
# plotting the top 10/20 tokens, by positive and negative feeling and a correlogram to generate business insight. Last,

airbnb_afinn <- airbnb_tokens_nostop %>% inner_join(get_sentiments('afinn'))
airbnb_bing <- airbnb_tokens_nostop %>% inner_join(get_sentiments('bing'))
booking_afinn <- booking_tokens_nostop %>% inner_join(get_sentiments('afinn'))
booking_bing <- booking_tokens_nostop %>% inner_join(get_sentiments('bing'))

# 1) Performing the sum, using the afinn library

airbnb_afinn %>% 
  summarise(min_value = min(value), max_value = max(value), overall_value = sum(airbnb_afinn$value))
  
booking_afinn %>% 
  summarise(min_value = min(value), max_value = max(value), overall_value = sum(booking_afinn$value)) 
  

# 2) Working with the bing library

airbnb_bing %>% count(sentiment) %>% arrange(desc(n))
airbnb_bing$sentiment[airbnb_bing$word == 'refund'] <-  'negative' 
booking_bing %>% count(sentiment) %>% arrange(desc(n))
booking_bing$sentiment[booking_bing$word == 'refund'] <-  'negative' 

bind_rows(airbnb_bing %>% filter(sentiment == 'positive') %>% top_n(15, n), airbnb_bing %>% 
            filter(sentiment == 'negative') %>% top_n(15, n))%>%mutate(word2 = fct_reorder(word, n)) %>%
            ggplot(aes(x = word2, y = n, fill = sentiment)) + geom_col(show.legend = F) + theme_classic() +
            facet_wrap(~ sentiment, scales = "free") + coord_flip() + theme(plot.title = element_text(hjust = 0.5)) +
            labs(x = 'Words', y = 'Frequency', title = 'Airbnb - Top 15 Tokens - Bing Library')+  
            geom_text(aes(label = n), hjust = .6, size = 3)

bind_rows(booking_bing %>% filter(sentiment == 'positive') %>% top_n(15, n), booking_bing %>% 
            filter(sentiment == 'negative') %>% top_n(15, n))%>%mutate(word2 = fct_reorder(word, n)) %>%
            ggplot(aes(x = word2, y = n, fill = sentiment)) + geom_col(show.legend = F) + theme_classic() +
            facet_wrap(~ sentiment, scales = "free") + coord_flip() + theme(plot.title = element_text(hjust = 0.5)) +
            labs(x = 'Words', y = 'Frequency', title = 'Booking - Top 15 Tokens - Bing Library')+  
            geom_text(aes(label = n), hjust = .6, size = 3)

 # 3) Doing a correlogram between Airbnb x Booking.com

correlogram_table <- bind_rows(airbnb_bing, booking_bing) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `booking`)

ggplot(correlogram_table, aes(x=proportion, y=`airbnb`, 
                      color = sentiment))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  #scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=1)+
  #theme(legend.position = "none")+
  labs(y= "Airbnb", x=NULL) + theme_classic()
