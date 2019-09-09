#Load libraries
library(tidyverse)
library(reshape2)
library(ChannelAttribution)
library(markovchain)

# Simulate marketing channel dataset
set.seed(42)
df <- data.frame(client_id = sample(c(1:1000), 5000, replace = TRUE),
                  date = sample(c(1:32), 5000, replace = TRUE),
                  channel = sample(c(0:9), 5000, replace = TRUE,
                                   prob = c(0.1, 0.15, 0.05, 0.07, 0.11, 0.07, 0.13, 0.1, 0.06, 0.16)))
df$date <- as.Date(df$date, origin = "2015-01-01")
df$channel <- paste0('channel_', df$channel)

head(df)

# Aggregate channels to the paths for each customer
df <- df %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = 1,
            conv_null = 0) %>%
  ungroup()

# Calculating the model - Markov chains Attribution
markov <- markov_model(df,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)

#Show results, transition matrix and removal_effects
head(markov$result)
tail(markov$transition_matrix)
markov$removal_effects

# Calculating heuristic models - First, Last & Linear Touch Attribution
heur <- heuristic_models(df,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_value = NULL,
                     sep = ">")

#Create heuristic models dataframe, df2
channel_name <- heur$channel_name
first_touch<- heur$first_touch
last_touch <- heur$last_touch
linear_touch <- round(heur$linear_touch,0)

df2 <- cbind(channel_name, first_touch, last_touch, linear_touch)

#Add markov chain and heuristic results to "results" df
markov_result <- markov$result %>%
  separate(channel_name, c(NA,'channel_name'), sep = '_') %>% 
  rename(markov_results = total_conversions) %>% 
  mutate(markov_results = round(markov_results, 0))

results <- merge(df2, markov_result, all.x = TRUE)

#Plot results
results2 <- gather(results, 'type', 'attr_count',2:5)
results2$channel_name <- as.factor(results2$channel_name)

ggplot(results2, aes(channel_name, attr_count, fill = type))+
  geom_bar(stat = 'identity', position= position_dodge())+
  theme_minimal()+
  theme(legend.position="bottom")+
  scale_y_continuous(breaks = seq(25,200,25))+
  xlab('Channel Name')+
  ylab('Conversions')+
  ggtitle('Multi-Touch Attribution Results')

#Heatmap of transition probabilities
trans_matrix <- markov$transition_matrix

ggplot(trans_matrix, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition Matrix Heatmap")

  