setwd("C:/Users/jaidy/Desktop/W2024/W2024 - S1/COSC 421/Project")

library(dplyr)
library(tidyr)
library(igraph)

spotify_df <- read.csv("spotify-2023.csv", encoding = "latin1")

collaborations <- spotify_df %>% filter(artist_count > 1)

artist_pairs <- data.frame(Artist_1 = character(), Artist_2 = character(), stringsAsFactors = FALSE)

for (artists in collaborations$`artist.s._name`) {
  artist_list <- trimws(unlist(strsplit(artists, ",")))
  
  if (length(artist_list) > 1) {
    pairs <- combn(artist_list, 2, simplify = FALSE)
  
    for (pair in pairs) {
      artist_pairs <- rbind(artist_pairs, data.frame(Artist_1 = pair[1], Artist_2 = pair[2]))
    }
  }
}

artist_pairs_count <- artist_pairs %>%
  group_by(Artist_1, Artist_2) %>%
  summarize(Collaboration_Count = n()) %>%
  arrange(desc(Collaboration_Count))

graph <- graph_from_data_frame(artist_pairs_count, directed = FALSE)

plot(graph,
     vertex.size = 3,
     vertex.label.cex = 0.5,
     edge.width = artist_pairs_count$Collaboration_Count, 
     main = "Artist Collaboration Network", vertex.label = NA)


degree_centrality <- degree(graph, mode = "all")

betweenness_centrality <- betweenness(graph, directed = FALSE)

centrality_df <- data.frame(
  Artist = V(graph)$name,
  Degree_Centrality = degree_centrality,
  Betweenness_Centrality = betweenness_centrality
)

centrality_df <- centrality_df %>%
  arrange(desc(Degree_Centrality), desc(Betweenness_Centrality))

print(centrality_df)
