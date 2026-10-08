# Load functions for retrieving Bluesky posts.
library(atrrr)
library(wordcloud)
library(RColorBrewer)
library(atrrr)
library(tm)
library(igraph)

# Use a Bluesky app password.
bsky_app_pw <- "jhog-jtdj-a2mf-zm74"
auth(user = "anii21.bsky.social", password = bsky_app_pw)

# Search Bluesky posts containing "Impact of generative AI".
# Posts are sorted from newest to oldest.
# The search is limited to posts from 1 January 2025 onwards,
# with a maximum of 200 posts returned.
search_skeets_AI = search_skeet(
  "Impact of generative AI",
  sort = "latest",
  since = "2025-01-01",
  limit = 200
)

# Extract the text of the collected Bluesky posts.
skeets_texts = search_skeets_AI$text

# Show the first three posts.
skeets_texts[1:3]

# Create a text corpus from the collected Bluesky posts.
Bluesky.corpus = Corpus(VectorSource(skeets_texts))

# Convert the text to UTF-8 encoding to handle different characters correctly.
Bluesky.corpus = tm_map(
  Bluesky.corpus, 
  function(x) iconv(x, to = "UTF-8", sub = "byte")
  )

# This code is given by AI to clean the URLs and bare domains like youtube.com/...
# Create a function to remove URLs and website domains from the posts.
# URLs are removed because they do not provide useful information
# for analysing the main textual content.
remove_urls = content_transformer(function(x) {
  x = gsub("https?://\\S+", " ", x, perl = TRUE)                       # http(s):// links
  x = gsub("\\b[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}(/\\S*)?\\b", " ", x, perl = TRUE)  # bare domains like youtube.com/...
  x
})

# Apply the URL removal function to the corpus.
corpus = tm_map(Bluesky.corpus, remove_urls)  
# Convert the text to ASCII and replace unsupported characters with spaces.
corpus = tm_map(corpus, content_transformer(
  function(x) iconv(x, to = "ASCII", sub = " "))
  )
# Remove numbers from the posts
corpus = tm_map(corpus, removeNumbers)
# Remove punctuation from the posts
corpus = tm_map(corpus, removePunctuation)
# Remove extra white space from the posts
corpus = tm_map(corpus, stripWhitespace)
# Convert all text to lowercase so that words such as "AI" and "ai"
# are treated as the same term.
corpus = tm_map(corpus, content_transformer(tolower))
# Remove common stopwords and additional words that were considered
# uninformative for this analysis.
corpus = tm_map(corpus, removeWords,
                c(stopwords(), "will", "can", "get", "use", "new", "people", "read", "discuss", "also"))
# Apply stemming 
corpus = tm_map(corpus, stemDocument)


# Create term-document matrix
# Rows represent terms and columns represent individual posts
tdm = TermDocumentMatrix(
  corpus,
  control = list(
    removePunctuation = TRUE,
    stopwords = TRUE,
    removeNumbers = TRUE,
    tolower = TRUE,
    stemming = TRUE
  )
)

# Identify documents that contain no terms after preprocessing
empties = which(colSums(as.matrix(tdm)) == 0)
# Remove empty documents from the term-document matrix.
tdm = tdm[, -empties]

# Convert to matrix
M = as.matrix(tdm)

#=========================
# Raw frequency
#=========================
# Calculate the total number of occurrences of each term
# across all posts
freqs = rowSums(M)
# Remove terms with missing frequency values.
freqs = freqs[!is.na(freqs)]

# Sort the terms by frequency and select the 20 most frequent terms
o = order(freqs, decreasing = TRUE)[1:20]
# Display the names of the 20 most frequent terms
names(freqs)[o]

# Create a word cloud showing frequently occurring terms.
# random.order = FALSE displays terms in frequency order.
# min.freq = 3 includes terms appearing at least three times.
# max.words = 100 limits the word cloud to 100 terms.
wordcloud(
  names(freqs),
  freqs,
  random.order = FALSE,
  min.freq = 3,
  max.words = 100,
  colors = brewer.pal(8, "Dark2"),
  scale = c(2, .5)
)
# Add a title to the raw-frequency word cloud
title("Genarative AI-related Bluesky Posts: Most Frequent Terms")

#==================
#TF-IDF
#===================
# TF-IDF weighting to identify terms that are relatively
# distinctive within individual posts
tdmw = weightTfIdf(tdm)

# Convert the weighted term-document matrix into a regular matrix
T = as.matrix(tdmw)

# Calculate the total TF-IDF weight of each term across the posts
freqsw = rowSums(T)

# Top 20 TF-IDF terms
o = order(freqsw, decreasing = TRUE)[1:20]
names(freqsw)[o]

# Create a word cloud showing terms with higher TF-IDF weights
wordcloud(
  names(freqsw),
  freqsw,
  random.order = FALSE,
  min.freq = 3,
  max.words = 100,
  colors = brewer.pal(8, "Dark2"),
  scale = c(1, .5)
)

# Add a title to the TF-IDF word cloud
title("Genarative AI-related Bluesky Posts: TF-IDF Terms")


#======================================================================================
# RQ2 - Is there a relationship between the topic/theme of the post and its engagement? 
#======================================================================================

# Calculate engagement by adding likes, reposts, and replies for each post
search_skeets_AI$engagement =
  search_skeets_AI$like_count +
  search_skeets_AI$repost_count +
  search_skeets_AI$reply_count

# Before asigning into any topic first give NA values to all coloumns 
# Each post will be assigned a topic based on keywords in its text
search_skeets_AI$topic = NA

# Classify posts containing environmental keywords as "Environment"
search_skeets_AI$topic[
  grepl("environment|climate|water|energy|carbon|emission|sustainab|planet|pollution|electricity|datacenter|data center|co2",
        search_skeets_AI$text,
        ignore.case = TRUE)
] = "Environment"

# Classify unassigned posts containing education-related keywords.
# is.na() ensures that posts already assigned to another topic
# are not reclassified.
search_skeets_AI$topic[
  is.na(search_skeets_AI$topic) &
    grepl("education|student|school|university|teacher|learning|learn|children|teach",
          search_skeets_AI$text,
          ignore.case = TRUE)
] = "Education"

# Classify remaining unassigned posts containing work-related keywords
search_skeets_AI$topic[
  is.na(search_skeets_AI$topic) &
    grepl("work|job|employment|worker|workplace|career|business|economy|economic|company|companies|executive|hr|market|consumer|finance",
          search_skeets_AI$text,
          ignore.case = TRUE)
] = "Work"

# Classify remaining unassigned posts containing social and ethical keywords
search_skeets_AI$topic[
  is.na(search_skeets_AI$topic) &
    grepl("society|social|ethic|ethical|democracy|trust|consent|public|attitude|health|regulation|protest|impact on people",
          search_skeets_AI$text,
          ignore.case = TRUE)
] = "Society_Ethics"


# Classify remaining unassigned posts containing technology and creativity keywords
search_skeets_AI$topic[
  is.na(search_skeets_AI$topic) &
    grepl("technology|software|creative|creativity|art|music|journalism|media|book|author|ai system|hallucination|innovation",
          search_skeets_AI$text,
          ignore.case = TRUE)
] = "Technology_Creativity"

#check topic count
table(search_skeets_AI$topic, useNA = "ifany")

# Create High / Low engagement groups
# using the median engagement
# if we arrange all numbers into ascending order the middle number is median
# then the left side is lower engagement levels and the right side is highest engagemnet levels 
median_engagement =
  median(
    search_skeets_AI$engagement,
    na.rm = TRUE
  )

# Classify posts with engagement equal to or above the median as "High".
# Posts below the median are classified as "Low"
search_skeets_AI$engagement_group =
  ifelse(
    search_skeets_AI$engagement >= median_engagement,
    "High",
    "Low"
  )


# Create a contingency table showing the number of posts
# in each topic and engagement group
tab = table(
  search_skeets_AI$topic,
  search_skeets_AI$engagement_group
)
# Display the contingency table
tab
sum(tab)

# Create a grouped bar chart comparing engagement levels across topics
barplot(
  tab,
  beside = TRUE,
  col = c("skyblue","plum1", "khaki", "pink", "tan"),
  legend.text = rownames(tab),
  ylab = "Number of posts",
  xlab = "Engagement level",
  main = "Topic and Engagement Level"
)


#State the hypotheses
# H0: Topic and engagement level are independent.
# HA: Topic and engagement level are not independent.

# Chi-squared test of independence
# simulate.p.value = TRUE estimates the p-value using simulation
ct = chisq.test(
  tab,
  simulate.p.value = TRUE
)
# Display the test results, including the p-value
ct

#Display expected frequencies
ct$expected
# Extract the p-value
ct$p.value


#====================================================================
# RQ 2.2 - Has the distribution of discussion topics about the 
# impact of generative AI changed between 2025 and 2026?
#====================================================================
# Search AI-related posts from 2025 only
search_skeets_AI_2025 = search_skeet(
    "Impact of generative AI",
    sort = "latest",
    since = "2025-01-01",
    until = "2025-12-31",
    limit = 200
  )
grep("creat|date|time|indexed", names(search_skeets_AI_2025), value = TRUE)
library(tm)
# Extract the year from each post's creation date
search_skeets_AI_2025$year =
  substr(search_skeets_AI_2025$indexed_at, 1, 4)

# Check the number of posts collected for each year
table(search_skeets_AI_2025$year, useNA = "ifany")

# Search for AI-related Bluesky posts published in 2026
search_skeets_AI_2026 = search_skeet(
  "Impact of generative AI",
  sort = "latest",
  since = "2026-01-01",
  until = "2026-12-31",
  limit = 200
)

# Extract the year from each post's creation date
search_skeets_AI_2026$year =
  substr(search_skeets_AI_2026$indexed_at, 1, 4)

# Check the number of posts collected for each year
table(
  search_skeets_AI_2026$year,
  useNA = "ifany"
)

# Check the total number of posts collected for 2026
nrow(search_skeets_AI_2026)

# remove posts with missing years
search_skeets_AI_2025$year <-
  substr(search_skeets_AI_2025$indexed_at, 1, 4)

table(search_skeets_AI_2025$year, useNA = "ifany")


# Create a function to classify posts into predefined topics
# based on keywords in their text (Same topics from RQ1)
classify_topic = function(data) {
  
  # First classified all column with missing values
  data$topic = NA
  
  # Assign posts containing environmental keywords to Environment
  data$topic[
    grepl(
      "environment|climate|water|energy|carbon|emission|sustainab|planet|pollution|electricity|datacenter|data center|co2",
      data$text,
      ignore.case = TRUE
    )
  ] = "Environment"
  
  data$topic[
    #to avoid same post classified into different topics....
    is.na(data$topic) &
      grepl(
        "education|student|school|university|teacher|learning|learn|children|teach",
        data$text,
        ignore.case = TRUE
      )
  ] = "Education"
  
  data$topic[
    is.na(data$topic) &
      grepl(
        "work|job|employment|worker|workplace|career|business|economy|economic|company|companies|executive|hr|market|consumer|finance",
        data$text,
        ignore.case = TRUE
      )
  ] = "Work"
  
  data$topic[
    is.na(data$topic) &
      grepl(
        "society|social|ethic|ethical|democracy|trust|consent|public|attitude|health|regulation|protest|impact on people",
        data$text,
        ignore.case = TRUE
      )
  ] = "Society_Ethics"
  
  data$topic[
    is.na(data$topic) &
      grepl(
        "technology|software|creative|creativity|art|music|journalism|media|book|author|ai system|hallucination|innovation",
        data$text,
        ignore.case = TRUE
      )
  ] = "Technology_Creativity"
  
  # Return the dataset with the assigned topic column
  return(data)
}

# Apply the same topic classification function to both datasets
search_skeets_AI_2025 =
  classify_topic(search_skeets_AI_2025)

search_skeets_AI_2026 =
  classify_topic(search_skeets_AI_2026)

# Combine the 2025 and 2026 data sets into one data set
RQ2_data = rbind(
    search_skeets_AI_2025,
    search_skeets_AI_2026
  )
# Remove posts that could not be assigned to a topic
RQ2_data = RQ2_data[
    !is.na(RQ2_data$topic),
  ]
# Create a contingency table showing the number of posts
# in each topic for each year
tab = table(
    RQ2_data$year,
    RQ2_data$topic
  )
tab


# Perform a chi-squared test to examine the association
# between publication year and topic
ct = chisq.test(tab)
ct

# Display expected frequencies under the null hypothesis
ct$expected

# Repeat the chi-squared test using a simulated p-value
# This helps when expected frequencies are small
ct_sim = chisq.test(
    tab,
    simulate.p.value = TRUE
  )
# Display the simulation-based test results
ct_sim
# Extract the simulation-based p-value
ct_sim$p.value


# =================================================================
# RQ3: Can the posts be clustered based on their textual content?
# =================================================================
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Use the TF-IDF matrix created in RQ1
# Transpose it so rows represent posts and columns represent terms
posts.matrix = t(as.matrix(T))
dim(posts.matrix)

# Find posts with no terms after cleaning
empties = which(rowSums(abs(posts.matrix)) == 0)

# Remove empty posts if any exist
if (length(empties) > 0) {
  posts.matrix = posts.matrix[-empties, , drop = FALSE]
}
# Check the size of the matrix after removing empty posts
dim(posts.matrix)


# Normalise each post so that its vector has a length of 1
norm.posts.matrix = posts.matrix / sqrt(rowSums(posts.matrix^2))

# Calculate cosine distance between post
# cosine distance = (euclidean distance^2) / 2
D = dist(norm.posts.matrix, method = "euclidean")^2 / 2

# Use MDS to represent the distance data in 50 dimensions
mds.posts.matrix = cmdscale(D, k = 50)
dim(mds.posts.matrix)

# # Use the elbow method to help choose the number of clusters
set.seed(123)   # so results are the same every run
n = 15
SSW = rep(0, n)

for (a in 1:n) {
  K = kmeans(mds.posts.matrix, centers = a, nstart = 20)
  SSW[a] = K$tot.withinss
}

# Plot the elbow graph to compare different cluster numbers
plot(
  1:n, SSW,
  type = "b",
  xlab = "Number of Clusters",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method for Selecting Number of Clusters"
)


# Compare cluster sizes for k = 2 to 6
for (k in 2:6) {
  set.seed(123)
  Ktmp = kmeans(mds.posts.matrix, centers = k, nstart = 20)
  cat("\nNumber of clusters:", k, "\n")
  print(table(Ktmp$cluster))
}

# Perform the final k-means clustering using 3 clusters
# K=3 got from the elbow plot we initially created
k.final = 3

set.seed(123)
K = kmeans(mds.posts.matrix, centers = k.final, nstart = 20)

# Display the number of posts in each cluster
table(K$cluster)

# Reduce the data to 2 dimensions for visualisation
mds2.posts.matrix = cmdscale(D, k = 2)

# Plot the posts using different colours for each cluster
plot(
  mds2.posts.matrix,
  col = K$cluster,
  xlab = "MDS Dimension 1",
  ylab = "MDS Dimension 2",
  main = "Clusters of Bluesky Posts"
)

# Add a legend to identify the clusters
legend(
  "topright",
  legend = 1:k.final,
  col = 1:k.final,
  pch = 1,
  title = "Cluster"
)

# Find the top 20 terms in each cluster
top.words = list()

for (cluster.number in 1:k.final) {
  
  # Select the posts belonging to the current cluster
  clusterpostsId = which(K$cluster == cluster.number)
  clusterposts = posts.matrix[clusterpostsId, , drop = FALSE]
  # Calculate the average TF-IDF weight of each term
  clusterTermWeight = colMeans(clusterposts)
  
  # Select the 20 terms with the highest average weights
  top = sort(clusterTermWeight, decreasing = TRUE)[1:20]
  top.words[[cluster.number]] = top
  
  # Display the top terms for each cluster
  cat("\n============================\n")
  cat("Cluster", cluster.number, "\n")
  cat("============================\n")
  print(top)
}

# Create a word cloud for each cluster using its top 20 terms
for (cluster.number in 1:k.final) {
  
  top = top.words[[cluster.number]]
  
  wordcloud(
    words = names(top),
    freq = top,
    random.order = FALSE,
    max.words = 20,
    colors = brewer.pal(8, "Dark2"),
    scale = c(2, 0.5)
  )
  
  title(paste("Cluster", cluster.number))
}

#===========================================================================
#RQ4.1: How are Bluesky users connected through discussions about the 
# impact of generative AI?
# Idea: build the network from REPLIES and MENTIONS 
#===========================================================================

library(atrrr)
library(igraph)

# Use the AI-related posts collected earlier
posts <- search_skeets_AI
posts <- posts[!duplicated(posts$uri), ]   # remove duplicate posts

# Get the author ID (DID) from each post
get_did <- function(uri) sub("^at://([^/]+)/.*", "\\1", uri)

# Create a table to match user IDs (DIDs) with user handles
did_to_handle <- setNames(posts$author_handle, get_did(posts$uri))
# Remove duplicate DIDs
did_to_handle <- did_to_handle[!duplicated(names(did_to_handle))]

# Convert a DID to a user handle when the handle is available
to_handle <- function(x) {
  out <- x
  known <- x %in% names(did_to_handle)
  out[known] <- unname(did_to_handle[x[known]])
  out
}

# Create connections from replies
# A replies to B, so the direction is A -> B
parent_did <- get_did(posts$in_reply_to)
is_reply   <- !is.na(parent_did)

el_replies <- cbind(
  from = posts$author_handle[is_reply],
  to   = to_handle(parent_did[is_reply])
)

# Create connections from mentions
# A mentions B, so the direction is A -> B 
el_mentions <- do.call(rbind, lapply(seq_len(nrow(posts)), function(i) {
  m <- unlist(posts$mentions[[i]])
  m <- m[!is.na(m)]
  if (length(m) == 0) return(NULL)
  cbind(from = rep(posts$author_handle[i], length(m)),
        to   = to_handle(m))
}))

# Combine replies and mentions into one edge list
el_talk <- rbind(el_replies, el_mentions)
# Remove connections where users connect to themselves
el_talk <- el_talk[el_talk[, "from"] != el_talk[, "to"], , drop = FALSE]  # no self-loops
# Check the number of connections and view the first few
dim(el_talk)
head(el_talk)

# Create a directed network
g_talk <- graph_from_edgelist(el_talk, directed = TRUE)


# Basic information about the network
vcount(g_talk)                              # Number of users
ecount(g_talk)                              # Number of connections
round(edge_density(g_talk), 4)              # Network density
components(g_talk)$no                       # number of separate groups
max(components(g_talk, mode = "weak")$csize) # size of the biggest group

# Identifies Who is central in the discussion? 
# In-degree = how many times someone is replied to or mentioned
cat("\nMost replied to / mentioned (in-degree):\n")
print(head(sort(degree(g_talk, mode = "in"), decreasing = TRUE), 5))

# Out-degree = who starts the most conversations
cat("\nMost active talkers (out-degree):\n")
print(head(sort(degree(g_talk, mode = "out"), decreasing = TRUE), 5))

# Betweenness = identifies users who connect different parts of the network
cat("\nBridges (betweenness):\n")
print(round(head(sort(betweenness(g_talk, directed = TRUE), decreasing = TRUE), 5), 4))

# PageRank = measures the importance of users in the network
cat("\nPageRank:\n")
print(round(head(sort(page_rank(g_talk, directed = TRUE)$vector, decreasing = TRUE), 5), 4))

# Degree distribution (compare with Barabasi-Albert vs Erdos-Renyi)
plot(degree_distribution(g_talk, mode = "all"), type = "h",
     xlab = "Degree", ylab = "Proportion",
     main = "Discussion network degree distribution")

# Find groups of users who talk together
g_und <- as.undirected(g_talk, mode = "collapse")
# Detect communities in the network
comm  <- cluster_walktrap(g_und)
# Show the size of each community
sizes(comm)
# Measure how clearly the communities are separated
modularity(comm)

# Plot the largest connected group
# This makes the network easier to read
comp   <- components(g_talk, mode = "weak")
big    <- which.max(comp$csize)

# Keep only the largest group
g_big  <- induced_subgraph(g_talk, V(g_talk)[comp$membership == big])
# Find communities in the largest group
comm_b <- cluster_walktrap(as.undirected(g_big, mode = "collapse"))

# Calculate the in-degree of users in the largest group
indeg <- degree(g_big, mode = "in")

# Create the network plot
# Larger nodes = more replies/mentions
# Different colours = different communities
plot(g_big,
     layout            = layout_with_fr(g_big),
     vertex.size       = 4 + 3 * sqrt(indeg),
     vertex.color      = membership(comm_b),
     vertex.label      = ifelse(indeg >= 3, V(g_big)$name, NA),  # label only the main users
     vertex.label.cex  = 0.6,
     edge.arrow.size   = 0.25,
     main = "Bluesky discussion network: generative AI")



#=============================================================================
# RQ 4.2 - Which accounts are the most central in the follow network 
# around the most-followed author among post about Generative AI
#=============================================================================

#identify seed user
# Retrieve profiles for each distinct post author
author_info <- get_user_info(unique(posts$author_handle))
# Display the five authors with the most followers
top_candidates <- head(
  author_info[
    order(author_info$followers_count,decreasing = TRUE), 
    c("actor_handle", "actor_name", "followers_count", "follows_count")
    ], 
  5)
top_candidates

# Select the author with the highest number of 
# followers as the seed user
seed_user_handle <- author_info$actor_handle[
  which.max(author_info$followers_count)
]
seed_user_handle

# The seed is selected only from authors of retrieved impact of generative AI posts. Sorting 
# current follower counts in descending order makes the first author the required seed.
# Posts, search rankings, follower counts and available accounts can change between runs.

# Get seed users friends
# Get accounts followed by the seed user
friends_handles <- get_follows(seed_user_handle, limit = 10)$actor_handle
friends_handles

#Get friends of friends
# Apply get_follows() to every direct friend
more_friends <- lapply(friends_handles, get_follows, limit = 20)
# Extract only the account handles from each result
more_friends_handles <- lapply(more_friends, `[[`, "actor_handle")
# Count the retrieved friends for each direct friend.
sapply(more_friends_handles, length)

#Create the directed edge list
#seq_along(friends_handles) gives the valid positions of the direct friends. If the list is empty, it returns
#integer(0), so lapply() performs zero iterations; 1:length(friends_handles) would instead return c(1,0).
# Make one row for each edge from the seed user to a direct friend.
el_seed <- cbind(
  # Repeat the seed handle once for every direct friend.
  from = rep(seed_user_handle, length(friends_handles)),
  # Use the direct-friend handles as the edge destinations.
  to = friends_handles
)
# For an empty list, seq_along() returns integer(0), so lapply() has no iterations.
# Make the second-level edges, one direct friend at a time.
el_friends <- lapply(seq_along(friends_handles), function(i) {
  # Repeat this direct friend's handle for each of their friends.
  cbind(from = rep(friends_handles[i],
                   length(more_friends_handles[[i]])),
        # Use that person's friends as the edge destinations.
        to = more_friends_handles[[i]])
})
# Join the first- and second-level edges into one edge list.
el <- unique(rbind(el_seed, do.call(rbind, el_friends)))
# Remove self-follows, where an account would point to itself.
el <- el[el[, "from"] != el[, "to"], , drop = FALSE]
# Show the number of rows and columns in the final edge list.
dim(el)
# Preview the first few follow relationships.
head(el)

#Note on seq_along(): It creates an index sequence with the same length as friends_handles. If there are no
#direct friends, it returns integer(0), and lapply() safely returns an empty list without running the function.
#This avoids the invalid positions from 1:length(friends_handles), which becomes c(1, 0) for an empty list.

#Each row means that the account in from follows the account in to.
# Build and plot the network
# Graph containing only the seed user and direct friends
g_seed <- graph_from_edgelist(el_seed, directed = TRUE)
# Full two-level network from the complete edge list
g <- graph_from_edgelist(el, directed = TRUE)
# Keep users connected to more than one account.
# This includes direct friends and shared second-level friends.
# Centrality is still calculated on g, not g2.
g2 <- induced_subgraph(g, V(g)[degree(g) > 1])
# Plot the seed user and direct friends only
plot(g_seed, layout = layout_with_fr(g_seed), vertex.size = 7)

#Plot all labels at a tiny size to show that the full network is unreadable.
plot(g, layout = layout_with_fr(g), vertex.size = 5,
     vertex.label.cex = 0.25)
# Plot the subgraph of shared second-level friends with Kamada-Kawai positioning
plot(g2, layout = layout_with_drl(g2), vertex.size = 7)

# Analyse the network
# all centrality results below are calculated on the entire graph g. 
# The smaller graph g2 is used only for visualisation 
# because removing vertices would change paths and centrality scores.

# Count the number of users
vcount(g)

# Count the number of follow connections
ecount(g)

round(edge_density(g), 4)

# Plot the degree distribution for shape comparison
plot(degree_distribution(g), type = "h",
     xlab = "Degree", ylab = "Proportion",
     main = "Bluesky degree distribution")

#The Bluesky degree distribution is more similar in shape to a Barabasi-Albert graph. It is right-skewed, with
#many users having few connections and a small number acting as high-degree hubs. The Erdos-Renyi distribution
#is more concentrated around a typical degree. This conclusion applies only to the collected sample.
# Highest degree scores are most central
head(sort(degree(g), decreasing = TRUE), 5)

# Highest igraph closeness scores are most central
round(head(sort(closeness(g, mode = "all"),
                decreasing = TRUE), 5), 4)
# Highest betweenness scores are most central
round(head(sort(betweenness(g, directed = TRUE),
                decreasing = TRUE), 5), 4)
#For these igraph outputs, the highest score wins for degree, closeness and betweenness. High degree identifies
#accounts with the largest total number of sampled connections. High closeness identifies short paths to other
#sampled accounts, while high betweenness identifies bridges. This differs from a manual total-distance calculation
#of closeness, where the lowest value wins. The graph is normally sparse because it contains only two limited
#outward layers.

