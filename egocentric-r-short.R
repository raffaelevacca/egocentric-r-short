# Author: Raffaele Vacca <raffaele.vacca@unimi.it>
#
# 
# License: Creative Commons Attribution-NonCommercial-ShareAlike CC BY-NC-SA 
# http://creativecommons.org/licenses/by-nc-sa/3.0/

## ---- packages

# Load all packages used in this script
library(tidyverse)
library(igraph)
library(ggraph)
library(skimr)
library(janitor)
library(magrittr)

## ---- end-packages

############################################################################## #
###                      EGO LEVEL VS ALTER LEVEL DATA                      ====
############################################################################## #

## ---- levels

# Load the data.
load("./Data/data.rda")

# The file we just loaded includes the following objects.

# * Ego-level attribute data
ego.df

# * Attribute data for alters and ego-alter ties (all alters and ego-alter ties 
# in one data frame). 
alter.attr.all

# Level-1 join: Ego attributes into alter-level data (one to many).
(data <- left_join(alter.attr.all, ego.df, by= "ego_ID"))

# Note that the new data frame has one row for each alter, and the same ego ID
# and ego attribute value is repeated for all alters belonging to the same ego.
data %>% 
  dplyr::select(alter_ID, ego_ID, ego.age, ego.edu)

# ---- end-levels
############################################################################## #
###              EGO-NETWORKS AS IGRAPH OBJECTS                             ====
############################################################################## #
## ---- igraph

# The data.rda file, which we loaded earlier, also includes the ego-network of 
# ego 28 (as igraph object)
gr.28

# The graph is Undirected, Named, Weighted. It has 45 vertices and 259 edges. It
# has a vertex attribute called "name", and an edge attribute called "weight". 
# We see several vertex attributes (see codebook.xlsx for their meaning).

# Let's reassign it to a new, generic object name. This makes the code more 
# generic and more easily re-usable on any ego-network igraph object 
# (of any ego ID).
gr <- gr.28

# We also have the same network, with ego included: note the different number
# of vertices and edges compared to the previous graph.
gr.ego.28
gr.ego <- gr.ego.28

# Show the graph. Note that ego is not included.
set.seed(607)
ggraph(gr) + 
  geom_edge_link() + # Draw edges
  geom_node_point(size=5, color="blue") + # Draw nodes
  theme_graph(base_family = 'Helvetica') # Set graph theme and font


# Vertex and edge sequences and attributes                                  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Vertex sequences and edge sequences can be extracted from a graph.

# Vertex sequence of the whole graph. Notice that vertices are displayed by
# "names" (alter IDs in this case).
V(gr)

# Edge sequence of the whole graph. Notice that vertex names (alter IDs) are
# used here too.
E(gr)

# View vertex attributes and calculate statistics on them.

# Closeness of alters to ego.
V(gr)$alter.clo

# Mean closeness of alters to ego. 
V(gr)$alter.clo %>%
mean(na.rm=TRUE)

# The previous line uses the pipe operator: it's the same as the following
mean(V(gr)$alter.clo, na.rm=TRUE)

# More descriptive statistics on alter closeness.
V(gr)$alter.clo %>%
skimr::skim_tee() 

# Alter's country of residence.
V(gr)$alter.res

# Frequencies.
V(gr)$alter.res %>%
janitor::tabyl()

# Alter IDs are stored in the "name" vertex attribute
V(gr)$name

# Also edges have attributes, in this case "weight".
E(gr)$weight

# Frequencies of edge weights.
E(gr)$weight %>%
tabyl()


# Displaying vertex attributes in network visualization                     ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Let's visualize our ego-network, using different node colors for different 
# countries of residence of alters.

# Vertex attribute with alter's country of residence.
V(gr)$alter.res

# Plot with alter.res as node color
set.seed(607)
ggraph(gr) + 
  geom_edge_link() + # Draw edges
  geom_node_point(aes(color= alter.res), size=5) + # Draw nodes setting alter.res 
  # as node color and fixed node size
  theme_graph(base_family = 'Helvetica')

# Plot with alter ID labels instead of circles
set.seed(607)
ggraph(gr) + 
  geom_edge_link() +
  geom_node_label(aes(label= name)) + 
  theme_graph(base_family = 'Helvetica')

# Let's look at the same network, but with ego included
set.seed(607)
ggraph(gr.ego) + 
  geom_edge_link() +
  geom_node_label(aes(label= name)) + 
  theme_graph(base_family = 'Helvetica')


# ---- end-igraph

############################################################################## #
###                 MEASURES OF EGO-NETWORK COMPOSITION                     ====
############################################################################## #
## ---- composition

# For compositional measures all we need is the alter attribute data frame.
# The data.rda file loaded above includes the alter attribute data frame for
# ego ID 28.
alter.attr.28

# Compositional measures based on an alter attribute                  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Summary of continuous variable: Average alter closeness.

# Check out the relevant variable
alter.attr.28 %>%
  pull(alter.clo)

# Battery of descriptive stats.
alter.attr.28 %>%
  skim_tee(alter.clo) 

# Get a single summary measure (useful when writing functions)
mean(alter.attr.28$alter.clo, na.rm = TRUE)

# Summary of categorical variable: Proportion female alters.

# Check out the relevant vector
alter.attr.28$alter.sex

# Get frequencies
alter.attr.28 %>%
  tabyl(alter.sex)

# Same for nationalities
alter.attr.28 %>%
  tabyl(alter.nat)

# Another way to get the proportion of a specific category (this is useful when
# writing functions).
mean(alter.attr.28$alter.sex == "Female")
mean(alter.attr.28$alter.nat == "Sri Lanka")

# The function dplyr::summarise() allows us to calculate multiple measures, name
# them, and put them together in a data frame.
alter.attr.28 %>%
  summarise(
    mean.clo = mean(alter.clo, na.rm=TRUE), 
    prop.fem = mean(alter.sex=="Female"), 
    count.nat.slk = sum(alter.nat=="Sri Lanka"), 
    count.nat.ita = sum(alter.nat=="Italy"), 
    count.nat.oth = sum(alter.nat=="Other")
  )

# What if we want to calculate the same measures for all ego-networks in the data?
# We'll have to use the data frame with all alter attributes from all egos.
alter.attr.all

# dplyr allows us to "group" a data frame by a factor (here, ego IDs) so all
# measures we calculate on that data frame via summarise (means, proportions,
# etc.) are calculated by the groups given by that factor (here, for each ego
# ID).
alter.attr.all %>% 
  group_by(ego_ID) %>% 
  summarise(
    mean.clo = mean(alter.clo, na.rm=TRUE), 
    prop.fem = mean(alter.sex=="Female"), 
    count.nat.slk = sum(alter.nat=="Sri Lanka"), 
    count.nat.ita = sum(alter.nat=="Italy"), 
    count.nat.oth = sum(alter.nat=="Other")
  )

# We'll talk more about this and show more examples in the next sections.


# Compositional measures of homophily between ego and alters                ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Level-1 join: Bring ego-level data into alter-level data frame for ego 28.
(data.28 <- left_join(alter.attr.28, ego.df, by= "ego_ID"))

# Note the left join: We only retain rows in the left data frame (i.e., alters
# of ego 28), and discard all egos in the right data frame that do not
# correspond to those rows.

# Example: Proportion of alters of the same gender as ego.

# View the relevant data
data.28 %>% 
  dplyr::select(alter_ID, ego_ID, alter.sex, ego.sex)

# First create a vector that is TRUE whenever alter has the same sex as ego in 
# data.28 (the joined data frame for ego 28).
data.28$alter.sex == data.28$ego.sex

# The proportion we're looking for is simply the proportion of TRUE's in this
# vector.
mean(data.28$alter.sex == data.28$ego.sex)

# Similarly: count of alters who are in the same age bracket as the ego.
sum(data.28$alter.age.cat == data.28$ego.age.cat)

# Again, we can put these measures together into a data frame row with dplyr.
data.28 %>%
  summarise(
    prop.same.gender = mean(alter.sex == ego.sex), 
    count.same.age = sum(alter.age.cat == ego.age.cat)
  )

# ---- end-composition
############################################################################## #
###                    COMPOSITION OF MANY EGO-NETWORKS                     ====
############################################################################## #
## ---- split-df


# The summarise function offers a concise syntax to calculate summary 
# statistics on a data frame's variables. 

# Let's see what happens if we apply this function to the alter attribute data
# frame including all alters, without grouping it by ego ID.

# * Mean alter closeness:
alter.attr.all %>%
  summarise(mean.clo = mean(alter.clo, na.rm = TRUE))
# * N of distinct values in the alter nationality variable (i.e., number of 
# distinct nationalities of alters):
alter.attr.all %>%
  summarise(N.nat = n_distinct(alter.nat))
# * N of distinct values in the alter nationality, country of residence, and
# age bracket variables. In this case, we apply the same summarizing function
# to multiple variables (not just one), to be selected via across().
alter.attr.all %>%
  summarise(., across(c(alter.nat, alter.res, alter.age.cat), n_distinct))

# Because we ran this without previously grouping the data frame by ego ID, each
# function is calculated on all alters from all egos pooled (all rows of the
# alter attribute data frame), not on the set of alters of each ego.

# If we group the data frame by ego_ID, each of those summary statistics is
# calculated for each ego:

# * Mean alter closeness:
alter.attr.all %>%
  # Group by ego ID
  group_by(ego_ID) %>%
  # Calculate summary measure
  summarise(mean.clo = mean(alter.clo, na.rm = TRUE))

# * N of distinct values in the alter nationality variable (i.e., number of 
# distinct nationalities of alters):
alter.attr.all %>%
  group_by(ego_ID) %>%
  summarise(N.nat = n_distinct(alter.nat))

# * N of distinct values in the alter nationality, country of residence, and
# age bracket variables:
alter.attr.all %>%
  group_by(ego_ID) %>%
  summarise(., across(c(alter.nat, alter.res, alter.age.cat), n_distinct))

# We can save these results in a separate, ego-level dataset (each row is one 
# ego)
N.rel <- alter.attr.all %>%
  group_by(ego_ID) %>%
  summarise(., across(c(alter.nat, alter.res, alter.age.cat), n_distinct))

N.rel

# After getting compositional summary variables for each ego, we might want to
# join them with other ego-level data (level-2 join).

# Merge with summary variables.
ego.df %>%
  left_join(N.rel, by= "ego_ID")

# ---- end-split-df

############################################################################## #
###                   MEASURES OF EGO-NETWORK STRUCTURE                     ====
############################################################################## #
## ---- structure

# For structural measures we need the ego-network as an igraph.

# The data.rda file, which we loaded earlier, includes the igraph object of
# ego ID 28's network.
gr.28

# Let's reassign it to a new, generic object name. This makes the code more 
# generic and more easily re-usable on any ego-network igraph object 
# (of any ego ID).
gr <- gr.28

# Measures based only on the structure of alter-alter ties                  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Structural characteristics of the network.

# Network density.
edge_density(gr)

# Number of components.
components(gr)

# This is a list, we only need its 3rd element (number of components).
components(gr)$no

# Summarization of structural characteristics of the alters.

# Average alter degree.
degree(gr) %>% mean

# Max alter betweenness.
betweenness(gr, weights = NA) %>% max

# Number of "isolate" alters.

# Check out the alter degree vector.
degree(gr)

# Count the number of isolates, i.e. alters with degree==0
sum(degree(gr)==0)

# All sorts of more complicated structural analyses can be run on an ego-network
# using igraph or statnet functions. For example, here are the results of the
# Girvan-Newman community-detection algorithm on the ego-network.
cluster_edge_betweenness(gr, weights= NULL)

# What if we want to calculate the same structural measure (e.g. density) on all
# ego-networks? We can take the list of all ego-networks and run the same 
# function on every list element with the purrr package in tidyverse.

# List that contains all our ego-networks.
class(gr.list)
length(gr.list)
head(gr.list)

# We can use purrr::map() to run the same function on every element of the list.
gr.list %>%
  purrr::map_dbl(edge_density)

# We'll talk more about this and show more examples in the section about 
# multiple ego-networks.

# ---- end-structure

############################################################################## #
###                  SPLIT-APPLY-COMBINE ON LISTS VIA PURRR                 ====
############################################################################## #
## ---- split-list

# A simple structural measure such as network density can be applied to every
# ego-network (i.e., every element of our list), and returns a scalar for each
# network. All the scalars can be put in a vector.
gr.list %>%
  purrr::map_dbl(edge_density) 

# Note that the vector names (taken from gr.list names) are the ego IDs.

# If you want the same result as a nice data frame with ego IDs, use enframe().
gr.list %>%
  map_dbl(edge_density) %>% 
  enframe(name = "ego_ID", value = "density")

# Same thing, with number of components in each ego network. Note the ~ .x 
# syntax
gr.list %>%
  map_dbl(~ components(.x)$no) %>%
  enframe()
  

# With map_dfr() we can calculate multiple structural measures at once on every 
# ego-network, and return the results as a single ego-level data frame.

# This assumes that we are applying an operation that returns one data frame row
# for each ego-network. For example, take one ego-network from our list.
gr <- gr.list[[10]]

# Apply an operation to that ego-network, which returns one data frame row.
tibble(dens = edge_density(gr),
       mean.deg = mean(igraph::degree(gr)),
       mean.bet = mean(igraph::betweenness(gr, weights = NA)),
       deg.centr = centr_degree(gr)$centralization)

# Now we can do the same thing but for all ego-networks at once. The result is a 
# single ego-level data frame, with one row for each ego. 
# Note the .id argument in map_dfr().
gr.list %>%
  map_dfr(~ tibble(dens= edge_density(.x),
                   mean.deg= mean(igraph::degree(.x)),
                   mean.bet= mean(igraph::betweenness(.x, weights = NA)),
                   deg.centr= centr_degree(.x)$centralization),
          .id = "ego_ID")

# ---- end-split-list