library(ggplot2)
library(dplyr)
library(readr)
library(mapdata)
library(maps)
library(pheatmap)

##Assignment 1 =====


##===== Accessing the Pteropodidae TSV File =====

# In order to access the Pteropodidae Family tsv file, we use the read_tsv function to download the TSV file from the BOLD API.
df_TSV <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Pteropodidae&format=tsv")


# To avoid the need to re-download the data every time the code is run (which saves time) and to ensure consistency and accessibility of the file, we will write the file to disk with write_tsv and then access the file from our local storage with read_tsv.
write_tsv(df_TSV, "Pteropodidae_BOLD_data.tsv")

df_TSV <- read_tsv(file = "../data/Pteropodidae_BOLD_data.tsv") #Adjust path as required.


##***************************

# Question 1: How does location impact genetic diversity (BINs) of the Southeast Asian Pteropodidae Family species of bat?

##***************************

##----- 1.1 Filtering & Cleaning Data -----

# In the previous section, the tsv file was read into the R session and assigned to the object df_TSV.

# We can view the first few rows of the dataset to understand its structure (the columns present and the type of data they hold) so we can find out what data the data frame holds and what can be used.

head(df_TSV)

# Display all column names and identify the relevant columns for our analysis: species_name, bin_uri, country, latitude (lat), and longitude (lon).
colnames(df_TSV)

# Create a new data frame that includes only the selected columns to simplify our analysis.
df_TSV_bat_countries <- df_TSV %>%
  select(species_name, bin_uri, country, lat, lon)  

head(df_TSV_bat_countries)  # Check the new data frame

# Clean NA values from data to ensure accurate analysis as NA values could lead to misleading results.
df_TSV_bat_country_noNA <- df_TSV_bat_countries %>%
  filter(!is.na(species_name) & !is.na(country) & !is.na(bin_uri) & !is.na(lat) & !is.na(lon))

head(df_TSV_bat_country_noNA)  # Review the cleaned data frame

# Define a vector of Southeast Asian countries to filter our dataset for relevant locations.
vec_SEA_countries <- c("Thailand", "Vietnam", "Malaysia", "Indonesia", "Philippines", "Singapore", "Brunei", "Myanmar", "Burma", "Cambodia", "Laos", "Timor-Leste", "East Timor")

# Filter the dataset to retain only records from Southeast Asian countries, focusing our analysis on this region.
df_TSV_SEA_bats <- df_TSV_bat_country_noNA %>%
  filter(country %in% vec_SEA_countries) 

df_TSV_SEA_bats  # Display the filtered data

# There are multiple entries for certain species. To determine how many unique BINs (DNA barcodes) exist in the Southeast Asian region, we need to consider only the unique values in the bin_uri column.


##----- 1.2 Finding Unique BINs from South East Asia -----

# Get the unique BINs in the Southeast Asia dataset to assess genetic diversity.
unique_BINs <- unique(df_TSV_SEA_bats$bin_uri)

# Count the number of unique BINs to quantify genetic diversity within the entire Southeast Asian bat population.
num_unique_BINs <- n_distinct(df_TSV_SEA_bats$bin_uri)
num_unique_BINs  

# Conclusion: We find that there are 35 unique BINs/species that belong to the Southeast Asian region as a whole. 

# Count unique BINs by country to understand how genetic diversity is distributed across Southeast Asia.
country_unique_bins <- df_TSV_SEA_bats %>%
  group_by(country) %>%  # Group the data by country
  summarise(num_unique_BINs = n_distinct(bin_uri), .groups = 'drop')  # Count unique BINs for each country

# Display the result
country_unique_bins

# Conclusion: We can conclude the genetic diversity of each of the Southeast Asian countries. Indonesia appears to be the most diverse, while Cambodia the least.


##----- 1.3a Visualizing SEA Bat Data: Map -----

# Get the world map data from ggplot2 to provide geographic context for the bat distribution.
map_data_world <- map_data("world")

# Filter for Southeast Asian countries to create a map showing the regions of interest.
map_data_SEA <- map_data_world %>%
  filter(region %in% vec_SEA_countries)

# View the map data to ensure correct filtering.
head(map_data_SEA)

# Plot the map with bat locations, providing a visual representation of bat distribution in Southeast Asia.
ggplot() +
  geom_polygon(data = map_data_SEA, aes(x = long, y = lat, group = group), 
               fill = "lightblue", color = "black") +  # Draw the country boundaries
  geom_point(data = df_TSV_SEA_bats, aes(x = lon, y = lat), 
             color = "red", size = 1, alpha = 0.7) +  # Add bat locations to the map
  theme_minimal() +  # Use a minimal theme for better clarity
  labs(title = "Figure 1: Distribution of Pteropodidae Species in Southeast Asia",
       x = "Longitude",
       y = "Latitude") +
  coord_fixed(1.3)  # Maintain aspect ratio for geographic accuracy


##----- 1.3b Visualizing SEA Bat Data: Bar Plot -----

# Create a bar plot of the number of unique BINs for each country
ggplot(country_unique_bins, aes(x = reorder(country, num_unique_BINs), y = num_unique_BINs)) +
  geom_bar(stat = "identity", fill = "darkred") +  # Create the bar chart
  labs(title = "Figure 2: Genetic Diversity of Pteropodidae Species in Southeast Asian Countries",
       x = "Country",
       y = "Number of Unique BINs") +
  theme_minimal() +  # Use a minimal theme for better aesthetics
  coord_flip()  # Flip the coordinates for better readability if there are many countries




##===== Question 2 =====

##***************************

# Question 2: How does genetic diversity (BINs) impact impact zoonotic transmission of  Southeast Asian Pteropodidae Family species?

##***************************

##----- 2.1 Creating a Data Frame: Susceptibility to Zoonotic Disease Transmission -----

# Create a new data frame with disease data for various bat species as the original TSV file lacks this information.
#Using data found from 'Parasite and viral species richness of Southeast Asian bats: Fragmentation of area distribution matters' by Gay et al. (2014). Table 1 from this journal article has been made into a data frame for the purpose of later merging with the data frame df_TSV_SEA_bats. 
df_SEA_Species_ZDT <- data.frame(
  species_name = c("Acerodon leucotis", "Aethalops alecto", "Aselliscus stoliczkanus", 
                   "Balionycteris maculata", "Cheiromeles torquatus", "Chironax melanocephalus", 
                   "Coelops frithii", "Cynopterus brachyotis", "Cynopterus horsfieldii", 
                   "Cynopterus sphinx", "Dobsonia exoleta", "Dyacopterus spadiceus", 
                   "Emballonura monticola", "Eonycteris major", "Eonycteris spelaea", 
                   "Eptesicus dimissus", "Eptesicus serotinus", "Glischropus tylopus", 
                   "Harpyionycteris whiteheadi", "Hesperoptenus blanfordi", "Hipposideros armiger", 
                   "Hipposideros bicolor", "Hipposideros cervinus", "Hipposideros cineraceus", 
                   "Hipposideros diadema", "Hipposideros galeritus", "Hipposideros larvatus", 
                   "Hipposideros lylei", "Hipposideros pomona", "Hipposideros pratti", 
                   "Ia io", "Kerivoula hardwickii", "Kerivoula minuta", "Kerivoula pellucida", 
                   "Macroglossus minimus", "Macroglossus sobrinus", "Megaderma lyra", 
                   "Megaderma spasma", "Miniopterus magnater", "Miniopterus medius", 
                   "Miniopterus pusillus", "Murina aurata", "Murina cyclotis", "Murina suilla", 
                   "Myotis adversus", "Myotis chinensis", "Myotis hasseltii", "Myotis horsfieldii", 
                   "Myotis muricola", "Nyctimene minutus", "Penthetor lucasi", 
                   "Philetor brachypterus", "Pipistrellus abramus", "Pipistrellus javanicus", 
                   "Pipistrellus stenopterus", "Ptenochirus jagori", "Pteropus giganteus", 
                   "Pteropus hypomelanus", "Pteropus vampyrus", "Rhinolophus acuminatus", 
                   "Rhinolophus affinis", "Rhinolophus lepidus", "Rhinolophus macrotis", 
                   "Rhinolophus malayanus", "Rhinolophus marshalli", "Rhinolophus pearsonii", 
                   "Rhinolophus pusillus", "Rhinolophus sinicus", "Rhinolophus stheno", 
                   "Rhinopoma microphyllum", "Rousettus amplexicaudatus", "Rousettus leschenaultii", 
                   "Saccolaimus saccolaimus", "Scotophilus kuhlii", "Tadarida jobensis", 
                   "Tadarida mops", "Tadarida plicata", "Taphozous longimanus", 
                   "Taphozous melanopogon", "Taphozous theobaldi", "Thoopterus nigrescens", 
                   "Tylonycteris pachypus", "Tylonycteris robustula"
  ), 
  endoparasite = c(0, 0, 0, 0, 1, 0, 2, 1, 1, 0, 0, 0, 0, 0, 3, 0, 
                   2, 1, 0, 1, 5, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 
                   0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 2, 0, 6, 0, 
                   2, 1, 1, 5, 1, 1, 1, 4, 0, 0, 0, 7, 0, 0, 0, 1, 3, 
                   0, 0, 0, 1
  ), 
  ectoparasite = c(0, 5, 1, 3, 10, 4, 0, 11, 4, 0, 1, 2, 5, 0, 20, 1, 
                   0, 1, 1, 0, 11, 9, 0, 6, 7, 4, 15, 1, 0, 0, 0, 1, 1, 
                   1, 6, 2, 7, 7, 0, 11, 0, 0, 1, 1, 1, 0, 1, 6, 0, 
                   1, 10, 0, 0, 1, 7, 0, 0, 3, 3, 1, 8, 6, 3, 1, 0, 
                   0, 0, 0, 6, 0, 9, 4, 3, 10, 1, 6, 11, 5, 9, 3, 1, 
                   9, 7
  ), 
  virus_sp = c(0, 0, 0, 0, 0, 0, 0, 6, 2, 1, 0, 0, 0, 0, 5, 
               0, 1, 0, 0, 0, 7, 0, 0, 0, 2, 0, 5, 0, 2, 0, 
               1, 0, 0, 0, 1, 0, 0, 0, 5, 0, 6, 1, 0, 0, 0, 
               1, 0, 2, 0, 0, 0, 1, 5, 0, 0, 1, 5, 4, 2, 0, 2, 
               0, 4, 0, 0, 3, 1, 6, 0, 0, 3, 4, 0, 7, 0, 0, 
               4, 0, 3, 2, 0, 2, 0
  )
)

df_SEA_Species_ZDT

#There are 83 SEA Bat species displayed in this data frame.


##----- 2.2 Filtering & Sorting Data -----

#Filter the df_TSV_SEA_bats data frame against the df_SEA_Species_ZDT data frame for overlapping species names.

df_combined <- merge(df_TSV_SEA_bats[, c("species_name", "bin_uri", "country")], 
                     df_SEA_Species_ZDT, 
                     by = "species_name")
df_combined

# Count unique BINs per species in df_combined
bin_count_per_species <- df_combined %>%
  group_by(species_name) %>%
  summarise(bin_count = n_distinct(bin_uri))
bin_count_per_species

# Merge BIN counts with zoonotic disease transmission data
df_final <- merge(bin_count_per_species, df_combined, by = "species_name")
df_final


##----- 2.3 Finding Correlation Values Between BIN Counts & Susceptibility to Zoonotic Disease Transmission -----

# Calculate correlation between BIN count and each zoonotic variable
cor_bin_endoparasite <- cor(df_final$bin_count, df_combined$endoparasite, use = "complete.obs")
cor_bin_ectoparasite <- cor(df_final$bin_count, df_combined$ectoparasite, use = "complete.obs")
cor_bin_virus_sp <- cor(df_final$bin_count, df_combined$virus_sp, use = "complete.obs")

# Display all correlation values
cor_bin_endoparasite
cor_bin_ectoparasite
cor_bin_virus_sp


##----- 2.4 Visualizing Data: Scatter Plots -----

#In order to demonstrate the relationship between genetic diversity (BINs) and each type of zoonotic disease, we can create a scatter plot for each.

# Scatter plot for Endoparasites
ggplot(df_final, aes(x = bin_count, y = df_combined$endoparasite)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +
  labs(title = "Figure 3: Correlation between BIN Count and Endoparasites", x = "BIN Count", y = "Endoparasites")

# Scatter plot for Ectoparasites
ggplot(df_final, aes(x = bin_count, y = df_combined$ectoparasite)) +
  geom_point() +
  geom_smooth(method = "lm", col = "green") +
  labs(title = "Figure 4: Correlation between BIN Count and Ectoparasites", x = "BIN Count", y = "Ectoparasites")

# Scatter plot for Virus Species
ggplot(df_final, aes(x = bin_count, y = df_combined$virus_sp)) +
  geom_point() +
  geom_smooth(method = "lm", col = "purple") +
  labs(title = "Figure 5: Correlation between BIN Count and Virus Species", x = "BIN Count", y = "Virus Species")



