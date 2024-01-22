#### Setting Up Environment ####
library(tidyverse)
#Load data
Subnational_HDI_data <- read_csv("GDL-Subnational-HDI-data.csv")
head(Subnational_HDI_data)

#### China ####
# Filter into a df with only data from China
china_data <- filter(Subnational_HDI_data, Country == "China" & Region != "Total")
head(china_data)

#make a long form table
china_data_long <- pivot_longer(china_data, cols = `1990`:`2021`, names_to = "Year", values_to = "Value")

#turn year into a factor
china_data_long$Year <- factor(china_data_long$Year)

head(china_data_long)

# Add HDI Categories 
china_data_long <- china_data_long %>%
  mutate(HDI_Category = cut(
    Value,
    breaks = c(-Inf, 0.549, 0.699, 0.799, Inf),
    labels = c("Low", "Medium", "High", "Very High")
  ))

# Reorganize by the HDI of the province in 1990
order <- china_data_long %>%
  filter(Year == "1990") %>%
  arrange(Value) %>%
  pull(Region)

china_data_long$Region <- factor(china_data_long$Region, levels = order)

# Plot
custom_colors <- c("Low" = "darkred", "Medium" = "orange", "High" = "deepskyblue", "Very High" = "darkblue")


CHINA<-ggplot(china_data_long, aes(x=Year, y=Region, fill=HDI_Category)) +
  geom_tile(colour="#fdf6e3", size=1, alpha=0.8) +  
  labs(title = "HDI Variation Across Chinese Provinces (1990-2021)", x = "Year", y = "Province", fill = "HDI Level", ) +
  scale_y_discrete(expand=c(0, 0)) +  
  scale_x_discrete(expand=c(0, 0), breaks=c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) +  
  scale_fill_manual(values = custom_colors) +
  theme_solarized(base_size=9) +  
  theme(
    plot.title = element_text(colour = "black",size=10),
    legend.text=element_text(face="bold", color="black"), 
    axis.ticks=element_line(size=.3),  
    panel.border=element_blank(),
    axis.title=element_text(face="bold", color="black"),
    legend.title=element_text(face="bold", color="black"),
  )

#### Mexico ####

# Filter into a df with only data from Mexico
mexico_data <- filter(Subnational_HDI_data, Country == "Mexico" & Region != "Total")
head(mexico_data)

# Make a long-form table
mexico_data_long <- pivot_longer(mexico_data, cols = `1990`:`2021`, names_to = "Year", values_to = "Value")

# Turn year into a factor
mexico_data_long$Year <- factor(mexico_data_long$Year)

# Add HDI Categories 
mexico_data_long <- mexico_data_long %>%
  mutate(HDI_Category = cut(
    Value,
    breaks = c(-Inf, 0.549, 0.699, 0.799, Inf),
    labels = c("Low", "Medium", "High", "Very High")
  ))

# Reorganize by the HDI of the province in 1990
order <- mexico_data_long %>%
  filter(Year == "1990") %>%
  arrange(Value) %>%
  pull(Region)

mexico_data_long$Region <- factor(mexico_data_long$Region, levels = order)

# Plot

ggplot(mexico_data_long, aes(x=Year, y=Region, fill=HDI_Category)) +
  geom_tile(colour="#fdf6e3", size=.5, alpha=0.9) + 
  labs( title = "HDI Variation Across Mexican States (1990-2021)", x = "Year", y = "States", fill = "HDI Level") +
  scale_y_discrete(expand=c(0, 0)) + 
  scale_x_discrete(expand=c(0, 0), breaks=c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) +
  scale_fill_manual(values = custom_colors) +
  theme_solarized(base_size=9) +  
  theme(
    plot.title = element_text(colour = "black",size=10),
    legend.text=element_text(face="bold", color="black"), 
    axis.ticks=element_line(size=.3),  
    panel.border=element_blank(),
    axis.title=element_text(face="bold", color="black"),
    legend.title=element_text(face="bold", color="black"),
  )

#### India ####
# Filter into a df with only data from India
India_data <- filter(Subnational_HDI_data, Country == "India" & Region != "Total")
head(India_data)

# Make a long-form table
India_data_long <- pivot_longer(India_data, cols = `1990`:`2021`, names_to = "Year", values_to = "Value")

# Turn year into a factor
India_data_long$Year <- factor(India_data_long$Year)

# Add HDI Categories 
India_data_long <- India_data_long %>%
  mutate(HDI_Category = cut(
    Value,
    breaks = c(-Inf, 0.549, 0.699, 0.799, Inf),
    labels = c("Low", "Medium", "High", "Very High")
  ))

# Reorganize by the HDI of the province in 1990
order <- India_data_long %>%
  filter(Year == "1990") %>%
  arrange(Value) %>%
  pull(Region)

India_data_long$Region <- factor(India_data_long$Region, levels = order)

# Plot

ggplot(India_data_long, aes(x=Year, y=Region, fill=HDI_Category)) +
  geom_tile(colour="#fdf6e3", size=.5, alpha=0.9) +  
  labs( title = "HDI Variation Across Indian States (1990-2021)", x = "Year", y = "State", fill = "HDI Level") +
  scale_y_discrete(expand=c(0, 0)) +  
  scale_x_discrete(expand=c(0, 0), breaks=c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) +  
  scale_fill_manual(values = custom_colors) +
  theme_solarized(base_size=9) +  
  theme(
    plot.title = element_text(colour = "black",size=10),
    legend.text=element_text(face="bold", color="black"), 
    axis.ticks=element_line(size=.3),  
    panel.border=element_blank(),
    axis.title=element_text(face="bold", color="black"),
    legend.title=element_text(face="bold", color="black"),
  )

#### Brazil ####
# Filter into a df with only data from Brazil
Brazil_data <- filter(Subnational_HDI_data, Country == "Brazil" & Region != "Total")
head(Brazil_data)

# Make a long-form table
Brazil_data_long <- pivot_longer(Brazil_data, cols = `1990`:`2021`, names_to = "Year", values_to = "Value")

# Turn year into a factor
Brazil_data_long$Year <- factor(Brazil_data_long$Year)

# Add HDI Categories 
Brazil_data_long <- Brazil_data_long %>%
  mutate(HDI_Category = cut(
    Value,
    breaks = c(-Inf, 0.549, 0.699, 0.799, Inf),
    labels = c("Low", "Medium", "High", "Very High")
  ))

# Reorganize by the HDI of the province in 2021
order <- Brazil_data_long %>%
  filter(Year == "1990") %>%
  arrange(Value) %>%
  pull(Region)

Brazil_data_long$Region <- factor(Brazil_data_long$Region, levels = order)

# Plot

ggplot(Brazil_data_long, aes(x=Year, y=Region, fill=HDI_Category)) +
  geom_tile(colour="#fdf6e3", size=.5, alpha=0.9) +  
  labs( title = "HDI Variation Across Brazilian States (1990-2021)", x = "Year", y = "States", fill = "HDI Level") +
  scale_y_discrete(expand=c(0, 0)) + 
  scale_x_discrete(expand=c(0, 0), breaks=c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) + 
  scale_fill_manual(values = custom_colors) +
  theme_solarized(base_size=9) +  
  theme(
    plot.title = element_text(colour = "black",size=10),
    legend.text=element_text(face="bold", color="black"), 
    axis.ticks=element_line(size=.3),  
    panel.border=element_blank(),
    axis.title=element_text(face="bold", color="black"),
    legend.title=element_text(face="bold", color="black"),
  )
#### Indonesia ####
# Filter into a df with only data from Indonesia
Indonesia_data <- filter(Subnational_HDI_data, Country == "Indonesia" & Region != "Total")
head(Indonesia_data)

# Make a long-form table
Indonesia_data_long <- pivot_longer(Indonesia_data, cols = `1990`:`2021`, names_to = "Year", values_to = "Value")

# Turn year into a factor
Indonesia_data_long$Year <- factor(Indonesia_data_long$Year)

# Add HDI Categories 
Indonesia_data_long <- Indonesia_data_long %>%
  mutate(HDI_Category = cut(
    Value,
    breaks = c(-Inf, 0.549, 0.699, 0.799, Inf),
    labels = c("Low", "Medium", "High", "Very High")
  ))

# Reorganize by the HDI of the province in 2021
order <- Indonesia_data_long %>%
  filter(Year == "1990") %>%
  arrange(Value) %>%
  pull(Region)

Indonesia_data_long$Region <- factor(Indonesia_data_long$Region, levels = order)

# Plot

ggplot(Indonesia_data_long, aes(x=Year, y=Region, fill=HDI_Category)) +
  geom_tile(colour="#fdf6e3", size=.5, alpha=0.9) +  
  labs( title = "HDI Variation Across Indonesian Provinces (1990-2021)", x = "Year", y = "Province", fill = "HDI Level") +
  scale_y_discrete(expand=c(0, 0)) + 
  scale_x_discrete(expand=c(0, 0), breaks=c("1990", "1995", "2000", "2005", "2010", "2015", "2020")) + 
  scale_fill_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_solarized(base_size=9) +  
  theme(
    plot.title = element_text(colour = "black",size=10),
    legend.text=element_text(face="bold", color="black"), 
    axis.ticks=element_line(size=.3),  
    panel.border=element_blank(),
    axis.title=element_text(face="bold", color="black"),
    legend.title=element_text(face="bold", color="black"),
  )