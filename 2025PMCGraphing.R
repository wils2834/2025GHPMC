GH2025 <- read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\Root Greenhouse 2025\\Git hub for GH\\2025GHPMC\\PMC_DR2025_all_calcsBadjustedmeans.csv", header = TRUE)

library(ggplot2)
library(ggpubr)
library(lme4)
library(emmeans)
library(multcompView)
library(dplyr)
library(multcomp)

# Remove blank rows and empty sample types
GH2025 <- GH2025 %>%
  filter(UID != "blank") %>%
  filter(!is.na(Type_of_Sample), Type_of_Sample != "")

# Define the desired order of timepoints
levels <- c("Pre-Collection", "1daypost-firstcut", "1daypre-secondcut",
            "1daypost-secondcut","1daypre-thirdcut","1daypost-thirdcut",  
            "1daypre-termination", "termination")

# Convert Type_of_Sample to factor with specified order
GH2025$Type_of_Sample <- factor(GH2025$Type_of_Sample, levels = levels)

# Summary statistics by timepoint (original scale)
GH2025 %>%
  group_by(Type_of_Sample) %>%
  summarize(
    avg_value = mean(mean_ugC.g.day, na.rm = TRUE),
    sd_value = sd(mean_ugC.g.day, na.rm = TRUE),
    n = n()
  )
# Create unique core identifier
GH2025 <- GH2025 %>%
  mutate(Core_ID = paste(Group_of_Cores, Rep_in_Field, sep = "_"))

# Check structure
GH2025 %>% count(Core_ID)

# Create square root transformation
GH2025 <- GH2025 %>%
  mutate(sqrt_ugC = sqrt(mean_ugC.g.day))

# Mixed-effects model with Core_ID as random effect
model_mixed_final <- lmer(sqrt_ugC ~ Type_of_Sample + Depth + (1 | Core_ID), 
                          data = GH2025)
summary(model_mixed_final)

# Check assumptions
plot(model_mixed_final)
qqnorm(residuals(model_mixed_final))
qqline(residuals(model_mixed_final))

# Get estimated marginal means
emm_final <- emmeans(model_mixed_final, ~ Type_of_Sample)
print(emm_final)

# Pairwise comparisons
pairs_final <- pairs(emm_final, adjust = "tukey")
print(pairs_final)

# Compact letter display
cld_final <- cld(emm_final, Letters = letters, adjust = "tukey")
print(cld_final)

# Back-transform for plotting
cld_df <- as.data.frame(cld_final)
cld_df$Type_of_Sample <- factor(cld_df$Type_of_Sample, levels = levels)
cld_df$backtransformed_mean <- (cld_df$emmean)^2
# Rename .group to label
cld_df <- cld_df %>%
  rename(label = .group)

print(cld_df[, c("Type_of_Sample", "backtransformed_mean", "label")])


# Publication-quality boxplot (on ORIGINAL scale for interpretability)
ggplot(GH2025, aes(x = Type_of_Sample, y = mean_ugC.g.day)) +
  geom_boxplot(aes(fill = Type_of_Sample), color = "black", alpha = 0.6) +
  geom_text(
    data = cld_df,
    aes(
      x = Type_of_Sample,
      y = max(GH2025$mean_ugC.g.day, na.rm = TRUE) + 20,
      label = label
    ),
    inherit.aes = FALSE,
    size = 7,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    title = "Carbon Mineralization by Collection Timepoint",
    x = NULL,
    y = expression(paste(mu, "g C g"^-1, " day"^-1))
  ) +
  scale_x_discrete(labels = c(
    "Pre-Collection" = "Pre-\nCollection",
    "1daypost-firstcut" = "1 Day Post\nFirst Cut",
    "1daypre-secondcut" = "1 Day Pre\nSecond Cut",
    "1daypost-secondcut" = "1 Day Post\nSecond Cut",
    "1daypre-thirdcut" = "1 Day Pre\nThird Cut",
    "1daypost-thirdcut" = "1 Day Post\nThird Cut",
    "1daypre-termination" = "1 Day Pre\nTermination",
    "termination" = "Termination"
  )) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold", color = "black"),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    legend.position = "none"
  )

# Alternative: Boxplot with cleaner labels
ggplot(GH2025, aes(x = Type_of_Sample, y = mean_ugC.g.day)) +
  geom_boxplot(aes(fill = Type_of_Sample), color = "black", alpha = 0.6) +
  geom_text(data = cld_df, aes(x = Type_of_Sample, label = label),  
            y = max(GH2025$mean_ugC.g.day, na.rm = TRUE) + 15,
            inherit.aes = FALSE, size = 4.5, fontface = "bold", color = "black") +
  labs(
    title = "Carbon Mineralization By Collection Timepoint",
    x = NULL,
    y = expression(paste(mu, "g C g"^-1, " day"^-1))
  ) +
  scale_fill_manual(values = c(
    "Pre-Collection" = "#E69F00",           # Orange
    "1daypost-firstcut" = "#56B4E9",        # Sky blue
    "1daypre-secondcut" = "#009E73",        # Green
    "1daypost-secondcut" = "#F0E442",       # Yellow
    "1daypre-thirdcut" = "#0072B2",         # Dark blue
    "1daypost-thirdcut" = "#D55E00",        # Red-orange
    "1daypre-termination" = "#800020",      # Maroon
    "termination" = "#228B22"               # Forest green
  )) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 14, face = "bold", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

