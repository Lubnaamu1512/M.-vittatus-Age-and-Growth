#NOAA type AGE BIAS PLOT
# Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)

file_path <- "C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/VER vs SO.xlsx"
data <- read_excel(file_path)

# NOAA-style calculations for Age Bias Graph
summary_data <- data %>%
  group_by(R1) %>%
  reframe(
    mean_R2 = mean(R2, na.rm = TRUE),
    sd_R2 = sd(R2, na.rm = TRUE),
    se_R2 = sd_R2 / sqrt(n()),
    n = n(),
    diff = mean_R2 - first(R1),
    percent_bias = (diff / first(R1)) * 100,
    t_value = diff / se_R2,
    p_value = 2 * pt(-abs(t_value), df = n - 1)
  )

print(summary_data)

# Age Bias Plot
ggplot(summary_data, aes(x = R1, y = mean_R2)) +
  # Correct 1:1 line from 0 to 6
  geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "red", linewidth = 1) +
  
  # Error bars for SE
  geom_errorbar(aes(ymin = mean_R2 - se_R2, ymax = mean_R2 + se_R2), width = 0.05, linewidth = 1) +
  
  # Mean points
  geom_point(size = 4, color = "blue") +
  
  # Set axis limits from 0 to 6
  scale_x_continuous(limits = c(0, 6), breaks = 0:6) +
  scale_y_continuous(limits = c(0, 6), breaks = 0:6) +
  
  # Titles and labels
  labs(
    title = "Age Bias Plot (Between Structures)",
    x = "Vertebrae",
    y = "Grated Otolith"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 30)

#IMPROVED PLOT (95%CI)
# Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)

file_path <- "C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/VER vs PS.xlsx"
data <- read_excel(file_path)

# NOAA-style calculations for Age Bias Graph
summary_data <- data %>%
  group_by(R1) %>%
  reframe(
    mean_R2      = mean(R2, na.rm = TRUE),
    sd_R2        = sd(R2, na.rm = TRUE),
    se_R2        = sd_R2 / sqrt(n()),
    n            = n(),
    diff         = mean_R2 - first(R1),
    percent_bias = ifelse(first(R1) == 0, NA, (diff / first(R1)) * 100),
    t_value      = ifelse(se_R2 > 0, diff / se_R2, NA),
    p_value      = ifelse(n > 1, 2 * pt(-abs(t_value), df = n - 1), NA)
  )

print(summary_data)

# Age Bias Plot
ggplot(summary_data, aes(x = R1, y = mean_R2)) +
  
  # 1:1 reference line
  geom_abline(slope = 1, intercept = 0,
              linetype = "solid", color = "red", linewidth = 1) +
  
  # 95% Confidence Interval error bars
  geom_errorbar(aes(ymin = mean_R2 - 1.96 * se_R2,
                    ymax = mean_R2 + 1.96 * se_R2),
                width = 0.05, linewidth = 1) +
  
  # Mean points
  geom_point(size = 4, color = "blue") +
  
  # Sample size labels
  #geom_text(aes(label = paste0("n=", n)),
            #vjust = -1.5, size = 5, color = "black") +
  
  # Axis limits
  scale_x_continuous(limits = c(0, 6), breaks = 0:6) +
  scale_y_continuous(limits = c(0, 6.5), breaks = 0:6) +
  
  # Labels
  labs(
    title = "Age Bias Plot (Between Structures)",
    x     = "Vertebrae",
    y     = "Sectioned Pectoral Spine"
  ) +
  
  # Theme
  theme_minimal(base_size = 30)
  #theme_classic(base_size = 14) +
  #theme(
   # plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    #axis.title = element_text(size = 13),
    #axis.text  = element_text(size = 11)
  #)


#save the plot
ggsave(
  filename = "C:/Users/lyasm/OneDrive/Desktop//R files/PA/BPA (MODAL)/Age_Bias_Plot_VER_vs_PS.jpg",
  plot = last_plot(),
  width = 12, height = 8,
  units = "in",
  dpi = 600
)


# Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)  # for combining plots

# =============================
# 1. Import Data
# =============================
data <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/PA/RPA/PS.xlsx")

# Check column names
names(data)
head(data)

# =============================
# 2. Function for Summary Stats
# =============================
bias_summary <- function(df, ref, comp) {
  df %>%
    group_by(across(all_of(ref))) %>%
    reframe(
      mean_comp    = mean(.data[[comp]], na.rm = TRUE),
      sd_comp      = sd(.data[[comp]], na.rm = TRUE),
      se_comp      = sd_comp / sqrt(n()),
      n            = n(),
      diff         = mean_comp - first(.data[[ref]]),
      percent_bias = ifelse(first(.data[[ref]]) == 0, NA,
                            (diff / first(.data[[ref]])) * 100),
      t_value      = ifelse(se_comp > 0, diff / se_comp, NA),
      p_value      = ifelse(n > 1, 2 * pt(-abs(t_value), df = n - 1), NA)
    ) %>%
    rename(ref_age = 1)
}

# =============================
# 3. Calculate for Each Pair
# =============================
sum_R1_R2 <- bias_summary(data, "R1", "R2")
sum_R1_R3 <- bias_summary(data, "R1", "R3")
sum_R2_R3 <- bias_summary(data, "R2", "R3")

# =============================
# 4. Function for Plotting
# =============================
bias_plot <- function(summary_df, xlab, ylab, title) {
  ggplot(summary_df, aes(x = ref_age, y = mean_comp)) +
    
    # 1:1 reference line
    geom_abline(slope = 1, intercept = 0,
                linetype = "solid", color = "red", linewidth = 1) +
    
    # 95% CI error bars
    geom_errorbar(aes(ymin = mean_comp - 1.96 * se_comp,
                      ymax = mean_comp + 1.96 * se_comp),
                  width = 0.05, linewidth = 0.8) +
    
    # Mean points
    geom_point(size = 2, color = "blue") +
    
    # Axis limits
    scale_x_continuous(limits = c(0, 6), breaks = 0:6) +
    scale_y_continuous(limits = c(0, 6.5), breaks = 0:6) +
    
    # ✅ THIS WAS MISSING — adds proper axis labels and title
    labs(x = xlab, y = ylab) +
    
    # Theme
    theme_minimal(base_size = 28)
}

# =============================
# 5. Create Individual Plots
# =============================
p1 <- bias_plot(sum_R1_R2,
                xlab  = "Reader 1",
                ylab  = "Reader 2"
                )

p2 <- bias_plot(sum_R1_R3,
                xlab  = "Reader 1",
                ylab  = "Reader 3"
                )

p3 <- bias_plot(sum_R2_R3,
                xlab  = "Reader 2",
                ylab  = "Reader 3"
                )

# =============================
# 6. Combine All 3 Plots
# =============================
combined_plot <- p1 + p2 + p3 +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Age Bias Plots — Pectoral Spine (Between Readers)",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
    )
  )

print(combined_plot)

# =============================
# 7. Save Combined Plot
# =============================
ggsave(
  filename = "C:/Users/lyasm/OneDrive/Desktop//R files/PA/RPA/Age_Bias_Plot_PS (R1+R2+R3).jpg",
  plot = last_plot(),
  width = 20, height = 6,
  units = "in",
  dpi = 600
)



#Precision statistical test
#Normality test
shapiro_test_result <- lapply(BPA, shapiro.test)
print(shapiro_test_result)

shapiro_test_result <- lapply(BPA[structure_cols], shapiro.test)
print(shapiro_test_result)

# ===== Load Required Libraries =====
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(PMCMRplus)
library(multcompView)

# ===== Step 1: Read Data =====
BPA <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/MEAN AGE.xlsx",
                  col_types = c("text", "skip", "numeric", "numeric", "numeric", "numeric", "numeric"))

# ===== Step 2: Setup Structure Columns and Age Matrix =====
structure_cols <- c("Mean_Ver", "Mean_Sec_Lap", "Mean_OB", "Mean_Lap", "Mean_Pec_Spine")
age_mat <- as.matrix(BPA[, structure_cols])
rownames(age_mat) <- BPA$Fish_ID

# ===== Step 3: Friedman Test (Base R) =====
friedman_result <- friedman.test(age_mat)
cat("===== Friedman Test Result =====\n")
print(friedman_result)

# ===== Step 4: Conover Post Hoc Test =====
conover_result <- frdAllPairsConoverTest(y = age_mat, p.adjust.method = "bonferroni")

# Extract and format results
z_df <- as.data.frame(as.table(conover_result$statistic))
p_df <- as.data.frame(as.table(conover_result$p.value))
colnames(z_df) <- c("Group1", "Group2", "Z_value")
colnames(p_df) <- c("Group1", "Group2", "p_value")

# Merge and annotate significance
# Convert to character BEFORE filtering
clean_results <- merge(z_df, p_df, by = c("Group1", "Group2")) %>%
  mutate(
    Group1 = as.character(Group1),
    Group2 = as.character(Group2)
  ) %>%
  filter(!is.na(Z_value), Group1 != Group2) %>%
  mutate(Significance = case_when(
    p_value < 0.0001 ~ "****",
    p_value < 0.001  ~ "***",
    p_value < 0.01   ~ "**",
    p_value < 0.05   ~ "*",
    TRUE             ~ "ns"
  )) %>%
  arrange(p_value)

cat("\n===== Conover Post Hoc Results =====\n")
print(clean_results, row.names = FALSE)

# ===== Step 5: Assign Superscripts (Manual Logic Based on Our Agreement) =====
# Manual superscripts based on Conover results:
# Mean_Ver          -> "a"
# Mean_Sec_Lap      -> "ab"
# Mean_OB           -> "b"
# Mean_Lap          -> "c"
# Mean_Pec_Spine    -> "d"

superscripts <- c(
  "Mean_Ver" = "a",
  "Mean_Sec_Lap" = "ab",
  "Mean_OB" = "b",
  "Mean_Lap" = "c",
  "Mean_Pec_Spine" = "d"
)

# Create long format from wide data
BPA_long <- BPA %>%
  pivot_longer(cols = all_of(structure_cols), names_to = "Structure", values_to = "Age")

# ===== Step 6: Prepare Data for Plotting =====
summary_df <- BPA_long %>%
  group_by(Structure) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    se_age = sd(Age, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    Superscript = superscripts[Structure],
    Display = case_when(
      Structure == "Mean_Ver" ~ "Vertebrae",
      Structure == "Mean_Sec_Lap" ~ "Grated Otolith",
      Structure == "Mean_OB" ~ "Opercular Bone",
      Structure == "Mean_Lap" ~ "Whole Otolith",
      Structure == "Mean_Pec_Spine" ~ "Sectioned\nPectoral Spine"
    ),
    Structure = factor(Structure, levels = structure_cols),
    Display = factor(Display, levels = c("Vertebrae", "Grated Otolith", "Opercular Bone", "Whole Otolith", "Sectioned\nPectoral Spine"))
  )
# ===== Step 7: Plot =====
ggplot(summary_df, aes(x = Display, y = mean_age, fill = Display)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_age - se_age, ymax = mean_age + se_age), width = 0.2) +
  geom_text(aes(label = Superscript, y = mean_age + se_age + 0.1), size = 8) +
  ylim(0, 3) +
  labs(
    title = "Mean Age Estimates by Structure\n(Friedman Test with Conover Post Hoc)",
    x = "Structures",
    y = "Mean Age Estimates (years)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(size = 17, angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 15)),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )
# Show the summary table used for plotting
summary_df %>%
  select(Display, mean_age, se_age, Superscript) %>%
  arrange(Display)

ggsave(
  "C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/Mean_Age_plot.jpg",
  dpi = 600,
  width = 10, height = 8, units = "in"
)

# Load package to read image metadata
library(magick)

# Read the JPG
img <- image_read("C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/Mean_Age_plot.jpg")

# Write out again with 600 dpi metadata
image_write(
  img,
  path = "C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/Mean_Age_plot_600dpi.jpg",
  format = "jpg",
  density = "600x600"
)

# Check info
info <- image_info(image_read("C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/Mean_Age_plot_600dpi.jpg"))
print(info)



#MIA
library(readxl)     # For reading Excel files
library(ggplot2)    # For plotting
head(MIA_R)
MIA_R$Months <- factor(MIA_R$Months, 
                          levels = c("January", "February", "March", "April", 
                                     "May", "June", "July", "August", 
                                     "September", "October", "November", "December"))

MIA_monthly_plot <- ggplot(MIA_R, aes(x = Months, y = `Average MIA`, group = 1)) +
  geom_line(color = "orangered3", linewidth = 1) +  # ← updated here
  geom_point(size = 3, color = "navy") +
  geom_errorbar(aes(ymin = `Average MIA` - SE, ymax = `Average MIA` + SE), 
                width = 0.2, linewidth = 0.6, color = "black") +
  theme_minimal() +
  labs(x = "Months",
       y = "Average Marginal Increment Analysis") +
  theme(
    axis.text = element_text(size = 20),         # Axis tick labels (months and numbers)
    axis.title.x = element_text(size = 22, margin = margin(t = 10)),  # Top margin
    axis.title.y = element_text(size = 22, margin = margin(r = 14)),   # Right margin
    axis.text.x = element_text(angle = 45, hjust = 1)  # Keep label rotation
  )

MIA_monthly_plot

ggsave(
  filename = "MIA_monthly_plot.png",       # Change to your preferred file name and path
  plot = last_plot(),                      # Saves the last created plot; use variable name if stored
  width = 15,                               # Set width in inches (customize as needed)
  height = 7,                              # Set height in inches (customize as needed)
  dpi = 600,                               # Specifies 600 dpi resolution
  bg = "white"
)


#SMOOTH LINE
# First install ggalt if not already installed
#install.packages("ggalt")
# Load necessary packages
library(ggplot2)
library(ggalt)
library(ggplot2)
library(dplyr)

# Interpolate a smooth curve using spline
spline_data <- as.data.frame(spline(MIA_R$MonthNum, MIA_R$`Average MIA`, n = 80))
colnames(spline_data) <- c("MonthNum", "MIA")

# Plot
ggplot() +
  geom_line(data = spline_data, aes(x = MonthNum, y = MIA), color = "orangered3", linewidth = 1) +
  geom_point(data = MIA_R, aes(x = MonthNum, y = `Average MIA`), size = 3, color = "navy") +
  geom_errorbar(data = MIA_R,
                aes(x = MonthNum, ymin = `Average MIA` - SE, ymax = `Average MIA` + SE),
                width = 0.2, linewidth = 0.6, color = "black") +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December")
  ) +
  theme_minimal() +
  labs(title = "Monthly MIA with Standard Error",
       x = "Months",
       y = "Average Marginal Increment Ratio") +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 19, margin = margin(t = 10)),
    axis.title.y = element_text(size = 19, margin = margin(r = 16)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#==============PRECISION RESULTS USING R functions=============================

#==============================================================================
#              BETWEEN STRUCTURES
#==============================================================================
library(FSA)
library(readxl)
library(writexl)

# =============================
# 1. Import your Excel data
# =============================
ages <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/PA/BPA (MODAL)/VER vs PS.xlsx")

head(ages)

# =============================
# 2. Precision Metrics
# =============================
prec_results <- agePrecision(~R1+R2, data=ages)

# Look at all results
prec_results

# Extract metrics properly
pa  <- prec_results$PercAgree       # Percent agreement (exact match)
ape <- prec_results$APE             # Average Percent Error
cv  <- prec_results$ACV             # Coefficient of Variation

# Summary table
summary_table <- data.frame(
  Metric = c("Percent Agreement (Exact)",
             "Average Percent Error",
             "Coefficient of Variation"),
  Value = c(pa, ape, cv)
)

print(summary_table)

# =============================
# 3. Age-Bias Test
# =============================
# Age-Bias Test (Reader 2 vs Reader 1)
ab <- ageBias(R2 ~ R1, data=ages)
print(ab)

# Use the ageBias result directly
plotAB(ab,
       xlab="Vertebrae",
       ylab="Pectoral spine",
       main="Age-Bias Plot (R1 vs R2)")


summary(ab, what="Bowker")
summary(ab, what="EvansHoenig")


# =============================
# 5. Save Results
# =============================
#write_xlsx(summary_table,
           #"C:/Users/lyasm/OneDrive/Desktop/R files/PA/RPA/PrecisionResults.xlsx")

# ================== END SCRIPT ==================

#=================Regression fit======================================
# Load package
library(car)

# Fit regression model: Reader 2 ~ Reader 1
fit <- lm(R2 ~ R1, data = ages)

# Check regression summary
summary(fit)

# Test slope = 1 and intercept = 0
linearHypothesis(fit, c("(Intercept) = 0", "R1 = 1"))

#=====MID LINE AND READERS LINE GRAPH===========================================
# Load ggplot2
library(ggplot2)

# Age-bias plot
ggplot(ages, aes(x = R1, y = R2)) +
  geom_point(alpha = 0.6, size = 2) +                                   # observed points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +  # 1:1 line
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +    # fitted regression line
  labs(
    x = "Reader 1 Age",
    y = "Reader 2 Age",
    title = "Age-Bias Plot: Reader 2 vs Reader 1"
  ) +
  theme_minimal(base_size = 14)

citation("friedman.test")


#------------------------------------------------------------------------------
#===============PRESCISION WITH 3 READERS======================================
#------------------------------------------------------------------------------
library(FSA)
library(readxl)
library(writexl)

# =============================
# 1. Import your Excel data
# =============================
ages <- read_excel("C:/Users/lyasm/OneDrive/Desktop/R files/PA/RPA/PS.xlsx")
head(ages)

# =============================
# 2. Precision Metrics (ALL 3 READERS)
# =============================
prec_results <- agePrecision(~R1 + R2 + R3, data=ages)

# View results
prec_results

# Extract metrics
pa  <- prec_results$PercAgree
ape <- prec_results$APE
cv  <- prec_results$ACV

# Summary table
summary_table <- data.frame(
  Metric = c("Percent Agreement (Exact)",
             "Average Percent Error",
             "Coefficient of Variation"),
  Value = c(pa, ape, cv)
)

print(summary_table)

# Optional: save results
#write_xlsx(summary_table,
           #"C:/Users/lyasm/OneDrive/Desktop/R files/Precision_summary_3readers.xlsx")

#EXPERT READER VS OTHER READERS
agePrecision(~R1+R2, data=ages)
agePrecision(~R1+R3, data=ages)
agePrecision(~R2+R3, data=ages)


#====AGE BIAS TEST (R1 VS R2)==================================================
ab21 <- ageBias(R2 ~ R1, data=ages)

print(ab21)

plotAB(ab21,
       xlab="Reader 1 (R1)",
       ylab="Reader 2 (R2)",
       main="Age-Bias Plot (R2 vs R1)")

summary(ab21, what="Bowker")
summary(ab21, what="EvansHoenig")

#======(R1 vs R3)==============================================================
ab31 <- ageBias(R3 ~ R1, data=ages)

print(ab31)

plotAB(ab31,
       xlab="Reader 1 (R1)",
       ylab="Reader 3 (R3)",
       main="Age-Bias Plot (R3 vs R1)")

summary(ab31, what="Bowker")
summary(ab31, what="EvansHoenig")

#======(R2 vs R3)==============================================================
ab32 <- ageBias(R3 ~ R2, data=ages)

print(ab32)

plotAB(ab32,
       xlab="Reader 2 (R2)",
       ylab="Reader 3 (R3)",
       main="Age-Bias Plot (R3 vs R2)")

summary(ab32, what="Bowker")
summary(ab32, what="EvansHoenig")

