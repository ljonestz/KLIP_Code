######################################################################################################
#           DATA AND ANALYSIS FOR KLIP KENYA SURVEY
#           AUTHOR: LINDSEY JONES, WORLD BANK
#           DATE: SEPT 2020
######################################################################################################

#------------------- list of issues to fix ---------------------------

# Trouble with klip2$SECTION2HOUSEHOLDCHARACTERISTICS.Q201g
# Confusing SECTION2HOUSEHOLDCHARACTERISTICS.Q201g; SECTION2HOUSEHOLDCHARACTERISTICS.Q201g; SECTION2HOUSEHOLDCHARACTERISTICS.Q202a9; SECTION2HOUSEHOLDCHARACTERISTICS.Q202a9
# Look at 806 and compare beneficiaries and non. Answers look the same event though different Qs

#
##
### --------------------LOAD PACKAGES--------------------------
##
#

#install.packages("librarian")    # Install if librarian is not already loaded
librarian::shelf(
  sjPlot, estimatr, ggthemes, reshape, ggthemr, gridExtra,
  readxl, tidyr, forcats, huxtable, ggplot2, ggthemr, dplyr,
  lubridate, foreign, kableExtra, knitr, stargazer, haven, tab,
  styler, sjlabelled, lm.beta, stringr
)

#
##
### --------------------SET UP KLIP DATABASE (BENEFICIARIES)--------------------
##
#

# Create beneficiary dataframe
klip2 <- read_dta("~/Dropbox/KLIP Consumer Value Lindsey/dta-files/Beneficiaries.dta")
klip2 <- as.data.frame(klip2)

# Make colnames as attributes
attr(klip2$endtime, "label") <- "endtime"
attr(klip2$today, "label") <- "today"

colnames(klip2) <- unlist(lapply(colnames(klip2), function(xx) {
  attr(klip2[, xx], "label")
}))

klip2$treat <- "Beneficiary"

# Load question responses
klipchoice <- read_excel("~/Dropbox/KLIP Consumer Value Lindsey/Titus data/Analysis frame KLIP Percep study/KLIP-Beneficiaries-Codebook.xlsx", sheet = "choices")

# format names NEED TO ADDRESS ISSUE WITH 201g and Q501c
klipnames <- gsub("choices_", "", klipchoice$`list name`)
klipnames2 <- gsub("_", ":", klipnames)
klipchoice$`list name` <- klipnames2

kliplevels <- klipchoice %>%
  spread(`list name`, `label::English`) %>%
  mutate_if(is.character, as.factor)
rownames(kliplevels) <- kliplevels$name

kliplevels <- kliplevels %>%
  dplyr::select(-Consent, -County, -`SECTION2HOUSEHOLDCHARACTERISTICS:Q201g`, -`Section5:Q501c`, -`Section5:Q506`)

colnames(kliplevels) <- make.names(colnames(kliplevels))
colnames(klip2) <- make.names(colnames(klip2))

# Fix blank space
klip2$`Section7FinancialServices.Q705` <- ifelse(klip2$`Section7FinancialServices.Q705` == "", "99", klip2$`Section7FinancialServices.Q705`)

# Convert character to factor
klip2[colnames(kliplevels[, 2:111])] <- lapply(klip2[colnames(kliplevels[, 2:111])], function(dd) {
  as.factor(dd)
})

# Merge factor levels between datasets (except for name)
for (i in colnames(kliplevels[, 2:111])) {
  lev <- kliplevels %>%
    dplyr::select(name, i) %>%
    drop_na()
  levels(klip2[[i]]) <- lev[[2]][lev[[1]] %in% levels(klip2[[i]])]
}

# characters to factors
klip2 <- klip2 %>%
  mutate_if(is.character, as.factor)

#Remove abnormal cattle loss
klip2 <- klip2%>%
  filter(!SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18==5000)

#
##
### --------------------SET UP KLIP DATABASE (NON-BENEFICIARIES)--------------------
##
#

# Create non-beneficiary dataframe
klipnon2 <- read_dta("~/Dropbox/KLIP Consumer Value Lindsey/dta-files/NonBeneficiaries.dta")
klipnon2 <- as.data.frame(klipnon2)

# Make colnames as attributes
attr(klipnon2[, 2], "label") <- colnames(klipnon2[2])
attr(klipnon2[, 3], "label") <- colnames(klipnon2[3])
attr(klipnon2[, 4], "label") <- colnames(klipnon2[4])
attr(klipnon2[, 5], "label") <- colnames(klipnon2[5])
attr(klipnon2[, 6], "label") <- colnames(klipnon2[6])
attr(klipnon2[, 7], "label") <- colnames(klipnon2[7])
attr(klipnon2[, 8], "label") <- colnames(klipnon2[8])
attr(klipnon2[, 9], "label") <- colnames(klipnon2[9])
attr(klipnon2[, 10], "label") <- colnames(klipnon2[10])
attr(klipnon2[, 11], "label") <- colnames(klipnon2[11])
attr(klipnon2[, 12], "label") <- colnames(klipnon2[12])
attr(klipnon2[, 238], "label") <- colnames(klipnon2[238])
attr(klipnon2[, 239], "label") <- colnames(klipnon2[239])
colnames(klipnon2) <- unlist(lapply(colnames(klipnon2), function(xx) {
  attr(klipnon2[, xx], "label")
}))

# Load question responses
klipnonchoice <- read_excel("~/Dropbox/KLIP Consumer Value Lindsey/Titus data/Analysis frame KLIP Percep study/KLIP-Non-Beneficiaries-Codebook.xlsx", sheet = "choices")

# format names NEED TO ADDRESS ISSUE WITH 201g and Q501c
klipnonnames <- gsub("choices_", "", klipnonchoice$`list name`)
klipnonnames2 <- gsub("_", ":", klipnonnames)
klipnonchoice$`list name` <- klipnonnames2
klipnonlevels <- klipnonchoice %>%
  spread(`list name`, `label::English`) %>%
  mutate_if(is.character, as.factor)
rownames(klipnonlevels) <- klipnonlevels$name
klipnonlevels <- klipnonlevels %>%
  dplyr::select(-Consent, -county, -`Section5:Q501c`, -`Section5:Q506`, -`SECTION2InsuranceAwarenessHouseholdAssets:Q201e`)
colnames(klipnonlevels) <- make.names(colnames(klipnonlevels))
colnames(klipnon2) <- make.names(colnames(klipnon2))

# Fix blank space (CHECK WHAT'S DRIVING THIS)
klipnon2$`Section7FinancialServices.Q703` <- ifelse(klipnon2$`Section7FinancialServices.Q703` == "", "99", klipnon2$`Section7FinancialServices.Q703`)

# Convert character to factor
klipnon2[colnames(klipnonlevels[, 2:94])] <- lapply(klipnon2[colnames(klipnonlevels[, 2:94])], function(dd) {
  as.factor(dd)
})

# Merge factor levels between datasets (except for name)
for (i in colnames(klipnonlevels[, 2:94])) {
  lev <- klipnonlevels %>%
    dplyr::select(name, i) %>%
    drop_na()
  levels(klipnon2[[i]]) <- lev[[2]][lev[[1]] %in% levels(klipnon2[[i]])]
}

# Assing beneficiary tag
klipnon2$treat <- "Non-beneficiary"

# characters to factors and rename
klipnon2 <- klipnon2 %>%
  mutate_if(is.character, as.factor)

# Replace name
names(klipnon2)[names(klipnon2) == "county"] <- "County"

#
##
### --------------------MERGE DATASETS----------------------------------------
##
#

samecol <- c(
  "SECTION2InsuranceAwarenessHouseholdAssets.Q204cow", "SECTION2InsuranceAwarenessHouseholdAssets.Q204camel",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q204goat", "SECTION2InsuranceAwarenessHouseholdAssets.Q204sheep",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q201a", "SECTION2InsuranceAwarenessHouseholdAssets.Q201b", "SECTION2InsuranceAwarenessHouseholdAssets.Q201c",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q201d", "SECTION2InsuranceAwarenessHouseholdAssets.cattleownedsep18",
  "SECTION2InsuranceAwarenessHouseholdAssets.camelownedsep18", "SECTION2InsuranceAwarenessHouseholdAssets.goatsownedsep18",
  "SECTION2InsuranceAwarenessHouseholdAssets.sheepownedsep18", "SECTION2InsuranceAwarenessHouseholdAssets.donkeyownedsep18",
  "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockowned", "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockcount",
  "SECTION2InsuranceAwarenessHouseholdAssets.cattleherdsepoct19", "SECTION2InsuranceAwarenessHouseholdAssets.camelherdsepcot19",
  "SECTION2InsuranceAwarenessHouseholdAssets.goatsherdsepoct19", "SECTION2InsuranceAwarenessHouseholdAssets.sheepherdsepoct19",
  "SECTION2InsuranceAwarenessHouseholdAssets.donkeyherdsepoct19", "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockherd",
  "SECTION2InsuranceAwarenessHouseholdAssets.otherlivestockherdcount", "SECTION2InsuranceAwarenessHouseholdAssets.Q205a", "SECTION2InsuranceAwarenessHouseholdAssets.Q205b",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q205c", "SECTION2InsuranceAwarenessHouseholdAssets.Q205d",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q205e", "SECTION2InsuranceAwarenessHouseholdAssets.Q205f",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q205f1", "SECTION2InsuranceAwarenessHouseholdAssets.Q206a",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208a", "SECTION2InsuranceAwarenessHouseholdAssets.Q208b",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208c", "SECTION2InsuranceAwarenessHouseholdAssets.Q208d",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208e", "SECTION2InsuranceAwarenessHouseholdAssets.Q208f",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208g", "SECTION2InsuranceAwarenessHouseholdAssets.Q208h",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q208i", "SECTION2InsuranceAwarenessHouseholdAssets.Q208i1",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q209", "SECTION2InsuranceAwarenessHouseholdAssets.Q209other",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q211a", "SECTION2InsuranceAwarenessHouseholdAssets.Q212a", "SECTION2InsuranceAwarenessHouseholdAssets.Q212b",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212c", "SECTION2InsuranceAwarenessHouseholdAssets.Q212d",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212e", "SECTION2InsuranceAwarenessHouseholdAssets.Q212f",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212g", "SECTION2InsuranceAwarenessHouseholdAssets.Q212h",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212i", "SECTION2InsuranceAwarenessHouseholdAssets.Q212j",
  "SECTION2InsuranceAwarenessHouseholdAssets.Q212k", "SECTION2InsuranceAwarenessHouseholdAssets.Q213b",
  "SECTION3Localmarketperception.Q306a", "SECTION3Localmarketperception.Q306b",
  "SECTION3Localmarketperception.Q306c", "SECTION3Localmarketperception.Q306d",
  "SECTION3Localmarketperception.Q306e", "SECTION3Localmarketperception.Q306f",
  "SECTION3Localmarketperception.Q306g", "SECTION3Localmarketperception.Q306h",
  "SECTION4Schoolattendanceperception.Q401", "SECTION4Schoolattendanceperception.Q402",
  "SECTION4Schoolattendanceperception.Q403", "SECTION4Schoolattendanceperception.Comment4", "Section5.Q501",
  "Section5.life", "Section5.annuities", "Section5.property", "Section5.vehicle",
  "Section5.health", "Section5.agric", "Section5.land", "Section5.chamma",
  "Section5.otherInsTypes", "Section5.knowOtherInsTypes", "Section5.Q501b",
  "Section5.Q501c", "Section5.Q502.Q502a", "Section5.Q502.Q502b",
  "Section5.Q502.Q502c", "Section5.Q502.Q502d", "Section5.Q502.Q502e",
  "Section5.Q502.Q502f", "Section5.Q502.Q502g", "Section7FinancialServices.Q701", "SECTION8HUMANITAIRANASSISTANCE.Q801"
)

# same questions
# klipnon2[84] klip[88]
# klipnon2[88] klip[111]
# klipnon2[228:230] klip[278:280]
# klipnon2[232:233] klip[282:283]

# Change colnames in non-beniciary database to align with beneficiaries
colnames(klipnon2) <- gsub("InsuranceAwarenessHouseholdAssets", "HOUSEHOLDCHARACTERISTICS", colnames(klipnon2))
samecol <- gsub("InsuranceAwarenessHouseholdAssets", "HOUSEHOLDCHARACTERISTICS", samecol)
samecol <- gsub("choices_", "", samecol)
samecol <- gsub("_", ":", samecol)

# Perform merge
klip <- klip2 %>%
  full_join(klipnon2, by = intersect(c(colnames(klip2[1:84]), colnames(klip2[231:264]), "treat", samecol), c(colnames(klipnon2[1:84]), colnames(klipnon2[186:218]), "treat", samecol))) %>%
  group_by(`meta.instanceID`)
klip <- as.data.frame(klip)

# Change variable types
cat <- c(
  "SECTION1.HouseHoldMembers.HHGender", "SECTION1.HouseHoldMembers.Q104", "SECTION1.HouseHoldMembers.MaritalStatus",
  "Section5.Q501b"
)
klip[cat] <- lapply(klip[cat], function(xx) {
  as.factor(xx)
})
klipnon2[cat] <- lapply(klipnon2[cat], function(xx) {
  as.factor(xx)
})

# characters to factors
klip <- klip %>%
  mutate_if(is.character, as.factor)

# Sort factors
klip <- klip %>%
  mutate(
    SECTION1.HouseHoldMembers.Q104 = as.numeric(SECTION1.HouseHoldMembers.Q104),
    SECTION1.HouseHoldMembers.Q104 = case_when(
      SECTION1.HouseHoldMembers.Q104 == 7 ~ "No Response ",
      SECTION1.HouseHoldMembers.Q104 == 6 ~ "Secondary",
      SECTION1.HouseHoldMembers.Q104 == 5 ~ "No formal Edu",
      SECTION1.HouseHoldMembers.Q104 == 4 ~ "Post Grad",
      SECTION1.HouseHoldMembers.Q104 == 3 ~ "University",
      SECTION1.HouseHoldMembers.Q104 == 2 ~ "Secondary",
      SECTION1.HouseHoldMembers.Q104 == 1 ~ "Primary",
      TRUE ~ NA_character_
    ),
    SECTION1.HouseHoldMembers.Q104 = as.factor(SECTION1.HouseHoldMembers.Q104),
    SECTION1.HouseHoldMembers.Q104 = factor(SECTION1.HouseHoldMembers.Q104,
                                            levels = c(
                                              "No formal Edu", "Primary", "Secondary", 
                                              "University", "Post Grad", "No Response"
                                            )))

klip$SECTION1.HouseHoldMembers.Q104 <- fct_relevel(klip$SECTION1.HouseHoldMembers.Q104, "No formal Edu", "Primary", "Secondary", "University", "Post Grad", "No Response ")
klip$County <- as.factor(ifelse(klip$County == "", NA, paste(klip$County)))

# Calculate total pay
klip <- klip %>%
  mutate(
    totalpay = rowSums(klip[c("SECTION2HOUSEHOLDCHARACTERISTICS.Q202a2", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a4", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a6", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a8", "SECTION2HOUSEHOLDCHARACTERISTICS.Q202a10")], na.rm = T),
    totalpay = ifelse(treat == "Non-beneficiary", NA, totalpay)
  )

# cattle since (Note that those with 0 aren't factored in)
klip <- klip %>%
  mutate(
    cattlegain = SECTION2HOUSEHOLDCHARACTERISTICS.cattleherdsepoct19 - SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18,
    cattlegainpcraw = ((cattlegain / SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18) * 100),
    cattlegainpc = dplyr::case_when(
      cattlegainpcraw == NaN ~ 0,
      cattlegainpcraw == Inf ~ NA_real_,
      cattlegainpcraw <= -10000 ~ NA_real_,
      cattlegainpcraw >= 4000 ~ NA_real_,
      TRUE ~ cattlegainpcraw
    )
  )

# Perception merges
klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge <- ifelse(klip$treat == "Beneficiary", paste(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211c),
                                                           ifelse(klip$treat == "Non-beneficiary", paste(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211b.y),
                                                                  NA
                                                           )
)
klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge <- as.factor(ifelse(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge == "NA", NA, klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge))
levels(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge) <- levels(klip$SECTION2HOUSEHOLDCHARACTERISTICS.Q211b.x)

klip$Section5.Q503amerge <- ifelse(klip$treat == "Beneficiary", paste(klip$Section5.Q503b),
                                   ifelse(klip$treat == "Non-beneficiary", paste(klip$Section5.Q503a),
                                          NA
                                   )
)
klip$Section5.Q503amerge <- as.factor(ifelse(klip$Section5.Q503amerge == "NA", NA, klip$Section5.Q503amerge))
levels(klip$Section5.Q503amerge) <- levels(klip$Section5.Q503b)

klip$Section5.Q503bmerge <- ifelse(klip$treat == "Beneficiary", paste(klip$Section5.Q503c),
                                   ifelse(klip$treat == "Non-beneficiary", paste(klip$Section5.Q503b),
                                          NA
                                   )
)
klip$Section5.Q503bmerge <- as.factor(ifelse(klip$Section5.Q503bmerge == "NA", NA, klip$Section5.Q503bmerge))
levels(klip$Section5.Q503bmerge) <- levels(klip$Section5.Q503c)

#Remove cattle outliers
klip <- klip %>%
  mutate(cattlegain = case_when(
    cattlegain <= -1000 ~ NA_real_,
    TRUE ~ cattlegain
  ))

#
##
### --------------------GRAPHING FUNCTIONS ----------------------------------------
##
#

stacked_bar_solo <- function(grouping, varia) {
  myaexp <- tt %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq),
      Var2 = factor(Var2, levels = rev(levels(Var2))))
  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(x = Var1,
          y = Freq,
          fill = Var2)
    ) +
    geom_col(
      data = neg,
      aes(x = Var1,
          y = -Freq,
          fill = Var2)
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold")
    ) +
    ylim(-100, 100)
}

stacked_bar_group <- function(grouping, varia) {
  myaexp <- klip2 %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq),
      Var2 = factor(Var2, levels = rev(levels(Var2))))
  plot <- ggplot() +
    geom_col(
      data = pos,
      aes(x = Var1,
          y = Freq,
          fill = Var2)
    ) +
    geom_col(
      data = neg,
      aes(x = Var1,
          y = -Freq,
          fill = Var2)
    ) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11, face = "bold")
    ) +
    ylim(-100, 100)
}

stacked_bar_dataset <- function(data_source, grouping, varia) {
  myaexp <- data_source %>%
    dplyr::select(grouping, varia)
  # myaexp <- subset(myaexp, myaexp[[varia]] !=c("No Response "))
  # myaexp[[varia]] <- droplevels(myaexp[[varia]])
  myaexp <- subset(myaexp, myaexp[[varia]] != c("Do Not Know"))
  myaexp[[varia]] <- droplevels(myaexp[[varia]])
  mdfr <- reshape::melt(myaexp, id.vars = grouping)
  a <- data.frame(prop.table(table(mdfr[[grouping]], mdfr$value), 1) * 100)
  neg <- a %>%
    filter(Var2 %in% levels(Var2)[1:3]) %>%
    mutate(Freq = case_when(
      Var2 == "Neutral" ~ Freq / 2,
      TRUE ~ Freq
    ))
  pos <- a %>%
    filter(Var2 %in% levels(Var2)[3:5]) %>%
    mutate(
      Freq = case_when(
        Var2 == "Neutral" ~ Freq / 2,
        TRUE ~ Freq
      ),
      Var2 = factor(Var2, levels = rev(levels(Var2)))
    )
  plot <- ggplot() +
    geom_col(data = pos, aes(
      x = Var1,
      y = Freq,
      fill = Var2
    )) +
    geom_col(data = neg, aes(
      x = Var1,
      y = -Freq,
      fill = Var2
    )) +
    coord_flip() +
    theme_fivethirtyeight() +
    scale_fill_brewer(
      name = "",
      limits = levels(neg$Var2),
      labels = levels(neg$Var2),
      palette = "RdBu"
    ) +
    theme(plot.title = element_text(
      size = 11,
      face = "bold"
    )) +
    ylim(-60, 100)
}
#
##
### --------------------ANALYSIS-------------------------------------------------
##
#

# GGtheme
ggthemr("fresh")


#-------------------Crosstab (can't include `SECTION1.HouseHoldMembers.Q104` for some reason)--------------------
tab <- tabmulti(`SECTION1.HouseHoldMembers.HHGender` + `SECTION1.HouseHoldMembers.Q103a` + `SECTION1.HouseHoldMembers.Q106` +`SECTION1.HouseHoldMembers.MaritalStatus` + `SECTION1.HouseHoldMembers.Q106` +
                  SECTION2HOUSEHOLDCHARACTERISTICS.Q213b + `Section5.Q501b` + SECTION2HOUSEHOLDCHARACTERISTICS.cattleownedsep18 + SECTION2HOUSEHOLDCHARACTERISTICS.Q205a ~ treat, data = klip)
tabh <- as_hux(tab)
quick_html(theme_article(tabh), file = "Tables/klipxtab.html")
quick_docx(theme_article(tabh), file = "Tables/klipxtab.docx")

#----------------Drought experience Plots----------------------------------------------------------------
treat_plot <- lapply(levels(klip$treat), function(yy){
  klip <- klip %>% filter(treat == yy)
  
  county_prop <- lapply(levels(klip$County), function(tt) {
    klip <- klip %>% filter(County == tt)
    
    prop <- unlist(
      lapply(
        colnames(klip %>% select(contains("SECTION2HOUSEHOLDCHARACTERISTICS.Q212"))), function(cc){
          table(klip %>% select(cc))[1] / count(klip %>% select(cc)) 
        }
      )
    )
    
    prop <- as.data.frame(prop)
    prop$date <- 2009:2019
    return(prop)
  })
  
  one <- ggplot(county_prop[[1]], aes(date, prop)) + geom_point(size = 3)  +
    theme_fivethirtyeight() + ggtitle(levels(klip$County)[1]) + geom_smooth(method= "loess") + ylim(0.2, 1) +
    scale_x_continuous(breaks=c(2009,2019)) + ylab(levels(klip$treat)[levels(klip$treat)==yy])
  
  two <-  ggplot(county_prop[[2]], aes(date, prop)) + geom_point(size = 3)  +
    theme_fivethirtyeight() + ggtitle(levels(klip$County)[2]) + geom_smooth(method= "loess") + ylim(0.2, 1) +
    scale_x_continuous(breaks=c(2009,2019))
  
  
  three <- ggplot(county_prop[[3]], aes(date, prop)) + geom_point(size = 3)  +
    theme_fivethirtyeight() + ggtitle(levels(klip$County)[3]) + geom_smooth(method= "loess") + ylim(0.2, 1) +
    scale_x_continuous(breaks=c(2009,2019))
  
  four <-  ggplot(county_prop[[4]], aes(date, prop)) + geom_point(size = 3)  +
    theme_fivethirtyeight() + ggtitle(levels(klip$County)[4]) + geom_smooth(method= "loess") + ylim(0.2, 1) +
    scale_x_continuous(breaks=c(2009,2019))
  
  return(grid.arrange(one,two,three,four, nrow=1))
})

drought_yr <- grid.arrange(treat_plot[[1]], treat_plot[[2]], nrow=2)

ggsave("Plots/drought_yr.pdf",  drought_yr, width = 10, height = 6)

#------------------------Type of payout ---------------------------------------------------
type_payout <- klip2 %>% 
  group_by(County, SECTION2HOUSEHOLDCHARACTERISTICS.Q201f) %>%  
  drop_na(SECTION2HOUSEHOLDCHARACTERISTICS.Q201f) %>%
  mutate(SECTION2HOUSEHOLDCHARACTERISTICS.Q201f = droplevels(SECTION2HOUSEHOLDCHARACTERISTICS.Q201f))%>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(SECTION2HOUSEHOLDCHARACTERISTICS.Q201f, fill = SECTION2HOUSEHOLDCHARACTERISTICS.Q201f)) + 
  facet_wrap(~County) +
  geom_histogram(aes(x = SECTION2HOUSEHOLDCHARACTERISTICS.Q201f, y = freq), stat = "identity")+
  coord_flip() +
  theme_538() + 
  facet_wrap(~County) + 
  ylab("Proportion") + 
  xlab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, face = "bold"),
        strip.text.x = element_text(size = 14,  face = "bold"),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12))

receive_payout <- klip2 %>% 
  group_by(County, SECTION2HOUSEHOLDCHARACTERISTICS.Q201e) %>%  
  drop_na(SECTION2HOUSEHOLDCHARACTERISTICS.Q201e) %>%
  mutate(SECTION2HOUSEHOLDCHARACTERISTICS.Q201e = droplevels(SECTION2HOUSEHOLDCHARACTERISTICS.Q201e))%>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(SECTION2HOUSEHOLDCHARACTERISTICS.Q201e, fill = SECTION2HOUSEHOLDCHARACTERISTICS.Q201e)) + 
  facet_wrap(~County) +
  geom_histogram(aes(x = SECTION2HOUSEHOLDCHARACTERISTICS.Q201e, y = freq), stat = "identity")+
  coord_flip() +
  theme_538() + 
  ylab("Proportion") + 
  xlab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, face = "bold"),
        strip.text.x = element_text(size = 14,  face = "bold"),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12))

ggsave("Plots/receive_payout.pdf", receive_payout, width = 9, height = 6)
ggsave("Plots/type_payout.pdf", type_payout, width = 11, height = 9)

#-----------------------Payout year by County--------------------------------------------------
payoutyr <- klip2 %>%
  dplyr::rename(
    "2015" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a2, "2016" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a4,
    "2017" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a6, "2018" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a8,
    "2019" = SECTION2HOUSEHOLDCHARACTERISTICS.Q202a10
  ) %>%
  group_by(County) %>%
  dplyr::select("2015", "2016", "2017", "2018", "2019") %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  gather(key, value, -County) %>%
  mutate(key = as.numeric(key)) %>%
  ggplot(aes(key, value, color = County)) +
  geom_point(aes(size = 1.5)) +
  geom_smooth(method = "loess", se = T) +
  facet_wrap(~County) +
  theme_fivethirtyeight() +
  ggtitle("") +
  theme(
    strip.text = element_text(size = 16),
    legend.position = "none",
    axis.title = element_text(size = 16,
                              face = "bold"),
    panel.spacing = unit(2, "lines")
  ) +
  ylab('Value (Ksh)') +
  xlab("Year") +
  scale_x_continuous(breaks=c(2015,2019)) 

ggsave("Plots/payoutyr.pdf", payoutyr, width = 6, height = 6)

#-------------------------------------Regression--------------------------------------------
klip_lm <- klip %>%
  remove_all_labels() %>%
  mutate(totalpay = totalpay / 10000) %>%
  rename(
    'Gender_HH' = SECTION1.HouseHoldMembers.HHGender, 'Age_HH' = SECTION1.HouseHoldMembers.Q103a,
    'Size_of_household' = SECTION1.HouseHoldMembers.Q106,'Highest_education' = SECTION1.HouseHoldMembers.Q104,
    'Marital_status' = SECTION1.HouseHoldMembers.MaritalStatus, "Have you heard of KLIP?" =   SECTION2HOUSEHOLDCHARACTERISTICS.Q201a, 
    "Any other insurance?" = Section5.Q501b, "Cattle_lost_this_year" = SECTION2HOUSEHOLDCHARACTERISTICS.Q205a
  )

paylm <- lm_robust(
  totalpay ~ `Gender_HH` + scale(`Age_HH`) + scale(`Size_of_household`) +
    `Highest_education` + `Marital_status` +  scale(`Cattle_lost_this_year`),
  clusters = SubCounty,
  klip_lm
)

payplot <- plot_model(paylm,
                      prefix.labels = "varname",
                      vline.color = "black") +
  theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 14,  face = "bold"),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12)) +
  ggtitle("")

ggsave("Plots/payplot.pdf", payplot, width = 10, height = 6)

#---------------------- Plots for Perception of Quality of Index-------------------------------------------
testone <- klip2 %>% 
  select(SECTION3Localmarketperception.Q304n) %>% 
  mutate(one = "The index trigger gave a good representation of the actual quality of the pasture")
testtwo <- klip2 %>% 
  select(SECTION3Localmarketperception.Q304o) %>% 
  mutate(one = "In my opinion the index is a reliable tool to determine payouts")

colnames(testone) <- "one"
colnames(testtwo) <- "one"
tt <- rbind(testone, testtwo)
colnames(tt) <- c("one", "two")

graphy <- stacked_bar_solo("two", "one") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(-50, 70) + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom")

ggsave("Plots/graphy.pdf",  graphy, width = 10, height = 4)

#--------------------------Understanding-------------------------------------------
#Graph 1
# Registration process
testone <- klip2 %>% 
  select(SECTION3Localmarketperception.Q304) %>%
  mutate(one = "There were some registration criteria that I did not completely understand")
testtwo <- klip2 %>%
  select(SECTION3Localmarketperception.Q303h) %>%
  mutate(one = "Most pastoralists do not even know if they're registered for KLIP")
testthree <- klip2 %>%
  select(SECTION3Localmarketperception.Q303g) %>%
  mutate(one = "All pastoralists understand how KLIP works")

colnames(testone) <- "one"
colnames(testtwo) <- "one"
colnames(testthree) <- "one"
tt <- rbind(testone, testtwo, testthree)
colnames(tt) <- c("one", "two")

graphy2 <- stacked_bar_solo("two", "one") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(-70, 70) + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom")

ggsave("Plots/graphy2.pdf",  graphy2, width = 10, height = 4)

#-------------------Graph 2------------------------------
testone <- klip2 %>% 
  select(SECTION3Localmarketperception.Q304j) %>% 
  mutate(one = "I am aware of exactly when I should receive a payout or not")

testtwo <- klip2 %>% 
  select(SECTION3Localmarketperception.Q304k) %>% 
  mutate(one = "The registration process for KLIP should be improved")

testthree <- klip2 %>% 
  select(SECTION2HOUSEHOLDCHARACTERISTICS.Q202eii) %>% 
  mutate(one = "Communication/Feedback on KLIP related matters from the provider is regular")

colnames(testone) <- "one"
colnames(testtwo) <- "one"
colnames(testthree) <- "one"
tt <- rbind(testone, testtwo, testthree)
colnames(tt) <- c("one", "two")

graphy <- stacked_bar_solo("two", "one") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(-80, 90) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("Plots/understanding.pdf", graphy, width = 7, height = 4)

#-----------------------Registration process-------------------------
testone <- klip2 %>% 
  select(SECTION3Localmarketperception.Q304) %>% 
  mutate(one = "I had difficulty accessing the payout at the bank")

testtwo <- klip2 %>% 
  select(SECTION3Localmarketperception.Q304k) %>% 
  mutate(one = "The Insurance payouts I have received were the amounts I expected")

testthree <- klip2 %>%
  select(Section7FinancialServices.Q706iii) %>%
  mutate(one = "Using mobile phone to access KLIP Insurance payouts is better than the Bank ")

colnames(testone) <- "one"
colnames(testtwo) <- "one"
colnames(testthree) <- "one"
tt <- rbind(testone, testtwo, testthree)
colnames(tt) <- c("one", "two")

graphy3 <- stacked_bar_solo("two", "one") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(-40, 90) + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom")

ggsave("Plots/graphy3.pdf",  graphy3, width = 10, height = 4)

#----------------Registration process----------------------------------
# Method of payment
graphy4 <- stacked_bar_group("County", "SECTION3Localmarketperception.Q304k") +
  ggtitle("The method of payment of the payout is the best method for me") + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14)) +
  ylim(-50, 100)

ggsave("Plots/graphy4.pdf",  graphy4, width = 10, height = 6)

#----------------------Local market perceptions---------------------------------------
one <- stacked_bar_group("County", "SECTION3Localmarketperception.Q303a") + 
  ggtitle("The payout from KLIP came at exactly the right time") +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14)) +
  ylim(-90, 50)

two <- stacked_bar_group("County", "SECTION3Localmarketperception.Q303b") + 
  ggtitle("The payout was too late for it to be useful for my livestock") +
  theme(legend.position = "bottom") + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14)) +
  ylim(-70, 100)

combinetime <- grid.arrange(one, two, ncol = 2)

ggsave("Plots/combinetime.pdf", combinetime, width = 13.5, height = 5)

#------------------KLIP Impacts
# Registration process
testone <- klip2 %>%
  select(SECTION8HUMANITAIRANASSISTANCE.Q808) %>% 
  mutate(one = "With KLIP you can be certain that they will issue Payout when there is severe drought")

testtwo <- klip2 %>% 
  select(SECTION8HUMANITAIRANASSISTANCE.Q811) %>% 
  mutate(one = " KLIP payout is always sufficient to help with our livestock during draught or short rains")

colnames(testone) <- "one"
colnames(testtwo) <- "one"
tt <- rbind(testone, testtwo)
colnames(tt) <- c("one", "two")

graph_impact <- stacked_bar_solo("two", "one") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(-40, 90) + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom")

ggsave("Plots/graphy_impact.pdf",  graph_impact, width = 10, height = 4)

#-----------------Agree that KLIP payouts are helpful-------------------------------------------
# Graph one
one <- stacked_bar_dataset(klip, "treat", "SECTION2HOUSEHOLDCHARACTERISTICS.Q211a") + 
  ggtitle("a) KLIP Insurance payouts are helpful for households during draught period") + 
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=14)
  )

two <- stacked_bar_dataset(klip, "treat", "SECTION2HOUSEHOLDCHARACTERISTICS.Q211bmerge") + 
  ggtitle("b) KLIP beneficiaries households are much better off than those not registered in KLIP") + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(size=14)
  )

comb <- cowplot::plot_grid(one, two,
                           ncol = 1,
                           align = "v",
                           rel_heights = c(1, 1.2)
) 

ggsave("Plots/combhelp.pdf", comb, width = 10, height =5)

#---------------------------Education------------------------------------------------
one <- stacked_bar_dataset(klip, "treat", "SECTION4Schoolattendanceperception.Q401") + 
  ggtitle("a) Households receiving KLIP are more likely to keep their children in school") + 
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=14)
  )

two <- stacked_bar_dataset(klip, "treat", "SECTION4Schoolattendanceperception.Q402") + 
  ggtitle("b) School attendance for KLIP beneficiaries is higher than for those with no KLIP payout") + 
  theme(legend.position = "none",
        axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size=14)
  )

three <- stacked_bar_dataset(klip, "treat", "SECTION4Schoolattendanceperception.Q403") + 
  ggtitle("c) KLIP payouts support general well-being of children within Households ") + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        plot.title = element_text(size=14)
  )

comb_edu <- cowplot::plot_grid(one, two, three,
                               ncol = 1,
                               align = "v",
                               rel_heights = c(1, 1, 1.2)
) 

ggsave("Plots/combedu.pdf", comb_edu, width = 10, height =8)

#------ Spillovers--------------
#-----Graph one----
one <- stacked_bar_dataset(klip, "treat", "SECTION3Localmarketperception.Q306a") + 
  ggtitle("Have KLIP payouts affected the numbers of livestock traded at local market?") +
  ylim(-80, 80) + 
  theme(legend.position = "none") 

two <- stacked_bar_dataset(klip, "treat", "SECTION3Localmarketperception.Q306d") +
  ggtitle("KLIP payout payouts led to additional livestock traded") +
  ylim(-80, 80) + 
  theme(legend.position = "none") 

three <- stacked_bar_dataset(klip, "treat", "SECTION3Localmarketperception.Q306b") +
  ggtitle("Has KLIP payouts affected prices of livestock traded at local market?") + 
  theme(legend.position = "bottom") +
  ylim(-80, 80) 

spillover_one <- cowplot::plot_grid(one, two, three,  ncol = 1, align = "v", rel_heights = c(1, 1, 1.3))

ggsave("Plots/spillover_one.pdf", spillover_one, width = 10, height = 7)

#-----Graph two----
four <- stacked_bar_dataset(klip, "treat", "SECTION3Localmarketperception.Q306c") + 
  ggtitle("Has KLIP payouts affected human food prices at local market?") + 
  theme(legend.position = "none") 
five <- stacked_bar_dataset(klip, "treat", "SECTION3Localmarketperception.Q306h") + 
  ggtitle("KLIP payouts caused no changes at local market prices") + 
  theme(legend.position = "bottom")

spillover_two <- cowplot::plot_grid(four, five,  ncol = 1, align = "v", rel_heights = c(1, 1.3))

ggsave("Plots/spillover_two.pdf", spillover_two, width = 10, height = 4)

#-----Graph three------
six <- stacked_bar_dataset(klip, "treat", "SECTION3Localmarketperception.Q306f") +
  ggtitle("Land/asset cost or rates decreased as a result of KLIP payouts") +
  ylim(-90, 50) +
  theme(legend.position = "bottom") 

seven <- stacked_bar_dataset(klip, "treat", "SECTION3Localmarketperception.Q306g") +
  ggtitle("Vaccines, medicines price Increased as a result of KLIP payouts") +
  theme(legend.position = "bottom") +
  ylim(-90, 50)

spillover_three <- cowplot::plot_grid(six, seven, ncol = 1, align = "v", rel_heights = c(1, 1.3))

ggsave("Plots/spillover_three.pdf", spillover_three, width = 10, height = 4)

#--------------Wider safety net comparisons---------------------------
testone <- klip2 %>% 
  select(SECTION8HUMANITAIRANASSISTANCE.Q805) %>%
  mutate(one = "Insurance Payouts are more reliable than Humanitarian Assistance")

testtwo <- klip2 %>% 
  select(SECTION8HUMANITAIRANASSISTANCE.Q811) %>% 
  mutate(one = "I would prefer KLIP to humanitarian aid  (msaada) issued by Government of other Agencies")

colnames(testone) <- "one"
colnames(testtwo) <- "one"
tt <- rbind(testone, testtwo)
colnames(tt) <- c("one", "two")

impact_compare <- stacked_bar_solo("two", "one") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ylim(-40, 90) + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom")

ggsave("Plots/impact_compare.pdf",  impact_compare, width = 10, height = 4)
