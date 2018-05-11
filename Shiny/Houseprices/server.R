# Load "necessary" packages
library(shiny)
library(DT)
library(readr)
library(dplyr)

# Load data
house <- read_tsv('houseprices_data.txt')
names(house) <- gsub(" ", "", names(house))

# Find variables responsible for missing values (NAs)
nas <- house %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  select_if(. > 0)

# Exclude variables missing more than 1500 values (roughly >50% of dataset)
exclude_nas <- nas %>% select_if(. > 1500) %>% names(.)
house %<>% select(-one_of(exclude_nas)) # select_ threw error, replaced with select()
# raw %<>% select(-one_of(exclude_nas))

# Seems like a false positive garage, where GarageType = 'Detachd' but all other data is missing. Remove that garage.
house %<>% mutate(GarageType = replace(GarageType, is.na(GarageCars), NA))

# One house has a garage but is missing some other values. Impute average and median value.
## Might want to consider imputing some mean/median YrBlt and Area here, rather than 0's
house %<>% mutate(GarageQual = replace(GarageQual, PID == '0903426160', 'TA'))
house %<>% mutate(GarageCond = replace(GarageCond, PID == '0903426160', 'TA'))

# Impute missing values:
## 0 for the single row with missing basement square feet, as all other no-basement have all 0's there
## 'SBrkr' for the one missing Electrical field, as the vast majority have this value (2682/2930)
house %<>% replace_na(list(BsmtFinSF1 = 0, 
                           BsmtFinSF2 = 0, 
                           BsmtUnfSF = 0, 
                           TotalBsmtSF = 0,
                           BsmtFullBath = 0,
                           BsmtHalfBath = 0,
                           # Single missing value, imputed as (by far) most typical
                           Electrical = 'SBrkr',
                           GarageCars = 0,
                           GarageArea = 0,
                           GarageYrBlt = 0,
                           #LotFrontage = 0, -- excluded as part of >300 NAs
                           # All cases where MasVnrType === NA, taken to mean 'None', so set to 0
                           MasVnrArea = 0,
                           LotFrontage = 0))

# Calculate house total square feet
house %<>% mutate(TotalSF = TotalBsmtSF + `1stFlrSF` + `2ndFlrSF`)

# We see one with GarageYrBlt year 2207: overwrite to same year as house was built (by far most typical)
house %<>% mutate(GarageYrBlt = replace(GarageYrBlt, PID == '0916384070', 2006))

# Now that we have taken care of all found individual cases, we can replace NAs with an additional factor level
house %<>% replace_na(list(GarageType = 'None',
                           GarageFinish = 'None',
                           GarageQual = 'None',
                           GarageCond = 'None',
                           MasVnrType = 'None',
                           FireplaceQu = 'None',
                           #Alley = 'None',
                           BsmtQual = 'None',
                           BsmtCond = 'None',
                           BsmtExposure = 'None',
                           BsmtFinType1 = 'None',
                           BsmtFinType2 = 'None'))

# Now that we have imputed missing values, the question is the level of sparsity in terms of % of 0's
sparse_num <- house %>% 
  summarise_all(funs(sum(. == 0) / n())) %>%
  select_if(. > 0)

# Drop variables with sparsity > 50%, ratio of 'None' for categorical and 0 for numeric
# drop_vars <- c(names(sparse_num %>% select_if(. > 0.5)), names(sparse_cat %>% select_if(. > 0.4)))
# house %<>% select(-one_of(drop_vars))

# Converting ordinal string factors to numbers
house %<>% mutate(
  LotShape = recode(LotShape, "Reg" = 4, "IR1" = 3, "IR2" = 2, "IR3" = 1),
  LandSlope = recode(LandSlope, "Gtl" = 3, "Mod" = 2, "Sev" = 1),
  ExterQual = recode(ExterQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1),
  ExterCond = recode(ExterCond, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1),
  BsmtQual = recode(BsmtQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "None" = 0),
  BsmtCond = recode(BsmtCond, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "None" = 0),
  BsmtExposure = recode(BsmtExposure, "Gd" = 4, "Av" = 3, "Mn" = 2, "No" = 1, "None" = 0),
  BsmtFinType1 = recode(BsmtFinType1, "GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3, "LwQ" = 2, "Unf" = 1, "None" = 0),
  BsmtFinType2 = recode(BsmtFinType2, "GLQ" = 6, "ALQ" = 5, "BLQ" = 4, "Rec" = 3, "LwQ" = 2, "Unf" = 1, "None" = 0),
  HeatingQC = recode(HeatingQC, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1),
  Electrical = recode(Electrical, "SBrkr" = 5, "FuseA" = 4, "FuseF" = 3, "FuseP" = 2, "Mix" = 1),
  KitchenQual = recode(KitchenQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1),
  Functional = recode(Functional, "Typ" = 8, "Min1" = 7, "Min2" = 6, "Mod" = 5, "Maj1" = 4, "Maj2" = 3, "Sev" = 2, "Sal" = 1),
  GarageFinish = recode(GarageFinish, "Fin" = 3, "RFn" = 2, "Unf" = 1, "None" = 0),
  GarageCond = recode(GarageCond, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "None" = 0),
  GarageQual = recode(GarageQual, "Ex" = 5, "Gd" = 4, "TA" = 3, "Fa" = 2, "Po" = 1, "None" = 0),
  PavedDrive = recode(PavedDrive, "Y" = 3, "P" = 2, "N" = 1))

# Dropping a few manually
house %<>% select(-Order, -PID)

# Whatever is character at this point is a factor
house %<>% mutate_if(.predicate = is.character, .funs = factor)
# Also make month sold a factor
house %<>% mutate(MoSold = factor(MoSold))

# Some feature engineering
house %<>% mutate(HouseAge = YrSold - YearBuilt, 
                  Remodeled = ifelse(YearBuilt != `YearRemod/Add`, 1, 0),
                  NewHouse = ifelse(YrSold == YearBuilt, 1, 0),
                  GarageAge = ifelse(GarageYrBlt == 0, 0, ifelse(YrSold == GarageYrBlt, 1, YrSold - GarageYrBlt)),
                  SoldInSummer = ifelse(MoSold %in% c("5", "6", "7"), 1, 0)
                  # Add total SF
)

# Dropping those variables
house %<>% select(-c(YrSold, YearBuilt, `YearRemod/Add`, GarageYrBlt, MoSold))

# Create vectors of variable names
vars_factor <- names(house %>% select_if(is.factor))
vars_numeric <- names(house %>% select_if(is.numeric) %>% select(-SalePrice))
vars_output <- "SalePrice"
if (length(vars_factor) + length(vars_numeric) + 1 != ncol(house)) {
  print("We have a problem, Houston!")
}

# Drop
house %<>% select(-LotConfig, -BldgType, -HouseStyle, -RoofStyle)

# Collapse factor levels based on Birk's keen insight
house %<>% mutate(
  MSSubClass = fct_collapse(MSSubClass, 
                            `1StNew` = "020", 
                            `1St` = c("030", "040", "045", "050"),
                            `2StNew` = "060",
                            `2St` = c("070", "075"),
                            Split = c("080", "085"),
                            Multi = c("090", "190"),
                            PUDNew = "120",
                            PUD = c("150", "160", "180")),
  MSZoning = fct_collapse(MSZoning,
                          Res = c("RH", "RL", "RM"),
                          NonRes = c("A (agr)", "C (all)", "I (all)")),
  Condition1 = fct_collapse(Condition1,
                            Pos = c("PosA", "PosN"),
                            Neg = c("RRAe", "RRAn", "RRNe", "RRNn", "Artery", "Feedr")),
  Exterior1st = fct_collapse(Exterior1st,
                             Good = c("CemntBd", "ImStucc", "PreCast", "Stone", "VinylSd"),
                             Bad = c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock", "HdBoard", "MetalSd", "Plywood", "Stucco", "Wd Sdng", "WdShing")),
  Exterior2nd = fct_collapse(Exterior2nd,
                             Good = c("Other", "PreCast", "VinylSd"),
                             Bad = c("CmentBd", "AsbShng", "AsphShn", "Brk Cmn", "BrkFace", "CBlock", "HdBoard", "ImStucc", "MetalSd", "Plywood", "Stone", "Stucco", "Wd Sdng", "Wd Shng")),
  Foundation = fct_collapse(Foundation,
                            PC = "PConc",
                            Other = c("BrkTil", "CBlock", "Slab", "Stone", "Wood")),
  GarageType = fct_collapse(GarageType,
                            Good = c("Attchd", "BuiltIn"),
                            Bad = c("2Types", "Basment", "CarPort", "Detchd")),
  SaleType = fct_collapse(SaleType,
                          Old = c("COD", "Con", "ConLD", "ConLI", "ConLw", "CWD", "Oth", "VWD", "WD")),
  SaleCondition = fct_collapse(SaleCondition,
                               New = "Partial",
                               Old = c("Abnorml", "AdjLand", "Alloca", "Family", "Normal"))
)

# Drop two of those that have very, very few observations in some group, as they are edge cases
house %<>% select(-MSZoning, -Condition1)

# Update variable name vectors
vars_factor <- names(house %>% select_if(is.factor))
vars_numeric <- names(house %>% select_if(is.numeric) %>% select(-SalePrice))



# Define server logic required to draw the plots
function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(data = house %>% filter(!(Neighborhood %in% input$neighbourhoods)),
      aes_string(x = input$x, y = "SalePrice")) +
    geom_point(aes_string(col = input$z)) +
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, input$splines), se = F) +
    labs(
      y = "Sale Price ($)"
    )
  })
  
  output$boxPlot <- renderPlot({
    ggplot(data = house %>% filter(!(Neighborhood %in% input$neighbourhoods))) +
      geom_boxplot(aes_string(x = input$z, y = "SalePrice", fill = input$z)) +
      theme(legend.position="none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      labs(
        x = ""
      )
  })
  
  output$instructions <- renderUI({
    HTML("Click and drag selection of observations in scatter plot to inspect closer.<br>")
  })
  
  # Create data table
  output$houseTable <- DT::renderDataTable({
    brushedPoints(house, brush = input$plot_brush) %>% 
      filter(!(Neighborhood %in% input$neighbourhoods)) %>%
      select(SalePrice, Neighborhood, TotalSF, HouseAge, SaleCondition, GarageCars, GarageAge)
  })
  
}

