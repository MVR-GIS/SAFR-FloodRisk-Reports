#' @description This creates a boxplot of building damage accounts by the 
#' specific community.
#' @param community_losses dataframe; of structural damages by communities.
#' @param communityname character; the name of the community 
#' 
#' @return Returns a boxplot of Community-specific building damages to the five
#'  frequency categories. 
#' 
StructureBuildingDamagebyCommunityBoxPlot <- function(community_losses, communityname) {
  community_losses <- ld[ld$StudyCity == communityname, ]
  community_losses$BldgDmgPct = as.numeric(community_losses$BldgDmgPct)
  ggplot(community_losses, aes(x=Scenario_Frequency, y=BldgDmgPct), 
         stat="identity", position="dodge", fill="key") +
    geom_boxplot() +
    xlab("Flood Frequency") +
    ylab("Percent Damaged") + 
    labs(title = paste(unique(community_losses$StudyCity),"Mean Building Damages by Frequency"))
  
}
