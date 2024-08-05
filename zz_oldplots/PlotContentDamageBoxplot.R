#' @description This creates a boxplot of content damage accounts by the 
#' specific community.
#' 
#' @param community_losses dataframe; of structural damages by communities.
#' @param communityname character; the name of the community 
#' 
#' @return Returns a boxplot of Community-specific content damages to the five 
#' frequency categories. 
#' 
StructureContentDamagebyCommunityBoxPlot <- function(community_losses, communityname) {
  community_losses <- ld[ld$StudyCity == communityname, ]
  #Change any character variables to numeric variables, group and sum 
  community_losses$ContDmgPct = as.numeric(community_losses$ContDmgPct)
  ggplot(community_losses, aes(x=Scenario_Frequency, y=ContDmgPct), 
         stat="identity", position="dodge", fill="key") +
    geom_boxplot() +
    xlab("Flood Frequency") +
    ylab("Percent Damaged") + 
    labs(title = paste(unique(community_losses$StudyCity), "Mean Content Damages by Frequency"))
}
