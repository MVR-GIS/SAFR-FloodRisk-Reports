#' @description This creates a boxplot of building damage accounts by the 
#' specific community.
#' @param community_losses dataframe; of structural damages by communities.
#' @param communityname character; the name of the community 
#' 
#' @return Returns a boxplot of Community-specific building damages to the five
#'  frequency categories. 
#' 
StructureBuildingDamagebyCommunityBoxPlot <- function(community_losses, communityname) {
  community_losses <- community_losses[community_losses$StudyCity == communityname, ]
  
  #subset for factoring and order
  community_losses <- community_losses[community_losses$Scenario_Frequency != "Avg Ann", ]
  
  community_losses$Scenario_Frequency <- factor(community_losses$Scenario_Frequency, 
                                                levels=c("ACE 0.2", "ACE 0.5", "ACE 1", "ACE 2", "ACE 4", "ACE 10"), order = TRUE) 
  
  
  
  community_losses$BldgDmgPct = as.numeric(community_losses$BldgDmgPct)
  ggplot(community_losses, aes(x=Scenario_Frequency, y=BldgDmgPct), 
         stat="identity", position="dodge", fill="key") +
    geom_boxplot() +
    xlab("Flood Frequency") +
    ylab("Percent Structure Damaged") 
  
}
