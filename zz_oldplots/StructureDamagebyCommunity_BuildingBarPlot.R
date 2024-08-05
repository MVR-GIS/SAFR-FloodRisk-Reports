#' @description Creates a mutliple-bar chart by specific community for total, 
#' content, and inventory loss types.
#' 
#' @param community_losses dataframe; of structural damages by communities.
#' @param communityname character; the name of the community 
#' 
#' @return Returns a bar chart for the annual chance exceedence 
#' total structural losses in a given community.
#' 
StructureDamagebyCommunity_BuildingBarPlot <- function(community_losses, communityname) {
  community_losses <- community_losses[community_losses$StudyCity == communityname, ]
  
  #subset for factoring and order
  community_losses <- community_losses[community_losses$Scenario_Frequency != "Avg Ann", ]
  
  community_losses$Scenario_Frequency <- factor(community_losses$Scenario_Frequency, 
                                                levels=c("ACE 0.2", "ACE 1", "ACE 2", "ACE 4", "ACE 10"), order = TRUE) 
  
  #Group and Summarise by Loss Type
  CommunityStructuresByFrequency <- community_losses %>%
    group_by(Scenario_Frequency) %>%
    summarise(StructureLossBldgSum = sum(BldgLossUSD), 
              StructurecontentLossbyfrequency = sum(ContentLossUSD),
              Structureinventorylossbyfrequency = sum(InventoryLossUSD))
  
  ggplot(CommunityStructuresByFrequency, mapping = aes(x=Scenario_Frequency)) +
    geom_col(aes(x=Scenario_Frequency, y=StructureLossBldgSum/1000)) +
    xlab("Frequency") +
    ylab("Building Loss in USD ($ ,000)") +
    theme(legend.title = element_text(color = "blue", size = 13, face = "bold"))
}