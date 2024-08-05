#' @description Creates a stacked bar chart by specific community for total, 
#' content, and inventory loss types.
#' 
#' @param community_losses dataframe; of structural damages by communities.
#' @param communityname character; the name of the community 
#' 
#' @return Returns a stacked bar chart for the annual chance exceedence 
#' total structural losses in a given community.
#' 
StructureDamagebyCommunity <- function(community_losses, communityname) {
  community_losses <- community_losses[community_losses$StudyCity == communityname, ]
  
  #subset for factoring and order
  community_losses <- community_losses[community_losses$Scenario_Frequency != "Avg Ann", ]
  
  community_losses$Scenario_Frequency <- factor(community_losses$Scenario_Frequency, 
                                                levels=c("ACE 0.2", "ACE 0.5", "ACE 1", "ACE 2", "ACE 4", "ACE 10"), order = TRUE) 
  
  #Group and Summarise by Loss Type
  CommunityStructuresByFrequency <- community_losses %>%
    group_by(Scenario_Frequency) %>%
    summarise(StructureLossBldgSum = sum(BldgLossUSD), 
              StructurecontentLossbyfrequency = sum(ContentLossUSD),
              Structureinventorylossbyfrequency = sum(InventoryLossUSD))
  CommunityLong <- gather(CommunityStructuresByFrequency, 
                          Loss_Category, cost, -Scenario_Frequency)
  #New Plot for StackedBarChart
  ggplot(CommunityLong) + 
    geom_col(aes(x = Scenario_Frequency, y = log10(cost), fill=Loss_Category))  +
    theme(legend.position = "right") +
    xlab("Frequency") +
    ylab("Loss in USD (log10)") +
    theme(legend.title = element_text(color = "black", size = 10, face = "bold")) +
    scale_color_brewer(aesthetics = c("color", "fill"),
                       palette = "Set2", 
                       name="Loss Type", 
                       labels=c("Building Loss", "Inventory Loss", "Content Loss" ))
}


