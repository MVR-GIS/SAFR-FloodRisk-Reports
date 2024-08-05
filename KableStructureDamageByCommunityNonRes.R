#' @description Creates a table of damages by community, non-residential only.
#' 
#' @param community_losses dataframe; of structural damages by communities.
#' @param communityname character; the name of the community 
#' 
#' @return Returns a knitr::kable table of Community-specific damages in five 
#' frequency categories for non-residential structures only. 

KableStructureDamageByCommunityNonRes <- function(community_losses, communityname) {
  community_losses <- community_losses[community_losses$StudyCity == communityname, ]
  
  community_losses$Is_Res <- with(community_losses, ifelse(Occupancy==1, 'Residential', 'Non-Residential'))
  
  community_losses$Scenario_Frequency <- factor(community_losses$Scenario_Freq,
                                                levels=c("ACE 0.2", "ACE 0.5", "ACE 1", "ACE 2", "ACE 4", "ACE 10", "Avg Ann"), order = TRUE) 
  
  
  CommunityStructuresByFrequencyNResForPlot <- community_losses %>%
    filter(Total_Loss > 0 & Is_Res == 'Non-Residential') %>%
    group_by(Scenario_Frequency, .drop = FALSE) %>%
    summarise(StructureLossSum = round(sum(Total_Loss), -1),
              StructureLossBldgSum = round(sum(BldgLossUSD),-1),
              StructurecontentLossbyfrequency = round(sum(ContentLossUSD),-1),
              Structureinventorylossbyfrequency = round(sum(InventoryLossUSD),-1),
              StructureCountbyfrequency = n())
  
  kable(CommunityStructuresByFrequencyNResForPlot, 
        format.args = list(big.mark = ","),
        col.names = c("Frequency","Total Loss",
                      "Building Loss",
                      "Content Loss", "Inventory Loss", "Structure Count"),
        caption = paste(unique(community_losses$StudyCity), " Non-Residential Losses by Frequency in USD"), 
        align = NULL) %>%
    kable_styling(latex_options = "hold_position")
}