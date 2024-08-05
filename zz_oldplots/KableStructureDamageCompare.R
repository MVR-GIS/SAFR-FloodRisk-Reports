#' @description Creates a table of comparing community damages to project damages.
#' 
#' @param community_losses dataframe; of structural damages by communities.
#' @param communityname character; the name of the community 
#' 
#' @return Returns a knitr::kable table of comparing damages in five 
#' frequency categories. 
#' 
KableStructureDamageCompare <- function(community_losses, communityname){
  project_losses <- community_losses
  
  project_losses$Scenario_Frequency <- factor(project_losses$Scenario_Frequency,
                                                levels=c("ACE 0.2", "ACE 0.5", "ACE 1", "ACE 2", "ACE 4", "ACE 10", "Avg Ann"), order = TRUE) 
  
  community_losses <- community_losses[community_losses$StudyCity == communityname, ]
  
  community_losses$Scenario_Frequency <- factor(community_losses$Scenario_Frequency, 
                                                levels=c("ACE 0.2", "ACE 0.5", "ACE 1", "ACE 2", "ACE 4", "ACE 10", "Avg Ann"), order = TRUE) 
  
  ProjectLossesByFrequency <- project_losses %>%
    group_by(Scenario_Frequency) %>%
    summarize(ProjLossSum = sum(Total_Loss))
  
  CommunityLossesByFrequency <- community_losses %>%
    group_by(Scenario_Frequency) %>%
    summarize(CommLossSum = sum(Total_Loss))
  
  LossesByFreq <- left_join(ProjectLossesByFrequency, CommunityLossesByFrequency, by="Scenario_Frequency")
  
  LossesByFreqForTable <- mutate(LossesByFreq, ComLossPct = ((CommLossSum / ProjLossSum)* 100))
  
  kable(LossesByFreqForTable, 
        format.args = list(big.mark = ","),
        col.names = c("Frequency", "Project Total Loss",
                      "County Total Loss",
                      "County % of Project Loss"),
        digits = c(0,-1,-1,1),
        caption = paste(unique(community_losses$StudyCity), "County and Project Losses in USD"), 
        align = NULL) %>%
    kable_styling(latex_options = "hold_position")
    
}