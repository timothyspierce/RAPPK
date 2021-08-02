library(ggplot2)
library(gridExtra)
library(grid)
##########################################################################county
count_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

#county w/o internet subscription (3 income ranges)
rappk_data1 <- c(count_tbl("S2801_C02", 2019)[23,4])
rappk_data2 <- c(count_tbl("S2801_C02", 2019)[27,4])
rappk_data3 <- c(count_tbl("S2801_C02", 2019)[31,4])
#putting them in he same data frame
rappk_data <- data.frame(cbind(rappk_data1, rappk_data2, rappk_data3))
# putting estimate in the same column
rappk_data2 <- data.frame(pct = c(rappk_data[,1], rappk_data[,2], rappk_data[,3]))
#getting the percent that have internet
rappk_data2 <- mutate(rappk_data2, pct2 = 100-pct)
#Putting it estimates in same column
rappk_data3 <- data.frame(pct = c(rappk_data2[,1], rappk_data2[,2]))
rappk_data3 <- mutate(rappk_data3, grp = c("< $20,000", "$20,000-$74,999", "> $75,000",
                                           "< $20,000", "$20,000-$74,999", "> $75,000"))
rappk_data3 <- mutate(rappk_data3, int = c("No Internet", "No Internet", "No Internet",
                                           "Internet", "Internet", "Internet"))
colnames(rappk_data3) <- c("Percentage", "Income Range", "Int")
#putting income range in order
rappk_data3[,2] <- factor(rappk_data3[,2], levels = c("< $20,000", "$20,000-$74,999", "> $75,000"))

#creating a position variable for the percentage labels
rappk_data4 <- rappk_data3%>%
  group_by(`Income Range`) %>%
  arrange(`Income Range`, desc(Int)) %>%
  mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 

#Graph for rappk internet usage vs income
#round the percentage labels but us raw percentage for constructing the bars
#centering the tittle(plot.title)
#adjust the legend text and axis texts
#Got rid of legend title
#used two colors from cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")
rappk_plot <- ggplot(data = rappk_data4, aes(x = `Income Range`, y = Percentage)) +
  geom_col(aes(fill = Int), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = paste0(round(Percentage),"%"), group =Int), color = "white")+
  coord_flip() +ggtitle("Internet Subscription based on Income in Rappahannock") +
  theme(plot.title = element_text(hjust = 0.5, size=20),
         legend.title = element_blank(),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =14),
        axis.title.y=element_text(size =14)) +
  scale_fill_manual(values=c("#009E73","#D55E00"))


####################################################################subdivision
##computer access
sub_tbl <- function(varcode, year){
  data.frame(get_acs(geography = "county subdivision", state = 51,
                     county =  157,
                     table = varcode,
                     year = year))}

sub_info <- sub_tbl("S2801", 2019)
#using only the computer and no computer estimates from all subdivisions
sub_comp<- sub_info %>%
  group_by(NAME) %>%
  slice(c(33,42)) 

#labeling
sub_comp <- mutate(sub_comp, key = c("Computer", "No Computer"))
sub_comp2 <- sub_comp %>% ungroup()
sub_comp2 <- mutate(sub_comp2, District = c("Hampton", "Hampton", "Jackson", "Jackson",
                                            "Piedmont", "Piedmont", "Stonewall-Hawthorne",
                                            "Stonewall-Hawthorne", "Wakefield", "Wakefield"))
colnames(sub_comp2) <- c("id", "NAME", "variable", "Percentage", "moe", "key", "District")
#putting the districts in the order I want to present (should appear alphabetical on graph)
sub_comp2$District <- factor( sub_comp2$District, levels = c("Wakefield", "Stonewall-Hawthorne","Piedmont", "Jackson", "Hampton"))

#creates position variable for the percentages on the graph
sub_comp3 <- sub_comp2 %>%
  group_by(District) %>%
  arrange(District, desc(key)) %>%
  mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 

#Graphs the computer subscriptions for Rappk districts
#round the percentage labels but us raw percentage for constructing the bars
#centering the tittle(plot.title)
#adjust the legend text and axis texts
#Got rid of legend title
#used two colors from cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")
comp_plot <- ggplot(data = sub_comp3, aes(x = District, y = Percentage)) +
  geom_col(aes(fill = key), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = paste0(round(Percentage),"%"), group =key), color = "white")+
  coord_flip() +
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =14),
        axis.title.y=element_text(size =14)) +
  scale_fill_manual(values=c("#009E73","#D55E00"))

##internet
#using only internet and no internet estimates from every district
sub_int<- sub_info %>%
  group_by(NAME) %>%
  slice(c(43,50)) 

#labeling
sub_int <- mutate(sub_int, key = c("Internet", "No Internet"))
sub_int2 <- sub_int %>% ungroup()
sub_int2 <- mutate(sub_int2, District = c("Hampton", "Hampton", "Jackson", "Jackson",
                                        "Piedmont", "Piedmont", "Stonewall-Hawthorne",
                                        "Stonewall-Hawthorne", "Wakefield", "Wakefield"))
colnames(sub_int2) <- c("id", "NAME", "variable", "Percentage", "moe", "key", "District")

#putting the districts in the order I want to present (should appear alphabetical on graph)
sub_int2$District <- factor(sub_int2$District, levels = c("Wakefield", "Stonewall-Hawthorne","Piedmont", "Jackson", "Hampton"))


#position of the percentage labels
sub_int3 <- sub_int2 %>%
  group_by(District) %>%
  arrange(District, desc(key)) %>%
  mutate(lab_ypos = cumsum(Percentage) - 0.25 * Percentage) 

#Graph fro internet subscription in RAPPK districts
#round the percentage labels but us raw percentage for constructing the bars
#centering the tittle(plot.title)
#adjust the legend text and axis texts
#Got rid of legend title
#used two colors from cbPalette <- c("#E69F00","#56B4E9","#009E73","#0072B2","#D55E00", "#CC79A7")

sub_plot <- ggplot(data = sub_int3, aes(x = District, y = Percentage)) +
  geom_col(aes(fill = key), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = paste0(round(Percentage), "%"), group =key), color = "white")+
  coord_flip() +
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size =14),
        axis.title.y=element_blank()) +
  scale_fill_manual(values=c("#009E73","#D55E00"))

#######################################################arranging district graphs
#having RAPPK internet x income at the top and district graphs after
row1 <- rappk_plot
row2 <-grid.arrange(comp_plot, sub_plot, ncol=2,
             top = textGrob("Computer and Internet Subscriptions in Rappahannock Districts",
             gp=gpar(fontsize=20)))
grid.arrange(row1, row2, ncol=1)
