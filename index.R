## Load Libraries
knitr::opts_chunk$set(echo = FALSE)
library(kableExtra)
library(pagedown)
library(DiagrammeR)
library(tidyverse)
library(DiagrammeRsvg)
library(magick)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(forcats)
library(viridis)
library(scales)
library(stats)

rhg_cols0 <- c("#48937e", "#5d7f53", "#b5e271", "#f6f28c", "#face6e", 
               "#fda878", "#f781b9", "#d2356f")
rhg_cols <- c("#34544c", "#607d48", "#b6bd67",
              "#fcd476", "#e78247", "#db535b", "#eb9fa9", "#f2d5ce")

df <- data.frame("Column" = c("Country Name", "Country Code", "Indicator Name", "Indicator Code", "1960 to 2020"),
                 "DType" = c("object", "object", "object", "object", "float"),
                 "Unique" = c(266, 266, 1442, 1442, 383838),
                 "Variable Description" = c("Name of country or region", "Abbreviated code for country or region", "Name of measured development indicator", "Abbreviated code for indicator", "Column years ranging from 1960 to 2020"))

## Data Profile
knitr::kable(df, col.names = c("Data Columns", "Type", "Unique" ,"Description"), align = "l") %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("basic", "hover"), font_size = 11, html_font = "Roboto Condensed") %>%
  kable_paper(lightable_options =  c("striped", "basic"), font_size = 11, html_font = "Roboto Condensed")%>%
  row_spec(0, color = "black", background = "#b2beb585")



df_map <- read.csv('data/world_map.csv')
# common map properties
g <- list(
  scope = 'world',
  showland = TRUE,
  landcolor = toRGB("grey90"),
  countrycolor = toRGB("gray30"),
  showcountries = TRUE,
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator'),
  resolution = 90,
  subunitcolor = toRGB("white")
)

one_map <- function(dat) {
  plot_geo(dat) %>%
    add_markers(x = ~Longitude..generated., y = ~Latitude..generated., color = I("#33554c"), alpha = 0.5) %>%
    layout(geo = g)}

df_map <- df_map %>% do(mafig = one_map(.))
fig <- df_map %>% subplot(nrows = 1)
fig <- fig %>% layout(
  showlegend = FALSE,
  hovermode = TRUE
)

fig


df1 <- data.frame("Indicator" = c("GDP per capita, PPP", 
                                  "Industry, value added", 
                                  "Urban Population", 
                                  "Research & Development Expenditure", 
                                  "Foreign Direct Investment, net inflows", 
                                  "Total CO2 emissions"),
                  "Measure" = c("current international $", 
                                "% of GDP", 
                                "% of total population", 
                                "% of GDP", 
                                "% of GDP", 
                                "Thousand metric tons"),
                  "Description" = c("Annual percentage growth rate of the sum of gross value added by all residents in economy", 
                                    "Value added in mining, manufacturing, construction, electricity, water, and gas", 
                                    "Number of persons residing in 'urban' area per 100 total population", 
                                    "	Gross domestic expenditures on research and development (R&D), as a percent of GDP", 
                                    "Net inflows of foreign investment in an economy's operating enterprise", 
                                    "Carbon dioxide produced during consumption of solid, liquid, and gas fuels and gas flaring")
                  )


knitr::kable(df1, col.names = c("Indicator Name", "Measure", "Description")) %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("basic", "hover"), font_size = 11, html_font = "Roboto Condensed") %>%
  kable_paper(lightable_options =  c("striped", "basic"), font_size = 11, html_font = "Roboto Condensed")%>%
  row_spec(0, color = "black", background = "#b2beb585")


## Data Process
g1 <- grViz("digraph {
  graph [layout = dot, rankdir = LR]

  node [shape = circle, style = filled, fillcolor = whitesmoke, color = lightgray, peripheries = 2, 
        fontname = 'Roboto Condensed', penwidth = 0.4, fixedsize = true, width= 1.05, height = 0.67, 
        fontsize = 10.5, distortion = 0.25, alpha_fillcolor = 85]
  
  edge [arrowsize = 0.75, penwidth = 0.55, color = black]

  import [shape = folder, peripheries = 1, label = 'Import \n WDIData.csv', 
          style = filled, fillcolor=whitesmoke, penwidth = 3, color='#b2beb585']

export [shape = folder, peripheries = 2, label = 'Export as \n WDIClean.hyper', 
        style=filled, color=white,  penwidth = 3, fillcolor = '#b2beb585']

step1 [label = 'Select World \n Development \n Indicators']
step1a [label = '- GDP Growth % \n - Urban Population \n - R&D Expenditure \n ...', shape = box, height = 0.758, peripheries =1,
        style = rounded, color = '#b2beb5', fontcolor = DarkGray, fontsize = 8, width = 1]

step2 [label =  'Pivot each \n year column \n into rows']
step3 [label = 'Remove \n redundant \n columns']

step4 [label = 'Filter Values', shape = box, style='filled, rounded', height = 0.6, 
       peripheries =1, fillcolor = whitesmoke]
step4a [label = 'Filter out \n null row \n values', width = 0.7, fontsize = 9, 
        peripheries =1, fillcolor = whitesmoke]
step4b [label = 'Filter out \n grouped \n countries', width = 0.7, fontsize = 9,
        peripheries =1, fillcolor = whitesmoke]
step5 [label = 'Pivot indicator \n rows into \n columns']

step1 -> step2 -> step3 -> step4

import -> step1	[arrowhead=e, color='#b2beb5',style=filled, minlen = 0.2, penwidth = 3, arrowsize = 0.5]

step1 -> step1a [arrowhead = none, minlen = 0.2, color=gray]

step4a -> step4b [arrowhead=none, minlen = 0.2, color = white]
step4 -> {step4a step4b} [arrowhead=none]
{step4a step4b} -> step5 [headport = w]

step5 -> export [arrowhead=e, color='#b2beb5',style=filled, minlen = 0.2, penwidth = 3, arrowsize = 0.5]

}")

svg <- g1 %>% export_svg()

fileConn<-file("assets/flow1.svg")
writeLines(svg, fileConn)
close(fileConn)



#### Various Visualizations



df <- read.csv("data/yearly_co2.csv")

p <- df %>%
  mutate(text = paste("Country: ", Country.Name, "\nYear: ", ï..Year.of.Year, sep="")) %>%
  
  arrange(Total.CO2.emissions..thousand.metric.tons.of.CO2.) %>%
  mutate(Country = factor(Country.Name, levels=c("United States", "China", "Russian Federation", "Japan", "India", "United Kingdom", "Canada", "Germany"))) %>%
  ggplot( aes(x=ï..Year.of.Year, y=Total.CO2.emissions..thousand.metric.tons.of.CO2., group = Country, color=Country, text=text)) + 
  geom_line(size=0.75) + scale_color_manual(values = rhg_cols) + theme_ipsum() +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  labs(title="Annual CO2 Emissions By Country | 1960-2018", x = "Year", y="Thousand metric tons of CO2")+ 
  theme(plot.title = element_text(size = 10, color="#36454f", family = "Frank Ruhl Libre"),
legend.text = element_text(size=8, color="#36454f", family = "Frank Ruhl Libre"),
legend.title = element_text(colour = "white", size = 0),
axis.title.x  = element_text(size=9, color="#36454f", family = "Frank Ruhl Libre"),
axis.title.y  = element_text(size=9, color="#36454f", family = "Frank Ruhl Libre"),
axis.text = element_text(colour = "gray", family = "Roboto Condensed", face="bold"),
axis.text.x = element_text(size = 9),
axis.text.y = element_text(size = 8)) +
  scale_x_continuous(n.breaks = 8)

p <- ggplotly(p, tooltip = "text")
#p
p
# p %>%layout(title = "<b>Total Awards by Year by Gender</b>",
#          xaxis = list(title = "<b>Year</b>"),
#          yaxis = list(title = "<b>Number of Awards</b>"),
#          yaxis2 = list(title = "<b>Ratio of Awards (%)</b>"),
#          margin = list(t = 70))
# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/ggplotlyAreachart.html"))

df2 <- read.csv("data/co2_gdp.csv")
rhg_cols <- c("#34544c", "#607d48", "#b6bd67",
              "#fcd476", "#e78247", "#db535b", "#eb9fa9", "#f2d5ce")

# Interactive version
p <- df2 %>%
  mutate(Population..total=round(Population..total/1000000,3)) %>%
  mutate(GDP.per.capita..PPP..current.international... = round(GDP.per.capita..PPP..current.international..., 2)) %>%
  mutate(CO2.emissions..metric.tons.per.capita. = round(CO2.emissions..metric.tons.per.capita., 2)) %>%

  arrange((Population..total)) %>%
  mutate(country = factor(ï..Country.Name, ï..Country.Name)) %>%
  mutate(text = paste("Country: ", country, "\nRegion: ", Regiona, "\nGDP per capita: ", GDP.per.capita..PPP..current.international..., "\nCO2 Emissions: ", CO2.emissions..metric.tons.per.capita., sep="")) %>%
  ggplot(aes(x=GDP.per.capita..PPP..current.international..., 
             y=CO2.emissions..metric.tons.per.capita., 
             size=Population..total, color=Regiona, text=text)) +
    geom_point(alpha=0.85) +
    scale_size(range = c(0.4, 15)) +
    scale_color_manual(values = rhg_cols, guide="none") +
  scale_x_continuous(trans='log2', labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_y_continuous(trans='log2') +
    theme_ipsum() +
  labs(title="CO2 Emissions & GDP per Capita | 2018", x = "GDP per capita, PPP (curent international $)", y="CO2 Emissions (metric tons per capita)")+ 
  theme(plot.title = element_text(size = 10, color="#36454f", family = "Frank Ruhl Libre"),
        axis.title.x  = element_text(size=9, color="#36454f", family = "Frank Ruhl Libre"),
        axis.title.y  = element_text(size=9, color="#36454f", family = "Frank Ruhl Libre"),
        axis.text = element_text(colour = "gray", family = "Roboto Condensed", face="bold"),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size=8, color="#36454f", family = "Frank Ruhl Libre"),
        legend.title = element_text(colour = "white", size = 0),
        legend.position = "top")

pp <- ggplotly(p, tooltip="text")
pp



### PCA Analysis


#knitr::include_graphics("assets/CO2Final.png", dpi = 90)
df3 <- read.csv("data/PCA1.csv") %>% 
  drop_na()

colnames(df3) <- c('Country','Region', 'Total_CO2_Emissions', 
                   'CO2_Emissions_kgPPP', 'CO2_Emissions', 'Foreign_Investment',
                   'GDP_Growth', 'GDP_perCapita', 'Industry_Value', 'Population', 
                   'Research', 'Urban_Population')

## Subset out categorical columns
X <- subset(df3, select = -c(Country, Region, Total_CO2_Emissions, CO2_Emissions_kgPPP, GDP_perCapita, Population))


## Principal Components Analysis
prin_comp <- prcomp(X)

## Explained Variance Ratio 
explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
explained_variance_ratio <- 100 * explained_variance_ratio

## Components Dataframe
components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, df3$Country)
components <- cbind(components, df3$Region)

components$PC3 <- -components$PC3
components$PC2 <- -components$PC2


axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
ticklen=4,
titlefont=list(size=13))

fig <- components %>%
  plot_ly()  %>%
  add_trace(type = 'splom',
            dimensions = list(
              list(label = paste('PC 1 (',toString(round(explained_variance_ratio[1],1)),'%)', 
                                 sep = ''), values=~PC1),
              list(label=paste('PC 2 (',toString(round(explained_variance_ratio[2],1)),'%)',
                               sep = ''), values=~PC2),
              list(label=paste('PC 3 (',toString(round(explained_variance_ratio[3],1)),'%)',
                               sep = ''), values=~PC3),
              list(label=paste('PC 4 (',toString(round(explained_variance_ratio[4],1)),'%)',
                               sep = ''), values=~PC4)),
            color = ~df3$Region, 
            colors = c("#34544c", "#607d48", "#b6bd67",
                       "#fcd476", "#e78247", "#db535b", "#eb9fa9", "#f2d5ce")) %>%
  style(diagonal = list(visible = FALSE)) %>%
  layout(
    legend=list(title=list(text='color')),
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis, xaxis3=axis, xaxis4=axis,
    yaxis2=axis, yaxis3=axis, yaxis4=axis)

fig


prin_comp <- prcomp(X, rank. = 2, scale. = TRUE, tol = 0.1)
components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, df3$Region)
components$PC2 <- -components$PC2
explained_variance <- summary(prin_comp)[["sdev"]]
explained_variance <- explained_variance[1:2]
comp <- prin_comp[["rotation"]]
comp[,'PC2'] <- - comp[,'PC2']
loadings <- comp
for (i in seq(explained_variance)){
  loadings[,i] <- comp[,i] * explained_variance[i]
}

features = c(' ', 'Foreign \nInvestment', 'GDP', 'Industry \nValue', '    R&D', 'Urban \nPopulation')
t <- list(
  family = "Frank Ruhl Libre",
  size = 10,
  color = '#36454f')
t2 <- list(
  family = "Frank Ruhl Libre",
  size = 11,
  color = 'black')

fig <- plot_ly(components, x = ~PC1, y = ~PC2, color = ~df3$Region, colors = c("#34544c", "#607d48", "#b6bd67","#fcd476", "#e78247", "#db535b", "#eb9fa9", "#f2d5ce"), type = 'scatter', mode = 'markers',alpha = 0.85, marker = list(size = 11)) %>%
  layout(
    legend=list(title=list(text='Region'), font=t),
    plot_bgcolor = "white",
    xaxis = list(
      title = "0", range=list(-2,2.5)),
    yaxis = list(
      title = "1", range=list(-2.5, 3)))
for (i in seq(6)){
  fig <- fig %>%
    add_segments(x = 0, xend = loadings[i, 1], y = 0, yend = loadings[i, 2] + 0.1, line = list(color = 'black'),alpha=0.5, inherit = FALSE, showlegend = FALSE) %>%
    add_annotations(x=loadings[i, 1], y=loadings[i, 2] + 0.1, ax = 0, ay = 0,text = features[i], xanchor = 'center', yanchor= 'bottom', font=t2)
}

fig






