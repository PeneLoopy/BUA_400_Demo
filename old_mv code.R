

# Example

```{r import stock data, warning=F, message=F, echo=F, include=F}
getSymbols("NFLX", from = Sys.Date()-7, to = Sys.Date())
NFLX_mr <- NFLX |> fortify.zoo() |> as_tibble(.name_repair = "minimal") |>
  rename("date" = "Index") |>
  filter(date==max(date))

getSymbols("AMC", from = Sys.Date()-7, to = Sys.Date())
AMC_mr <- AMC |> fortify.zoo() |> as_tibble(.name_repair = "minimal") |>
  rename("date" = "Index") |>
  filter(date==max(date))


getSymbols("AMC", from = "2013-01-01", to = "2022-12-31")
getSymbols("NFLX", from = "2013-01-01", to = "2022-12-31")

```

## Row

Value boxes show the present day values of Netflix and AMC stocks. The interactive Highchart plots show the trend over time for the range of data examined in the Netflix plots on the following pages. Stock data was downloaded from [Yahoo Finance](https://finance.yahoo.com/).


## Row {height=20%}

```{r}
#| content: valuebox
#| title: "Last Update"
list(
  color = "green",
  value = stamp("Sat. Jan. 1, 1999", quiet = T)(NFLX_mr$date)
)

```

```{r}
#| content: valuebox
#| title: "Netflix Adjusted Close"

list(
  color = "red",
  value = NFLX_mr$NFLX.Adjusted |> round(2)
)
```


```{r}
#| content: valuebox
#| title: "AMC Adjusted Close"

list(
  color = "blue",
  value = AMC_mr$AMC.Adjusted |> round(2)
)

```

## Row {height=75%}

```{r  pg2 nflx stock trends, warning=F, message=F, echo=F}

# create nflx and AMC plots of adjusted, high and low for this time frame
(NFLX_fts <- hchart(NFLX$NFLX.Adjusted, name="NFLX Adj.", color="red") |>
    hc_add_series(NFLX$NFLX.High, name="NFLX Hi." , color="red4") |>
    hc_add_series(NFLX$NFLX.Low, name="NFLX Lo." , color="lightcoral"))

```

```{r pg2 AMC stock trends, warning=F, message=F, echo=F}

(AMC_fts <- hchart(AMC$AMC.Adjusted, name="AMC Adj.", color="blue") |>
    hc_add_series(AMC$AMC.High, name="AMC Hi." , color="darkblue") |>
    hc_add_series(AMC$AMC.Low, name="AMC Lo." , color="lightblue"))


```


## Row {height=5%}

The trend in the two plots appear similar but the y-axis axis scale differs.

This plot is an alternative to the [High-Low Candlestick plot](https://jkunst.com/highcharter/articles/stock.html) which can also be created as a highchart.


# Nextflix and AMC Dygraphs

## Row {height=20%}

Add text here

## Row {height=80%}

```{r pg3 nflx dygraph, warning=F, message=F, echo=F}

# create nflx dygraph plot of adjusted, high and low for this time frame
# dyaxis commands remove grid lines and label axes to make plot more readable (optional)
# dyEvent commands add lines for events (could add shaded region instead) and range selector
# dyRangeSelector() adds range reselector to plot

(nflx_dg <- dygraph(NFLX[,c(2,3,6)], main="Netflix Stock Trends 2013 - 2022") |>
    dySeries("NFLX.Adjusted", label="NFLX Adj.", color= "red") |>
    dySeries("NFLX.High", label="NFLX Hi.", color= "red4") |>
    dySeries("NFLX.Low", label="NFLX Lo.", color= "lightcoral") |>
    dyAxis("y", label = "", drawGrid = FALSE) |>
    dyAxis("x", label = "", drawGrid = FALSE) |>
    dyEvent("2020-3-12", label = "Most Theaters closed", labelLoc = "bottom") |>
    dyEvent("2021-6-14", label = "Most Theaters Re-opened", labelLoc = "bottom") |>
    dyShading(from="2020-3-12", to="2021-6-14", color = "lightgrey") |>
    dyRangeSelector())

```

```{r pg3 amc dygraph, warning=F, message=F, echo=F}

# create amc dygraph plot of adjusted, high and low for this time frame
# dyaxis commands remove grid lines and label axes to make plot more readable (optional)
# dyEvent commands add lines for events (could add shaded region instead) and range selector
# dyRangeSelector() adds range reselector to plot

(amc_dg <- dygraph(AMC[,c(2,3,6)], main="AMC Stock Trends 2013 - 2022") |>
    dySeries("AMC.Adjusted", label="AMC Adj.", color= "blue") |>
    dySeries("AMC.High", label="AMC Hi.", color= "darkblue") |>
    dySeries("AMC.Low", label="AMC Lo.", color= "lightblue") |>
    dyAxis("y", label = "", drawGrid = FALSE) |>
    dyAxis("x", label = "", drawGrid = FALSE) |>
    dyEvent("2020-3-12", label = "Most Theaters closed", labelLoc = "bottom") |>
    dyEvent("2021-6-14", label = "Most Theaters Re-opened", labelLoc = "bottom") |>
    dyShading(from="2020-3-12", to="2021-6-14", color = "lightgrey") |>
    dyRangeSelector())

```

# Bar Chart of Movie Trends

## Row

### Column {width=70%}

```{r pg4 nflx mv release period data mgmt, include = F}

nflx_mv_plot1 <- nflx_mv |>                                 # reshape data to long format for plot
  pivot_longer(cols=c("comedies","action_adventr","docs",
                      "dramas","international","kids"),
               names_to="genre", values_to="n") 

nflx_mv_plot1 <- nflx_mv_plot1 |>                         # simplify release period (see instructions)
  mutate(release_period = ifelse(release_period %in% c("2001-2005", "2006-2010"), 
                                 "2001-2010", 
                                 release_period))


nflx_mv_plot1 <- nflx_mv_plot1 |>                         # add one time period to correct filter statement
  filter(release_period %in% c("1981-2000","2001-2010", "2011-2015", "2016-2021"))


nflx_mv_plot1 <- nflx_mv_plot1 |>                        # create factor variable min_ageF from min_age
  mutate(min_ageF =  factor(min_age, levels = c(0, 7, 13, 17)))


nflx_mv_plot1 <- nflx_mv_plot1 |>                           # create genre factor variable, genreF
  mutate(genreF = factor(genre,                             # note that order must be corrected
                         levels = c("international","dramas","comedies","docs","kids","action_adventr"),
                         labels = c("Int","Dr","C","Do","K","A/A"))) 

```


```{r pg4 nflx mv release period bar chart, fig.dim = c(10, 5), echo=F}

# creates a grouped stacked bar_chart to show proportion in each min_age cat in each bar
(nflx_mv_barplot <- nflx_mv_plot1 |>
    # creates a grouped stacked bar_chart to show proportion in each age_min cat in each bar
    ggplot() +
    
    geom_bar(aes(x=genreF, y=n, fill=min_ageF),
             stat="identity", position="stack") + 
    
    theme_economist_white() +
    
    # facet_grid creates a separate panel for each period
    facet_grid(~release_period) + 
    
    # adjust colors all at once by changing palette
    scale_fill_brewer(palette = "Spectral") +
    
    # labels axes, titles, caption, and legend
    labs(x="Genre", y="Number of Movies", fill="Min. Age",
         title="Release Time Periods of Netflix Movies by Genre",
         subtitle="I=International   D=Drama   C=Comedy   Do=Documentaries   K=Kids   A/A=Action/Adventure",
         caption="Data Source: https://www.kaggle.com/shivamb/netflix-shows"))

```

### Column {width=30%}

#### Row

Number of Netflix Movies from each Genre and Release Period.

#### Row

```{r pg4 nflx summary table, echo=F, message=F}

nflx_smry1 <- nflx_mv_plot1 |>
  select(release_period, genreF, n) |>                                         # select variables
  group_by(release_period, genreF) |>                                       # group and summarize data
  summarize(n=sum(n)) |>
  pivot_wider(id_cols = release_period, 
              names_from = genreF,
              values_from = n) |>                                    # reshape data for table output
  rename("Release Time Period" = "release_period") 

kable(nflx_smry1)                                     # print out presentation table
```

#### Row

Genres are shown in plot and table in order of prevalence.

Information on [grouped and stacked bar charts](https://www.r-graph-gallery.com/stacked-barplot.html)

Information on plots with [facets](https://ggplot2.tidyverse.org/reference/facet_grid.html) 

Bonus link for [multi-panel plots](http://www.sthda.com/english/wiki/ggplot2-facet-split-a-plot-into-a-matrix-of-panels)


#  Netflix Movies Added Each Year

## Row

### Column {width=75%}

```{r pg5 nflx mv area plot data mgmt, include=F}

nflx_mv_plot2 <- nflx_mv |>                           # start with nflx_tv
  mutate(total = sum(c_across(comedies:kids))) |>     # sum all genres rowwise (columns 5 through 10)  
  
  select(year_added, min_age, total) |>               # keep only the 3 columns needed for plot
  filter(year_added >= 2013) |>
  
  group_by(year_added, min_age) |>                    # summarize by year and age_min category
  summarize(total=sum(total, na_rm=T)) |>             
  mutate(min_ageF = factor(min_age, levels=c(0,7,13,17))) 

```

```{r pg5 nflx tv area plot, fig.dim = c(10, 6), echo=F}

# area (proportion) plot code begins here
# plot code is incomplete (see instructions)
(nflx_mv_area_plot <- nflx_mv_plot2 |>
    
    ggplot() +                                # shows stacked areas (proportions) attributed to each category
    geom_area(aes(x=year_added, y=total, fill=min_ageF)) +                              
    theme_economist_white() +
    theme(legend.position = "bottom") +        # move legend to bottom
    scale_x_continuous(breaks=seq(2013, 2021, 1)) +                     # modifies x axis so each year is shown  (See HW 4)
    scale_fill_brewer(palette = "Spectral") +  # modifies color palette
    labs(x="Year", y="Number of Movies", fill="Min. Age",   # format and add plot labels
         title="Number of Netflix Movies Added Each Year",
         subtitle="2013 - 2021",
         caption="Data Source: https://www.kaggle.com/shivamb/netflix-shows"))

```

### Column {width=25%}

#### Row

This plot does not include genre information (shown on Page 2).  

Further analyses would benefit from having data that differentiates between Netflix original content and Netflix content purchased from other sources.

#### Row

Information on [Area plots](https://www.r-graph-gallery.com/136-stacked-area-chart)

Information on [R Color Options](https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)

# Netflix Pie Chart Series

## Row

### Column {width=75%}

```{r pg6 pie chart series, message=F, warning=F, echo=F}

# start with nflx_mv
nflx_mv_pie <- nflx_mv |> 
  rowwise() |>                                        # rowwise needed here 
  mutate(total = sum(c_across(comedies:kids))) |>     # sum across genres (colS 5-10) 
  ungroup() |>                                        # ungroup rowwise 
  select(year_added, min_age, total) |>               # select vars needed
  filter(year_added >= 2013) |>
  group_by(year_added, min_age) |>                    # group and summarize
  summarize(total=sum(total, na_rm=T)) |>
  mutate(min_ageF = factor(min_age, levels=c(0,7,13,17)),
         min_age_txt = paste(min_age, " (",total,")", sep="")) 

# create pie charts for each year
# 2013 was a special case with no movies in the '13' category
p2013 <- nflx_mv_pie |>
  filter(year_added==2013) |>
  mutate(min_ageF = factor(min_ageF,
                           labels=min_age_txt)) |>
  
  ggplot(aes(x="", y=total, fill=min_ageF)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  scale_fill_manual(values = c("#d7191c", "#fdae61", "#2b83ba")) +
  labs(fill = "Min. Age (#)", subtitle=2013)

# create function to do other years
nflx_pie_plot <- function(yr){
  nflx_mv_pie |>
    filter(year_added==yr) |>
    mutate(min_ageF = factor(min_ageF,
                             labels=min_age_txt)) |>
    ggplot(aes(x="", y=total, fill=min_ageF)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    scale_fill_brewer(palette = "Spectral") +
    labs(fill = "Min. Age (#)", subtitle=yr)
}

# run pie chart function to create plots for 2014 - 2021
p2014 <- nflx_pie_plot(yr=2014)
p2015 <- nflx_pie_plot(yr=2015)
p2016 <- nflx_pie_plot(yr=2016)
p2017 <- nflx_pie_plot(yr=2017)
p2018 <- nflx_pie_plot(yr=2018)
p2019 <- nflx_pie_plot(yr=2019)
p2020 <- nflx_pie_plot(yr=2020)
p2021 <- nflx_pie_plot(yr=2021)

# display all pie charts in a 3x3 grid
grid.arrange(p2013, p2014, p2015, 
             p2016, p2017, p2018, 
             p2019, p2020, p2021,
             ncol=3)

```

### Column (width=25%)

This pie chart series is similar to the area plot in terms of the information it provides but this option ___.


# Netflix Revenue Plots

## Row 

The plots below ...

## Row

```{r pg7 rev plot data mgmt, warning=F, message=F, echo=F, include=F}

revenue <- read_csv("data/statistic_id273883_netflixs-quarterly-revenue-2013-2021.csv",
                    show_col_types=F, skip=3, col_names = c("qtr", "Revenue"))

net_income <- read_csv("data/statistic_id273884_netflixs-quarterly-net-income-2013-2021.csv", 
                       show_col_types=F, skip=3, col_names = c("qtr", "Net Income"))

rev_inc <- full_join(revenue, net_income) |>
  separate(qtr, into = c("qtr", "yr"), sep=" ") |>
  mutate(dt = ifelse(qtr=="Q1", "3/31", NA),
         dt = ifelse(qtr=="Q2", "6/30", dt),
         dt = ifelse(qtr=="Q3", "9/30", dt),
         dt = ifelse(qtr=="Q4", "12/31", dt)) |>
  select(dt, yr, Revenue, `Net Income`) |>
  unite("qtr", dt:yr, sep="/") |>
  mutate(qtr = mdy(qtr)) 

rev_inc_long1 <- pivot_longer(data = rev_inc,
                              cols = Revenue:`Net Income`, names_to = "type", values_to = "amt")

rev_inc_long2 <- rev_inc |>
  mutate(Expenditures = Revenue - `Net Income`) |>
  select(qtr, Expenditures, `Net Income`) |>
  pivot_longer(cols = Expenditures:`Net Income`, names_to = "type", values_to = "amt")

```


```{r pg7 rev line plot, message=F, warning=F, echo=F}

(nflx_rev_line_plt <- rev_inc_long1 |>
    ggplot() +
    geom_line(aes(x=qtr, y=amt, col=type), size=2) +
    theme_classic() +
    labs(x="Date", y="U.S. Dollars (Mil $)", col="",
         title="Netflix Revenue and Net Income 2013 - 2021",
         caption="Data Sources: Statista (www.statista.com) and Netflix") +
    scale_x_date(date_breaks = "year", date_labels = "%Y") +
    scale_color_manual(values = c("#2b83ba","#d7191c")) +
    theme(legend.position="bottom"))

```

```{r pg7 rev area plot, message=F, warning=F, echo=F}

(nflx_rev_area_plt <- rev_inc_long2 |>
    ggplot() +
    geom_area(aes(x=qtr, y=amt, fill=type)) +
    theme_classic() +
    labs(x="Date", y="U.S. Dollars (Mil $)", fill="",
         title="Netflix Expenditures and Net Income 2013 - 2021",
         subtitle="Total Revenue = Expenditures + Net Income",
         caption="Data Sources: Statista (www.statista.com) and Netflix") +
    scale_x_date(date_breaks = "year", date_labels = "%Y") +
    scale_fill_manual(values = c("#d7191c","#2b83ba")) +
    theme(legend.position="bottom"))

```



# About 

This dashboard was created using [Quarto](https://quarto.org/) in [RStudio](https://posit.co/), and the [R Language and Environment](https://cran.r-project.org/).

The datasets used to create this dashboard were downloaded from 

- [Yahoo Finance](https://finance.yahoo.com/) 

- [Kaggle](https://www.kaggle.com/)

- [Statista](https://www.statista.com/)

## Row

**Software Citations**
  
  Arnold J (2024). _ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'_. R package version 5.1.0,
https://github.com/jrnold/ggthemes, <https://jrnold.github.io/ggthemes/>.

Auguie B (2017). _gridExtra: Miscellaneous Functions for "Grid" Graphics_. R package version 2.3,
<https://CRAN.R-project.org/package=gridExtra>.

Bache S, Wickham H (2022). _magrittr: A Forward-Pipe Operator for R_. R package version 2.0.3,
<https://CRAN.R-project.org/package=magrittr>.

Dancho M, Vaughan D (2023). _tidyquant: Tidy Quantitative Financial Analysis_. R package version 1.0.7,
<https://github.com/business-science/tidyquant>.

Kunst J (2022). _highcharter: A Wrapper for the 'Highcharts' Library_. R package version 0.9.4,
<https://CRAN.R-project.org/package=highcharter>.

Neuwirth E (2022). _RColorBrewer: ColorBrewer Palettes_. R package version 1.1-3, <https://CRAN.R-project.org/package=RColorBrewer>.

Posit team (2024). RStudio: Integrated Development Environment for R. Posit Software, PBC, Boston, MA. URL
http://www.posit.co/.

R Core Team (2024). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing,
Vienna, Austria. <https://www.R-project.org/>.

Rinker, T. W. & Kurkiewicz, D. (2017). pacman: Package Management for R. version 0.5.0. Buffalo, New York.
http://github.com/trinker/pacman

Vanderkam D, Allaire J, Owen J, Gromer D, Thieurmel B (2018). _dygraphs: Interface to 'Dygraphs' Interactive Time
Series Charting Library_. R package version 1.1.1.6, <https://github.com/rstudio/dygraphs>.

Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M,
Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C,
Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686.
doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

Xie Y (2024). _knitr: A General-Purpose Package for Dynamic Report Generation in R_. R package version 1.48,
<https://yihui.org/knitr/>.

Yihui Xie (2015) Dynamic Documents with R and knitr. 2nd edition. Chapman and Hall/CRC. ISBN 978-1498716963

Yihui Xie (2014) knitr: A Comprehensive Tool for Reproducible Research in R. In Victoria Stodden, Friedrich Leisch and
Roger D. Peng, editors, Implementing Reproducible Computational Research. Chapman and Hall/CRC. ISBN 978-1466561595

Zhu H (2024). _kableExtra: Construct Complex Table with 'kable' and Pipe Syntax_. R package version 1.4.0,
https://github.com/haozhu233/kableExtra, <http://haozhu233.github.io/kableExtra/>.



```{r  data import, warning=F, message=F, echo=F, include=F}
# import data and do prelim cleaning
nflx <- read_csv("data/netflix_titles.csv", show_col_types=F) |>
  select(show_id, type, date_added, release_year, rating, listed_in) |> # useful variables (subjective)
  separate(listed_in, sep=", ", into=c("g1", "g2", "g3")) |>         # separate out genre listings 
  # will result in lots of NAs
  mutate(date_added = mdy(date_added),                              # convert data_added to a date var
         year_added = year(date_added)) |>       
  filter(!is.na(year_added)) |>
  filter(!rating %in% c("NR", "UR", "66 min", "74 min", "84 min"))

```

```{r simplify ratings and release year data, warning=F, message=F, echo=F, include=F}
# create min_age variable from ratings data
nflx <- nflx |>
  mutate(min_age = ifelse(rating %in% c("G", "TV-G", "TV-Y"), 0, NA),
         min_age = ifelse(rating %in% c("PG", "TV-PG", "TV-Y7", "TV-Y7-FV"), 7, min_age),
         min_age = ifelse(rating %in% c("PG-13", "TV-14"), 13, min_age),
         min_age = ifelse(rating %in% c("NC-17", "TV-MA", "R"), 17, min_age))

# create release_period variable from release year
nflx <- nflx |>
  mutate(release_period = ifelse(release_year <= 1980, "1925-1980", NA),
         release_period = ifelse(release_year > 1980 & release_year <= 2000, "1981-2000", release_period),
         release_period = ifelse(release_year > 2000 & release_year <= 2005, "2001-2005", release_period),
         release_period = ifelse(release_year > 2005 & release_year <= 2010, "2006-2010", release_period),
         release_period = ifelse(release_year > 2010 & release_year <= 2015, "2011-2015", release_period),
         release_period = ifelse(release_year > 2015, "2016-2021", release_period))

```

```{r genre data cleaning, warning=F, message=F, echo=F, include=F}
# iterative cleaning, simplifying and filtering of genre information
# helpful to create new dataset for this step
nflx <- nflx |>
  select(show_id, type, year_added, release_period, min_age, g1, g2, g3) |>
  pivot_longer(cols=c("g1","g2","g3"), names_to="g", values_to="genre") |>
  select(!g) |>
  mutate(genre=tolower(genre),
         genre = str_trim(genre),
         genre = gsub(" movies", "", genre),
         genre = gsub("movies", "", genre),
         genre = gsub(" tv shows", "", genre),
         genre = gsub("tv shows", "", genre),
         genre = gsub("tv ", "", genre),
         genre = gsub(" tv", "", genre),
         
         genre = ifelse(genre == "drama", "dramas", genre),
         
         genre = ifelse(genre %in% c("stand-up comedy", 
                                     "stand-up comedy & talk shows"), 
                        "comedies", genre),
         
         genre = ifelse(genre %in% c("documentaries", "docuseries"), 
                        "docs", genre),
         
         genre = ifelse(genre %in% c("children & family", "kids'"), 
                        "kids", genre),
         
         genre = gsub("action & adventure", "action_adventr", genre))|>
  
  filter(genre %in% c("action_adventr", "comedies", "docs", 
                      "dramas", "international", "kids"))

# table(nflx$type, nflx$genre)

```

```{r summarizing dataset by relevant variables, warning=F, message=F, echo=F, include=F}
nflx_wide <- nflx |>
  group_by(type, release_period, year_added, min_age, genre) |>
  summarize(n = n()) |>
  pivot_wider(id_cols=c(type, release_period, year_added, min_age), 
              names_from = genre, values_from = n)

nflx_wide[is.na(nflx_wide)] <- 0

# example dashboard will be created using cleaned and managed TV data
nflx_tv <- nflx_wide |>
  filter(type == "TV Show") 

# for hw 5 you will create a dataset of just movies:
nflx_mv <- nflx_wide |>
  filter(type == "Movie") 

```

# Intro

## Background {width="50%"}

-   EDA is 2026 should be interactive

-   This course teaches students to explore, analyze, and
communicate with data.

-   Analysts and consultants learn these skills 'on the
        job'.

-   This course is designed to 'flatten' the learning curve
for young analysts.

##  {width="50%"}

![](img/Slide1_Graphic_Faded.png)

# Plan

## Technical Skills

-   GitHub Projects and Websites

-   Students will develop multiple repositories.

-   They will also have the option to build a website to
organize their 'portfolio'.

-   Agility with R and/or Python

-   Using AI and gaining proficiency

-   Learning AI's limitations and flaws

    -   Work with familiar and unfamilar data

    -   Compare AI suggestions to work done without AI

-   Students will work collaboritively to suggest and
    execute exploratory and analytical methods.

##  {width="50%"}

![](img/Slide1_Graphic_Faded.png)

```{r mv bar chart data, include=F}

nflx_mv_plot1 <- nflx_mv |>                                 # reshape data to long format for plot
  pivot_longer(cols=c("comedies","action_adventr","docs",
                      "dramas","international","kids"),
               names_to="genre", values_to="n") 

nflx_mv_plot1 <- nflx_mv_plot1 |>                         # simplify release period (see instructions)
  mutate(release_period = ifelse(release_period %in% c("2001-2005", "2006-2010"), 
                                 "2001-2010", 
                                 release_period))


nflx_mv_plot1 <- nflx_mv_plot1 |>                         # add one time period to correct filter statement
  filter(release_period %in% c("1981-2000","2001-2010", "2011-2015", "2016-2021"))


nflx_mv_plot1 <- nflx_mv_plot1 |>                        # create factor variable min_ageF from min_age
  mutate(min_ageF =  factor(min_age, levels = c(0, 7, 13, 17)))


nflx_mv_plot1 <- nflx_mv_plot1 |>                           # create genre factor variable, genreF
  mutate(genreF = factor(genre,                             # note that order must be corrected
                         levels = c("international","dramas","comedies","docs","kids","action_adventr"),
                         labels = c("Int","Dr","C","Do","K","A/A"))) 

```

```{r tv bar chart data, include=F}

nflx_tv_plot1 <- nflx_tv |>                                 # reshape data to long format for plot
  pivot_longer(cols=c("comedies","action_adventr","docs",
                      "dramas","international","kids"),
               names_to="genre", values_to="n") 

nflx_tv_plot1 <- nflx_tv_plot1 |>                          # simplify release period (see instructions)
   mutate(release_period = ifelse(release_period %in% c("2001-2005", "2006-2010"), 
                                  "2001-2010", 
                                  release_period))

nflx_tv_plot1 <- nflx_tv_plot1 |>                          # add one time period to correct filter statement
  filter(release_period %in% c("1981-2000", "2001-2010", "2011-2015", "2016-2021"))


nflx_tv_plot1 <- nflx_tv_plot1 |>                         # create factor variable min_ageF from min_age
   mutate(min_ageF =  factor(min_age))


nflx_tv_plot1 <- nflx_tv_plot1 |>                          # create genre factor variable, genreF
  mutate(genreF = factor(genre,                            # note that order must be corrected 
                         levels = c("international","dramas","comedies","kids","docs","action_adventr"),
                         labels = c("Int","Dr","C","K","Do","A/A"))) 

```

# MV Bar Chart

## Column {width="70%"}

```{r mv barchart}

# creates a grouped stacked bar_chart to show proportion in each min_age cat in each bar
(nflx_mv_barplot <- nflx_mv_plot1 |>
    # creates a grouped stacked bar_chart to show proportion in each age_min cat in each bar
    ggplot() +
    
    geom_bar(aes(x=genreF, y=n, fill=min_ageF),
             stat="identity", position="stack") + 
    
    theme_economist_white() +
    
    # facet_grid creates a separate panel for each period
    facet_grid(~release_period) + 
    
    # adjust colors all at once by changing palette
    scale_fill_brewer(palette = "Spectral") +
    
    # labels axes, titles, caption, and legend
    labs(x="Genre", y="Number of Movies", fill="Min. Age",
         title="Release Time Periods of Netflix Movies by Genre",
         subtitle="I=International   D=Drama   C=Comedy   Do=Documentaries   K=Kids   A/A=Action/Adventure",
         caption="Data Source: https://www.kaggle.com/shivamb/netflix-shows"))

```

## Column {width="30%"}

### Row

Number of Movies from each Genre and Release Period.

```{r mv smry table, echo=F, message=F}

nflx_smry_m1 <- nflx_mv_plot1 |>
  select(release_period, genreF, n) |>                                         # select variables
  group_by(release_period, genreF) |>                                       # group and summarize data
  summarize(n=sum(n)) |>
  pivot_wider(id_cols = release_period, 
              names_from = genreF,
              values_from = n) |>               # reshape data for table output
  rename("Release Time Period" = "release_period") 

kable(nflx_smry_m1)                                     # print out presentation table
```

# TV Bar Chart

## Column {width="70%"}

```{r tv barchart}

# creates a grouped stacked bar_chart to show proportion in each min_age cat in each bar
(nflx_tv_barplot <- nflx_tv_plot1 |>
  # creates a grouped stacked bar_chart to show proportion in each age_min cat in each bar
  ggplot() +
    
  geom_bar(aes(x=genreF, y=n, fill=min_ageF),
           stat="identity", position="stack") + 
  
  theme_economist_white() +

# facet_grid creates a separate panel for each period
  facet_grid(~release_period) + 
  
# adjust colors all at once by changing palette
  scale_fill_brewer(palette = "Spectral") +
  
# labels axes, titles, caption, and legend
  labs(x="Genre", y="Number of TV shows", fill="Min. Age",
       title="Release Time Periods of Netflix TV Shows by Genre",
       subtitle="I=International   D=Drama   A/A=Action/Adventure   C=Comedy   K=Kids   Do=Docuseries",
       caption="Data Source: https://www.kaggle.com/shivamb/netflix-shows"))


```

## Column {width="30%"}

### Row

Number of TV Shows from each Genre and Release Period.

```{r tv summary table, echo=F, message=F}

nflx_smry_t1 <- nflx_tv_plot1 |>
  select(release_period, genreF, n) |>                # select variables
  group_by(release_period, genreF) |>                 # group and summarize data
  summarize(n = sum(n)) |>
  ungroup() |>
  pivot_wider(id_cols = release_period, names_from = genreF, values_from = n) |> # reshape data for table
  rename("Release Time Period" = "release_period") 

kable(nflx_smry_t1)                                     # print out presentation table
```
