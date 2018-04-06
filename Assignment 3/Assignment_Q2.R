library(magrittr)
suppressMessages(library(dplyr))
library(readr)
library(ggplot2)
suppressMessages(library(tidyr))

# Helper functions
analyze_group_differences <- function(df, group, start, end) {
  avgs_by_group <-  df %>% 
    dplyr::rename_(group = group) %>% 
    select_("group", paste0("`", start, "`:`", end,"`")) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise_all(mean, na.rm = TRUE) %>% 
    na.omit
  
  vars_by_difference <- avgs_by_group %>% 
    dplyr::select(-group) %>% 
    apply(2, function(x) x[1] - x[2]) %>% 
    sort %>% 
    names
  
  avgs_by_group %>% 
    tidyr::gather(Variable, `avg response`, -group) %>% 
    ggplot(aes(x = Variable, y = `avg response`, group = group, colour = group)) + 
    geom_point(size = 5) + 
    scale_x_discrete(limits = vars_by_difference) +
    ylim(1, 5) +
    geom_hline(yintercept = 3) +
    coord_flip() + 
    theme(axis.text.y = element_text(face="bold", color="black", size=14),
          legend.position="top")
  
}

# Reading data
df <- readr::read_delim("responses.csv", delim = ",")
dim(df)

# Gender differences in music preferences
df %>% analyze_group_differences("Gender", "Dance", "Opera")

# Gender differences in interests
df %>% analyze_group_differences("Gender", "History", "Pets")

# Gender differences in phobias
df %>% analyze_group_differences("Gender", "Flying", "Fear of public speaking")

