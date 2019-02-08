library(tidyverse)

cody <- read_csv(here::here("Latin/latin.csv"))

clean <- cody%>%
  rename(origin = `Origin language and etymology`, affix = Affix)%>%
  mutate(affix = str_remove_all(affix,"\\(BrE\\)"),
         affix = str_remove_all(affix,"\\(AmE\\)"),
         affix = str_remove(affix,"\\[[0-9]\\]"),
         origin = str_extract(origin,"Greek|Latin"),
         affix = str_remove(affix, ",$"),
         type = case_when(
           str_detect(affix,"-$")~ "prefix",
           str_detect(affix,"^-")~ "suffix",
         )
         )%>%
  separate_rows(sep = ",",affix)%>%
  mutate(affix = str_squish(affix),
         type = case_when(
           str_detect(affix,"-$")~ "prefix",
           str_detect(affix,"^-")~ "suffix",
           T~ "unknown"
         ),
         affix = str_replace_all(affix,"\\)","\\)\\{0,1\\}"),
         affix = str_remove(affix,"-$"),
         affix = str_remove(affix,"^-"),
         length = str_length(affix)
         )%>%
  arrange(desc(length))%>%
  select(-length)

str_detect("hydroflask",clean$affix)

classify <- function(word){
  word=deparse(substitute(word))
  cbind(root =str_extract(word,clean$affix)[!is.na(str_extract(word,clean$affix))],
    clean[str_detect(word,clean$affix),2:4])
}


classify(fibromyalgia)
