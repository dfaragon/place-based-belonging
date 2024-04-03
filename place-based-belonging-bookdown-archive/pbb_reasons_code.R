library(ggraph)

ay_1<- 2122 #CHANGE year (academic year format 1920 etc.)
ay_2<- 2223 #CHANGE year (academic year format 1920 etc.)

#get campus reasons data
campus_reasons<- read_csv(
  paste0(
    "data/processed/pbb_eoy_1718_thru_",
    ay_1,
    "_campus_reasons.csv"
  )
) %>%
  left_join(
    .,
    places %>%
      filter(level == "campus") %>%
      filter(!is.na(place)),
    by = c("ayear", "place")
  ) %>%
  within(cohort[is.na(cohort)]<- "no cohort")

#get emu reasons data
emu_reasons<- read_csv(
  paste0(
    "data/processed/pbb_eoy_1718_thru_",
    ay_1,
    "_emu_reasons.csv"
  ),
  guess_max = 1700
) %>%
  left_join(
    .,
    places %>%
      filter(level == "emu"),
    by = c("ayear", "place")
  ) %>%
  within(cohort[is.na(cohort)]<- "no cohort")

#function to remove nonanswers and invalid resps
invalid_fun<- function(df) {
  df<- df %>%
    filter(
      reason != "not sure" &
        reason != "unsure" &
        reason != "not sure just does" &
        reason != "no comment" &
        reason != "i do not know" &
        reason != "i honestly do not know" &
        reason != "do not know anything about it" &
        reason != "i do not know what is there" &
        reason != "i do not know how to explain it" &
        reason != "i do not know this building" &
        reason != "i do not know much about this place" &
        reason != "i do not know exactly it is just a feeling i have" &
        reason != "nothing" &
        reason != "n a" &
        reason != "see above" &
        !str_detect(reason, "same as above") &
        !str_detect(reason, "same as the last") &
        !str_detect(reason, "same as before") &
        !str_detect(reason, "same as will") &
        !str_detect(reason, "same reason i do not") &
        !str_detect(reason, "misread") &
        !str_detect(reason, "read question wrong") &
        !str_detect(reason, "read the question wrong") &
        !str_detect(reason, "did the wrong one") &
        !str_detect(reason, "click") &
        !str_detect(reason, "invalid") &
        !str_detect(reason, "meant to choose") &
        !str_detect(reason, "meant to pick") &
        !str_detect(reason, "meant to press") &
        !str_detect(reason, "meant to select") &
        !str_detect(reason, "meant to say") &
        !str_detect(reason, "meant to tap") &
        !str_detect(reason, "did not mean") &
        !str_detect(reason, "opps")
    )
  return(df)
}

#compute numbers of invalid responses by sample and place
invalid_cam<- anti_join(
  campus_reasons,
  campus_reasons %>%
    invalid_fun(),
  by = c("id", "ayear", "sent", "full_place")
) %>%
  filter(ayear > 1718) %>%
  group_by(sample, sent, agg_place, full_place) %>%
  count()

invalid_emu<- anti_join(
  emu_reasons,
  emu_reasons %>%
    invalid_fun(),
  by = c("id", "ayear", "sent", "full_place")
) %>%
  filter(ayear > 1819) %>%
  group_by(sample, sent, agg_place, full_place) %>%
  count()

#clean up reasons data
campus_reasons<- campus_reasons %>%
  mutate(
    #replace workout variants with single word equivalents
    reason = gsub("work out", "exercise", reason),
    reason = gsub("workout", "exercise", reason),
    reason = gsub("worked out", "exercised", reason),
    reason = gsub("working out", "exercising", reason),
    #replace students with student
    reason = gsub("students", "student", reason),
    #replace athletes with athlete
    reason = gsub("athletes", "athlete", reason),
    #replace friends with friend
    reason = gsub("friends", "friend", reason),
    #replace ras with ra
    reason = gsub("ras", "ra", reason),
    #replace classes with class
    reason = gsub("classes", "class", reason),
    #replace games with game
    reason = gsub("games", "game", reason),
    #replace buildings with building
    reason = gsub("buildings", "building", reason),
    #replace halls with hall
    reason = gsub("halls", "hall", reason),
    #replace pathway, pathway oregon with pathwayoregon
    reason = gsub("pathwayoregon", "pway", reason),
    reason = gsub("pathway oregon", "pway", reason),
    reason = gsub("pathway", "pway", reason),
    reason = gsub("pway", "pathwayoregon", reason),
    #replace psy and psych with psychology
    reason = gsub("psychology", "psy", reason),
    reason = gsub("psych", "psy", reason),
    reason = gsub("psy", "psychology", reason),
    #replace gov with government
    reason = gsub("government", "gov", reason),
    reason = gsub("gov", "government", reason),
    #uncontraction didn't work right in 2122 likely reason is failing to get rid of the dumb symbol string Qtrics sometimes puts in place of apostrophes
    reason = gsub(" t ", " not ", reason),
    reason = gsub(" s ", " ", reason),
    reason = gsub(" ve ", " have ", reason),
    reason = gsub(" m ", " am ", reason),
    reason = gsub(" d ", " would ", reason),
    reason = gsub(" re ", " are ", reason),
    reason = gsub(" ll ", " are ", reason),
    #correct t oget to get
    reason = gsub(" t oget ", " to get ", reason),
    #correct t oget to get
    reason = gsub(" go t ", " go to ", reason),
    #correct judgt to judged
    reason = gsub("wirh", "with", reason),
    #correct judgt to judged
    reason = gsub("judgt", "judged", reason),
    #correct click to clique so I can get rid of invalid resps (all other instances of click are people saying they clicked the wrong thing on the map)
    reason = gsub("type of click", "type of clique", reason),
    reason = gsub("clicky", "cliquey", reason)
  ) %>%
  #just throwing out ayear == 1718 altogether - methods are least consistent with other years and there weren't a full 4 cohorts contributing data then
  filter(ayear > 1718) %>%
  #need to adjust LLC place names, because they were combined in reasons followup in 1819
  within(
    level[
      place == "LLC"
    ]<- "campus"
  ) %>%
  within(
    full_place[
      place == "LLC"
    ]<- "Living Learning Center"
  ) %>%
  within(
    agg_place[
      place == "LLC"
    ]<- "University Housing"
  ) %>%
  within(
    full_place[
      place == "LLCNorth"
    ]<- "Living Learning Center"
  ) %>%
  within(
    full_place[
      place == "LLCSouth"
    ]<- "Living Learning Center"
  ) %>%
  #remove nonanswers and invalid resps
  invalid_fun()

emu_reasons<- emu_reasons %>%
  mutate(
    #replace students with student
    reason = gsub("students", "student", reason),
    #replace friends with friend
    reason = gsub("friends", "friend", reason),
    #replace classes with class
    reason = gsub("classes", "class", reason),
    #replace games with game
    reason = gsub("games", "game", reason),
    #replace buildings with building
    reason = gsub("buildings", "building", reason),
    #replace gov with government
    reason = gsub("government", "gov", reason),
    reason = gsub("gov", "government", reason),
    #replace acepting with accepting
    reason = gsub("acepting", "accepting", reason),
    #uncontraction didn't work right in 2122 likely reason is failing to get rid of the dumb symbol string Qtrics sometimes puts in place of apostrophes
    reason = gsub(" t ", " not ", reason),
    reason = gsub(" s ", " ", reason),
    reason = gsub(" ve ", " have ", reason),
    reason = gsub(" m ", " am ", reason),
    reason = gsub(" d ", " would ", reason),
    reason = gsub(" re ", " are ", reason),
    reason = gsub(" ll ", " are ", reason)
  ) %>%
  #omit 1819 (different methods than 1920 and 2122) and 1718 (for same reasons as omission from campus level)
  filter(ayear > 1819) %>%
  #remove nonanswers and invalid resps
  invalid_fun()

vnm_n<- campus_reasons %>% names()

#read annotated data (see pbb_udpipe_annotation.R)
pos_cam<- read_csv(
  "data/processed/pbb_campus_reasons_parts_of_speech_thru_2122.csv"
) %>%
  within(lemma[lemma == "cma"]<- "cmae")

vnm_pos<- pos_cam %>% names()

pos_emu<- read_csv(
  "data/processed/pbb_emu_reasons_parts_of_speech_thru_2122.csv"
)

#annotate data with tidytext and join emotion classification from nrc lexicon
sent_nrc<- get_sentiments("nrc")

emo_cam<- campus_reasons %>%
  unnest_tokens(word, reason) %>%
  inner_join(sent_nrc)

vnm_emo<- emo_cam %>% names()

emo_emu<- emu_reasons %>%
  unnest_tokens(word, reason) %>%
  inner_join(sent_nrc)

#junk adjectives
junk_adj<- c(
  "connected", "disconnected", "disconnect", "accepted", "accepting",
  "fit", "many", "most", "much", "more", "other"
)

#junk nouns
junk_noun<- c(
  "lots", "lot", "place", "places", "fit"
)

#junk verbs
#junk_verb<- c(
#"have", "had", "go", "get", "belong", "belonging", "belongs", "belonged", #"fit"
#)

#function for getting place n
np_fun<- function(df, bdb, s_vec, p, thresh) {
  counts<- df %>%
    filter(
      sent == bdb &
        sample %in% c(s_vec)
    ) %>%
    count(!!sym(p)) %>%
    filter(n > thresh)
  return(counts)
}

#function for getting n waves students have contributed data to (used for reasons data but not click data. the only application to click data is the overall amalgamation of data. all other click data are broken down by wave.)
nw_fun<- function(dat, s_vec, p, b_vec, db_vec) {
  df<- dat %>%
    filter(
      sample %in% c(s_vec)
    ) %>%
    filter(
      !!sym(p) %in% c(b_vec) |
        !!sym(p) %in% c(db_vec)
    ) %>%
    group_by(id) %>%
    count(ayear) %>%
    ungroup() %>%
    count(id) %>%
    rename(n_waves = n) %>%
    count(n_waves)
}

#function for getting n responses contributed within aggregated places (used for emu, housing, and lokey)
nmr_fun<- function(dat, s_vec, bdb) {
  df<- dat %>%
    filter(
      sample %in% c(s_vec) &
        sent == bdb
    ) %>%
    count(id) %>%
    count(n) %>%
    rename(
      n_resps = n,
      n = nn
    )
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#top words just aren't very informative. leaving function here in case it is ever of use
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#function to plot top 10 most frequent terms
#top10_fun<- function(pos_df, pos, p, p_lab, junk, bc, tc, t) {
#plot<- pos_df %>%
#filter(
#upos == pos &
#!!sym(p) == p_lab
#) %>%
#filter(!token %in% junk) %>%
#group_by(token) %>%
#summarise(count = n()) %>%
#arrange(desc(count)) %>%
#head(10) %>%
#mutate(
#token = factor(token),
#token = fct_reorder(token, count)
#) %>%
#ggplot(
#.,
#aes(
#x = token,
#y = count,
#fill = token
#)
#) +
#geom_bar(
#stat = "identity",
#position = "dodge",
#fill = bc
#) +
#labs(
#title = t,
#x = "",
#y = "Frequency"
#) +
#coord_flip() +
#geom_text(
#aes(label = count),
#hjust = -0.5,
#size = 5,
#color = tc
#) +
#guides(fill = "none") +
#theme(
#panel.background = element_rect(fill = "#30313A"),
#plot.title = element_text(hjust = 0.5, size = 15, color = tc),
#plot.background = element_rect(color = "#30313A", fill = "#30313A"),
#panel.grid = element_blank(),
#axis.title = element_text(size = 13, color = tc),
#axis.text = element_text(size = 12, color = tc),
#axis.ticks.y = element_blank()
#)
#return(plot)
#}

#function for networks of adjacent adjective-noun bigrams
bigram_fun<- function(dat, p, p_lab, thresh, junk, lin_c, tex_c) {
  set.seed(42)
  df<- dat %>%
    filter(!!sym(p) %in% p_lab)
  df<- cooccurrence(
    df$token,
    relevant = df$upos %in% c("NOUN", "ADJ"),
    skipgram = 1
  ) %>%
    data.frame()
  bigram<- df %>%
    filter(
      cooc > thresh &
        !term1 %in% c(junk) &
        !term2 %in% c(junk)
    ) %>%
    graph_from_data_frame() %>%
    ggraph(., layout = "fr") +
    geom_edge_link(
      aes(
        width = cooc,
        start_cap = ggraph::label_rect(node1.name),
        end_cap = ggraph::label_rect(node2.name)
      ),
      color = lin_c,
      lineend = "round",
      linejoin = "round",
      n = 6000
    ) +
    geom_node_label(
      aes(label = name),
      label.r = unit(0.35, "lines"),
      label.padding = unit(0.35, "lines"),
      size = 3,
      color = tex_c,
      fill = "#30313A"
    ) +
    theme(
      panel.background = element_rect(fill = "#30313A"),
      plot.background = element_rect(color = "#30313A", fill = "#30313A"),
      legend.position = "none"
    )
  return(bigram)
}

#function for computing min and max of coocurrence freq
cooc_minmax_fun<- function(dat, p, p_lab, junk, thresh) {
  df<- dat %>%
    filter(!!sym(p) %in% p_lab)
  df<- cooccurrence(
    df$token,
    relevant = df$upos %in% c("NOUN", "ADJ"),
    skipgram = 1
  ) %>%
    data.frame()
  minmax<- df %>%
    filter(
      !term1 %in% c(junk) &
        !term2 %in% c(junk)
    ) %>%
    summarise(
      min_cooc = min(cooc + thresh),
      max_cooc = max(cooc)
    )
  return(minmax)
}

#function for wordclouds of keyword pairs using RAKE algorithm
cloud_fun<- function(dat, p, p_lab, thresh, junk, tc, bc) {
  set.seed(42)
  df<- dat %>%
    filter(!!sym(p) %in% p_lab)
  df<- keywords_rake(
    df,
    term = "lemma",
    group = "doc_id",
    relevant = df$upos %in% c("NOUN", "ADJ")
  )
  df<- df %>%
    rename(word = keyword) %>%
    select(word, freq) %>%
    filter(
      freq > thresh &
        !word %in% c(junk)
    )
  cloud<- wordcloud2(
    data = df,
    size = 1,
    minSize = 9,
    minRotation = pi/4,
    maxRotation = -pi/6,
    rotateRatio = 0.5,
    color = tc,
    backgroundColor = bc
  )
  return(cloud)
}

#function for bar plots of emotional content
emo_bar_fun<- function(dat, p, p_lab, t, brk, lmt, cp, b_cp, e_cp, tc, tc_y) {
  plot<- dat %>%
    filter(!!sym(p) %in% p_lab) %>%
    count(sentiment) %>%
    filter(
      sentiment != "positive" &
        sentiment != "negative"
    ) %>%
    mutate(
      perc = n/sum(n)*100,
      sentiment = factor(sentiment),
      sentiment = fct_reorder(sentiment, perc)
    ) %>%
    ggplot(
      .,
      aes(
        x = sentiment,
        y = perc,
        fill = sentiment
      )
    ) +
    geom_bar(
      stat = "identity",
      position = "dodge"
    ) +
    labs(
      title = t,
      x = "",
      y = "Percentage"
    ) +
    coord_flip() +
    scale_y_continuous(
      breaks = brk,
      limits = lmt
    ) +
    scale_fill_viridis_d(
      option = cp,
      begin = b_cp,
      end = e_cp
    ) +
    geom_text(
      aes(label = sprintf("%0.1f", perc)),
      hjust = -0.5,
      size = 5.5,
      color = tc
    ) +
    guides(fill = "none") +
    theme(
      panel.background = element_rect(fill = "#30313A", size = 1),
      plot.background = element_rect(
        color = "#30313A",
        fill = "#30313A"
      ),
      plot.margin = margin(0.5, 0, 0.5, 0, unit = "cm"),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(
        color = tc,
        size = 19,
        hjust = 0.5
      ),
      axis.text.x = element_text(
        color = tc,
        size = 16
      ),
      axis.text.y = element_text(
        color = tc_y, #throws "Warning message: Vectorized input to `element_text()` is not officially supported. Results may be unexpected or may change in future versions of ggplot2." lol :-D oh well...gonna do it anyway 'cause it's pretty slick
        size = 16
      ),
      axis.title = element_text(
        color = tc, size = 17
      )
    )
  return(plot)
}


#first, a bit of validation ####

#see how belong and don't belong reasons were classified insofar as valence
val_class_cam<- emo_cam %>%
  group_by(sent, sentiment) %>%
  count() %>%
  filter(
    sentiment == "positive" |
      sentiment == "negative"
  ) %>%
  group_by(sent) %>%
  summarise(perc = round(n/sum(n)*100, 1)) %>%
  ungroup() %>%
  bind_cols(., grab = c(0, 1, 1, 0)) %>%
  filter(grab == 1) %>%
  select(-grab)

val_class_emu<- emo_emu %>%
  group_by(sent, sentiment) %>%
  count() %>%
  filter(
    sentiment == "positive" |
      sentiment == "negative"
  ) %>%
  group_by(sent) %>%
  summarise(perc = round(n/sum(n)*100, 1)) %>%
  ungroup() %>%
  bind_cols(., grab = c(0, 1, 1, 0)) %>%
  filter(grab == 1) %>%
  select(-grab)

#see how belong and don't belong reasons were classified insofar as emotion
emo_class_bar_fun<- function(dat) {
  plot<- dat %>%
    group_by(sent, sentiment) %>%
    count() %>%
    filter(
      sentiment != "positive" &
        sentiment != "negative"
    ) %>%
    group_by(sent) %>%
    summarise(perc = n/sum(n)*100) %>%
    bind_cols(
      .,
      emo = factor(
        rep(
          c(
            "Anger",
            "Anticipation",
            "Disgust",
            "Fear",
            "Joy",
            "Sadness",
            "Surprise",
            "Trust"
          ),
          2
        ),
        levels = c(
          "Trust",
          "Surprise",
          "Sadness",
          "Joy",
          "Fear",
          "Disgust",
          "Anticipation",
          "Anger"
        )
      )
    ) %>%
    mutate(
      sent = factor(sent, levels = c("Don't Belong", "Belong"))
    ) %>%
    ggplot(
      .,
      aes(
        x = emo,
        y = perc,
        fill = sent
      )
    ) +
    geom_bar(
      stat = "identity",
      position = "dodge"
    ) +
    xlab("") +
    ylab("Percentage") +
    ylim(0, 50) +
    coord_flip() +
    scale_fill_manual(
      name = "",
      values = c("#CB1B4F", "#357BA2"),
      guide = guide_legend(reverse = T)
    ) +
    geom_text(
      aes(label = sprintf("%0.1f", perc)),
      position = position_dodge(width = 1),
      hjust = -0.2,
      size = 5.5,
      color = c(rep("#78D6AE", 8), rep("#F6AA82", 8))
    ) +
    theme(
      panel.background = element_rect(fill = "#30313A"),
      plot.background = element_rect(color = "#30313A", fill = "#30313A"),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(color = "white", size = 13),
      axis.title = element_text(color = "white", size = 14),
      legend.background = element_blank(),
      legend.text = element_text(color = "white", size = 11),
      legend.key = element_rect(color = "#30313A")
    )
  return(plot)
}

#campus
emo_class_cam<- emo_class_bar_fun(emo_cam)

#emu
emo_class_emu<- emo_class_bar_fun(emo_emu)

#object label abbreviations
#b = belong
#bg = bigram (wordnet)
#cmm = cooccurrence min and max
#db = don't belong
#ebar = bar plot of emotions
#emu = emu
#i = international
#gen = places that won't be aggregated and thus don't need special treatment
#gr = graduate
#gsh = global scholars hall
#llc = living learning center
#lok = lokey complex
#nc = number of clicks
#nmr = number of responses contributed to aggregated place analyses
#nsp = number of reasons per selected (n > 19) place
#np = number of reasons per place
#nw = number of waves students contributed data
#pl = place
#src = student recreation complex
#ug = undergraduate
#uh = university housing
#uhs = university health services
#us = united states
#wc = wordcloud

#compute n for places by sentiment and n for waves ####

#us undergrad ####

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#here's what's feasible for analysis of undergrad data:

#belong:

#allen
#autzen
#cemetery
#chapman
#frohnmayer
#hedco
#lawrence
#library
#lillis
#matt knight
#mckenzie
#oregon
#straub
#src
#tykeson
#uhs
#housing
#barnhart
#bean
#carson
#earl
#gsh
#hamilton
#kalapuya
#llc
#unthank
#walton
#lokey
#columbia
#klamath
#lisb
#sci commons
#willamette
#emu
#atrium east
#courtyard
#craft
#duck nest
#falling sky
#fishbowl
#fresh market
#lgbtqa3
#mills
#mcc
#olounge
#taylor
#womens

#don't belong:

#autzen
#cemetery
#frohnmayer
#hayward
#hedco
#jaqua
#law
#lawrence
#library
#lillis
#matt knight
#src
#housing
#bean
#carson
#gsh
#hamilton
#kalapuya
#llc
#unthank
#walton
#lokey
#klamath
#lisb
#willamette
#emu
#fishbowl

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#places that won't be disaggregated (i.e., not housing, lokey, or emu) ####

#n places belong
np_gen_b_us_ug<- campus_reasons %>%
  filter(
    !agg_place %in% c(
      "University Housing",
      "Lokey Complex",
      "Erb Memorial Union"
    )
  ) %>%
  np_fun(
    .,
    "Belong",
    "us ug",
    vnm_n[13],
    19
  ) #autzen complex is ok because only ask followup for stadium

#n places don't belong
np_gen_db_us_ug<- campus_reasons %>%
  filter(
    !agg_place %in% c(
      "University Housing",
      "Lokey Complex",
      "Erb Memorial Union"
    )
  ) %>%
  np_fun(
    .,
    "Don't Belong",
    "us ug",
    vnm_n[13],
    19
  ) #autzen complex is ok because only ask followup for stadium

#n waves
nw_gen_us_ug<- nw_fun(
  campus_reasons,
  "us ug",
  vnm_n[13],
  np_gen_b_us_ug$agg_place,
  np_gen_db_us_ug$agg_place
)

#university housing places ####

#n places belong - for overall
np_uh_b_us_ug<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  np_fun(
    .,
    "Belong",
    "us ug",
    vnm_n[12],
    0
  )

#n places belong with n > 19 - for selected places
nsp_uh_b_us_ug<- np_uh_b_us_ug %>%
  filter(n > 19)

#n belong responses contributed to overall housing analyses
nmr_uh_b_us_ug<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  nmr_fun(., "us ug", "Belong")

#n places don't belong
np_uh_db_us_ug<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  np_fun(
    .,
    "Don't Belong",
    "us ug",
    vnm_n[12],
    0
  )

#n places don't belong with n > 19 - for selected places
nsp_uh_db_us_ug<- np_uh_db_us_ug %>%
  filter(n > 19)

#n don't belong responses contributed to overall housing analyses
nmr_uh_db_us_ug<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  nmr_fun(., "us ug", "Don't Belong")

#n waves
nw_uh_us_ug<- nw_fun(
  campus_reasons,
  "us ug",
  vnm_n[12],
  np_uh_b_us_ug$full_place,
  np_uh_db_us_ug$full_place
)

#lokey complex places ####

#n places belong - for overall
np_lok_b_us_ug<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  np_fun(
    .,
    "Belong",
    "us ug",
    vnm_n[12],
    0
  )

#n places belong with n > 19 - for selected places
nsp_lok_b_us_ug<- np_lok_b_us_ug %>%
  filter(n > 19)

#n belong responses contributed to overall lokey analyses
nmr_lok_b_us_ug<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  nmr_fun(., "us ug", "Belong")

#n places don't belong
np_lok_db_us_ug<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  np_fun(
    .,
    "Don't Belong",
    "us ug",
    vnm_n[12],
    0
  )

#n places don't belong with n > 19 - for selected places
nsp_lok_db_us_ug<- np_lok_db_us_ug %>%
  filter(n > 19)

#n don't belong responses contributed to overall lokey analyses
nmr_lok_db_us_ug<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  nmr_fun(., "us ug", "Don't Belong")

#n waves
nw_lok_us_ug<- nw_fun(
  campus_reasons,
  "us ug",
  vnm_n[12],
  np_lok_b_us_ug$full_place,
  np_lok_db_us_ug$full_place
)

#emu places ####

#n places belong - for overall
np_emu_b_us_ug<- np_fun(
  emu_reasons,
  "Belong",
  "us ug",
  vnm_n[12],
  0
)

#n places belong with n > 19 - for selected places
nsp_emu_b_us_ug<- np_emu_b_us_ug %>%
  filter(n > 19)

#n belong responses contributed to overall emu analyses
nmr_emu_b_us_ug<- nmr_fun(emu_reasons, "us ug", "Belong")

#n places don't belong
np_emu_db_us_ug<- np_fun(
  emu_reasons,
  "Don't Belong",
  "us ug",
  vnm_n[12],
  0
)

#n places don't belong with n > 19 - for selected places
nsp_emu_db_us_ug<- np_emu_db_us_ug %>%
  filter(n > 19)

#n don't belong responses contributed to overall emu analyses
nmr_emu_db_us_ug<- nmr_fun(emu_reasons, "us ug", "Don't Belong")

#n waves
nw_emu_us_ug<- nw_fun(
  emu_reasons,
  "us ug",
  vnm_n[12],
  np_emu_b_us_ug$full_place,
  np_emu_db_us_ug$full_place
)

#international undergrad and grad ####

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#here's what's feasible for analysis of international data:

#belong:

#library
#src
#housing
#lokey
#emu
#mills

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#places that won't be disaggregated (i.e., not housing, lokey, or emu) ####

#n places belong
np_gen_b_i<- campus_reasons %>%
  filter(
    !agg_place %in% c(
      "University Housing",
      "Lokey Complex",
      "Erb Memorial Union"
    )
  ) %>%
  np_fun(
    .,
    "Belong",
    c("intl ug", "intl gr"),
    vnm_n[13],
    19
  ) #autzen complex is ok because only ask followup for stadium

#n places don't belong
np_gen_db_i<- campus_reasons %>%
  filter(
    !agg_place %in% c(
      "University Housing",
      "Lokey Complex",
      "Erb Memorial Union"
    )
  ) %>%
  np_fun(
    .,
    "Don't Belong",
    c("intl ug", "intl gr"),
    vnm_n[13],
    19
  ) #autzen complex is ok because only ask followup for stadium
#insufficient data

#n waves
nw_gen_i<- nw_fun(
  campus_reasons,
  c("intl ug", "intl gr"),
  vnm_n[13],
  np_gen_b_i$agg_place,
  np_gen_db_i$agg_place
)

#university housing places ####

#n places belong - for overall
np_uh_b_i<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  np_fun(
    .,
    "Belong",
    c("intl ug", "intl gr"),
    vnm_n[12],
    0
  )

#n places belong with n > 19 - for selected places
nsp_uh_b_i<- np_uh_b_i %>%
  filter(n > 19) #insufficient data for disaggregation

#n belong responses contributed to overall housing analyses
nmr_uh_b_i<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  nmr_fun(., c("intl ug", "intl gr"), "Belong")

#n places don't belong
np_uh_db_i<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  np_fun(
    .,
    "Don't Belong",
    c("intl ug", "intl gr"),
    vnm_n[12],
    0
  ) #insufficient data for aggregation

#n places don't belong with n > 19 - for selected places
nsp_uh_db_i<- np_uh_db_i %>%
  filter(n > 19) #insufficient data for disaggregation

#n don't belong responses contributed to overall housing analyses
nmr_uh_db_i<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  nmr_fun(., c("intl ug", "intl gr"), "Don't Belong")

#n waves
nw_uh_i<- nw_fun(
  campus_reasons,
  c("intl ug", "intl gr"),
  vnm_n[12],
  np_uh_b_i$full_place,
  np_uh_db_i$full_place
)

#lokey complex places ####

#n places belong - for overall
np_lok_b_i<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  np_fun(
    .,
    "Belong",
    c("intl ug", "intl gr"),
    vnm_n[12],
    0
  )

#n places belong with n > 19 - for selected places
nsp_lok_b_i<- np_lok_b_i %>%
  filter(n > 19) #insufficient data for disaggregation

#n belong responses contributed to overall lokey analyses
nmr_lok_b_i<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  nmr_fun(., c("intl ug", "intl gr"), "Belong")

#n places don't belong
np_lok_db_i<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  np_fun(
    .,
    "Don't Belong",
    c("intl ug", "intl gr"),
    vnm_n[12],
    0
  ) #insufficient data for aggregation

#n places don't belong with n > 19 - for selected places
nsp_lok_db_i<- np_lok_db_i %>%
  filter(n > 19) #insufficient data for disaggregation

#n don't belong responses contributed to overall lokey analyses
nmr_lok_db_i<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  nmr_fun(., c("intl ug", "intl gr"), "Don't Belong")

#n waves
nw_lok_i<- nw_fun(
  campus_reasons,
  c("intl ug", "intl gr"),
  vnm_n[12],
  np_lok_b_i$full_place,
  np_lok_db_i$full_place
)

#emu places ####

#n places belong - for overall
np_emu_b_i<- np_fun(
  emu_reasons,
  "Belong",
  c("intl ug", "intl gr"),
  vnm_n[12],
  0
)

#n places belong with n > 19 - for selected places
nsp_emu_b_i<- np_emu_b_i %>%
  filter(n > 19)

#n belong responses contributed to overall emu analyses
nmr_emu_b_i<- nmr_fun(emu_reasons, c("intl ug", "intl gr"), "Belong")

#n places don't belong
np_emu_db_i<- np_fun(
  emu_reasons,
  "Don't Belong",
  c("intl ug", "intl gr"),
  vnm_n[12],
  0
) #insufficient data for aggregation

#n places don't belong with n > 19 - for selected places
nsp_emu_db_i<- np_emu_db_i %>%
  filter(n > 19) #insufficient data for disaggregation

#n don't belong responses contributed to overall emu analyses
nmr_emu_db_i<- nmr_fun(emu_reasons, c("intl ug", "intl gr"), "Don't Belong")

#n waves
nw_emu_i<- nw_fun(
  emu_reasons,
  c("intl ug", "intl gr"),
  vnm_n[12],
  np_emu_b_i$full_place,
  np_emu_db_i$full_place
)

#us and international grad ####

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#here's what's feasible for analysis of grad data:

#belong:

#library
#src

#don't belong:

#src

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#places that won't be disaggregated (i.e., not housing, lokey, or emu) ####

#n places belong
np_gen_b_gr<- campus_reasons %>%
  filter(
    !agg_place %in% c(
      "University Housing",
      "Lokey Complex",
      "Erb Memorial Union"
    )
  ) %>%
  np_fun(
    .,
    "Belong",
    c("us gr", "intl gr"),
    vnm_n[13],
    19
  ) #autzen complex is ok because only ask followup for stadium

#n places don't belong
np_gen_db_gr<- campus_reasons %>%
  filter(
    !agg_place %in% c(
      "University Housing",
      "Lokey Complex",
      "Erb Memorial Union"
    )
  ) %>%
  np_fun(
    .,
    "Don't Belong",
    c("us gr", "intl gr"),
    vnm_n[13],
    19
  ) #autzen complex is ok because only ask followup for stadium

#n waves
nw_gen_gr<- nw_fun(
  campus_reasons,
  c("us gr", "intl gr"),
  vnm_n[13],
  np_gen_b_gr$agg_place,
  np_gen_db_gr$agg_place
)

#university housing places ####

#n places belong - for overall
np_uh_b_gr<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  np_fun(
    .,
    "Belong",
    c("us gr", "intl gr"),
    vnm_n[12],
    0
  )

#n places belong with n > 19 - for selected places
nsp_uh_b_gr<- np_uh_b_gr %>%
  filter(n > 19) #insufficient data for disaggregation

#n belong responses contributed to overall housing analyses
nmr_uh_b_gr<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  nmr_fun(., c("us gr", "intl gr"), "Belong")

#n places don't belong
np_uh_db_gr<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  np_fun(
    .,
    "Don't Belong",
    c("us gr", "intl gr"),
    vnm_n[12],
    0
  ) #insufficient data for aggregation

#n places don't belong with n > 19 - for selected places
nsp_uh_db_gr<- np_uh_db_gr %>%
  filter(n > 19) #insufficient data for disaggregation

#n don't belong responses contributed to overall housing analyses
nmr_uh_db_gr<- campus_reasons %>%
  filter(agg_place == "University Housing") %>%
  nmr_fun(., c("us gr", "intl gr"), "Don't Belong")

#n waves
nw_uh_gr<- nw_fun(
  campus_reasons,
  c("us gr", "intl gr"),
  vnm_n[12],
  np_uh_b_gr$full_place,
  np_uh_db_gr$full_place
)

#lokey complex places ####
#!!!!!!!!!!! insufficient data !!!!!!!!!!!

#n places belong - for overall
np_lok_b_gr<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  np_fun(
    .,
    "Belong",
    c("us gr", "intl gr"),
    vnm_n[12],
    0
  ) #insufficient data for aggregation

#n places belong with n > 19 - for selected places
nsp_lok_b_gr<- np_lok_b_gr %>%
  filter(n > 19) #insufficient data for disaggregation

#n belong responses contributed to overall lokey analyses
nmr_lok_b_gr<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  nmr_fun(., c("us gr", "intl gr"), "Belong")

#n places don't belong
np_lok_db_gr<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  np_fun(
    .,
    "Don't Belong",
    c("us gr", "intl gr"),
    vnm_n[12],
    0
  ) #insufficient data for aggregation

#n places don't belong with n > 19 - for selected places
nsp_lok_db_gr<- np_lok_db_gr %>%
  filter(n > 19) #insufficient data for disaggregation

#n don't belong responses contributed to overall lokey analyses
nmr_lok_db_gr<- campus_reasons %>%
  filter(agg_place == "Lokey Complex") %>%
  nmr_fun(., c("us gr", "intl gr"), "Don't Belong")

#n waves
nw_lok_gr<- nw_fun(
  campus_reasons,
  c("us gr", "intl gr"),
  vnm_n[12],
  np_lok_b_gr$full_place,
  np_lok_db_gr$full_place
) #insufficient data

#emu places ####
#!!!!!!!!!!! insufficient data !!!!!!!!!!!

#n places belong - for overall
np_emu_b_gr<- np_fun(
  emu_reasons,
  "Belong",
  c("us gr", "intl gr"),
  vnm_n[12],
  0
) #insufficient data for aggregation

#n places belong with n > 19 - for selected places
nsp_emu_b_gr<- np_emu_b_gr %>%
  filter(n > 19) #insufficient data for disaggregation

#n belong responses contributed to overall emu analyses
nmr_emu_b_gr<- nmr_fun(emu_reasons, c("us gr", "intl gr"), "Belong")

#n places don't belong
np_emu_db_gr<- np_fun(
  emu_reasons,
  "Don't Belong",
  c("us gr", "intl gr"),
  vnm_n[12],
  0
) #insufficient data for aggregation

#n places don't belong with n > 19 - for selected places
nsp_emu_db_gr<- np_emu_db_gr %>%
  filter(n > 19) #insufficient data for disaggregation

#n don't belong responses contributed to overall emu analyses
nmr_emu_db_gr<- nmr_fun(emu_reasons, c("us gr", "intl gr"), "Don't Belong")

#n waves
nw_emu_gr<- nw_fun(
  emu_reasons,
  c("us gr", "intl gr"),
  vnm_n[12],
  np_emu_b_gr$full_place,
  np_emu_db_gr$full_place
) #insufficient data

#visualize reasons data ####

#us undergrad ####

#bigrams ####

#campus data
b_pos<- pos_cam %>%
  filter(
    sent == "Belong" &
      sample == "us ug"
  )

db_pos<- pos_cam %>%
  filter(
    sent == "Don't Belong" &
      sample == "us ug"
  )

#allen
bg_allen_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Allen",
  1,
  c(junk_noun, junk_adj, "allen"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_allen_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Allen",
  c(junk_noun, junk_adj, "allen"),
  1
)

#autzen
bg_autzen_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Autzen Stadium",
  1,
  c(junk_noun, junk_adj, "autzen"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_autzen_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Autzen Stadium",
  c(junk_noun, junk_adj, "autzen"),
  1
)

bg_autzen_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Autzen Stadium",
  1,
  c(junk_noun, junk_adj, "autzen"),
  "#357BA2",
  "#78D6AE"
)

cmm_autzen_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Autzen Stadium",
  c(junk_noun, junk_adj, "autzen"),
  1
)

#cemetery
bg_cemetery_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Cemetery",
  0,
  c(junk_noun, junk_adj, "cemetery"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_cemetery_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Cemetery",
  c(junk_noun, junk_adj, "cemetery"),
  0
)

bg_cemetery_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Cemetery",
  0,
  c(junk_noun, junk_adj, "cemetery"),
  "#357BA2",
  "#78D6AE"
)

cmm_cemetery_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Cemetery",
  c(junk_noun, junk_adj, "cemetery"),
  0
)

#chapman
bg_chapman_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Chapman",
  1,
  c(junk_noun, junk_adj, "chapman"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_chapman_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Chapman",
  c(junk_noun, junk_adj, "chapman"),
  1
)

#frohnmayer
bg_frohnmayer_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Frohnmayer",
  1,
  c(junk_noun, junk_adj, "frohnmayer"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_frohnmayer_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Frohnmayer",
  c(junk_noun, junk_adj, "frohnmayer"),
  1
)

bg_frohnmayer_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Frohnmayer",
  1,
  c(junk_noun, junk_adj, "frohnmayer"),
  "#357BA2",
  "#78D6AE"
)

cmm_frohnmayer_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Frohnmayer",
  c(junk_noun, junk_adj, "frohnmayer"),
  1
)

#hayward
bg_hayward_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Hayward Field",
  1,
  c(junk_noun, junk_adj, "hayward"),
  "#357BA2",
  "#78D6AE"
)

cmm_hayward_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Hayward Field",
  c(junk_noun, junk_adj, "hayward"),
  1
)

#hedco
bg_hedco_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "HEDCO",
  0,
  c(junk_noun, junk_adj, "hedco"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_hedco_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "HEDCO",
  c(junk_noun, junk_adj, "hedco"),
  0
)

bg_hedco_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "HEDCO",
  0,
  c(junk_noun, junk_adj, "hedco"),
  "#357BA2",
  "#78D6AE"
)

cmm_hedco_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "HEDCO",
  c(junk_noun, junk_adj, "hedco"),
  0
)

#jaqua
bg_jaqua_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Jaqua",
  0,
  c(junk_noun, junk_adj, "jaqua"),
  "#357BA2",
  "#78D6AE"
)

cmm_jaqua_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Jaqua",
  c(junk_noun, junk_adj, "jaqua"),
  0
)

#law
bg_law_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Knight Law",
  0,
  c(junk_noun, junk_adj, "knight"),
  "#357BA2",
  "#78D6AE"
)

cmm_law_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Knight Law",
  c(junk_noun, junk_adj, "knight"),
  0
)

#lawrence
bg_lawrence_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Lawrence",
  1,
  c(junk_noun, junk_adj, "lawrence"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_lawrence_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Lawrence",
  c(junk_noun, junk_adj, "lawrence"),
  1
)

bg_lawrence_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Lawrence",
  0,
  c(junk_noun, junk_adj, "lawrence"),
  "#357BA2",
  "#78D6AE"
)

cmm_lawrence_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Lawrence",
  c(junk_noun, junk_adj, "lawrence"),
  0
)

#library
bg_library_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  2,
  c(junk_noun, junk_adj, "library", "knight"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_library_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  c(junk_noun, junk_adj, "library", "knight"),
  2
)

bg_library_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Library",
  1,
  c(junk_noun, junk_adj, "library", "knight"),
  "#357BA2",
  "#78D6AE"
)

cmm_library_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Library",
  c(junk_noun, junk_adj, "library", "knight"),
  1
)

#lillis
bg_lillis_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "Lillis Complex",
  1,
  c(junk_noun, junk_adj, "lillis", "lundquist"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_lillis_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "Lillis Complex",
  c(junk_noun, junk_adj, "lillis", "lundquist"),
  1
)

bg_lillis_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[19],
  "Lillis Complex",
  1,
  c(junk_noun, junk_adj, "lillis", "lundquist"),
  "#357BA2",
  "#78D6AE"
)

cmm_lillis_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[19],
  "Lillis Complex",
  c(junk_noun, junk_adj, "lillis", "lundquist"),
  1
)

#lokey overall
bg_lokey_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "Lokey Complex",
  1,
  c(junk_noun, junk_adj, "lokey"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_lokey_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "Lokey Complex",
  c(junk_noun, junk_adj, "lokey"),
  1
)

bg_lokey_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[19],
  "Lokey Complex",
  1,
  c(junk_noun, junk_adj, "lokey"),
  "#357BA2",
  "#78D6AE"
)

cmm_lokey_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[19],
  "Lokey Complex",
  c(junk_noun, junk_adj, "lokey"),
  1
)

#columbia
bg_columbia_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Columbia",
  0,
  c(junk_noun, junk_adj, "columbia"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_columbia_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Columbia",
  c(junk_noun, junk_adj, "columbia"),
  0
)

#klamath
bg_klamath_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Klamath",
  0,
  c(junk_noun, junk_adj, "klamath"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_klamath_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Columbia",
  c(junk_noun, junk_adj, "klamath"),
  0
)

bg_klamath_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Klamath",
  0,
  c(junk_noun, junk_adj, "klamath"),
  "#357BA2",
  "#78D6AE"
)

cmm_klamath_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Columbia",
  c(junk_noun, junk_adj, "klamath"),
  0
)

#lisb
bg_lisb_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Lewis Integrative Science",
  0,
  c(junk_noun, junk_adj, "lisb", "lewis"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_lisb_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Lewis Integrative Science",
  c(junk_noun, junk_adj, "lisb", "lewis"),
  0
)

bg_lisb_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Lewis Integrative Science",
  0,
  c(junk_noun, junk_adj, "lisb", "lewis"),
  "#357BA2",
  "#78D6AE"
)

cmm_lisb_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Lewis Integrative Science",
  c(junk_noun, junk_adj, "lisb", "lewis"),
  0
)

#sci commons
bg_scicom_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Science Commons",
  1,
  c(junk_noun, junk_adj, "commons", "library"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_scicom_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Science Commons",
  c(junk_noun, junk_adj, "commons", "library"),
  1
)

#willamette
bg_willamette_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Willamette",
  1,
  c(junk_noun, junk_adj, "willamette"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_willamette_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Willamette",
  c(junk_noun, junk_adj, "willamette"),
  1
)

bg_willamette_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Willamette",
  1,
  c(junk_noun, junk_adj, "willamette"),
  "#357BA2",
  "#78D6AE"
)

cmm_willamette_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Willamette",
  c(junk_noun, junk_adj, "willamette"),
  1
)

#matt knight
bg_mattknight_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Matthew Knight Arena",
  1,
  c(junk_noun, junk_adj, "matt", "matthew", "knight"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_mattknight_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Matthew Knight Arena",
  c(junk_noun, junk_adj, "matt", "matthew", "knight"),
  1
)

bg_mattknight_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Matthew Knight Arena",
  1,
  c(junk_noun, junk_adj, "matt", "matthew", "knight"),
  "#357BA2",
  "#78D6AE"
)

cmm_mattknight_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Matthew Knight Arena",
  c(junk_noun, junk_adj, "matt", "matthew", "knight"),
  1
)

#mckenzie
bg_mckenzie_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "McKenzie",
  0,
  c(junk_noun, junk_adj, "mckenzie"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_mckenzie_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "McKenzie",
  c(junk_noun, junk_adj, "mckenzie"),
  0
)

#oregon
bg_oregon_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Oregon",
  0,
  c(junk_noun, junk_adj, "oregon"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_oregon_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Oregon",
  c(junk_noun, junk_adj, "oregon"),
  0
)

#straub
bg_straub_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Straub",
  0,
  c(junk_noun, junk_adj, "straub"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_straub_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Straub",
  c(junk_noun, junk_adj, "straub"),
  0
)

#src
bg_src_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  2,
  c(junk_noun, junk_adj, "src", "the rec", "rec", "center"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_src_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  c(junk_noun, junk_adj, "src", "the rec", "rec", "center"),
  2
)

bg_src_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  2,
  c(junk_noun, junk_adj, "src", "the rec", "rec", "center"),
  "#357BA2",
  "#78D6AE"
)

cmm_src_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  c(junk_noun, junk_adj, "src", "the rec", "rec", "center"),
  2
)

#tykeson
bg_tykeson_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Tykeson",
  0,
  c(junk_noun, junk_adj, "tykeson"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_tykeson_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Tykeson",
  c(junk_noun, junk_adj, "tykeson"),
  0
)

#uhs
bg_uhs_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "University Health Services",
  0,
  c(junk_noun, junk_adj),
  "#CB1B4F",
  "#F6AA82"
)

cmm_uhs_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "University Health Services",
  c(junk_noun, junk_adj),
  0
)

#housing overall
bg_housing_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "University Housing",
  2,
  c(junk_noun, junk_adj),
  "#CB1B4F",
  "#F6AA82"
)

cmm_housing_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "University Housing",
  c(junk_noun, junk_adj),
  2
)

bg_housing_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[19],
  "University Housing",
  1,
  c(junk_noun, junk_adj),
  "#357BA2",
  "#78D6AE"
)

cmm_housing_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[19],
  "University Housing",
  c(junk_noun, junk_adj),
  2
)

#barnhart
bg_barnhart_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Barnhart",
  0,
  c(junk_noun, junk_adj, "barnhart"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_barnhart_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Barnhart",
  c(junk_noun, junk_adj, "barnhart"),
  0
)

#bean
bg_bean_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Bean",
  1,
  c(junk_noun, junk_adj, "bean"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_bean_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Bean",
  c(junk_noun, junk_adj, "bean"),
  1
)

bg_bean_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Bean",
  1,
  c(junk_noun, junk_adj, "bean"),
  "#357BA2",
  "#78D6AE"
) #this is a good example of how unique pairs can junk up a bigram simply because pairs are unique (i.e., freq of cooc is low). threshold of 0 is unintelligible nonsense. threshold of 1 has 3 intelligible even if uninformative connections

cmm_bean_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Bean",
  c(junk_noun, junk_adj, "bean"),
  1
)

#carson
bg_carson_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Carson",
  1,
  c(junk_noun, junk_adj, "carson"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_carson_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Carson",
  c(junk_noun, junk_adj, "carson"),
  1
)

bg_carson_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Carson",
  1,
  c(junk_noun, junk_adj, "carson"),
  "#357BA2",
  "#78D6AE"
)

cmm_carson_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Carson",
  c(junk_noun, junk_adj, "carson"),
  1
)

#earl
bg_earl_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Earl",
  0,
  c(junk_noun, junk_adj, "earl"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_earl_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Earl",
  c(junk_noun, junk_adj, "earl"),
  0
)

#gsh
bg_gsh_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Global Scholars",
  1,
  c(junk_noun, junk_adj, "gsh", "global", "scholars"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_gsh_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Global Scholars",
  c(junk_noun, junk_adj, "gsh", "global", "scholars"),
  1
)

bg_gsh_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Global Scholars",
  1,
  c(junk_noun, junk_adj, "gsh", "global", "scholars"),
  "#357BA2",
  "#78D6AE"
)

cmm_gsh_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Global Scholars",
  c(junk_noun, junk_adj, "gsh", "global", "scholars"),
  1
)

#hamilton
bg_hamilton_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Hamilton",
  1,
  c(junk_noun, junk_adj, "hamilton"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_hamilton_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Hamilton",
  c(junk_noun, junk_adj, "hamilton"),
  1
)

bg_hamilton_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Hamilton",
  0,
  c(junk_noun, junk_adj, "hamilton"),
  "#357BA2",
  "#78D6AE"
)

cmm_hamilton_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Hamilton",
  c(junk_noun, junk_adj, "hamilton"),
  1
)

#kalapuya
bg_kalapuya_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Kalapuya Ilihi",
  1,
  c(junk_noun, junk_adj, "kalapuya"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_kalapuya_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Kalapuya Ilihi",
  c(junk_noun, junk_adj, "kalapuya"),
  1
)

bg_kalapuya_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Kalapuya Ilihi",
  1,
  c(junk_noun, junk_adj, "kalapuya"),
  "#357BA2",
  "#78D6AE"
)

cmm_kalapuya_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Kalapuya Ilihi",
  c(junk_noun, junk_adj, "kalapuya"),
  1
)

#llc
bg_llc_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Living Learning Center",
  0,
  c(junk_noun, junk_adj, "llc", "living", "center"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_llc_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Living Learning Center",
  c(junk_noun, junk_adj, "llc", "living", "center"),
  0
)

bg_llc_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Living Learning Center",
  0,
  c(junk_noun, junk_adj, "llc", "living", "center"),
  "#357BA2",
  "#78D6AE"
)

cmm_llc_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Living Learning Center",
  c(junk_noun, junk_adj, "llc", "living", "center"),
  0
)

#unthank
bg_unthank_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Unthank",
  0,
  c(junk_noun, junk_adj, "unthank"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_unthank_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Unthank",
  c(junk_noun, junk_adj, "unthank"),
  0
)

bg_unthank_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Unthank",
  0,
  c(junk_noun, junk_adj, "unthank"),
  "#357BA2",
  "#78D6AE"
)

cmm_unthank_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Unthank",
  c(junk_noun, junk_adj, "unthank"),
  0
)

#walton
bg_walton_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Walton",
  0,
  c(junk_noun, junk_adj, "walton"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_walton_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Walton",
  c(junk_noun, junk_adj, "walton"),
  0
)

bg_walton_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Walton",
  0,
  c(junk_noun, junk_adj, "walton"),
  "#357BA2",
  "#78D6AE"
)

cmm_walton_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Walton",
  c(junk_noun, junk_adj, "walton"),
  0
)

#emu data
b_pos<- pos_emu %>%
  filter(
    sent == "Belong" &
      sample == "us ug"
  )

db_pos<- pos_emu %>%
  filter(
    sent == "Don't Belong" &
      sample == "us ug"
  )

#emu overall
bg_emu_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  c(np_emu_b_us_ug$full_place),
  2,
  c(junk_noun, junk_adj, "emu"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_emu_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  c(np_emu_b_us_ug$full_place),
  c(junk_noun, junk_adj, "emu"),
  2
)

bg_emu_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  c(np_emu_db_us_ug$full_place),
  1,
  c(junk_noun, junk_adj, "emu"),
  "#357BA2",
  "#78D6AE"
)

cmm_emu_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  c(np_emu_b_us_ug$full_place),
  c(junk_noun, junk_adj, "emu"),
  1
)

#atrium east
bg_atrium_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Atrium East",
  0,
  c(junk_noun, junk_adj, "atrium"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_atrium_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Atrium East",
  c(junk_noun, junk_adj, "atrium"),
  0
)

#courtyard
bg_courtyard_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Courtyard",
  0,
  c(junk_noun, junk_adj, "courtyard"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_courtyard_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Courtyard",
  c(junk_noun, junk_adj, "courtyard"),
  0
)

#craft center
bg_craft_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Craft Center",
  0,
  c(junk_noun, junk_adj, "center"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_craft_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Craft Center",
  c(junk_noun, junk_adj, "center"),
  0
)

#duck nest
bg_ducknest_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Duck Nest",
  0,
  c(junk_noun, junk_adj, "nest"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_ducknest_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Duck Nest",
  c(junk_noun, junk_adj, "nest"),
  0
)

#falling sky
bg_fallingsky_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Falling Sky Pizzeria and Public House",
  0,
  c(junk_noun, junk_adj, "falling", "fall", "sky"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_fallingsky_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Falling Sky Pizzeria and Public House",
  c(junk_noun, junk_adj, "falling", "fall", "sky"),
  0
)

#fishbowl
bg_fishbowl_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Fishbowl",
  1,
  c(junk_noun, junk_adj, "fishbowl"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_fishbowl_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Fishbowl",
  c(junk_noun, junk_adj, "fishbowl"),
  1
)

bg_fishbowl_db_us_ug<- bigram_fun(
  db_pos,
  vnm_pos[18],
  "Fishbowl",
  0,
  c(junk_noun, junk_adj, "fishbowl"),
  "#357BA2",
  "#78D6AE"
)

cmm_fishbowl_db_us_ug<- cooc_minmax_fun(
  db_pos,
  vnm_pos[18],
  "Fishbowl",
  c(junk_noun, junk_adj, "fishbowl"),
  0
)

#fresh market
bg_freshmarket_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Fresh Market",
  0,
  c(junk_noun, junk_adj, "market"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_freshmarket_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Fresh Market",
  c(junk_noun, junk_adj, "market"),
  0
)

#lgbtqa3
bg_lgbtqa3_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "LGBTQA3",
  0,
  c(junk_noun, junk_adj, "lgbtqa3"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_lgbtqa3_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "LGBTQA3",
  c(junk_noun, junk_adj, "lgbtqa3"),
  0
)

#mills
bg_mills_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Mills International Center",
  0,
  c(junk_noun, junk_adj, "mills", "center"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_mills_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Mills International Center",
  c(junk_noun, junk_adj, "mills", "center"),
  0
)

#mcc
bg_mcc_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Multicultural Center",
  0,
  c(junk_noun, junk_adj, "mcc", "center", "international"), #people have a tendency to think mcc is mills, so deleting the obvious
  "#CB1B4F",
  "#F6AA82"
)

cmm_mcc_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Multicultural Center",
  c(junk_noun, junk_adj, "mcc", "center", "international"),
  0
)

#olounge
bg_olounge_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "O Lounge",
  0,
  c(junk_noun, junk_adj, " o ", "lounge"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_olounge_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "O Lounge",
  c(junk_noun, junk_adj, " o ", "lounge"),
  0
)

#taylor
bg_taylor_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Taylor Lounge",
  0,
  c(junk_noun, junk_adj, "taylor"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_taylor_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Taylor Lounge",
  c(junk_noun, junk_adj, " o ", "taylor"),
  0
)

#womens
bg_womens_b_us_ug<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Women's Center",
  1,
  c(junk_noun, junk_adj),
  "#CB1B4F",
  "#F6AA82"
)

cmm_womens_b_us_ug<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Women's Center",
  c(junk_noun, junk_adj, " o ", "womens"),
  1
)

#wordclouds ####

#campus data
b_pos<- pos_cam %>%
  filter(
    sent == "Belong" &
      sample == "us ug"
  )

db_pos<- pos_cam %>%
  filter(
    sent == "Don't Belong" &
      sample == "us ug"
  )

#allen
wc_allen_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Allen",
  1,
  c(junk_noun, junk_adj, "allen"),
  "#F6AA82",
  "#CB1B4F"
)

#autzen
wc_autzen_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Autzen Stadium",
  1,
  c(junk_noun, junk_adj, "autzen", "autzen stadium"),
  "#F6AA82",
  "#CB1B4F"
)

wc_autzen_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Autzen Stadium",
  1,
  c(junk_noun, junk_adj, "autzen", "autzen stadium"),
  "#78D6AE",
  "#357BA2"
)

#cemetery
wc_cemetery_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Cemetery",
  1,
  c(junk_noun, junk_adj, "cemetery"),
  "#F6AA82",
  "#CB1B4F"
)

wc_cemetery_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Cemetery",
  1,
  c(junk_noun, junk_adj, "cemetery"),
  "#78D6AE",
  "#357BA2"
)

#chapman
wc_chapman_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Chapman",
  1,
  c(junk_noun, junk_adj, "chapman"),
  "#F6AA82",
  "#CB1B4F"
)

#frohnmayer
wc_frohnmayer_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Frohnmayer",
  1,
  c(junk_noun, junk_adj, "frohnmayer"),
  "#F6AA82",
  "#CB1B4F"
)

wc_frohnmayer_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Frohnmayer",
  1,
  c(junk_noun, junk_adj, "frohnmayer"),
  "#78D6AE",
  "#357BA2"
)

#hayward
wc_hayward_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Hayward Field",
  1,
  c(junk_noun, junk_adj, "hayward", "hayward field"),
  "#78D6AE",
  "#357BA2"
)

#hedco
wc_hedco_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "HEDCO",
  1,
  c(junk_noun, junk_adj, "hedco"),
  "#F6AA82",
  "#CB1B4F"
)

wc_hedco_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "HEDCO",
  1,
  c(junk_noun, junk_adj, "hedco"),
  "#78D6AE",
  "#357BA2"
)

#jaqua
wc_jaqua_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Jaqua",
  1,
  c(junk_noun, junk_adj, "jaqua", "jaqua center"),
  "#78D6AE",
  "#357BA2"
)

#law
wc_law_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Knight Law",
  1,
  c(junk_noun, junk_adj, "knight", "law center"),
  "#78D6AE",
  "#357BA2"
)

#lawrence
wc_lawrence_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Lawrence",
  1,
  c(junk_noun, junk_adj, "lawrence"),
  "#F6AA82",
  "#CB1B4F"
)

wc_lawrence_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Lawrence",
  1,
  c(junk_noun, junk_adj, "lawrence"),
  "#78D6AE",
  "#357BA2"
)

#library
wc_library_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  1,
  c(junk_noun, junk_adj, "library", "knight library"),
  "#F6AA82",
  "#CB1B4F"
)

wc_library_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Library",
  1,
  c(junk_noun, junk_adj, "library", "knight library"),
  "#78D6AE",
  "#357BA2"
)

#lillis
wc_lillis_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "Lillis Complex",
  1,
  c(junk_noun, junk_adj, "lillis"),
  "#F6AA82",
  "#CB1B4F"
)

wc_lillis_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[19],
  "Lillis Complex",
  1,
  c(junk_noun, junk_adj, "lillis"),
  "#78D6AE",
  "#357BA2"
)

#lokey overall
wc_lokey_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "Lokey Complex",
  1,
  c(junk_noun, junk_adj),
  "#F6AA82",
  "#CB1B4F"
)

wc_lokey_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[19],
  "Lokey Complex",
  1,
  c(junk_noun, junk_adj),
  "#78D6AE",
  "#357BA2"
)

#columbia
wc_columbia_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Columbia",
  1,
  c(junk_noun, junk_adj, "columbia", "hall", "columbia hall"),
  "#F6AA82",
  "#CB1B4F"
)

#klamath
wc_klamath_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Klamath",
  1,
  c(junk_noun, junk_adj, "klamath", "hall", "klamath hall"),
  "#F6AA82",
  "#CB1B4F"
)

wc_klamath_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Klamath",
  1,
  c(junk_noun, junk_adj, "klamath", "hall", "klamath hall"),
  "#78D6AE",
  "#357BA2"
)

#lisb
wc_lisb_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Lewis Integrative Science",
  1,
  c(junk_noun, junk_adj, "lisb", "lewis"),
  "#F6AA82",
  "#CB1B4F"
)

wc_lisb_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Lewis Integrative Science",
  1,
  c(junk_noun, junk_adj, "lisb", "lewis"),
  "#78D6AE",
  "#357BA2"
)

#sci commons
wc_scicom_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Science Commons",
  1,
  c(junk_noun, junk_adj, "science commons", "science library"),
  "#F6AA82",
  "#CB1B4F"
)

#willamette
wc_willamette_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Willamette",
  1,
  c(junk_noun, junk_adj, "willamette", "hall", "willamette hall"),
  "#F6AA82",
  "#CB1B4F"
)

wc_willamette_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Willamette",
  1,
  c(junk_noun, junk_adj, "willamette", "hall", "willamette hall"),
  "#78D6AE",
  "#357BA2"
)

#matt knight
wc_mattknight_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Matthew Knight Arena",
  1,
  c(junk_noun, junk_adj, "matt", "matthew", "knight"),
  "#F6AA82",
  "#CB1B4F"
)

wc_mattknight_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Matthew Knight Arena",
  1,
  c(junk_noun, junk_adj, "matt", "matthew", "knight"),
  "#78D6AE",
  "#357BA2"
)

#mckenzie
wc_mckenzie_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "McKenzie",
  1,
  c(junk_noun, junk_adj, "mckenzie"),
  "#F6AA82",
  "#CB1B4F"
)

#oregon
wc_oregon_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Oregon",
  1,
  c(junk_noun, junk_adj, "oregon", "oregon hall"),
  "#F6AA82",
  "#CB1B4F"
)

#straub
wc_straub_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Straub",
  1,
  c(junk_noun, junk_adj, "straub"),
  "#F6AA82",
  "#CB1B4F"
)

#src
wc_src_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  1,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#F6AA82",
  "#CB1B4F"
)

wc_src_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  1,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#78D6AE",
  "#357BA2"
)

#tykeson
wc_tykeson_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Tykeson",
  1,
  c(junk_noun, junk_adj, "tykeson"),
  "#F6AA82",
  "#CB1B4F"
)

#uhs
wc_uhs_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "University Health Services",
  1,
  c(junk_noun, junk_adj),
  "#F6AA82",
  "#CB1B4F"
)

#housing overall
wc_housing_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "University Housing",
  1,
  c(junk_noun, junk_adj),
  "#F6AA82",
  "#CB1B4F"
)

wc_housing_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[19],
  "University Housing",
  1,
  c(junk_noun, junk_adj),
  "#78D6AE",
  "#357BA2"
)

#barnhart
wc_barnhart_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Barnhart",
  1,
  c(junk_noun, junk_adj, "barnhart", "hall", "barnhart hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

#bean
wc_bean_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Bean",
  1,
  c(junk_noun, junk_adj, "bean", "hall", "bean hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_bean_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Bean",
  1,
  c(junk_noun, junk_adj, "bean", "hall", "bean hall", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#carson
wc_carson_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Carson",
  1,
  c(junk_noun, junk_adj, "carson", "hall", "carson hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_carson_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Carson",
  1,
  c(junk_noun, junk_adj, "carson", "hall", "carson hall", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#earl
wc_earl_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Earl",
  1,
  c(junk_noun, junk_adj, "earl", "hall", "earl hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

#gsh
wc_gsh_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Global Scholars",
  1,
  c(junk_noun, junk_adj, "gsh", "global scholars", "hall", "global scholars hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_gsh_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Global Scholars",
  1,
  c(junk_noun, junk_adj, "gsh", "global scholars", "hall", "global scholars hall", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#hamilton
wc_hamilton_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Hamilton",
  1,
  c(junk_noun, junk_adj, "hamilton", "hall", "hamilton hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_hamilton_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Hamilton",
  1,
  c(junk_noun, junk_adj, "hamilton", "hall", "hamilton hall", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#kalapuya
wc_kalapuya_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Kalapuya Ilihi",
  1,
  c(junk_noun, junk_adj, "kalapuya", "hall", "kalapuya hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_kalapuya_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Kalapuya Ilihi",
  1,
  c(junk_noun, junk_adj, "kalapuya", "hall", "kalapuya hall", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#llc
wc_llc_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Living Learning Center",
  1,
  c(junk_noun, junk_adj, "llc", "living learning center", "center", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_llc_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Living Learning Center",
  1,
  c(junk_noun, junk_adj, "llc", "living learning center", "center", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#unthank
wc_unthank_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Unthank",
  1,
  c(junk_noun, junk_adj, "unthank", "hall", "unthank hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_unthank_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Unthank",
  1,
  c(junk_noun, junk_adj, "unthank", "hall", "unthank hall", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#walton
wc_walton_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Walton",
  1,
  c(junk_noun, junk_adj, "walton", "hall", "walton hall", "residence hall", "dorm"),
  "#F6AA82",
  "#CB1B4F"
)

wc_walton_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Walton",
  1,
  c(junk_noun, junk_adj, "walton", "hall", "walton hall", "residence hall", "dorm"),
  "#78D6AE",
  "#357BA2"
)

#emu data
b_pos<- pos_emu %>%
  filter(
    sent == "Belong" &
      sample == "us ug"
  )

db_pos<- pos_emu %>%
  filter(
    sent == "Don't Belong" &
      sample == "us ug"
  )

#emu overall
wc_emu_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  c(np_emu_b_us_ug$full_place),
  1,
  c(junk_noun, junk_adj, "emu"),
  "#F6AA82",
  "#CB1B4F"
)

wc_emu_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  c(np_emu_db_us_ug$full_place),
  1,
  c(junk_noun, junk_adj, "emu"),
  "#78D6AE",
  "#357BA2"
)

#atrium east
wc_atrium_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Atrium East",
  1,
  c(junk_noun, junk_adj, "atrium", "atrium east", "east atrium"),
  "#F6AA82",
  "#CB1B4F"
)

#courtyard
wc_courtyard_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Courtyard",
  1,
  c(junk_noun, junk_adj, "courtyard"),
  "#F6AA82",
  "#CB1B4F"
)

#craft center
wc_craft_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Craft Center",
  1,
  c(junk_noun, junk_adj, "center", "craft center"),
  "#F6AA82",
  "#CB1B4F"
)

#duck nest
wc_ducknest_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Duck Nest",
  1,
  c(junk_noun, junk_adj, "duck nest"),
  "#F6AA82",
  "#CB1B4F"
)

#falling sky
wc_fallingsky_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Falling Sky Pizzeria and Public House",
  1,
  c(junk_noun, junk_adj, "falling sky", "fall sky", "sky"),
  "#F6AA82",
  "#CB1B4F"
)

#fishbowl
wc_fishbowl_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Fishbowl",
  1,
  c(junk_noun, junk_adj, "fishbowl"),
  "#F6AA82",
  "#CB1B4F"
)

wc_fishbowl_db_us_ug<- cloud_fun(
  db_pos,
  vnm_pos[18],
  "Fishbowl",
  1,
  c(junk_noun, junk_adj, "fishbowl"),
  "#78D6AE",
  "#357BA2"
)

#fresh market
wc_freshmarket_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Fresh Market",
  1,
  c(junk_noun, junk_adj, "fresh market", "market"),
  "#F6AA82",
  "#CB1B4F"
)

#lgbtqa3
wc_lgbtqa3_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "LGBTQA3",
  1,
  c(junk_noun, junk_adj, "lgbtqa3", "lgbtqa3 center"),
  "#F6AA82",
  "#CB1B4F"
)

#mills
wc_mills_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Mills International Center",
  1,
  c(junk_noun, junk_adj, "mills", "mills center"),
  "#F6AA82",
  "#CB1B4F"
)

#mcc
wc_mcc_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Multicultural Center",
  1,
  c(junk_noun, junk_adj, "mcc", "multicultural center"),
  "#F6AA82",
  "#CB1B4F"
)

#olounge
wc_olounge_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "O Lounge",
  1,
  c(junk_noun, junk_adj, "o lounge", "of lounge", "lounge"),
  "#F6AA82",
  "#CB1B4F"
)

#taylor
wc_taylor_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Taylor Lounge",
  1,
  c(junk_noun, junk_adj, "taylor", "taylor lounge", "lounge"),
  "#F6AA82",
  "#CB1B4F"
)

#womens
wc_womens_b_us_ug<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Women's Center",
  1,
  c(junk_noun, junk_adj, "center", "womens center"),
  "#F6AA82",
  "#CB1B4F"
)

#emotions ####

#campus data
b_emo<- emo_cam %>%
  filter(
    sent == "Belong" &
      sample == "us ug"
  )

db_emo<- emo_cam %>%
  filter(
    sent == "Don't Belong" &
      sample == "us ug"
  )

#allen
ebar_allen_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Allen",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_allen_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#autzen
ebar_autzen_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Autzen Stadium",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_autzen_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Autzen Stadium",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#cemetery
ebar_cemetery_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Cemetery",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_cemetery_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Cemetery",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#chapman
ebar_chapman_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Chapman",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_chapman_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#frohnmayer
ebar_frohnmayer_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Frohnmayer",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_frohnmayer_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Frohnmayer",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#hayward
ebar_hayward_b_us_ug<- image_read("images/pbb/no_data_rocket_b.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")


ebar_hayward_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Hayward Field",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#hedco
ebar_hedco_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "HEDCO",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_hedco_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "HEDCO",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(4, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 1.5,
    y = 40,
    size = 5,
    label = "0 words classified as",
    color = "#78D6AE"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.2,
    y = 40,
    size = 5,
    label = "anger, fear, joy, or surprise",
    color = "#78D6AE"
  )
#^^^ 4 of 8 emotions - missing anger, fear, joy, or surprise

#jaqua
ebar_jaqua_b_us_ug<- image_read("images/pbb/no_data_rocket_b.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

ebar_jaqua_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Jaqua",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#law
ebar_law_b_us_ug<- image_read("images/pbb/no_data_rocket_b.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

ebar_law_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Knight Law",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50, 60, 70, 80),
  c(0, 80),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 3,
    y = 55,
    size = 5,
    label = "0 words classified as",
    color = "#78D6AE"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.6,
    y = 55,
    size = 5,
    label = "anger or surprise",
    color = "#78D6AE"
  )
#^^^ 6 of 8 emotions - missing anger and surprise

#lawrence
ebar_lawrence_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Lawrence",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_lawrence_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Lawrence",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 3,
    y = 40,
    size = 5,
    label = "0 words classified as",
    color = "#78D6AE"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.6,
    y = 40,
    size = 5,
    label = "anger or fear",
    color = "#78D6AE"
  )
#^^^ 6 of 8 emotions - missing anger and fear

#library
ebar_library_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Library",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_library_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Library",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#lillis
ebar_lillis_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "Lillis Complex",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_lillis_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[12],
  "Lillis Complex",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#lokey overall
ebar_lokey_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "Lokey Complex",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_lokey_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[12],
  "Lokey Complex",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#columbia
ebar_columbia_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Columbia",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.5,
    y = 35,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.1,
    y = 35,
    size = 5,
    label = "fear or sadness",
    color = "#F6AA82"
  )
#^^^ 6 of 8 emotions - missing fear and sadness

ebar_columbia_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#klamath
ebar_klamath_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Klamath",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_klamath_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Klamath",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#lisb
ebar_lisb_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Lewis Integrative Science",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(5, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 1.6,
    y = 35,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.2,
    y = 35,
    size = 5,
    label = "anger, disgust, sadness",
    color = "#F6AA82"
  )
#^^^ 5 of 8 emotions - missing anger, disgust, and sadness

ebar_lisb_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Lewis Integrative Science",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#sci commons
ebar_scicom_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Science Commons",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_scicom_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#willamette
ebar_willamette_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Willamette",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_willamette_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Willamette",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#matt knight
ebar_mattknight_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Matthew Knight Arena",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.5,
    y = 35,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.1,
    y = 35,
    size = 5,
    label = "anger or disgust",
    color = "#F6AA82"
  )
#^^^ 6 of 8 emotions - missing anger and disgust

ebar_mattknight_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Matthew Knight Arena",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#mckenzie
ebar_mckenzie_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "McKenzie",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_mckenzie_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#oregon
ebar_oregon_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Oregon",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_oregon_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#straub
ebar_straub_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Straub",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.3,
    y = 30,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.9,
    y = 30,
    size = 5,
    label = "anger or fear",
    color = "#F6AA82"
  )
#^^^ 6 of 8 emotions - missing anger and fear

ebar_straub_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#src
ebar_src_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "Student Recreation Complex",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_src_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[12],
  "Student Recreation Complex",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#tykeson
ebar_tykeson_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Tykeson",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_tykeson_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#uhs
ebar_uhs_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "University Health Services",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.3,
    y = 30,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.9,
    y = 30,
    size = 5,
    label = "disgust or surprise",
    color = "#F6AA82"
  )
#^^^ 6 of 8 emotions - missing disgust and surprise

ebar_uhs_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#housing overall
ebar_housing_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "University Housing",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_housing_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[12],
  "University Housing",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#barnhart
ebar_barnhart_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Barnhart",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_barnhart_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#bean
ebar_bean_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Bean",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_bean_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Bean",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#carson
ebar_carson_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Carson",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(7, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.5,
    y = 30,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.1,
    y = 30,
    size = 5,
    label = "disgust",
    color = "#F6AA82"
  )
#^^^ 7 of 8 emotions - missing disgust

ebar_carson_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Carson",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#earl
ebar_earl_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Earl",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.3,
    y = 30,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.9,
    y = 30,
    size = 5,
    label = "anger or sadness",
    color = "#F6AA82"
  )
#^^^ 6 of 8 emotions - missing anger and sadness

ebar_earl_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#gsh
ebar_gsh_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Global Scholars",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_gsh_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Global Scholars",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#hamilton
ebar_hamilton_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Hamilton",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_hamilton_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Hamilton",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#kalapuya
ebar_kalapuya_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Kalapuya Ilihi",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(7, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.5,
    y = 30,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.1,
    y = 30,
    size = 5,
    label = "anger",
    color = "#F6AA82"
  )
#^^^ 7 of 8 emotions - missing anger

ebar_kalapuya_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Kalapuya Ilihi",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#llc
ebar_llc_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Living Learning Center",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_llc_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Living Learning Center",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#unthank
ebar_unthank_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Unthank",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_unthank_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Unthank",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#walton
ebar_walton_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Walton",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_walton_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Walton",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#emu data
b_emo<- emo_emu %>%
  filter(
    sent == "Belong" &
      sample == "us ug"
  )

db_emo<- emo_emu %>%
  filter(
    sent == "Don't Belong" &
      sample == "us ug"
  )

#emu overall
ebar_emu_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  c(np_emu_b_us_ug$full_place),
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_emu_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  c(np_emu_db_us_ug$full_place),
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#atrium east
ebar_atrium_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Atrium East",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.3,
    y = 35,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.9,
    y = 35,
    size = 5,
    label = "anger or disgust",
    color = "#F6AA82"
  )
#^^^ 6 of 8 emotions - missing anger and disgust

ebar_atrium_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#courtyard
ebar_courtyard_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Courtyard",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_courtyard_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#craft center
ebar_craft_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Craft Center",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_craft_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#duck nest
ebar_ducknest_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Duck Nest",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_ducknest_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#falling sky
ebar_fallingsky_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Falling Sky Pizzeria and Public House",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_fallingsky_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#fishbowl
ebar_fishbowl_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Fishbowl",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_fishbowl_db_us_ug<- emo_bar_fun(
  db_emo,
  vnm_emo[11],
  "Fishbowl",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#fresh market
ebar_freshmarket_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Fresh Market",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_freshmarket_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#lgbtqa3
ebar_lgbtqa3_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "LGBTQA3",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(6, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.3,
    y = 30,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.9,
    y = 30,
    size = 5,
    label = "anger or disgust",
    color = "#F6AA82"
  )
#^^^ 6 of 8 emotions - missing anger and disgust

ebar_lgbtqa3_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#mills
ebar_mills_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Mills International Center",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(7, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.5,
    y = 27,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.1,
    y = 27,
    size = 5,
    label = "disgust",
    color = "#F6AA82"
  )
#^^^ 7 of 8 emotions - missing disgust

ebar_mills_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#mcc
ebar_mcc_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Multicultural Center",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(7, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.8,
    y = 30,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.4,
    y = 30,
    size = 5,
    label = "anger",
    color = "#F6AA82"
  )
#^^^ 7 of 8 emotions - missing anger

ebar_mcc_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#olounge
ebar_olounge_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "O Lounge",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_olounge_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#taylor
ebar_taylor_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Taylor Lounge",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_taylor_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#womens
ebar_womens_b_us_ug<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Women's Center",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_womens_db_us_ug<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#international undergrad and grad ####

#bigrams ####

#emu data
b_pos<- pos_emu %>%
  filter(
    sent == "Belong" &
      sample %in% c("intl ug", "intl gr")
  )

#emu overall
bg_emu_b_i<- bigram_fun(
  b_pos,
  vnm_pos[18],
  c(np_emu_b_i$full_place),
  0,
  c(junk_noun, junk_adj, "emu"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_emu_b_i<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  c(np_emu_b_i$full_place),
  c(junk_noun, junk_adj, "emu"),
  0
)

#mills
bg_mills_b_i<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Mills International Center",
  0,
  c(junk_noun, junk_adj, "mills", "center"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_mills_b_i<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Mills International Center",
  c(junk_noun, junk_adj, "mills", "center"),
  0
)

#campus data
b_pos<- pos_cam %>%
  filter(
    sent == "Belong" &
      sample %in% c("intl ug", "intl gr")
  )

#library
bg_library_b_i<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  0,
  c(junk_noun, junk_adj, "library", "knight"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_library_b_i<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  c(junk_noun, junk_adj, "library", "knight"),
  0
)

#lokey overall
bg_lokey_b_i<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "Lokey Complex",
  0,
  c(junk_noun, junk_adj, "lokey"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_lokey_b_i<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "Lokey Complex",
  c(junk_noun, junk_adj, "lokey"),
  0
)

#src
bg_src_b_i<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  0,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_src_b_i<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  0
)

#housing overall
bg_housing_b_i<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "University Housing",
  0,
  c(junk_noun, junk_adj),
  "#CB1B4F",
  "#F6AA82"
)

cmm_housing_b_i<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "University Housing",
  c(junk_noun, junk_adj),
  0
)

#wordclouds ####

#emu data
b_pos<- pos_emu %>%
  filter(
    sent == "Belong" &
      sample %in% c("intl ug", "intl gr")
  )

#emu overall
wc_emu_b_i<- cloud_fun(
  b_pos,
  vnm_pos[18],
  c(np_emu_b_i$full_place),
  0,
  c(junk_noun, junk_adj, "emu"),
  "#F6AA82",
  "#CB1B4F"
)

#mills
wc_mills_b_i<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Mills International Center",
  0,
  c(junk_noun, junk_adj, "mill", "mills", "mills center"),
  "#F6AA82",
  "#CB1B4F"
)

#campus data
b_pos<- pos_cam %>%
  filter(
    sent == "Belong" &
      sample %in% c("intl ug", "intl gr")
  )

#library
wc_library_b_i<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  0,
  c(junk_noun, junk_adj, "library", "knight library"),
  "#F6AA82",
  "#CB1B4F"
)

#lokey overall
wc_lokey_b_i<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "Lokey Complex",
  0,
  c(junk_noun, junk_adj),
  "#F6AA82",
  "#CB1B4F"
)

#src
wc_src_b_i<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  0,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#F6AA82",
  "#CB1B4F"
)

#housing overall
wc_housing_b_i<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "University Housing",
  0,
  c(junk_noun, junk_adj),
  "#F6AA82",
  "#CB1B4F"
)

#emotions ####

#emu data
b_emo<- emo_emu %>%
  filter(
    sent == "Belong" &
      sample %in% c("intl ug", "intl gr")
  )

#emu overall
ebar_emu_b_i<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  c(np_emu_b_i$full_place),
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_emu_db_i<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#mills
ebar_mills_b_i<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Mills International Center",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_mills_db_i<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#campus data
b_emo<- emo_cam %>%
  filter(
    sent == "Belong" &
      sample %in% c("intl ug", "intl gr")
  )

#library
ebar_library_b_i<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Library",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(7, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.5,
    y = 24,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 2.1,
    y = 24,
    size = 5,
    label = "disgust",
    color = "#F6AA82"
  )
#^^^ 7 of 8 emotions - missing disgust

ebar_library_db_i<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#lokey overall
ebar_lokey_b_i<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "Lokey Complex",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(5, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2,
    y = 32,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.6,
    y = 32,
    size = 5,
    label = "anger, disgust, or fear",
    color = "#F6AA82"
  )
#^^^ 5 of 8 emotions - missing anger, disgust, and fear

ebar_lokey_db_i<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#src
ebar_src_b_i<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "Student Recreation Complex",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_src_db_i<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#housing overall
ebar_housing_b_i<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "University Housing",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
)

ebar_housing_db_i<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#us and international grad ####

#bigrams ####

#campus data
b_pos<- pos_cam %>%
  filter(
    sent == "Belong" &
      sample %in% c("us gr", "intl gr")
  )

db_pos<- pos_cam %>%
  filter(
    sent == "Don't Belong" &
      sample %in% c("us gr", "intl gr")
  )

#library
bg_library_b_gr<- bigram_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  0,
  c(junk_noun, junk_adj, "library", "knight"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_library_b_gr<- cooc_minmax_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  c(junk_noun, junk_adj, "library", "knight"),
  0
)

#src
bg_src_b_gr<- bigram_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  0,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#CB1B4F",
  "#F6AA82"
)

cmm_src_b_gr<- cooc_minmax_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  0
)

bg_src_db_gr<- bigram_fun(
  db_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  0,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#357BA2",
  "#78D6AE"
)

cmm_src_db_gr<- cooc_minmax_fun(
  db_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  0
)

#wordclouds ####

#campus data
b_pos<- pos_cam %>%
  filter(
    sent == "Belong" &
      sample %in% c("us gr", "intl gr")
  )

db_pos<- pos_cam %>%
  filter(
    sent == "Don't Belong" &
      sample %in% c("us gr", "intl gr")
  )

#library
wc_library_b_gr<- cloud_fun(
  b_pos,
  vnm_pos[18],
  "Library",
  0,
  c(junk_noun, junk_adj, "library", "knight library"),
  "#F6AA82",
  "#CB1B4F"
)

#src
wc_src_b_gr<- cloud_fun(
  b_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  0,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#F6AA82",
  "#CB1B4F"
)

wc_src_db_gr<- cloud_fun(
  db_pos,
  vnm_pos[19],
  "Student Recreation Complex",
  0,
  c(junk_noun, junk_adj, "src", "the rec", "rec"),
  "#78D6AE",
  "#357BA2"
)

#emotions ####

#campus data
b_emo<- emo_cam %>%
  filter(
    sent == "Belong" &
      sample %in% c("us gr", "intl gr")
  )

db_emo<- emo_cam %>%
  filter(
    sent == "Don't Belong" &
      sample %in% c("us gr", "intl gr")
  )

#library
ebar_library_b_gr<- emo_bar_fun(
  b_emo,
  vnm_emo[11],
  "Library",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(7, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2,
    y = 26,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.6,
    y = 26,
    size = 5,
    label = "anger",
    color = "#F6AA82"
  )
#^^^ 7 of 8 emotions - missing anger

ebar_library_db_gr<- image_read("images/pbb/no_data_mako_db.png") %>%
  image_background("#33343D") %>%
  image_scale("1344x960") %>%
  image_border("#33343D", "0x17")

#src
ebar_src_b_gr<- emo_bar_fun(
  b_emo,
  vnm_emo[12],
  "Student Recreation Complex",
  "Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "rocket",
  0.833,
  0.5,
  "#F6AA82",
  rocket(8, begin = 0.833, end = 0.5)
) +
  ggplot2::annotate(#NRC masks ggplot2::annotate()
    "text",
    x = 2.3,
    y = 27,
    size = 5,
    label = "0 words classified as",
    color = "#F6AA82"
  ) +
  ggplot2::annotate(
    "text",
    x = 1.9,
    y = 27,
    size = 5,
    label = "disgust",
    color = "#F6AA82"
  )
#^^^ 7 of 8 emotions - missing disgust

ebar_src_db_gr<- emo_bar_fun(
  db_emo,
  vnm_emo[12],
  "Student Recreation Complex",
  "Don't Belong",
  c(0, 10, 20, 30, 40, 50),
  c(0, 50),
  "mako",
  0.833,
  0.5,
  "#78D6AE",
  mako(8, begin = 0.833, end = 0.5)
)

#let's make some automated text for ayear in these notes
ayrs_cam<- "Spring 2019, Spring 2020, and Spring 2022" #CHANGE add Spring 2023 when it comes in

ayrs_i<- c("Spring 2020", "Spring 2022") #CHANGE add Spring 2023 when it comes in

ayrs_gr<- "Spring 2022" #CHANGE add Spring 2023 when it comes in

ayrs_emu<- "Spring 2020 and Spring 2022" #CHANGE add Spring 2023 when it comes in


