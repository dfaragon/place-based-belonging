#rendering reactable tables
reactable_fun<- function(dat) {
  options(
    reactable.theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
  )
  rt<- dat %>%
    reactable(
      .,
      groupBy = "agg_place",
      showPageSizeOptions = T,
      paginateSubRows = T,
      defaultSorted = c("agg_place", "full_place"),
      sortable = T,
      showSortable = T,
      striped = T,
      highlight = T,
      bordered = T,
      defaultColDef = colDef(
        vAlign = "center",
        headerVAlign = "bottom"
      ),
      columns = list(
        agg_place = colDef(
          name = "Aggregated Place",
          filterable = T,
          align = "left",
          minWidth = 240
        ),
        full_place = colDef(
          name = "Place",
          align = "left",
          minWidth = 215
        ),
        n_b = colDef(
          aggregate = "sum",
          align = "center",
          format = colFormat(separators = T, digits = 0),
          html = T,
          header = JS(
            'function(column) {
              return `<div style="font-style: italic">n</div>` + "Belong"
            }'
          )
        ),
        n_db = colDef(
          aggregate = "sum",
          align = "center",
          format = colFormat(separators = T, digits = 0),
          html = T,
          header = JS(
            'function(column) {
              return `<div style="font-style: italic">n</div>` + "Don\'t" +
              "<br>Belong"
            }'
          )
        ),
        perc_click_b = colDef(
          aggregate = "sum",
          align = "center",
          format = colFormat(percent = T, digits = 1),
          html = T,
          header = JS(
            'function(column) {
              return "Click" + "<br>Belong"
            }'
          )
        ),
        perc_click_db = colDef(
          aggregate = "sum",
          align = "center",
          format = colFormat(percent = T, digits = 1),
          html = T,
          header = JS(
            'function(column) {
              return "Click" + "<br>Don\'t" + "<br>Belong"
            }'
          )
        ),
        perc_stud_b = colDef(
          aggregate = "sum",
          align = "center",
          format = colFormat(percent = T, digits = 1),
          html = T,
          header = JS(
            'function(column) {
              return "Student" + "<br>Belong"
            }'
          )
        ),
        perc_stud_db = colDef(
          aggregate = "sum",
          align = "center",
          format = colFormat(percent = T, digits = 1),
          html = T,
          header = JS(
            'function(column) {
              return "Student" + "<br>Don\'t" + "<br>Belong"
            }'
          )
        ),
        incl = colDef(
          aggregate = "mean",
          name = "Inclusive",
          align = "center",
          format = colFormat(percent = T, digits = 1)
        )
      )
    )
  return(rt)
}
