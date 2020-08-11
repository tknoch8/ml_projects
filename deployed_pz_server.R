readRenviron(".Renviron")

require(shiny)
# require(shinyBS)
# require(shinyjs)
require(leaflet)
require(lubridate)
require(plotly)
require(tidyverse)
require(reactable)

theme_set(hrbrthemes::theme_ipsum())

set.seed(4321)

dat <- rio::import("app_data.RData")

my_colors <- c(
  "#993333", # red
  "#943894", # purple
  # "#66ff99", # light green
  "#333399", # dark blue
  "#7a5252", # black
  "#bf7540", # burnt orange
  "#d9d9d9", # light gray
  "#1f9346", # green
  "#409fbf", # teal
  "#ebc6eb", # pink
  "#cccc66", # yellow
  "#263913"  # dark green
)

legend_pal <- leaflet::colorFactor(
  palette = my_colors,
  domain = dat$actor1,
  ordered = TRUE
)

color_lookup <- dat %>% 
  # mutate(actor1 = factor(actor1, levels = c()))
  mutate(actor_color = legend_pal(actor1)) %>% 
  distinct(actor1, actor_color)

dat <- dat %>% 
  # mutate(actor1 = fct_relevel(
  #   "Katiba Macina",
  #   "Al Mourabitoune Battalion",
  #   "JNIM: Group for Support of Islam and Muslims",
  #   "Islamic State (Greater Sahara)",
  #   "Ansaroul Islam",
  #   "AQIM: Al Qaeda in the Islamic Maghreb"
  # )) %>% 
  left_join(color_lookup)

my_font <- list(
  size = 8
)

my_font_b <- list(
  size = 12
)

m <- list(
  l = 1,
  r = 1,
  b = 1,
  t = 30,
  pad = 1
)

require(sf)

# read all historical shapefiles

macina_19 <- sf::st_read("shapes/Historic Maps/Macina 19th Century.shp") %>% 
  st_transform(4326)
# glimpse(macina_19)

mk_sudan_1800 <- sf::st_read("shapes/Historic Maps/Major Kingdoms of the Sudan 1800.shp") %>% 
  st_transform(4326)
# glimpse(mk_sudan_1800)

people_emp_wa_xx <- sf::st_read("shapes/Historic Maps/Peoples and Empires of West Africa Map XX The Islamic Revival.shp") %>% 
  st_transform(4326)
# glimpse(people_emp_wa_xx)

sphere_shaikh_masina <- sf::st_read("shapes/Historic Maps/The Sphere of Shaikh Hamad of Masina Western Sudan.shp") %>% 
  st_transform(4326)
# glimpse(sphere_shaikh_masina)

states_guinea_19 <- sf::st_read("shapes/Historic Maps/VII States of the Sudan and Guinea 19th Century.shp") %>% 
  st_transform(4326)
# glimpse(states_guinea_19)

how_many_actors <- as.numeric(Sys.getenv("num_top_actors"))

test <- dat %>%
  # mutate(actor1 = fct_lump(actor1, how_many_actors)) %>%
  pull(actor1) %>%
  as.character() %>%
  unique()

shinyServer(function(input, output, session) {
  
  # don't know if this is necessary
  fire_icon <- leaflet::makeIcon(
    iconUrl = paste0(here::here("www", "fire_icon.png")),
    iconWidth = 16, iconHeight = 16
  )
  
  # for defaults such as selected actors
  defaults <- reactiveValues()
  
  # for dataframes/other things that need to be used in multiple
  # reactive contexts
  data_pile <- reactiveValues()
  
  ####---- main legend ----####
  # output$leg_re <- renderReactable({
  #   
  #   tab_dat <- dat %>% 
  #     distinct(actor1, actor_color) %>% 
  #     rename(Color = actor_color,
  #            `Actor 1` = actor1)
  #   
  #   # need to add click actions to this
  #   reactable(
  #     tab_dat,
  #     style = list(
  #       fontSize = "11px"
  #     ),
  #     columns = list(
  #       Color = colDef(
  #         style = function(value) {
  #           color <- value
  #           list(background = color,
  #                color = rgb(0, 0, 0, 0))
  #         },
  #         minWidth = 40,
  #         maxWidth = 50
  #       )
  #       # `Actor 1` = colDef(
  #       #   maxWidth = 200
  #       # )
  #     ),
  #     borderless = TRUE,
  #     compact = TRUE,
  #     showSortIcon = FALSE,
  #     showPageInfo = FALSE,
  #     showPageSizeOptions = FALSE,
  #     showPagination = FALSE,
  #     highlight = TRUE,
  #     onClick = JS("
  #       function(rowInfo, colInfo) {
  #         // Send the click event to Shiny, which will be available at input$clicked
  #         // (Note that the row index starts at 0 in JavaScript, so we add 1)
  #         if (window.Shiny) {
  #           Shiny.onInputChange('clicked', { column: colInfo.id, index: rowInfo.index + 1 })
  #         }
  #       }
  #     "),
  #     class = "my-tbl",
  #     rowClass = "my-row",
  #     defaultColDef = colDef(headerClass = "my-header")
  #   )
  #   
  # })
  
  output$date_selector <- renderUI({
    
    dateRangeInput(
      "my_date_input",
      "",
      start = min(dat$event_date),
      end = max(dat$event_date),
      min = min(dat$event_date),
      max = max(dat$event_date)
    )
    
  })
  
  leg_vals <- reactiveValues()
  
  ####---- capture actor1 from legend click ----####
  observeEvent(input$clicked, {
    output$clicked_info <- renderPrint({
      input$clicked
    })
    
    my_index <- input$clicked$index
    
    leg_vals$actor <- as.character(color_lookup$actor1[my_index])
    
    message("... captured legend click: ", leg_vals$actor)
    
  })
  
  ####---- bl_bar ----####
  output$bl_bar <- renderPlotly({
    
    req(input$my_date_input)
    
    
    # defaults$default_actors <- c(
    #   "Katiba Macina",
    #   "Al Mourabitoune Battalion",
    #   # "Movement of the Liberation of Maasina",
    #   "JNIM",
    #   # "JNIM: Group for Support of Islam and Muslims and/or Islamic State (Greater Sahara)"
    #   "ISGS",
    #   # "Islamic State (Greater Sahara) and/or Ansaroul Islam"
    #   "Ansaroul Islam",
    #   # "Islamic State (Greater Sahara) and/or Ansaroul Islam"
    #   "AQIM"
    # )
    
    # change to informed default. Fill in above and comment this
    # default_actors <- dat %>% 
    #   # mutate(actor1 = fct_lump(actor1, 6)) %>% 
    #   filter(actor1 != "Other") %>% 
    #   pull(actor1) %>% 
    #   as.character() %>% 
    #   unique()
    
    pd <- dat %>% 
      # filter(actor1 %in% input$my_actors) %>% 
      # actor1 selection input
      # filter(actor1 %in% defaults$default_actors) %>%
      # filter by date range
      filter(date %within% lubridate::interval(
        as.Date(input$my_date_input[1]), as.Date(input$my_date_input[2])
      )) %>%
      count(actor1) %>% 
      left_join(color_lookup) %>% 
      # plotly x-axis includes all factor levels, regardless of presence in data
      mutate(actor1 = as.character(actor1)) %>% 
      mutate(actor1 = fct_reorder(actor1, desc(n)))
    
    count_other <- dat %>% 
      filter(actor1 == "Other") %>% 
      nrow()
    
    data_pile$pd <- pd
    
    plot_ly(
      data = pd %>% 
        filter(actor1 != "Other") %>% 
        mutate(actor1 = as.character(actor1)) %>% 
        mutate(actor1 = fct_reorder(actor1, desc(n))),
      type = "bar",
      x = ~actor1,
      y = ~n,
      # height = 250,
      color = ~I(actor_color),
      source = "main_bar"
    ) %>% 
      layout(
        yaxis = list(
          title = "Count",
          titlefont = list(color = "#ffffff")
        ),
        xaxis = list(
          title = "",
          font = list(
            size = 12
          ),
          titlefont = list(color = "#ffffff")
        ),
        margin = list(
          b = 250,
          t = 55
        ),
        annotations = list(
          list(
            x = 0,
            y = 1.05,
            text = paste0(formatC(count_other, big.mark = ","), " 'Other' events not shown"),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            font = list(size = 10)
          ),
          list(
            x = 0,
            y = 1.16,
            text = "Events by actor",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            font = list(size = 16)
          )
        ),
        plot_bgcolor = "#262626",
        paper_bgcolor = "#262626",
        font = list(color = "#cccccc")
        # xaxis = list(tickfont = list(color = "#ffffff")),
        # yaxis = list(tickfont = list(color = "#ffffff")),
        # legend = list(font = list(color = "#ffffff"))
      ) %>% 
      hide_legend()
    
  })
  
  ####---- bl_bar_table ----####
  output$pd_dat_table <- renderReactable({
    
    req(input$my_date_input)
    req(data_pile$pd)
    
    data_pile$pd %>%
      select(-actor_color) %>% 
      reactable(
        defaultSorted = list(n = "desc"),
        class = "my-tbl",
        rowClass = "my-row",
        defaultColDef = colDef(headerClass = "my-header")
      )
    
  })
  
  ## make three leaflets, one with historic shapefiles
  
  ####---- map_1 ----####
  output$map_1 <- renderLeaflet({
    
    message("..... making base map")
    
    leaflet() %>% 
      addProviderTiles(
        provider = leaflet::providers$CartoDB.DarkMatter,
        group = "Dark"
      ) %>% 
      addProviderTiles(
        provider = leaflet::providers$OpenStreetMap,
        group = "Light"
      ) %>% 
      # addTiles() %>% 
      addPolygons(data = macina_19,
                  group = "Macina 19th Century",
                  stroke = TRUE,
                  color = "blue",
                  fillColor = "black",
                  popup = "Macina 19th Century") %>% 
      # popupOptions = popupOptions(keepInView = TRUE)) %>% 
      # labelOptions = labelOptions(noHide = T, direction = "bottom",
      #                             style = list(
      #                               "color" = "red",
      #                               "font-family" = "serif",
      #                               "font-style" = "italic",
      #                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      #                               "font-size" = "12px",
      #                               "border-color" = "rgba(0,0,0,0.5)"
      #                             ))) %>% 
      addPolygons(data = mk_sudan_1800,
                  group = "Major Kingdoms of the Sudan 1800",
                  stroke = TRUE,
                  color = "green",
                  fillColor = "black",
                  popup = "Major Kingdoms of the Sudan 1800") %>% 
      addPolygons(data = people_emp_wa_xx,
                  group = "Peoples and Empires of West Africa Map XX The Islamic Revival",
                  stroke = TRUE,
                  color = "red",
                  fillColor = "black",
                  popup = "Peoples qnd Empires of West Africa Map XX The Islamic Revival") %>% 
      addPolygons(data = sphere_shaikh_masina,
                  group = "The Sphere of Shaikh Hamad of Masina Western Sudan",
                  stroke = TRUE,
                  color = "yellow",
                  fillColor = "black",
                  popup = "The Sphere of Shaikh Hamad of Masina Western Sudan") %>% 
      addPolygons(data = states_guinea_19,
                  group = "VII States of the Sudan and Guinea 19th Century",
                  stroke = TRUE,
                  color = "purple",
                  fillColor = "black",
                  popup = "VII States of the Sudan and Guinea 19th Century") %>% 
      addLayersControl(
        overlayGroups = c(
          "Macina 19th Century",
          "Major Kingdoms of the Sudan 1800",
          "Peoples and Empires of West Africa Map XX The Islamic Revival",
          "The Sphere of Shaikh Hamad of Masina Western Sudan",
          "VII States of the Sudan and Guinea 19th Century"
        ),
        baseGroups = c(
          "Dark",
          "Light"
        )
      ) %>% 
      hideGroup(
        c(
          "Macina 19th Century",
          "Major Kingdoms of the Sudan 1800",
          "Peoples and Empires of West Africa Map XX The Islamic Revival",
          "The Sphere of Shaikh Hamad of Masina Western Sudan",
          "VII States of the Sudan and Guinea 19th Century"
        )
      ) %>% 
      setView(lng = -1.531230,
              lat = 12,
              zoom = 4)
    
  })
  
  st <- reactiveValues()
  
  # observeEvent(event_data("plotly_click", source = "main_bar", priority = "event"), {
  #   input$clicked <- event_data("plotly_click", source = "main_bar", priority = "event")[3]
  # })
  
  ####---- map_1 marker proxy ----####
  observeEvent(c(event_data("plotly_click", source = "main_bar", priority = "event"), input$clicked), {
    
    message("clicked bar: ", event_data("plotly_click", source = "main_bar", priority = "event")[3])
    
    # actor_clicked <- as.character(event_data("plotly_click", source = "main_bar")[3])
    
    # clicked_list <- c()
    
    ############## try[ ######
    
    actors <- data_pile$pd %>% 
      # filter(actor1 != "Other") %>% 
      mutate(actor1 = as.character(actor1)) %>% 
      pull(actor1) %>% 
      unique()
    
    actors_status <- rep(0, length(actors))
    
    names(actors_status) <- actors
    
    ####---- START HERE, need to be able to switch between bar and legend clicks ----####
    # need to reset bar plot click when legend is clicked and vice versa
    if (is.null(input$clicked)) {
      message("..... input$clicked is null, using bar click")
      actor_clicked <- as.character(event_data("plotly_click", source = "main_bar", priority = "event")[3])
      
    } else {
      req(leg_vals$actor)
      actor_clicked <- leg_vals$actor
      
    }
    
    message("......... actor_clicked: ", actor_clicked)
    
    # js$resetClick()
    
    # event_data("plotly_click", source = "main_bar")[3] <- NULL
    
    # if (actors_status[[actor_clicked]] == 0) {
    #   actors_status[[actor_clicked]] <- 1
    # } else if (actors_status[[actor_clicked]] == 1) {
    #   actors_status[[actor_clicked]] <- 0
    # }
    
    ####### try] #######
    
    # click_df <- crossing(
    #   name = data_pile$pd %>% pull(actor1) %>% as.character() %>% unique(),
    #   clicked = 0
    # )
    
    # message("... click_df: ", str(click_df))
    
    if (actors_status[[actor_clicked]] == 0) {
      actors_status[[actor_clicked]] <- 1
    } else if (actors_status[[actor_clicked]] == 1) {
      actors_status[[actor_clicked]] <- 0
    }
    
    observeEvent(actor_clicked, {
      
      # actor_clicked
      
      # if (actors_status[[actor_clicked]] == 0) {
      #   actors_status[[actor_clicked]] <- 1
      # } else if (actors_status[[actor_clicked]] == 1) {
      #   actors_status[[actor_clicked]] <- 0
      # }
      
      message(".... actor_status[[actor_clicked]]: ", actors_status[[actor_clicked]])
      
      # click_df <- click_df %>% 
      #   filter(name == actor_clicked) %>% 
      #   mutate(clicked = if_else(clicked == 0, 1, 0))
      
      # if (click_df %>% filter(name == actor_clicked) %>% pull(clicked) == 0) {
      #   click_df$clicked[click_df$name == actor_clicked] <- 1
      # } else if (click_df %>% filter(name == actor_clicked) %>% pull(clicked) == 1) {
      #   click_df$clicked[click_df$name == actor_clicked] <- 0
      # }
      
      # message("clicks: ", str(click_df))
      # message("... clicked_list: ", clicked_list)
      
    })
    
    # clicked_list <- append(clicked_list, actor_clicked)
    
    # append(multiples, actor_clicked)
    
    # message(".... actor_clicked: ", actor_clicked)
    # message(".... multiples selected: ", paste(multiples, collapse = ", "))
    
    # req(input$my_actors)
    req(input$my_date_input)
    # req(defaults$default_actors)
    
    # pal_vals$legend_pal <- legend_pal
    
    message("... date constraint: ", input$my_date_input[1], ", ", input$my_date_input[2])
    
    # ones <- click_df %>% 
    #   filter(clicked == 1)
    # 
    # zeros <- click_df %>% 
    #   filter(clicked == 0)
    
    proxy_dat <- dat %>% 
      filter(actor1 == actor_clicked) %>% 
      filter(date %within% lubridate::interval(
        as.Date(input$my_date_input[1]), as.Date(input$my_date_input[2])
      )) %>%
      # filter(actor1 %in% defaults$default_actors) %>% 
      mutate(actor1 = as.character(actor1))
    
    data_pile$for_map_table <- proxy_dat
    
    # glimpse(proxy_dat)
    message("......nrow(proxy_dat): ", nrow(proxy_dat))
    
    message(".... ->>> actor_status[[actor_clicked]]: ", actors_status[[actor_clicked]])
    
    if (actors_status[[actor_clicked]] == 1) {
      message("............ adding markers")
      leafletProxy("map_1", session) %>% 
        # clearGroup("acled_events_1") %>% 
        addCircleMarkers(
          data = proxy_dat,
          # lng = ~longitude,
          # lat = ~latitude,
          group = "acled_events_1",
          # group = as.character(actor_clicked),
          # layerId = as.character(actor_clicked),
          radius = 5,
          fillOpacity = .7,
          stroke = TRUE,
          color = ~case_when(
            actor1 == "JNIM" ~ "#000099",
            actor1 == "ISGS" ~ "#cc3300",
            actor1 == "Boko Haram (JAS)" ~ "#cc3300",
            actor1 == "Al Mourabitoune" ~ "#cccccc"
          ),
          weight = 1,
          fillColor = ~actor_color,
          popup = ~actor1
        )
    } else if (actors_status[[actor_clicked]] == 0) {
      message("........... trying to clear markers")
      leafletProxy("map_1", session) %>% 
        clearGroup(group = as.character(actor_clicked))
    }
    
    
      # addLegend(
      #   position = "bottomleft",
      #   pal = legend_pal,
      #   # labels = actor_pal,
      #   values = dat %>% 
      #     filter(date %within% lubridate::interval(
      #       as.Date(input$my_date_input[1]), as.Date(input$my_date_input[2])
      #     )) %>%
      #     filter(actor1 %in% input$my_actors) %>% 
      #     pull(actor1),
      #   title = "Actor 1",
      #   layerId = "legend"
      # )
    
  })
  
  observeEvent(input$clear_actors, {

    leafletProxy("map_1", session) %>%
      clearGroup("acled_events_1")

  })
  
  observe({
    
    # data_pile$for_map_table
    
    ####---- map_1 table ----####
    output$map_1_table <- renderReactable({
      
      col_width <- 400
      
      # intercept with full data reactable if no bar has been clicked
      if (is.null(event_data("plotly_click", source = "main_bar"))) {
        message("... no bar clicked, reverting to full data for map_1_table")
        return(
          dat %>% 
            select(
              event_date,
              event_type,
              fatalities,
              everything()
            ) %>% 
            reactable::reactable(
              searchable = TRUE,
              columns = list(
                notes = colDef(minWidth = 500),
                assoc_actor_1 = colDef(minWidth = col_width),
                assoc_actor_2 = colDef(minWidth = col_width),
                fatalities = colDef(style = JS("
                  function(rowInfo) {
                    var value = rowInfo.row.fatalities
                    if (value > 0) {
                      var color = '#ff0000'
                    } else if (value == 0) {
                      var color = '#4d9900'
                    }
                    return { color: color, fontWeight: 'bold' }
                  }                              
                "))
              ),
              defaultSorted = list(date = "desc"),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(5, 10, 40, 100),
              defaultPageSize = 5,
              highlight = TRUE,
              class = "my-tbl",
              rowClass = "my-row",
              defaultColDef = colDef(headerClass = "my-header")
            )
        )
      }
      
      req(data_pile$for_map_table)
      
      message("...... making reactable for map_1 data")
      
      data_pile$for_map_table %>% 
        # select(-notes) %>% 
        reactable::reactable(
          columns = list(
            notes = colDef(minWidth = 500),
            assoc_actor_1 = colDef(minWidth = col_width),
            assoc_actor_2 = colDef(minWidth = col_width)
          ),
          defaultSorted = list(date = "desc"),
          showPageSizeOptions = TRUE, 
          pageSizeOptions = c(10, 40, 100),
          defaultPageSize = 10
        )
      
    })
    
  })
  
  ####---- selectizeInput for timeline actors ----####
  observe({
    # input$tl_ac
    data_pile$pd
    output$tl_ac <- renderUI({
      req(data_pile$pd)
      message("...making tl_ac ui element")
      my_choices <- data_pile$pd %>% 
        pull(actor1) %>% 
        unique() %>% 
        sort()
      message("... actor choices for timeline: ", my_choices)
      selectizeInput(
        "tl_ac",
        "Actors",
        choices = my_choices,
        selected = "JNIM",
        multiple = TRUE
      )
    })
  })
  
  ####---- bl_timeline ----####
  observe({
    
    # defaults$default_actors
    # message(".......selected actors: ", input$my_actors)
    req(input$my_date_input)
    req(input$tl_ac)
    
    # dat
    
    message("... making bl_timeline...")
    
    output$bl_timeline <- renderPlotly({
      
      # req(defaults$default_actors)
      
      p <- dat %>% 
        filter(actor1 %in% input$tl_ac) %>%
        filter(date %within% lubridate::interval(
          as.Date(input$my_date_input[1]), as.Date(input$my_date_input[2])
        )) %>% 
        filter(event_date >= Sys.Date() - lubridate::days(90)) %>% 
        mutate(event_date = as.Date(event_date)) %>% 
        ggplot(aes(event_date, actor1, color = sub_event_type)) +
        geom_point() +
        theme(legend.position = "top",
              text = element_text(color = "#cccccc")) +
        # axis.text.y = element_text(angle = 45))
        labs(color = "Sub event type",
             y = "",
             x = "")
      
      ####---- NEED to get legend title text same color ----####
      ggplotly(p,
               height = 485) %>% 
        layout(
          #showlegend = FALSE,
          title = "Last 90 Days",
          plot_bgcolor = "#262626",
          paper_bgcolor = "#262626",
          font = list(color = "#cccccc"),
          xaxis = list(tickfont = list(color = "#cccccc")),
          yaxis = list(tickfont = list(color = "#cccccc")),
          legend = list(text = list(color = "#cccccc"), title = list(color = "#cccccc"), font = list(color = "#cccccc"))
        )
      
    })
    
  })
  
})