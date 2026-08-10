library(reactable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(plotly)

classification_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # Top Control Bar (Round Range Slider & Single Round Inspector)
    fluidRow(
      column(width = 12,
             box(
               title = "Matchday Round Selection & Window Filter",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               div(style = "background: #f8fafc; padding: 15px; border-radius: 8px; border: 1px solid #e2e8f0;",
                   fluidRow(
                     column(width = 6,
                            sliderInput(
                              inputId = ns("round_range_slider"),
                              label = "Filter Points Between Rounds (Window):",
                              min = 1,
                              max = 38,
                              value = c(1, 38),
                              step = 1,
                              width = "100%"
                            )
                     ),
                     column(width = 6,
                            selectInput(
                              inputId = ns("single_round_select"),
                              label = "Inspect Specific Matchday Round:",
                              choices = c("All Rounds" = "all"),
                              selected = "all",
                              width = "100%"
                            )
                     )
                   )
               )
             )
      )
    ),

    # Rank Position Evolution Chart across Rounds (Inverted Y-axis)
    fluidRow(
      column(width = 12,
             box(
               title = "Rank Position Evolution Across Rounds",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               plotly::plotlyOutput(ns("rank_evolution_plot"), height = "320px")
             )
      )
    ),

    # Standings Classification Table for Selected Round Range
    fluidRow(
      column(width = 12,
             box(
               title = "Standings Classification & Points Breakdown",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               reactable::reactableOutput(ns("classification_table"))
             )
      )
    ),

    # Dream Team & Round Rewards Box
    fluidRow(
      column(width = 12,
             box(
               title = "Matchday Best Players & Dream Team Rewards",
               width = 12,
               status = "info",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               uiOutput(ns("dreamteam_box_ui"))
             )
      )
    )
  )
}

classification_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Standings History Reactive
    standings_history_RV <- reactive({
      req(is_module_active() == TRUE)
      req(championship_id())
      champ_id <- championship_id()

      df <- tryCatch({
        get_league_standings_history(champ_id)
      }, error = function(e) {
        print(paste0("[Classification] Error fetching standings history: ", e$message))
        NULL
      })
      return(df)
    })

    # Render Rank Position Evolution Chart
    output$rank_evolution_plot <- plotly::renderPlotly({
      req(is_module_active() == TRUE)
      history_df <- standings_history_RV()

      has_data <- !is.null(history_df) && nrow(history_df) > 0 && "position" %in% colnames(history_df) && any(!is.na(history_df$position) & history_df$position > 0)

      if (!has_data) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor = "rgba(0,0,0,0)",
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  x = 0.5,
                  y = 0.5,
                  xref = "paper",
                  yref = "paper",
                  text = "<b>No matchday round data available yet.</b><br><span style='font-size: 12px; color: #64748b;'>Rank position evolution across rounds will appear once matchday scores are recorded.</span>",
                  showarrow = FALSE,
                  font = list(size = 14, color = "#334155"),
                  bgcolor = "#f8fafc",
                  bordercolor = "#cbd5e1",
                  borderwidth = 1,
                  borderpad = 16
                )
              )
            )
        )
      }

      # Format dates
      history_df$date <- as.POSIXct(history_df$recorded_at, format = "%Y-%m-%dT%H:%M:%S")
      if (any(is.na(history_df$date))) {
        history_df$date <- as.POSIXct(history_df$recorded_at)
      }
      history_df <- history_df %>% dplyr::arrange(date)

      # Inverted Y-axis rank position chart (1st place at top)
      plotly::plot_ly(
        data = history_df,
        x = ~date,
        y = ~position,
        color = ~teamname,
        type = "scatter",
        mode = "lines+markers",
        line = list(width = 2, shape = "spline"),
        marker = list(size = 6),
        hoverinfo = "text",
        text = ~paste0("<b>", teamname, "</b><br>Rank Position: #", position, "<br>Points: ", points, "<br>Date: ", format(date, "%d-%m-%Y"))
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "Matchday Timeline", gridcolor = "#f1f5f9", zeroline = FALSE, tickformat = "%d-%m"),
          yaxis = list(title = "Standings Position (Rank 1 at top)", gridcolor = "#f1f5f9", autorange = "reversed", dtick = 1),
          legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center"),
          margin = list(l = 60, r = 20, t = 10, b = 40)
        )
    })

    # Render Classification Table
    output$classification_table <- reactable::renderReactable({
      req(is_module_active() == TRUE)
      teams_df <- user_teams_RV()
      req(teams_df)

      if (is.null(teams_df) || nrow(teams_df) == 0) return(NULL)

      # Sort teams by rank position or total points
      teams_df <- teams_df %>%
        dplyr::mutate(
          rank_num = if ("position" %in% colnames(teams_df)) suppressWarnings(as.numeric(position)) else seq_len(nrow(teams_df)),
          pts_num = if ("points" %in% colnames(teams_df)) suppressWarnings(as.numeric(points)) else 0,
          val_num = if ("teamValue" %in% colnames(teams_df)) suppressWarnings(as.numeric(teamValue)) else if ("team_value" %in% colnames(teams_df)) suppressWarnings(as.numeric(team_value)) else 0,
          team_name_clean = if ("teamname" %in% colnames(teams_df)) teamname else name,
          is_active_clean = if ("is_active" %in% colnames(teams_df)) isTRUE(as.logical(is_active)) else TRUE
        ) %>%
        dplyr::arrange(rank_num)

      active_members <- nrow(teams_df)
      ranking_prizes_df <- calculate_futmondo_ranking_prizes(money = 30000000, members = active_members)

      teams_df$ranking_prize <- if (nrow(ranking_prizes_df) >= nrow(teams_df)) ranking_prizes_df$prize[seq_len(nrow(teams_df))] else 0
      teams_df$point_earnings <- teams_df$pts_num * 70000
      teams_df$total_earnings <- teams_df$point_earnings + teams_df$ranking_prize

      df_display <- teams_df %>%
        dplyr::transmute(
          Rank = rank_num,
          `User Team` = team_name_clean,
          `Active` = is_active_clean,
          `Total Points` = pts_num,
          `Point Earnings` = point_earnings,
          `Ranking Prize` = ranking_prize,
          `Total Estimated Money` = total_earnings,
          `Squad Value` = val_num
        )

      reactable::reactable(
        df_display,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        bordered = FALSE,
        defaultPageSize = 10,
        columns = list(
          Rank = colDef(name = "Rank #", align = "center", width = 80, style = list(fontWeight = "700", color = "#0f172a")),
          `User Team` = colDef(
            align = "left",
            cell = function(val, index) {
              is_act <- df_display$Active[index]
              if (!is_act) {
                shiny::tags$span(
                  style = "font-weight: 600; color: #64748b;",
                  val,
                  shiny::tags$span(style = "margin-left: 6px; font-size: 10px; background-color: #f1f5f9; color: #64748b; padding: 2px 6px; border-radius: 4px; font-weight: 600;", "Inactive")
                )
              } else {
                shiny::tags$span(style = "font-weight: 600; color: #0f172a;", val)
              }
            }
          ),
          Active = colDef(show = FALSE),
          `Total Points` = colDef(align = "center", style = list(fontWeight = "700", color = "#3b82f6")),
          `Point Earnings` = colDef(name = "Point Earnings (€)", align = "right", cell = function(val) format_table_currency(val)),
          `Ranking Prize` = colDef(name = "Ranking Prize (€)", align = "right", cell = function(val) format_table_currency(val)),
          `Total Estimated Money` = colDef(
            name = "Total Money Earned (€)",
            align = "right",
            cell = function(val) {
              formatted <- format_table_currency(val)
              shiny::tags$span(style = "color: #10b981; font-weight: 700;", formatted)
            }
          ),
          `Squad Value` = colDef(name = "Squad Value (€)", align = "right", cell = function(val) format_table_currency(val))
        )
      )
    })

    # Render Dream Team Box UI
    output$dreamteam_box_ui <- renderUI({
      req(is_module_active() == TRUE)
      teams_df <- user_teams_RV()
      active_members <- if (!is.null(teams_df) && nrow(teams_df) > 0) nrow(teams_df) else 1
      prizes_sample <- calculate_futmondo_ranking_prizes(money = 30000000, members = active_members)
      first_prize <- if (nrow(prizes_sample) >= 1) format_table_currency(prizes_sample$prize[1]) else "10.000.000 €"
      last_prize <- if (nrow(prizes_sample) >= 1) format_table_currency(prizes_sample$prize[nrow(prizes_sample)]) else "2.000.000 €"

      div(
        style = "padding: 18px; background: #f8fafc; border-radius: 8px; border: 1px solid #e2e8f0;",
        div(
          style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
          shiny::tags$i(class = "fa-solid fa-trophy", style = "font-size: 28px; color: #f59e0b;"),
          div(
            h4(style = "font-weight: 700; color: #0f172a; margin: 0; font-size: 16px;", "Official Futmondo Round Rewards & Ranking Distribution"),
            p(style = "color: #64748b; margin: 2px 0 0 0; font-size: 12px;", paste0("Active League Size: ", active_members, " Teams | Mode: Flop Ranking"))
          )
        ),
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 12px; margin-top: 15px;",
          div(
            style = "background: #ffffff; padding: 12px; border-radius: 8px; border: 1px solid #cbd5e1; text-align: center;",
            span(style = "color: #64748b; font-size: 11px; font-weight: 600; text-transform: uppercase;", "Points Earnings"),
            h4(style = "font-weight: 700; color: #3b82f6; margin: 4px 0 0 0;", "70.000 € / point")
          ),
          div(
            style = "background: #ffffff; padding: 12px; border-radius: 8px; border: 1px solid #cbd5e1; text-align: center;",
            span(style = "color: #64748b; font-size: 11px; font-weight: 600; text-transform: uppercase;", "Ranking Pool Distribution"),
            h4(style = "font-weight: 700; color: #10b981; margin: 4px 0 0 0;", paste0("30.000.000 € (1º: ", first_prize, " | ", active_members, "º: ", last_prize, ")"))
          ),
          div(
            style = "background: #ffffff; padding: 12px; border-radius: 8px; border: 1px solid #cbd5e1; text-align: center;",
            span(style = "color: #64748b; font-size: 11px; font-weight: 600; text-transform: uppercase;", "Matchday MVP Bonus"),
            h4(style = "font-weight: 700; color: #f59e0b; margin: 4px 0 0 0;", "1.000.000 €")
          ),
          div(
            style = "background: #ffffff; padding: 12px; border-radius: 8px; border: 1px solid #cbd5e1; text-align: center;",
            span(style = "color: #64748b; font-size: 11px; font-weight: 600; text-transform: uppercase;", "Dream Team Player"),
            h4(style = "font-weight: 700; color: #8b5cf6; margin: 4px 0 0 0;", "500.000 €")
          )
        )
      )
    })
  })
}