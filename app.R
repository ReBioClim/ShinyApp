library(shiny)
library(sf)
library(leaflet)
library(DT)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)

score_levels <- c("very low","low","medium","high","very high"); score_lookup <- setNames(1:5, score_levels)
microclimate_raw <- c("Bila Nisa"=3.6,"Geberbach"=3.8,"Piasnica"=3.5,"Teplica"=3.8)
streams_raw <- c("BilaNisa","Geberbach","Piasnica","Teplica")
streams_lab <- c(BilaNisa="Bila Nisa",Geberbach="Geberbach",Piasnica="Piasnica",Teplica="Teplica")
quadrant_colors <- c("Actual conflict"="#cc415e","Actual synergy"="#2f41dd","Potential conflict"="#e9ce2c","Potential synergy"="#66d7d1")
class_colors <- setNames(colorRampPalette(c("#66d7d1","#2f41dd"))(5),score_levels)
map_category_levels <- list(Continuouness.for.fish...mzb=c("no","limited","yes"),Special.bed.structures.Bonus=c("none","few / slight","many"),Passability=c("not possible","hardly","medium","well","very well"),Accessibility=c("not possible","hardly","medium","well","very well"))
stream_shapes <- c("Bila Nisa"=21,Geberbach=22,Piasnica=23,Teplica=24,Case=21)
quadrant_labels <- c("Actual synergy"="Recognized Asset","Potential synergy"="High Restoration Opportunity","Potential conflict"="Perception Gap","Actual conflict"="Information/ Restoration Gap")
classification_table <- data.frame(
  `Spatial condition`=c("> 3","≤ 3","> 3","≤ 3"),
  `Public support`=c("≥ 3","≥ 3","< 3","< 3"),
  `Conflict or synergy for restoration`=c("Actual synergy","Potential synergy","Potential conflict","Actual conflict"),
  `Quadrant Location`=c("Top right","Top left","Bottom right","Bottom left"),
  `Quadrant Name`=c("Recognized asset","High restoration opportunity","Perception gap","Information and restoration gap"),
  `Explanation / Interpretation`=c(
    "Good spatial condition or high spatial availability, which aligns with public support and preferences -> no awareness-building needed, maintain and protect spatial condition.",
    "Current spatial condition is low and public support is high -> strong potential to implement restoration measure that aligns with public perception and acceptance.",
    "Current spatial condition is good, but it is disliked or undervalued by the public -> need to inform about the benefits to prevent conflicts.",
    "Current spatial condition is bad (restoration needed) but public support is low -> restoration would not be accepted; awareness-building is needed first."),
  check.names=FALSE)

variable_definitions <- read.csv("variable_definitions.csv", check.names=FALSE)
example_cases <- read.csv("calculator_examples.csv", check.names=FALSE)
variable_labels <- setNames(variable_definitions$label, variable_definitions$variable)
example_text <- paste(c(paste(names(example_cases), collapse=","), apply(example_cases, 1, paste, collapse=",")), collapse="\n")
paste_example_text <- sub("^case,measure,sc,ps", "case,measure,spatial_condition,public_support", example_text)

clean <- function(x) {x <- tolower(trimws(as.character(x))); x[x %in% c("","na","n.a.","nan")] <- NA; x}
score <- function(x) {y <- suppressWarnings(as.integer(x)); ifelse(!is.na(y) & y %in% 1:5, y, unname(score_lookup[clean(x)]))}
quad <- function(sc,ps) case_when(sc>3 & ps>=3~"Actual synergy", sc<=3 & ps>=3~"Potential synergy", sc>3 & ps<3~"Potential conflict", TRUE~"Actual conflict")
add_quadrant_fields <- function(x) mutate(x, class=quad(sc,ps), quadrant=unname(quadrant_labels[class]))

scores <- {
  raw <- read.csv("syncon_collaborate_table.csv", check.names=FALSE); names(raw) <- make.unique(names(raw)); names(raw)[1] <- "Measure_card"
  raw <- filter(raw, !is.na(`Short name`), `Short name`!="", `Short name`!="'--'")
  bind_rows(lapply(seq_len(nrow(raw)), \(i) bind_rows(lapply(streams_raw, \(s)
    tibble(measure=raw$Measure_card[i], abbr=raw$`Short name`[i], pilot_stream=streams_lab[[s]], sc=score(raw[[paste0(s,"_spatial")]][i]), ps=score(raw[[paste0(s,"_public")]][i])))))) |>
    filter(!is.na(sc), !is.na(ps)) |> add_quadrant_fields()
}
sections <- st_read("pilot_50msection_spatial_variables.gpkg", quiet=TRUE) |> st_transform(4326)
map_vars <- names(variable_labels)[names(variable_labels) %in% names(sections)]

plot_quadrants <- function(x, lab="abbr") {
  if(!"pilot_stream" %in% names(x)) x$pilot_stream <- "Case"
  jittered <- position_jitter(width=.06, height=.06, seed=42)
  p <- ggplot(x, aes(sc, ps)) +
    geom_vline(xintercept=3.5, linetype="22", color="#bdbdbd") + geom_hline(yintercept=2.5, linetype="22", color="#bdbdbd") +
    geom_text(data=data.frame(x=c(1.0,3.8,1.0,3.8),y=c(3.35,3.35,1.45,1.45),
      label=c("High Restoration\nOpportunity","Recognized Asset","Information /\nRestoration Gap","Perception Gap")),
      aes(x=x,y=y,label=label),inherit.aes=FALSE,hjust=0,color="black",fontface="bold",size=3.6) +
    geom_point(aes(fill=class, shape=pilot_stream), position=jittered, size=3.9, color="black", stroke=.25) +
    geom_point(aes(color=class), alpha=0, show.legend=TRUE) +
    geom_text_repel(aes(label=.data[[lab]]), position=jittered, color="black", size=2.7,
      max.overlaps=Inf, seed=42, box.padding=.22, point.padding=.12, min.segment.length=0,
      segment.color="#66d7d1", show.legend=FALSE) +
    scale_x_continuous("Spatial condition", breaks=1:5, limits=c(.5,5.5)) + scale_y_continuous("Public support", breaks=1:5, limits=c(.5,5.5)) +
    scale_fill_manual(values=quadrant_colors, limits=names(quadrant_colors), drop=FALSE, guide="none") +
    scale_color_manual("Category", values=quadrant_colors, limits=names(quadrant_colors), drop=FALSE) +
    scale_shape_manual("Water Body", values=stream_shapes) +
    guides(color=guide_legend(override.aes=list(shape=21, fill=unname(quadrant_colors), alpha=1, size=3)), shape=guide_legend(override.aes=list(fill="white", color="black", size=3))) +
    coord_cartesian(clip="off") + theme_minimal(base_size=14) + theme(text=element_text(color="black"), axis.text=element_text(color="black"), panel.grid.major=element_line(color="#eeeeee",linewidth=.3), panel.grid.minor=element_blank(), legend.position="bottom", legend.text=element_text(size=9), legend.title=element_text(size=9), legend.key.size=grid::unit(.35,"cm"), legend.spacing.x=grid::unit(.1,"cm"), plot.background=element_rect(fill="#ffffff", color=NA), plot.margin=margin(20,95,20,95))
  if(nrow(x)>20 && length(unique(x$pilot_stream))>1) p <- p + facet_wrap(~pilot_stream,ncol=2)
  p
}

report_abbr <- function(x) {
  x <- trimws(x)
  x[x=="Accesibility_Along_Stream"] <- "Accessibility_Along_Stream"
  x[x=="Groyne_Baffles_DeadWood"] <- "Groynes_Baffles_DeadWood"
  x
}
scores$abbr <- report_abbr(scores$abbr)
example_cases$measure <- report_abbr(example_cases$measure)
report_measure_names <- c(
  Increase_Space="Increase space for river and floodplain",
  Accessibility_Along_Stream="Provide accessibility along the stream",
  Riparian_Trees="Plant riparian trees",
  Green_Corridor="Add transversal green corridor",
  Remove_TechRiverbed="Remove technical riverbed",
  Remove_Concrete_Channel="Remove concrete channel; install fascines where necessary",
  Riparian_Buffer="Plant riparian vegetation (buffer strips)",
  Remove_BiologicalBarriers="Remove in-stream barriers",
  Instream_Structures="Add in-stream structures (dead wood, stones)",
  Access_Stream="Provide access into the stream",
  Reconnect_Floodplains="Reconnect floodplains",
  Green_Network="Create an interconnected network of green spaces",
  Nature_Observation="Enable nature observation",
  Groynes_Baffles_DeadWood="Add groynes, baffles, dead wood, and circulation trees",
  Service_Buffer="Ensure continuous service buffer",
  Integrate_Programmes="Integrate programs (e.g., leisure, education, swimming)",
  Unseal_Surfaces="Unseal impervious surfaces")
measure_choices <- tibble(abbr=names(report_measure_names),measure=unname(report_measure_names))
toolkit_purpose <- c(
  Increase_Space="Widen the stream corridor to reduce downstream flooding.",
  Accessibility_Along_Stream="Create continuous walking and cycling paths along the stream.",
  Riparian_Trees="Plant riverbank trees to increase shade over the water.",
  Green_Corridor="Connect ecosystems with green corridors for wildlife movement.",
  Remove_TechRiverbed="Remove engineered riverbed and use natural materials where needed.",
  Remove_Concrete_Channel="Remove concrete banks and use natural reinforcement where needed.",
  Riparian_Buffer="Plant buffer strips of at least 5 m to filter urban runoff and pollutants.",
  Remove_BiologicalBarriers="Remove or adapt barriers to improve wildlife passage.",
  Instream_Structures="Add wood or stones to diversify flow and habitat.",
  Access_Stream="Create places to enter and interact with the stream.",
  Reconnect_Floodplains="Allow water to overflow into adjacent land.",
  Green_Network="Connect ecological zones and recreational routes.",
  Nature_Observation="Create places for wildlife observation and leisure.",
  Groynes_Baffles_DeadWood="Use natural embankments to vary flow, erosion and sedimentation.",
  Service_Buffer="Maintain access beside the stream for upkeep.",
  Integrate_Programmes="Support play, learning and relaxation beside the stream.",
  Unseal_Surfaces="Remove sealed surfaces to increase infiltration and reduce runoff.")
spatial_overview <- c(
  Access_Stream="Access to the water", Integrate_Programmes="Leisure, education and sport POIs within 50 m",
  Nature_Observation="Benches within 50 m", Instream_Structures="Riverbed structure",
  Remove_Concrete_Channel="Bed and bank structure", Service_Buffer="Bank and surrounding environmental structure",
  Groynes_Baffles_DeadWood="Special riverbed structures", Unseal_Surfaces="Impervious cover within 50 m (inverse)",
  Remove_TechRiverbed="Share of 50 m sections with riverbed construction",
  Green_Corridor="Bank vegetation and surrounding land use", Increase_Space="Distance from stream to nearest building",
  Remove_BiologicalBarriers="Continuity for fish and macroinvertebrates",
  Green_Network="Bank vegetation and surrounding land use",
  Accessibility_Along_Stream="Pedestrian and cycling network length within 50 m",
  Reconnect_Floodplains="Surrounding environmental structure", Riparian_Trees="Canopy cover within 10 m",
  Riparian_Buffer="Bank vegetation and mapped 3 m buffer strip (pilot proxy)")
public_overview <- c(
  Access_Stream="Access to water", Integrate_Programmes="Education, inspiration, social contact and recreation",
  Nature_Observation="Nature observation", Instream_Structures="Preference for in-stream elements",
  Remove_Concrete_Channel="Preference concerning concrete streams (inverse)", Service_Buffer="Greenery and riparian vegetation",
  Groynes_Baffles_DeadWood="Preference for dead wood", Unseal_Surfaces="Urban-park preference as proxy (inverse)",
  Remove_TechRiverbed="Preference for meanders and in-stream elements",
  Green_Corridor="Habitat for plants and animals", Increase_Space="Preference for meanders and in-stream elements",
  Remove_BiologicalBarriers="Fish passage", Green_Network="Riparian vegetation and surrounding greenery",
  Accessibility_Along_Stream="Ability to travel along the stream",
  Reconnect_Floodplains="Preference for pools and floodplain wetlands",
  Riparian_Trees="Importance of microclimate improvement", Riparian_Buffer="Preference for wide riparian vegetation")

card_pages <- c(Riparian_Trees=1, Riparian_Buffer=4, Instream_Structures=5,
  Groynes_Baffles_DeadWood=6, Remove_Concrete_Channel=7, Remove_TechRiverbed=9,
  Unseal_Surfaces=12, Remove_BiologicalBarriers=15, Green_Corridor=19,
  Green_Network=20, Accessibility_Along_Stream=25, Access_Stream=28,
  Integrate_Programmes=30, Nature_Observation=33, Service_Buffer=34,
  Increase_Space=35, Reconnect_Floodplains=36)
measure_names <- setNames(measure_choices$measure, measure_choices$abbr)
measure_names["Groyne_Baffles_DeadWood"] <- measure_names["Groynes_Baffles_DeadWood"]
case_measure_choices <- setNames(measure_choices$abbr, measure_choices$measure)
spatial_rules <- list(
  Riparian_Trees=list(variable="canopy_cover_10m",label="Riparian canopy cover",higher=TRUE),
  Integrate_Programmes=list(variable="poi_total_50m",label="Program POIs",higher=TRUE),
  Nature_Observation=list(variable="bench_count_50m",label="Bench count",higher=TRUE),
  Unseal_Surfaces=list(variable="impervious_cover_50m",label="Impervious cover",higher=FALSE))
# Variable wording: report Table 1. Units/scales: Table 1, Section 2.4 and map figures.
spatial_input_reference <- tibble::tribble(
  ~measure, ~variable_name, ~unit, ~definition,
  "Access_Stream", "Accessibility to the water", "5-step scale from very good to not possible", "This variable directly ranks the accessibility to the water on a 5-step scale from very good to not possible.",
  "Integrate_Programmes", "Point of interest (*1)", "count", "The number of related programme’s points of interest that include leisure, education, sports, etc., representing the opportunities for multifunctional purpose.",
  "Nature_Observation", "Bench count (*1)", "count", "The bench installation provides infrastructure and spots for wildlife observation and leisure.",
  "Instream_Structures", "Stream Bed (Main Parameter)", "5-level index scale", "This combination of different stream bed parameters, including substrate and its diversity, the degree of the occurrence of special bed structures or bed constructions, gives insight into the need for structural improvements.",
  "Remove_Concrete_Channel", "Mean of stream bed structure and stream bank structure", "5-level index scale", "This combination gives insight into the degree of existing technical channel constructions, including the stream course and profile, the stream bed, the base of embankment and the stream bank.",
  "Service_Buffer", "Mean of stream bank structure and stream environmental structure (*1)", "5-level index scale", "This combination covers especially the condition of the base of embankment and the land use and vegetation of the stream bank and environment. It thus enables conclusions to be drawn about the current functionality of the buffer strip.",
  "Groynes_Baffles_DeadWood", "Special bed structures", "none (very low), few (medium) or many (very high)", "Special bed structures is a parameter assessed in the stream structural mapping It can be none (very low), few (medium) or many (very high) and provides a rough overview of the current presence of such structures.",
  "Unseal_Surfaces", "Impervious cover (*2)", "percentage", "The impervious cover represents the percentage of surfaces that are sealed.",
  "Remove_TechRiverbed", "Percentage of stream sections where 10 to 100% of the stream bed are (technically) constructed", "Percentage of stream sections", "Classified in 20% steps, this variable directly corresponds to the need for removing technical constructions.",
  "Green_Corridor", "Mean of vegetation left/right (stream bank) and Land use and vegetation left/right (stream environment) (*1)", "5-level index scale", "This variable provides information on the current status of vegetation along the stream banks and in the surrounding area, as a contribution to transversal green corridors.",
  "Increase_Space", "Distance from stream to building", "m", "Mean distance between stream and the closest building for every 10m along the 50m section, which indicates the available space that can be further developed as stream corridors to reduce downstream flooding risk.",
  "Remove_BiologicalBarriers", "Continuousness (for aquatic organisms)", "three-point scale ranging from ‘no’ via ‘limited’ to ‘yes’", "Continuousness assesses the barrier effect of transverse facilities as well as bed constructions without a sediment cover on a three-point scale ranging from ‘no’ via ‘limited’ to ‘yes’.",
  "Green_Network", "Mean of Vegetation left/right (stream bank) and Land use and vegetation left/right (stream environment) (*1)", "5-level index scale", "This variable provides information on the current status of vegetation along the stream banks and in the surrounding area, as a contribution to an interconnected network of green spaces.",
  "Accessibility_Along_Stream", "Slow mobility length (*1)", "m", "Slow mobility length indicates walking and cycling path possibilities along the stream.",
  "Reconnect_Floodplains", "Environmental structure (*1)", "5-level index scale", "This combination of different stream environmental parameters, including land use and vegetation or pollutions of the water’s edge areas, gives insight into the degree of naturalness of the area.",
  "Riparian_Trees", "Canopy cover", "0–1", "The tree cover in the riparian zones (10m buffer on both sides of the section) represent the current riparian tree cover, which is related to shading on waterbody.",
  "Riparian_Buffer", "Mean of vegetation left/right (bank) and river buffer strip 3 m (environment)", "5-level index scale", "This variable assesses the vegetation at the stream bank and the adjacent buffer strip.") |>
  arrange(match(measure,measure_choices$abbr)) |>
  mutate(spatial_indicator=variable_name,.after=measure)
public_input_reference <- tibble::tribble(
  ~measure, ~variable_name, ~unit, ~definition,
  "Access_Stream", "Access to water - stairs and pier (a)", "Choice-experiment coefficient", "Access to water - stairs and pier (a)",
  "Integrate_Programmes", "Education, inspiration, casual contacts and recreational opportunities (c)", "1–4", "Education, inspiration, casual contacts and recreational opportunities (c)",
  "Nature_Observation", "Observing nature (d)", "0–3", "Observing nature (d)",
  "Instream_Structures", "Isles, water terraces, stones, dead wood in the stream (b)", "1–10", "Isles, water terraces, stones, dead wood in the stream (b)",
  "Remove_Concrete_Channel", "Concrete stream (b) (*2)", "1–10", "Concrete stream (b) (*2)",
  "Service_Buffer", "Arrangement of greenery in the wider area and riparian vegetation (a)", "Choice-experiment coefficient", "Arrangement of greenery in the wider area and riparian vegetation (a)",
  "Groynes_Baffles_DeadWood", "Dead wood (b)", "1–10", "Dead wood (b)",
  "Unseal_Surfaces", "Urban park (a) as proxy for impervious surface (*2)", "Choice-experiment coefficient", "Urban park (a) as proxy for impervious surface (*2)",
  "Remove_TechRiverbed", "Meanders with/-out elements (a)", "Choice-experiment coefficient", "Meanders with/-out elements (a)",
  "Green_Corridor", "Habitats for plants and animals (c)", "1–4", "Habitats for plants and animals (c)",
  "Increase_Space", "Meanders with elements (a)", "Choice-experiment coefficient", "Meanders with elements (a)",
  "Remove_BiologicalBarriers", "Fish passage (b)", "1–10", "Fish passage (b)",
  "Green_Network", "Wide riparian vegetation and natural arrangement of greenery in the wider area (a)", "Choice-experiment coefficient", "Wide riparian vegetation and natural arrangement of greenery in the wider area (a)",
  "Accessibility_Along_Stream", "Get from \"A to B\" (d)", "0–3", "Get from \"A to B\" (d)",
  "Reconnect_Floodplains", "Pools and wetlands in floodplains (b)", "1–10", "Pools and wetlands in floodplains (b)",
  "Riparian_Trees", "Improvement of microclimate (c) (as proxy for the added value of tree)", "1–4", "Improvement of microclimate (c) (as proxy for the added value of tree)",
  "Riparian_Buffer", "Wide riparian vegetation (b)", "1–10", "Wide riparian vegetation (b)") |>
  arrange(match(measure,measure_choices$abbr)) |>
  mutate(public_support_indicator=variable_name,.after=measure)
normalize_case_measure <- function(x) {
  x <- report_abbr(trimws(as.character(x))); matched <- unname(case_measure_choices[x])
  ifelse(is.na(matched),x,matched)
}
score_spatial_raw <- function(measure, value) {
  rule <- spatial_rules[[measure]]
  if (!is.null(rule) && is.finite(value)) {
    ref <- sections[[rule$variable]]; p <- if(rule$higher) mean(ref<=value,na.rm=TRUE) else mean(ref>=value,na.rm=TRUE)
    return(as.integer(pmax(1,pmin(5,ceiling(5*p)))))
  }
  if(is.finite(value) && value %in% 1:5) as.integer(value) else NA_integer_
}
score_public_raw <- function(value, scale) {
  if(!is.finite(value)) return(NA_integer_)
  x <- switch(scale,
    benefit_1_4=1+4*(value-1)/3,
    rating_1_10=1+4*(value-1)/9,
    use_0_3=1+4*value/3,
    prepared_1_5=value,
    NA_real_)
  as.integer(pmax(1,pmin(5,round(x))))
}
valid_public_value <- function(value,scale) {
  bounds <- list(benefit_1_4=c(1,4),rating_1_10=c(1,10),use_0_3=c(0,3),prepared_1_5=c(1,5))[[scale]]
  !is.null(bounds) && is.finite(value) && value>=bounds[1] && value<=bounds[2] && (scale!="prepared_1_5" || value %in% 1:5)
}
units <- c(bench_count_50m="count", canopy_cover_10m="proportion (0–1)", impervious_cover_50m="%", poi_total_50m="count", slowmobility_length_m_50m="m", building_distance_mean_m="m")
intro <- function(title, text) div(class="step-note", h3(title), tags$p(text))
help_box <- div(class="step-note", h3("Help"),
  tags$p("Responses to questions raised by city partners."),
  h5("Who is the tool for, and how does it relate to other project outputs?"), tags$p("City planners and restoration teams can compare restoration needs with public preferences and use the results in restoration planning and stakeholder discussions. The Toolkit describes the measures; spatial mapping and citizen surveys provide the evidence; SynCon brings these perspectives together. Links to other consortium tools have not yet been added."),
  h5("Where are the analytical steps and measure descriptions?"), tags$p("ReBioClim Example presents the pilot workflow. Steps 1–4 follow the same sequence: restoration measures, spatial data, public support and results. Step 1 lists the 17 measures and their indicators; click a row to open its Toolkit card. Steps 2 and 3 place variable definitions and original units beside the downloadable tables."),
  h5("How do I enter a new city or study area?"), tags$p("Download the two tables in Steps 2 and 3. Use one row for each study area and restoration measure, with matching case and measure names in both files. Follow each page's input-scale instructions, upload both files in Step 4 and click Calculate SynCon. Alternatively, paste standardized spatial-condition and public-support scores from 1 to 5. The case identifies your study area; the measure identifies the restoration action being assessed."),
  h5("Who prepares the spatial and survey data?"), tags$p("The city team defines the study area and restoration question. Geodata and ecological specialists prepare spatial analysis and structural mapping; survey specialists prepare the public-preference evidence. These tasks can be commissioned externally. Complete the preparation before uploading the case-level values."),
  h5("What do the five levels and map colors mean?"), tags$p("The final scale runs from very low (1) to very high (5). Spatial condition describes the relevant existing condition or availability; public support describes preference for the measure. Numeric spatial classes use the pooled pilot-section distribution, while structural indicators begin with field-assessment categories. Map colors become darker as values increase. ReBioClim Example shows section-level canopy values alongside the pilot mean and final score."),
  h5("How are public-support variables selected and weighted?"), tags$p("Each measure is linked to the relevant survey preference, benefit or site-use variable listed in Step 3. Where several variables represent one measure, the pilot method uses their average. The source evidence comprises choice-experiment preferences, feature ratings (1–10), benefit importance (1–4) and site use (0–3). Cross-variable interaction terms and additional weighting are not specified in the documented SynCon aggregation."),
  h5("Why use synergy and conflict, and why are the boundaries at 3.5 and 2.5?"), tags$p("The categories retain the pilot terminology. Spatial scores 4–5 fall on the right and 1–3 on the left; public-support scores 3–5 fall above and 1–2 below. The lines at 3.5 and 2.5 separate these integer groups. Partners suggested using the term consensus and a zero-centered scale. These suggestions have not yet been adopted in this app. The classification table explains the restoration meaning of all four categories."),
  h5("How can I inspect the results and use the map?"), tags$p("In ReBioClim Example, select a pilot stream and categories, then hover over a point for the measure name and scores. The plot and classification table use matching category colors. After reviewing the quadrant, use the map below to locate sections relevant to improving spatial conditions."),
  h5("Can the tool be used in German and on a desktop?"), tags$p("The layout is designed for desktop use. The current interface is in English. A German version is not yet available."))

workflow_ui <- function() page_navbar(title="ReBioClim Synergies and Conflicts", fillable=FALSE,
  theme=bs_theme(version=5, bootswatch="flatly", primary="#2f41dd", bg="#ffffff", fg="#000000"), header=includeCSS("app.css"),
  nav_panel("About", div(class="step-note", h3("ReBioClim: Restoring urban streams to promote Biodiversity, Climate adaptation and to improve quality of life in cities"),tags$p(
    "ReBioClim is an Interreg Central Europe project that develops nature-based approaches to urban stream restoration to promote biodiversity, climate adaptation and quality of life. Researchers, cities and practitioners work with stakeholders in Dresden, Jablonec nad Nisou, Poznań and Senica to develop restoration strategies and action plans."),
    tags$a(href="https://www.interreg-central.eu/projects/rebioclim/",target="_blank",rel="noopener","Explore the ReBioClim project and its outputs")),
    div(class="cardx", h4("Synergies and Conflicts analysis for urban stream restoration"),
      tags$p("A restoration measure may address a spatial need while receiving strong public support. In other cases, existing conditions and public preferences may point in different directions. Making these relationships visible helps city teams identify where restoration opportunities and questions for stakeholder engagement need closer examination."),
      h5("What this part of ReBioClim does"),
      tags$p("This analysis brings together ecological restoration goals, citizens’ preferences, urban spatial analysis and co-design workshop findings. It identifies where these perspectives align and where restoration needs or differences in public perception require further discussion. For each selected restoration measure, spatial condition and public support are compared on a five-level scale at pilot-site level."),
      h5("What you can explore here"),
      tags$p("This tool presents the results for Bila Nisa, Geberbach, Piasnica and Teplica. Explore the restoration measures, inspect mapped conditions along 50 m stream sections, review the indicator definitions and how the scores were assigned, and compare the pilot-level SynCon results."),
      h5("Who is it for?"),
      tags$p("City planners and restoration teams can use the tool to interpret pilot evidence and discuss restoration options. Geodata and survey specialists can use the accompanying indicator and scoring information to understand the basis of the results."),
      h5("Four steps: from measures to results"),
      tags$ol(class="workflow-steps",
        tags$li(tags$strong("Restoration Measures"), tags$p("Review the 17 restoration measures and their indicators.")),
        tags$li(tags$strong("Spatial Data"), tags$p("Prepare values for the study area's 50 m stream sections.")),
        tags$li(tags$strong("Public Support"), tags$p("Collect survey evidence on the original questionnaire scale.")),
        tags$li(tags$strong("SynCon Results"), tags$p("Compare spatial condition and public support in the four quadrants."))),
      tags$p("ReBioClim Example shows the data and results for the pilot streams. Follow Steps 1–4 to enter data for your own case."))),
  nav_panel("ReBioClim Example",intro("Introduction", "This page shows the analysis for four ReBioClim pilot streams. It starts with 17 restoration measures, uses spatial data from 50 m stream sections and public survey evidence, converts them into spatial-condition and public-support scores from 1 to 5, then classifies each measure in one of four SynCon quadrants. The four sections below follow the same steps as Steps 1–4 for your own case."),
    div(class="cardx",h5("1 Restoration Measures"),tags$p("The pilot analysis included all 17 restoration measures from the Toolkit. Each measure was linked to a spatial indicator and public-support evidence. The next page lists all 17 measures and opens their Toolkit cards.")),
    div(class="cardx",h5("2 Spatial Data"),tags$p("The pilot streams were divided into 50 m sections. Spatial indicators were calculated for each section and summarized at pilot-stream level. For riparian trees, the example below starts with section-level canopy cover and shows the pilot mean and spatial-condition score."),
      h5("Riparian canopy cover example"),DTOutput("canopy_sections"),DTOutput("score_spatial")),
    div(class="cardx",h5("3 Public Support"),tags$p("The pilot used face-to-face survey responses and choice-experiment evidence. For riparian trees, microclimate-benefit importance was summarized and standardized to a public-support score."),DTOutput("score_support")),
    div(class="cardx",h5("4 SynCon Results"),tags$p("Choose a pilot stream and one or more categories. Hover over a point to see its restoration measure and exact scores. Slight jitter separates overlapping points."),
      layout_columns(col_widths=c(4,8),fill=FALSE,
        div(selectInput("example_stream","Pilot stream",choices=unname(streams_lab)),checkboxGroupInput("example_categories","SynCon categories",choices=names(quadrant_colors),selected=names(quadrant_colors))),
        div(plotlyOutput("process_plot",height=650))),
      h5("Classification rules"),div(class="classification-table",DTOutput("classification_table"))),
    div(class="cardx",h5("Explore mapped conditions"),tags$p("After identifying a SynCon category, inspect the 50 m sections for the selected pilot stream to see where spatial conditions could be improved."),
      layout_columns(col_widths=c(3,9),fill=FALSE,
        div(selectInput("map_stream","Pilot stream",setNames(streams_raw,streams_lab)),selectInput("map_var","Spatial indicator",setNames(map_vars,variable_labels[map_vars])),radioButtons("basemap","Basemap",c("OpenStreetMap"="OpenStreetMap","Satellite"="Esri.WorldImagery"))),
        div(leafletOutput("map",height=650))))),
  nav_panel("1 Restoration Measures", intro("Restoration measures", "All 17 pilot measures are listed below. Click a row to enlarge its Toolkit card."),
    div(class="cardx measure-overview",h5("Measures and indicators"),DTOutput("mapping"))),
  nav_panel("2 Spatial Data", intro("Spatial data for your case", "Prepare one value per case and restoration measure. Download the table and fill the raw spatial values. Section-level measurements should be summarized before entry; for measures without a numeric conversion rule, use an agreed 1–5 spatial-condition class. Upload the completed table in Step 4."),
    div(class="cardx",h5("From 50 m sections to one stream-level input"),
      tags$p("Use one case name for the whole stream study area. For numeric spatial indicators, collect the raw values for its 50 m sections and calculate the mean for that stream. Calculate the mean from the raw measurements, before converting the stream-level value to a 1–5 score."),
      tags$p("Canopy-cover example: section values 0.20, 0.30 and 0.40 give a stream mean of (0.20 + 0.30 + 0.40) / 3 = 0.30. In the downloaded table, enter the stream name under case, Riparian_Trees under measure and 0.30 under raw_spatial_value. These three sections become one row, not three rows."),
      tags$p("For canopy cover, points of interest, bench count and impervious cover, enter the stream mean in the original input units. For the other measures, prepare the stream-level spatial-condition score first and enter that 1–5 score. Follow the variable definition for the aggregation: for example, the technical-riverbed variable is the percentage of sections with constructed beds. Repeat for each measure and each stream.")),
    div(class="cardx",downloadButton("download_spatial_template","Download spatial-data table")),
    div(class="cardx",h5("Variables, definitions and original units"),
      tags$p("Units below describe the original measurements and field assessments. For upload, enter case means for canopy cover, points of interest, bench count and impervious cover; enter standardized 1–5 spatial-condition scores for the other measures. Fill case and raw_spatial_value, with one row per case and measure."),
      DTOutput("spatial_input_reference"),
      tags$p("(*1) In 50m buffer on both sides of the section."),
      tags$p("(*2) Value was inverted.")),
    div(class="cardx",h5("Example: filling the spatial-data table"),tags$p("Example canopy-cover values for three rows in Step 4. Enter the case, measure and one summarized raw value in the downloaded table."),
      tags$table(class="table",tags$thead(tags$tr(tags$th("case"),tags$th("measure"),tags$th("spatial_indicator"),tags$th("raw_spatial_value"))),
        tags$tbody(
          tags$tr(tags$td("Bila Nisa"),tags$td("Riparian_Trees"),tags$td("Canopy cover within 10 m"),tags$td("0.30")),
          tags$tr(tags$td("Geberbach"),tags$td("Riparian_Trees"),tags$td("Canopy cover within 10 m"),tags$td("0.90")),
          tags$tr(tags$td("Teplica"),tags$td("Riparian_Trees"),tags$td("Canopy cover within 10 m"),tags$td("1.00")))),
      tags$p("Here, canopy cover is a proportion from 0 to 1. The app converts these values to spatial-condition scores of 2, 4 and 5."))),
  nav_panel("3 Public Support", intro("Public-support data for your case", "As in Step 2, use one row per case and restoration measure. For Riparian_Trees, enter the mean microclimate-importance rating on its original 1–4 scale. For other measures, enter a standardized 1–5 public-support score. Upload the completed table in Step 4."),
    div(class="cardx",h5("From survey responses to one stream-level input"),
      tags$p("Public-support evidence refers to the whole pilot site rather than individual 50 m sections. Group survey responses by the same stream study area used in Step 2. For rating questions, calculate the mean response for the relevant variable. Where several survey variables represent one measure, the pilot method uses their average. Choice-experiment measures use the model results for that site."),
      tags$p("Microclimate example: three respondents give ratings of 2, 3 and 4 on the original 1–4 scale. Their mean is (2 + 3 + 4) / 3 = 3. Enter the same stream name under case, Riparian_Trees under measure and 3 under raw_public_value. The responses become one row for the stream and measure."),
      tags$p("For Riparian_Trees, upload this mean 1–4 rating. For the other measures, prepare the stream-level 1–5 public-support score before filling the table. Match each row to the same case and measure in the spatial table, then upload both tables in Step 4.")),
    div(class="cardx",downloadButton("download_support_template","Download public-support table")),
    div(class="cardx",h5("Variables, definitions and original units"),
      tags$p("Units below describe the original survey evidence. For upload, enter the mean 1–4 microclimate-importance rating for Riparian_Trees and standardized 1–5 public-support scores for the other measures. Fill raw_public_value and use the same case and measure names as in your spatial-data table."),
      DTOutput("public_input_reference"),
      tags$p("(*2) Value was inverted."),
      tags$p("Choice-experiment coefficient: derived by analyzing residents’ choices between different stream restoration designs."),
      tags$p("The letters (a), (b), (c) and (d) identify the questionnaire section. See D1.3.1 Section 2.4 for more details.")),
    div(class="cardx",h5("Example: filling the public-support table"),tags$p("Use the same case and measure as in the spatial-data table. The example values below are mean microclimate-importance ratings on the original 1–4 scale."),
      tags$table(class="table",tags$thead(tags$tr(tags$th("case"),tags$th("measure"),tags$th("public_support_indicator"),tags$th("raw_public_value"))),
        tags$tbody(
          tags$tr(tags$td("Bila Nisa"),tags$td("Riparian_Trees"),tags$td("Importance of microclimate improvement"),tags$td("3")),
          tags$tr(tags$td("Geberbach"),tags$td("Riparian_Trees"),tags$td("Importance of microclimate improvement"),tags$td("4")),
          tags$tr(tags$td("Teplica"),tags$td("Riparian_Trees"),tags$td("Importance of microclimate improvement"),tags$td("4")))),
      tags$p("The app converts these to public-support scores of 4, 5 and 5. Together with Step 2, they reproduce the three Riparian_Trees rows in the Step 4 paste example."))),
  nav_panel("4 SynCon Results",intro("SynCon quadrant results", "Paste scores already standardized to 1–5, or upload your filled-out tables for the app to standardize. Click Calculate SynCon to see the final quadrant results."),
    div(class="cardx",h5("Input data"),radioButtons("syncon_mode",NULL,c("Upload your filled-out table"="upload","Paste standardized scores"="paste"),selected="upload",inline=TRUE),
      conditionalPanel("input.syncon_mode == 'upload'",layout_columns(col_widths=c(6,6),fill=FALSE,
        fileInput("spatial_upload","Spatial-data CSV",accept=".csv"),fileInput("support_upload","Public-support CSV",accept=".csv"))),
      conditionalPanel("input.syncon_mode == 'paste'",tags$p("Edit or replace the example below. Columns are case, measure, spatial_condition and public_support. Both scores must be 1–5."),textAreaInput("paste_sc_ps","Standardized input table",value=paste_example_text,rows=12)),
      actionButton("calculate_syncon","Calculate SynCon",class="btn-primary")),
    div(class="cardx",h5("SynCon quadrant results"),selectInput("result_case","Stream / case",choices=c("All streams"="All")),plotOutput("case_plot",height=650))),
  nav_panel("Help", help_box))

workflow_server <- function(input, output, session) {
  output$classification_table <- renderDT(
    datatable(classification_table,selection="none",rownames=FALSE,options=list(paging=FALSE,dom="t",scrollX=TRUE)) |>
      formatStyle(names(classification_table),valueColumns="Conflict or synergy for restoration",target="cell",
        backgroundColor=styleEqual(names(quadrant_colors),unname(quadrant_colors)),
        color=styleEqual(names(quadrant_colors),c("white","white","black","black"))),server=FALSE)
  pilot_plot_data <- reactive({
    req(input$example_stream)
    x <- scores |> filter(pilot_stream==input$example_stream,class %in% input$example_categories)
    validate(need(nrow(x)>0,"Select at least one SynCon category with results for this pilot stream."))
    i <- match(x$abbr,measure_choices$abbr)
    x |> mutate(plot_x=sc+((((i*7) %% 17)-8)*.006),plot_y=ps+((((i*11) %% 17)-8)*.006))
  })
  observeEvent(input$example_stream, {
    stream <- names(streams_lab)[unname(streams_lab)==input$example_stream]
    req(length(stream)==1)
    updateSelectInput(session,"map_stream",selected=stream)
  })
  output$process_plot <- renderPlotly({
    x <- as.data.frame(pilot_plot_data())
    labels <- list(
      list(x=1.15,y=3.6,text="High Restoration<br>Opportunity",showarrow=FALSE,xanchor="left",font=list(color="#000000")),
      list(x=3.75,y=3.6,text="Recognized Asset",showarrow=FALSE,xanchor="left",font=list(color="#000000")),
      list(x=1.15,y=1.5,text="Information /<br>Restoration Gap",showarrow=FALSE,xanchor="left",font=list(color="#000000")),
      list(x=3.75,y=1.5,text="Perception Gap",showarrow=FALSE,xanchor="left",font=list(color="#000000")))
    p <- plot_ly()
    for(category in names(quadrant_colors)) {
      z <- x[x$class==category,]
      marker <- list(size=14,color=unname(quadrant_colors[category]),line=list(color="#000000",width=1))
      if(nrow(z)) {
        tooltip <- paste0("<b>",htmltools::htmlEscape(unname(measure_names[z$abbr])),"</b><br>Spatial condition: ",z$sc," | Public support: ",z$ps)
        p <- add_trace(p,x=z$plot_x,y=z$plot_y,text=tooltip,type="scatter",mode="markers",
          name=category,hoverinfo="text",marker=marker,showlegend=TRUE)
      } else {
        p <- add_trace(p,x=0,y=0,type="scatter",mode="markers",name=category,
          hoverinfo="skip",marker=marker,showlegend=TRUE)
      }
    }
    p <- layout(p,xaxis=list(title="Spatial condition",range=c(.5,5.5),tickvals=1:5,zeroline=FALSE,fixedrange=TRUE),
      yaxis=list(title="Public support",range=c(.5,5.5),tickvals=1:5,zeroline=FALSE,fixedrange=TRUE),dragmode=FALSE,
      shapes=list(
        list(type="line",x0=3.5,x1=3.5,y0=.5,y1=5.5,line=list(color="#999999",dash="dash")),
        list(type="line",x0=.5,x1=5.5,y0=2.5,y1=2.5,line=list(color="#999999",dash="dash"))),
      annotations=labels,
      legend=list(orientation="h",x=0,y=-.18,font=list(size=10),itemwidth=30),margin=list(b=100),plot_bgcolor="#ffffff",paper_bgcolor="#ffffff")
    config(p,displayModeBar=FALSE,scrollZoom=FALSE,doubleClick=FALSE)
  })
  mapping_data <- reactive({
    measure_choices |>
      transmute(`Restoration measure`=measure,Abbreviation=abbr,
        `Toolkit purpose`=unname(toolkit_purpose[abbr]),
        `Spatial / structural indicator`=unname(spatial_overview[abbr]),
        `Public-support indicator`=unname(public_overview[abbr]))
  })
  output$mapping <- renderDT(datatable(mapping_data(),selection="none",rownames=FALSE,options=list(paging=FALSE,dom="t",scrollX=TRUE)),server=FALSE)
  output$spatial_input_reference <- renderDT(datatable(select(spatial_input_reference,measure,variable_name,unit,definition),colnames=c("Restoration measure","Variable name","Original unit / scale","Definition"),rownames=FALSE,selection="none",options=list(paging=FALSE,dom="t",scrollX=TRUE)),server=FALSE)
  output$public_input_reference <- renderDT(datatable(select(public_input_reference,measure,variable_name,unit,definition),colnames=c("Restoration measure","Variable name","Original unit / scale","Definition"),rownames=FALSE,selection="none",options=list(paging=FALSE,dom="t",scrollX=TRUE)),server=FALSE)
  observeEvent(input$mapping_cell_clicked, {
    i <- input$mapping_cell_clicked$row
    req(length(i)==1,i>=1,i<=nrow(measure_choices))
    abbr <- measure_choices$abbr[i]
    name <- measure_choices$measure[i]
    page <- unname(card_pages[abbr])
    req(!is.na(page))
    showModal(modalDialog(
      tags$div(class="toolkit-card-zoom",tags$img(src=paste0("measure-cards/card-",page,".png"),alt=paste("Toolkit card:",name))),
      title=name,size="l",easyClose=TRUE,footer=modalButton("Close")))
  })
  spatial_template_data <- reactive({
    spatial_input_reference |> mutate(case="",raw_spatial_value="") |>
      select(case,measure,spatial_indicator,raw_spatial_value,everything())
  })
  output$download_spatial_template <- downloadHandler(
    filename=function() "syncon-spatial-input.csv",
    content=function(file) write.csv(spatial_template_data(),file,row.names=FALSE,na="")
  )
  support_template_data <- reactive({
    public_input_reference |> mutate(case="",raw_public_value="") |>
      select(case,measure,public_support_indicator,raw_public_value,everything())
  })
  output$download_support_template <- downloadHandler(
    filename=function() "syncon-public-support-input.csv",
    content=function(file) write.csv(support_template_data(),file,row.names=FALSE,na="")
  )
  canopy_sections_data <- reactive({
    sections |> st_drop_geometry() |> transmute(`Pilot stream`=unname(streams_lab[stream_name]),`Section ID`=section_id,`Raw canopy cover (10 m buffer)`=round(canopy_cover_10m,3),`Map class`=canopy_cover_10m_class) |> arrange(match(`Pilot stream`,unname(streams_lab)),`Section ID`)
  })
  output$canopy_sections <- renderDT(datatable(canopy_sections_data(),selection="none",rownames=FALSE,options=list(pageLength=10,scrollX=TRUE)))
  score_pair_data <- reactive({
    spatial <- canopy_sections_data() |> group_by(`Pilot stream`) |> summarise(`Mean canopy cover (raw)`=round(mean(`Raw canopy cover (10 m buffer)`,na.rm=TRUE),3),.groups="drop")
    final <- scores |> filter(abbr=="Riparian_Trees") |> transmute(`Pilot stream`=pilot_stream,`Spatial condition`=sc,`Public support`=ps)
    left_join(spatial,final,by="Pilot stream") |> mutate(`Microclimate support (raw, 1–4)`=unname(microclimate_raw[`Pilot stream`])) |> select(`Pilot stream`,`Mean canopy cover (raw)`,`Spatial condition`,`Microclimate support (raw, 1–4)`,`Public support`) |> arrange(match(`Pilot stream`,unname(streams_lab)))
  })
  output$score_spatial <- renderDT(datatable(score_pair_data() |> select(`Pilot stream`,`Mean canopy cover (raw)`,`Spatial condition`),selection="none",rownames=FALSE,options=list(paging=FALSE,dom="t",scrollX=TRUE)))
  output$score_support <- renderDT(datatable(score_pair_data() |> select(`Pilot stream`,`Microclimate support (raw, 1–4)`,`Public support`),selection="none",rownames=FALSE,options=list(paging=FALSE,dom="t",scrollX=TRUE)))
  spatial_input <- reactive({
    if(is.null(input$spatial_upload)) return(tibble(case=character(),measure=character(),spatial_indicator=character(),raw_spatial_value=numeric()))
    x <- read.csv(input$spatial_upload$datapath,check.names=FALSE,na.strings=c("","NA")); names(x) <- gsub("[^a-z0-9]+","_",tolower(names(x)))
    validate(need(all(c("case","measure","raw_spatial_value") %in% names(x)),"SynCon spatial CSV needs case, measure and raw_spatial_value."))
    x <- tibble(case=trimws(as.character(x$case)),measure=trimws(as.character(x$measure)),spatial_indicator=if("spatial_indicator" %in% names(x)) as.character(x$spatial_indicator) else NA_character_,raw_spatial_value=trimws(as.character(x$raw_spatial_value)))
    x <- x |> filter(!is.na(raw_spatial_value)&nzchar(raw_spatial_value)) |> mutate(raw_spatial_value=suppressWarnings(as.numeric(raw_spatial_value)))
    validate(need(nrow(x)>0,"Fill at least one raw spatial value in the downloaded table."),need(all(!is.na(x$case)&nzchar(x$case)&!is.na(x$measure)&nzchar(x$measure)),"Every filled spatial row needs a case and measure."),need(all(is.finite(x$raw_spatial_value)),"Spatial values must be numeric."))
    validate(need(!anyDuplicated(paste(x$case,normalize_case_measure(x$measure),sep="\r")),"Use only one spatial row per case and restoration measure."))
    x
  })
  support_input <- reactive({
    if(is.null(input$support_upload)) return(tibble(case=character(),measure=character(),raw_public_value=numeric(),public_scale=character()))
    x <- read.csv(input$support_upload$datapath,check.names=FALSE,na.strings=c("","NA")); names(x) <- gsub("[^a-z0-9]+","_",tolower(names(x)))
    validate(need(all(c("case","measure","raw_public_value") %in% names(x)),"Public-support CSV needs case, measure and raw_public_value."))
    x <- tibble(case=trimws(as.character(x$case)),measure=trimws(as.character(x$measure)),raw_public_value=trimws(as.character(x$raw_public_value)),public_scale=if("public_scale" %in% names(x)) trimws(as.character(x$public_scale)) else NA_character_)
    x <- x |> filter(!is.na(raw_public_value)&nzchar(raw_public_value)) |>
      mutate(raw_public_value=suppressWarnings(as.numeric(raw_public_value)),public_scale=ifelse(is.na(public_scale)|!nzchar(public_scale),ifelse(normalize_case_measure(measure)=="Riparian_Trees","benefit_1_4","prepared_1_5"),public_scale))
    validate(need(nrow(x)>0,"Fill at least one public-support value in the downloaded table."),need(all(!is.na(x$case)&nzchar(x$case)&!is.na(x$measure)&nzchar(x$measure)),"Every filled public-support row needs a case and measure."),need(all(is.finite(x$raw_public_value)),"Every public-support row needs a numeric value."),need(all(x$public_scale %in% c("benefit_1_4","rating_1_10","use_0_3","prepared_1_5")),"An optional legacy scale is not recognized."))
    validate(need(!anyDuplicated(paste(x$case,normalize_case_measure(x$measure),sep="\r")),"Use only one public-support row per case and restoration measure."))
    x
  })
  case_calculation_data <- eventReactive(input$calculate_syncon, {
    if(identical(input$syncon_mode,"paste")) {
      validate(need(nzchar(trimws(input$paste_sc_ps)),"Paste a table with case, measure, spatial_condition and public_support, then click Calculate SynCon."))
      pasted <- tryCatch(read.csv(text=input$paste_sc_ps,check.names=FALSE),error=function(e) NULL)
      validate(need(!is.null(pasted),"The pasted table could not be read as CSV."))
      names(pasted) <- gsub("_+","_",gsub("[^a-z0-9]+","_",tolower(names(pasted))))
      if("sc" %in% names(pasted) && !"spatial_condition" %in% names(pasted)) pasted$spatial_condition <- pasted$sc
      if("ps" %in% names(pasted) && !"public_support" %in% names(pasted)) pasted$public_support <- pasted$ps
      validate(need(all(c("case","measure","spatial_condition","public_support") %in% names(pasted)),"The pasted table needs case, measure, spatial_condition and public_support columns."))
      pasted <- tibble(case=trimws(as.character(pasted$case)),measure=trimws(as.character(pasted$measure)),abbr=normalize_case_measure(pasted$measure),sc=score(pasted$spatial_condition),ps=score(pasted$public_support))
      validate(need(nrow(pasted)>0,"Paste at least one case."),need(all(!is.na(pasted$case)&nzchar(pasted$case)&pasted$abbr %in% measure_choices$abbr),"Every row needs a case and a recognized restoration measure."),need(all(!is.na(pasted$sc)&!is.na(pasted$ps)&pasted$sc %in% 1:5&pasted$ps %in% 1:5),"Spatial condition and public support must each be 1–5."),need(!anyDuplicated(paste(pasted$case,pasted$abbr,sep="\r")),"Use one row per case and restoration measure."))
      return(pasted |> mutate(`Study area`=case,`Restoration measure`=unname(measure_names[abbr]),Abbreviation=abbr,`Spatial raw value`=NA_real_,`Spatial condition`=sc,`Public-support raw value`=NA_real_,`Public-support scale`=NA_character_,`Public support`=ps,Category=quad(sc,ps),`Quadrant name`=unname(quadrant_labels[Category])) |>
        select(`Study area`,`Restoration measure`,Abbreviation,`Spatial raw value`,`Spatial condition`,`Public-support raw value`,`Public-support scale`,`Public support`,Category,`Quadrant name`))
    }
    validate(need(!is.null(input$spatial_upload) && !is.null(input$support_upload),"Upload both completed data tables above, then click Calculate SynCon."))
    spatial <- spatial_input() |> filter(!is.na(raw_spatial_value)) |> mutate(abbr=as.character(normalize_case_measure(measure)))
    support <- support_input() |> mutate(abbr=normalize_case_measure(measure))
    validate(need(all(spatial$abbr %in% measure_choices$abbr),"The spatial CSV contains an unknown measure."),need(all(support$abbr %in% measure_choices$abbr),"The public-support CSV contains an unknown measure."))
    validate(need(all(unlist(Map(valid_public_value,support$raw_public_value,support$public_scale))),"A public-support value falls outside its selected original scale."))
    spatial <- spatial |> mutate(SC=vapply(seq_len(nrow(spatial)),function(i) score_spatial_raw(abbr[i],raw_spatial_value[i]),integer(1)))
    support <- support |> mutate(PS=unlist(Map(score_public_raw,raw_public_value,public_scale)))
    x <- full_join(spatial,support,by=c("case","abbr"))
    x |> mutate(`Restoration measure`=unname(measure_names[abbr]),`Spatial raw value`=raw_spatial_value,`Spatial condition`=SC,`Public-support raw value`=raw_public_value,`Public-support scale`=public_scale,`Public support`=PS,Category=ifelse(!is.na(SC)&!is.na(PS),quad(SC,PS),NA_character_),`Quadrant name`=unname(quadrant_labels[Category])) |> select(`Study area`=case,`Restoration measure`,Abbreviation=abbr,`Spatial raw value`,`Spatial condition`,`Public-support raw value`,`Public-support scale`,`Public support`,Category,`Quadrant name`)
  })
  observeEvent(case_calculation_data(), {
    cases <- unique(case_calculation_data()$`Study area`)
    updateSelectInput(session,"result_case",choices=c("All streams"="All",setNames(cases,cases)),selected="All")
  })
  case_result_data <- reactive({
    x <- case_calculation_data()
    if(!is.null(input$result_case) && input$result_case!="All") x <- x |> filter(`Study area`==input$result_case)
    x
  })
  output$case_plot <- renderPlot({
    validate(need(isTRUE(input$calculate_syncon > 0),"Enter data and click Calculate SynCon."))
    x <- case_result_data() |> filter(!is.na(`Spatial condition`),!is.na(`Public support`)) |> transmute(abbr=Abbreviation,sc=`Spatial condition`,ps=`Public support`,pilot_stream=ifelse(`Study area` %in% names(stream_shapes),`Study area`,"Case")) |> add_quadrant_fields()
    validate(need(nrow(x)>0,"No complete quadrant result is available for this stream or case.")); plot_quadrants(x,"abbr") + guides(shape="none")
  },res=144)
}

ui <- workflow_ui()

server <- function(input, output, session) {
  workflow_server(input, output, session)
  map_data <- reactive(filter(sections, stream_name==input$map_stream))
  output$map <- renderLeaflet({
    g <- map_data(); v <- input$map_var; req(v); cc <- paste0(v,"_class")
    vals <- if(cc %in% names(g)) g[[cc]] else g[[v]]
    levels <- if(cc %in% names(g)) score_levels else map_category_levels[[v]]
    req(length(levels)>0)
    pal <- colorFactor(colorRampPalette(c("#66d7d1","#2f41dd"))(length(levels)), levels=levels, ordered=TRUE)
    unit <- unname(units[v]); if(is.na(unit)) unit <- "original category"
    raw_values <- ifelse(is.na(g[[v]]),"No data",as.character(g[[v]]))
    labels <- paste("Section",g$section_id,"|",variable_labels[[v]],"|",raw_values,unit)
    popups <- paste0("<strong>Section ",g$section_id,"</strong><br>",htmltools::htmlEscape(variable_labels[[v]]),": ",htmltools::htmlEscape(raw_values)," (",unit,")",if(cc %in% names(g)) paste0("<br>Class: ",htmltools::htmlEscape(as.character(vals))) else "")
    legend_levels <- rev(levels)
    legend_rows <- paste0("<div class='map-legend-row'><i style='background:",pal(legend_levels),"'></i><span>",htmltools::htmlEscape(legend_levels),"</span></div>",collapse="")
    legend_html <- paste0("<div class='map-legend-title'>",htmltools::htmlEscape(variable_labels[[v]]),"</div>",legend_rows)
    leaflet(g) |> addProviderTiles(providers[[input$basemap]]) |> addPolylines(color=~pal(vals), weight=7, opacity=.9, label=labels,popup=popups) |> addControl(htmltools::HTML(legend_html),position="bottomright",className="map-legend")
  })
}
shinyApp(ui, server)
