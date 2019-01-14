#
# Relative Abundance Barplot
#
# Plot when button is pressed
do_relabu_bar <- eventReactive(input$relabu_bar_plot_btn, {
    p <- relabu_barplot(MAE = vals$MAE,
                        tax_level = input$relabu_bar_taxlev,
                        order_organisms = input$relabu_bar_org_order,
                        sort_by = input$relabu_bar_sort,
                        group_samples = input$relabu_bar_group_samples,
                        group_conditions = input$relabu_bar_group_conditions,
                        sample_conditions = input$relabu_bar_sample_conditions,
                        isolate_samples = input$relabu_bar_sample_iso,
                        discard_samples = input$relabu_bar_sample_dis,
                        show_legend = input$relabu_bar_legend)
    return(p)
})

# Reaction to button pressing
output$relabu_bar_plot <- renderPlotly({
    p <- do_relabu_bar()
    return(p)
})

# Used to dynamically adjust plot height
output$relabu_bar_dynamic_plot <- renderUI({
    height = paste(input$relabu_bar_height, "px", sep="")
    plotlyOutput("relabu_bar_plot", width="800px", height=height)
})

# Return unique organisms for a given tax level
output$relabu_bar_org_order <- renderUI({
    organisms <- unique(as.data.frame(rowData(experiments(vals$MAE)[[1]]))[,input$relabu_bar_taxlev])
    selectizeInput('relabu_bar_org_order', label='Order Organisms', choices=organisms, multiple=TRUE)
})

#
# Relative Abundance Heatmap
#
# Plot when button is pressed
do_relabu_heatmap <- eventReactive(input$relabu_heatmap_plot_btn, {
    p <- relabu_heatmap(MAE = vals$MAE,
                        tax_level = input$relabu_heatmap_taxlev,
                        sort_by = input$relabu_heatmap_sort,
                        sample_conditions = input$relabu_heatmap_conditions,
                        isolate_organisms = input$relabu_heatmap_org_iso,
                        isolate_samples = input$relabu_heatmap_sample_iso,
                        discard_samples = input$relabu_heatmap_sample_dis,
                        log_cpm = input$relabu_heatmap_logcpm)
    return(p)
})

# Reaction to button pressing
output$relabu_heatmap_plot <- renderPlotly({
    p <- do_relabu_heatmap()
    return(p)
})

# Used to dynamically adjust plot height
output$relabu_heatmap_dynamic_plot <- renderUI({
    height = paste(input$relabu_heatmap_height, "px", sep="")
    plotlyOutput("relabu_heatmap_plot", width="800px", height=height)
})

# Return unique organisms for a given tax level
output$relabu_heatmap_org_iso <- renderUI({
    organisms <- unique(as.data.frame(rowData(experiments(vals$MAE)[[1]]))[,input$relabu_heatmap_taxlev])
    selectizeInput('relabu_heatmap_org_iso', label='Isolate Organisms', choices=organisms, multiple=TRUE)
})

#
# Relative Abundance Boxplot
#
# Plot when button is pressed
do_relabu_box <- eventReactive(input$relabu_box_plot_btn, {
    p <- relabu_boxplot(MAE = vals$MAE,
                        tax_level = input$relabu_box_taxlev,
                        condition = input$relabu_box_condition,
                        organisms = input$relabu_box_organisms,
                        datatype = input$relabu_box_datatype)
    return(p)
})

# Reaction to button pressing
output$relabu_box_plot <- renderPlotly({
    p <- do_relabu_box()
    return(p)
})

# Return unique organisms for a given tax level
output$relabu_box_organisms <- renderUI({
    organisms <- unique(as.data.frame(rowData(experiments(vals$MAE)[[1]]))[,input$relabu_box_taxlev])
    selectizeInput('relabu_box_organisms', label='Organisms', choices=organisms, multiple=TRUE)
})