# Reportable Shiny Application

## Introduction

This vignette demonstrates how to create a fully functional reportable
Shiny application using `teal.reporter`. While `teal` provides automatic
reporting functionality (as described in the [Getting
Started](https://insightsengineering.github.io/teal.reporter/articles/getting-started-with-teal-reporter.md)
vignette), this guide shows how to integrate reporting capabilities into
standalone Shiny applications or when you need more control over the
reporting process.

Before diving into this vignette, we recommend familiarizing yourself
with:

- [Getting Started with
  teal.reporter](https://insightsengineering.github.io/teal.reporter/articles/getting-started-with-teal-reporter.md) -
  Overview of the package and its integration with `teal`
- [teal_report
  Class](https://insightsengineering.github.io/teal.reporter/articles/teal-report-class.md) -
  Understanding `teal_report` and `teal_card` objects, which are the
  foundation for creating reproducible report content

## Reporter Modules

`teal.reporter` provides a complete suite of Shiny modules for report
management in applications:

- **Add card button** - Add views/cards to a report
- **Preview report button** - Preview the report content in a modal
- **Download report button** - Download the report in various formats
- **Load report button** - Upload previously saved reports
- **Reset report button** - Clear all cards from the report

This vignette demonstrates how to integrate all five modules into a
Shiny application. The key steps are:

1.  Add the UI components of all modules to your app’s interface.
2.  Initialize a `Reporter` instance.
3.  Build reproducible `teal_report` object by executing a code and
    adding arbitrary markdown elements.
4.  Extract `teal_card` object containing the content to be added to
    reports.
5.  Invoke the server functions with the `Reporter` instance and
    `teal_card` reactive.

The code added to introduce the reporter functionality is wrapped in
`### REPORTER` code blocks.

First, load the required packages:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`shiny`](https://shiny.posit.co/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.reporter`](https://github.com/insightsengineering/teal.reporter)`)`

A simple `shiny` app with all reporter modules integrated:

\
`ui`` ``<-`` ``bslib``::`[`page_fluid`](https://rstudio.github.io/bslib/reference/page.html)`(`\
`  ``bslib``::`[`card`](https://rstudio.github.io/bslib/reference/card.html)`(`\
`    ``bslib``::`[`card_header`](https://rstudio.github.io/bslib/reference/card_body.html)`(``"Reporter Modules Demo"``)``,`\
`    ``bslib``::`[`layout_sidebar`](https://rstudio.github.io/bslib/reference/sidebar.html)`(`\
`      sidebar ``=`` ``bslib``::`[`sidebar`](https://rstudio.github.io/bslib/reference/sidebar.html)`(`\
`        ``### REPORTER`\
`        ``teal.reporter``::`[`add_card_button_ui`](https://insightsengineering.github.io/teal.reporter/reference/add_card_button.md)`(``"add_card"``, label ``=`` ``"Add Card"``)``,`\
`        ``teal.reporter``::`[`preview_report_button_ui`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer.md)`(``"preview"``)``,`\
`        ``teal.reporter``::`[`download_report_button_ui`](https://insightsengineering.github.io/teal.reporter/reference/download_report_button.md)`(``"download"``, label ``=`` ``"Download"``)``,`\
`        ``teal.reporter``::`[`report_load_ui`](https://insightsengineering.github.io/teal.reporter/reference/load_report_button.md)`(``"load"``, label ``=`` ``"Load"``)``,`\
`        ``teal.reporter``::`[`reset_report_button_ui`](https://insightsengineering.github.io/teal.reporter/reference/reset_report_button.md)`(``"reset"``, label ``=`` ``"Reset"``)``,`\
`        ``###`\
`      ``)``,`\
`      ``bslib``::`[`card`](https://rstudio.github.io/bslib/reference/card.html)`(`\
`        ``bslib``::`[`card_header`](https://rstudio.github.io/bslib/reference/card_body.html)`(``"Summary Statistics by Cylinder"``)``,`\
`        `[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)`(`\
`          ``"stat"``,`\
`          label ``=`` ``"Select Statistic:"``,`\
`          choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"mean"``, ``"median"``, ``"sd"``)``,`\
`          selected ``=`` ``"mean"`\
`        ``)``,`\
`        `[`tableOutput`](https://rdrr.io/pkg/shiny/man/renderTable.html)`(``"table"``)`\
`      ``)`\
`    ``)`\
`  ``)`\
`)`\
\
`server`` ``<-`` ``function``(``input``, ``output``, ``session``)`` ``{`\
`  ``# Here we start with empty teal_report object`\
`  ``data`` ``<-`` `[`teal_report`](https://insightsengineering.github.io/teal.reporter/reference/teal_report.md)`(``)`\
\
`  ``# Create summary table`\
`  ``with_summary_table`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(``{`\
`    `[`req`](https://rdrr.io/pkg/shiny/man/req.html)`(``input``$``stat``)`\
`    ``# Add section's header with dynamic content`\
`    `[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``data``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``data``)``, `[`paste`](https://rdrr.io/r/base/paste.html)`(``"## Summary Statistics:"``, ``input``$``stat``)``)`\
\
`    ``# Execute dynamically generated code (this stores evaluated code-chunk and its output)`\
`    `[`within`](https://rdrr.io/r/base/with.html)`(`\
`      ``data``,`\
`      expr ``=`` ``{`\
`        ``summary_table`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(`\
`          cyl ``=`` `[`sort`](https://rdrr.io/r/base/sort.html)`(`[`unique`](https://rdrr.io/r/base/unique.html)`(``mtcars``$``cyl``)``)``,`\
`          mpg ``=`` `[`tapply`](https://rdrr.io/r/base/tapply.html)`(``mtcars``$``mpg``, ``mtcars``$``cyl``, ``stat_fun``)``,`\
`          hp ``=`` `[`tapply`](https://rdrr.io/r/base/tapply.html)`(``mtcars``$``hp``, ``mtcars``$``cyl``, ``stat_fun``)``,`\
`          wt ``=`` `[`tapply`](https://rdrr.io/r/base/tapply.html)`(``mtcars``$``wt``, ``mtcars``$``cyl``, ``stat_fun``)`\
`        ``)`\
`        ``summary_table`\
`      ``}``,`\
`      stat_fun ``=`` `[`as.name`](https://rdrr.io/r/base/name.html)`(``input``$``stat``)`\
`    ``)`\
`  ``}``)`\
\
`  ``output``$``table`` ``<-`` `[`renderTable`](https://rdrr.io/pkg/shiny/man/renderTable.html)`(``{`\
`    ``` # extract `summary_table` from teal_report object ``\
`    ``teal.code``::`[`get_outputs`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_outputs.html)`(``with_summary_table``(``)``)``[[``1``]``]`\
`  ``}``)`\
\
`  ``### REPORTER`\
`  ``reporter`` ``<-`` `[`Reporter`](https://insightsengineering.github.io/teal.reporter/reference/Reporter.md)`$``new``(``)`\
`  ``reporter``$``set_id``(``"reporter_demo"``)`\
\
`  ``# extract teal_card object and hand it over to add_card_button_srv`\
`  ``card_r`` ``<-`` `[`reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)`(`[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``with_summary_table``(``)``)``)`\
`  ``teal.reporter``::`[`add_card_button_srv`](https://insightsengineering.github.io/teal.reporter/reference/add_card_button.md)`(``"add_card"``, reporter ``=`` ``reporter``, card_fun ``=`` ``card_r``)`\
`  ``teal.reporter``::`[`preview_report_button_srv`](https://insightsengineering.github.io/teal.reporter/reference/reporter_previewer.md)`(``"preview"``, ``reporter``)`\
`  ``teal.reporter``::`[`download_report_button_srv`](https://insightsengineering.github.io/teal.reporter/reference/download_report_button.md)`(``"download"``, ``reporter``)`\
`  ``teal.reporter``::`[`report_load_srv`](https://insightsengineering.github.io/teal.reporter/reference/load_report_button.md)`(``"load"``, ``reporter``)`\
`  ``teal.reporter``::`[`reset_report_button_srv`](https://insightsengineering.github.io/teal.reporter/reference/reset_report_button.md)`(``"reset"``, ``reporter``)`\
`  ``###`\
`}`\
\
[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)`(``ui ``=`` ``ui``, server ``=`` ``server``)`

## Module Overview

This example demonstrates all five reporter modules working together:

1.  **Add Card Button** - Allows users to add the current view to the
    report. When clicked, it opens a modal where users can provide a
    card name and optional comment. The `card_fun` reactive creates a
    `teal_card` containing the current tab’s content (plot or table)
    along with the corresponding R code.

2.  **Preview Report Button** - Opens a modal showing all added cards in
    an accordion view. Users can preview, edit, reorder, or remove
    individual cards before downloading. The badge shows the current
    number of cards in the report.

3.  **Download Report Button** - Generates and downloads the report as a
    zip file. Users can customize the report metadata (author, title,
    date), choose the output format (HTML, PDF, Word, PowerPoint),
    include a table of contents, and optionally include R code in the
    output.

4.  **Load Report Button** - Allows users to upload and restore a
    previously saved report (zip file). This enables users to continue
    working on reports across sessions.

5.  **Reset Report Button** - Clears all cards from the current report.
    The button is disabled when the report is empty and prompts for
    confirmation before resetting.

All modules share the same `Reporter` instance, ensuring synchronized
state management across the application.
