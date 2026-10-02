# teal_report Class

## Introduction

The `teal_report` class in `teal.reporter` provides a way to create
reproducible documents step by step by adding markdown content alongside
code chunks evaluation.

The `teal_report` class is built on top of
[`teal_data`](https://insightsengineering.github.io/teal.data/latest-tag/articles/teal-data.html),
inheriting all its reproducibility and code-tracking capabilities while
adding reporting-specific functionality through
[`teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md).

This vignette shows you how to build `teal_report` object and add or
remove its content.

## Creating a teal_report

A `teal_report` is an object where developers can add, edit and remove
various content (e.g. markdown content, plots, tables), and evaluate
code chunks. It provides a framework for building reproducible reports
by combining content management with automatic code tracking.

To ensure complete reproducibility, it’s recommended to start with an
empty `teal_report` and build up your data and analysis using
[`within()`](https://rdrr.io/r/base/with.html):

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.reporter`](https://github.com/insightsengineering/teal.reporter)`)`\
`report`` ``<-`` `[`teal_report`](https://insightsengineering.github.io/teal.reporter/reference/teal_report.md)`(``)`

## Adding content to the `teal_report`

### Adding arbitrary markdown content

Think of a `teal_report` as a list of Rmarkdown elements built and
evaluated step by step. Use `teal_card(report)` to access and change
elements of the document. To add a new element in the `teal_card` one
can use `c` method.

\
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`\
`  `[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)``,`\
`  ``"## Document section"``,`\
`  ``"Lorem ipsum dolor sit amet"`\
`)`\
\
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)`

    ## $`7b7c329f`
    ## [1] "## Document section"
    ## 
    ## $e0b0b244
    ## [1] "Lorem ipsum dolor sit amet"
    ## 
    ## attr(,"class")
    ## [1] "teal_card"
    ## attr(,"metadata")
    ## list()

### Adding reproducible code chunks

`teal_report` inherits all methods from `teal_data`. The class supports
[`within()`](https://rdrr.io/r/base/with.html) and
[`teal.code::eval_code()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/eval_code.html),
which execute arbitrary code in its environment. Consider this as
executing a code chunk in an Rmarkdown document. In the same time you
can access objects created during code execution.

\
`report`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``report``, ``{`\
`  ``a`` ``<-`` ``2`\
`}``)`\
`report``$``a`

    ## [1] 2

\
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)`

    ## $`7b7c329f`
    ## [1] "## Document section"
    ## 
    ## $e0b0b244
    ## [1] "Lorem ipsum dolor sit amet"
    ## 
    ## $`2951951b`
    ## [1] "a <- 2"
    ## attr(,"params")
    ## list()
    ## attr(,"lang")
    ## [1] "R"
    ## attr(,"class")
    ## [1] "code_chunk"
    ## 
    ## attr(,"class")
    ## [1] "teal_card"
    ## attr(,"metadata")
    ## list()

In the above chunk of code `a` is created but nothing has been output to
the console nor to the graphic devices. In case one decides to print or
plot, `teal_report` automatically captures outputs, which can be
retrieved with
[`teal.code::get_outputs()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_outputs.html).

\
`report`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``report``, ``{`\
`  ``head_of_iris`` ``<-`` `[`head`](https://rdrr.io/r/utils/head.html)`(``iris``)`\
`  ``head_of_iris`\
`}``)`\
\
`teal.code``::`[`get_outputs`](https://insightsengineering.github.io/teal.code/latest-tag/reference/get_outputs.html)`(``report``)`` ``# returns a list of all outputs`

    ## [[1]]
    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

## Modify `teal_report` content

`teal_report` allows to modify its content. Depending on the needs, one
can add, remove and replace element in the same way as one modifies a
list.

\
`# adding element at the beginning of the document`\
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``"# My report"``)``, `[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)``)`\
\
`# removing code_chunk(s)`\
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)`` ``<-`` `[`Filter`](https://rdrr.io/r/base/funprog.html)`(`\
`  ``function``(``x``)`` ``!`[`inherits`](https://rdrr.io/r/base/class.html)`(``x``, ``"code_chunk"``)``,`\
`  `[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)`\
`)`\
\
`# replace an element`\
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)``[[``1``]``]`` ``<-`` ``"# My report (replaced)"`\
\
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)`

    ## $`3aa693b7`
    ## [1] "# My report (replaced)"
    ## 
    ## $`7b7c329f`
    ## [1] "## Document section"
    ## 
    ## $e0b0b244
    ## [1] "Lorem ipsum dolor sit amet"
    ## 
    ## $c54d820d
    ## [[1]]
    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa
    ## 
    ## attr(,"class")
    ## [1] "chunk_output"
    ## 
    ## attr(,"class")
    ## [1] "teal_card"
    ## attr(,"metadata")
    ## list()

### Document metadata

In Rmarkdown it is possible to specify certain parameters as a YAML
header. `teal_report` allows to specify metadata using
[`metadata()`](https://insightsengineering.github.io/teal.reporter/reference/metadata.md).

\
[`metadata`](https://insightsengineering.github.io/teal.reporter/reference/metadata.md)`(`[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)`(``report``)``)`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  title ``=`` ``"My Document"``,`\
`  author ``=`` ``"NEST"`\
`)`

## Preview report

Report can be previewed in form of HTML markup displayed in viewer of
your IDE. [`tools::toHTML`](https://rdrr.io/r/tools/toHTML.html) returns
`browsable` `shiny.tag` which can be used also in Shiny-application to
preview a report.

\
`tools``::`[`toHTML`](https://rdrr.io/r/tools/toHTML.html)`(``report``)`

## Output teal_report

`teal_report` supports several output formats. `render` for
`teal_report` utilizes
[`rmarkdown::render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
so it supports the same [output
formats](https://pkgs.rstudio.com/rmarkdown/reference/index.html#output-formats)
and arguments.

\
[`render`](https://insightsengineering.github.io/teal.reporter/reference/render.md)`(``report``, output_format ``=`` ``rmarkdown``::`[`pdf_document`](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html)`(``)``, global_knitr ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``fig.width ``=`` ``10``)``)`

## Key Benefits

Using `teal_report` in your modules provides several advantages:

1.  **Reproducibility**: All code is automatically captured via the
    underlying `teal_data` infrastructure
2.  **Consistency**: Standardized way to create reports across modules
3.  **Flexibility**: Easy to add different types of content to reports
4.  **Integration**: Works seamlessly with the teal reporter
    infrastructure
5.  **Code Tracking**: Inherited `eval_code()` functionality ensures all
    computations are reproducible

## Further Reading

For more details on the underlying `teal_data` functionality, see the
[Introduction to
teal.data](https://insightsengineering.github.io/teal.data/latest-tag/articles/teal-data.html).

For more information on the `teal_report` class usage in `teal`, see the
[Adding support for Reproducible Report Documents in
teal](https://insightsengineering.github.io/teal/latest-tag/articles/adding-support-for-reporting.html).
