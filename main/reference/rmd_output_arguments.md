# Get document output arguments from the `rmarkdown` package

Retrieves the arguments for a specified document output type from the
`rmarkdown` package.

## Usage

``` r
rmd_output_arguments(output_name, default_values = FALSE)
```

## Arguments

- output_name:

  (`character`) `rmarkdown` output name.

- default_values:

  (`logical(1)`) if to return a default values for each argument.

## Examples

``` r
rmd_output_arguments("pdf_document")
#>  [1] "toc"                "toc_depth"          "number_sections"   
#>  [4] "fig_width"          "fig_height"         "fig_crop"          
#>  [7] "fig_caption"        "dev"                "df_print"          
#> [10] "highlight"          "template"           "keep_tex"          
#> [13] "keep_md"            "latex_engine"       "citation_package"  
#> [16] "includes"           "md_extensions"      "output_extensions" 
#> [19] "pandoc_args"        "extra_dependencies"
rmd_output_arguments("pdf_document", TRUE)
#> $toc
#> [1] FALSE
#> 
#> $toc_depth
#> [1] 2
#> 
#> $number_sections
#> [1] FALSE
#> 
#> $fig_width
#> [1] 6.5
#> 
#> $fig_height
#> [1] 4.5
#> 
#> $fig_crop
#> [1] "auto"
#> 
#> $fig_caption
#> [1] TRUE
#> 
#> $dev
#> [1] "pdf"
#> 
#> $df_print
#> [1] "default"
#> 
#> $highlight
#> [1] "default"
#> 
#> $template
#> [1] "default"
#> 
#> $keep_tex
#> [1] FALSE
#> 
#> $keep_md
#> [1] FALSE
#> 
#> $latex_engine
#> [1] "pdflatex"
#> 
#> $citation_package
#> c("default", "natbib", "biblatex")
#> 
#> $includes
#> NULL
#> 
#> $md_extensions
#> NULL
#> 
#> $output_extensions
#> NULL
#> 
#> $pandoc_args
#> NULL
#> 
#> $extra_dependencies
#> NULL
#> 
```
