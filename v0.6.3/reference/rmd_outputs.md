# Get document output types from the `rmarkdown` package

Retrieves vector of available document output types from the `rmarkdown`
package, such as `pdf_document`, `html_document`, etc.

## Usage

``` r
rmd_outputs()
```

## Value

`character` vector.

## Examples

``` r
rmd_outputs()
#>  [1] "beamer_presentation"      "context_document"        
#>  [3] "github_document"          "html_document"           
#>  [5] "html_document_base"       "html_extras_for_document"
#>  [7] "ioslides_presentation"    "latex_document"          
#>  [9] "md_document"              "odt_document"            
#> [11] "pdf_document"             "powerpoint_presentation" 
#> [13] "rtf_document"             "slidy_presentation"      
#> [15] "word_document"           
```
