# `teal_card`: An `S3` class for managing `teal` reports

**\[experimental\]**

The `teal_card` `S3` class provides functionality to store, manage,
edit, and adjust report contents. It enables users to create,
manipulate, and serialize report-related data efficiently.

The `teal_card()` function serves two purposes:

1.  When called with a `teal_report` object, it acts as a getter and
    returns the card slot.

2.  When called with other arguments, it creates a new `teal_card`
    object from those arguments.

This function ensures that input is converted to a teal_card object. It
accepts various input types and converts them appropriately.

## Usage

``` r
teal_card(...)

teal_card(x) <- value

as.teal_card(x)

# S3 method for class 'teal_card'
c(...)

# S3 method for class 'teal_card'
x[i]
```

## Arguments

- ...:

  Elements from which `teal_card` will be combined.

- x:

  Object to convert to teal_card

- value:

  (`teal_card`) object to set in the `teal_report`.

- i:

  index specifying elements to extract or replace

## Value

An `S3` `list` of class `teal_card`.

A teal_card object

## Details

The `teal_card` class supports [`c()`](https://rdrr.io/r/base/c.html)
and `x[i]` methods for combining and subsetting elements. However, these
methods only function correctly when the first element is a `teal_card`.

## See also

[`code_chunk()`](https://insightsengineering.github.io/teal.reporter/reference/code_chunk.md),
[`render()`](https://insightsengineering.github.io/teal.reporter/reference/render.md),
`toHTML()`

## Examples

``` r
# create an empty card
report <- teal_card()

# Create a card with content
report <- teal_card(
  "## Headline",
  "This is `iris` table",
  code_chunk("print(iris)", lang = "R"),
  iris
)

# Add elements to the report
report <- c(
  report,
  list("## mtcars Table"),
  code_chunk("print(mtcars)", lang = "R"),
  mtcars
)

# Subset the report to keep only the first two elements
report[1:2]
#> $`516cb264`
#> [1] "## Headline"
#> 
#> $c320f4be
#> [1] "This is `iris` table"
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> list()

# Replace element
report[[1]] <- "## Iris Table"

# Append element
report <- append(report, teal_card("# Awesome Report"), after = 0)
tools::toHTML(report)

  
    Awesome Report

    Iris Table

    This is iris table

    
      
        
          
            
            
              
                
                R
              
            
          
        
        
          
            print(iris)
```


          
    .cl-fcff1010{table-layout:auto;}.cl-fcf0d004{font-family:'Arial';font-size:9pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fcf0d018{font-family:'Arial';font-size:9pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fcf58798{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:8.2pt;padding-right:5.4pt;line-height: 1;background-color:transparent;}.cl-fcf587ac{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fcf587c0{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5.4pt;padding-right:5.4pt;line-height: 1;background-color:transparent;}.cl-fcf587ca{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:0;padding-left:5.4pt;padding-right:5.4pt;line-height: 1;background-color:transparent;}.cl-fcf5bc18{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.5pt solid rgba(102, 102, 102, 1.00);border-left: 0.5pt solid rgba(102, 102, 102, 1.00);border-right: 0.5pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc22{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.5pt solid rgba(102, 102, 102, 1.00);border-left: 0.5pt solid rgba(102, 102, 102, 1.00);border-right: 0.5pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc2c{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.5pt solid rgba(102, 102, 102, 1.00);border-left: 0.5pt solid rgba(102, 102, 102, 1.00);border-right: 0.5pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc2d{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0.5pt solid rgba(102, 102, 102, 1.00);border-left: 0.5pt solid rgba(102, 102, 102, 1.00);border-right: 0.5pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc36{background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.5pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc40{background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc4a{background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc54{background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0.5pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc55{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0.5pt solid rgba(102, 102, 102, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc5e{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc5f{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fcf5bc68{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0.5pt solid rgba(102, 102, 102, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


     
