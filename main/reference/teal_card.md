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
#> $be2fde43
#> [1] "## Headline"
#> 
#> $f43772bf
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

\
      

\
\

|     | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width | Species    |
|-----|--------------|-------------|--------------|-------------|------------|
| 1   | 5.1          | 3.5         | 1.4          | 0.2         | setosa     |
| 2   | 4.9          | 3           | 1.4          | 0.2         | setosa     |
| 3   | 4.7          | 3.2         | 1.3          | 0.2         | setosa     |
| 4   | 4.6          | 3.1         | 1.5          | 0.2         | setosa     |
| 5   | 5            | 3.6         | 1.4          | 0.2         | setosa     |
| 6   | 5.4          | 3.9         | 1.7          | 0.4         | setosa     |
| 7   | 4.6          | 3.4         | 1.4          | 0.3         | setosa     |
| 8   | 5            | 3.4         | 1.5          | 0.2         | setosa     |
| 9   | 4.4          | 2.9         | 1.4          | 0.2         | setosa     |
| 10  | 4.9          | 3.1         | 1.5          | 0.1         | setosa     |
| 11  | 5.4          | 3.7         | 1.5          | 0.2         | setosa     |
| 12  | 4.8          | 3.4         | 1.6          | 0.2         | setosa     |
| 13  | 4.8          | 3           | 1.4          | 0.1         | setosa     |
| 14  | 4.3          | 3           | 1.1          | 0.1         | setosa     |
| 15  | 5.8          | 4           | 1.2          | 0.2         | setosa     |
| 16  | 5.7          | 4.4         | 1.5          | 0.4         | setosa     |
| 17  | 5.4          | 3.9         | 1.3          | 0.4         | setosa     |
| 18  | 5.1          | 3.5         | 1.4          | 0.3         | setosa     |
| 19  | 5.7          | 3.8         | 1.7          | 0.3         | setosa     |
| 20  | 5.1          | 3.8         | 1.5          | 0.3         | setosa     |
| 21  | 5.4          | 3.4         | 1.7          | 0.2         | setosa     |
| 22  | 5.1          | 3.7         | 1.5          | 0.4         | setosa     |
| 23  | 4.6          | 3.6         | 1            | 0.2         | setosa     |
| 24  | 5.1          | 3.3         | 1.7          | 0.5         | setosa     |
| 25  | 4.8          | 3.4         | 1.9          | 0.2         | setosa     |
| 26  | 5            | 3           | 1.6          | 0.2         | setosa     |
| 27  | 5            | 3.4         | 1.6          | 0.4         | setosa     |
| 28  | 5.2          | 3.5         | 1.5          | 0.2         | setosa     |
| 29  | 5.2          | 3.4         | 1.4          | 0.2         | setosa     |
| 30  | 4.7          | 3.2         | 1.6          | 0.2         | setosa     |
| 31  | 4.8          | 3.1         | 1.6          | 0.2         | setosa     |
| 32  | 5.4          | 3.4         | 1.5          | 0.4         | setosa     |
| 33  | 5.2          | 4.1         | 1.5          | 0.1         | setosa     |
| 34  | 5.5          | 4.2         | 1.4          | 0.2         | setosa     |
| 35  | 4.9          | 3.1         | 1.5          | 0.2         | setosa     |
| 36  | 5            | 3.2         | 1.2          | 0.2         | setosa     |
| 37  | 5.5          | 3.5         | 1.3          | 0.2         | setosa     |
| 38  | 4.9          | 3.6         | 1.4          | 0.1         | setosa     |
| 39  | 4.4          | 3           | 1.3          | 0.2         | setosa     |
| 40  | 5.1          | 3.4         | 1.5          | 0.2         | setosa     |
| 41  | 5            | 3.5         | 1.3          | 0.3         | setosa     |
| 42  | 4.5          | 2.3         | 1.3          | 0.3         | setosa     |
| 43  | 4.4          | 3.2         | 1.3          | 0.2         | setosa     |
| 44  | 5            | 3.5         | 1.6          | 0.6         | setosa     |
| 45  | 5.1          | 3.8         | 1.9          | 0.4         | setosa     |
| 46  | 4.8          | 3           | 1.4          | 0.3         | setosa     |
| 47  | 5.1          | 3.8         | 1.6          | 0.2         | setosa     |
| 48  | 4.6          | 3.2         | 1.4          | 0.2         | setosa     |
| 49  | 5.3          | 3.7         | 1.5          | 0.2         | setosa     |
| 50  | 5            | 3.3         | 1.4          | 0.2         | setosa     |
| 51  | 7            | 3.2         | 4.7          | 1.4         | versicolor |
| 52  | 6.4          | 3.2         | 4.5          | 1.5         | versicolor |
| 53  | 6.9          | 3.1         | 4.9          | 1.5         | versicolor |
| 54  | 5.5          | 2.3         | 4            | 1.3         | versicolor |
| 55  | 6.5          | 2.8         | 4.6          | 1.5         | versicolor |
| 56  | 5.7          | 2.8         | 4.5          | 1.3         | versicolor |
| 57  | 6.3          | 3.3         | 4.7          | 1.6         | versicolor |
| 58  | 4.9          | 2.4         | 3.3          | 1           | versicolor |
| 59  | 6.6          | 2.9         | 4.6          | 1.3         | versicolor |
| 60  | 5.2          | 2.7         | 3.9          | 1.4         | versicolor |
| 61  | 5            | 2           | 3.5          | 1           | versicolor |
| 62  | 5.9          | 3           | 4.2          | 1.5         | versicolor |
| 63  | 6            | 2.2         | 4            | 1           | versicolor |
| 64  | 6.1          | 2.9         | 4.7          | 1.4         | versicolor |
| 65  | 5.6          | 2.9         | 3.6          | 1.3         | versicolor |
| 66  | 6.7          | 3.1         | 4.4          | 1.4         | versicolor |
| 67  | 5.6          | 3           | 4.5          | 1.5         | versicolor |
| 68  | 5.8          | 2.7         | 4.1          | 1           | versicolor |
| 69  | 6.2          | 2.2         | 4.5          | 1.5         | versicolor |
| 70  | 5.6          | 2.5         | 3.9          | 1.1         | versicolor |
| 71  | 5.9          | 3.2         | 4.8          | 1.8         | versicolor |
| 72  | 6.1          | 2.8         | 4            | 1.3         | versicolor |
| 73  | 6.3          | 2.5         | 4.9          | 1.5         | versicolor |
| 74  | 6.1          | 2.8         | 4.7          | 1.2         | versicolor |
| 75  | 6.4          | 2.9         | 4.3          | 1.3         | versicolor |
| 76  | 6.6          | 3           | 4.4          | 1.4         | versicolor |
| 77  | 6.8          | 2.8         | 4.8          | 1.4         | versicolor |
| 78  | 6.7          | 3           | 5            | 1.7         | versicolor |
| 79  | 6            | 2.9         | 4.5          | 1.5         | versicolor |
| 80  | 5.7          | 2.6         | 3.5          | 1           | versicolor |
| 81  | 5.5          | 2.4         | 3.8          | 1.1         | versicolor |
| 82  | 5.5          | 2.4         | 3.7          | 1           | versicolor |
| 83  | 5.8          | 2.7         | 3.9          | 1.2         | versicolor |
| 84  | 6            | 2.7         | 5.1          | 1.6         | versicolor |
| 85  | 5.4          | 3           | 4.5          | 1.5         | versicolor |
| 86  | 6            | 3.4         | 4.5          | 1.6         | versicolor |
| 87  | 6.7          | 3.1         | 4.7          | 1.5         | versicolor |
| 88  | 6.3          | 2.3         | 4.4          | 1.3         | versicolor |
| 89  | 5.6          | 3           | 4.1          | 1.3         | versicolor |
| 90  | 5.5          | 2.5         | 4            | 1.3         | versicolor |
| 91  | 5.5          | 2.6         | 4.4          | 1.2         | versicolor |
| 92  | 6.1          | 3           | 4.6          | 1.4         | versicolor |
| 93  | 5.8          | 2.6         | 4            | 1.2         | versicolor |
| 94  | 5            | 2.3         | 3.3          | 1           | versicolor |
| 95  | 5.6          | 2.7         | 4.2          | 1.3         | versicolor |
| 96  | 5.7          | 3           | 4.2          | 1.2         | versicolor |
| 97  | 5.7          | 2.9         | 4.2          | 1.3         | versicolor |
| 98  | 6.2          | 2.9         | 4.3          | 1.3         | versicolor |
| 99  | 5.1          | 2.5         | 3            | 1.1         | versicolor |
| 100 | 5.7          | 2.8         | 4.1          | 1.3         | versicolor |
| 101 | 6.3          | 3.3         | 6            | 2.5         | virginica  |
| 102 | 5.8          | 2.7         | 5.1          | 1.9         | virginica  |
| 103 | 7.1          | 3           | 5.9          | 2.1         | virginica  |
| 104 | 6.3          | 2.9         | 5.6          | 1.8         | virginica  |
| 105 | 6.5          | 3           | 5.8          | 2.2         | virginica  |
| 106 | 7.6          | 3           | 6.6          | 2.1         | virginica  |
| 107 | 4.9          | 2.5         | 4.5          | 1.7         | virginica  |
| 108 | 7.3          | 2.9         | 6.3          | 1.8         | virginica  |
| 109 | 6.7          | 2.5         | 5.8          | 1.8         | virginica  |
| 110 | 7.2          | 3.6         | 6.1          | 2.5         | virginica  |
| 111 | 6.5          | 3.2         | 5.1          | 2           | virginica  |
| 112 | 6.4          | 2.7         | 5.3          | 1.9         | virginica  |
| 113 | 6.8          | 3           | 5.5          | 2.1         | virginica  |
| 114 | 5.7          | 2.5         | 5            | 2           | virginica  |
| 115 | 5.8          | 2.8         | 5.1          | 2.4         | virginica  |
| 116 | 6.4          | 3.2         | 5.3          | 2.3         | virginica  |
| 117 | 6.5          | 3           | 5.5          | 1.8         | virginica  |
| 118 | 7.7          | 3.8         | 6.7          | 2.2         | virginica  |
| 119 | 7.7          | 2.6         | 6.9          | 2.3         | virginica  |
| 120 | 6            | 2.2         | 5            | 1.5         | virginica  |
| 121 | 6.9          | 3.2         | 5.7          | 2.3         | virginica  |
| 122 | 5.6          | 2.8         | 4.9          | 2           | virginica  |
| 123 | 7.7          | 2.8         | 6.7          | 2           | virginica  |
| 124 | 6.3          | 2.7         | 4.9          | 1.8         | virginica  |
| 125 | 6.7          | 3.3         | 5.7          | 2.1         | virginica  |
| 126 | 7.2          | 3.2         | 6            | 1.8         | virginica  |
| 127 | 6.2          | 2.8         | 4.8          | 1.8         | virginica  |
| 128 | 6.1          | 3           | 4.9          | 1.8         | virginica  |
| 129 | 6.4          | 2.8         | 5.6          | 2.1         | virginica  |
| 130 | 7.2          | 3           | 5.8          | 1.6         | virginica  |
| 131 | 7.4          | 2.8         | 6.1          | 1.9         | virginica  |
| 132 | 7.9          | 3.8         | 6.4          | 2           | virginica  |
| 133 | 6.4          | 2.8         | 5.6          | 2.2         | virginica  |
| 134 | 6.3          | 2.8         | 5.1          | 1.5         | virginica  |
| 135 | 6.1          | 2.6         | 5.6          | 1.4         | virginica  |
| 136 | 7.7          | 3           | 6.1          | 2.3         | virginica  |
| 137 | 6.3          | 3.4         | 5.6          | 2.4         | virginica  |
| 138 | 6.4          | 3.1         | 5.5          | 1.8         | virginica  |
| 139 | 6            | 3           | 4.8          | 1.8         | virginica  |
| 140 | 6.9          | 3.1         | 5.4          | 2.1         | virginica  |
| 141 | 6.7          | 3.1         | 5.6          | 2.4         | virginica  |
| 142 | 6.9          | 3.1         | 5.1          | 2.3         | virginica  |
| 143 | 5.8          | 2.7         | 5.1          | 1.9         | virginica  |
| 144 | 6.8          | 3.2         | 5.9          | 2.3         | virginica  |
| 145 | 6.7          | 3.3         | 5.7          | 2.5         | virginica  |
| 146 | 6.7          | 3           | 5.2          | 2.3         | virginica  |
| 147 | 6.3          | 2.5         | 5            | 1.9         | virginica  |
| 148 | 6.5          | 3           | 5.2          | 2           | virginica  |
| 149 | 6.2          | 3.4         | 5.4          | 2.3         | virginica  |
| 150 | 5.9          | 3           | 5.1          | 1.8         | virginica  |

\

\
    

## mtcars Table

R

``` R
print(mtcars)
```

\
      

\
\

|                     | mpg  | cyl | disp  | hp  | drat | wt    | qsec  | vs  | am  | gear | carb |
|---------------------|------|-----|-------|-----|------|-------|-------|-----|-----|------|------|
| Mazda RX4           | 21   | 6   | 160   | 110 | 3.9  | 2.62  | 16.46 | 0   | 1   | 4    | 4    |
| Mazda RX4 Wag       | 21   | 6   | 160   | 110 | 3.9  | 2.875 | 17.02 | 0   | 1   | 4    | 4    |
| Datsun 710          | 22.8 | 4   | 108   | 93  | 3.85 | 2.32  | 18.61 | 1   | 1   | 4    | 1    |
| Hornet 4 Drive      | 21.4 | 6   | 258   | 110 | 3.08 | 3.215 | 19.44 | 1   | 0   | 3    | 1    |
| Hornet Sportabout   | 18.7 | 8   | 360   | 175 | 3.15 | 3.44  | 17.02 | 0   | 0   | 3    | 2    |
| Valiant             | 18.1 | 6   | 225   | 105 | 2.76 | 3.46  | 20.22 | 1   | 0   | 3    | 1    |
| Duster 360          | 14.3 | 8   | 360   | 245 | 3.21 | 3.57  | 15.84 | 0   | 0   | 3    | 4    |
| Merc 240D           | 24.4 | 4   | 146.7 | 62  | 3.69 | 3.19  | 20    | 1   | 0   | 4    | 2    |
| Merc 230            | 22.8 | 4   | 140.8 | 95  | 3.92 | 3.15  | 22.9  | 1   | 0   | 4    | 2    |
| Merc 280            | 19.2 | 6   | 167.6 | 123 | 3.92 | 3.44  | 18.3  | 1   | 0   | 4    | 4    |
| Merc 280C           | 17.8 | 6   | 167.6 | 123 | 3.92 | 3.44  | 18.9  | 1   | 0   | 4    | 4    |
| Merc 450SE          | 16.4 | 8   | 275.8 | 180 | 3.07 | 4.07  | 17.4  | 0   | 0   | 3    | 3    |
| Merc 450SL          | 17.3 | 8   | 275.8 | 180 | 3.07 | 3.73  | 17.6  | 0   | 0   | 3    | 3    |
| Merc 450SLC         | 15.2 | 8   | 275.8 | 180 | 3.07 | 3.78  | 18    | 0   | 0   | 3    | 3    |
| Cadillac Fleetwood  | 10.4 | 8   | 472   | 205 | 2.93 | 5.25  | 17.98 | 0   | 0   | 3    | 4    |
| Lincoln Continental | 10.4 | 8   | 460   | 215 | 3    | 5.424 | 17.82 | 0   | 0   | 3    | 4    |
| Chrysler Imperial   | 14.7 | 8   | 440   | 230 | 3.23 | 5.345 | 17.42 | 0   | 0   | 3    | 4    |
| Fiat 128            | 32.4 | 4   | 78.7  | 66  | 4.08 | 2.2   | 19.47 | 1   | 1   | 4    | 1    |
| Honda Civic         | 30.4 | 4   | 75.7  | 52  | 4.93 | 1.615 | 18.52 | 1   | 1   | 4    | 2    |
| Toyota Corolla      | 33.9 | 4   | 71.1  | 65  | 4.22 | 1.835 | 19.9  | 1   | 1   | 4    | 1    |
| Toyota Corona       | 21.5 | 4   | 120.1 | 97  | 3.7  | 2.465 | 20.01 | 1   | 0   | 3    | 1    |
| Dodge Challenger    | 15.5 | 8   | 318   | 150 | 2.76 | 3.52  | 16.87 | 0   | 0   | 3    | 2    |
| AMC Javelin         | 15.2 | 8   | 304   | 150 | 3.15 | 3.435 | 17.3  | 0   | 0   | 3    | 2    |
| Camaro Z28          | 13.3 | 8   | 350   | 245 | 3.73 | 3.84  | 15.41 | 0   | 0   | 3    | 4    |
| Pontiac Firebird    | 19.2 | 8   | 400   | 175 | 3.08 | 3.845 | 17.05 | 0   | 0   | 3    | 2    |
| Fiat X1-9           | 27.3 | 4   | 79    | 66  | 4.08 | 1.935 | 18.9  | 1   | 1   | 4    | 1    |
| Porsche 914-2       | 26   | 4   | 120.3 | 91  | 4.43 | 2.14  | 16.7  | 0   | 1   | 5    | 2    |
| Lotus Europa        | 30.4 | 4   | 95.1  | 113 | 3.77 | 1.513 | 16.9  | 1   | 1   | 5    | 2    |
| Ford Pantera L      | 15.8 | 8   | 351   | 264 | 4.22 | 3.17  | 14.5  | 0   | 1   | 5    | 4    |
| Ferrari Dino        | 19.7 | 6   | 145   | 175 | 3.62 | 2.77  | 15.5  | 0   | 1   | 5    | 6    |
| Maserati Bora       | 15   | 8   | 301   | 335 | 3.54 | 3.57  | 14.6  | 0   | 1   | 5    | 8    |
| Volvo 142E          | 21.4 | 4   | 121   | 109 | 4.11 | 2.78  | 18.6  | 1   | 1   | 4    | 2    |

\

\
    

if ([interactive](https://rdrr.io/r/base/interactive.html)()) {
[render](https://insightsengineering.github.io/teal.reporter/reference/render.md)(report,
output_format =
rmarkdown::[pdf_document](https://pkgs.rstudio.com/rmarkdown/reference/pdf_document.html)())
}
