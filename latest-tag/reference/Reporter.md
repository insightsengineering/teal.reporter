# `Reporter`: An `R6` class for managing reports

This `R6` class is designed to store and manage reports, facilitating
the creation, manipulation, and serialization of report-related data. It
supports both `ReportCard` and `teal_card` objects, allowing flexibility
in the types of reports that can be stored and managed.

## Note

if Report has an id when converting to JSON then It will be compared to
the currently available one.

if Report has an id when converting to JSON then It will be compared to
the currently available one.

## Methods

### Public methods

- [`Reporter$new()`](#method-Reporter-new)

- [`Reporter$append_cards()`](#method-Reporter-append_cards)

- [`Reporter$reorder_cards()`](#method-Reporter-reorder_cards)

- [`Reporter$replace_card()`](#method-Reporter-replace_card)

- [`Reporter$get_cards()`](#method-Reporter-get_cards)

- [`Reporter$get_blocks()`](#method-Reporter-get_blocks)

- [`Reporter$reset()`](#method-Reporter-reset)

- [`Reporter$remove_cards()`](#method-Reporter-remove_cards)

- [`Reporter$get_metadata()`](#method-Reporter-get_metadata)

- [`Reporter$append_metadata()`](#method-Reporter-append_metadata)

- [`Reporter$from_reporter()`](#method-Reporter-from_reporter)

- [`Reporter$to_list()`](#method-Reporter-to_list)

- [`Reporter$write_figures()`](#method-Reporter-write_figures)

- [`Reporter$from_list()`](#method-Reporter-from_list)

- [`Reporter$to_jsondir()`](#method-Reporter-to_jsondir)

- [`Reporter$from_jsondir()`](#method-Reporter-from_jsondir)

- [`Reporter$set_id()`](#method-Reporter-set_id)

- [`Reporter$open_previewer()`](#method-Reporter-open_previewer)

- [`Reporter$get_cached_html()`](#method-Reporter-get_cached_html)

- [`Reporter$get_id()`](#method-Reporter-get_id)

- [`Reporter$set_template()`](#method-Reporter-set_template)

- [`Reporter$get_template()`](#method-Reporter-get_template)

- [`Reporter$clone()`](#method-Reporter-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Initialize a `Reporter` object.

#### Usage

    Reporter$new()

#### Returns

Object of class `Reporter`, invisibly.

#### Examples

    reporter <- Reporter$new()

------------------------------------------------------------------------

### Method `append_cards()`

Append one or more `ReportCard` or `teal_card` objects to the
`Reporter`.

#### Usage

    Reporter$append_cards(cards)

#### Arguments

- `cards`:

  (`ReportCard` or `teal_card`) or a list of such objects

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `reorder_cards()`

Reorders `teal_card` objects in `Reporter`.

Reorders `teal_card` objects in `Reporter`.

#### Usage

    Reporter$reorder_cards(new_order)

#### Arguments

- `new_order`:

  `character` vector with names of `teal_card` objects to be set in this
  order.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `replace_card()`

Sets `ReportCard` or `teal_card` content.

#### Usage

    Reporter$replace_card(card, card_id)

#### Arguments

- `card`:

  The new object (`ReportCard` or `teal_card`) to replace the existing
  one.

- `card_id`:

  (`character(1)`) the unique id of the card to be replaced.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `get_cards()`

Retrieves all `teal_card` objects contained in `Reporter`.

#### Usage

    Reporter$get_cards()

#### Returns

A (`list`) of
[`teal_card`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
objects.

------------------------------------------------------------------------

### Method `get_blocks()`

Compiles and returns all content blocks from the `teal_card` objects in
the `Reporter`.

#### Usage

    Reporter$get_blocks(sep = "\\newpage")

#### Arguments

- `sep`:

  An optional separator to insert between each content block. Default is
  a `\n\\newpage\n` markdown.

#### Returns

[`list()`](https://rdrr.io/r/base/list.html) of `teal_card`

------------------------------------------------------------------------

### Method `reset()`

Resets the `Reporter`, removing all `teal_card` objects and metadata.

#### Usage

    Reporter$reset()

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `remove_cards()`

Removes specific `teal_card` objects from the `Reporter` by their
indices.

#### Usage

    Reporter$remove_cards(ids = NULL)

#### Arguments

- `ids`:

  (`integer`, `character`) the indexes of cards (either name)

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `get_metadata()`

Get the metadata associated with this `Reporter`.

#### Usage

    Reporter$get_metadata()

#### Returns

`named list` of metadata to be appended.

#### Examples

    reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
    reporter$get_metadata()

------------------------------------------------------------------------

### Method `append_metadata()`

Appends metadata to this `Reporter`.

#### Usage

    Reporter$append_metadata(meta)

#### Arguments

- `meta`:

  (`named list`) of metadata to be appended.

#### Returns

`self`, invisibly.

#### Examples

    reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
    reporter$get_metadata()

------------------------------------------------------------------------

### Method `from_reporter()`

Reinitializes a `Reporter` instance by copying the report cards and
metadata from another `Reporter`.

#### Usage

    Reporter$from_reporter(reporter)

#### Arguments

- `reporter`:

  (`Reporter`) instance to copy from.

#### Returns

invisibly self

#### Examples

    reporter <- Reporter$new()
    reporter$from_reporter(reporter)

------------------------------------------------------------------------

### Method `to_list()`

Convert a `Reporter` to a list and transfer any associated files to
specified directory.

#### Usage

    Reporter$to_list(output_dir)

#### Arguments

- `output_dir`:

  (`character(1)`) a path to the directory where files will be copied.

#### Returns

`named list` representing the `Reporter` instance, including version
information, metadata, and report cards.

#### Examples

    reporter <- Reporter$new()
    tmp_dir <- file.path(tempdir(), "testdir")
    dir.create(tmp_dir)
    reporter$to_list(tmp_dir)

------------------------------------------------------------------------

### Method `write_figures()`

Extracts and saves all figure elements from the `teal_card` objects in
the `Reporter` to a specified directory.

#### Usage

    Reporter$write_figures(output_dir, sub_directory = "figures")

#### Arguments

- `output_dir`:

  (`character(1)`) a path to the directory where figures will be saved.

- `sub_directory`:

  (`character(1)`) a sub-directory within `output_dir` to save figures.

------------------------------------------------------------------------

### Method `from_list()`

Reinitializes a `Reporter` from a list representation and associated
files in a specified directory.

#### Usage

    Reporter$from_list(rlist, output_dir)

#### Arguments

- `rlist`:

  (`named list`) representing a `Reporter` instance.

- `output_dir`:

  (`character(1)`) a path to the directory from which files will be
  copied.

#### Returns

`self`, invisibly.

#### Examples

    reporter <- Reporter$new()
    tmp_dir <- file.path(tempdir(), "testdir")
    unlink(tmp_dir, recursive = TRUE)
    dir.create(tmp_dir)
    reporter$from_list(reporter$to_list(tmp_dir), tmp_dir)

------------------------------------------------------------------------

### Method `to_jsondir()`

Serializes the `Reporter` to a `JSON` file and copies any associated
files to a specified directory.

#### Usage

    Reporter$to_jsondir(output_dir)

#### Arguments

- `output_dir`:

  (`character(1)`) a path to the directory where files will be copied,
  `JSON` and statics.

#### Returns

`output_dir` argument.

#### Examples

    reporter <- Reporter$new()
    tmp_dir <- file.path(tempdir(), "jsondir")
    dir.create(tmp_dir)
    reporter$to_jsondir(tmp_dir)

------------------------------------------------------------------------

### Method `from_jsondir()`

Reinitializes a `Reporter` from a `JSON ` file and files in a specified
directory.

#### Usage

    Reporter$from_jsondir(output_dir)

#### Arguments

- `output_dir`:

  (`character(1)`) a path to the directory with files, `JSON` and
  statics.

#### Returns

`self`, invisibly.

#### Examples

    reporter <- Reporter$new()
    tmp_dir <- file.path(tempdir(), "jsondir")
    dir.create(tmp_dir)
    unlink(list.files(tmp_dir, recursive = TRUE))
    reporter$to_jsondir(tmp_dir)
    reporter$from_jsondir(tmp_dir)

------------------------------------------------------------------------

### Method `set_id()`

Set the `Reporter` id Optionally add id to a `Reporter` which will be
compared when it is rebuilt from a list. The id is added to the
downloaded file name.

#### Usage

    Reporter$set_id(id)

#### Arguments

- `id`:

  (`character(1)`) a Report id.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method `open_previewer()`

Get or set the reactive trigger to open the previewer modal.

#### Usage

    Reporter$open_previewer(val)

#### Arguments

- `val`:

  value to the passed to the reactive trigger.

#### Returns

`reactiveVal` value

------------------------------------------------------------------------

### Method `get_cached_html()`

Get cached HTML for a specific `teal_card` by its id.

#### Usage

    Reporter$get_cached_html(card_id)

#### Arguments

- `card_id`:

  (`character(1)`) the unique id of the card.

------------------------------------------------------------------------

### Method `get_id()`

Get the `Reporter` id

#### Usage

    Reporter$get_id()

#### Returns

`character(1)` the `Reporter` id.

------------------------------------------------------------------------

### Method `set_template()`

Set template function for `teal_card` Set a function that is called on
every report content (of class `teal_card`) added through
`$append_cards`

#### Usage

    Reporter$set_template(template)

#### Arguments

- `template`:

  (`function`) a template function.

#### Returns

`self`, invisibly.

#### Examples


    reporter <- teal.reporter::Reporter$new()
    template_fun <- function(document) {
      disclaimer <- teal.reporter::teal_card("Here comes disclaimer text")
      c(disclaimer, document)
    }
    reporter$set_template(template_fun)
    doc1 <- teal.reporter::teal_card("## Header 2 text", "Regular text")
    metadata(doc1, "title") <- "Welcome card"
    reporter$append_cards(doc1)
    reporter$get_cards()

------------------------------------------------------------------------

### Method `get_template()`

Get the `Reporter` template

#### Usage

    Reporter$get_template()

#### Returns

a template `function`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Reporter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(ggplot2)

card1 <- teal_card("## Header 2 text", "A paragraph of default text")
card1 <- c(card1, ggplot(iris, aes(x = Petal.Length)) + geom_histogram())
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
metadata(card1, "title") <- "Card1"

card2 <- teal_card("Document introduction")
metadata(card2, "title") <- "Card2"

reporter <- Reporter$new()
reporter$append_cards(list(card1, card2))
library(rtables)
# With the card1 from above
lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
table_res2 <- build_table(lyt, airquality)
#> Split var [Day] was not character or factor. Converting to factor
card2 <- teal_card(
  "## Header 2 text",
  "A paragraph of default text",
  table_res2
)
metadata(card2, "title") <- "Card2"

reporter <- Reporter$new()
reporter$append_cards(list(card1, card2))

names(reporter$get_cards())
#> [1] "card_9b86adfd" "card_209f027c"
reporter$reorder_cards(c("Card2", "Card1"))
names(reporter$get_cards())
#> [1] "card_9b86adfd" "card_209f027c"
# With card1 and card2 from above

metadata(reporter$get_cards()[[1]], "title")
#> [1] "Card1"
reporter$replace_card(card2, names(reporter$get_cards())[[1]])
metadata(reporter$get_cards()[[1]], "title")
#> [1] "Card2"
# With card1 and card2 from above

reporter <- Reporter$new()
reporter$append_cards(list(card1, card2))
reporter$get_cards()
#> $card_158f14ec
#> $`86674098`
#> [1] "## Header 2 text"
#> 
#> $`3ea567c2`
#> [1] "A paragraph of default text"
#> 
#> $f603d362
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> attr(,"metadata")$title
#> [1] "Card1"
#> 
#> 
#> $card_6b298b64
#> $eea971b3
#> [1] "## Header 2 text"
#> 
#> $aa8159e6
#> [1] "A paragraph of default text"
#> 
#> $f00cca51

#>              all obs     
#> —————————————————————————
#> 1                        
#>   mean        77.75      
#> 2                        
#>   mean          43       
#> 3                        
#>   mean        33.25      
#> 4                        
#>   mean   62.3333333333333
#> 5                        
#>   mean   48.6666666666667
#> 6                        
#>   mean         41.5      
#> 7                        
#>   mean         54.2      
#> 8                        
#>   mean          57       
#> 9                        
#>   mean         61.4      
#> 10                       
#>   mean   49.3333333333333
#> 11                       
#>   mean         25.5      
#> 12                       
#>   mean        22.75      
#> 13                       
#>   mean         23.4      
#> 14                       
#>   mean   29.3333333333333
#> 15                       
#>   mean   12.6666666666667
#> 16                       
#>   mean         30.2      
#> 17                       
#>   mean         36.6      
#> 18                       
#>   mean         24.6      
#> 19                       
#>   mean         35.2      
#> 20                       
#>   mean         29.4      
#> 21                       
#>   mean        12.75      
#> 22                       
#>   mean   14.3333333333333
#> 23                       
#>   mean          20       
#> 24                       
#>   mean          41       
#> 25                       
#>   mean   96.6666666666667
#> 26                       
#>   mean          41       
#> 27                       
#>   mean          52       
#> 28                       
#>   mean        48.75      
#> 29                       
#>   mean        57.75      
#> 30                       
#>   mean        70.75      
#> 31                       
#>   mean   60.3333333333333
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> attr(,"metadata")$title
#> [1] "Card2"
#> 
#> 
# With card1 and card2 from above

reporter <- Reporter$new()
reporter$append_cards(list(card1, card2))
reporter$get_blocks()
#> $`4766d19a`
#> [1] "# Card1"
#> 
#> $`7e3d5813`
#> [1] "## Header 2 text"
#> 
#> $a4962984
#> [1] "A paragraph of default text"
#> 
#> $`0d49ce75`
#> 
#> $da1cd3e7
#> [1] "\\newpage"
#> 
#> $`32c20268`
#> [1] "# Card2"
#> 
#> $`3a907785`
#> [1] "## Header 2 text"
#> 
#> $`0a10bfa0`
#> [1] "A paragraph of default text"
#> 
#> $a26f4dcd
#>              all obs     
#> —————————————————————————
#> 1                        
#>   mean        77.75      
#> 2                        
#>   mean          43       
#> 3                        
#>   mean        33.25      
#> 4                        
#>   mean   62.3333333333333
#> 5                        
#>   mean   48.6666666666667
#> 6                        
#>   mean         41.5      
#> 7                        
#>   mean         54.2      
#> 8                        
#>   mean          57       
#> 9                        
#>   mean         61.4      
#> 10                       
#>   mean   49.3333333333333
#> 11                       
#>   mean         25.5      
#> 12                       
#>   mean        22.75      
#> 13                       
#>   mean         23.4      
#> 14                       
#>   mean   29.3333333333333
#> 15                       
#>   mean   12.6666666666667
#> 16                       
#>   mean         30.2      
#> 17                       
#>   mean         36.6      
#> 18                       
#>   mean         24.6      
#> 19                       
#>   mean         35.2      
#> 20                       
#>   mean         29.4      
#> 21                       
#>   mean        12.75      
#> 22                       
#>   mean   14.3333333333333
#> 23                       
#>   mean          20       
#> 24                       
#>   mean          41       
#> 25                       
#>   mean   96.6666666666667
#> 26                       
#>   mean          41       
#> 27                       
#>   mean          52       
#> 28                       
#>   mean        48.75      
#> 29                       
#>   mean        57.75      
#> 30                       
#>   mean        70.75      
#> 31                       
#>   mean   60.3333333333333
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> list()

## ------------------------------------------------
## Method `Reporter$new`
## ------------------------------------------------

reporter <- Reporter$new()


## ------------------------------------------------
## Method `Reporter$get_metadata`
## ------------------------------------------------

reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
reporter$get_metadata()
#> $sth
#> [1] "sth"
#> 


## ------------------------------------------------
## Method `Reporter$append_metadata`
## ------------------------------------------------

reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
reporter$get_metadata()
#> $sth
#> [1] "sth"
#> 


## ------------------------------------------------
## Method `Reporter$from_reporter`
## ------------------------------------------------

reporter <- Reporter$new()
reporter$from_reporter(reporter)
#> Warning: `Reporter$from_reporter()` was deprecated in teal.reporter 0.6.0.

## ------------------------------------------------
## Method `Reporter$to_list`
## ------------------------------------------------

reporter <- Reporter$new()
tmp_dir <- file.path(tempdir(), "testdir")
dir.create(tmp_dir)
reporter$to_list(tmp_dir)
#> $name
#> [1] "teal Reporter"
#> 
#> $version
#> [1] "1"
#> 
#> $id
#> [1] ""
#> 
#> $cards
#> list()
#> 
#> $metadata
#> list()
#> 

## ------------------------------------------------
## Method `Reporter$from_list`
## ------------------------------------------------

reporter <- Reporter$new()
tmp_dir <- file.path(tempdir(), "testdir")
unlink(tmp_dir, recursive = TRUE)
dir.create(tmp_dir)
reporter$from_list(reporter$to_list(tmp_dir), tmp_dir)

## ------------------------------------------------
## Method `Reporter$to_jsondir`
## ------------------------------------------------

reporter <- Reporter$new()
tmp_dir <- file.path(tempdir(), "jsondir")
dir.create(tmp_dir)
reporter$to_jsondir(tmp_dir)
#> [1] "/tmp/RtmpVDy0wa/jsondir"

## ------------------------------------------------
## Method `Reporter$from_jsondir`
## ------------------------------------------------

reporter <- Reporter$new()
tmp_dir <- file.path(tempdir(), "jsondir")
dir.create(tmp_dir)
#> Warning: '/tmp/RtmpVDy0wa/jsondir' already exists
unlink(list.files(tmp_dir, recursive = TRUE))
reporter$to_jsondir(tmp_dir)
#> [1] "/tmp/RtmpVDy0wa/jsondir"
reporter$from_jsondir(tmp_dir)

## ------------------------------------------------
## Method `Reporter$set_template`
## ------------------------------------------------


reporter <- teal.reporter::Reporter$new()
template_fun <- function(document) {
  disclaimer <- teal.reporter::teal_card("Here comes disclaimer text")
  c(disclaimer, document)
}
reporter$set_template(template_fun)
doc1 <- teal.reporter::teal_card("## Header 2 text", "Regular text")
metadata(doc1, "title") <- "Welcome card"
reporter$append_cards(doc1)
reporter$get_cards()
#> $card_4a4c4ed8
#> $ef7e0e44
#> [1] "Here comes disclaimer text"
#> 
#> $`15e3e723`
#> [1] "## Header 2 text"
#> 
#> $`01aa6aa3`
#> [1] "Regular text"
#> 
#> attr(,"class")
#> [1] "teal_card"
#> attr(,"metadata")
#> attr(,"metadata")$title
#> [1] "Welcome card"
#> 
#> 
```
