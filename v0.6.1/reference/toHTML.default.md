# Convert report objects to HTML

**\[experimental\]**

The `toHTML` S3 generic method converts various report objects into HTML
representations. This is the primary method for rendering report content
for display in web browsers, IDE Viewer, or for inclusion in Shiny
applications.

## Usage

``` r
# Default S3 method
toHTML(x, ...)
```

## Arguments

- x:

  The object to convert to HTML. Supported types include:

  - `teal_card`: A list-like structure containing report elements

  - `teal_report`: A report object containing a `teal_card`

  - `ReportCard`: Deprecated R6 class for report cards

  - `code_chunk`: Code blocks created with
    [`code_chunk()`](https://insightsengineering.github.io/teal.reporter/reference/code_chunk.md)

  - `chunk_output`: Output from evaluated code chunks

  - Plot objects: `ggplot`, `recordedplot`, `trellis`, `grob`

  - Table objects: `data.frame`, `rtables`, `TableTree`,
    `ElementaryTable`, `listing_df`, `gtsummary`, `flextable`,
    `datatables`

  - Text: `character` strings (rendered as markdown)

  - Other objects: Conditions, model summaries, etc.

- ...:

  Additional arguments passed to methods.

## Value

An HTML representation of the input object. The exact return type
depends on the input class:

- For `teal_card`: A
  [`bslib::card()`](https://rstudio.github.io/bslib/reference/card.html)
  containing all elements

- For `code_chunk`: A
  [`bslib::accordion()`](https://rstudio.github.io/bslib/reference/accordion.html)
  with the code

- For plots: A `shiny::tags$img()` tag

- For text: HTML markup from markdown conversion

- For tables: HTML table elements

All returns are wrapped with
[`htmltools::browsable()`](https://rstudio.github.io/htmltools/reference/browsable.html)
to enable viewer display.

## Details

### Relationship with `teal_card`

The `teal_card` class is a central component in the `teal.reporter`
ecosystem. It is an S3 list where each element represents a piece of
report content (text, plots, tables, code chunks, etc.). The `toHTML`
method for `teal_card` objects:

1.  Iterates through each element in the `teal_card` list

2.  Calls `toHTML()` recursively on each element based on its class

3.  Wraps all converted elements in a
    [`bslib::card()`](https://rstudio.github.io/bslib/reference/card.html)
    container

This hierarchical conversion allows complex report structures to be
rendered as styled HTML with proper formatting for each content type.

### Content Type Conversions

**Text and Markdown:** Character strings are converted to HTML using
`CommonMark` markdown syntax. Supports headers, lists, code blocks,
emphasis, and other markdown features.

**Code Chunks:** Created with
[`code_chunk()`](https://insightsengineering.github.io/teal.reporter/reference/code_chunk.md),
these are rendered as collapsible Bootstrap accordions with syntax
highlighting. The accordion includes the programming language indicator
and an icon.

**Plots:** Plot objects (`ggplot`, `recordedplot`, `trellis`, `grob`)
are converted to PNG images with base64-encoded data URIs, making them
self-contained in the HTML output.

**Tables:** Table objects are converted to styled HTML tables, typically
via `flextable` for consistent formatting.

### Viewer Integration

All HTML output is wrapped with
[`htmltools::browsable()`](https://rstudio.github.io/htmltools/reference/browsable.html),
which enables:

- Automatic render in IDE Viewer when displayed interactively

- Proper HTML dependency injection (Bootstrap CSS/JavaScript, Font
  Awesome icons, etc.)

- Standalone HTML files with all required resources

You can override the `browsable` behavior with:

    print(toHTML(x), browse = FALSE)  # Print markup to console instead

## See also

- [`teal_report()`](https://insightsengineering.github.io/teal.reporter/reference/teal_report.md)
  for creating report objects

- [`teal_card()`](https://insightsengineering.github.io/teal.reporter/reference/teal_card.md)
  for creating report cards

- [`code_chunk()`](https://insightsengineering.github.io/teal.reporter/reference/code_chunk.md)
  for creating code blocks

- [`render()`](https://insightsengineering.github.io/teal.reporter/reference/render.md)
  for rendering complete reports to files

## Examples

``` r
# Initialize empty report
report <- teal_report()

# Add arbitrary markdown elements to the report's teal_card
teal_card(report) <- c(
  teal_card(report),
  "## Document section",
  "Lorem ipsum dolor sit amet"
)

# Use within() to execute code and add code-chunk
report <- within(report, a <- 2)

# within() automatically captures code and outputs
report <- within(report, plot(a))

html <- tools::toHTML(report)
# display HTML markup in viewer
html

  
    Document section

    Lorem ipsum dolor sit amet

    
      
        
          
            
            
              
                
                R
              
            
          
        
        
          
            a <- 2
```

R

``` R
plot(a)
```

![](data:image/png;base64,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z+bNm2+wcNWqVevXrw+cB1CTtGjRorS0tFOnTp/9WFpa2rp16/xOCaidNgqs4t5DBow++YAJ7f4yuv/WJ1x03o1LWk97uO4OxxYGD1y/Q/ddv3NXVPPmzXv27LnBwrZt2y5atCh4KkBNceSRR44bN+7mm29u3779nDlz7rjjjgcffDDfkwJqo41Ogcq0GHbtzMkjt29S1Pawq/6w5+J7bn9u8zOuP3XHH3DBrNWv33Hmwf179tr7yLP/33PLPj91fv1Tv95n9NSS9PuvFmAD3bt3nzhx4pgxY/bcc8/LL7/8lltuadrUtfaAPPimbqq/1aAjt0qSJGm6/4Sb9/+hI6z++68Gn/H+kZdecnzBSzdOGDxk0d8fPadHvSRdNf+1uW0c7QNi9enT59577833LIDaLvtXcn/z8We2HH/fBYduWZDsPWjXZsMGnPSHAx8/vWu2xwUAyJfsXyWhuE7Rqk+WVyZJkiRFW4+66pyGfxp92ctrHBsEAGqqrAdW0faH/KLBtQftd8KFNz1bkiaF2xz/54s73HDQ4FNve60822MDAORD9vdgFXUb9+Csq4Y0+eiteaurkiQp7Hj4LU9MHtFwfcPttm6R9SOUAAA5l4vCyTTebsjYS4Z8ZdAO/cf8vv+YHAwNAJB7vqkGACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIVpTtAdJV81/7qE7nbdvWTZJk3cInbv7vv724uKDDLj876pcDt6ib7dEBAHIv63uwKp7/3dCRN8+rTJLKd/98wK4/n/RmQfvNGi2578Q9fvLbF9dme3QAgNzL+h6sL1S8duuk9w+58/mr9myYJEl6XI8hfa9+ZOzkIQ1yNgMAgJzI4TlY69dVbb1Dt897KtN6u+0aLC0pS3M3PgBAbuQisCrenXrB+P/6/RPlrV6/8drnViVJUvnJK5OvvLfurj2bZXIwPgBATmU9sIp2Pe2uyafvs02jsgWr2uzUeO6LCyuT8vtO3u/yypOuG7dj7o5QAgDkStYLJ9N46z77bd3n6wuHTp53UJG4AgBqpvxUTlFRwfuTR456YciUqw9q9S1HCefNm/f2229vvLCqqirr8wMA+AHytRsp02TbPfevt1W9bz8Ha/HixS+88MIGC6uqqjbbbLPsTg0A4IfJW2C17HPUaX2+6x69evXq1avXBgunTJlSWlqaxXkBAPxgvioHACCYwAIACJb9Q4SVb9x06oXTS7/hzPSi7sdcfc7ezV0KCwCoWbIfWIXbDD52yHPHHH9Hk+N+c2iXwq/estlmddUVAFDj5OAk9+JWOx168Zn3znxyyHH/Z8/i7I8HAJBfOforwsb7Tpy8Y2uXFgUAaoMcNU+maZdeTXMzFABAnvkrQgCAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGBF+Z7Af6Zp06YXX3zxPffcE7XCp59+ul69elFrY1NWXl5eVFRUWFiY74mQC2VlZQ0aNMj3LMiFdevWFRQUFBVVs7czvp/KyspevXpFrW3+/Pn169ePWtsGMmmaZmnVm77Kysp99913xowZ+Z4IuTBx4sR+/foNGjQo3xMhFwYOHDhz5sx8z4JcuPTSS7t16zZ48OB8T4RcqEabtkOEAADBBBYAQDCBBQAQTGABAAQTWAAAwWp1YGUyGX/ZW3sUFha6RkPtYdOuPWzatUo12rRr9WUakiRZtWpV48aN8z0LcqGsrKxevXoFBbX6Q0XtYdOuPdasWVNcXFyN3nf5IarRpl3bAwsAIJxP8wAAwQQWAEAwgQUAEExgAQAEE1gAAMEEFgBAMIEFABCscMKECfmeQ46kK96cMfXOB59fXHfzTu0ablSW6xa//PBfpk57vrRxp85t6mXyMUMCffr+rL/eed/TH6ZtO23eZMMLEKYrXp9+x+1/e/Kdta06b9WiOC8TJNiako9WFTeuv/EFvStKXpl219S//8/yxh23bm3TrhEqV3y0pKJRozobPZuVS+c8cNut9z3x6pI6W3TusPHrPNXQt27an1u/qmRV2rDeJnid2dry+5eufOy0PQaeNX3B4tmXDt195N0ff/3yqute+8OQPkfc8Oqy0qcv3PfHv3pipauvVmvlL160V5/Rd7275LUbfrHbgX+eW/nVG9Nl00/cbZ+Js0rLFj4wrt+AC2aX5WuaxKl46w8HHXTNO5UbLq/6+C8je+//u2cXLXjk3J/0G/+YTbsGSFdMO3XA+L+v22j54ntG9d5nwqMfly3752VDewydtPHvA9XOt23an0tXPDa+9/anPFKe00n9u9JaoWrhpH1b7nXt/Mo0rfrkr7/osOuFr1V85dbFNw9rv/c1H1amaVq56MHfnPrfc9blbar8cCv+enjb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)

\# Print HTML markup to console instead of viewer
[print](https://rdrr.io/r/base/print.html)(html, browse = FALSE) \#\>
\<div class="container-fluid"\> \#\> \<div class="card bslib-card
bslib-mb-spacing html-fill-item html-fill-container"
data-bslib-card-init data-require-bs-caller="card()"
data-require-bs-version="5"\> \#\> \<div class="card-body
bslib-gap-spacing html-fill-item html-fill-container"
style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;"\> \#\>
\<h2\>Document section\</h2\> \#\> \#\> \<p\>Lorem ipsum dolor sit
amet\</p\> \#\> \#\> \<div class="accordion code_chunk"
data-require-bs-caller="accordion()" data-require-bs-version="5"
id="bslib-accordion-6406"\> \#\> \<div class="accordion-item"
data-value="rcode"\> \#\> \<div class="accordion-header"\> \#\> \<button
class="accordion-button collapsed" type="button"
data-bs-toggle="collapse" data-bs-target="#bslib-accordion-panel-2005"
aria-expanded="false" aria-controls="bslib-accordion-panel-2005"\> \#\>
\<div class="accordion-icon"\>\</div\> \#\> \<div
class="accordion-title"\> \#\> \<span\> \#\> \<i class="fas fa-code"
role="presentation" aria-label="code icon"\>\</i\> \#\> R \#\> \</span\>
\#\> \</div\> \#\> \</button\> \#\> \</div\> \#\> \<div
id="bslib-accordion-panel-2005" class="accordion-collapse collapse"\>
\#\> \<div class="accordion-body"\> \#\> \<pre\>\<code
class="language-R"\>a &lt;- 2\</code\>\</pre\> \#\> \</div\> \#\>
\</div\> \#\> \</div\> \#\> \</div\> \#\> \<div class="accordion
code_chunk" data-require-bs-caller="accordion()"
data-require-bs-version="5" id="bslib-accordion-1519"\> \#\> \<div
class="accordion-item" data-value="rcode"\> \#\> \<div
class="accordion-header"\> \#\> \<button class="accordion-button
collapsed" type="button" data-bs-toggle="collapse"
data-bs-target="#bslib-accordion-panel-3886" aria-expanded="false"
aria-controls="bslib-accordion-panel-3886"\> \#\> \<div
class="accordion-icon"\>\</div\> \#\> \<div class="accordion-title"\>
\#\> \<span\> \#\> \<i class="fas fa-code" role="presentation"
aria-label="code icon"\>\</i\> \#\> R \#\> \</span\> \#\> \</div\> \#\>
\</button\> \#\> \</div\> \#\> \<div id="bslib-accordion-panel-3886"
class="accordion-collapse collapse"\> \#\> \<div
class="accordion-body"\> \#\> \<pre\>\<code
class="language-R"\>plot(a)\</code\>\</pre\> \#\> \</div\> \#\> \</div\>
\#\> \</div\> \#\> \</div\> \#\> \<img
src="data:image/png;base64,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z+bNm2+wcNWqVevXrw+cB1CTtGjRorS0tFOnTp/9WFpa2rp16/xOCaidNgqs4t5DBow++YAJ7f4yuv/WJ1x03o1LWk97uO4OxxYGD1y/Q/ddv3NXVPPmzXv27LnBwrZt2y5atCh4KkBNceSRR44bN+7mm29u3779nDlz7rjjjgcffDDfkwJqo41Ogcq0GHbtzMkjt29S1Pawq/6w5+J7bn9u8zOuP3XHH3DBrNWv33Hmwf179tr7yLP/33PLPj91fv1Tv95n9NSS9PuvFmAD3bt3nzhx4pgxY/bcc8/LL7/8lltuadrUtfaAPPimbqq/1aAjt0qSJGm6/4Sb9/+hI6z++68Gn/H+kZdecnzBSzdOGDxk0d8fPadHvSRdNf+1uW0c7QNi9enT59577833LIDaLvtXcn/z8We2HH/fBYduWZDsPWjXZsMGnPSHAx8/vWu2xwUAyJfsXyWhuE7Rqk+WVyZJkiRFW4+66pyGfxp92ctrHBsEAGqqrAdW0faH/KLBtQftd8KFNz1bkiaF2xz/54s73HDQ4FNve60822MDAORD9vdgFXUb9+Csq4Y0+eiteaurkiQp7Hj4LU9MHtFwfcPttm6R9SOUAAA5l4vCyTTebsjYS4Z8ZdAO/cf8vv+YHAwNAJB7vqkGACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIVpTtAdJV81/7qE7nbdvWTZJk3cInbv7vv724uKDDLj876pcDt6ib7dEBAHIv63uwKp7/3dCRN8+rTJLKd/98wK4/n/RmQfvNGi2578Q9fvLbF9dme3QAgNzL+h6sL1S8duuk9w+58/mr9myYJEl6XI8hfa9+ZOzkIQ1yNgMAgJzI4TlY69dVbb1Dt897KtN6u+0aLC0pS3M3PgBAbuQisCrenXrB+P/6/RPlrV6/8drnViVJUvnJK5OvvLfurj2bZXIwPgBATmU9sIp2Pe2uyafvs02jsgWr2uzUeO6LCyuT8vtO3u/yypOuG7dj7o5QAgDkStYLJ9N46z77bd3n6wuHTp53UJG4AgBqpvxUTlFRwfuTR456YciUqw9q9S1HCefNm/f2229vvLCqqirr8wMA+AHytRsp02TbPfevt1W9bz8Ha/HixS+88MIGC6uqqjbbbLPsTg0A4IfJW2C17HPUaX2+6x69evXq1avXBgunTJlSWlqaxXkBAPxgvioHACCYwAIACJb9Q4SVb9x06oXTS7/hzPSi7sdcfc7ezV0KCwCoWbIfWIXbDD52yHPHHH9Hk+N+c2iXwq/estlmddUVAFDj5OAk9+JWOx168Zn3znxyyHH/Z8/i7I8HAJBfOforwsb7Tpy8Y2uXFgUAaoMcNU+maZdeTXMzFABAnvkrQgCAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGACCwAgmMACAAgmsAAAggksAIBgAgsAIJjAAgAIJrAAAIIJLACAYAILACCYwAIACCawAACCCSwAgGBF+Z7Af6Zp06YXX3zxPffcE7XCp59+ul69elFrY1NWXl5eVFRUWFiY74mQC2VlZQ0aNMj3LMiFdevWFRQUFBVVs7czvp/KyspevXpFrW3+/Pn169ePWtsGMmmaZmnVm77Kysp99913xowZ+Z4IuTBx4sR+/foNGjQo3xMhFwYOHDhz5sx8z4JcuPTSS7t16zZ48OB8T4RcqEabtkOEAADBBBYAQDCBBQAQTGABAAQTWAAAwWp1YGUyGX/ZW3sUFha6RkPtYdOuPWzatUo12rRr9WUakiRZtWpV48aN8z0LcqGsrKxevXoFBbX6Q0XtYdOuPdasWVNcXFyN3nf5IarRpl3bAwsAIJxP8wAAwQQWAEAwgQUAEExgAQAEE1gAAMEEFgBAMIEFABCscMKECfmeQ46kK96cMfXOB59fXHfzTu0ablSW6xa//PBfpk57vrRxp85t6mXyMUMCffr+rL/eed/TH6ZtO23eZMMLEKYrXp9+x+1/e/Kdta06b9WiOC8TJNiako9WFTeuv/EFvStKXpl219S//8/yxh23bm3TrhEqV3y0pKJRozobPZuVS+c8cNut9z3x6pI6W3TusPHrPNXQt27an1u/qmRV2rDeJnid2dry+5eufOy0PQaeNX3B4tmXDt195N0ff/3yqute+8OQPkfc8Oqy0qcv3PfHv3pipauvVmvlL160V5/Rd7275LUbfrHbgX+eW/nVG9Nl00/cbZ+Js0rLFj4wrt+AC2aX5WuaxKl46w8HHXTNO5UbLq/6+C8je+//u2cXLXjk3J/0G/+YTbsGSFdMO3XA+L+v22j54ntG9d5nwqMfly3752VDewydtPHvA9XOt23an0tXPDa+9/anPFKe00n9u9JaoWrhpH1b7nXt/Mo0rfrkr7/osOuFr1V85dbFNw9rv/c1H1amaVq56MHfnPrfc9blbar8cCv+enjb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"
style="width: 100%; height: auto; max-width: 800.000000px"/\> \#\>
\</div\> \#\> \<script
data-bslib-card-init\>bslib.Card.initializeAllCards();\</script\> \#\>
\</div\> \#\> \</div\>
