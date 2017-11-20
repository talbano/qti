
qti
===

The qti package provides functions for reading and writing QTI XML files in R, so as to support the analysis of assessment item content. These functions implement XML parsing from the xml2 package.

QTI, question and test interoperability, is a complex XML standard for storing and sharing assessment information and results. The qti package aims to cover the simpler QTI item interactions, currently single and multiple choice, along with assessment manifest files. QTI can files can also be built from scratch, as shown below.

Installation
------------

Install qti from github with:

``` r
# install.packages("devtools")
devtools::install_github("talbano/qti")
```

Usage
-----

Here, we build a basic multiple choice assessment item from scratch, write to a temporary QTI file, and read back in.

``` r
## Load package
library("qti")

## Build an item from scratch
item <- qti_item(
  id = 999,
  title = "Example Item",
  type = "choice",
  prompt = "What does this image tell you? <img src='life.png' />",
  options = c("Everything", "Something",
    "Nothing, but look at this code:<br/><pre>lm(life ~ R)</pre>"),
  key = c(1, 1, 0)
)

## Default view is parsed text, with HTML formatting removed and
## placeholders added for any images, tables, math, and code
item
#> 
#> qti_item
#> Example Item 
#> 999 
#> 
#> What does this image tell you? [img] 
#> 
#> Everything
#> 
#> Something
#> 
#> Nothing, but look at this code: [pre]

## Write to file
temp <- tempfile(fileext = ".qti")
write_qti(item, temp)
readLines(temp)
#>  [1] "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"                                                                                                                                                                                                                                                                                                
#>  [2] "<!-- QTI template for choice interaction item -->"                                                                                                                                                                                                                                                                                         
#>  [3] "<!-- Built with the qti R package, https://github.com/talbano/qti -->"                                                                                                                                                                                                                                                                     
#>  [4] "<assessmentItem xmlns=\"http://www.imsglobal.org/xsd/imsqti_v2p2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.imsglobal.org/xsd/imsqti_v2p2  http://www.imsglobal.org/xsd/qti/qtiv2p2/imsqti_v2p2.xsd\" identifier=\"999\" title=\"Example Item\" adaptive=\"false\" timeDependent=\"false\">"
#>  [5] "  <responseDeclaration identifier=\"RESPONSE\" cardinality=\"single\" baseType=\"identifier\">"                                                                                                                                                                                                                                            
#>  [6] "    <correctResponse>"                                                                                                                                                                                                                                                                                                                     
#>  [7] "      <value>ChoiceA</value>"                                                                                                                                                                                                                                                                                                              
#>  [8] "      <value>ChoiceB</value>"                                                                                                                                                                                                                                                                                                              
#>  [9] "    </correctResponse>"                                                                                                                                                                                                                                                                                                                    
#> [10] "  </responseDeclaration>"                                                                                                                                                                                                                                                                                                                  
#> [11] "  <outcomeDeclaration identifier=\"SCORE\" cardinality=\"single\" baseType=\"float\">"                                                                                                                                                                                                                                                     
#> [12] "    <defaultValue>"                                                                                                                                                                                                                                                                                                                        
#> [13] "      <value>0</value>"                                                                                                                                                                                                                                                                                                                    
#> [14] "    </defaultValue>"                                                                                                                                                                                                                                                                                                                       
#> [15] "  </outcomeDeclaration>"                                                                                                                                                                                                                                                                                                                   
#> [16] "  <itemBody>"                                                                                                                                                                                                                                                                                                                              
#> [17] "    <prompt>What does this image tell you? <img src=\"life.png\"/></prompt>"                                                                                                                                                                                                                                                               
#> [18] "    <choiceInteraction responseIdentifier=\"RESPONSE\" shuffle=\"false\" maxChoices=\"1\">"                                                                                                                                                                                                                                                
#> [19] "        <simpleChoice identifier=\"ChoiceA\">Everything</simpleChoice><simpleChoice identifier=\"ChoiceB\">Something</simpleChoice><simpleChoice identifier=\"ChoiceC\">Nothing, but look at this code:<br/><pre>lm(life ~ R)</pre></simpleChoice></choiceInteraction>"                                                                    
#> [20] "  </itemBody>"                                                                                                                                                                                                                                                                                                                             
#> [21] "  <responseProcessing template=\"http://www.imsglobal.org/question/qti_v2p2/rptemplates/match_correct\"/>"                                                                                                                                                                                                                                 
#> [22] "</assessmentItem>"

## Read as QTI
read_qti(temp)
#> 
#> qti_item
#> Example Item 
#> 999 
#> 
#> What does this image tell you? [img] 
#> 
#> Everything
#> 
#> Something
#> 
#> Nothing, but look at this code: [pre]
```
