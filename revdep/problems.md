# BiocOncoTK

Version: 1.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: BiocParallel
      
      Attaching package: 'DelayedArray'
      
      The following objects are masked from 'package:matrixStats':
      
          colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
      
      The following objects are masked from 'package:base':
      
          aperm, apply
      
      Error: package or namespace load failed for 'restfulSE' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
       there is no package called 'GO.db'
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 18-30 (BiocOncoTK.Rmd) 
    Error: processing vignette 'BiocOncoTK.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘org.Hs.eg.db’ ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
      ‘TxDb.Hsapiens.UCSC.hg18.knownGene’ ‘FDb.InfiniumMethylation.hg19’
      ‘EnsDb.Hsapiens.v75’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.9Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘DBI’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      (/Users/hadley/Documents/tidyverse/bigrquery/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:118)
    ggFeatureSegs: no visible binding for global variable ‘symbol’
      (/Users/hadley/Documents/tidyverse/bigrquery/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:124-126)
    ggMutDens : <anonymous>: no visible binding for global variable
      ‘Consequence’
    ggMutDens: no visible binding for global variable ‘project_short_name’
      (/Users/hadley/Documents/tidyverse/bigrquery/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:67-68)
    ggMutDens: no visible binding for global variable ‘project_short_name’
      (/Users/hadley/Documents/tidyverse/bigrquery/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:70)
    ggMutDens: no visible binding for global variable ‘project_short_name’
      (/Users/hadley/Documents/tidyverse/bigrquery/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/mc3utils.R:71-73)
    mc3toGR : <anonymous>: no visible binding for global variable
      ‘Consequence’
    rainfall: no visible global function definition for ‘genome’
      (/Users/hadley/Documents/tidyverse/bigrquery/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/rainfall3.R:152)
    tumNorSet : <anonymous>: no visible global function definition for
      ‘pancan_SE’
      (/Users/hadley/Documents/tidyverse/bigrquery/revdep/checks.noindex/BiocOncoTK/new/BiocOncoTK.Rcheck/00_pkg_src/BiocOncoTK/R/pancan.R:132-134)
    Undefined global functions or variables:
      BiocFileCache Consequence genes genome pancan_SE project_short_name
      seqlengths symbol tfstart
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 46 marked UTF-8 strings
    ```

# restfulSE

Version: 1.4.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘GO.db’
    
    Packages suggested but not available for checking:
      ‘org.Mm.eg.db’ ‘org.Hs.eg.db’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

