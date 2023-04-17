# can correclty print a lazy query

    Code
      print(bq_mtcars)
    Output
      # Source:   table<mtcars> [?? x 11]
      # Database: BigQueryConnection
            am  carb    vs  qsec    wt  drat  disp    hp   cyl  gear   mpg
         <int> <int> <int> <dbl> <dbl> <dbl> <dbl> <int> <int> <int> <dbl>
       1     0     2     1  20    3.19  3.69  147.    62     4     4  24.4
       2     0     2     1  22.9  3.15  3.92  141.    95     4     4  22.8
       3     0     1     1  20.0  2.46  3.7   120.    97     4     3  21.5
       4     0     1     1  19.4  3.22  3.08  258    110     6     3  21.4
       5     0     1     1  20.2  3.46  2.76  225    105     6     3  18.1
       6     0     4     1  18.3  3.44  3.92  168.   123     6     4  19.2
       7     0     4     1  18.9  3.44  3.92  168.   123     6     4  17.8
       8     0     2     0  17.0  3.44  3.15  360    175     8     3  18.7
       9     0     4     0  15.8  3.57  3.21  360    245     8     3  14.3
      10     0     3     0  17.4  4.07  3.07  276.   180     8     3  16.4
      # i more rows

