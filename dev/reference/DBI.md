# DBI methods

Implementations of pure virtual functions defined in the `DBI` package.

## Usage

``` r
# S4 method for class 'bq_dataset'
dbConnect(drv, ...)

# S4 method for class 'BigQueryDriver'
show(object)

# S4 method for class 'BigQueryDriver'
dbGetInfo(dbObj, ...)

# S4 method for class 'BigQueryDriver'
dbIsValid(dbObj, ...)

# S4 method for class 'BigQueryDriver'
dbDataType(dbObj, obj, ...)

# S4 method for class 'BigQueryConnection'
show(object)

# S4 method for class 'BigQueryConnection'
dbIsValid(dbObj, ...)

# S4 method for class 'BigQueryConnection'
dbDisconnect(conn, ...)

# S4 method for class 'BigQueryConnection,character'
dbSendQuery(conn, statement, ..., params = NULL)

# S4 method for class 'BigQueryConnection,character'
dbExecute(conn, statement, ...)

# S4 method for class 'BigQueryConnection,character'
dbQuoteString(conn, x, ...)

# S4 method for class 'BigQueryConnection,SQL'
dbQuoteString(conn, x, ...)

# S4 method for class 'BigQueryConnection,character'
dbQuoteIdentifier(conn, x, ...)

# S4 method for class 'BigQueryConnection,SQL'
dbQuoteIdentifier(conn, x, ...)

# S4 method for class 'BigQueryConnection,logical'
dbQuoteLiteral(conn, x, ...)

# S4 method for class 'BigQueryConnection'
dbDataType(dbObj, obj, ...)

# S4 method for class 'BigQueryConnection,character,data.frame'
dbWriteTable(
  conn,
  name,
  value,
  ...,
  overwrite = FALSE,
  append = FALSE,
  field.types = NULL,
  temporary = FALSE,
  row.names = NA
)

# S4 method for class 'BigQueryConnection,Id,data.frame'
dbWriteTable(
  conn,
  name,
  value,
  ...,
  overwrite = FALSE,
  append = FALSE,
  field.types = NULL,
  temporary = FALSE,
  row.names = NA
)

# S4 method for class 'BigQueryConnection,AsIs,data.frame'
dbWriteTable(
  conn,
  name,
  value,
  ...,
  overwrite = FALSE,
  append = FALSE,
  field.types = NULL,
  temporary = FALSE,
  row.names = NA
)

# S4 method for class 'BigQueryConnection,character,data.frame'
dbAppendTable(conn, name, value, ..., row.names = NULL)

# S4 method for class 'BigQueryConnection,Id,data.frame'
dbAppendTable(conn, name, value, ..., row.names = NULL)

# S4 method for class 'BigQueryConnection,AsIs,data.frame'
dbAppendTable(conn, name, value, ..., row.names = NULL)

# S4 method for class 'BigQueryConnection'
dbCreateTable(conn, name, fields, ..., row.names = NULL, temporary = FALSE)

# S4 method for class 'BigQueryConnection'
dbCreateTable(conn, name, fields, ..., row.names = NULL, temporary = FALSE)

# S4 method for class 'BigQueryConnection,character'
dbReadTable(conn, name, ...)

# S4 method for class 'BigQueryConnection,Id'
dbReadTable(conn, name, ...)

# S4 method for class 'BigQueryConnection,AsIs'
dbReadTable(conn, name, ...)

# S4 method for class 'BigQueryConnection'
dbListTables(conn, ...)

# S4 method for class 'BigQueryConnection,character'
dbExistsTable(conn, name, ...)

# S4 method for class 'BigQueryConnection,Id'
dbExistsTable(conn, name, ...)

# S4 method for class 'BigQueryConnection,AsIs'
dbExistsTable(conn, name, ...)

# S4 method for class 'BigQueryConnection,character'
dbListFields(conn, name, ...)

# S4 method for class 'BigQueryConnection,Id'
dbListFields(conn, name, ...)

# S4 method for class 'BigQueryConnection,AsIs'
dbListFields(conn, name, ...)

# S4 method for class 'BigQueryConnection,character'
dbRemoveTable(conn, name, ...)

# S4 method for class 'BigQueryConnection,Id'
dbRemoveTable(conn, name, ...)

# S4 method for class 'BigQueryConnection,AsIs'
dbRemoveTable(conn, name, ...)

# S4 method for class 'BigQueryConnection'
dbGetInfo(dbObj, ...)

# S4 method for class 'BigQueryConnection'
dbBegin(conn, ...)

# S4 method for class 'BigQueryConnection'
dbCommit(conn, ...)

# S4 method for class 'BigQueryConnection'
dbRollback(conn, ...)

# S4 method for class 'BigQueryResult'
show(object)

# S4 method for class 'BigQueryResult'
dbIsValid(dbObj, ...)

# S4 method for class 'BigQueryResult'
dbClearResult(res, ...)

# S4 method for class 'BigQueryResult'
dbFetch(res, n = -1, ...)

# S4 method for class 'BigQueryResult'
dbHasCompleted(res, ...)

# S4 method for class 'BigQueryResult'
dbGetStatement(res, ...)

# S4 method for class 'BigQueryResult'
dbColumnInfo(res, ...)

# S4 method for class 'BigQueryResult'
dbGetRowCount(res, ...)

# S4 method for class 'BigQueryResult'
dbGetRowsAffected(res, ...)

# S4 method for class 'BigQueryResult'
dbBind(res, params, ...)
```

## Arguments

- ...:

  Other arguments to methods.

- object:

  Any R object

- dbObj:

  An object inheriting from
  [DBI::DBIObject](https://dbi.r-dbi.org/reference/DBIObject-class.html),
  i.e.
  [DBI::DBIDriver](https://dbi.r-dbi.org/reference/DBIDriver-class.html),
  [DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html),
  or a
  [DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html).

- obj:

  An R object whose SQL type we want to determine.

- conn:

  A
  [DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
  object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- statement:

  a character string containing SQL.

- params:

  For [`dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html), a list
  of values, named or unnamed, or a data frame, with one element/column
  per query parameter. For
  [`dbBindArrow()`](https://dbi.r-dbi.org/reference/dbBind.html), values
  as a nanoarrow stream, with one column per query parameter.

- x:

  A character vector to quote as string.

- name:

  The table name, passed on to
  [`dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
  Options are:

  - a character string with the unquoted DBMS table name, e.g.
    `"table_name"`,

  - a call to [`Id()`](https://dbi.r-dbi.org/reference/Id.html) with
    components to the fully qualified table name, e.g.
    `Id(schema = "my_schema", table = "table_name")`

  - a call to [`SQL()`](https://dbi.r-dbi.org/reference/SQL.html) with
    the quoted and fully qualified table name given verbatim, e.g.
    `SQL('"my_schema"."table_name"')`

- value:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) (or coercible
  to data.frame).

- overwrite:

  a logical specifying whether to overwrite an existing table or not.
  Its default is `FALSE`.

- append:

  a logical specifying whether to append to an existing table in the
  DBMS. Its default is `FALSE`.

- field.types, temporary:

  Ignored. Included for compatibility with generic.

- row.names:

  A logical specifying whether the `row.names` should be output to the
  output DBMS table; if `TRUE`, an extra field whose name will be
  whatever the R identifier `"row.names"` maps to the DBMS (see
  [`DBI::make.db.names()`](https://dbi.r-dbi.org/reference/make.db.names.html)).
  If `NA` will add rows names if they are characters, otherwise will
  ignore.

- fields:

  Either a character vector or a data frame.

  A named character vector: Names are column names, values are types.
  Names are escaped with
  [`dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).
  Field types are unescaped.

  A data frame: field types are generated using
  [`dbDataType()`](https://dbi.r-dbi.org/reference/dbDataType.html).

- res:

  An object inheriting from
  [DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html).

- n:

  maximum number of records to retrieve per fetch. Use `n = -1` or
  `n = Inf` to retrieve all pending records. Some implementations may
  recognize other special values.
