# recursive printing of subfields

    Code
      print(z3)
    Output
      <bq_field> z3 <RECORD>
        z2 <RECORD>
          z1 <RECORD>
            x <STRING>
            y <INTEGER>
      
    Code
      print(z3$fields)
    Output
      <bq_fields>
        z2 <RECORD>
          z1 <RECORD>
            x <STRING>
            y <INTEGER>
      

# tests its inputs

    Code
      bq_field(1)
    Condition
      Error in `bq_field()`:
      ! `name` must be a single string, not the number 1.
    Code
      bq_field("x", 1)
    Condition
      Error in `bq_field()`:
      ! `type` must be a single string, not the number 1.
    Code
      bq_field("x", "y", mode = 1)
    Condition
      Error in `bq_field()`:
      ! `mode` must be a single string, not the number 1.
    Code
      bq_field("x", "y", description = 1)
    Condition
      Error in `bq_field()`:
      ! `description` must be a single string or `NULL`, not the number 1.

