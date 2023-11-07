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
      

