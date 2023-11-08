# parameter json doesn't change without notice

    Code
      as_bq_params(list(scalar = "a", vector = c("a", "b", "c")))
    Output
      [
        {
          "name": "scalar",
          "parameterType": {
            "type": "STRING"
          },
          "parameterValue": {
            "value": "a"
          }
        },
        {
          "name": "vector",
          "parameterType": {
            "type": "ARRAY",
            "arrayType": {
              "type": "STRING"
            }
          },
          "parameterValue": {
            "arrayValues": [
              {
                "value": "a"
              },
              {
                "value": "b"
              },
              {
                "value": "c"
              }
            ]
          }
        }
      ]

# checks inputs

    Code
      bq_param_scalar(1:3)
    Condition
      Error in `bq_param_scalar()`:
      ! `value` must be length 1, not 3.
    Code
      bq_param_array(integer())
    Condition
      Error in `bq_param_array()`:
      ! `value` can't be zero-length.

