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

