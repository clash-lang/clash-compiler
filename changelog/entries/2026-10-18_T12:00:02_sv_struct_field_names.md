---
issues: []
prs: [3503]
---

# CHANGED
Fields of SystemVerilog structs for records are now named after the record fields, e.g. `baudCount` instead of `Tx_sel1` for the field `baudCount` of a record `Tx`. When a record field isn't a valid SystemVerilog identifier, such as a keyword like `input` or a name with a `'`, the struct's fields are named like `Tx_sel1_baudCount` instead. Fields of tuples and other products without field names keep names like `Tuple2_sel0`.
