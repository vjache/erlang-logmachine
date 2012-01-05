-type timestamp() :: {MegaSeconds :: non_neg_integer(), 
                      Seconds :: non_neg_integer(), 
                      MicroSeconds :: non_neg_integer() } .
-type history_entry() :: {Timestamp :: timestamp(), Event :: any()} .