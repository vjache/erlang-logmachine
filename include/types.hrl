-type timestamp() :: {MegaSeconds :: non_neg_integer(), 
                      Seconds :: non_neg_integer(), 
                      MicroSeconds :: non_neg_integer() } .
-type history_entry() :: {Timestamp :: timestamp(), Event :: any()} .

-type period() :: {non_neg_integer(), 
                   sec | 
                   min | 
                   hour | 
                   day} .

%
% Instance configuration item (or instance config property)
%
-type instance_config_item() :: 
          {evict_period, period()} | 
          {dir, file:name()} |
          {arch_dir, file:name()} |
          {evict_after, period()} |
          {reopen_period, period()} |
          {archive_after, period()} |
          {locate_em, atom()}.

%
% This type specification describes a config structure
%
-type config() :: {logmachine, 
                   [ instance_config_item() | % Config properties common for all instances
                     {instances, 
                      [{ InstanceName :: atom(), % Instance section 
                         [instance_config_item()]}] % Instance specific properties
                     }
                   ]} .