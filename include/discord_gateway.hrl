-define(MAX_RETIRES, 5).

-record(payload, {op :: non_neg_integer(),
                  d :: map(),
                  s=undefined :: non_neg_integer() | undefined,
                  t=undefined :: binary() | undefined
                 }).

-record(connection, {args :: string(),
                     host :: string(),
                     mref :: reference(),
                     pid :: pid(),
                     stream=undefined :: reference() | undefined
                    }).

-type payload() :: #payload{}.
% -type connection() :: #connection{}.
