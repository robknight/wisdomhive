-record(trade_event, { contract_name = "", quantity = 0 }).
-record(create_market_event, { user }).
-record(create_contract_event, { name = "", description = "" }).
-record(market_status_event, { status = '' }).
-record(balance_change_event, { amount = 0 }).

-type(wh_event() :: #trade_event{} | #create_market_event{} |
                    #create_contract_event{} | #market_status_event{}).
                    
-record(event, { sequence :: integer(), source, status :: atom(), event :: wh_event() }).