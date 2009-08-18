-record(contract, { name, description, quantity = 0, price = 0, status=active, final_price=undefined  }).
-record(market, { name, status=closed, contracts=[], creator, created_date, expires_date=0 }).
-record(account, { owner, balance, created_date, portfolio }).
-record(portfolio_entry, { market_name, contract_name, quantity }).
