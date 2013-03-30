%% allowed scales for report_request
-type scale()  :: minute | hour | day | week | month.

%% info about a stocks transaction
-record(stock_record, {
          name     ::string(),    %% stock name
          datetime ::integer(),   %% time of stock deal
          price    ::float(),     %% price of a single stock
          count    ::integer()    %% how many stocks
        }).

%% request about transactions for a period of time
-record(report_request, {
          name     ::string(),    %% stock name
          datefrom,               %% ::datetime() start time of the period
          dateto,                 %% ::datetime() end time of the period
          scale    ::scale()      %% scale of the period
        }).

%% single line from transactions report
-record(report_line, {
          timeopen,                %% ::datetime() time period
          priceopen  ::float(),    %% price for open for period
          priceclose ::float(),    %% price for close of period
          pricemin   ::float(),    %% min price of period
          pricemax   ::float(),    %% max price of period
          volume     ::integer()   %% total volume of transactions for period
        }).