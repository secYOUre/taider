
-type caldate_year()  :: integer().
-type caldate_month() :: 1..12.
-type caldate_day()   :: 1..31.

-record(caldate, {
        year  :: caldate_year(),
        month :: caldate_month(),
        day   :: caldate_day()
        }).

-define(MONTHTAB,  [0, 31, 61, 92, 122, 153, 184, 214, 245, 275, 306, 337]).
-define(TIMES365,  [0, 365, 730, 1095]).
-define(TIMES36524,[0, 36524, 73048, 109572]).

