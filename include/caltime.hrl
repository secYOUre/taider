

-include("caldate.hrl").

-type caltime_hour()    :: 0..23.
-type caltime_minute()  :: 0..59.
-type caltime_second()  :: 0..60.         %% accounting for leap seconds
-type caltime_offset()  :: integer().

-record(caltime, {
        date    :: #caldate{},
        hour    :: caltime_hour(),
        minutes :: caltime_minute(),
        seconds :: caltime_second(),
        offset  :: caltime_offset()
        }).
