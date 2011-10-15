

-type tai_seconds() :: non_neg_integer().
-type tai_nano()    :: 0..999999999.
-type tai_atto()    :: 0..999999999.

-record(tai, { 
        x :: tai_seconds()       % TAI seconds 
        }).

-record(taia, {
         sec :: tai_seconds(),   % TAI seconds
        nano :: tai_nano(),      % TAI nanoseconds
        atto :: tai_atto()       % TAI attoseconds
        }).


-define(TAI_PACK, 8).

-define(TAIA_PACK, 16).
-define(TAIA_FMTFRAC, 18).
