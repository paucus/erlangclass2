-module(test).
-export([print/2]).

print(Num,Msg)->

    io:format("~s ",[string:copies(" ",4*Num)++Msg]).
