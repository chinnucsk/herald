-module(herald).

-export([push/2, new_device/3]).

new_device(DeviceType, DeviceAddress, LogId) ->
    case DeviceType of
        android ->
            {ok, Pid} = herald_sup:start_android(DeviceAddress, LogId),
            Pid;
        _ ->
            undefined
    end.

push(Device, Message) when is_pid(Device)->
    herald_android:push(Device, Message);
push(_, _) ->
    {error, 'invalid-device'}.
