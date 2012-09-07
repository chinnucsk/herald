-module(herald).

-export([push/2, new_device/3, set_device_address/2, stop/1]).

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

set_device_address(Device, DeviceAddress) ->
    herald_android:set_registration_id(Device, DeviceAddress).

stop(Device) ->
    herald_android:stop(Device).
