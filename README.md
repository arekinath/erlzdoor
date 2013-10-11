
A binding for SmartOS libzdoor for erlang.

## How it works

This is a NIF binding that defers most of its work off onto a worker thread (spawned at startup). The worker thread handles requests to open/close a door. When a door is opened, the process that called zdoor:open/2 "owns" the door from then on.

The opened door spawns its own threads behind the scenes to handle actual requests (these are injected by the kernel). We convert these into Erlang messages (while the handler thread is blocked on a wait condition), and send them to the owner process.

Finally, the owner process calls into the zdoor:reply/2 NIF and this unlocks the door server thread and returns the given reply value back to the door. The owner process can also use zdoor:req_info/1 to identify the zone and door that sent the particular request.

## Example

    #!/usr/bin/env escript
    %%! -smp enable -pa ebin/

    do_door() ->
      receive
        {zdoor, Req, Bin} ->
          Data = binary:part(Bin, {0, byte_size(Bin)-1}),
          Info = zdoor:req_info(Req),
          io:format("data = ~p\ndoor = ~p\n", [Data, Info]),
          zdoor:reply(Req, <<"1\n">>),
          do_door()
      end.

    main([Zone]) ->
      zdoor:open(Zone, "_joyent_sshd_key_is_authorized"),
      do_door().

Running this example script (we'll call it 'test.es'):

    [root@test ~/erlzdoor]# ./test.es testzone
    data = <<"admin 100 05:1c:eb:88:35:5c:95:78:94:c3:95:60:f4:c6:ea:57">>
    door = {zdoor_req,"testzone","_joyent_sshd_key_is_authorized",
                      <0.2.0>}
    data = <<"admin 100 05:1c:eb:88:35:5c:95:78:94:c3:95:60:f4:c6:ea:57">>
    door = {zdoor_req,"testzone","_joyent_sshd_key_is_authorized",
                      <0.2.0>}
