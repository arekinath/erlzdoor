
A binding for SmartOS libzdoor for erlang.

## How it works

This is a NIF binding that defers most of its work off onto a worker thread (spawned at startup). The worker thread handles requests to open/close a door. When a door is opened, the process that called zdoor:open/2 "owns" the door from then on.

The opened door spawns its own threads behind the scenes to handle actual requests (these are injected by the kernel). We convert these into Erlang messages (while the handler thread is blocked on a wait condition), and send them to the owner process.

Finally, the owner process calls into the zdoor:reply/2 NIF and this unlocks the door server thread and returns the given reply value back to the door.

(currently the owner process of a door cannot be changed once opened, and only one request/reply sequence can be in progress at once)

## Example

    1> zdoor:open("zonetest", "_joyent_sshd_key_is_authorized").
    ok
    3> flush().
    Shell got {zdoor,1,
                     <<97,100,109,105,110,32,49,48,48,32,48,53,58,49,99,58,101,98,
                       58,56,56,58,51,53,58,53,99,58,57,53,58,55,56,58,57,52,58,
                       99,51,58,57,53,58,54,48,58,102,52,58,99,54,58,101,97,58,53,
                       55,0>>}
    ok
    4> zdoor:reply(1,<<"0\n">>).
    ok
    7> flush().
    Shell got {zdoor,2,
                     <<97,100,109,105,110,32,49,48,48,32,48,53,58,49,99,58,101,98,
                       58,56,56,58,51,53,58,53,99,58,57,53,58,55,56,58,57,52,58,
                       99,51,58,57,53,58,54,48,58,102,52,58,99,54,58,101,97,58,53,
                       55,0>>}
    ok
    8> zdoor:reply(2,<<"1\n">>).
    ok
