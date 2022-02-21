Contract event logging
======================

Contract event logging is a way for contracts to deliver event-like information to external application.
It is an important pattern in contract writing to allow external applications to respond to communication
from Tezos contracts to effect changes in application states outside Tezos.
There is blossoming use of event logs in basic indexing services and cross-chain bridges or applications.
In this document, we will explain how event logs are supported in Tezos contract on the Michelson level.

Event
-----
A contract event entry in Tezos consists of the following data.

- An event ``tag`` of type ``string``, which is an express way to deliver human-readable message to indexers
  and off-chain consumers
- An event ``data`` of type ``event``
  which is declared by the emitting contract in a similar way to declaration of ``storage`` and ``parameter``

Each successful contract execution attaches a list of contract events arranged in the chronological order
to the transaction receipt made ready for consumption by services observing the chain.

Runtime semantics
-----------------
To support event logging, an instruction ``EMIT`` is introduced into the Michelson language.
In this case, event emitting contracts are obliged to declare an event type at its top-level Michelson code,
along with the compulsory ``storage`` and ``parameter``.
Other contracts not emitting any events may omit this declaration, and a ``never`` type will be assigned to
the ``event`` type in lieu.

Events can be emitted at places other than views. Views are short computation on immutable contract data.
Emitting events in this case hardly produces useful information while introducing unneccesary latency to
the computation.

Emitting events is also allowed in bodies of new contracts to be created via ``Icreate_contract``.

Retrieving events
-----------------
Events successfully emitted can be read off directly from transaction results.
This is typically achieved by making JSON RPCs to the block service.
It will return a list of operations, each including the event entries with the information above.

Here is a sample result from a call.

::

    {
      "protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
      "chain_id": "NetXdQprcVkpaWU",
      "hash": "opNX59asPwNZGu2kFiHFVJzn7QwtyaExoLtxdZSm3Q3o4jDdSmo",
      "branch": "BLockGenesisGenesisGenesisGenesisGenesisf79b5d1CoW2",
      "contents": [
        {
          "kind": "transaction",
          // ... elided for brevity
          "metadata": {
            // ... elided for brevity
            "operation_result": {
              "status": "applied",
              // ... elided for brevity
              "events": [                                           // <~
                {                                                   // <~
                  "tag": "tag1",                                    // <~
                  "data": {                                         // <~
                    "prim": "Right",                                // <~
                    "args": [                                       // <~
                      {                                             // <~
                        "string": "right"                           // <~
                      }                                             // <~
                    ]                                               // <~
                  }                                                 // <~
                }                                                   // <~
              ]                                                     // <~
            }
          }
        }
      ],
      "signature": "<signature>"
    }

Similarly, event type declarations can be extracted by interfacing with the contract RPC,
which is available in the Michelson script of the contract under the `event` primitive.
