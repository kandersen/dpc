### Third Attempt

We have two states for the server - it is performs a receive from one to the other, and a send to go back again. The initial state is the one awaiting the receive. Receiving the request from a client yields the obligation to send a message `MustSend` which is linear. That is, it can and must be consumed by sending a message.

If we start in the `ServerIdle` state without any resources (specifically the `MustSend`), we can reason that either no transition has ever happened, or the next receive we do must be from the `ServerIdle` state. I.e.
```
Inv(Nothing) = True
Inv(Just (client, ns)) = MustSend

listener :: Label -> OPB Job -> M a
listener l r = do
  { r : OPB Job Inv }
  mmsg <- receive[ServerReceive(mapInc)] l  <<<----- (1)
  { r : OPB Job Inv /\ (mmsg = Nothing \/ (mmsg = Just msg /\ MustSend(l))) }
  case mmsg of
    Nothing ->
      { r : OPB Job Inv }
      return ()
      { r : OPB Job Inv}      
    Just (_, client, ns, _) ->
      { r : OPB Job Inv /\ MustSend(l) } 
      put (client, ns) r
      { r : OPB Job Inv }
  { r : OPB Job Inv }
  listener l r
  { False }

handler :: Label -> OPB Job -> M a
handler l r = do
  { r : OPB Job Inv }
  (client, ns) <- take r
  { r : OPB Job Inv /\ MustSend(l) }
  send[ServerResponse(mapInc)] l client ((+1) <$> ns) <<<---- (2)
  { r : OPB Job Inv }
  handler l r
  { False }
```

There are 2 points in these programs where we must take neighbouring threads' network interference into account - (1) and (2). The send and receives are atomic network operations, so are the only places we need to take interference into account.

At (2), we own `MustSend` which is incompatible with any other guards - the rely is empty (or, I guess, reflexive): the local environment cannot take any protlet actions for the local node. We must be in the state `ServerJob` and we hold the obligation to respond.

At (1), we own no obligations, so we don't know where in the network we are. They may be other threads racing to receive the same message we are trying to receive, but if they have, then they have also gained the obligation to respond to it, and, hence, must have ensured that the obligation is fulfilled. When we eventually do receive the appointed message, we obtain the obligation to send, and hence know we are in the `ServerJob` state. The rely before this program point is thus the total relation - the neighbouring threads can do anything - and the same as (2) after this receive.

The specs of send and receive for the ARPC protlet is thus something like

```
  receive[ReceiveARPC(p)] l :
    { emp }
    { ret. exists s. 
        if ret == Nothing 
          then emp
          else ret = Just msg /\
               p.serverReceive(msg, s) = Just s' /\
               n |->_l Running s' /\
               MustSend(l, p.serverSend) }

  send[RespondARPC(p)] l nid msg : 
    { MustSend(l,p.serverSend) /\ 
      n |->_l Running s /\
      p.serverSend(s) = Just(nid, msg, s') }
    { ret. ret = () /\ n |->_l Running s' }             
```


