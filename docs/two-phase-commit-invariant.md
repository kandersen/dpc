# Questions

* What is this `next_data` that's mentioned all over the place?
* What does the log track/enforce?

# Consequences on simulation:

  1. My simulation needs to be able to express the soup! Maybe... Or maybe, I can just inspect the _incomming messages_ field of each node. Add them all up and you have the undelivered part of the soup.
  2. My protocol needs to speak of rounds.
  3. My simulation has broadcasting as one atomic operation. The invariant below is more finegrained.

# Top-Level Invariant

    Definition Inv (d : dstatelet) :=
      exists round l,
      [\/ EverythingInit d round l
       , exists next_data, PhaseOne d round next_data l
       | exists next_data, PhaseTwo d round next_data l].

OK, So at any given instant, the protocol is in one of 3 states:

  1. The initial state
  2. in phase 1, for some `round`
  3. in phase 2, for some `round`

# Initial State

    Definition EverythingInit (d : dstatelet) (round : nat) (l : Log) : Prop :=
      [/\ cn_state d (round, CInit) l &
        forall pt, pt \in pts ->
          [/\ pt_state d (round, PInit) l pt,
              no_msg_from_to pt cn (dsoup d) &
              no_msg_from_to cn pt (dsoup d)]].

  1. The coordinator is in the initial state `(round, CInit)`
  2. Every participant is the initial state `(round, PInit)` and there are no undelivered messages between participants and coordinators.

# Phase 1

    Definition pt_PhaseOne (d : dstatelet) (round : nat)
               (next_data : data) (l : Log) (pt : nid) : Prop :=
      [\/ [/\ pt_state d (round, PInit) l pt,
           no_msg_from_to pt cn (dsoup d) &
           msg_spec cn pt prep_req (round :: next_data) (dsoup d)]
       ,  [/\ pt_state d (round, PGotRequest next_data) l pt,
           no_msg_from_to pt cn (dsoup d) &
           no_msg_from_to cn pt (dsoup d)]
       ,  [/\ pt_state d (round, PRespondedYes next_data) l pt,
           no_msg_from_to cn pt (dsoup d) &
           msg_spec pt cn prep_yes [::round] (dsoup d)]
       |  [/\ pt_state d (round, PRespondedNo next_data) l pt,
           no_msg_from_to cn pt (dsoup d) &
           msg_spec pt cn prep_no [::round] (dsoup d)]].

One of the following must hold for a given participant `pt`:

  1. Participant is in state `(round, PInit)`, there are no undelivered messages to the coordinator from `pt` and there is a Prep Request message undelivered to the participant from the coordinator.
  2. Participant is in state `(round, PGotRequest)`, and there is mutually no undelivered messages between participants and coordinator.
  3. Participant is in state `(round, RespondedYes)`, there is no message from the coordinator to the participant and the participant has sent `yes`
  4. Participant is in state `(round, RespondedNo)`, there is no message from the coordinator to the participant and the participant has sent `no`

```
Definition pt_PhaseOneResponded (d : dstatelet) (round : nat) (next_data : data) (l : Log) (committed : bool) (pt : nid) : Prop :=
  [/\ no_msg_from_to cn pt (dsoup d), no_msg_from_to pt cn (dsoup d) &
      if committed
        then pt_state d (round, PRespondedYes next_data) l pt
        else pt_state d (round, PRespondedNo next_data) l pt].
```

All of the following must hold,

  1. Mutually no outstanding messages between participant and coordinator
  2. if `comiited`, then participant must be in the RespondedYes, otherwise No state

```
Definition pt_Init d round l pt :=
  [/\ pt_state d (round, PInit) l pt,
     no_msg_from_to pt cn (dsoup d) &
     no_msg_from_to cn pt (dsoup d)].
```

A participant in the initial state is in the `(round, PInit)` state and 

  1. there are no outstanding messages between coordinator and the participant

```
Definition PhaseOne (d : dstatelet) (round : nat) (next_data : data) (l : Log) :=
  (exists sent, cn_PhaseOneSend d round next_data l sent) \/
  (exists recvd, cn_PhaseOneReceive d round next_data l recvd).
```

Phase One begins as soon as the coordinator sends the (first) prep message. My simulation sends all at once, so as soon as the prep is broadcast, participants move to phase one as well.

```
Definition cn_PhaseOneSend d round next_data l sent :=
    [/\ cn_state d (round, CSentPrep next_data sent) l,
     uniq sent, {subset sent <= pts} &
                forall pt, pt \in pts ->
                           if pt \in sent
                           then pt_PhaseOne d round next_data l pt
                           else pt_Init d round l pt].
```

As responses start coming back, the coordinator keeps track of who have responded. The ones that have are in phase one, or the participants have moved to having responded.

```
Definition cn_PhaseOneReceive d round next_data l recvd :=
     let rps := map fst recvd in
     [/\ cn_state d (round, CWaitPrepResponse next_data recvd) l,
      uniq rps, {subset rps <= pts} ,
      forall pt b, pt \in pts -> (pt, b) \in recvd ->
                   pt_PhaseOneResponded d round next_data l b pt &
      forall pt,   pt \in pts -> pt \notin rps ->
                   pt_PhaseOne d round next_data l pt].
```

# Phase 2

Definition pt_PhaseTwoCommit d round next_data l pt :=
  [\/ [/\ pt_state d (round, PRespondedYes next_data) l pt,
       msg_spec cn pt commit_req [::round] (dsoup d) &
       no_msg_from_to pt cn (dsoup d)]
   , [/\ pt_state d (round, PCommitted next_data) (rcons l (true, next_data)) pt,
      no_msg_from_to cn pt (dsoup d) & no_msg_from_to pt cn (dsoup d)]
  | [/\ pt_state d (round.+1, PInit) (rcons l (true, next_data)) pt,
     no_msg_from_to cn pt (dsoup d) &
     msg_spec pt cn commit_ack [::round] (dsoup d)
  ]].

Definition pt_PhaseTwoAbort d round next_data l pt :=
  [\/ [/\ (pt_state d (round, PRespondedYes next_data) l pt \/
           pt_state d (round, PRespondedNo next_data) l pt),
       msg_spec cn pt abort_req [::round] (dsoup d) &
       no_msg_from_to pt cn (dsoup d)]
   , [/\ pt_state d (round, PAborted next_data) (rcons l (false, next_data)) pt,
      no_msg_from_to cn pt (dsoup d) & no_msg_from_to pt cn (dsoup d)]
  | [/\ pt_state d (round.+1, PInit) (rcons l (false, next_data)) pt,
     no_msg_from_to cn pt (dsoup d) &
     msg_spec pt cn abort_ack [::round] (dsoup d)
  ]].

Definition pt_PhaseTwoResponded d round next_data l b pt :=
  [/\ pt_state d (round.+1, PInit) (rcons l (b, next_data)) pt,
   no_msg_from_to cn pt (dsoup d) & no_msg_from_to pt cn (dsoup d)].


Definition cn_PhaseTwoSendCommits d round next_data l sent :=
     [/\ cn_state d (round, CSentCommit next_data sent) l,
      uniq sent, {subset sent <= pts} &
       forall pt, pt \in pts ->
       if pt \in sent
       then pt_PhaseTwoCommit d round next_data l pt
       else pt_PhaseOneResponded d round next_data l true pt].
       
Definition cn_PhaseTwoSendAborts d round next_data l sent :=
     [/\ cn_state d (round, CSentAbort next_data sent) l,
      uniq sent, {subset sent <= pts} &
      forall pt, pt \in pts ->
      if pt \in sent
      then pt_PhaseTwoAbort d round next_data l pt
      else exists b, pt_PhaseOneResponded d round next_data l b pt].

Definition cn_PhaseTwoReceiveCommits d round next_data l recvd :=
      [/\ cn_state d (round, CWaitAckCommit next_data recvd) l,
       uniq recvd, {subset recvd <= pts} &
       forall pt, pt \in pts ->
       if pt \in recvd
       then pt_PhaseTwoResponded d round next_data l true pt
       else pt_PhaseTwoCommit d round next_data l pt].

Definition cn_PhaseTwoReceiveAborts d round next_data l recvd :=
     [/\ cn_state d (round, CWaitAckAbort next_data recvd) l,
      uniq recvd, {subset recvd <= pts} &
      forall pt, pt \in pts ->
      if pt \in recvd
      then pt_PhaseTwoResponded d round next_data l false pt
      else pt_PhaseTwoAbort d round next_data l pt].

Definition PhaseTwoCommit d round next_data lg :=
  [\/ exists sent : seq nid, cn_PhaseTwoSendCommits d round next_data lg sent |
     exists recvd : seq nid, cn_PhaseTwoReceiveCommits d round next_data lg recvd ].

Definition PhaseTwoAbort d round next_data lg :=
  [\/ exists sent : seq nid, cn_PhaseTwoSendAborts d round next_data lg sent |
     exists recvd : seq nid, cn_PhaseTwoReceiveAborts d round next_data lg recvd ].

Definition PhaseTwo (d : dstatelet) (round : nat) (next_data : data) (l : Log) :=
  [\/ exists sent, cn_PhaseTwoSendCommits d round next_data l sent,
     exists sent, cn_PhaseTwoSendAborts d round next_data l sent,
     exists recvd, cn_PhaseTwoReceiveCommits d round next_data l recvd |
   exists recvd, cn_PhaseTwoReceiveAborts d round next_data l recvd].