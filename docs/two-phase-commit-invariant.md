# Top-Level Invariant

    Definition Inv (d : dstatelet) :=
      exists round l,
      [\/ EverythingInit d round l
       , exists next_data, PhaseOne d round next_data l
       | exists next_data, PhaseTwo d round next_data l].

OK, So at any given instant, the protocol is in one of 3 states.

# Initial State

    Definition EverythingInit (d : dstatelet) (round : nat) (l : Log) : Prop :=
      [/\ cn_state d (round, CInit) l &
          forall pt, pt \in pts ->
                [/\ pt_state d (round, PInit) l pt,
                   no_msg_from_to pt cn (dsoup d) &
                   no_msg_from_to cn pt (dsoup d)]].

  1. The coordinator is in the initial state `(round, CInit)`
  2. Every participant is the initial state `(round, PInit)` and there are no undelivered messages between participants and coordinators.

Consequences on simulation:
  1. My simulation needs to be able to express the soup! Maybe... Or maybe, I can just inspect the _incomming messages_ field of each node.
  2. My protocol needs to speak of rounds.

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
  3. Participant is in state `(round, RespondedYes)`
  4. Participant is in state `(round, RespondedNo)`

    Definition pt_PhaseOneResponded (d : dstatelet) (round : nat) (next_data : data)
               (l : Log) (committed : bool) (pt : nid) : Prop :=
      [/\ no_msg_from_to cn pt (dsoup d), no_msg_from_to pt cn (dsoup d) &
          if committed
          then pt_state d (round, PRespondedYes next_data) l pt
          else pt_state d (round, PRespondedNo next_data) l pt].


Definition pt_Init d round l pt :=
  [/\ pt_state d (round, PInit) l pt,
     no_msg_from_to pt cn (dsoup d) &
     no_msg_from_to cn pt (dsoup d)].

Definition cn_PhaseOneSend d round next_data l sent :=
    [/\ cn_state d (round, CSentPrep next_data sent) l,
     uniq sent, {subset sent <= pts} &
                forall pt, pt \in pts ->
                           if pt \in sent
                           then pt_PhaseOne d round next_data l pt
                           else pt_Init d round l pt].

Definition cn_PhaseOneReceive d round next_data l recvd :=
     let rps := map fst recvd in
     [/\ cn_state d (round, CWaitPrepResponse next_data recvd) l,
      uniq rps, {subset rps <= pts} ,
      forall pt b, pt \in pts -> (pt, b) \in recvd ->
                   pt_PhaseOneResponded d round next_data l b pt &
      forall pt,   pt \in pts -> pt \notin rps ->
                   pt_PhaseOne d round next_data l pt].

Definition PhaseOne (d : dstatelet) (round : nat) (next_data : data) (l : Log) :=
  (exists sent, cn_PhaseOneSend d round next_data l sent) \/
  (exists recvd, cn_PhaseOneReceive d round next_data l recvd).

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