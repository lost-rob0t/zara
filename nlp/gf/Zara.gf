abstract Zara = Numeral [Digits, Dig, IDig, IIDig, D_0, D_1, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9] ** {
  flags startcat = Utterance ;
  cat Utterance ; Request ; Duration ; Unit ; App ; Reply ;
  fun
    Say, Address, Polite, AddressPolite : Request -> Utterance ;
    Greet, Help, Identity, Thanks, Acknowledge, Cancel : Request ;
    TimerMissing, OpenMissing : Request ;
    SetTimer, Followup, Correction : Duration -> Request ;
    OpenApp : App -> Request ;
    DurationOf : Digits -> Unit -> Duration ;
    Seconds, Minutes, Hours : Unit ;
    Settings, Termux, Firefox : App ;
    GreetingReply, IdentityReply, HelpReply, ThanksReply, AcknowledgedReply : Reply ;
    CancelledReply, DurationReply, TargetReply, PendingReply, UnsupportedReply : Reply ;
}
