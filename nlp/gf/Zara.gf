abstract Zara = {
  flags startcat = Utterance ;
  cat Utterance ; Request ; Duration ; Unit ; App ; Reply ;
  fun
    Say, Address, Polite, AddressPolite : Request -> Utterance ;
    Greet, Help, Identity, Thanks, Acknowledge, Cancel : Request ;
    TimerMissing, OpenMissing : Request ;
    SetTimer, Followup, Correction : Duration -> Request ;
    OpenApp : App -> Request ;
    DurationOf : Int -> Unit -> Duration ;
    Seconds, Minutes, Hours : Unit ;
    Settings, Termux, Firefox : App ;
    GreetingReply, HelpReply, ThanksReply, AcknowledgedReply : Reply ;
    CancelledReply, DurationReply, TargetReply, PendingReply, UnsupportedReply : Reply ;
}
