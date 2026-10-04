concrete ZaraCanonical of Zara = {
  lincat Utterance, Request, Duration, Unit, App, Reply = {s : Str} ;
  lin
    Say request = request ;
    Address request = request ;
    Polite request = request ;
    AddressPolite request = request ;
    Greet = {s = "hello"} ;
    Help = {s = "help"} ;
    Identity = {s = "who are you"} ;
    Thanks = {s = "thanks"} ;
    Acknowledge = {s = "okay"} ;
    Cancel = {s = "cancel"} ;
    TimerMissing = {s = "timer"} ;
    OpenMissing = {s = "open"} ;
    SetTimer duration = {s = "timer" ++ duration.s} ;
    Followup duration = duration ;
    Correction duration = {s = "actually" ++ duration.s} ;
    OpenApp app = {s = "open" ++ app.s} ;
    DurationOf count unit = {s = count.s ++ unit.s} ;
    Seconds = {s = "seconds"} ;
    Minutes = {s = "minutes"} ;
    Hours = {s = "hours"} ;
    Settings = {s = "settings"} ;
    Termux = {s = "termux"} ;
    Firefox = {s = "firefox"} ;
    GreetingReply = {s = "greeting"} ;
    HelpReply = {s = "help"} ;
    ThanksReply = {s = "thanks"} ;
    AcknowledgedReply = {s = "acknowledged"} ;
    CancelledReply = {s = "cancelled"} ;
    DurationReply = {s = "clarify_duration"} ;
    TargetReply = {s = "clarify_target"} ;
    PendingReply = {s = "dispatch_required"} ;
    UnsupportedReply = {s = "unsupported"} ;
}
