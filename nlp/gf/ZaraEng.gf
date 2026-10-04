concrete ZaraEng of Zara = NumeralEng [Digits, Dig, IDig, IIDig, D_0, D_1, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9] ** open SyntaxEng, ParadigmsEng in {
  lincat
    Utterance, Request, Duration, Reply = {s : Str} ;
    Unit = N ;
    App = NP ;
  lin
    Say request = request ;
    Address request = {s = variants {"hey zara" ; "zara"} ++ request.s} ;
    Polite request = {s = "please" ++ request.s} ;
    AddressPolite request = {s = variants {"hey zara" ; "zara"} ++ "please" ++ request.s} ;
    Greet = {s = variants {"hello" ; "hi"}} ;
    Help = {s = variants {"help" ; "help me" ; "what can you do"}} ;
    Identity = {s = "who are you"} ;
    Thanks = {s = variants {"thanks" ; "thank you"}} ;
    Acknowledge = {s = variants {"okay" ; "ok" ; "understood"}} ;
    Cancel = {s = variants {"cancel" ; "cancel that" ; "never mind"}} ;
    TimerMissing = {s = variants {"set a timer" ; "timer"}} ;
    OpenMissing = {s = "open"} ;
    SetTimer duration = {s = variants {"set a timer for" ; "start a timer for"} ++ duration.s} ;
    Followup duration = duration ;
    Correction duration = {s = "actually" ++ duration.s} ;
    OpenApp app = mkUtt (mkImp (mkVP (mkV2 (mkV "open")) app)) ;
    DurationOf count unit = mkUtt (mkNP (mkDet (mkCard count)) unit) ;
    Seconds = mkN "second" ;
    Minutes = mkN "minute" ;
    Hours = mkN "hour" ;
    Settings = mkNP (mkPN "settings") ;
    Termux = mkNP (mkPN "termux") ;
    Firefox = mkNP (mkPN "firefox") ;
    GreetingReply = {s = "Hello. What would you like to do?"} ;
    IdentityReply = {s = "I am a symbolic assistant powered by Prolog."} ;
    HelpReply = {s = "I can help with timers and opening apps. What would you like to do?"} ;
    ThanksReply = {s = "You are welcome."} ;
    AcknowledgedReply = {s = "Understood."} ;
    CancelledReply = {s = "The pending request is cancelled."} ;
    DurationReply = {s = "How long should the timer run?"} ;
    TargetReply = {s = "Which app would you like to open?"} ;
    PendingReply = {s = "I understood the request. It still needs execution."} ;
    UnsupportedReply = {s = "I do not have a symbolic answer for that yet."} ;
}
