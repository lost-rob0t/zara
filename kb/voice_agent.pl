:- module(kb_voice_agent, [control_phrase/2, task_prefix/1, action_verb/1, courtesy/1]).

control_phrase([stop], pause_all).
control_phrase([pause], pause_all).
control_phrase([stop,talking], speech_only).
control_phrase([stop,speaking], speech_only).
control_phrase([be,quiet], speech_only).
control_phrase([stop,all,agents], cancel_all).
control_phrase([stop,all,work], cancel_all).
control_phrase([stop,all,tasks], cancel_all).
control_phrase([cancel,all,tasks], cancel_all).
control_phrase([cancel,everything], cancel_all).
control_phrase([pause,all,agents], pause_all).
control_phrase([pause,all,work], pause_all).
control_phrase([pause,all,tasks], pause_all).
control_phrase([resume,all,agents], resume_all).
control_phrase([resume,all,work], resume_all).
control_phrase([resume,all,tasks], resume_all).
control_phrase([resume,work], resume_all).
control_phrase([list,agents], status).
control_phrase([list,tasks], status).
control_phrase([fleet,status], status).
control_phrase([what,are,my,agents,doing], status).
control_phrase([pause,this,task], pause_task).
control_phrase([cancel,this,task], cancel_task).
control_phrase([resume,this,task], resume_task).

task_prefix([start,a,task,to]).
task_prefix([start,a,new,task,to]).
task_prefix([new,task]).
task_prefix([another,task]).
task_prefix([have,an,agent]).
task_prefix([send,an,agent,to]).
task_prefix([work,on]).

action_verb(research).
action_verb(investigate).
action_verb(implement).
action_verb(build).
action_verb(fix).
action_verb(review).
action_verb(analyze).
action_verb(write).

courtesy([zara]).
courtesy([please]).
courtesy([can,you]).
courtesy([could,you]).
courtesy([also]).
courtesy([and,also]).
