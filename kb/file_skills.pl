:- module(kb_file_skills, [file_skill/3]).

% skill, LangChain tool, access class. The executor enforces the configured
% repository boundary; this KB declares only the available operations.
file_skill(read, read_file, read).
file_skill(write, write_file, write).
file_skill(diff, diff_file, read).
file_skill(list, list_dir, read).
