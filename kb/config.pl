:- module(kb_config,
    [
        app_mapping/2,
        direct_app/1,
        todo_destination/1,
        todo_context_mode/1,
        search_engine/1,
        dictation_command/1,
        dictation_stop_phrase/1

    ]).

:- use_module('../modules/config_loader').
:- discontiguous kb_config:app_mapping/2.
:- initialization(config_loader:load_user_config).

% ============================================================
% ZARATHUSTRA DEFAULT CONFIGURATION
% ============================================================
% This configuration provides sensible defaults that work across
% most Linux distributions. Users can override these by creating
% ~/.config/zarathushtra/config.pl with their own definitions.

% ---- TODO Settings ----
% Where to store TODO entries (Org-mode format)
todo_destination("~/todo.org").

% Context inference mode for TODO categorization
% Options: infer | infer_with_llm | llm_only
todo_context_mode(infer).

% Search engine template for the `search` intent.
% Users can override this in ~/.zarathushtra/config.pl
search_engine("https://duckduckgo.com/?q=~w").

% ---- Dictation (zara-dictate) ----
% Command used to start dictation. Users can override this in ~/.zarathushtra/config.pl
% Examples:
%   dictation_command("zara-dictate tiny cpu").
%   dictation_command("zara-dictate small cpu 16 2 'end voice'").
%   dictation_command("ZARA_STOP_PHRASE='end voice' zara-dictate small cpu").

dictation_command("zara-dictate").

% Spoken stop phrases (case-insensitive). Used by the Prolog intent matcher.
% Add as many as you like; each fact is checked.
dictation_stop_phrase("end voice").
dictation_stop_phrase("stop voice").
dictation_stop_phrase("stop voice mode").
dictation_stop_phrase("stop dictation").
dictation_stop_phrase("end dictation").

% ============================================================
% WEB APPLICATIONS
% ============================================================
% Common web services via Firefox

app_mapping(youtube, "xdg-open --new-window https://youtube.com").
app_mapping(github, "xdg-open --new-window https://github.com").
app_mapping(reddit, "xdg-open --new-window https://reddit.com").
app_mapping(gmail, "xdg-open --new-window https://gmail.com").
app_mapping(maps, "xdg-open --new-window https://maps.google.com").
app_mapping(translate, "xdg-open --new-window https://translate.google.com").
app_mapping(calendar, "xdg-open --new-window https://calendar.google.com").
app_mapping(drive, "xdg-open --new-window https://drive.google.com").
app_mapping(twitter, "xdg-open --new-window https://twitter.com").
app_mapping(linkedin, "xdg-open --new-window https://linkedin.com").

% ============================================================
% CORE APPLICATIONS
% ============================================================

% Text Editors
app_mapping(editor, "vim").
app_mapping(vim, "vim").
app_mapping(nvim, "nvim").
app_mapping(nano, "nano").
app_mapping(gedit, "gedit").
app_mapping(kate, "kate").
app_mapping(code, "code").  % VS Code

% Terminals
app_mapping(terminal, "x-terminal-emulator").  % Uses system default
app_mapping(gnome_terminal, "gnome-terminal").
app_mapping(konsole, "konsole").
app_mapping(xterm, "xterm").
app_mapping(alacritty, "alacritty").

% File Managers
app_mapping(files, "xdg-open ~").
app_mapping(nautilus, "nautilus").
app_mapping(dolphin, "dolphin").
app_mapping(thunar, "thunar").
app_mapping(pcmanfm, "pcmanfm").

% Browser
app_mapping(browser, "xdg-open").
app_mapping(xdg-open, "xdg-open").
app_mapping(chrome, "google-chrome").
app_mapping(chromium, "chromium").

% ============================================================
% MEDIA APPLICATIONS
% ============================================================

% Audio/Video Players
app_mapping(music, "rhythmbox").
app_mapping(video, "vlc").
app_mapping(vlc, "vlc").
app_mapping(mpv, "mpv").

% Media Services
app_mapping(spotify, "xdg-open --new-window https://open.spotify.com").
app_mapping(netflix, "xdg-open --new-window https://netflix.com").
app_mapping(twitch, "xdg-open --new-window https://twitch.tv").

% ============================================================
% COMMUNICATION
% ============================================================

app_mapping(email, "thunderbird").
app_mapping(mail, "thunderbird").
app_mapping(chat, "xdg-open --new-window https://discord.com/app").
app_mapping(slack, "xdg-open --new-window https://slack.com").
app_mapping(teams, "xdg-open --new-window https://teams.microsoft.com").
app_mapping(zoom, "zoom").

% ============================================================
% PRODUCTIVITY
% ============================================================

% Office Suite
app_mapping(word, "libreoffice --writer").
app_mapping(writer, "libreoffice --writer").
app_mapping(calc, "libreoffice --calc").
app_mapping(excel, "libreoffice --calc").
app_mapping(spreadsheet, "libreoffice --calc").
app_mapping(impress, "libreoffice --impress").
app_mapping(powerpoint, "libreoffice --impress").

% Note-taking
app_mapping(notes, "gedit ~/notes.txt").
app_mapping(notepad, "gedit").

% PDF Viewer
app_mapping(pdf, "evince").
app_mapping(evince, "evince").

% ============================================================
% DEVELOPMENT
% ============================================================

% IDEs
app_mapping(ide, "code").
app_mapping(vscode, "code").
app_mapping(pycharm, "pycharm").
app_mapping(intellij, "idea").

% Version Control
app_mapping(git, "x-terminal-emulator -e 'git status; bash'").
app_mapping(gitk, "gitk").

% Databases
app_mapping(sqlite, "sqlitebrowser").

% ============================================================
% GRAPHICS & DESIGN
% ============================================================

app_mapping(gimp, "gimp").
app_mapping(inkscape, "inkscape").
app_mapping(blender, "blender").
app_mapping(krita, "krita").

% ============================================================
% SYSTEM UTILITIES
% ============================================================

% System Monitoring
app_mapping(task_manager, "gnome-system-monitor").
app_mapping(system_monitor, "gnome-system-monitor").
app_mapping(htop, "x-terminal-emulator -e htop").
app_mapping(top, "x-terminal-emulator -e top").

% System Settings
app_mapping(settings, "gnome-control-center").
app_mapping(network, "nm-connection-editor").
app_mapping(sound, "pavucontrol").
app_mapping(volume, "pavucontrol").

% Screenshots
app_mapping(screenshot, "gnome-screenshot -i").
app_mapping(screen_record, "simplescreenrecorder").

% ============================================================
% PACKAGE MANAGEMENT
% ============================================================

% Common package managers (will try to detect which one works)
app_mapping(software, "gnome-software").
app_mapping(packages, "gnome-software").
app_mapping(updates, "gnome-software --mode=updates").

% ============================================================
% GAMING & ENTERTAINMENT
% ============================================================

app_mapping(steam, "steam").
app_mapping(games, "steam").

% ============================================================
% DIRECTORY SHORTCUTS
% ============================================================

app_mapping(downloads, "xdg-open ~/Downloads").
app_mapping(documents, "xdg-open ~/Documents").
app_mapping(pictures, "xdg-open ~/Pictures").
app_mapping(videos, "xdg-open ~/Videos").
app_mapping(music_folder, "xdg-open ~/Music").
app_mapping(desktop, "xdg-open ~/Desktop").
app_mapping(home, "xdg-open ~").

% ============================================================
% POWER MANAGEMENT
% ============================================================

app_mapping(lock, "xdg-screensaver lock").
app_mapping(logout, "gnome-session-quit").
app_mapping(shutdown, "systemctl poweroff").
app_mapping(reboot, "systemctl reboot").
app_mapping(suspend, "systemctl suspend").
app_mapping(hibernate, "systemctl hibernate").

% ============================================================
% DIRECT APPS
% ============================================================
% Applications that can be launched directly by name
% without requiring an app_mapping entry

direct_app(xdg-open).
direct_app(chrome).
direct_app(chromium).
direct_app(vim).
direct_app(nvim).
direct_app(nano).
direct_app(emacs).
direct_app(gedit).
direct_app(kate).
direct_app(code).
direct_app(terminal).
direct_app('gnome-terminal').
direct_app(konsole).
direct_app(xterm).
direct_app(alacritty).
direct_app(kitty).
direct_app(gimp).
direct_app(inkscape).
direct_app(blender).
direct_app(vlc).
direct_app(rhythmbox).
direct_app(thunderbird).
direct_app(libreoffice).
direct_app(steam).
direct_app(discord).
direct_app(zoom).
direct_app(htop).
direct_app(top).
