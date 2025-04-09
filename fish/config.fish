set -g fish_greeting

set -x EDITOR vim
set -x SYSTEMD_EDITOR vim
set -x MANPAGER "vim -M +MANPAGER - "

set -x vi

set -U fish_user_paths ~/.local/bin $fish_user_paths

if status is-interactive
  # Commands to run in interactive sessions can go here
end
