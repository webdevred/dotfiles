set -g fish_greeting

set -x EDITOR vim
set -x SYSTEMD_EDITOR vim
set -x LESS '-N -M -R --shift 5'

set -U fish_autosuggestion_enabled 0
set -x vi

set -U fish_user_paths ~/.local/bin $fish_user_paths

if status is-interactive
  # Commands to run in interactive sessions can go here
end
