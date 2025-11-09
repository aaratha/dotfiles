# Dotfiles Setup Guide

## Starship
1. Install Starship
```bash
curl -sS https://starship.rs/install.sh | sh
```
2. Add starship to config.fish
```bash
starship init fish | source
```

## Docker Server Compose
+ Tools: Portainer, Jellyfin, Gluetun, Qbittorrent, Prowlarr, Radarr, FlareSolverr 
+ Make docker and config directories
```bash
mkdir -p ~/docker/config
cd ~/docker/config && mkdir portainer jellyfin \
gluetun qbittorrent prowlarr radarr firesolverr
```

## Tmux
1. Install > > TPM 
```bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```
2. Refresh Tmux: Ctrl+s, I

