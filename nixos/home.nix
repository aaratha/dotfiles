{ config, pkgs, ... }:


{
  home.username = "aaratha";
  home.homeDirectory = "/home/aaratha";
  home.stateVersion = "26.05";
  
  home.packages = with pkgs; [
    kitty
    firefox
    neovim
    git
    waybar
    fastfetch
    rofi
    dunst
    networkmanagerapplet
    slurp
    brightnessctl
    pavucontrol
    starship
    eza
    home-manager
    stow
    tmux
    zoxide
    emacs-pgtk
    nerd-fonts.jetbrains-mono
    nerd-fonts.roboto-mono
    nodejs
    quickshell
    matugen
    hyprcursor
    rose-pine-hyprcursor
    rustup
    copilot-language-server
    prettier
    ripgrep
    fd
    lazygit
    fzf
    claude-agent-acp
    python3
    claude-code
    gh
    rclone
    claude-agent-acp
    copilot-language-server
    ghostty
    inkscape
    texlive.combined.scheme-medium
    bespokesynth
    # nur.repos.Ev357.helium
    nmap
    glow
    wlogout
    proton-vpn
    qbittorrent
    unrar
    bat
    smartmontools
    google-chrome
    spotify-player
    ncspot
    nautilus
    broot
    gdu
    appimage-run
  ];
  home.sessionPath = [
    "$HOME/.local/bin"
  ];


  programs.git.enable = true;
  programs.git.settings = {
    credential."https://github.com" = {
      helper = "!gh auth git-credential";
    };
    user.name  = "Aseem Ratha";
    user.email = "aseemratha@gmail.com";
  };
  programs.bash.enable = true;
  programs.home-manager.enable = true;
  systemd.user.services.rclone-gdrive = {
    Unit = {
      Description = "rclone mount for Google Drive";
      After = [ "network-online.target" ];
      Wants = [ "network-online.target" ];
    };
    Service = {
      Type = "notify";
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p %h/mnt/gdrive";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone mount remote: %h/mnt/gdrive \
          --vfs-cache-mode writes \
          --vfs-cache-max-age 24h \
          --dir-cache-time 72h \
          --poll-interval 15s
      '';
      ExecStop = "${pkgs.fuse}/bin/fusermount -u %h/mnt/gdrive";
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install.WantedBy = [ "default.target" ];
  };
  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "lock";
        action = "loginctl lock-session";
        text = "Lock";
        keybind = "l";
      }
      {
        label = "hibernate";
        action = "systemctl hibernate";
        text = "Hibernate";
        keybind = "h";
      }
      {
        label = "suspend";
        action = "systemctl suspend";
        text = "Suspend";
        keybind = "s";
      }
      {
        label = "logout";
        action = "hyprctl dispatch exit";
        text = "Logout";
        keybind = "e";
      }
      {
        label = "shutdown";
        action = "systemctl poweroff";
        text = "Shutdown";
        keybind = "p";
      }
      {
        label = "reboot";
        action = "systemctl reboot";
        text = "Reboot";
        keybind = "r";
      }
    ];
  };
}
