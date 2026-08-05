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
    kdePackages.dolphin
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
  ];
  home.sessionPath = [
    "$HOME/.local/bin"
  ];

  programs.git.enable = true;
  programs.git.extraConfig = {
    credential."https://github.com" = {
        helper = "!gh auth git-credential";
    };
  };
  programs.bash.enable = true;
  programs.home-manager.enable = true;
}
