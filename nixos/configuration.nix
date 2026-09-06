# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:


{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.efi.canTouchEfiVariables = true;

  systemd.sleep.settings.Sleep = {
    AllowSuspend = "yes";
    AllowHibernation = "yes";
    AllowHybridSleep = "yes";
    AllowSuspendThenHibernate = "yes";
  };

  systemd.sleep.settings.Sleep = {
    HibernateDelaySec = "1h";
  };

  services.logind.settings.Login = {
    IdleAction = "hibernate";        # or "suspend", "poweroff", "ignore"
    IdleActionSec = "15min";
    HandleLidSwitch = "suspend";
    HandlePowerKey = "hibernate";
  };

  services.timesyncd.enable = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.nameservers = [ "1.1.1.1" "8.8.8.8" ];

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;
  networking.networkmanager.insertNameservers = ["1.1.1.1" "8.8.8.8"];

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users."aaratha" = {
    isNormalUser = true;
    description = "Aseem Ratha";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
    shell = pkgs.fish;
  };

  fileSystems."/mnt/hdd" = {
    device = "/dev/disk/by-uuid/01DA933CDEFE0080";
    fsType = "ntfs-3g"; # "ntfs3";  # modern in-kernel NTFS driver, faster than ntfs-3g
    options = [ "rw" "uid=1000" "gid=100" "umask=000" "nofail" ];
  };
  fileSystems."/mnt/windows" = {
    device = "/dev/disk/by-uuid/ACBCCF82BCCF4614";
    fsType = "ntfs3";
    options = [ "rw" "uid=1000" "gid=100" "umask=022" "nofail" ];
  };  # allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
    curl
    btop
    unzip
    pciutils
    usbutils
    file
    tree 
    clang
    libclang
    gcc
    gnumake
    xdg-utils
    xrandr
    xdpyinfo
    xclip
    xsel
    parted
    fish
    awww
    tree-sitter
    lua-language-server
    libvterm
    glib.dev
    cmake
    libtool
    nix-search
    prl-tools
    wl-clipboard
    efibootmgr
    steam
    pkg-config
    direnv
    nix-direnv
    # Dev dependencies
    wayland.dev
    wayland-protocols
    wayland-scanner
    libxkbcommon.dev
    libdecor.dev
    libX11.dev
    libXcursor.dev
    libXi.dev
    libXrandr.dev
    libXext.dev
    libGL.dev
    freetype.dev
  ];

  environment.variables = {
    LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
    PKG_CONFIG_PATH = "/run/current-system/sw/lib/pkgconfig";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  hardware.graphics.enable = true;
  programs.hyprland.enable = true;
  programs.steam.enable = true;
  services.displayManager.sddm.enable = true;
  services.xserver.enable = true;
  services.xserver.videoDrivers = ["nvidia"];
  programs.nix-ld.enable = true;
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
 };
  
  services.pipewire.enable = true;

  programs.fish.enable = true;
  programs.fuse.enable = true;
  programs.fuse.userAllowOther = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.11"; # Did you read the comment?

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  nix.settings = {
    min-free = 5 * 1024 * 1024 * 1024;   # trigger GC below 5GB free
    max-free = 15 * 1024 * 1024 * 1024;  # GC until 15GB free
    keep-outputs = false;
    keep-derivations = false;
  };
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };

}
