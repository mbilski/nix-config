# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.loader.grub = {
    enable = true;
    device = "nodev";
    version = 2;
    efiSupport = true;
  };

  boot.loader.grub.extraEntries = ''
    menuentry "Ubuntu" {
      search --set=ubuntu --fs-uuid 7f0ad1e5-b890-494a-8613-871a095a5a6c
      configfile "($ubuntu)/boot/grub/grub.cfg"
    }
  '';

  fileSystems."/mnt/ubuntu" =
    { device = "/dev/nvme0n1p5";
      fsType = "ext4";
    };

  networking.hostName = "t470";
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "pl";
    defaultLocale = "pl_PL.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # console
    wget xsel vim tmux git tig fasd

    # gui
    chromium emacs zoom-us zathura

    # xserver
    rofi conky xorg.xmodmap xorg.xkill xorg.xbacklight
    lxappearance adapta-gtk-theme tango-icon-theme

    # applets
    networkmanagerapplet pavucontrol pasystray

    # dev
    scala maven jdk jetbrains.idea-community
    go dep gnumake protobuf
    elmPackages.elm

    # containers
    docker docker_compose
  ];

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts
    dina-font
    proggyfonts
    fontforge
    powerline-fonts
    font-awesome-ttf
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  sound.mediaKeys = {
    enable = true;
    volumeStep = "5%";
  };
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "pl";
  services.xserver.libinput.enable = true;
  services.xserver.windowManager.i3.enable = true;
  services.xserver.displayManager.lightdm.enable = true;

  users.extraUsers.mbilski = {
    isNormalUser = true;
    home = "/home/mbilski";
    description = "Mateusz Bilski";
    extraGroups = [ "wheel" "networkmanager" "audio" ];
    uid = 1000;
    shell = pkgs.zsh;
  };

  programs.zsh.enable = true;
  programs.zsh.promptInit = "";
  programs.zsh.interactiveShellInit = ''
    export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/
    ZSH_THEME="robbyrussell"
    plugins=(git)
    source $ZSH/oh-my-zsh.sh
    eval "$(fasd --init auto)"
    export GOROOT=${pkgs.go}/share/go
  '';

  system.stateVersion = "18.03";
}
