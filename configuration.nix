{ config, pkgs, ... }:

let
  secrets = import ./secrets.nix;

  polybarWithExtras = pkgs.polybar.override {
    i3Support = true;
    mpdSupport = true;
  };

  minikube28 = pkgs.callPackage ./pkgs/minikube28 {
    inherit (pkgs.darwin.apple_sdk.frameworks) vmnet;
  };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmpOnTmpfs = true;

  boot.loader.grub = {
    enable = true;
    device = "nodev";
    version = 2;
    efiSupport = true;
  };

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 7d";

  networking = {
    nameservers = [ "8.8.8.8" "35.168.75.32" ];
    hostName = "t470";
    networkmanager.enable = true;
    extraHosts = "
      127.0.0.1 t470
      10.50.2.78 jenkins.cloudentity.com
      10.50.2.162 download.microperimeter.cloudentity.com
      10.50.2.162 docs.cloudentity.com
    ";
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "pl";
    defaultLocale = "pl_PL.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  environment.variables = {
    EDITOR = "vim";
  };

  programs.light.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # console
    wget xsel vim tmux git tig fasd openvpn unzip zip
    jq polybarWithExtras ntfs3g exfat
    neofetch tree psmisc sxiv urxvt_font_size urxvt_perl
    gnupg cacert graphviz openssl pkgconfig
    shellcheck weechat htop ctop cfssl wrk peek
    iptables ranger bat highlight dialog
    yq fzf autorandr silver-searcher
    spotify pgcli cmus cloc xclip bc vagrant hugo mplayer

    # gui
    google-chrome firefox emacs zoom-us zathura
    shotwell transmission-gtk vlc slack gparted
    chromedriver

    # xserver
    rofi conky xorg.xmodmap xorg.xkill xorg.xbacklight
    lxappearance adapta-gtk-theme papirus-icon-theme
    feh scrot compton

    # applets
    networkmanagerapplet pavucontrol pasystray udiskie

    # dev
    ## java scala
    scala sbt maven jdk jetbrains.idea-community

    ## go
    go godef dep glide gnumake protobuf3_5 gotools

    ## elm
    elmPackages.elm asciidoctor

    ## js
    nodejs-8_x

    ## haskell
    ghc stack cabal-install gcc binutils-unwrapped

    ## rust
    rustup rustracer

    ## python
    (python27.withPackages(ps: with ps; [ websocket_client livestreamer ]))

    # containers
    docker_compose docker-machine kubectl minikube28 kubernetes-helm
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
    siji
    unifont
    nerdfonts
    font-awesome_5
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    tcp = {
      enable = true;
      anonymousClients.allowedIpRanges = ["127.0.0.1"];
    };
  };

  services.udisks2.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "pl";
  services.xserver.libinput.enable = true;

  services.xserver.displayManager.slim = {
    enable = true;
    defaultUser = "mbilski";
    theme = pkgs.fetchurl {
      url = "https://github.com/edwtjo/nixos-black-theme/archive/v1.0.tar.gz";
      sha256 = "13bm7k3p6k7yq47nba08bn48cfv536k4ipnwwp1q1l2ydlp85r9d";
    };
  };

  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
  };

  users.groups.video = {};

  users.extraUsers.mbilski = {
    isNormalUser = true;
    home = "/home/mbilski";
    description = "Mateusz Bilski";
    extraGroups = ["wheel" "networkmanager" "audio" "docker" "kubernetes" "video"];
    uid = 1000;
    shell = pkgs.zsh;
  };

  programs.zsh.enable = true;
  programs.zsh.promptInit = "";
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.autosuggestions.enable = true;
  programs.zsh.interactiveShellInit = ''
    export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/
    ZSH_CACHE_DIR=$HOME/.cache/oh-my-zsh
    ZSH_CUSTOM=$HOME/.zsh
    ZSH_THEME="gister"
    plugins=(git)
    source $ZSH/oh-my-zsh.sh
    source "$(fzf-share)/key-bindings.zsh"
    eval "$(fasd --init auto)"
    export GOROOT=${pkgs.go}/share/go
  '';

  virtualisation.docker.enable = true;

  virtualisation.virtualbox.host = {
      enable = true;
      headless = true;
  };

  system.stateVersion = "18.03";
}
