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

  security.pki.certificates = [ ''
-----BEGIN CERTIFICATE-----
MIIDfjCCAmagAwIBAgIUbbKW/5zxSHGcKeGZ4CSBsV1te4gwDQYJKoZIhvcNAQEL
BQAwVzELMAkGA1UEBhMCVVMxEzARBgNVBAgTCldhc2hpbmd0b24xEDAOBgNVBAcT
B1NlYXR0bGUxFDASBgNVBAoTC0Nsb3VkZW50aXR5MQswCQYDVQQLEwJDQTAeFw0x
OTA3MTQxMDA2MDBaFw0yNDA3MTIxMDA2MDBaMFcxCzAJBgNVBAYTAlVTMRMwEQYD
VQQIEwpXYXNoaW5ndG9uMRAwDgYDVQQHEwdTZWF0dGxlMRQwEgYDVQQKEwtDbG91
ZGVudGl0eTELMAkGA1UECxMCQ0EwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK
AoIBAQC4N+Kteutd+D53EB9B5YbLZu95VWs4KwehJucjFETAkBE65KWYELbDwyUh
GGd+a649blA6M1Ce80s58EjXEkf9Luer5TxNFo5DDdfzYm0SDtoisjeamYJtJLdE
+/SMvHZkaKzx6VttTBkcw2aUzDpBdes31g80Uqv0oSnhak7D3pyMsET/pixxpHFU
DsSQEfv1nLFTwui+L2kF9tJOADSO8t2Z7DadtHCdAzruvZ3IxisvN5memhXxAPHq
XRWuoqd6EvwDJwwT247j1Etr2rUIuvMTuCABtFT7IwncJbFla0kXpL3nlYPONGUa
kG4smVM4R0loQTHhKxZtWpgFcpjbAgMBAAGjQjBAMA4GA1UdDwEB/wQEAwIBBjAP
BgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBTbX+r7j7wRfv1LsHzJnR32MUYRazAN
BgkqhkiG9w0BAQsFAAOCAQEAQJkR/S8fmi/II6hPSbvilKf3Bau20kwRgR66iHYu
qxoRDbytgQaFfU8EPVtH65akvLLT8wZ7dBPOGnqI50KlDtfyrAPRaq+cuf6Ny6QR
9URE3sh1Lz01D/+gXGSpfkdi96pG+oQm5WkhdnHY6BKK+UAyM1V2alzUzJze2DT5
ZVX6adBgUqTyOK+aEfg0eBLLPmfcD1b4V9vOaxs/O1Sl2AG6lLuYYsZqHx6OEqk1
SxSljV469X4dvsNxV9EL4uPFVui+CzkfGONNGkRbbZDu+fw4c6lD95maS/UobP7y
IMzY+r2NfK8zz6U0MxmIas/P86BqX/qM6qEjMHPKBJ6hBQ==
-----END CERTIFICATE-----

-----BEGIN CERTIFICATE-----
MIIEvTCCAqWgAwIBAgIJAMjU19gt2p1PMA0GCSqGSIb3DQEBCwUAMBIxEDAOBgNV
BAMMB29wLXRlc3QwHhcNMTgxMDEyMTM0NTExWhcNMTkxMDEyMTM0NTExWjASMRAw
DgYDVQQDDAdvcC10ZXN0MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA
2zDYX4tAjA+9Zo4hZdvAzs7mO+b0Lc7Fj878fv0KTBcl4qifi9nVJ87T41+TSOJw
O+w/J21OoFmPKdygVFYRV6bxeu7TEUTgOlE8hOqFULFnryIt1wYQNMGN5GWF3jcF
eDzWVI+gpnn2EkZHx+ADn+y+zR38CBHyyQbqcIM92wTrttGvEQaDJMCEtP4rP68G
TrC3d7MgMrsESp38SaEm1HylMr5VkQ6e6cILlGiXe5QbHr44eGUmQ63PtfEfJFDT
7JhllRA+C6POdm9OkUoyfiWSaJTHXAl9bKT1SzTnCZH09bS2RKsYZN7WYOEWlrg0
cK0T4qqkGK6LNDIjkXxwm5tqTy55TGHDdfLBoX/sSGVuq6IcO2NZwz+5KN6AeYgY
KJJrYDcHzbmdbckjYKt6FWc7tD96hmkxVR4zwmZXmUeCeH8UZhwTkA0ZFi3Bs1ZA
rpTIbzvCoFQbuPx1evme9USZzM5LxG6OJGYLpQfmJGkXkUfNHPeiaqb3JdHzXdeU
hbLZLHB+3LIg4GsOLSOFJ75vtOYOETQu0T+Bp+qxFMR76CUzzi9fDZf1B1RyJHRe
JzM1jOLIc8wFCNmJ6o3O+dZmv+E3t52Y5dOBAAb8BwldHeeWy5IjkEx4HhFqjnb8
2r8X2bELIYnkg887IsxhpM3kah9ClDRGk3rkU0QFmvMCAwEAAaMWMBQwEgYDVR0R
BAswCYIHb3AtdGVzdDANBgkqhkiG9w0BAQsFAAOCAgEALsnFnDrX9vdJb43fwxRz
8r2qiUPF7Tc/oGKEueMRcykw5k/vtoKqqnC8WVDYsP7Ga6f0KP7bgJxR2dx5IikY
Ajzdd7p/5mKBj507STksNmzESdLGuft3JBdidsYU3G2359uJ2+lHcMgScRWUcX+F
/JKYp336t8RcjWcFSeb/wJrHJIxUOL3rkw619LlW/+T1ZlLxCpV9hBuLmqg+3MbB
9/k5ixdGpnJAo6g6pCmH5r0der20qumMjnecz0MpUXG60f2ODG00G90jXdIGdPdk
tvp41MMqtKQmWHyQxNVn/Bb0JperKf7ECpZe60k97Pfwn5Ei9jzVnMvsdJSVFHdk
L7/H42u1cbCW+UxJmJkbTpWmlv0DW73ZIFaztddS35uheWXvUWyMuA8yxcc7uyjG
dWvwSNuGX6cm9YOfrAZswd2JCYL0LbII6xxKH1/JxW2ScC6M2zMe7CtAmbuIS3Ca
4wZZLYZd15MhvwHouvyjWfBBrJM2fRTcURCOag2EsPuzfip3lLDAp3TNW9lbobBl
ZLNPhhd9ErKlnb1PHR/C8FAkOEk5uaF+FYb+yUP/LUjE4ydRqQ5vZEc59CG1mb2R
z1b8xDF54lrm7fQliGSJeuc7qLXibvn8m/dIfnTsQB+446VVXuhHgWgVt8AJKIkP
ngB61uUFVpzUGM6d3Xpqnts=
-----END CERTIFICATE-----
  '' ];

  networking = {
    nameservers = [ "8.8.8.8" "35.168.75.32" ];
    hostName = "t470";
    networkmanager.enable = true;
    extraHosts = "
      127.0.0.1 t470
      127.0.0.1 authorization.cloudentity.com
      10.50.2.78 jenkins.cloudentity.com
      10.50.2.162 download.microperimeter.cloudentity.com
      10.50.2.162 docs.cloudentity.com
      127.0.0.1 public.oauth.cloudentity.com admin.oauth.cloudentity.com login.oauth.cloudentity.com
      127.0.0.1 op-test op rp-test
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
    subdl

    # gui
    google-chrome firefox emacs zoom-us zathura
    shotwell transmission-gtk vlc slack gparted
    chromedriver

    # xserver
    rofi conky xorg.xmodmap xorg.xkill xorg.xbacklight
    lxappearance adapta-gtk-theme papirus-icon-theme
    feh scrot compton xcompmgr

    # applets
    networkmanagerapplet pavucontrol pasystray udiskie

    # dev
    ## java scala
    scala sbt maven jdk jetbrains.idea-community

    ## go
    go_1_12 gnumake protobuf3_5

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
    export GOROOT=${pkgs.go_1_12}/share/go
  '';

  virtualisation.docker.enable = true;

  virtualisation.virtualbox.host = {
      enable = true;
      headless = true;
  };

  system.stateVersion = "18.03";
}
