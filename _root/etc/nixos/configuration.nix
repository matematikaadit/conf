# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.grub = {
  # Use the GRUB 2 boot loader.
    enable = true;
    version = 2;
  # Define on which hard drive you want to install Grub.
    device = "/dev/sda";
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
    hostId = "578f496d";
    # wireless.enable = true;  # Enables wireless.
    networkmanager.enable = true; # Enable NetworkManager
    # Static IP setting
    interfaces.enp19s0.ip4 = [ { address = "192.168.10.2"; prefixLength = 24; } ];
    defaultGateway = "192.168.10.1";
    nameservers = [ "8.8.8.8" ];
  };

  # Set timezone
  time.timeZone = "Asia/Jakarta";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  nixpkgs.config = {
    allowUnfree = true;

    firefox = {
      enableGoogleTalkPlugin = true;
      enableAdobeFlash = true;
    };

    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # Utils
    wget
    dmenu
    xclip
    rxvt_unicode
    # Lightweight Browser
    surf
    w3m
    # Full Browser
    firefoxWrapper
    chromium
    # Entertainment
    mpv
    # Dev stuff
    # haskellPackages.yi
    vimHugeX
    gitAndTools.gitFull
    # Compiler
    haskellPackages.ghc
    fpc
    gcc
    clang
    # Build system
    gnumake
    # haskellPackages.shake
    # Messaging
    hexchat
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";

    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      default = "xmonad";
    };

    desktopManager = {
      xterm.enable = false;
      default = "none";
    };

    displayManager = {
      # Change default cursor from X to left pointer
      sessionCommands = "xsetroot -cursor_name left_ptr";
      slim = {
        enable = true;
        defaultUser = "adit";
        # autoLogin = true;
      };
    };

    synaptics.enable = true;
  };

  # Show nixos manual in tty 8
  services.nixosManual.showManual = true;

  # Fonts https://nixos.org/wiki/Fonts
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    enableCoreFonts = true;
    fonts = with pkgs; [
      inconsolata
      ubuntu_font_family
    ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.adit = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/adit";
    description = "Adit Cahya Ramadhan";
    extraGroups = ["wheel" "networkmanager"];
  };
 
}

# vim:sw=2:ts=2:et:ai:bs=indent,eol,start
