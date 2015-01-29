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
    networkmanager.enable = true; # Enable NetworkManager

    # Extrahosts
    extraHosts = ''
      192.168.0.1 lan
      192.168.1.1 modem
    '';
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
      enableAdobeFlash = true;
    };
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # Utils
    anki
    dmenu
    feh
    file
    gmrun
    mupdf
    nix-repl
    rxvt_unicode
    tree
    wget
    xclip

    # Lightweight Browser
    surf
    w3m

    # Full Browser
    firefoxWrapper

    # Entertainment
    mpv
    openttd
    timidity # openttd deps (midi renderer)
    transmission_gtk

    # Dev stuff
    darcs
    gitAndTools.gitFull
    haskellPackages.yi
    vimHugeX

    # Compiler
    clang
    fpc
    gcc
    haskellPackages.ghc

    # Build system
    gnumake
    haskellPackages.shake

    # Messaging
    hexchat
  ];

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
      sessionCommands = with pkgs.xlibs; ''
        ${xsetroot}/bin/xsetroot -cursor_name left_ptr
      '';

      slim = {
        enable = true;
        defaultUser = "adit";
        autoLogin = false;
        theme = pkgs.fetchurl {
          url = mirror://sourceforge/slim.berlios/slim-wave.tar.gz;
          sha256 = "0ndr419i5myzcylvxb89m9grl2xyq6fbnyc3lkd711mzlmnnfxdy";
        };
      };
    };

    synaptics.enable = true;
  };

  # Fonts https://nixos.org/wiki/Fonts
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    enableCoreFonts = true;
    fonts = with pkgs; [
      inconsolata
      ubuntu_font_family
      # Japanese font
      kochi-substitute
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
