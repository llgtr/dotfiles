{
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {
    #Dash = stdenv.mkDerivation {
    #  name = "Dash";
    #  src = fetchurl {
    #    url = https://kapeli.com/downloads/v4/Dash.zip;
    #    #sha256 = "afe990100a08f098042905c8279330a1399802ae6a7e50faadee3cb4084e8258";
    #    sha256 = "";
    #  };
    #  buildInputs = [ unzip ];
    #  buildCommand = ''
    #    mkdir -p "$out/Applications/Dash.app"
    #    unzip $src -d "$out/Applications/"
    #  '';
    #};

    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        zsh
        htop
        tmux
        tree
        silver-searcher
        python3
        leiningen
        nodejs-11_x
        yarn
        #pandoc
        vim
        emacs
      ];
      pathsToLink = [ "/share" "/bin" "/Applications" ];
    };
  };
}
