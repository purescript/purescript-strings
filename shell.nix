let 
  purescript-overlay =
    builtins.fetchGit {
      name = "purescript-overlay";
      url = "https://github.com/thomashoneyman/purescript-overlay.git";
      rev = "526c92c34a1a0213dc5c4761756375e13d85f8d4";
    };
  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "69ea3146f3c4f715c5dbc6e0f8ba7d0ee57bb3bd";
    };
  erlangReleases =
    builtins.fetchGit {
      name = "nixpkgs-nixerl";
      url = "https://github.com/id3as/nixpkgs-nixerl.git";
      rev = "2822128d0fe5c8aac42f0b80045e80e6ac22bfcc";
    };

  pkgs = import <nixpkgs> {
      overlays = [
        (import purescript-overlay).overlays.default
        (import purerlReleases)
        (import erlangReleases)

      ];
    };
  erlang = pkgs.nixerl.erlang-26-1;

in

pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = with pkgs.buildPackages; 
      [ 
        nodejs 
        spago-unstable
        purs-bin.purs-0_15_14
        purs-tidy
        purerl.purerl-0-0-22
        erlang.erlang
    ];
    
}

