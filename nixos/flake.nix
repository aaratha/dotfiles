{
  description = "My NixOS Configuration";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs-davinci = {
      url = "github:nixos/nixpkgs/d457818da697aa7711ff3599be23ab8850573a46";
    };
  };

  outputs = { self, nixpkgs, home-manager, nur, nixpkgs-davinci, ... }@inputs:
  {
    nixosConfigurations.nixos = 
      nixpkgs.lib.nixosSystem {

        system = "x86_64-linux";

        modules = [
          ./configuration.nix

        home-manager.nixosModules.home-manager
        # Adds the NUR overlay
        nur.modules.nixos.default
        # NUR modules can be imported directly:
        nur.repos.iopq.modules.nixos.xraya

	  {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;
      home-manager.extraSpecialArgs = { inherit self inputs; };

      home-manager.users.aaratha = import ./home.nix;
	  }
	];
      };
  };
}
