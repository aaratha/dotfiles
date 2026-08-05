{
  description = "My NixOS Configuration";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
  {
    nixosConfigurations.nixos = 
      nixpkgs.lib.nixosSystem {

        system = "aarch64-linux";

	modules = [
          ./configuration.nix

	  home-manager.nixosModules.home-manager

	  {
	    home-manager.useGlobalPkgs = true;
	    home-manager.useUserPackages = true;

            home-manager.users.aaratha = import ./home.nix;
	  }
	];
      };
  };
}
