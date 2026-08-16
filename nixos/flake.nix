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
  };

  outputs = { self, nixpkgs, home-manager, nur, ... }:
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

            home-manager.users.aaratha = import ./home.nix;
	  }
	];
      };
  };
}
