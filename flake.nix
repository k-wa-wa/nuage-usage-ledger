{
  description = "クラスタ使用量の日次帳票 (MVS 3.8j のバッチジョブ)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      # nuage-cluster の mvs-config が MVS に書き込む資材。出力の形は次のとおりで、これが両者の約束事である。
      #   members/<データセット名>/<メンバー名>  書き込むメンバー
      #   schedule                              ZTIMER の予定表 (SYS2.CONTROL(ZTIMER)) に足す行
      #   datasets.json                         無ければ作るデータセット
      packages = forAllSystems (pkgs: {
        default = pkgs.runCommandLocal "nuage-usage-report-mvs" { } ''
          mkdir -p $out/members/NUAGE.USAGE.JCLLIB
          # Git では JCL・REXX・COBOL を別々に持ち、ここで 1 本のジョブにまとめる
          substitute ${./mvs/USAGERPT.jcl} $out/members/NUAGE.USAGE.JCLLIB/USAGERPT \
            --replace-fail '{{USAGEGET}}' "$(cat ${./mvs/USAGEGET.rexx})" \
            --replace-fail '{{USAGERPT}}' "$(cat ${./mvs/USAGERPT.cbl})"
          cp ${./mvs/schedule} $out/schedule
          cp ${./mvs/datasets.json} $out/datasets.json
        '';
      });

      # カード (80 桁) と MVS の制約に合っているかを検査する (nix flake check)
      checks = forAllSystems (pkgs: {
        mvs-sources = pkgs.runCommandLocal "check-mvs-sources" { nativeBuildInputs = [ pkgs.python3 ]; } ''
          python3 ${./scripts/check_mvs_sources.py} ${
            self.packages.${pkgs.stdenv.hostPlatform.system}.default
          }
          touch $out
        '';
      });
    };
}
