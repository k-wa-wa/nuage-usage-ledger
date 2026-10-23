{
  description = "クラスタ使用量の勘定 (MVS 3.8j のバッチジョブと KICKS のオンライン照会)";

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
      #   rakf                                  RAKF の権限表 (SYS1.SECURE.CNTL(PROFILES)) に足す行
      #   datasets.json                         無ければ作るデータセット
      #   build/<ジョブ名>                       内容が変わったら流すビルドジョブ (KICKS の導入の後。RC 4 まで成功)
      #   cicerone3270/                         3270 端末の案内 (cicerone3270) に足す辞書
      packages = forAllSystems (pkgs: {
        default = pkgs.runCommandLocal "nuage-usage-ledger-mvs" { } ''
          mkdir -p $out/members/NUAGE.USAGE.JCLLIB $out/members/NUAGE.USAGE.CNTL \
            $out/members/NUAGE.USAGE.CLIST $out/build
          # Git では JCL・REXX・COBOL を別々に持ち、ここで 1 本のジョブにまとめる
          substitute ${./mvs/USAGERPT.jcl} $out/members/NUAGE.USAGE.JCLLIB/USAGERPT \
            --replace-fail '{{USAGEGET}}' "$(cat ${./mvs/USAGEGET.rexx})" \
            --replace-fail '{{USAGERPT}}' "$(cat ${./mvs/USAGERPT.cbl})"
          substitute ${./mvs/LEDGER.jcl} $out/members/NUAGE.USAGE.JCLLIB/LEDGER \
            --replace-fail '{{USAGEGET}}' "$(cat ${./mvs/USAGEGET.rexx})" \
            --replace-fail '{{TRANGEN}}' "$(cat ${./mvs/TRANGEN.cbl})" \
            --replace-fail '{{POSTING}}' "$(cat ${./mvs/POSTING.cbl})"
          substitute ${./mvs/LEDGMON.jcl} $out/members/NUAGE.USAGE.JCLLIB/LEDGMON \
            --replace-fail '{{MONSEL}}' "$(cat ${./mvs/MONSEL.cbl})" \
            --replace-fail '{{ACCTLDG}}' "$(cat ${./mvs/ACCTLDG.cbl})"
          substitute ${./mvs/LEDGINQ.jcl} $out/members/NUAGE.USAGE.JCLLIB/LEDGINQ \
            --replace-fail '{{INQBLD}}' "$(cat ${./mvs/INQBLD.cbl})"
          # オンライン (KICKS): 表・マップ・プログラムを 1 本のビルドジョブにまとめる。
          # nuage-cluster が内容の変化を見て流す (build/)。手で流せるよう JCLLIB にも置く
          substitute ${./mvs/online/LEDGBLD.jcl} $out/build/LEDGBLD \
            --replace-fail '{{KIKSITLG}}' "$(cat ${./mvs/online/KIKSITLG.asm})" \
            --replace-fail '{{KIKPCTLG}}' "$(cat ${./mvs/online/KIKPCTLG.asm})" \
            --replace-fail '{{KIKPPTLG}}' "$(cat ${./mvs/online/KIKPPTLG.asm})" \
            --replace-fail '{{KIKFCTLG}}' "$(cat ${./mvs/online/KIKFCTLG.asm})" \
            --replace-fail '{{LBALSET}}' "$(cat ${./mvs/online/LBALSET.bms})" \
            --replace-fail '{{LLSTSET}}' "$(cat ${./mvs/online/LLSTSET.bms})" \
            --replace-fail '{{LBALPGM}}' "$(cat ${./mvs/online/LBALPGM.cbl})" \
            --replace-fail '{{LLSTPGM}}' "$(cat ${./mvs/online/LLSTPGM.cbl})"
          cp $out/build/LEDGBLD $out/members/NUAGE.USAGE.JCLLIB/LEDGBLD
          cp ${./mvs/online/LEDGER.clist} $out/members/NUAGE.USAGE.CLIST/LEDGER
          # マスタ (換算表・勘定マスタ定義) は Git で宣言する
          cp ${./mvs/cntl/RATES} $out/members/NUAGE.USAGE.CNTL/RATES
          cp ${./mvs/cntl/ACCTDEF} $out/members/NUAGE.USAGE.CNTL/ACCTDEF
          cp ${./mvs/schedule} $out/schedule
          cp ${./mvs/rakf} $out/rakf
          cp ${./mvs/datasets.json} $out/datasets.json
          cp -r ${./guide} $out/cicerone3270
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
        # cicerone3270 の辞書が TOML として読めるか
        guide = pkgs.runCommandLocal "check-guide" { nativeBuildInputs = [ pkgs.python3 ]; } ''
          python3 -c 'import sys, tomllib; [tomllib.load(open(f, "rb")) for f in sys.argv[1:]]' ${./guide}/*.toml
          touch $out
        '';
      });
    };
}
