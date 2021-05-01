homeDirectory: { pkgs, lib, config, ... }:
  let
    gmailOauth2Tools = with pkgs; stdenv.mkDerivation {
      name = "gmail-oauth2-tools";
      src = gmail-oauth2-tools;
      buildInputs = [ python2 pass oauth2Lib ];
      installPhase = ''
      mkdir -p $out/bin
      cp python/oauth2.py $out/bin/gmail-oauth2-tools
      chmod +x $out/bin/gmail-oauth2-tools

      # authorization script
      cat > $out/bin/gmail-get-token <<EOF
      #! /usr/bin/env bash
      ID=\$(${pass}/bin/pass google-api/id)
      SECRET=\$(${pass}/bin/pass google-api/secret)
      TOKEN=\$(${pass}/bin/pass google-api/\$1)
      ACCOUNT=\$1

      $out/bin/gmail-oauth2-tools --user=\$ACCOUNT --client_id=\$ID --client_secret=\$SECRET --refresh_token=\$TOKEN --generate_oauth2_token | awk -F" " '{if(NR==1)print \$3}'
      EOF

      chmod +x $out/bin/gmail-get-token
    '';
    };
    oauth2Lib = with pkgs; stdenv.mkDerivation {
      name = "sasl2-oauth";
      src = oauth2-lib;
      nativeBuildInputs = [
        autoreconfHook
        nixpkgs.legacyPackages."${system}".cyrus_sasl
      ];
    };
  in {
    imports = [
      {
        programs.notmuch = {
          enable = true;
          maildir.synchronizeFlags = true;
          new = {
            tags = [ "new" ]; # to be used together with afew
          };
          hooks = {
            postNew = ''
                ${pkgs.afew}/bin/afew --new --tag
              '';
          };
          search.excludeTags = [ "trash" "spam" ];
          extraConfig = {
            index = {
              header = "List List-Id";
            };
          };
        };
      }
      {
        programs.afew.enable = true;
      
        programs.afew.extraConfig = with builtins;
          let
            folder-fn = mailboxes: account: with account;
              concatStringsSep " " (map (box: "${name}/${box}") mailboxes);
      
            rule-fn = account: with account;
              ''
                ${name}/inbox   = 'tag:spam':${name}/spam                       'NOT tag:inbox':${name}/archive 'tag:trash':${name}/trash
                ${name}/archive = 'tag:inbox':${name}/inbox                     'tag:spam':${name}/spam         'tag:trash':${name}/trash
                ${name}/spam    = 'NOT tag:spam AND tag:inbox':${name}/inbox    'NOT tag:spam':${name}/archive
                ${name}/trash   = 'tag:inbox AND (NOT tag:trash)':${name}/inbox 'NOT tag:trash':${name}/archive
              '';
      
            move-folders = "folders = " + concatStringsSep " "
              (map (folder-fn [ "inbox" "spam" "archive" "trash" ])
                (attrValues config.accounts.email.accounts));
      
            move-rules = concatStringsSep
              "\n" (map rule-fn (attrValues config.accounts.email.accounts));
          in
            ''
              [MailMover]
              rename = True
            '' + move-folders + "\n" + move-rules;
      }
      {
        programs.afew.extraConfig = ''
          [SpamFilter]
          [ArchiveSentMailsFilter]
          [OnlyAllowListFilter]
          [Filter.1]
          query = NOT path:"/.*/inbox/**/"
          tags = -new
          message = remove the "new" tag for message not found in the inbox folder
          [Filter.2]
          query = from:github.com AND tag:new
          tags = -new +github
          [InboxFilter]
        '';
      
        home.file = {
          ".config/afew/OnlyAllowListFilter.py" = {
            text = ''
              import os
              import json
              
              from afew.filters.HeaderMatchingFilter import HeaderMatchingFilter
              from afew.FilterRegistry import register_filter
              
              
              AFEW_CONFIG_DIR = os.path.expanduser("~/.config/afew")
              
              with open("{}/allow_list".format(AFEW_CONFIG_DIR)) as f:
                  ALLOW_LIST = json.load(f)
              
              
              @register_filter
              class OnlyAllowListFilter(HeaderMatchingFilter):
                  message = 'Remove the "new" tag for all messages with field `List-Id` that is not in the ALLOW_LIST'
                  query = 'tag:new'
                  pattern = r"<(?P<list_id>[a-z0-9!#$%&'*+/=?^_`{|}~-]+)\."
              
                  # overriding the `handle_message` method to remove inbox tag conditionally
                  def handle_message(self, message):
                      if not self._tag_blacklist.intersection(message.get_tags()):
                          value = message.get_header('List-Id')
                          match = self.pattern.search(value)
                          if match:
                              list_id = match.group('list_id').lower()
                              self.add_tags(message, 'lists', list_id)
                              if list_id not in ALLOW_LIST:
                                  self.remove_tags(message, 'new')
            '';
          };
          ".config/afew/allow_list" = {
            text = ''
              [
              "egrads",
              "econseminars",
              "placement20-21",
              "devlunch",
              "mrg2016f",
              "seminar281"
              ]
            '';
          };
        };
      
        home.activation = {
          removeAfewFilterCache = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            if [ -d ~/.config/afew/__pycache__ ]; then
                $DRY_RUN_CMD rm -r $VERBOSE_ARG ~/.config/afew/__pycache__
            fi
          '';
        };
      }
      {
        programs.msmtp.enable = true;
      }
      {
        home.packages = [ pkgs.w3m ];
      }
    ];

    nixpkgs.overlays = [
      (
        final: prev:
        {
          cyrus_sasl = prev.cyrus_sasl.overrideAttrs (div: rec {
            postInstall = ''
              for lib in ${oauth2Lib}/lib/sasl2/*; do
                ln -sf $lib $out/lib/sasl2/
              done
            '';
          });
        }
      )
    ];

    programs.mbsync.enable = true;

    home.packages = [ gmailOauth2Tools ];

    services = {
      mbsync = {
        enable = true;
        # sync every 5 minutes, but alerts can be less frequent
        frequency = "*:0/5";
        postExec = ''${pkgs.notmuch}/bin/notmuch new'';
      };
    };

    accounts.email = let
      mailDir = "${homeDirectory}/.mail";
      mbsyncExtraConf = {
        Create = "Slave";
        Expunge = "Both";
        SyncState = "*";
        Sync = "all";
      };
    in {
      maildirBasePath = mailDir;
      accounts = {
        personal = {
          realName = self.realUser;
          userName = "junyi.yi.hou@gmail.com";
          address = "junyi.yi.hou@gmail.com";
          flavor = "gmail.com";
          folders = {
            drafts = "drafts";
            inbox = "inbox";
            sent = "sent";
            trash = "trash";
          };
          mbsync = {
            enable = true;
            extraConfig.account = {
              AuthMechs = "XOAUTH2";
              PipelineDepth = 1;
              TimeOut = 60;
            };
            groups = {
              "personal".channels = {
                "inbox" = {
                  patterns = [ "INBOX" ];
                  extraConfig = mbsyncExtraConf;
                };
                "sent" = {
                  masterPattern = "[Gmail]/Sent Mail";
                  slavePattern = "sent";
                  extraConfig = mbsyncExtraConf;
                };
                "archive" = {
                  masterPattern = "[Gmail]/All Mail";
                  slavePattern = "archive";
                  extraConfig = mbsyncExtraConf;
                };
                "drafts" = {
                  masterPattern = "[Gmail]/Drafts";
                  slavePattern = "drafts";
                  extraConfig = mbsyncExtraConf;
                };
                "trash" = {
                  masterPattern = "[Gmail]/Bin";
                  slavePattern = "trash";
                  extraConfig = mbsyncExtraConf;
                };
                "spam" = {
                  masterPattern = "[Gmail]/Spam";
                  slavePattern = "spam";
                  extraConfig = mbsyncExtraConf;
                };
              };
            };
          };
          msmtp = {
            enable = true;
            extraConfig = {
              auth = "oauthbearer";
            };
          };
          notmuch.enable = true;
          passwordCommand = "${gmail-oauth2-tools}/bin/gmail-get-token junyi.yi.hou@gmail.com";
        
          primary = true;
        };
        berkeley = {
          realName = self.realUser;
          userName = "junyi.hou@berkeley.edu";
          address = "junyi.hou@berkeley.edu";
          flavor = "gmail.com";
          folders = {
            drafts = "drafts";
            inbox = "inbox";
            sent = "sent";
            trash = "trash";
          };
          mbsync = {
            enable = true;
            extraConfig.account = {
              AuthMechs = "XOAUTH2";
              PipelineDepth = 1;
              TimeOut = 60;
            };
            groups = {
              "berkeley".channels = {
                "inbox" = {
                  patterns = [ "INBOX" ];
                  extraConfig = {
                    Create = "Slave";
                    Expunge = "Both";
                    SyncState = "*";
                    Sync = "all";
                  };
                };
                "sent" = {
                  masterPattern = "[Gmail]/Sent Mail";
                  slavePattern = "sent";
                  extraConfig = mbsyncExtraConf;
                };
                "archive" = {
                  masterPattern = "[Gmail]/All Mail";
                  slavePattern = "archive";
                  extraConfig = mbsyncExtraConf;
                };
                "drafts" = {
                  masterPattern = "[Gmail]/Drafts";
                  slavePattern = "drafts";
                  extraConfig = mbsyncExtraConf;
                };
                "trash" = {
                  masterPattern = "[Gmail]/Trash";
                  slavePattern = "trash";
                  extraConfig = mbsyncExtraConf;
                };
                "spam" = {
                  masterPattern = "[Gmail]/Spam";
                  slavePattern = "spam";
                  extraConfig = mbsyncExtraConf;
                };
              };
            };
          };
          msmtp = {
            enable = true;
            extraConfig = {
              auth = "oauthbearer";
            };
          };
          notmuch.enable = true;
          passwordCommand = "${gmail-oauth2-tools}/bin/gmail-get-token junyi.hou@berkeley.edu";
        };
      };
    };
    
    home.activation.initMailDir =
      let
        mailDir = "${homeDirectory}/.mail";
        mkAccountDir = with builtins; concatStringsSep "\n"
          (map (account: ''$DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${mailDir}/${account.name}'')
            (attrValues config.accounts.email.accounts));
      in
        lib.hm.dag.entryAfter [ "writeBoundary" ] mkAccountDir;
    home.file."${homeDirectory}/.emacs.d/site-lisp/gatsby:accounts.el" =
      let
        defaultAcc = with builtins; head (
          filter (a: a.primary) (attrValues config.accounts.email.accounts));
        allAccounts = with builtins;
          concatStringsSep " "
            (map (a: ''"'' + a + ''"'')
              (catAttrs "address"
                (filter (a: a.flavor == "gmail.com")
                  (sort (a1: a2: if a1.primary then true else false)
                    (attrValues config.accounts.email.accounts)))));
        mailDir = "${homeDirectory}/.mail";
      in {
        text = ''
          ;;; gatsby:accounts.el -*- lexical-binding: t; -*-
    
          ;; the default account
          (setq user-full-name "${defaultAcc.realName}"
                user-mail-address "${defaultAcc.address}")
    
          ;; record all accounts
          (defconst user-all-mail-addresses '(${allAccounts}) "All registered email addresses")
    
          ;; smtpmail settings
          (setq smtpmail-queue-mail nil
                smtpmail-queue-dir (let* ((dir "${mailDir}/queue"))
                                     (unless (file-exists-p dir)
                                       (make-directory dir))
                                     (format "%s/cur" dir)))
    
          (provide 'gatsby:accounts)
          ;;; gatsby:accounts.el ends here
        '';
      };
  }
