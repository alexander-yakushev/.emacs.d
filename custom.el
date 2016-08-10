(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style
   (quote
    (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))
 '(ac-auto-show-menu 0.0)
 '(ac-auto-start nil)
 '(ac-quick-help-delay 0.5)
 '(ac-trigger-key "TAB")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(ansi-term-color-vector
   [unspecified "#001100" "#007700" "#00bb00" "#007700" "#009900" "#00bb00" "#009900" "#00bb00"] t)
 '(backup-by-copying t)
 '(beacon-blink-delay 0.1)
 '(bibtex-dialect (quote biblatex))
 '(browse-url-browser-function (quote browse-url-chromium))
 '(cider-auto-jump-to-error nil)
 '(cider-boot-parameters "cider repl -s wait")
 '(cider-cljs-lein-repl
   "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
 '(cider-inspector-page-size 32)
 '(cider-lein-parameters "trampoline repl :headless")
 '(cider-prompt-for-symbol nil)
 '(cider-use-overlays nil)
 '(cider-use-tooltips nil)
 '(clean-buffer-list-delay-general 1)
 '(cljr-eagerly-build-asts-on-startup nil)
 '(cljr-favor-prefix-notation nil)
 '(clojure-defun-indents
   (quote
    (set-content-view! on-ui transact fact facts fact-group)))
 '(completion-styles (quote (basic partial-completion emacs22)))
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("abb628b7ecdc570350db589ea22f8ae0f4b9e5304ea313532acbb84d703eecbd" "64da9a8dba17dcf210420875eba3f1a5ea6272217dc403706e4e2c985aa537fa" "1d6a2b8d5719875cd5f268ea4c2d4a24254122f9c63619b45d82404dd7359951" "2c73253d050a229d56ce25b7e5360aa2f7566dfd80174da8e53bd9d3e612a310" "479f188da96dcf244be270724c23de58607c031626bde8ba8243799f209d16b1" "36012edb5bc7070a17e989984e0ecc1d1e9c94326bdd0fbd76c2a45ebfe7da54" "a922c743710bb5d7c14995345549141f01211ff5089057dc718a5a33104c3fd1" "cb8039d38d197de5049bd2e0e57b0a9001d89d820c3b36c945a12d6b5198e810" "a93f214aac52d55f7f53dc95ba2ebd87814dbc812ad0750960ee4229da5c9321" "1b4243872807cfad4804d7781c51d051dfcc143b244da56827071a9c2e10ab7f" "c73236c58c77d76271fef510552c4c43c4c69748f4bfd900b132ad17cc065611" "744c95ef0117ca34c5d4261cb00aff6750fd60338240c28501adb5701621e076" "5424f18165ed7fd9c3ec8ea43d801dc9c71ab9da2b044000162a47c102ef09ea" "1dfd7a150e80fdb4563f594716d09d849f4c50bcea12825bd8d284c05a87a3e1" "8e0781b24291a7b29a411ba29ed01c8c2ee696c03c3dfdb3c3e89f8655db78ed" "c55d8474e898e1231c49547d50e15d05c387e4111f4085f5fb7120a7418165c2" "8ffaf449297bd9a08517f4b03a4df9dbf3e347652746cefceb3ee57c8e584b9f" "b6d649c9f972b491686e7fa634535653e6222c1faca1ab71b3117854470a79ae" "6ae93caf30ad7eef728589a4d7b7befadecade71d78b904a64a0480608a7b61e" "3f873e7cb090efbdceafb8f54afed391899172dd917bb7a354737a8bb048bd71" "b110da1a5934e91717b5c490709aba3c60eb4595194bbf9fdcbb97d247c70cfa" "90b1aeef48eb5498b58f7085a54b5d2c9efef2bb98d71d85e77427ce37aec223" "9e87bddff84cbce28c01fa05eb22f986d770628fa202cd5ca0cd7ed53db2f068" "2f8c57e0f449547a9fd89a49220491d88e2facd7756c49b70fead256d37fd4fa" "3a3917dbcc6571ef3942c2bf4c4240f70b5c4bc0b28192be6d3f9acd83607a24" "9567c8b113a53efdf4e7f3ab47564cb44b27ee231ece20811bb191698b1b8b6b" "11bfbaeb7c000f1e0792c664c8c948eeb896a771cb1aa19121c7ee01ad4e612b" "7e346cf2cb6a8324930c9f07ce050e9b7dfae5a315cd8ed3af6bcc94343f8402" "b2028956188cf668e27a130c027e7f240c24c705c1517108b98a9645644711d9" "275ed2b17e22759bed06564fddbb703c7b0893c76d17a0f353614f556c46d05e" "5cc9df26a180d14a6c5fc47df24d05305636c80030a85cf65e31f420d7836688" "ee3b22b48b269b83aa385b3915d88a9bf4f18e82bb52e20211c7574381a4029a" "08dc5159473fa2250619880857eee06b7f4067f5f15b0ee8878c91f135cef6d5" "cb18233197cedab557c70d171b511bed49cc702f428750925280090c31498bd2" "07840b49217157323d6ea4ccbdecc451b5989ebdc6e06cb0b4d742a141475a44" "1edf370d2840c0bf4c031a044f3f500731b41a3fd96b02e4c257522c7457882e" "46b20113556c07c1173d99edc6609473a106c13871da8fc9acb6534224f1e3e4" "91fba9a99f7b64390e1f56319c3dbbaed22de1b9676b3c73d935bf62277b799c" "d1a42ed39a15a843cccadf107ee0242b5f78bfbb5b70ba3ce19f3ea9fda8f52d" "1a2b131a7844bad234832963d565097efc88111b196fb75757885c159c5f8137" "a7b47876e5da7cac6f5e61cca7a040a365ca2c498823654bd4076add8edf34c5" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "a17f246690840fcf3fc26cb845ffedd2d8e1161cae386c14df61dabb9af3a5a9" "0bd7a42bd443517e5e61dac3cabc24018fbd0c6b2b4199b3c4efd9e3727efd30" "232f715279fc131ed4facf6a517b84d23dca145fcc0e09c5e0f90eb534e1680f" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "d69a0f6d860eeff5ca5f229d0373690782a99aee2410a3eed8a31332a7101f1e" "2f4afdef79a7f8a6b54f7e70959e059d7e09cf234d412662e0897cacd46f04b4" "b1bcb837df0455af8e91114b7a3bddfa084cde32ceb16b1b468d5e5e8605a835" "2162da67ce86c514aff010de1b040fb26663ca42afebc2de26515d741121c435" "df7dd02705b5868a4bf38069d3b5335412b8210cabfbcd86860cea3b19ba2a3d" "e1551b5516e0a439b6ab019ba00cee866e735f66f22ff67a5d882ad0f1383454" "39e93a10eb292941640adfe28509e0c3eeb84e30cbfed6ef9841be136081ca34" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" "0ae52e74c576120c6863403922ee00340a3bf3051615674c4b937f9c99b24535" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "f1a6cbc40528dbee63390fc81da426f1b00b4fc09a60fe35752f5838b12fbe0a" "e254f8e18ba82e55572c5e18f3ac9c2bd6728a7e500f6cc216e0c6f6f8ea7003" "43aeadb0c8634a9b2f981ed096b3c7823c511d507a51c604e4667becb5ef6e35" "101a10b15bbbd0d5a0e56e4773e614962197886780afb2d62523a63a144ad96c" "c70cc9c4c6257d70f5c11b90cb9e8b1e54e6edd6aa43f39879746e16a70533f5" "9f6750057fefba39c184783c7b80ddd9c63bc6e8064846b423b4362c9e930404" "06fc6014871028d24b4e03db24b9efee48bd73dce0afdc15e9124f09fab64afa" "890d09dcc8d2326e98eee74b307b2cc42f07ab7701bcff521e6152aa3e08f7a8" "b4ec581daad15aa7020b722523dc6bcea850bfbdbe31bfeb11c45ea51899bd75" "e8e744a1b0726814ac3ab86ad5ccdf658b9ff1c5a63c4dc23841007874044d4a" "f2503f0a035c2122984e90eb184185769ee665de5864edc19b339856942d2d2d" "e8bba3c8e8caea2c7a8b6932b0db8d9bdb468c9b44bf554f37b56093d23fde57" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "76bd62f6ce376bf0597fab7f478eaa98cd94a7b41f0ae46de63a958fbe99c1d9" "09669536b4a71f409e7e2fd56609cd7f0dff2850d4cbfb43916cc1843c463b80" "294834baa9ca874795a3181cce7aaf228b1e3fb3899587ffd3ae7546de328c90" "f21caace402180ab3dc5157d2bb843c4daafbe64aadc362c9f4558ac17ce43a2" "e033c4abd259afac2475abd9545f2099a567eb0e5ec4d1ed13567a77c1919f8f" "16d6e7f87846801e17e0c8abc331cf6fa55bec73185a86a431aca6bec5d28a0a" "e681c4fc684a543ce97c2d55082c6585182c0089f605dc9a5fe193870f03edc6" "82cbb553a225b75ee49901fa06562941fbfe5e6fed24cda985e7ea59af7ddc80" "db9feb330fd7cb170b01b8c3c6ecdc5179fc321f1a4824da6c53609b033b2810" "6916fa929b497ab630e23f2a4785b3b72ce9877640ae52088c65c00f8897d67f" "ad68cb14359254795c6b96d76334aaacb739c04f64a4a8567964d4a20aa723d2" "1462969067f2ff901993b313085d47e16badeec58b63b9ed67fa660cebaaddae" "b83c1e19c912f0d84a543b37367242f8a3ad2ed3aec80f5363d0d82ba4621e7d" "e2e4e109357cfcebccb17961950da6b84f72187ade0920a4494013489df648fe" "75c0b9f9f90d95ac03f8647c75a91ec68437c12ff598e2abb22418cd4b255af0" "bf81a86f9cfa079a7bb9841bc6ecf9a2e8999b85e4ae1a4d0138975921315713" "01c5ebefcabc983c907ee30e429225337d0b4556cc1d21df0330d337275facbb" "e24679edfdea016519c0e2d4a5e57157a11f928b7ef4361d00c23a7fe54b8e01" "d43120398682953ef18fd7e11e69c94e44d39bb2ab450c4e64815311542acbff" "3fb38c0c32f0b8ea93170be4d33631c607c60c709a546cb6199659e6308aedf7" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(debug-on-error nil)
 '(default-input-method "russian-computer")
 '(delete-old-versions t)
 '(dired-omit-extensions
   (quote
    (".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".bcf" ".out")))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-idle-delay 1)
 '(electric-indent-mode nil)
 '(fci-rule-color "#282a2e")
 '(fill-column 80)
 '(find-directory-functions (quote (cvs-dired-noselect dired-noselect sr-dired)))
 '(floobits-python-executable "python2.7")
 '(flymake-start-syntax-check-on-newline nil)
 '(font-lock-maximum-decoration (quote ((dired-mode) (sunrise) (t . t))))
 '(frame-resize-pixelwise t)
 '(git-commit-finish-query-functions nil)
 '(global-whitespace-mode t)
 '(gradle-mode nil)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*magit" "\\*buffer-selection" "\\*Compile-log\\*" "\\*Quail" "\\*Minibuf" "\\*Completions" "\\*Shell" "\\*Customize" "\\*Help\\*" ".+ (Sunrise)")))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inferior-lisp-program "ccl" t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/usr/bin/aspell")
 '(kept-new-versions 6)
 '(latex-block-names (quote ("dmath" "lstlisting")))
 '(ls-lisp-verbosity (quote (links uid gid)))
 '(magit-blame-goto-chunk-hook nil)
 '(magit-commit-arguments nil)
 '(magit-commit-show-diff nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-gh-pulls-arguments (quote ("--use-pr-editor")))
 '(magit-push-always-verify nil)
 '(magit-revision-insert-related-refs nil)
 '(magit-time-format-string "%b %d, %Y %T")
 '(midnight-mode t nil (midnight))
 '(minimap-display-semantic-overlays nil)
 '(minimap-update-delay 0.2)
 '(minimap-width-fraction 0.15)
 '(minimap-window-location (quote right))
 '(mouse-yank-at-point t)
 '(nrepl-log-messages t)
 '(openwith-associations
   (quote
    (("\\.pdf\\'" "evince"
      (file))
     ("\\.mp3\\'" "xmms"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jpe?g\\|png\\)\\'" "feh"
      (file))
     ("\\.odt\\'" "lowriter"
      (file))
     ("\\.docx?\\'" "lowriter"
      (file))
     ("\\.xlsx?\\'" "localc"
      (file))
     ("\\.svg\\'" "display"
      (file)))))
 '(openwith-mode t)
 '(org-directory "~/Documents/Notes/")
 '(org-reveal-root "../reveal-js")
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("SC" . "http://joseito.republika.pl/sunrise-commander/")
     ("melpa" . "http://melpa.milkbox.net/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/"))))
 '(pop-up-windows nil)
 '(projectile-cache-file "/home/unlogic/.emacs.d/var/projectile.cache")
 '(projectile-enable-caching nil)
 '(projectile-mode-line " Projectile")
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 20000)
 '(refheap-token "39bee1d0-8aa8-4168-ba05-24627adb1c9b")
 '(refheap-user "alexyakushev")
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((auto-fill-mode . -1)
     (eval hs-hide-all)
     (langtool-local-disabled-rules "WHITESPACE_RULE" "MORFOLOGIK_RULE_EN_GB")
     (langtool-local-disabled-rules "WHITESPACE_RULE")
     (langtool-local-disabled-rules quote
                                    ("WHITESPACE_RULE"))
     (whitespace-line-column . 10000)
     (lexical-binding . t))))
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
 '(sr-attributes-display-mask (quote (nil nil nil nil t nil nil nil t)))
 '(sr-avfs-root "~/.avfs")
 '(sr-cursor-follows-mouse nil)
 '(sr-listing-switches "-alh")
 '(sr-show-file-attributes nil)
 '(tramp-default-method "ssh")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(version-control t)
 '(visible-bell nil)
 '(wakatime-api-key "d63979c5-252c-4bdc-bd01-c2dba0471c4d")
 '(wakatime-cli-path "~/Software/wakatime/wakatime/cli.py")
 '(warning-suppress-types (quote ((undo discard-info))))
 '(whitespace-style (quote (face trailing tabs empty)))
 '(yas/trigger-key "C-o"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(fringe ((t (:background "white"))))
 '(git-timemachine-minibuffer-detail-face ((t (:foreground "sea green"))))
 '(magit-blame-heading ((t (:background "grey80" :foreground "black"))))
 '(magit-branch-local ((t (:foreground "SkyBlue4"))))
 '(magit-branch-remote ((t (:foreground "DarkOliveGreen4"))))
 '(magit-diff-file-heading ((t nil)))
 '(magit-section-heading ((t (:weight bold))))
 '(magit-tag ((t (:foreground "Goldenrod4"))))
 '(minimap-active-region-background ((t (:background "#494949"))))
 '(mode-line ((t (:weight normal :box nil))))
 '(mode-line-inactive ((t (:weight normal :box nil))))
 '(preview-reference-face ((t (:foreground "#00CCCC" :background "#CCCCCC"))))
 '(show-paren-match ((t (:foreground "grey70" :bold nil :background "#008800"))))
 '(show-paren-mismatch ((t (:foreground "grey70" :bold nil :background "#880000"))))
 '(sr-active-path-face ((t (:foreground "#00CCCC" :weight bold :height 120))))
 '(sr-passive-path-face ((t (:foreground "#008888" :weight bold :height 120))))
 '(whitespace-space ((t (:background "#dd0000" :foreground "#999999"))))
 '(whitespace-trailing ((t (:background "deep sky blue" :foreground "deep sky blue")))))
