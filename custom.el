(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.0)
 '(ac-auto-start nil)
 '(ac-quick-help-delay 0.5)
 '(ac-trigger-key "TAB")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#d6d6d6"))
 '(ansi-term-color-vector
   [unspecified "#001100" "#007700" "#00bb00" "#007700" "#009900" "#00bb00" "#009900" "#00bb00"] t)
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms '((".*" "~/.local/share/emacs-saves/" t)))
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.cache/emacs-saves")))
 '(bibtex-dialect 'biblatex)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(browse-url-generic-program "open")
 '(cider-auto-jump-to-error nil)
 '(cider-boot-parameters "repl -s wait")
 '(cider-cljs-lein-repl
   "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
 '(cider-clojure-cli-aliases ":dev:user:hotload")
 '(cider-clojure-cli-command "clojure")
 '(cider-default-repl-command "boot")
 '(cider-enlighten-mode nil)
 '(cider-font-lock-dynamically '(macro deprecated))
 '(cider-inject-dependencies-at-jack-in t)
 '(cider-inspector-page-size 40)
 '(cider-lein-parameters "trampoline repl :headless")
 '(cider-preferred-build-tool "boot")
 '(cider-prompt-for-symbol nil)
 '(cider-repl-history-file "~/.emacs.d/.cider-history")
 '(cider-repl-history-size 10000)
 '(cider-repl-init-code
   '("(do (clojure.core/apply clojure.core/require clojure.main/repl-requires) (load-file (str (System/getProperty \"user.home\") \"/.clojure/user.clj\")) (in-ns 'user))"))
 '(cider-repl-use-pretty-printing nil)
 '(cider-use-overlays nil)
 '(cider-use-tooltips nil)
 '(clean-buffer-list-delay-general 1)
 '(clj-decompiler-inject-dependencies-at-jack-in nil)
 '(cljr-cljc-clojure-test-declaration
   "#?(:clj [clojure.test :refer :all]
:cljs [cljs.test :as t :include-macros true])")
 '(cljr-clojure-test-declaration "[clojure.test :refer :all]")
 '(cljr-eagerly-build-asts-on-startup nil)
 '(cljr-favor-prefix-notation nil)
 '(cljr-magic-requires nil)
 '(cljr-populate-artifact-cache-on-startup nil)
 '(clojure-build-tool-files '("build.boot" "project.clj" "deps.edn" "shadow-cljs.edn"))
 '(clojure-defun-indents '(set-content-view! on-ui transact fact facts fact-group))
 '(clojure-omit-space-between-tag-and-delimiters '(91 123 40))
 '(clojure-toplevel-inside-comment-form t)
 '(company-quickhelp-delay 0.5)
 '(company-quickhelp-mode t)
 '(completion-styles '(basic partial-completion emacs22))
 '(create-lockfiles nil)
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))
 '(debug-on-error nil)
 '(default-frame-alist '((inhibit-double-buffering . t) (vertical-scroll-bars)))
 '(default-input-method "ukrainian-computer")
 '(deft-directory "/Users/alex/mega/Docs/Text/deft/")
 '(deft-extensions '("org" "txt" "md"))
 '(deft-use-filename-as-title t)
 '(delete-old-versions t)
 '(diff-switches "-u")
 '(dired-kill-when-opening-new-dired-buffer t)
 '(dired-omit-extensions
   '(".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".bcf" ".out"))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+")
 '(dired-omit-verbose nil)
 '(display-fill-column-indicator-character 183)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-idle-delay 0.5)
 '(electric-indent-mode nil)
 '(fci-rule-color "#d6d6d6" t)
 '(ffap-machine-p-known 'reject)
 '(fill-column 80)
 '(find-directory-functions '(cvs-dired-noselect dired-noselect sr-dired))
 '(fish-indent-offset 2)
 '(font-lock-maximum-decoration '((dired-mode) (sunrise) (t . t)))
 '(frame-resize-pixelwise t)
 '(git-commit-finish-query-functions nil)
 '(git-timemachine-global-git-arguments '("-c" "log.showSignature=false" "--no-pager"))
 '(global-auto-revert-non-file-buffers t)
 '(global-whitespace-mode t)
 '(helm-ag-base-command "rg --no-heading --smart-case")
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*magit" "\\*buffer-selection" "\\*Compile-log\\*" "\\*Quail" "\\*Minibuf" "\\*Completions" "\\*Shell" "\\*Customize" "\\*Help\\*" ".+ (Sunrise)"))
 '(highlight-parentheses-colors '("turquoise1" "khaki2" "firebrick2" "blue1" "magenta2"))
 '(highlight-parentheses-delay 0.5)
 '(idle-highlight-exceptions nil)
 '(ido-auto-merge-work-directories-length nil)
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-max-prospects 10)
 '(ido-use-filename-at-point 'guess)
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ispell-program-name "/usr/local/bin/aspell")
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(kept-new-versions 6)
 '(latex-block-names '("dmath" "lstlisting"))
 '(ls-lisp-verbosity '(links uid gid))
 '(mac-auto-operator-composition-characters "!\"#$%&'*+,-./:;<=>?@^_`|~")
 '(magit-blame-goto-chunk-hook nil)
 '(magit-commit-arguments nil)
 '(magit-commit-show-diff nil)
 '(magit-completing-read-function 'magit-ido-completing-read)
 '(magit-diff-expansion-threshold 1)
 '(magit-gh-pulls-arguments '("--use-pr-editor"))
 '(magit-push-always-verify nil)
 '(magit-revision-insert-related-refs nil)
 '(magit-section-initial-visibility-alist nil)
 '(magit-time-format-string "%b %d, %Y %T")
 '(magit-visit-ref-behavior '(focus-on-ref create-branch checkout-any))
 '(midnight-mode t nil (midnight))
 '(mouse-yank-at-point t)
 '(nrepl-log-messages nil)
 '(openwith-associations
   '(("\\.pdf\\'" "evince"
      (file))
     ("\\.mp3\\'" "xmms"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jpe?g\\|png\\)\\'" "open"
      (file))
     ("\\.odt\\'" "lowriter"
      (file))
     ("\\.docx?\\'" "lowriter"
      (file))
     ("\\.xlsx?\\'" "localc"
      (file))))
 '(openwith-mode nil)
 '(org-directory "~/Documents/Notes/")
 '(org-reveal-root "../reveal-js")
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(inspector fireplace bm centered-window-mode cider clj-decompiler clj-refactor clojure-mode color-theme-sanityinc-tomorrow company company-flx company-quickhelp csv-mode deft docker-tramp dockerfile-mode dumb-jump ediff elisp-slime-nav fish-mode flyspell git-gutter git-timemachine gnu-elpa-keyring-update go-mode gradle-mode graphviz-dot-mode groovy-mode haskell-mode hcl-mode helm-ag helm-cider helm-swoop hideshow highlight highlight-parentheses highlight-symbol idle-highlight-mode ido-ubiquitous java-snippets javap-mode json-mode json-reformat lsp-mode lua-mode magit markdown-mode markdown-preview-mode multiple-cursors names nginx-mode nlinum openwith paredit paren-face pcre2el phi-search projectile rainbow-mode rust-mode rustic sass-mode smartparens smex string-edit sudo sunrise-x-checkpoints sunrise-x-loop sunrise-x-mirror sunrise-x-tabs systemd terraform-mode toml-mode unicode-fonts use-package visual-regexp vlf vundo wakatime-mode web-mode yaml-mode zencoding-mode))
 '(pixel-scroll-mode nil)
 '(pop-up-windows nil)
 '(projectile-cache-file "/home/unlogic/.emacs.d/var/projectile.cache")
 '(projectile-enable-caching nil)
 '(projectile-mode-line " Projectile")
 '(recentf-auto-cleanup 'never)
 '(recentf-max-saved-items 200)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (multiline-comment-handler . defun)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (auto-fill-mode . -1)
     (eval hs-hide-all)
     (whitespace-line-column . 10000)
     (lexical-binding . t)))
 '(save-place-file "~/.emacs.d/var/places")
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(sr-attributes-display-mask '(nil nil nil nil t nil nil nil t))
 '(sr-avfs-root "~/.avfs")
 '(sr-cursor-follows-mouse nil)
 '(sr-listing-switches "-alh")
 '(sr-show-file-attributes nil)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(use-short-answers t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00")))
 '(vc-annotate-very-old-color nil)
 '(version-control t)
 '(visible-bell nil)
 '(vr/default-feedback-limit 5)
 '(vr/engine 'pcre2el)
 '(wakatime-api-key "1eb79483-57c5-4467-bdf9-1db892e2c3e4")
 '(wakatime-cli-path "wakatime-cli")
 '(wakatime-python-bin "")
 '(warning-suppress-types '((comp) (undo discard-info)))
 '(whitespace-global-modes '(not magit-status-mode))
 '(whitespace-style '(face trailing tabs empty))
 '(world-clock-list
   '(("Europe/Kyiv" "Kyiv")
     ("Europe/London" "London")
     ("Europe/Berlin" "Berlin")
     ("America/New_York" "New York")
     ("America/Los_Angeles" "San Francisco")))
 '(yas/trigger-key "C-o"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "unknown" :family "Fira Code"))))
 '(fringe ((t (:background "white"))))
 '(git-timemachine-minibuffer-detail-face ((t (:foreground "sea green"))))
 '(magit-blame-heading ((t (:background "grey80" :foreground "black"))))
 '(magit-branch-local ((t (:foreground "SkyBlue4"))))
 '(magit-branch-remote ((t (:foreground "DarkOliveGreen4"))))
 '(magit-diff-added ((((class color) (background light)) :background "#ddffdd" :foreground "#22aa22") (((class color) (background dark)) :background "#335533" :foreground "#ddffdd")))
 '(magit-diff-added-highlight ((((class color) (background light)) :background "#cceecc" :foreground "#22aa22") (((class color) (background dark)) :background "#336633" :foreground "#cceecc")))
 '(magit-diff-file-heading ((t nil)))
 '(magit-diff-removed ((((class color) (background light)) :background "#ffdddd" :foreground "#aa2222") (((class color) (background dark)) :background "#553333" :foreground "#ffdddd")))
 '(magit-diff-removed-highlight ((((class color) (background light)) :background "#eecccc" :foreground "#aa2222") (((class color) (background dark)) :background "#663333" :foreground "#eecccc")))
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
 '(sr-tabs-active-face ((t (:inherit variable-pitch :weight bold :height 0.9))))
 '(sr-tabs-inactive-face ((t (:inherit variable-pitch :height 0.9))))
 '(whitespace-space ((t (:background "#dd0000" :foreground "#999999"))))
 '(whitespace-trailing ((t (:background "deep sky blue" :foreground "deep sky blue")))))
