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
 '(backup-by-copying t)
 '(bibtex-dialect (quote biblatex))
 '(cider-auto-jump-to-error nil)
 '(cider-inspector-page-size 32)
 '(cider-lein-parameters "trampoline repl :headless")
 '(cider-prompt-for-symbol nil)
 '(cider-use-overlays nil)
 '(clean-buffer-list-delay-general 1)
 '(clojure-defun-indents (quote (set-content-view! on-ui transact fact facts)))
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(debug-on-error nil)
 '(default-input-method "russian-computer")
 '(delete-old-versions t)
 '(dired-omit-extensions
   (quote
    (".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".bcf" ".out")))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+")
 '(eldoc-idle-delay 1)
 '(electric-indent-mode nil)
 '(fci-rule-color "#282a2e")
 '(fill-column 80)
 '(find-directory-functions (quote (cvs-dired-noselect dired-noselect sr-dired)))
 '(floobits-python-executable "python2.7")
 '(flymake-start-syntax-check-on-newline nil)
 '(font-lock-maximum-decoration (quote ((dired-mode) (sunrise) (t . t))))
 '(frame-resize-pixelwise t)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inferior-lisp-program "ccl" t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/usr/bin/aspell")
 '(kept-new-versions 6)
 '(latex-block-names (quote ("dmath" "lstlisting")))
 '(ls-lisp-verbosity (quote (links uid gid)))
 '(magit-commit-arguments nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-auto-show (quote (stage-all log-oneline log-select blame-follow)))
 '(magit-push-always-verify nil)
 '(magit-revision-insert-related-refs nil)
 '(magit-time-format-string "%b %d, %Y %T")
 '(midnight-mode t nil (midnight))
 '(minimap-width-fraction 0.1)
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
 '(org-mobile-directory "~/ownCloud/Notes")
 '(org-mobile-files
   (quote
    (org-agenda-files "~/Documents/Notes/gsoc.org" "~/Documents/Notes/life.org")))
 '(org-mobile-inbox-for-pull "~/Documents/Notes/from-mobile.org")
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("SC" . "http://joseito.republika.pl/sunrise-commander/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(persp-auto-save-opt 0)
 '(persp-auto-save-persps-to-their-file nil)
 '(pop-up-windows nil)
 '(projectile-enable-caching nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 20000)
 '(refheap-token "39bee1d0-8aa8-4168-ba05-24627adb1c9b")
 '(refheap-user "alexyakushev")
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval hs-hide-all)
     (langtool-local-disabled-rules "WHITESPACE_RULE" "MORFOLOGIK_RULE_EN_GB")
     (langtool-local-disabled-rules "WHITESPACE_RULE")
     (langtool-local-disabled-rules quote
                                    ("WHITESPACE_RULE"))
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
 '(smooth-scroll-margin 5)
 '(sr-attributes-display-mask (quote (nil nil nil nil t nil nil nil t)))
 '(sr-avfs-root "~/.avfs")
 '(sr-cursor-follows-mouse nil)
 '(sr-listing-switches "-alh")
 '(sr-show-file-attributes nil)
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
 '(yas/trigger-key "C-o"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(fringe ((t (:background "unspecified-bg"))))
 '(magit-branch-local ((t (:foreground "SkyBlue4"))))
 '(magit-branch-remote ((t (:foreground "DarkOliveGreen4"))))
 '(magit-diff-file-heading ((t nil)))
 '(magit-section-heading ((t (:weight bold))))
 '(magit-tag ((t (:foreground "Goldenrod4"))))
 '(minimap-active-region-background ((t (:background "#494949"))) t)
 '(preview-reference-face ((t (:foreground "#00CCCC" :background "#CCCCCC"))))
 '(show-paren-match ((t (:background "dim gray" :foreground "#00bb00"))))
 '(show-paren-mismatch ((t (:background nil :foreground "#ff0000" :bold t))))
 '(sr-active-path-face ((t (:foreground "#00CCCC" :weight bold :height 120))))
 '(sr-passive-path-face ((t (:foreground "#008888" :weight bold :height 120))))
 '(whitespace-space ((t (:background "#dd0000" :foreground "#999999"))))
 '(whitespace-trailing ((t (:background "#f2777a" :foreground "#f2777a")))))
