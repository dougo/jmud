(setq load-path (cons
		 (expand-file-name "~/emacs/jmud")
		 load-path))
(autoload 'moo-code-mode "j-moo-code" "Major mode for editing MOO-code." t)
(autoload 'mud "j-mud" "Connect to a MUD." t)
(setq auto-mode-alist (cons '("\\.moo$" . moo-code-mode) auto-mode-alist))
(global-set-key "\C-cm" 'mud)
(setq moo-use-@program t)
(setq moo-browser-worlds '(("LambdaMOO")))
(setq use-suppress-all-input t)
(setq moo-filter-hook
      (setq tinymud-filter-hook
	    '(mud-check-triggers mud-check-reconnect mud-activity-alert)))
(setq mud-do-activity-alert t)
