(mud-subtype "TinyMUD" "global"
  '(prompt . ">")
  '(connect-filter . tinymud-connect-filter)
  '(connect-command . "connect %s %s")
  '(filters . tinymud-filter-hook)
  '(connection-error-string . "Either that player does not exist, or has a different password.")
  '(page-regexp . "\\(You sense that [^ ]* is looking for you in \\|\\w+ pages: \\)")
  '(prefix-suffix-format . "PREFIX %s\nSUFFIX %s\n")
  )

(mud-subtype "TinyMUCK" "TinyMUD")
(mud-subtype "TinyMUSH" "TinyMUD")
(mud-subtype "TinyMUSE" "TinyMUSH"
	     '(page-regexp . "\\([^ ]* pages: .*\\)"))
(mud-subtype "TeenyMUD" "TinyMUD")
(mud-subtype "UnterMUD" "TinyMUD")

;;; TinyMUD

(defvar tinymud-filter-hook
  '(mud-check-reconnect mud-fill-lines)
  "*List of functions to call on each line of tinymud output.  The
function is called with no arguments and the buffer narrowed to just
the line.") 

(defvar tinymud-connection-error-string
  "Either that player does not exist, or has a different password.")

(defvar tinymud-macro-commands-file "~/.moo_tinymud_macros"
  "*Pathname of tinymud macros.")

(setq tinymud-output-filter nil)

(defun tinymud-connect-filter (proc string)
  "Filter for connecting to a TinyMUD server.  Replaced with tinymud-filter
once successful."
  (if (not (string-equal string tinymud-connection-error-string))
      (set-process-filter proc 'mud-filter)))

(provide 'j-tiny)

