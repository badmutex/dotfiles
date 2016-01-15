(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)))

(setq smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq mm-text-html-renderer 'gnus-w3m)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-date
        gnus-thread-sort-by-subject
        gnus-thread-sort-by-score))
