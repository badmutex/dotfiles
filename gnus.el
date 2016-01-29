
(setq user-mail-address "abdulwahidc@gmail.com"
      user-full-name "Badi' Abdul-Wahid")

;; (setq gnus-select-method 
;;       '(nnmaildir "gmail"
;;                   (directory "~/.maildir/abdulwahidc@gmail.com/")))

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port "imaps")
                                  (nnimap-stream ssl))

      ;; where we store the password
      ;; nntp-authinfo-file "~/.authinfo.gpg"
      ;; auth-source-debug t

      ;; agent seems to confuse nnimap
      gnus-agent nil

      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"


      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial attempt add work address.
;;
;; Doesn't quite support sending emails as the recipient (eg if
;; received @iu.edu then send from @iu.edu)
;;

;; (setq gnus-secondary-select-methods '((nnimap "iu"
;;                                               (nnimap-address "imap.exchange.iu.edu")
;;                                               (nnimap-server-port "imaps")
;;                                               (nnimap-stream ssl))))

;; gnus-agent nil

;; user-mail-address "badi@iu.edu"
;; smtpmail-smtp-service 587
;; send-mail-function 'smtpmail-send-it
;; smtpmail-smtp-user "badi"
;; smtpmail-smtp-server "mail-relay.iu.edu")


(setq mm-text-html-renderer 'shr
      ;; SHR rendering is pretty terrible with a dark theme
      ;; https://emacs.stackexchange.com/questions/2955
      ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2014-07/msg00243.html
      shr-color-visible-distance-min 50
      shr-color-visible-luminance-min 70


      gnus-thread-sort-functions '(gnus-thread-sort-by-number
                                   gnus-thread-sort-by-subject
                                   gnus-thread-sort-by-score
                                   gnus-thread-sort-by-most-recent-date)
      gnus-use-adaptive-scoring '(word line)
      gnus-default-adaptive-score-alist '((gnus-unread-mark)
                                          (gnus-ticked-mark (from 4))
                                          (gnus-dormant-mark (from 5))
                                          (gnus-del-mark (from -4) (subject -1))
                                          (gnus-read-mark (from 4) (subject 2))
                                          (gnus-expirable-mark (from -1) (subject -1))
                                          (gnus-killed-mark (from -1) (subject -3))
                                          (gnus-kill-file-mark)
                                          (gnus-ancient-mark)
                                          (gnus-low-score-mark)
                                          (gnus-catchup-mark (from -1) (subject -1)))

      ;; hide citations by default since I read by thread
      gnus-treat-hide-citation t

      ;; don't break threads when entering groups with new messages
      gnus-fetch-old-headers 'some

      ;; see C-u C-x = for information on character under cursor
      gnus-sum-thread-tree-root "╭▷" ; `>`
      gnus-sum-thread-tree-false-root "▷" ; `>`
      ;; gnus-sum-thread-tree-single-indent " " ; ``
      gnus-sum-thread-tree-vertical "│" ; `| `
      gnus-sum-thread-tree-indent " " ; `-`
      gnus-sum-thread-tree-leaf-with-other "├► " ;`+->`
      gnus-sum-thread-tree-single-leaf "╰► " ; `\->`
      gnus-user-date-format-alist '(((gnus-seconds-today) . "T   %H:%M")
                                    ((+ 86400 (gnus-seconds-today)) . "Y   %H:%M")
                                    (604800 . "%a %H:%M")
                                    ((gnus-seconds-month) . "%a %d")
                                    ((gnus-seconds-year) . "%b %d")
                                    (t . "%b %d %Y"))
      gnus-summary-line-format "%U%R%z %-12&user-date; %(%[%-42,42n%]:%) %B %s\n")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mime for mime-ifying messages
;;
;; use org-mime that comes with org-mode
;;
;; make sure that org-mime from elpa is not present, as it is an old
;; version that conflicts with one provided in org-mode/contrib/lisp.
;;
;; M-x package-show-package-list and uninstall it if necessary
;;
;; I was getting odd "symbol not found" error until I figured this
;; out.


;; org mime is autoloaded contrib/lisp and doesn't seem to be required
;; after `el-get-bundle` so we do it manually
(require 'org-mime)

;; this allows org-mode-based fancy structure editing and tables in a
;; gnus message buffer
(add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'turn-on-orgtbl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google-contacts for looking email address from google
;;
;; On first invokation, a web browser is opened to authorize the oauth
;; access to contacts. The credentials are then stored in
;; ~/.emacs.d/oauth2.plstore using gpg.
;;
;; The default encryption scheme is to use symetric encryption.  To
;; use public-key encryption, set plstore-encrypt-to to the identifier
;; in your gpg keys of the receiver.
;;
;; Do note that this sets it globally. If you do want to use symetric
;; encryption with plstore, you will need to unset it (or set it to
;; nil).

(setq plstore-encrypt-to "Badi' Abdul-Wahid")

(el-get-bundle google-contacts)
(require 'google-contacts-gnus)
(require 'google-contacts-message)

