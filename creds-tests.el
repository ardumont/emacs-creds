(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(require 'creds)

(setq dat '(("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")
            ("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993")
            ("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd")
            ("machine" "jabber" "login" "some-login" "password" "some-pwd")
            ("machine" "description" "name" "\"my" "name" "is\"" "blog" "some-blog" "mail" "some-mail")))

(expectations (desc "creds/get")
  (expect '("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")      (creds/get dat "machine0"))
  (expect '("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993") (creds/get dat "machine1"))
  (expect nil                                                                             (creds/get dat "something-that-does-not-exist"))
  (expect nil                                                                             (creds/get dat nil))
  (expect nil                                                                             (creds/get nil nil)))

(setq machine '("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass"))

(expectations (desc "creds/get-entry")
  (expect "machine0" (creds/get-entry machine "machine"))
  (expect "http"     (creds/get-entry machine "port"))
  (expect "nouser"   (creds/get-entry machine "login"))
  (expect "nopass"   (creds/get-entry machine "password"))
  (expect nil        (creds/get-entry machine "something-that-does-not-exist"))
  (expect nil        (creds/get-entry nil nil)))

(provide 'creds-tests)
