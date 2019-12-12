(define-keyset 'test-admin (read-keyset "test-admin-keyset"))

(namespace 'free)

(module inject-test 'test-admin

  (defschema account
    balance:decimal
    amount:decimal
    data)

  (deftable accounts:{account})

  (defun create-account (id init-bal)
    (insert accounts id
         { "balance": init-bal, "amount": init-bal, "data": "Created account" }))


  (defun read-account (id)
    "Read data for account ID"
    (+ { "account": id } (read accounts id)))


 (defun create-global-accounts ()
   (create-account "Acct1" 1000000.0)
   (create-account "Acct2" 0.0)

 (defun read-balance:decimal (id:string)
   (at "balance" (read accounts id))
 )
)

(create-table accounts)
(create-global-accounts)
