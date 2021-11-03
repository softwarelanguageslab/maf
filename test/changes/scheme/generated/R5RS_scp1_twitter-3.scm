; Changes:
; * removed: 0
; * added: 1
; * swaps: 4
; * negated predicates: 1
(letrec ((result ())
         (output (lambda (i)
                   (set! result (cons i result))))
         (linebreak (lambda ()
                      (set! result (cons 'linebreak result))))
         (output-all (lambda args
                       (for-each output args)))
         (output-all-sep (lambda (args)
                           (for-each (lambda (arg) (output arg) (output " ")) args)))
         (make-tweet (lambda (username text tags)
                       (letrec ((output-tweet (lambda ()
                                                (output-all "Tweet from " username "\n" text "\nTags: ")
                                                (output-all-sep tags)
                                                (linebreak)))
                                (dispatch (lambda (msg)
                                            (<change>
                                               ()
                                               (display output-tweet))
                                            (if (eq? msg 'text)
                                               text
                                               (if (eq? msg 'tags)
                                                  tags
                                                  (if (eq? msg 'username)
                                                     username
                                                     (if (eq? msg 'output)
                                                        output-tweet
                                                        (begin
                                                           (<change>
                                                              (output "error - wrong msg ")
                                                              (output msg))
                                                           (<change>
                                                              (output msg)
                                                              (output "error - wrong msg "))))))))))
                          (if (> (string-length text) 140) #f dispatch))))
         (make-account (lambda (name username)
                         (let ((followers ())
                               (tweets ())
                               (tweet-wall ()))
                            (letrec ((follow (lambda (account)
                                               ((account 'add-follower) dispatch)))
                                     (add-follower (lambda (account)
                                                     (set! followers (cons account followers))))
                                     (tweet (lambda (text . tags)
                                              (let ((tweet-obj (make-tweet username text tags)))
                                                 (set! tweets (cons tweet-obj tweets))
                                                 (set! tweet-wall (cons tweet-obj tweet-wall))
                                                 (for-each (lambda (follower) ((follower 'add-tweet-to-wall) tweet-obj)) followers))))
                                     (add-tweet-to-wall (lambda (tweet)
                                                          (set! tweet-wall (cons tweet tweet-wall))))
                                     (output-account (lambda (symbol)
                                                       (if (eq? symbol 'wall)
                                                          (output-wall)
                                                          (if (eq? symbol 'followers)
                                                             (output-followers)
                                                             (if (eq? symbol 'account)
                                                                (output-entire-account)
                                                                (output "wrong symbol given"))))))
                                     (output-wall (lambda ()
                                                    (output "TWEET WALL")
                                                    (linebreak)
                                                    (for-each
                                                       (lambda (tweet)
                                                          (<change>
                                                             ((tweet 'output))
                                                             (linebreak))
                                                          (<change>
                                                             (linebreak)
                                                             ((tweet 'output))))
                                                       tweet-wall)))
                                     (output-followers (lambda ()
                                                         (output "FOLLOWERS")
                                                         (linebreak)
                                                         (for-each (lambda (follower) (output (follower 'username)) (output " ")) followers)))
                                     (output-entire-account (lambda ()
                                                              (output-all "Twitter name " username "\n" "Name " name "\n")
                                                              (<change>
                                                                 (output-wall)
                                                                 (output-followers))
                                                              (<change>
                                                                 (output-followers)
                                                                 (output-wall))
                                                              (linebreak)
                                                              (linebreak)))
                                     (dispatch (lambda (msg)
                                                 (if (eq? msg 'name)
                                                    name
                                                    (if (eq? msg 'username)
                                                       username
                                                       (if (eq? msg 'output)
                                                          output-account
                                                          (if (eq? msg 'follow)
                                                             follow
                                                             (if (<change> (eq? msg 'add-follower) (not (eq? msg 'add-follower)))
                                                                add-follower
                                                                (if (eq? msg 'tweet)
                                                                   tweet
                                                                   (if (eq? msg 'add-tweet-to-wall)
                                                                      add-tweet-to-wall
                                                                      (begin
                                                                         (<change>
                                                                            (output "error - wrong msg ")
                                                                            (output msg))
                                                                         (<change>
                                                                            (output msg)
                                                                            (output "error - wrong msg ")))))))))))))
                               dispatch))))
         (my-tweet (make-tweet "madewael" "Racket is cool!" (list "#Racket" "#Scheme")))
         (res1 (equal? (my-tweet 'username) "madewael")))
   ((my-tweet 'output))
   (letrec ((accountE (make-account "Eline Philips" "ephilips"))
            (accountM (make-account "Mattias De Wael" "madewael")))
      ((accountE 'follow) accountM)
      ((accountM 'tweet) "Racket is cool!" "#Racket" "#Scheme")
      ((accountE 'tweet) "Hello World!")
      ((accountE 'output) 'account)
      ((accountM 'output) 'account)
      (if res1
         (equal?
            result
            (__toplevel_cons
               'linebreak
               (__toplevel_cons
                  'linebreak
                  (__toplevel_cons
                     " "
                     (__toplevel_cons
                        "ephilips"
                        (__toplevel_cons
                           'linebreak
                           (__toplevel_cons
                              "FOLLOWERS"
                              (__toplevel_cons
                                 'linebreak
                                 (__toplevel_cons
                                    'linebreak
                                    (__toplevel_cons
                                       " "
                                       (__toplevel_cons
                                          "#Scheme"
                                          (__toplevel_cons
                                             " "
                                             (__toplevel_cons
                                                "#Racket"
                                                (__toplevel_cons
                                                   "\nTags: "
                                                   (__toplevel_cons
                                                      "Racket is cool!"
                                                      (__toplevel_cons
                                                         "\n"
                                                         (__toplevel_cons
                                                            "madewael"
                                                            (__toplevel_cons
                                                               "Tweet from "
                                                               (__toplevel_cons
                                                                  'linebreak
                                                                  (__toplevel_cons
                                                                     "TWEET WALL"
                                                                     (__toplevel_cons
                                                                        "\n"
                                                                        (__toplevel_cons
                                                                           "Mattias De Wael"
                                                                           (__toplevel_cons
                                                                              "Name "
                                                                              (__toplevel_cons
                                                                                 "\n"
                                                                                 (__toplevel_cons
                                                                                    "madewael"
                                                                                    (__toplevel_cons
                                                                                       "Twitter name "
                                                                                       (__toplevel_cons
                                                                                          'linebreak
                                                                                          (__toplevel_cons
                                                                                             'linebreak
                                                                                             (__toplevel_cons
                                                                                                'linebreak
                                                                                                (__toplevel_cons
                                                                                                   "FOLLOWERS"
                                                                                                   (__toplevel_cons
                                                                                                      'linebreak
                                                                                                      (__toplevel_cons
                                                                                                         'linebreak
                                                                                                         (__toplevel_cons
                                                                                                            " "
                                                                                                            (__toplevel_cons
                                                                                                               "#Scheme"
                                                                                                               (__toplevel_cons
                                                                                                                  " "
                                                                                                                  (__toplevel_cons
                                                                                                                     "#Racket"
                                                                                                                     (__toplevel_cons
                                                                                                                        "\nTags: "
                                                                                                                        (__toplevel_cons
                                                                                                                           "Racket is cool!"
                                                                                                                           (__toplevel_cons
                                                                                                                              "\n"
                                                                                                                              (__toplevel_cons
                                                                                                                                 "madewael"
                                                                                                                                 (__toplevel_cons
                                                                                                                                    "Tweet from "
                                                                                                                                    (__toplevel_cons
                                                                                                                                       'linebreak
                                                                                                                                       (__toplevel_cons
                                                                                                                                          'linebreak
                                                                                                                                          (__toplevel_cons
                                                                                                                                             "\nTags: "
                                                                                                                                             (__toplevel_cons
                                                                                                                                                "Hello World!"
                                                                                                                                                (__toplevel_cons
                                                                                                                                                   "\n"
                                                                                                                                                   (__toplevel_cons
                                                                                                                                                      "ephilips"
                                                                                                                                                      (__toplevel_cons
                                                                                                                                                         "Tweet from "
                                                                                                                                                         (__toplevel_cons
                                                                                                                                                            'linebreak
                                                                                                                                                            (__toplevel_cons
                                                                                                                                                               "TWEET WALL"
                                                                                                                                                               (__toplevel_cons
                                                                                                                                                                  "\n"
                                                                                                                                                                  (__toplevel_cons
                                                                                                                                                                     "Eline Philips"
                                                                                                                                                                     (__toplevel_cons
                                                                                                                                                                        "Name "
                                                                                                                                                                        (__toplevel_cons
                                                                                                                                                                           "\n"
                                                                                                                                                                           (__toplevel_cons
                                                                                                                                                                              "ephilips"
                                                                                                                                                                              (__toplevel_cons
                                                                                                                                                                                 "Twitter name "
                                                                                                                                                                                 (__toplevel_cons
                                                                                                                                                                                    'linebreak
                                                                                                                                                                                    (__toplevel_cons
                                                                                                                                                                                       " "
                                                                                                                                                                                       (__toplevel_cons
                                                                                                                                                                                          "#Scheme"
                                                                                                                                                                                          (__toplevel_cons
                                                                                                                                                                                             " "
                                                                                                                                                                                             (__toplevel_cons
                                                                                                                                                                                                "#Racket"
                                                                                                                                                                                                (__toplevel_cons
                                                                                                                                                                                                   "\nTags: "
                                                                                                                                                                                                   (__toplevel_cons
                                                                                                                                                                                                      "Racket is cool!"
                                                                                                                                                                                                      (__toplevel_cons "\n" (__toplevel_cons "madewael" (__toplevel_cons "Tweet from " ()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
         #f)))